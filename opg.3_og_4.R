# hente allevents og filter
# 3.1
library(DBI)
library(RMariaDB)
library(readr)
library(dplyr)
library(caret)
library(tree)
library(rpart)
library(pROC)
library(factoextra)
library(tidyr)
library(gridExtra)
library(ggrepel)
library(patchwork)
library(tidyverse)
library(cluster)
library(RColorBrewer)
library(lubridate)
library(reshape2)
library(rsconnect)


con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "EKSAMEN",
  host = "localhost",
  port = 3306,
  user = "root",
  password = "Jernæblevej16tv!"
)

passes <- dbReadTable(con, "matchevents_passes")
players <- dbReadTable(con, "players")
common <- dbReadTable(con,"matchevents_common")
matchformation <- dbReadTable(con,"matchformations")
matchdetail <- dbReadTable(con,"matchdetail_players")
team <- dbReadTable(con,"teams")
matchdetail_base <- dbReadTable(con,"matchdetail_base")

# Gem dataframes som RDS-filer
# saveRDS(passes, "passes.rds")
# saveRDS(players, "players.rds")
# saveRDS(common, "common.rds")
# saveRDS(matchformation, "matchformation.rds")
# saveRDS(matchdetail, "matchdetail.rds")
# saveRDS(team, "team.rds")
# saveRDS(matchdetail_base, "matchdetail_base.rds")

# valg nøvendigt variabler og scale
# her er pr.spiller og kode retunerer fejl men der er ikke problem med dataframe
allpasses <- passes %>% 
  filter(PRIMARYTYPE == "pass") %>% 
  left_join(
    common %>% select(PLAYER_WYID,EVENT_WYID,MATCHTIMESTAMP,LOCATIONX,LOCATIONY),
    by = "EVENT_WYID"
  ) %>% 
  left_join(
    players %>% select(PLAYER_WYID,SHORTNAME),
    by = "PLAYER_WYID"
  )%>%
  left_join(
    matchformation %>% select(PLAYER_WYID, PLAYERPOSITION, MATCH_WYID),
    by = c("MATCH_WYID", "PLAYER_WYID"),
    relationship = "many-to-many"
  ) %>% 
  left_join(
    matchdetail %>% select(PLAYER_WYID,TEAM_WYID),
    by = "PLAYER_WYID"
  ) %>% 
  distinct(EVENT_WYID, .keep_all = TRUE) %>% 
  mutate(player_position = case_when(
    PLAYERPOSITION %in% c("cf", "ss", "lw", "rw", "lwf", "rwf") ~ 1,  # Angriber
    PLAYERPOSITION %in% c("amf", "dmf", "lcmf", "rcmf", "lcmf3", "rcmf3", "lamf", "ramf", "ldmf", "rdmf") ~ 2,  # Midtbane
    PLAYERPOSITION %in% c("cb", "lcb", "rcb", "lcb3", "rcb3", "lb", "rb", "lb5", "rb5", "lwb", "rwb") ~ 3,  # Forsvar
    PLAYERPOSITION %in% c("gk") ~ 4,  # Goalkeaper
    TRUE ~ NA_real_  
  ))

allpasses$MATCHTIMESTAMP <- gsub("^0", "", allpasses$MATCHTIMESTAMP) 
allpasses$MATCHTIMESTAMP <- period_to_seconds(hms(allpasses$MATCHTIMESTAMP))

# cluster pr.spiller
sp_passes <-allpasses %>%
  group_by(PLAYER_WYID,SHORTNAME) %>%
  summarise(
    player_position = player_position,
    team_id = TEAM_WYID,
    antal_afleveringer = n(),
    gennemsnit_længde = mean(LENGTH, na.rm = TRUE),
    sd_af = sd(LENGTH, na.rm = TRUE),
    gennemsnit_vinkel = mean(ANGLE, na.rm = TRUE),
    sd_vk = sd(ANGLE, na.rm=TRUE),
    accuracy = mean(ACCURATE == TRUE, na.rm = TRUE) ,
    sd_acc = sqrt(accuracy/100 * (1-accuracy/100) / antal_afleveringer) 
  ) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE) %>% 
  na.omit() %>% 
  filter(
    antal_afleveringer >= 108
  )


sp_passes <- sp_passes %>% 
  left_join(
    team %>% select(TEAM_WYID, TEAMNAME),
    by = c("team_id" = "TEAM_WYID")
  ) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)


checkpasses <- sp_passes[,-c(1,2,5)]

kmp_passes <- as.data.frame(scale(sp_passes[,-c(1:5,10:12)]))
kmp_passes$accuracy <- sp_passes$accuracy
kmp_passes$sd_acc <- sp_passes$sd_acc

fviz_nbclust(kmp_passes, kmeans, method = "wss", k.max = 10)
fviz_nbclust(kmp_passes, kmeans, method = "silhouette", k.max = 10)

sp_kmeans <- kmeans(kmp_passes,centers = 5,nstart = 15)
fviz_cluster(sp_kmeans,data = kmp_passes)

sp_passes$cluster <- as.factor(sp_kmeans$cluster)


# doubble check
checkpasses$cluster <- sp_kmeans$cluster
bssts <- checkpasses %>% 
  group_by(cluster, player_position) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = player_position, values_from = count, values_fill = 0)

bssts2 <- checkpasses %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
bssts2 <- bssts2[,-c(2,3)]

bssts3 <- checkpasses %>% 
  group_by(TEAMNAME) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
bssts3 <- bssts3[,-c(2,3,10)]

bssts4 <- checkpasses %>% 
  group_by(cluster, TEAMNAME) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = TEAMNAME, values_from = count, values_fill = 0)


# her er cluster pr.afleveringer i kampens interval

passes_sekv <- allpasses %>%
  filter(MATCHTIMESTAMP<5400) %>% 
  mutate(
    tidsinterval = floor(MATCHTIMESTAMP / 600) + 1
  )

passes_int <- passes_sekv %>%
  group_by(MATCH_WYID, tidsinterval) %>%
  summarise(
    antal_afleveringer = n(),
    gennemsnit_længde = mean(LENGTH, na.rm = TRUE),
    sd_af = sd(LENGTH, na.rm = TRUE),
    gennemsnit_vinkel = mean(ANGLE, na.rm = TRUE),
    sd_vk = sd(ANGLE, na.rm=TRUE),
    accuracy = mean(ACCURATE == TRUE, na.rm = TRUE),
    sd_acc = sqrt(accuracy/100 * (1-accuracy/100) / antal_afleveringer)
  ) %>%
  distinct(MATCH_WYID, tidsinterval, .keep_all = TRUE)

int_passes <- as.data.frame(scale(passes_int[,-c(1:2,8:9)]))
int_passes$accuracy <- passes_int$accuracy
int_passes$sd_acc <- passes_int$sd_acc

fviz_nbclust(int_passes, kmeans, method = "wss", k.max = 20)
fviz_nbclust(int_passes, kmeans, method = "silhouette", k.max = 20)

int_kmean <- kmeans(int_passes,centers = 9,nstart = 15)
fviz_cluster(int_kmean,data = int_passes)


passes_int$cluster <- as.factor(int_kmean$cluster)
check_pass <- passes_int[,-c(1)]

pssts <- check_pass %>% 
  group_by(cluster, tidsinterval) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = tidsinterval, values_from = count, values_fill = 0)

pssts2 <- check_pass %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
pssts2 <- pssts2[,-2]

pssts3 <- check_pass %>% 
  group_by(tidsinterval) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# pass_int for brøndby 7453
passes_int_bb <- passes_sekv %>%
  filter(TEAM_WYID==7453) %>% 
  group_by(MATCH_WYID, tidsinterval) %>%
  summarise(
    antal_afleveringer = n(),
    gennemsnit_længde = mean(LENGTH, na.rm = TRUE),
    sd_af = sd(LENGTH, na.rm = TRUE),
    gennemsnit_vinkel = mean(ANGLE, na.rm = TRUE),
    sd_vk = sd(ANGLE, na.rm=TRUE),
    accuracy = mean(ACCURATE == TRUE, na.rm = TRUE),
    sd_acc = sqrt(accuracy/100 * (1-accuracy/100) / antal_afleveringer)
  ) %>%
  distinct(MATCH_WYID, tidsinterval, .keep_all = TRUE)

int_passes_bb <- as.data.frame(scale(passes_int_bb[,-c(1:2,8:9)]))
int_passes_bb$accuracy <- passes_int_bb$accuracy
int_passes_bb$sd_acc <- passes_int_bb$sd_acc

fviz_nbclust(int_passes_bb, kmeans, method = "wss", k.max = 20)

bbssts <- passes_int_bb %>% 
  group_by(tidsinterval) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
bbssts <- bbssts[,-2]


# clustering pr.aflevering

pr_passes <- allpasses %>% 
  filter(MATCHTIMESTAMP<5400) %>% 
  mutate(
    tidsinterval = floor(MATCHTIMESTAMP / 900) + 1
  ) %>% 
  select(ANGLE, LENGTH, LOCATIONX,LOCATIONY,ENDLOCATIONX, ENDLOCATIONY, tidsinterval, ACCURATE)

prp_passes <- pr_passes%>% 
  mutate(
    across(c(ANGLE, LENGTH, LOCATIONX,LOCATIONY,ENDLOCATIONX, ENDLOCATIONY), scale)
  )
prp_passes <- prp_passes[,-c(7:9)]
# pca
data.pca <- princomp(prp_passes)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_pca_var(data.pca, col.var = "black")

# data er for stort dermed tager vi sample af dataer
fviz_nbclust(allpasses_sample, kmeans, method = "wss", k.max = 10)
fviz_nbclust(allpasses_sample, kmeans, method = "silhouette", k.max = 10)

kmean <- kmeans(prp_passes,centers = 5,nstart = 10)

pr_passes$cluster <- as.factor(kmean$cluster)
pr_passes$ACCURATE <- as.factor(pr_passes$ACCURATE)
fviz_cluster(kmean, data = allpasses)


# check og gruppere med cluster
pssts <- pr_passes %>% 
  group_by(cluster) %>% 
  mutate(sd_length = sd(LENGTH, na.rm=T),
         sd_vinkel = sd(ANGLE, na.rm=T),
         accuracy = mean(ACCURATE == "1", na.rm=TRUE)) %>% 
  summarise(across(where(is.numeric), ~round(mean(., na.rm = TRUE), digits = 2)))

pssts <- pssts[,-8]

pssts1 <- pr_passes %>% 
  group_by(cluster, tidsinterval) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = tidsinterval, values_from = count, values_fill = 0)


#############################################
# OPGAVE 4

library(shiny)
library(shinydashboard)
library(ggsoccer)
library(ggplot2)
library(dplyr)
library(DT)

# saveRDS(sp_passes, file = "sp_passes.rds")
# saveRDS(allpasses, file = "allpasses.rds")
# saveRDS(pssts, file = "pssts.rds")
# saveRDS(pr_passes, file = "pr_passes.rds")
sp_passes <- readRDS("sp_passes.rds")
allpasses <- readRDS("allpasses.rds")
pssts <- readRDS("pssts.rds")
pr_passes <- readRDS("pr_passes.rds")

###########
# OPG 4.1

# Opret dataframe med tabellens indhold
afleveringer_df <- data.frame(
  Cluster = c(1, 2, 3, 4, 5),
  Funktion_i_spillet = c(
    "Progressivt spil",
    "Skarpe pasninger udenfor feltzone",
    "Kombinationsspil", 
    "Kombinationsspil",
    "Skarpe pasninger udenfor feltzone"
  ),
  Banens_3_del = c(
    "Midt (33-66)",
    "Højt (66-100)",
    "Lavt (0-33)", 
    "Lavt (0-33)",
    "Højt (66-100)"
  ),
  Kommentar = c(
    "Fremad pasning",
    "Tæt på mål",
    "Tidligt opspil", 
    "Tidligt opspil",
    "Tæt på mål"
  )
)


# tæl antallet af aflevering i de 5 clustre
antal_pas <- sp_passes %>% 
  group_by(cluster) %>% 
  summarise(
    total_afleveringer = sum(antal_afleveringer)
  )

# tilføj ROLENAME
sp_passes <- sp_passes %>% 
  left_join(
    players %>% select(PLAYER_WYID, ROLENAME),
    by = "PLAYER_WYID"
  )

# Antal spillere per rolle i hvert cluster
rolle_pr_cluster <- sp_passes %>%
  group_by(cluster, ROLENAME) %>%
  summarise(antal = n(), .groups = "drop")


########
# OPG 4.2

# Alle brøndby spillere samlet for hele sæsonen
brøndby <- sp_passes %>%
  filter(TEAMNAME == "Brøndby") %>% 
  select(SHORTNAME, player_position, cluster, antal_afleveringer, gennemsnit_længde, gennemsnit_vinkel, accuracy, sd_af, sd_vk, sd_acc) %>% 
  mutate_at(vars(6:ncol(.)), ~round(., 2)) %>%
  rename(
    gns_længde = gennemsnit_længde,
    gns_vinkel = gennemsnit_vinkel
  )

brøndby_pos <- brøndby %>%
  left_join(
    select(players, PLAYER_WYID, ROLENAME) %>%
      group_by(PLAYER_WYID) %>%
      slice_head(n = 1),
    by = c("PLAYER_WYID")
  ) %>% 
  select(
    1:2,
    ROLENAME,
    4:ncol(.)
  )

saveRDS(brøndby_pos, "brøndby_pos.rds")

# alle spillere pr. matchID
brøndby_kamp <- allpasses %>%  
  group_by(MATCH_WYID,PLAYER_WYID,SHORTNAME) %>%
  summarise(
    player_position = player_position,
    team_id = TEAM_WYID,
    antal_afleveringer = n(),
    gennemsnit_længde = mean(LENGTH, na.rm = TRUE),
    sd_af = sd(LENGTH, na.rm = TRUE),
    gennemsnit_vinkel = mean(ANGLE, na.rm = TRUE),
    sd_vk = sd(ANGLE, na.rm=TRUE),
    accuracy = mean(ACCURATE == TRUE, na.rm = TRUE),
    sd_acc = sqrt(accuracy/100 * (1-accuracy/100) / antal_afleveringer) * 100
  ) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE) %>% 
  na.omit()

brøndby_kamp <- brøndby_kamp %>% 
  left_join(
    team %>% select(TEAM_WYID, TEAMNAME),
    by = c("team_id" = "TEAM_WYID")
  ) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)

# Alle brøndby spillere pr. kampe for
brøndby_kamp <- brøndby_kamp %>% 
  filter(TEAMNAME == "Brøndby")

# Tilføj teamname i matchdetail_base for at kunne lave label til dropdown
matchdetail_base$TEAMNAME <- team$TEAMNAME[match(matchdetail_base$TEAM_WYID, team$TEAM_WYID)]

# slet ligegyldige kolonner
matchdetail_base <- matchdetail_base[, -c(8:25)]

# lav label med teamname og score
match_summary <- matchdetail_base %>%
  group_by(MATCH_WYID) %>%
  mutate(IsHome = SIDE == "home") %>%  # Bemærk: "home" med lille h
  arrange(MATCH_WYID, !IsHome) %>%     # Sorter så hjemmehold kommer først
  summarize(
    MatchLabel = paste0(
      first(TEAMNAME), " - ", last(TEAMNAME), ", ",
      first(SCORE), "-", last(SCORE) 
    )
  )

# vælg nu kun labels hvor brøndby indgår
brøndby_matches <- match_summary %>%
  filter(grepl("Brøndby", MatchLabel))

# nu spillere med kamp label
brøndby_kamp_med_label <- merge(
  brøndby_kamp,
  brøndby_matches,
  by = "MATCH_WYID"
)

brøndby_kamp_med_label <- brøndby_kamp_med_label %>%
  rename(
    gns_længde = gennemsnit_længde,
    gns_vinkel = gennemsnit_vinkel) %>% 
  mutate(across(6:12, ~round(., 2))) %>% 
  select(-c(team_id, TEAMNAME))

# opdater med positioner fra kampene
brøndby_kamp_med_pos <- brøndby_kamp_med_label %>%
  left_join(
    select(matchformation, MATCH_WYID, PLAYER_WYID, PLAYERPOSITION) %>%
      group_by(MATCH_WYID, PLAYER_WYID) %>%
      slice_head(n = 1),
    by = c("MATCH_WYID", "PLAYER_WYID")
  ) %>%
  select(
    1:3,
    PLAYERPOSITION,
    -player_position,
    5:ncol(.)
  )

###################
# SHINY

ui <- dashboardPage(
  dashboardHeader(title = "BIF - Afleveringsanalyse"),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Opgave 4.1", tabName = "opg1"),
      menuItem("Opgave 4.2", tabName = "opg2")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Plot tab
      tabItem(tabName = "opg1",
              fluidRow(
                box(
                  width = 7,
                  plotOutput("passPlot", height = "600px")
                ),
                box(
                  title = "Forklaring af clusters",
                  width = 5,
                  DT::dataTableOutput("clusterinfo")
                )
              ),
              fluidRow(
                box(
                  title = "Afleveringsdata",
                  width = 12,
                  DT::dataTableOutput("dataTable")
                )
              ),
              fluidRow(
                box(
                  title = "Afleveringer fordelt i clusters",
                  width = 12,
                  plotOutput("passPlot2", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Positioner i clustre",
                  width = 12,
                  plotOutput("posPlot2", height = "600px")
                )
              )
      ),
      
      # Datasæt tab
      tabItem(tabName = "opg2",
              # divider
              tags$div(
                style = "border-top: 2px solid #3c8dbc; margin: 20px 0;",
                tags$h3("Afleveringer pr. kamp", 
                        style = "text-alging: center; color: #3c8dbc")
              ),
              fluidRow(
                # Filtreringsboks
                box(
                  width = 3,
                  status = "primary",
                  selectInput("matchfilter", "Vælg kamp",
                              choices = NULL)
                ),
                box(
                  title = "Hvordan performer spillerne i kampene?",
                  width = 9,
                  plotOutput("scatterPlot", height = "400px")
                ),
                box(
                  width = 12,
                  DT::dataTableOutput("labelDataTable")
                )
              ),
              # divider
              tags$div(
                style = "border-top: 2px solid #3c8dbc; margin: 20px 0;",
                tags$h3("Afleveringer for hele sæsonen", 
                        style = "text-alging: center; color: #3c8dbc")
              ),
              fluidRow(
                box(
                  width = 3,
                  sliderInput("accuracyFilter", 
                              "Vælg antal spillere",
                              min = 1, 
                              max = 26,
                              value = 3,
                              step = 1)
                ),
                box(
                  width = 9,
                  plotOutput("barplot", height = "400px")
                ),
                box(
                  width = 12,
                  DT::dataTableOutput("fullDataTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observe({
    match_choices <- unique(brøndby_kamp_med_pos$MatchLabel)
    updateSelectInput(session, "matchfilter", 
                      choices = match_choices,
                      selected = match_choices[1])
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    brøndby_kamp_med_pos %>% 
      filter(MatchLabel == input$matchfilter)
  })
  
  filtered_players <- reactive({
    # Sortér og filtrer spillere - FJERN FØRST ALLE GRUPPERINGER
    result <- brøndby_pos %>%
      ungroup() %>%  # Fjern alle grupperinger
      arrange(desc(antal_afleveringer)) %>%
      slice(1:input$accuracyFilter)
  })
  
  # Afleveringsplot
  output$passPlot <- renderPlot({
    ggplot(pssts) +
      annotate_pitch(
        colour = "white",
        fill = "darkgreen",
      ) +
      geom_segment(aes(x = LOCATIONX, y = LOCATIONY, 
                       xend = ENDLOCATIONX, yend = ENDLOCATIONY,
                       color = factor(cluster)),
                   arrow = arrow(length = unit(0.4, "cm")),
                   size = 2,
                   alpha = 1) +
      geom_point(aes(x = LOCATIONX, y = LOCATIONY, color = factor(cluster)), 
                 size = 2, alpha = 0.7) +
      scale_color_brewer(palette = "Set1", name = "Cluster") +
      theme_pitch() +
      theme(legend.position = "bottom",
            text = element_text(size = 14),
            plot.title = element_text(size = 24, face = "bold"),
            plot.subtitle = element_text(size = 20),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 16)) +
      labs(title = "Afleveringsanalyse",
           subtitle = "Visualisering af forskellige afleveringstyper")
  })
  
  # Cluster beskrivelse
  output$clusterinfo <- DT::renderDataTable({
    DT::datatable(afleveringer_df,  # Antager at du bruger den dataframe vi oprettede tidligere
                  options = list(pageLength = 5, dom = 't'),  # 't' gør, at tabellen vises uden pagination
                  rownames = FALSE)
  })
  
  # Datatable pssts
  output$dataTable <- DT::renderDataTable({
    DT::datatable(pssts,
                  options = list(pageLength = 5, dom = 't'),
                  rownames = FALSE)
  })
  
  # plot til antal afleveringer i clusters
  output$passPlot2 <- renderPlot({
    ggplot(antal_pas, aes(x = cluster, y = total_afleveringer, fill = cluster)) +
      geom_col() +
      labs(
        title = "Totalt antal afleveringer pr. cluster",
        x = "Cluster",
        y = "Antal afleveringer"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      geom_text(aes(label = total_afleveringer), vjust = -0.5) +
      theme(legend.position = "bottom")
  })
  
  # ROLENAME i clusters
  output$posPlot2 <- renderPlot({
    ggplot(rolle_pr_cluster, aes(x = cluster, y = antal, fill = ROLENAME)) +
      geom_col(position = "stack") +
      labs(
        title = "Fordeling af spillerroller i hvert cluster",
        x = "Cluster",
        y = "Antal spillere",
        fill = "Spillerrolle"
      ) +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal() +
      geom_text(
        aes(label = antal),
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold") +
      theme(legend.position = "bottom")
  })

  # Output for scatter plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = gns_længde, y = antal_afleveringer, color = accuracy)) +
      geom_point(size = 5, alpha = 0.7) +
      geom_text(aes(label = paste0(SHORTNAME, " (", round(accuracy, 1), "%)")), 
                hjust = 0.5, vjust = -1, size = 5) +
      theme_minimal() +
      scale_x_continuous(limits = c(8, 40)) +
      scale_y_continuous(limits = c(0, 130)) +
      labs(
        title = paste("Kamp:", input$matchfilter),
        x = "Gns. længde (meter)",
        y = "Antal afleveringer"
      ) +
      theme(legend.position = "none")
      
  })
  
  # tabel ud fra valgte label
  output$labelDataTable <- DT::renderDataTable({
    DT::datatable(filtered_data(),
                  options = list(
                    pageLength = 5,
                    columnDefs = list(list(
                      targets = which(colnames(filtered_data()) == "MatchLabel") -1,
                      visible = FALSE
                    ))
                  ),
                  rownames = FALSE
                  )
  })
  
  # barplot for alle spillere
  output$barplot <- renderPlot({
    
    # Hent de filtrerede spillere
    top_n_players <- filtered_players()
    
    # Sorter spillerne igen for at sikre korrekt rækkefølge
    ordered_players <- top_n_players %>%
      arrange(desc(antal_afleveringer))
    
    # Bevar rækkefølgen ved at konvertere SHORTNAME til en faktor
    ordered_players$SHORTNAME <- factor(
      ordered_players$SHORTNAME,
      levels = ordered_players$SHORTNAME
    )
    
    # Skab longformat data
    plot_data <- ordered_players %>%
      pivot_longer(
        cols = c(antal_afleveringer, accuracy),
        names_to = "Metric",
        values_to = "Value"
      )
    
    # Definer faktor-niveauerne
    plot_data$Metric <- factor(plot_data$Metric, 
                               levels = c("antal_afleveringer", "accuracy"))
    
    # Find skalering mellem de to akser
    scale_factor <- max(plot_data$Value[plot_data$Metric == "antal_afleveringer"], na.rm = TRUE) /
      max(plot_data$Value[plot_data$Metric == "accuracy"], na.rm = TRUE)
    
    # Lav plottet MED BEVARELSE AF FAKTOR-RÆKKEFØLGEN
    ggplot(plot_data, aes(x = SHORTNAME, fill = Metric)) +
      geom_bar(aes(y = ifelse(Metric == "antal_afleveringer", Value, Value * scale_factor)), 
               stat = "identity", position = "dodge") +
      geom_text(aes(y = ifelse(Metric == "antal_afleveringer", Value, Value * scale_factor), 
                    label = round(Value, 1)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      scale_y_continuous(
        name = "Antal afleveringer",
        sec.axis = sec_axis(~ . / scale_factor, name = "Afleveringspræcision (%)")
      ) +
      labs(x = "Spillere",
           fill = "Metrik",
           title = paste("Viser top", input$accuracyFilter, "spillere efter antal afleveringer")) +
      theme_minimal() +
      scale_fill_manual(
        name = "Metrik",
        values = c("antal_afleveringer" = "blue", "accuracy" = "lightblue"),
        labels = c("Antal afleveringer", "Afleveringspræcision (%)")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Alle bif spillere
  output$fullDataTable <- DT::renderDataTable({
    DT::datatable(brøndby_pos,
                  options = list(pageLength = 5),
                  rownames = FALSE
                  )
  })
}

# Kør applikationen
shinyApp(ui, server)

# lav geom_bar plot til hele sæsonen om med antal afleveringer og accurcy i hver deres y akser. 
# lav roler til antal spiller der skal vises i plot, desc fra høj til lav