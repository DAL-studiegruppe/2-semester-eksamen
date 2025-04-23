library(shiny)
library(shinydashboard)
library(ggsoccer)
library(ggplot2)
library(dplyr)
library(DT)
library(rsconnect)
library(tidyr)

# saveRDS(afleveringer_df, "afleveringer_df.rds")
# saveRDS(antal_pas, "antal_pas.rds")
# saveRDS(sp_passes, "sp_passes_with_rolename.rds")
# saveRDS(rolle_pr_cluster, "rolle_pr_cluster.rds")
# saveRDS(brøndby, "brøndby.rds")
# saveRDS(brøndby_pos, "brøndby_pos.rds")
# saveRDS(brøndby_kamp, "brøndby_kamp.rds")
# saveRDS(brøndby_kamp_med_label, "brøndby_kamp_med_label.rds")
# saveRDS(brøndby_kamp_med_pos, "brøndby_kamp_med_pos.rds")
# saveRDS(match_summary, "match_summary.rds")
# saveRDS(brøndby_matches, "brøndby_matches.rds")
# saveRDS(matchdetail_base, "matchdetail_base.rds")


afleveringer_df <- readRDS("afleveringer_df.rds")
antal_pas <- readRDS("antal_pas.rds")
sp_passes <- readRDS("sp_passes.rds")
rolle_pr_cluster <- readRDS("rolle_pr_cluster.rds")
brøndby <- readRDS("brøndby.rds")
brøndby_pos <- readRDS("brøndby_pos.rds")
brøndby_kamp <- readRDS("brøndby_kamp.rds")
brøndby_kamp_med_label <- readRDS("brøndby_kamp_med_label.rds")
brøndby_kamp_med_pos <- readRDS("brøndby_kamp_med_pos.rds")
match_summary <- readRDS("match_summary.rds")
brøndby_matches <- readRDS("brøndby_matches.rds")
matchdetail_base <- readRDS("matchdetail_base.rds")
players <- readRDS("players.RDS")
allpasses <- readRDS("allpasses.rds")
team <- readRDS("team.rds")
matchformation <- readRDS("matchformation.rds")
pssts <- readRDS("pssts.rds")
antal_pas <- readRDS("antal_pas.rds")


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
                  options = list(pageLength = 7, dom = 't'),  # 't' gør, at tabellen vises uden pagination
                  rownames = FALSE)
  })
  
  # Datatable pssts
  output$dataTable <- DT::renderDataTable({
    DT::datatable(pssts,
                  options = list(pageLength = 7, dom = 't'),
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
