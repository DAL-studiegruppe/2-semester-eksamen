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

#con <- dbConnect(
  #RMariaDB::MariaDB(),
  #dbname = "Eksamen",
  #host = "localhost",
  #port = 3306,
  #user = "root",
  #password = "psw"
#)

#passes <- dbReadTable(con, "matchevents_passes")
#players <- dbReadTable(con, "players")
#common <- dbReadTable(con,"matchevents_common")
#matchformation <- dbReadTable(con,"matchformations")
#matchdetail <- dbReadTable(con,"matchdetail_players")
#team <- dbReadTable(con,"teams")

#saveRDS(passes, file = "passes.rds")
#saveRDS(common, file = "common.rds")
#saveRDS(matchdetail, file = "matchdetail.rds")
#saveRDS(team, file = "team.rds")

passes <- readRDS("passes.rds")
players <- readRDS("players.rds")
common <- readRDS("common.rds")
matchformation <- readRDS("matchformation.rds")
matchdetail <- readRDS("matchdetail.rds")
team <- readRDS("team.rds")

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

# clustering pr.aflevering

pr_passes <- allpasses %>% 
  filter(MATCHTIMESTAMP<5400) %>% 
  mutate(
    tidsinterval = floor(MATCHTIMESTAMP / 900) + 1
  ) %>% 
  select(ANGLE, LENGTH, LOCATIONX,LOCATIONY,ENDLOCATIONX, ENDLOCATIONY, tidsinterval)

prp_passes <- pr_passes%>% 
  mutate(
    across(c(ANGLE, LENGTH, LOCATIONX,LOCATIONY,ENDLOCATIONX, ENDLOCATIONY), scale)
  )
prp_passes <- prp_passes[,-7]
# pca
data.pca <- princomp(prp_passes)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_pca_var(data.pca, col.var = "black")


kmean <- kmeans(prp_passes,centers = 5,nstart = 10)

pr_passes$cluster <- as.factor(kmean$cluster)
fviz_cluster(kmeans,data = allpasses)

# check og gruppere med cluster
pssts <- pr_passes %>% 
  group_by(cluster) %>% 
  mutate(sd_length = sd(LENGTH, na.rm=T),
         sd_vinkel = sd(ANGLE, na.rm=T),
         accuracy = mean(ACCURATE == TRUE, na.rm = TRUE)) %>% 
  summarise(across(where(is.numeric), ~round(mean(., na.rm = TRUE), digits = 1)))
pssts <- pssts[,-8]

pssts1 <- pr_passes %>% 
  group_by(cluster, tidsinterval) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = tidsinterval, values_from = count, values_fill = 0)

########################################################################################
### Her er andre metoder til cluster hvor vi ikke har brugt til besvar opg.3 ###########
########################################################################################

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
    accuracy = mean(ACCURATE == TRUE, na.rm = TRUE),
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


