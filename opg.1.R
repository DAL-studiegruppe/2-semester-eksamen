# hente allevents og filter
# 1.1
library(DBI)
library(RMariaDB)
library(readr)
library(dplyr)
library(caret)
library(tree)
library(rpart)
library(randomForest)
library(pROC)
library(ggsoccer)
library(metR)
source("funktion.R")

#con <- dbConnect(
  #RMariaDB::MariaDB(),
  #dbname = "Eksamen",
  #host = "localhost",
  #port = 3306,
  #user = "root",
  #password = "psw"
#)

#allevents <- dbReadTable(con, "AllEvents")
#matchformation <- dbReadTable(con, "matchformations")
#players <- dbReadTable(con, "players")

#saveRDS(allevents,"allevents.RDS")
#saveRDS(matchformation,"matchformations.RDS")
#saveRDS(players,"players.RDS")

allevents <- readRDS("allevents.RDS")
matchformation <- readRDS("matchformations.RDS")
players <- readRDS("players.RDS")

# filtere 23/24 season og 24/25 for opg 1.6
season_2324 <- allevents %>% 
  filter(SEASON_WYID == 188945)

season_2325 <- allevents %>% 
  filter(SEASON_WYID == 189918)

# filtere kun for shots
shot_2324 <- season_2324 %>% 
  filter(PRIMARYTYPE == "shot")

# test
test <- season_2324 %>% 
  filter(ATTACKWITHSHOT == 1)

# find nøvendigt kolon
opg.1 <- shot_2324 %>%
  rowwise() %>%  
  mutate(
    distance_to_goal = sqrt((LOCATIONX - 100)^2 + (LOCATIONY - 50)^2),
    is_foot = as.factor(ifelse(SHOTBODYPART %in% c("right_foot", "left_foot"), 1, 0)),
    angle_to_goal = calculate_angle_degrees(LOCATIONX, LOCATIONY, 100, 44.6, 100, 55.4)
  ) %>%
  ungroup()  

# joine player dataframe for find ude af players main foot
opg.1 <- opg.1%>% 
  left_join(
    players %>% select(PLAYER_WYID, FOOT, BIRTHAREA_WYID),
    by = "PLAYER_WYID",
    relationship = "many-to-many"
  )

# find players position når de laver et shot
opg.1 <- opg.1 %>% 
  left_join(
    matchformation %>% select(PLAYER_WYID, PLAYERPOSITION, MATCH_WYID),
    by = c("MATCH_WYID", "PLAYER_WYID"),
    relationship = "many-to-many"
  )

# brug distinct til filtere unique eventsid for fjerne duplikationer
opg.1 <- opg.1 %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

opg.1 <- opg.1 %>% 
  mutate(
    is_main_foot = case_when(
      FOOT == "right" & SHOTBODYPART == "right_foot" ~ 1,
      FOOT == "left" & SHOTBODYPART == "left_foot" ~ 1,
      FOOT == "both" & SHOTBODYPART %in% c("left_foot", "right_foot") ~ 1,      
      TRUE ~ 0
    )
  )

# difinere spillerens position
opg.1 <- opg.1 %>%
  mutate(player_position = case_when(
    PLAYERPOSITION %in% c("cf", "ss", "lw", "rw", "lwf", "rwf") ~ 1,  # Angriber
    PLAYERPOSITION %in% c("amf", "dmf", "lcmf", "rcmf", "lcmf3", "rcmf3", "lamf", "ramf", "ldmf", "rdmf") ~ 2,  # Midtbane
    PLAYERPOSITION %in% c("cb", "lcb", "rcb", "lcb3", "rcb3", "lb", "rb", "lb5", "rb5", "lwb", "rwb") ~ 3,  # Forsvar
    TRUE ~ NA_real_  # Håndterer "gk" og eventuelle andre positioner
  ))

# der er en enkelt NA fra gk i række 2510, tænker med fjerne den
opg.1 <- opg.1[-2150,]

# definere set_pieces med possessiontype 1 og 2
opg.1 <- opg.1 %>% 
  mutate(
    set_pieces = case_when(
      POSSESSIONTYPE1 == "set_piece_attack" ~ 1,
      POSSESSIONTYPE2 == "set_piece_attack" ~ 1,
      TRUE ~ 0
    )
  )

# definere shot after corner
opg.1 <- opg.1 %>% 
  mutate(
    shot_aftercorner = case_when(
      SECONDARYTYPE1 == "shot_after_corner" ~ 1,
      SECONDARYTYPE2 == "shot_after_corner" ~ 1,
      SECONDARYTYPE3 == "shot_after_corner" ~ 1,
      SECONDARYTYPE4 == "shot_after_corner" ~ 1,
      TRUE ~ 0
    )
  )

# definere counter attack fra possessiontype 1

opg.1 <- opg.1 %>% 
  mutate(
    counter_attack = case_when(
      POSSESSIONTYPE1 == "counterattack" ~ 1,
      TRUE ~ 0
    )
  )

# definere shot er kommer fra drible
dribble_primary <- c("touch", "duel", "acceleration")
dribble_secondary <- c("dribble", "carry", "progressive_run")

dribble_events <- season_2324 %>%
  filter(
    PRIMARYTYPE %in% dribble_primary |
      SECONDARYTYPE1 %in% dribble_secondary |
      SECONDARYTYPE2 %in% dribble_secondary |
      SECONDARYTYPE3 %in% dribble_secondary |
      SECONDARYTYPE4 %in% dribble_secondary |
      SECONDARYTYPE5 %in% dribble_secondary
  ) 

dribble_to_shot_direct <- dribble_events %>%
  inner_join(opg.1, by = c("RELATEDEVENT_WYID" = "EVENT_WYID", "MATCH_WYID", "POSSESSION_WYID"))

dribble_to_shot_direct <- dribble_to_shot_direct %>% 
  mutate(
    is_shot_by_dribble = 1
  )

opg.1 <- opg.1 %>% 
  left_join(
    dribble_to_shot_direct %>% select(shot_id = RELATEDEVENT_WYID, is_shot_by_dribble),
    by = c("EVENT_WYID" = "shot_id")
  )

opg.1 <- opg.1 %>%
  mutate(is_shot_by_dribble = ifelse(is.na(is_shot_by_dribble), 0, is_shot_by_dribble))

# træk de nøvendigt koloner 

topg.1 <- opg.1 %>% 
  select(SHOTISGOAL, SHOTONTARGET, distance_to_goal, angle_to_goal, is_foot, is_main_foot, 
         set_pieces, counter_attack, is_shot_by_dribble, player_position, shot_aftercorner,SHOTXG, LOCATIONX, LOCATIONY, BIRTHAREA_WYID
         )
topg.1 <- topg.1 %>%
  mutate(continent = case_when(
    # Europa (1)
    BIRTHAREA_WYID %in% c(40, 56, 100, 191, 203, 208, 234, 246, 250, 270, 288, 300, 
                          324, 332, 352, 380, 528, 578, 620, 643, 703, 705, 724, 
                          752, 756, 826) ~ 1,
    
    # Asien (2)
    BIRTHAREA_WYID %in% c(364, 392, 410, 422) ~ 2,
    
    # Sydamerika (3)
    BIRTHAREA_WYID %in% c(8, 76, 152, 600, 858) ~ 3,
    
    # Afrika (4)
    BIRTHAREA_WYID %in% c(276, 384, 566, 624, 694, 710, 854, 894) ~ 4,
    
    # Oceanien (5)
    BIRTHAREA_WYID %in% c(36, 554) ~ 5,
    
    # Nordamerika (6)
    BIRTHAREA_WYID %in% c(662, 840) ~ 6,
    
    # Ukendte (7)
    TRUE ~ 7
  ))

topg.1$SHOTISGOAL <- as.factor(topg.1$SHOTISGOAL)

#topg.1 <- topg.1 %>% 
  #mutate(across(c(SHOTISGOAL, SHOTONTARGET, is_foot, is_main_foot, 
                  #set_pieces, counter_attack, is_shot_by_dribble, player_position), as.factor))

# dele i træning og test dataer 

index <- createDataPartition(topg.1$SHOTISGOAL, p = 0.7, list = FALSE)

training <- topg.1[index, ]
test <- topg.1[-index, ]

# lave regression
# original SHOTISGOAL ~ distance_to_goal + angle_to_goal + is_foot + is_main_foot + set_pieces + counter_attack + is_shot_by_dribble + player_position
glm <- glm(SHOTISGOAL ~distance_to_goal + angle_to_goal + is_foot + LOCATIONX + LOCATIONY,
          family = binomial, 
          data = training)
summary(glm)

predict_glm <- predict(glm, type = "response")

predict_glm_test <- predict(glm, newdata = test,  type = "response")

roc_curve_glm <- roc(training$SHOTISGOAL, predict_glm)
plot(roc_curve_glm, print.auc=TRUE, auc.polygon=TRUE, grid=TRUE)


# Anvend threshold
preds_glm <- ifelse(predict_glm_test > 0.3, 1, 0)
table(preds_glm,test$SHOTISGOAL)

tt1 <- training %>% 
  mutate(pred <- predict_glm)

tt2 <- test %>% 
  mutate(pred <- predict_glm_test)

# glm graf
glm_matrix <- table(optimal_preds_glm, test$SHOTISGOAL)
glm_df <- as.data.frame(glm_matrix)
names(glm_df) <- c("Prediction", "Reference", "Frequency")
glm_accuracy <- sum(diag(glm_matrix)) / sum(glm_matrix)
glm_precision <- glm_matrix[2,2] / sum(glm_matrix[2,])

ggplot(data = glm_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white", size = 5) +
  scale_fill_gradient(low = "darkblue", high = "lightblue") +
  labs(title = "GLM Confusion Matrix",
       subtitle = paste("Accuracy:", round(glm_accuracy, 4), 
                        "Precision:", round(glm_precision, 4)
       ),
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# tree model
tree <- tree(SHOTISGOAL ~ distance_to_goal + angle_to_goal + is_foot + LOCATIONX + LOCATIONY, 
             data = topg.1)

tree2 <- rpart(SHOTISGOAL ~ distance_to_goal + 
                 is_foot + set_pieces + counter_attack, 
               data = topg.1,
               method = "class",control = rpart.control(cp = 0.001))

plot(tree, type = "uniform", main = "Decision Tree", margin = 0.1)
text(tree, pretty = 0)

plot(tree2)
text(tree2, use.n = TRUE, 
     all = TRUE, cex = 0.8)

summary(tree)

cv.tree <- cv.tree(tree, FUN = prune.misclass)
cv.tree

plot(cv.tree$size, cv.tree$dev, type = "b")
plot(cv.tree$k, cv.tree$dev, type = "b")

prune.tree <- prune.misclass(tree, best = 3)
plot(prune.tree)
text(prune.tree, pretty = 0)

tree.pred <- predict(prune.tree, test, type = "class")

tree_matrix <- confusionMatrix(tree.pred, test$SHOTISGOAL)
tree_matrix

# randomforest
rf <- randomForest(SHOTISGOAL ~ distance_to_goal + angle_to_goal + is_foot + LOCATIONX + LOCATIONY + continent, 
                   data = training, ntree = 1000)

varImpPlot(rf)
importance(rf)

predict_rf <- predict(rf, newdata=test)
rf_probs <- predict(rf, type = "prob")

rf_matrix <- table(predict_rf,test$SHOTISGOAL)
rf_matrix

# predict graf for rf
{
conf_df <- as.data.frame(rf_matrix)
names(conf_df) <- c("Prediction", "Reference", "Frequency")

# Beregn accuracy, precision, recall og F1 score
accuracy <- sum(diag(rf_matrix)) / sum(rf_matrix)
precision <- rf_matrix[2,2] / sum(rf_matrix[2,])

# Opret en pæn visualisering
ggplot(data = conf_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white", size = 5) +
  scale_fill_gradient(low = "darkblue", high = "lightblue") +
  labs(title = "Random Forest Confusion Matrix",
       subtitle = paste("Accuracy:", round(accuracy, 4), 
                        "Precision:", round(precision, 4)
                      ),
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
}

roc_curve <- roc(training$SHOTISGOAL, rf_probs[,2])
plot(roc_curve, print.auc=TRUE, auc.polygon=TRUE, grid=TRUE)
# kontrol
# Definér kontrolparametre for RFE
target_col <- "SHOTISGOAL"

ctrl <- rfeControl(
  functions = rf,  # Anvender Random Forest som model
  method = "cv",       # Krydsvalidering
  number = 5           # 5-fold krydsvalidering
)


# Få sandsynligheder i stedet for direkte klassifikation
rf_probs <- predict(rf, type = "prob")
rf_probs_test <- predict(rf, newdata = test,type = "prob")

roc_curve <- roc(training$SHOTISGOAL, rf_probs[,2])
plot(roc_curve)

# opg.1.6
shot_2325 <- season_2325 %>% 
  filter(PRIMARYTYPE == "shot")

opg.1.6 <- shot_2325 %>%
  rowwise() %>%  
  mutate(
    distance_to_goal = sqrt((LOCATIONX - 100)^2 + (LOCATIONY - 50)^2),
    is_foot = as.factor(ifelse(SHOTBODYPART %in% c("right_foot", "left_foot"), 1, 0)),
    angle_to_goal = calculate_angle_degrees(LOCATIONX, LOCATIONY, 100, 44.25, 100, 55.75)
  ) %>%
  ungroup()  

opg.1.6 <- opg.1.6 %>% 
  left_join(
    players %>% select(PLAYER_WYID, FOOT, BIRTHAREA_WYID),
    by = "PLAYER_WYID",
    relationship = "many-to-many"
  )
opg.1.6 <- opg.1.6 %>% 
  left_join(
    matchformation %>% select(PLAYER_WYID, PLAYERPOSITION, MATCH_WYID),
    by = c("MATCH_WYID", "PLAYER_WYID"),
    relationship = "many-to-many"
  )
opg.1.6 <- opg.1.6 %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

opg.1.6 <- opg.1.6 %>% 
  mutate(
    is_main_foot = case_when(
      FOOT == "right" & SHOTBODYPART == "right_foot" ~ 1,
      FOOT == "left" & SHOTBODYPART == "left_foot" ~ 1,
      FOOT == "both" & SHOTBODYPART %in% c("left_foot", "right_foot") ~ 1,      
      TRUE ~ 0
    )
  )

predict_1.6 <- predict(glm, newdata = opg.1.6,  type = "response")

roc_curve_1.6 <- roc(opg.1.6$SHOTISGOAL, predict_1.6)
plot(roc_curve_1.6, print.auc=TRUE, auc.polygon=TRUE, grid=TRUE)

preds_1.6 <- ifelse(predict_1.6 > 0.3, 1, 0)
table(preds_1.6,opg.1.6$SHOTISGOAL)

# kort analyse for afstand og vinkel

afstand_analyse <- topg.1 %>%
  mutate(afstand_gruppe = cut(distance_to_goal, breaks = seq(0, 70, by = 5)),
         SHOTISGOAL_num = as.numeric(as.character(SHOTISGOAL))) %>%
  group_by(afstand_gruppe) %>%
  summarise(
    antal_skud = n(),
    antal_mål = sum(SHOTISGOAL_num),
    faktisk_målrate = antal_mål / antal_skud,
    xG = mean(SHOTXG)
  ) %>% 
  mutate(across(c(faktisk_målrate, xG), ~round(., digits = 3)))

# Tilsvarende for vinkel
vinkel_analyse <- topg.1 %>%
  mutate(vinkel_gruppe = cut(angle_to_goal, breaks = seq(0, 180, by = 15)),
         SHOTISGOAL_num = as.numeric(as.character(SHOTISGOAL))) %>%
  group_by(vinkel_gruppe) %>%
  summarise(
    antal_skud = n(),
    antal_mål = sum(SHOTISGOAL_num),
    faktisk_målrate = antal_mål / antal_skud,
    xG = mean(SHOTXG)
  )%>% 
  mutate(across(c(faktisk_målrate, xG), ~round(., digits = 3)))

####### plot til 1.6
pitch_grid <- expand.grid(
  x = seq(0, 100, length.out = 100),
  y = seq(0, 100, length.out = 50)
)

# Tilføj afstand til mål (antager at målet er ved x = 100, y = 50)
pitch_grid <- pitch_grid %>%
  mutate(
    distance_to_goal = sqrt((x - 100)^2 + (y - 50)^2),
    afstand_gruppe = cut(distance_to_goal, breaks = seq(0, 70, by = 5))
  )

# Tilføj målrate fra afstand_analyse dataframe
pitch_grid <- pitch_grid %>%
  left_join(afstand_analyse, by = "afstand_gruppe") %>%
  mutate(faktisk_målrate = ifelse(is.na(faktisk_målrate), 0, faktisk_målrate))

# Lav plottet
ggplot(pitch_grid, aes(x = x, y = y, fill = faktisk_målrate)) +
  geom_tile(alpha = 0.7) +
  # Banen
  geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0), color = "white", size = 1) +
  geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100), color = "white", size = 1) +
  geom_segment(aes(x = 100, y = 100, xend = 0, yend = 100), color = "white", size = 1) +
  geom_segment(aes(x = 0, y = 100, xend = 0, yend = 0), color = "white", size = 1) +
  # Straffesparksfeltet
  geom_segment(aes(x = 83, y = 21.5, xend = 100, yend = 21.5), color = "white", size = 1) +
  geom_segment(aes(x = 83, y = 78.5, xend = 100, yend = 78.5), color = "white", size = 1) +
  geom_segment(aes(x = 83, y = 21.5, xend = 83, yend = 78.5), color = "white", size = 1) +
  # Målet
  geom_segment(aes(x = 100, y = 45, xend = 100, yend = 55), color = "white", size = 2) +
  # Opdateret farveskala med højere maksimumværdi
  scale_fill_gradientn(
    colors = c("darkgreen", "green", "yellow", "orange", "red", "darkred"),
    values = scales::rescale(c(0, 0.03, 0.05, 0.1, 0.2, 0.3, 0.6)),
    limits = c(0, 0.6),  # Nu op til 60%
    name = "Målrate"
  ) +
  # Tilføj tekst for forskellige sandsynligheder
  annotate("text", x = 93, y = 50, label = "30%", color = "white", size = 4, fontface = "bold") +
  annotate("text", x = 85, y = 50, label = "20%", color = "white", size = 4, fontface = "bold") +
  annotate("text", x = 75, y = 50, label = "10%", color = "white", size = 4, fontface = "bold") +
  annotate("text", x = 65, y = 50, label = "5%", color = "white", size = 4, fontface = "bold") +
  annotate("text", x = 55, y = 50, label = "3%", color = "white", size = 4, fontface = "bold") +
  annotate("text", x = 40, y = 50, label = "<3%", color = "white", size = 4, fontface = "bold") +
  # Tilføj label for højere målrater tæt på målet
  annotate("text", x = 98, y = 50, label = "50%+", color = "white", size = 4, fontface = "bold") +
  # Tilpas koordinatsystem
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "darkgreen")
  ) +
  labs(
    title = "Expected Goals (xG) baseret på afstand til mål",
    subtitle = "Superligaen 2023/2024") +
  theme(
    plot.title = element_text(color = "white", size = 16),
    plot.subtitle = element_text(color = "white", size = 14)  )
