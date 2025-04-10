# hente allevents og filter
# 2.1
library(DBI)
library(RMariaDB)
library(readr)
library(dplyr)
library(rpart)

#con <- dbConnect(
  #RMariaDB::MariaDB(),
  #dbname = "Eksamen",
  #host = "localhost",
  #port = 3306,
  #user = "root",
  #password = "psw"
#)

#opg.2 <- dbReadTable(con, "opg2")
#team <- dbReadTable(con, "teams")

#saveRDS(opg.2,"opg.2.RDS")
#saveRDS(team,"opg.2.team.RDS")

opg.2 <- readRDS("opg.2.RDS")
team <- read_rds("opg.2.team.RDS")
xg_list <- strsplit(opg.2$xG_values, ",")


opg.2 <- opg.2 %>% 
  mutate(
    sum_xg = sapply(xg_list, function(x) sum(as.numeric(x)))
  )

opg.2 <- opg.2 %>% 
  left_join(
    team %>% select(TEAM_WYID, TEAMNAME),
    by = "TEAM_WYID",
    relationship = "many-to-many"
  )

opg.2 <- opg.2 %>%
  distinct(MATCH_WYID, TEAM_WYID, .keep_all = TRUE)

opg.2 <- opg.2[,-5]

# trin for trin, vi beregne kampe manual først.
# starte med beregne sandsynlighed for mål
# vi antager Midtjylland mod Hvidovre
# beregne sandsynlighed for begge hold at scorer 0, 1, 2, ..., max_goal
max_goal <- 5
xgmj <- 2.5649460
xghv <- 0.2304520

mj <- numeric(max_goal + 1)
for (m in 0:max_goal) {
  mj[m+1] <- dpois(m,xgmj)
}

hv <- numeric(max_goal + 1)
for (m in 0:max_goal) {
  hv[m+1] <- dpois(m,xghv)
}

# gem resultater i matrix
result_matrix <- outer(mj, hv)
rownames(result_matrix) <- 0:max_goal  # Midtjylland mål
colnames(result_matrix) <- 0:max_goal  # Hvidovre mål

# beregn sandsynlighed for sejr, uafgjort og tab
# lower.tri(result_matrix) skaber en boolsk (TRUE/FALSE) matrix, hvor elementerne under diagonalen er TRUE (dvs. hvor Midtjylland scorer flere mål end Hvidovre)
# upper.tri(result_matrix) skaber en boolsk matrix, hvor elementerne over diagonalen er TRUE (dvs. hvor Midtjylland scorer færre mål end Hvidovre)
# diag(result_matrix) trækker diagonalen ud af matricen (dvs. hvor begge hold scorer lige mange mål)
p_win_mj <- sum(result_matrix[lower.tri(result_matrix)])
p_draw_mj <- sum(diag(result_matrix))
p_loss_mj <- sum(result_matrix[upper.tri(result_matrix)])

# xP for fc.midtjylland
xP_mj <- 3 * p_win_mj + 1 * p_draw_mj + 0 * p_loss_mj

# xP  for hvidovre
p_win_hv <- p_loss_mj               
p_draw_hv <- p_draw_mj         
p_loss_hv <- p_win_mj   

xP_hvidovre <- 3 * p_win_hv + 1 * p_draw_hv + 0 * p_loss_hv
# resultater viser at hvidovre har en xp tæt på 0 og midtjylland tæt på 3 dvs. man forventer hvidorve taber kampen mod midtjylland

# nu skal vi lave en store loop der kan køre igennem alle kamper brøndby har spillet i sæson 23/24
# første laver vi en funktion der kan beregne xP

beregne_xP <- function(xg_team, xg_modstand, max_goal=5){
  team_probs <- dpois(0:max_goal, xg_team)
  modstand_probs <- dpois(0:max_goal, xg_modstand)
  result_matrix <- outer(team_probs, modstand_probs)
  p_win <- sum(result_matrix[lower.tri(result_matrix)])   
  p_draw <- sum(diag(result_matrix))                      
  p_loss <- sum(result_matrix[upper.tri(result_matrix)])
  xP <- 3 * p_win + 1 * p_draw + 0 * p_loss
  return(list(
    xP = xP,
    p_win = p_win,
    p_draw = p_draw,
    p_loss = p_loss
  ))
}

beregne_xP(1.1289900,2.3526190)

# nu laver vi en loop der kan køre gennem alle brøndby kamper

beregn_xP_for_hold <- function(data, hold_navn) {
  # Find holdets ID
  hold_id <- unique(data$TEAM_WYID[data$TEAMNAME == hold_navn])
  
  # Opret en tom dataframe til at gemme resultaterne
  resultater <- data.frame(
    kamp_id = numeric(),
    modstander = character(),
    hold_xg = numeric(),
    modstander_xg = numeric(),
    xP = numeric(),
    p_win = numeric(),
    p_draw = numeric(),
    p_loss = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Find alle unikke kampid'er hvor holdet har spillet
  kamp_ids <- unique(data$MATCH_WYID[data$TEAM_WYID == hold_id | data$OPPONENTTEAM_WYID == hold_id])
  
  # Løkke gennem alle kampe
  for (kamp_id in kamp_ids) {
    kamp_data <- data[data$MATCH_WYID == kamp_id, ]
    
    # Hold data
    hold_række <- kamp_data[kamp_data$TEAM_WYID == hold_id, ]
    modstander_række <- kamp_data[kamp_data$TEAM_WYID != hold_id, ]
    
    # Hent xG-værdier
    hold_xg <- hold_række$sum_xg
    modstander_xg <- modstander_række$sum_xg
    modstander_navn <- modstander_række$TEAMNAME
    
    # Beregn xP
    resultat <- beregne_xP(hold_xg, modstander_xg)
    
    # Tilføj til resultattabel
    resultater <- rbind(resultater, data.frame(
      kamp_id = kamp_id,
      modstander = modstander_navn,
      hold_xg = hold_xg,
      modstander_xg = modstander_xg,
      xP = resultat$xP,
      p_win = resultat$p_win,
      p_draw = resultat$p_draw,
      p_loss = resultat$p_loss
    ))
  }
  return(resultater)
}

brøndby <- beregn_xP_for_hold(opg.2,"Brøndby")
midtjylland <- beregn_xP_for_hold(opg.2,"Midtjylland")
vejle <- beregn_xP_for_hold(opg.2,"Vejle")
fck <- beregn_xP_for_hold(opg.2,"København")
nj <- beregn_xP_for_hold(opg.2,"Nordsjælland")
AGF <- beregn_xP_for_hold(opg.2,"AGF")
silkeborg <- beregn_xP_for_hold(opg.2,"Silkeborg")

sum(brøndby$xP)
sum(midtjylland$xP)
sum(vejle$xP)
sum(fck$xP)
sum(nj$xP)
sum(AGF$xP)
sum(silkeborg$xP)
