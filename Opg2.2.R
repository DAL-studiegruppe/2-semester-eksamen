library(tidyverse)
#Retrieve
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "Eksamen",
  host = "localhost",
  port = 3306,
  user = "root",
  password = ""
)
brøndby <- dbReadTable(con,"matchevents_shots")
TeamMatch <- dbGetQuery(con, "SELECT * FROM teammatches")
dbDisconnect(con)
unique(opg.1$SHOTISGOAL)
#--------------------------------------------------------------------------#

brøndbyVS <- allevents %>% 
  filter(MATCH_WYID %in% c("5466044", "5466032", "5466040"))



BIFSIF <- 5466044
BIFVB <- 5466032
BIFVFF <- 5466040

Vejle <- 7473
Silkeborg <- 7461
Viborg <- 7456

# Filter for de ønskede kampe og summer data pr. hold
result_df <- allevents %>%
  # Filtrér for de ønskede kampe
  filter(MATCH_WYID %in% c("5466044", "5466032", "5466040")) %>%
  
  # Gruppér data efter kamp og hold
  group_by(MATCH_WYID, TEAM_WYID) %>%
  
  # Tilføj holdnavne baseret på TEAM_WYID
  mutate(
    team_name = case_when(
      TEAM_WYID == 7453 ~ "Brøndby",
      TEAM_WYID == 7473 ~ "Vejle",
      TEAM_WYID == 7461 ~ "Silkeborg",
      TEAM_WYID == 7456 ~ "Viborg",
      TRUE ~ "Unknown" # Tilfælde for ukendte TEAM_WYID
    )
  ) %>%
  # Gruppér data efter kamp og hold
  group_by(MATCH_WYID, TEAM_WYID, team_name) %>%
  # Opsummer data for hvert hold
  summarise(
    total_goals_penalty = sum(PRIMARYTYPE == "penalty", na.rm = TRUE),
    total_shots = sum(PRIMARYTYPE == "shot", na.rm = TRUE),
    offside = sum(PRIMARYTYPE == "offside", na.rm = TRUE),
    total_goals = sum(SHOTISGOAL == 1, na.rm = TRUE),
    .groups = "drop" # Fjern grupperingsstrukturen efter summarise
  )

# Se resultatet
print(result_df)
