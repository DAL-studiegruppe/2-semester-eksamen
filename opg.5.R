library(StatsBombR)
library(dplyr)
library(ggsoccer)
library(ggplot2)
source("util.R")

# træk dataer for kvinde vm kamper
#comb <- FreeCompetitions()
#FComp <- FreeCompetitions() %>% 
  #filter(competition_id %in% c(72), season_id %in% c(107))
#Matches <- FreeMatches(FComp)
#VM_kvinde <- free_allevents(MatchesDF = Matches, Parallel = TRUE)
#VM_kvinde <- allclean(VM_kvinde)
#saveRDS(VM_kvinde, file = "VM_kvinde.RDS")

VM_kvinde <- readRDS("VM_kvinde.RDS") 

# træk dataer for mænde vm kamper
#FCcomp_mand <- FreeCompetitions() %>% 
  #filter(competition_id %in% c(43), season_id %in% c(106))
#Matches_mand <- FreeMatches(FCcomp_mand)
#VM_mand <- free_allevents(MatchesDF = Matches_mand, Parallel = TRUE)
#VM_mand <- allclean(VM_mand)
#saveRDS(VM_mand, file = "VM_mand.RDS")

VM_mand <- readRDS("VM_mand.RDS") 

# 5.1
# Starte med skud
{
# Alle
længde_K <- mean(VM_kvinde$pass.length, na.rm = TRUE)
længde_M <- mean(VM_mand$pass.length, na.rm = TRUE)
  
# Målmænd
mean(VM_kvinde$pass.length[VM_kvinde$position.name == "Goalkeeper"], na.rm = TRUE)
mean(VM_mand$pass.length[VM_mand$position.name == "Goalkeeper"], na.rm = TRUE)
  
# Uden målmænd
mean(VM_kvinde$pass.length[VM_kvinde$position.name != "Goalkeeper"], na.rm = TRUE)
mean(VM_mand$pass.length[VM_mand$position.name != "Goalkeeper"], na.rm = TRUE)
  
# pct. mål - KVINDER
antal_mål <- sum(VM_kvinde$shot.outcome.name %in% c("Goal"))
antal_skud <- sum(!is.na(VM_kvinde$shot.outcome.name))
pct_mål <- antal_mål / antal_skud * 100
  
# pct. mål - MÆND
antal_mål_M <- sum(VM_mand$shot.outcome.name %in% c("Goal"))
antal_skud_M <- sum(!is.na(VM_mand$shot.outcome.name))
pct_mål_M <- antal_mål_M / antal_skud_M * 100
  
# Skud længde
skud_K <- VM_kvinde %>%
  filter(!is.na(shot.outcome.name)) %>% 
  select(location.x,
          location.y)
  
skud_K <- skud_K %>% 
   mutate(
    distance_to_goal = sqrt((location.x - 120)^2 + (location.y - 40)^2)
  )
  
mean(skud_K$distance_to_goal)
  
skud_M <- VM_mand %>%
  filter(!is.na(shot.outcome.name)) %>% 
  select(location.x,
          location.y)
  
skud_M <- skud_M %>% 
  mutate(
    distance_to_goal = sqrt((location.x - 120)^2 + (location.y - 40)^2)
  )
  
mean(skud_M$distance_to_goal)
}

# Beksrivende statistik
type_counts_C <- table(VM_kvinde$type.name)
type_counts_C <- as.data.frame(type_counts_C)

type_counts_A <- table(VM_mand$type.name)
type_counts_A <- as.data.frame(type_counts_A)

ggplot(type_counts_C, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Fordeling af event typer i VM kvindekampe",
       x = "Event Type",
       y = "Antal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Aflerveringer og missede skud
{
kvinde_P <- VM_kvinde %>% 
  group_by(match_id) %>% 
  summarise(
    fejl_afleveringer = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside"), na.rm = TRUE) + 
      sum(pass.height.name == TRUE, na.rm = TRUE),
    misset_skud = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE),
    .groups = "drop"
  )

mand_P <- VM_mand %>% 
  group_by(match_id) %>% 
  summarise(
    fejl_afleveringer = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside"), na.rm = TRUE) + 
      sum(pass.height.name == TRUE, na.rm = TRUE),
    misset_skud = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE),
    .groups = "drop"
  )

# pct for alle kampe
mand_Pp <- VM_mand %>% 
  group_by(match_id) %>% 
  summarise(
    afleverings_pct = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside")) / sum(!is.na(pass.height.name)) * 100,
    misset_skud_pct = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE) / 
      sum(!is.na(shot.outcome.name)) * 100
  )

kvinde_Pp <- VM_kvinde %>% 
  group_by(match_id) %>% 
  summarise(
    afleverings_pct = sum(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside")) / sum(!is.na(pass.height.name)) * 100,
    misset_skud_pct = sum(!is.na(shot.outcome.name) & shot.outcome.name != "Goal", na.rm = TRUE) / 
      sum(!is.na(shot.outcome.name)) * 100
  )

# pct til en samled df
mand_Pp <- mand_Pp %>%
  mutate(gender = "Mænd") %>%
  select(gender, everything(), -match_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), gender = "Mænd")

kvinde_Pp <- kvinde_Pp %>%
  mutate(gender = "Kvinder") %>%
  select(gender, everything(), -match_id) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), gender = "Kvinder")

Combined_Stats <- bind_rows(mand_Pp, kvinde_Pp)

# plot til pct
Combined_Stats_Long <- Combined_Stats %>%
  pivot_longer(cols = c(afleverings_pct, misset_skud_pct),
               names_to = "handling", values_to = "pct") %>%
  filter(!is.na(pct))

ggplot(Combined_Stats_Long, aes(x = handling, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = round(pct, 2)),  
            position = position_dodge(width = 0.9),
            vjust = -0.2,
            size = 5) +
  labs(title = "Kvinder laver flere fejl afleveringer, mens mænd derimod misser flere skud",
       subtitle = "Data fra VM-kampe",
       x = "Handling",
       y = "Procent",
       fill = "Køn") +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  scale_x_discrete(labels = c(
    "afleverings_pct" = "Fejl aflevering",
    "misset_skud_pct" = "Misset skud"
  )) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Frispark
{
kvinde_V_foul <- VM_kvinde %>% 
    group_by(match_id) %>% 
    summarise(
      foul_committed_count = sum(type.name == "Foul Committed", na.rm = TRUE),
      .groups = "drop"
    )
  
mand_V_foul <- VM_mand %>% 
    group_by(match_id) %>% 
    summarise(
      foul_committed_count = sum(type.name == "Foul Committed", na.rm = TRUE),
      .groups = "drop"
    )
  
# pct til en samled df
kvinde_V_foul <- kvinde_V_foul %>%
    mutate(gender = "Kvinder") %>%
    select(gender, everything())
  
mand_V_foul <- mand_V_foul %>%
    mutate(gender = "Mænd") %>%
    select(gender, everything())  
  
Combined_Stats_foul <- bind_rows(kvinde_V_foul, mand_V_foul)
  
Combined_Stats_foul <- Combined_Stats_foul %>%
    group_by(gender) %>%
    summarise(foul_committed_count = sum(foul_committed_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct = round((foul_committed_count / sum(foul_committed_count)) * 100, 2))
  
ggplot(Combined_Stats_foul, aes(x = gender, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Mænd begår oftere frispark",
    subtitle = "Baseret på VM-data",
    x = "Køn",
    y = "Procent"
  ) +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, lineheight = 1.2),
    plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_dodge(width = 0.7),
            vjust = -0.5)
}

# Pressure
{
kvinde_V_pres <- VM_kvinde %>% 
  group_by(match_id) %>% 
  summarise(
    pres_count = sum(type.name == "Pressure", na.rm = TRUE),
    .groups = "drop"
  )

mand_V_pres <- VM_mand %>% 
  group_by(match_id) %>% 
  summarise(
    pres_count = sum(type.name == "Pressure", na.rm = TRUE),
    .groups = "drop"
  )

# pct til en samled df
kvinde_V_pres <- kvinde_V_pres %>%
  mutate(gender = "Kvinder") %>%
  select(gender, everything())

mand_V_pres <- mand_V_pres %>%
  mutate(gender = "Mænd") %>%
  select(gender, everything())  

Combined_Stats_pres <- bind_rows(kvinde_V_pres, mand_V_pres)

Combined_Stats_pres <- Combined_Stats_pres %>%
  group_by(gender) %>%
  summarise(pres_count = sum(pres_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = round((pres_count / sum(pres_count)) * 100, 2))

ggplot(Combined_Stats_pres, aes(x = gender, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Kvinde giver mere pres på modstander",
    subtitle = "Baseret på VM-data",
    x = "Køn",
    y = "Procent"
  ) +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, lineheight = 1.2),
    plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_dodge(width = 0.7),
            vjust = -0.5)
}

# Miscontrol
{
kvinde_V_miscontrol <- VM_kvinde %>% 
  group_by(match_id) %>% 
  summarise(
    miscontrol_count = sum(type.name == "Miscontrol", na.rm = TRUE),
    .groups = "drop"
  )

mand_V_miscontrol <- VM_mand %>% 
  group_by(match_id) %>% 
  summarise(
    miscontrol_count = sum(type.name == "Miscontrol", na.rm = TRUE),
    .groups = "drop"
  )

# pct til en samled df
kvinde_V_miscontrol <- kvinde_V_miscontrol %>%
  mutate(gender = "Kvinder") %>%
  select(gender, everything())

mand_V_miscontrol <- mand_V_miscontrol %>%
  mutate(gender = "Mænd") %>%
  select(gender, everything())  

Combined_Stats_miscontrol <- bind_rows(kvinde_V_miscontrol, mand_V_miscontrol)

Combined_Stats_miscontrol <- Combined_Stats_miscontrol %>%
  group_by(gender) %>%
  summarise(miscontrol_count = sum(miscontrol_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = round((miscontrol_count / sum(miscontrol_count)) * 100, 2))

ggplot(Combined_Stats_miscontrol, aes(x = gender, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Kvinde begår oftere miscontrol",
    subtitle = "Baseret på VM-data",
    x = "Køn",
    y = "Procent"
  ) +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, lineheight = 1.2),
    plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_dodge(width = 0.7),
            vjust = -0.5)
}

# Bad behaviour
{
kvinde_V_bad <- VM_kvinde %>% 
  group_by(match_id) %>% 
  summarise(
    bad_count = sum(type.name == "Bad Behaviour", na.rm = TRUE),
    .groups = "drop"
  )

mand_V_bad <- VM_mand %>% 
  group_by(match_id) %>% 
  summarise(
    bad_count = sum(type.name == "Bad Behaviour", na.rm = TRUE),
    .groups = "drop"
  )

# pct til en samled df
kvinde_V_bad <- kvinde_V_bad %>%
  mutate(gender = "Kvinder") %>%
  select(gender, everything())

mand_V_bad <- mand_V_bad %>%
  mutate(gender = "Mænd") %>%
  select(gender, everything())  

Combined_Stats_bad <- bind_rows(kvinde_V_bad, mand_V_bad)

Combined_Stats_bad <- Combined_Stats_bad %>%
  group_by(gender) %>%
  summarise(bad_count = sum(bad_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = round((bad_count / sum(bad_count)) * 100, 2))

ggplot(Combined_Stats_bad, aes(x = gender, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Kvinde begår mindre bad behaviour",
    subtitle = "Baseret på VM-data",
    x = "Køn",
    y = "Procent"
  ) +
  scale_fill_manual(values = c("Mænd" = "#ff0000", "Kvinder" = "#034694")) +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, lineheight = 1.2),
    plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_dodge(width = 0.7),
            vjust = -0.5)
}


# 5.2
# vi skal definere sandsynlighed for score baseret på spillerens position
# vi trækker alle freezeframe fra VM for kvinde og mand
shot_m <- VM_mand %>% 
  filter(type.name == "Shot") 
shot_m <- shot_m %>%
  filter(!is.na(shot.freeze_frame)) %>%
  filter(lengths(shot.freeze_frame) > 0) %>%  
  rowwise() %>%  
  mutate(
    angle = mxangle(location),
    #dist = disttogoal(location),
    location.x = location[[1]][[1]],
    location.y = location[[2]][[1]],
    shot.end_location.x = shot.end_location[[1]][[1]],
    shot.end_location.y = shot.end_location[[2]][[1]]
  )
shot_m <- shot_m %>% 
  ungroup() %>% 
  mutate(shot_id = row_number())

shot_k <- VM_kvinde %>% 
  filter(type.name == "Shot") 
shot_k <- shot_k %>%
  filter(!is.na(shot.freeze_frame)) %>%
  filter(lengths(shot.freeze_frame) > 0) %>%  
  rowwise() %>%  
  mutate(
    angle = mxangle(location),
    #dist = disttogoal(location),
    location.x = location[[1]][[1]],
    location.y = location[[2]][[1]],
    shot.end_location.x = shot.end_location[[1]][[1]],
    shot.end_location.y = shot.end_location[[2]][[1]]
  )
shot_k <- shot_k %>% 
  ungroup() %>% 
  mutate(shot_id = row_number())


ff_k <- VM_kvinde %>%
  filter(type.name == "Shot") %>%
  filter(!is.na(shot.freeze_frame) & !sapply(shot.freeze_frame, is.null)) %>% 
  select(shot.outcome.name,shot.freeze_frame,shot.end_location,shot.statsbomb_xg)

ff_m <- VM_mand %>%
  filter(type.name == "Shot") %>%
  filter(!is.na(shot.freeze_frame) & !sapply(shot.freeze_frame, is.null)) %>% 
  select(shot.outcome.name,shot.freeze_frame,shot.end_location,shot.statsbomb_xg)

# test
{
testff <- ff_k$shot.freeze_frame[[1]]
testff <- jsonlite::flatten(testff)
testff <- testff %>% rowwise() %>% mutate(x=location[1])
testff <- testff %>% rowwise() %>% mutate(y=location[2])
  }

# træk alle freezeframe
# kvinde
{
  all_ff_k <- data.frame()
  
  ff_k <- ff_k %>%
    mutate(shot_id = row_number())
  
  for(i in 1:nrow(ff_k)) {
    current_ff <- ff_k$shot.freeze_frame[[i]]
    
    # Flatten JSON-strukturen
    current_ff <- jsonlite::flatten(current_ff)
    
    # Tilføj x og y koordinater
    current_ff <- current_ff %>% 
      rowwise() %>% 
      mutate(
        x = location[[1]],
        y = location[[2]]
      )
    
    # Tilføj shot_id for at kunne knytte freeze frame til det specifikke skud
    current_ff$shot_id <- ff_k$shot_id[i]
    current_ff$shot_res <- ff_k$shot.outcome.name[i]
    
    # Append til dataframen i stedet for at tilføje til en liste
    all_ff_k <- bind_rows(all_ff_k, current_ff)
  }
all_ff_k$DistToGoal <- NA
  
for(i in 1:nrow(all_ff_k)) {
    all_ff_k$DistToGoal[i] <- disttogoal(all_ff_k$x[i], all_ff_k$y[i])
  }
}

# mand
{
  all_ff_m <- data.frame()
  
  ff_m <- ff_m %>%
    mutate(shot_id = row_number())
  
  for(i in 1:nrow(ff_m)) {
    current_ff <- ff_m$shot.freeze_frame[[i]]
    
    # Flatten JSON-strukturen
    current_ff <- jsonlite::flatten(current_ff)
    
    # Tilføj x og y koordinater
    current_ff <- current_ff %>% 
      rowwise() %>% 
      mutate(
        x = location[[1]],
        y = location[[2]]
      )
    
    # Tilføj shot_id for at kunne knytte freeze frame til det specifikke skud
    current_ff$shot_id <- ff_m$shot_id[i]
    current_ff$shot_res <- ff_m$shot.outcome.name[i]
    
    # Append til dataframen i stedet for at tilføje til en liste
    all_ff_m <- bind_rows(all_ff_m, current_ff)
  }
all_ff_m$DistToGoal <- NA
  
for(i in 1:nrow(all_ff_m)) {
    all_ff_m$DistToGoal[i] <- disttogoal(all_ff_m$x[i], all_ff_m$y[i])
  }
}

# tjek modstander i trekant
# kvinde
{
  for (j in 1:nrow(shot_k)) {
    # Hent den aktuelle shot_id
    current_shot_id <- shot_k$shot_id[j]
    
    # Filtrer freeze frame data for denne shot_id
    current_ff <- all_ff_k %>% filter(shot_id == current_shot_id)
    
    # Filtrer for modstandere
    opponents <- current_ff %>% filter(!teammate)
    
    # Hvis der er modstandere
    if (nrow(opponents) > 0) {
      numOfOpps <- logical(nrow(opponents))
      
      for (i in 1:nrow(opponents)) {
        numOfOpps[i] <- is_opponent_inside_triangle(
          opponents[i, 'x'],
          opponents[i, 'y'],
          shot_k[j, 'location.x'],
          shot_k[j, 'location.y']
        )
      }
      
      shot_k[j, 'numops'] <- sum(numOfOpps)
    } else {
      shot_k[j, 'numops'] <- 0  
    }
  }

}

# mand
{
  for (j in 1:nrow(shot_m)) {
    # Hent den aktuelle shot_id
    current_shot_id <- shot_m$shot_id[j]
    
    # Filtrer freeze frame data for denne shot_id
    current_ff <- all_ff_m %>% filter(shot_id == current_shot_id)
    
    # Filtrer for modstandere
    opponents <- current_ff %>% filter(!teammate)
    
    # Hvis der er modstandere
    if (nrow(opponents) > 0) {
      numOfOpps <- logical(nrow(opponents))
      
      for (i in 1:nrow(opponents)) {
        numOfOpps[i] <- is_opponent_inside_triangle(
          opponents[i, 'x'],
          opponents[i, 'y'],
          shot_m[j, 'location.x'],
          shot_m[j, 'location.y']
        )
      }
      
      shot_m[j, 'numops'] <- sum(numOfOpps)
    } else {
      shot_m[j, 'numops'] <- 0  # Ingen modstandere for dette skud
    }
  }
  
}


# dataframe af shooter
shooterdfk <- shot_k %>% 
  mutate(
    x = location.x,
    y = location.y,
    numops = numops,  
    position.name = "S",
    position.id = 222,
    teammate = TRUE,
    shot_id = shot_id
  ) %>%  
  select(player.name, player.id, location, x, y, numops, position.name, position.id, teammate, shot_id, DistToGoal)

shooterdfm <- shot_m %>% 
  mutate(
  x = location.x,
  y = location.y,
  numops = numops,  
  position.name = "S",
  position.id = 222,
  teammate = TRUE,
  shot_id = shot_id
  ) %>%  
  select(player.name, player.id, location, x, y, numops, position.name, position.id, teammate, shot_id, DistToGoal)



# loop for kvinde
{
shooterdfk$ego <- F
for (j in 1:nrow(shooterdfk)) {
  # Hvis der ikke er nogen modstandere i skyttens trekant, er de ikke egoistiske
  if (shooterdfk$numops[j] <= 0) {
    shooterdfk$ego[j] <- FALSE
    next
  }
  
  # Hent den aktuelle shot_id
  current_shot_id <- shooterdfk$shot_id[j]
  
  # Filtrer freeze frame data for denne shot_id
  current_ff <- all_ff_k %>% filter(shot_id == current_shot_id)
  
  # Filtrer for holdkammerater (undtagen skytten selv)
  teammates <- current_ff %>% 
    filter(teammate==T) %>%
    filter(player.id != shooterdfk$player.id[j])
  
  # Hvis der ikke er holdkammerater, er shooter ikke egoistisk
  if (nrow(teammates) == 0) {
    shooterdfk$ego[j] <- FALSE
    next
  }
  
  # Flag til at markere om der findes mindst én holdkammerat med 2+ færre modstandere
  has_better_positioned_teammate <- FALSE
  
  for (t in 1:nrow(teammates)) {
    # Hent holdkammeratens position
    teammate_x <- teammates$x[t]
    teammate_y <- teammates$y[t]
    
    # Filtrer for modstandere
    opponents <- current_ff %>% filter(!teammate)
    
    # Hvis der er modstandere
    if (nrow(opponents) > 0) {
      teammate_num_opps <- 0
      
      for (i in 1:nrow(opponents)) {
        is_inside <- is_opponent_inside_triangle(
          opponents$x[i],
          opponents$y[i],
          teammate_x,
          teammate_y
        )
        
        if (is_inside) {
          teammate_num_opps <- teammate_num_opps + 1
        }
      }
      
      # Tjek om holdkammeraten har mindst 2 færre modstandere
      if ((shooterdfk$numops[j] - teammate_num_opps) >= 1) {
        has_better_positioned_teammate <- TRUE
        break  # Vi kan stoppe ved den første holdkammerat med 2+ færre modstandere
      }
    }
  }
  
  # Marker som egoistisk hvis mindst én holdkammerat har 2+ færre modstandere
  shooterdfk$ego[j] <- has_better_positioned_teammate
}
table(shooterdfk$ego)
}

# loop for mand
{
shooterdfm$ego <- FALSE

for (j in 1:nrow(shooterdfm)) {
  # Hvis der ikke er nogen modstandere i skyttens trekant, er de ikke egoistiske
  if (shooterdfm$numops[j] <= 0) {
    shooterdfm$ego[j] <- FALSE
    next
  }
  
  # Hent den aktuelle shot_id
  current_shot_id <- shooterdfm$shot_id[j]
  
  # Filtrer freeze frame data for denne shot_id
  current_ff <- all_ff_m %>% filter(shot_id == current_shot_id)
  
  # Filtrer for holdkammerater (undtagen skytten selv)
  teammates <- current_ff %>% 
    filter(teammate==T) %>%
    filter(player.id != shooterdfm$player.id[j])
  
  # Hvis der ikke er holdkammerater, er shooter ikke egoistisk
  if (nrow(teammates) == 0) {
    shooterdfm$ego[j] <- FALSE
    next
  }
  
  # Flag til at markere om der findes mindst én holdkammerat med 2+ færre modstandere
  has_better_positioned_teammate <- FALSE
  
  for (t in 1:nrow(teammates)) {
    # Hent holdkammeratens position
    teammate_x <- teammates$x[t]
    teammate_y <- teammates$y[t]
    
    # Filtrer for modstandere
    opponents <- current_ff %>% filter(!teammate)
    
    # Hvis der er modstandere
    if (nrow(opponents) > 0) {
      teammate_num_opps <- 0
      
      for (i in 1:nrow(opponents)) {
        is_inside <- is_opponent_inside_triangle(
          opponents$x[i],
          opponents$y[i],
          teammate_x,
          teammate_y
        )
        
        if (is_inside) {
          teammate_num_opps <- teammate_num_opps + 1
        }
      }
      
      # Tjek om holdkammeraten har mindst 1 færre modstandere
      if ((shooterdfm$numops[j] - teammate_num_opps) >= 1) {
        has_better_positioned_teammate <- TRUE
        break  # Vi kan stoppe ved den første holdkammerat med 1+ færre modstandere
      }
    }
  }
  
  # Marker som egoistisk hvis mindst én holdkammerat har 2+ færre modstandere
  shooterdfm$ego[j] <- has_better_positioned_teammate
}

table(shooterdfm$ego)
}

# plot for mand og kvind
{
egom <- data.frame(
  Kategori = c("Egoistisk", "Ikke-egoistisk"),
  Procent = c(
    sum(shooterdfm$ego) / nrow(shooterdfm) * 100,
    sum(!shooterdfm$ego) / nrow(shooterdfm) * 100
  )
)

ggplot(egom, aes(x = Kategori, y = Procent, fill = Kategori)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Egoistisk" = "#E74C3C", "Ikke-egoistisk" = "#3498DB")) +
  labs(title = "78,7% skud fra mand er egoistisk",
       x = NULL,
       y = "Procent (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.grid.major.x = element_blank())

egok <- data.frame(
  Kategori = c("Egoistisk", "Ikke-egoistisk"),
  Procent = c(
    sum(shooterdfk$ego) / nrow(shooterdfk) * 100,
    sum(!shooterdfk$ego) / nrow(shooterdfk) * 100
  )
)

ggplot(egok, aes(x = Kategori, y = Procent, fill = Kategori)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Egoistisk" = "#E74C3C", "Ikke-egoistisk" = "#3498DB")) +
  labs(title = "72,8% skud fra kvinde er egoistisk",
       x = NULL,
       y = "Procent (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.grid.major.x = element_blank())
}

# plot for ego shots
# Hent shooterdata
shooter5 <- shooterdfk %>% 
  filter(shot_id == 62)

# Hent alle spillere på banen for shot 5
players5 <- all_ff_k %>% 
  filter(shot_id == 62) %>%
  select(player.name, position.id, position.name, x, y, shot_id, teammate)

# For Shot ID 55
# Hent shooterdata
shooter7 <- shooterdfk %>% 
  filter(shot_id == 47)

# Hent alle spillere på banen for shot 55
players7 <- all_ff_k %>% 
  filter(shot_id == 47) %>%
  select(player.name, position.id, position.name, x, y, shot_id, teammate)

plot5 <- ggplot() +
  # Standardfodboldbane
  annotate_pitch(dimensions = pitch_statsbomb) +
  # Alle spillere
  geom_point(data = players5, 
             aes(x = x, y = y, fill = teammate), 
             size = 6, shape = 21, color = "white") +
  # Trøjenumre
  geom_text(data = players5,
            aes(x = x, y = y, label = position.id), 
            color = "black", size = 3, fontface = "bold") +
  # Shooter (som vi VED ikke er i players_data)
  geom_point(data = shooter5, 
             aes(x = x, y = y), 
             size = 10, shape = 21, fill = "yellow", color = "black") +
  # Shooter trøjenummer
  geom_text(data = shooter5,
            aes(x = x, y = y, label = position.id), 
            color = "black", size = 3, fontface = "bold") +
  # Pil fra shooter til mål
  geom_segment(data = shooter5,
               aes(x = x, y = y, xend = 120, yend = 40), 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "black", size = 1.2) +
  # Styling
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), 
                    name = "Hold",
                    labels = c("TRUE" = "Hold A", "FALSE" = "Hold B")) +
  theme_pitch() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  # Juster visningen til at fokusere på relevante del af banen
  coord_flip(xlim = c(90, 120), ylim = c(0, 80)) +
  ggtitle("Egoistisk Shot")

# plot 7
plot7 <- ggplot() +
  # Standardfodboldbane
  annotate_pitch(dimensions = pitch_statsbomb) +
  # Alle spillere
  geom_point(data = players7, 
             aes(x = x, y = y, fill = teammate), 
             size = 6, shape = 21, color = "white") +
  # Trøjenumre
  geom_text(data = players7,
            aes(x = x, y = y, label = position.id), 
            color = "black", size = 3, fontface = "bold") +
  # Shooter (som vi VED ikke er i players_data)
  geom_point(data = shooter7, 
             aes(x = x, y = y), 
             size = 10, shape = 21, fill = "yellow", color = "black") +
  # Shooter trøjenummer
  geom_text(data = shooter7,
            aes(x = x, y = y, label = position.id), 
            color = "black", size = 3, fontface = "bold") +
  # Pil fra shooter til mål
  geom_segment(data = shooter7,
               aes(x = x, y = y, xend = 120, yend = 40), 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "black", size = 1.2) +
  # Styling
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), 
                    name = "Hold",
                    labels = c("TRUE" = "Hold A", "FALSE" = "Hold B")) +
  theme_pitch() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  # Juster visningen til at fokusere på relevante del af banen
  coord_flip(xlim = c(90, 120), ylim = c(0, 80)) +
  ggtitle("Ikke egoistisk Shot")

# Vis plottet
print(plot5)
print(plot7)

