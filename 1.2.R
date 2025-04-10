# beskrivende statistik med visualisering

# aftand til mål
goals_only <- opg.1 %>% 
  filter(SHOTISGOAL == 1)

# Beregn gennemsnitsafstand for målene
avg_goal_distance <- mean(goals_only$distance_to_goal, na.rm = TRUE)

# Simpel visualisering af kun mål
p1 <- ggplot() +
  # Tilføj fodboldbanen
  annotate_pitch(fill = "springgreen4", colour = "white") +
  
  # Tilføj kun de succesfulde mål
  geom_point(data = goals_only, 
             aes(x = LOCATIONX, y = LOCATIONY),
             color = "blue", alpha = 0.8, size = 3) +
  
  # Tilføj simpel tekst om gennemsnitsafstand
  annotate("text", x = 20, y = 10, 
           label = paste("Gennemsnitsafstand til mål:", 
                         round(avg_goal_distance, 1), "m"),
           color = "white", fontface = "bold", size = 4.5) +
  
  # Vend banen rigtigt
  coord_flip(xlim = c(0, 100), ylim = c(0, 100)) +
  
  # Simpel titel
  labs(title = "Succesfulde mål",
       subtitle = "Blå punkter viser placeringer af alle scorede mål") +
  
  # Ryd op i temaet
  theme_pitch() +
  theme(
    panel.background = element_rect(fill = "springgreen4"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
p1

# angle_to_goal

# is_foot
goals_by_bodypart <- opg.1 %>%
  filter(SHOTISGOAL == 1) %>%
  group_by(is_foot) %>%
  summarise(
    antal_mål = n(),
    .groups = "drop"
  ) %>%
  mutate(
    kropsdel = ifelse(is_foot == 1, "Fod", "Anden kropsdel"),
    procent = antal_mål / sum(antal_mål) * 100
  )

# Barplot med procent
p3 <- ggplot(goals_by_bodypart, aes(x = kropsdel, y = procent, fill = kropsdel)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, fontface = "bold") +
  
  geom_text(aes(label = paste0("(", antal_mål, " mål)"), y = procent - 8), 
            color = "white", size = 4) +
  
  scale_fill_manual(values = c("Fod" = "blue", "Anden kropsdel" = "darkred")) +
  
  labs(title = "78,2% af alle mål er scoret med fod",
       subtitle = "Sammenligning af mål scoret med fod vs. andre kropsdele",
       x = "",
       y = "Procent (%)") +
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
p3

# is_main_foot
goals_by_mainfoot <- opg.1 %>%
  filter(SHOTISGOAL == 1) %>%
  group_by(is_main_foot) %>%
  summarise(
    antal_mål = n(),
    .groups = "drop"
  ) %>%
  mutate(
    kropsdel = ifelse(is_main_foot == 1, "Dominerende fod", "Anden fod eller kropsdel"),
    procent = antal_mål / sum(antal_mål) * 100
  )

# Barplot med procent
p4 <- ggplot(goals_by_mainfoot, aes(x = kropsdel, y = procent, fill = kropsdel)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, fontface = "bold") +
  
  geom_text(aes(label = paste0("(", antal_mål, " mål)"), y = procent - 8), 
            color = "white", size = 4) +
  
  scale_fill_manual(values = c("Dominerende fod" = "blue", "Anden fod eller kropsdel" = "darkred")) +
  
  labs(title = "59% af alle mål er scoret med dominerende fod",
       subtitle = "Sammenligning af mål scoret med dominerende fod vs. anden",
       x = "",
       y = "Procent (%)") +
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
p4

# spillerens position
position_mapping <- c("1" = "Angriber", "2" = "Midtbane", "3" = "Forsvar")

goals_by_position_group <- opg.1 %>%
  filter(SHOTISGOAL == 1) %>%
  # Konvertér player_position til de beskrivende navne
  mutate(position_group = position_mapping[as.character(player_position)]) %>%
  # Sørg for at position_group har den rigtige rækkefølge
  mutate(position_group = factor(position_group, 
                                 levels = c("Forsvar", "Midtbane", "Angriber"))) %>%
  group_by(position_group) %>%
  summarise(
    antal_mål = n(),
    .groups = "drop"
  ) %>%
  mutate(procent = antal_mål / sum(antal_mål) * 100)

# Barplot
p5 <- ggplot(goals_by_position_group, aes(x = position_group, y = antal_mål, fill = position_group)) +
  # Tilføj søjler
  geom_bar(stat = "identity", width = 0.7) +
  
  # Tilføj antal på søjlerne
  geom_text(aes(label = antal_mål), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, fontface = "bold") +
  
  # Tilføj procent under søjlerne
  geom_text(aes(label = paste0(round(procent, 1), "%"), y = antal_mål + max(antal_mål)*0.05), 
            color = "black", size = 4) +
  
  # Farver for de forskellige grupper
  scale_fill_manual(values = c(
    "Angriber" = "firebrick", 
    "Midtbane" = "goldenrod", 
    "Forsvar" = "darkblue"
  )) +
  
  # Titler og labels
  labs(title = "Angriber scoret 60% af mål",
       subtitle = "Angriber (1), Midtbane (2), Forsvar (3)",
       x = "",
       y = "Antal mål") +
  
  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
p5

# counter attack
counter_goals <- opg.1 %>%
  filter(SHOTISGOAL == 1) %>%
  group_by(counter_attack) %>%
  summarise(
    antal_mål = n(),
    .groups = "drop"
  ) %>%
  mutate(
    angrebstype = ifelse(counter_attack == 1, "Kontraangreb", "Andre angrebstyper"),
    procent = antal_mål / sum(antal_mål) * 100
  )

# Barplot for kontraangreb
p6 <- ggplot(counter_goals, aes(x = angrebstype, y = procent, fill = angrebstype)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, fontface = "bold") +
  
  geom_text(aes(label = paste0("(", antal_mål, " mål)"), y = procent - 8), 
            color = "white", size = 4) +
  
  scale_fill_manual(values = c("Kontraangreb" = "darkgreen", "Andre angrebstyper" = "darkorange")) +
  
  labs(title = paste0(round(counter_goals$procent[counter_goals$counter_attack == 1], 0), 
                      "% af alle mål er scoret på kontraangreb"),
       subtitle = "Sammenligning af mål scoret på kontraangreb vs. andre angrebstyper",
       x = "",
       y = "Procent (%)") +
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

p6

# set pieces
setpiece_goals <- opg.1 %>%
  filter(SHOTISGOAL == 1) %>%
  group_by(set_pieces) %>%
  summarise(
    antal_mål = n(),
    .groups = "drop"
  ) %>%
  # Tilføj en mere beskrivende label
  mutate(
    situationstype = ifelse(set_pieces == 1, "Dødbolde", "Åbent spil"),
    procent = antal_mål / sum(antal_mål) * 100
  )

# Barplot for dødbolde
p7 <- ggplot(setpiece_goals, aes(x = situationstype, y = procent, fill = situationstype)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, fontface = "bold") +
  
  geom_text(aes(label = paste0("(", antal_mål, " mål)"), y = procent - 8), 
            color = "white", size = 4) +
  
  scale_fill_manual(values = c("Dødbolde" = "darkblue", "Åbent spil" = "darkred")) +
  
  labs(title = paste0(round(setpiece_goals$procent[setpiece_goals$set_pieces == 1], 0), 
                      "% af alle mål er scoret på dødbolde"),
       subtitle = "Sammenligning af mål scoret på dødbolde vs. åbent spil",
       x = "",
       y = "Procent (%)") +
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
p7

# is shot by dribble
dribble_goals <- opg.1 %>%
  filter(SHOTISGOAL == 1) %>%
  group_by(is_shot_by_dribble) %>%
  summarise(
    antal_mål = n(),
    .groups = "drop"
  ) %>%
  mutate(
    skudtype = ifelse(is_shot_by_dribble == 1, "Efter dribling", "Uden dribling"),
    procent = antal_mål / sum(antal_mål) * 100
  )

# Plot i præcis samme stil som eksemplet
p8 <- ggplot(dribble_goals, aes(x = skudtype, y = procent, fill = skudtype)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 6, fontface = "bold") +
  
  geom_text(aes(label = paste0("(", antal_mål, " mål)"), y = procent - 8), 
            color = "white", size = 4) +
  
  scale_fill_manual(values = c("Efter dribling" = "purple", "Uden dribling" = "darkcyan")) +
  
  labs(title = paste0(round(dribble_goals$procent[dribble_goals$is_shot_by_dribble == 1], 0), 
                      "% af alle mål er scoret efter dribling"),
       subtitle = "Sammenligning af mål scoret efter dribling vs. uden dribling",
       x = "",
       y = "Procent (%)") +
  
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
p8
