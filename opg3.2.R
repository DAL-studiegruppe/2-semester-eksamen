# Load libraries
library(gridExtra)
library(grid)
library(ggplot2)
library(ggsoccer)
library(dplyr)
library(tidyr)
library(DBI)
library(RMariaDB)
library(readr)
library(mongolite)
library(purrr)
library(stringr)
library(transformr)
library(gganimate)
library(gifski)

con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "EKSAMEN",
  host = "localhost",
  port = 3306,
  user = "root",
  password = "Htcsmart167349!"
)

csv_files <- list.files("/Users/tobiasfrantsen/Documents/DataAnalyse/2.semester/Eksamen/Data", 
                        pattern = "\\.csv$", 
                        full.names = TRUE)

for (file in csv_files) {
  tabel_navn <- tools::file_path_sans_ext(basename(file))
  df <- read_csv(file)
  dbWriteTable(con, tabel_navn, df, overwrite = TRUE)
  
  cat("Importeret:", tabel_navn, "\n")
}

dbListTables(con)

vbob <- read.csv('vbob.csv')

conm = mongo(
  collection = "vbob",
  db = "vbob",
  url = "mongodb://localhost"
)

conm2 = mongo(
  collection = "vbob2",
  db = "vbob",
  url = "mongodb://localhost"
)

MongoBob = conm$find(query = '{}', fields = '{}')
##### MongoBob2 = conm2$find(query = '{}', fields = '{}') #####
MongoBob2 <- read_rds("trackingdata.rds")




####################
### Freeze frame ###
####################

Goal <- MongoBob2[c(36000:38000),] # Mads Frøkjær, der er målscorer, spiller i nr 29

# Opret dataframes til brug af freeze frame
SpillerNavneUde <- MongoBob[[13]][[1]]
SpillerNavneHjemme <- MongoBob[[12]][[1]]
UdeholdScorer <- Goal[[7]][[577]]
HjemmeholdConceede <- Goal[[6]][[577]] 

# Ændre varible navn til brug af Merge
colnames(SpillerNavneUde)[colnames(SpillerNavneUde) == "ssiId"] <- "playerId"
colnames(SpillerNavneHjemme)[colnames(SpillerNavneHjemme) == "ssiId"] <- "playerId"

MergeDF <- merge(SpillerNavneUde, UdeholdScorer, by = "playerId")
MergeDF$hold <- "away" 
MergedHjemme <- merge(SpillerNavneHjemme, HjemmeholdConceede, by = "playerId")
MergedHjemme$hold <- "home"

MergeDF <- rbind(MergeDF, MergedHjemme)
MergeDF <- MergeDF[,-c(1,5,6,7,9,10)]

# Boldens koordinator
ball <- data.frame(
  name = "ball",
  number.x = "ball",
  position = "ball",
  xyz = "-38.06, -27.10, 0.19",  # Boldens xyz-koordinater
  hold = "ball"
)

MergeDF <- rbind(MergeDF, ball)

# Split xyz koordinaterne ud og gør pænt
MergeDF <- MergeDF %>%
  separate(xyz, into = c("x", "y", "z"), sep = ", ", convert = TRUE)

MergeDF$x <- gsub("c\\(", "", MergeDF$x)

# Lav variabler til numerisk
MergeDF$x <- as.numeric(MergeDF$x)
MergeDF$y <- as.numeric(MergeDF$y)

# Da koordinatsystemet er anderledes skald er justeres for dette
MergeDF$x <- MergeDF$x + 52.5  # Flytter x-koordinaterne
MergeDF$y <- MergeDF$y + 34    # Flytter y-koordinaterne

colors <- c("home" = "blue", "away" = "red", "ball" = "black")


# Opret tekst til at vise spillernes numre og navne
player_info_away <- MergeDF %>%
  filter(hold == "away") %>%
  mutate(info = paste(number.x, "-", name)) %>%
  select(info)

# specifik_spiller <- "M. Frokjaer-Jensen"

MFJ <- MergeDF %>%
  filter(number.x == 29) %>%
  select(x, y) %>%
  rename(from_x = x, from_y = y)

# Plot freeze frame
plot <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "darkgreen") +
  
  # Plot spillerne som prikker
  geom_point(data = MergeDF %>% filter(hold != "ball"),  
             aes(x = x, y = y, color = hold, shape = hold), size = 3, alpha = 0.8) +
  
  # Plot bolden
  geom_point(data = MergeDF %>% filter(hold == "ball"),  
             aes(x = x, y = y), color = "yellow", size = 2) +
  
  # Tilføj spillernumre inde i prikkerne
  geom_text(data = MergeDF %>% filter(hold != "ball"),  
            aes(x = x, y = y, label = number.x), color = "white", size = 3, fontface = "bold") +
  
  # Definer farver og former (forskellige farver til holdene)
  scale_color_manual(values = c("home" = "red", "away" = "blue")) +  # Farver til hjemme og udehold
  scale_shape_manual(values = c("home" = 16, "away" = 16)) + # Gør at formerne (cirkel) er ens for begge hold
  
  # Stil og labels
  theme_minimal() +
  labs(title = "Freeze Frame", subtitle = "Spiller- og boldpositioner") +
  coord_fixed()

# Tilføj spillernavne og numre (kun for udeholdet) som tekst ved siden af plottet
player_list_plot <- grid.arrange(plot, 
                                 textGrob(paste(player_info_away$info, collapse = "\n"), 
                                          gp = gpar(fontsize = 12)), 
                                 ncol = 2, widths = c(3, 1)) 

########################
### Covering shadows ###
########################

# 1. Find Mads Frøkjær
mads <- MergeDF %>% filter(number.x == 29) %>% select(x, y)

# 2. Find medspillere (undtagen Mads selv)
medspillere <- MergeDF %>%
  filter(hold == "away", number.x != 29) %>%
  select(x, y, number.x)

# 3. Funktion til at lave afleverings-trekanter
make_triangle <- function(mads_x, mads_y, team_x, team_y, buffer = 1) {
  dx <- team_x - mads_x
  dy <- team_y - mads_y
  length <- sqrt(dx^2 + dy^2)
  
  dx <- dx / length
  dy <- dy / length
  
  perp_dx <- -dy
  perp_dy <- dx
  
  pt1_x <- team_x + perp_dx * buffer
  pt1_y <- team_y + perp_dy * buffer
  
  pt2_x <- team_x - perp_dx * buffer
  pt2_y <- team_y - perp_dy * buffer
  
  triangle_df <- data.frame(
    x = c(mads_x, pt1_x, pt2_x),
    y = c(mads_y, pt1_y, pt2_y),
    group = paste0("to_", team_x, "_", team_y)
  )
  
  return(triangle_df)
}

# 4. Lav alle trekanter
triangle_list <- medspillere %>%
  rowwise() %>%
  mutate(triangle = list(make_triangle(mads$x, mads$y, x, y))) %>%
  mutate(number = number.x) %>%
  pull(triangle, name = number)

all_triangles <- bind_rows(triangle_list, .id = "receiver_number")

# 5. Split trekanter i to grupper: én til nr. 7 (highlight), resten normale
highlight_triangle <- all_triangles %>% filter(receiver_number == "7")
other_triangles <- all_triangles %>% filter(receiver_number != "7")

# 6. Lav linjer fra Mads til medspillere
linjer <- medspillere %>%
  mutate(from_x = mads$x,
         from_y = mads$y)

# 7. Split linjer: highlight + andre
highlight_line <- linjer %>% filter(number.x == 7)
other_lines <- linjer %>% filter(number.x != 7)

# 8. Plot
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "darkgreen") +
  
  # Trekanter - almindelige (grå)
  geom_polygon(data = other_triangles, aes(x = x, y = y, group = group),
               fill = "gray60", alpha = 0.5, color = "black") +
  
  # Trekant til nr. 7 - highlight (grøn)
  geom_polygon(data = highlight_triangle, aes(x = x, y = y, group = group),
               fill = "green", alpha = 0.5, color = "black") +
  
  # Linjer - grå
  geom_segment(data = other_lines,
               aes(x = from_x, y = from_y, xend = x, yend = y),
               color = "gray80", linewidth = 0.7, linetype = "solid") +
  
  # Linje til nr. 7 - grøn
  geom_segment(data = highlight_line,
               aes(x = from_x, y = from_y, xend = x, yend = y),
               color = "lightgreen", linewidth = 1.2) +
  
  # Spillere
  geom_point(data = MergeDF %>% filter(hold != "ball"),  
             aes(x = x, y = y, color = hold), size = 3) +
  
  # Bold
  geom_point(data = MergeDF %>% filter(hold == "ball"),  
             aes(x = x, y = y), color = "yellow", size = 2) +
  
  # Spillernumre
  geom_text(data = MergeDF %>% filter(hold != "ball"),  
            aes(x = x, y = y, label = number.x), color = "white", size = 3, fontface = "bold") +
  
  scale_color_manual(values = c("home" = "red", "away" = "blue")) +
  coord_fixed() +
  labs(title = "Covering shadows fra Mads Frøkjær-Jensen",
       subtitle = "Mest progression er medspiller nr. 7")





####################
####### GIF ########
####################

# Udvælg frames
GIF <- MongoBob2[36068:38000,]
GIF <- GIF[,-c(1,2,4,5,9:11)]

# Fold lister ud
GIF_unfolded_rows <- GIF %>%
  unnest(awayPlayers, names_sep = "_")

GIF_unfolded_rows <- GIF_unfolded_rows[,-2]

GIF_unfolded_rows_home <- GIF %>%
  unnest(homePlayers, names_sep = "_")

GIF_unfolded_rows_home <- GIF_unfolded_rows_home[,-7]



# Træk XYZ-koordinaterne ud for hjemme- og ude-spillere samt bolden
GIF_unfolded_rows <- GIF_unfolded_rows %>%
  unnest_wider(awayPlayers_xyz, names_sep = "_")

GIF_unfolded_rows_home <- GIF_unfolded_rows_home %>%
  unnest_wider(homePlayers_xyz, names_sep = "_")

# Udtræk XYZ-koordinaterne fra 'ball$xyz' og opret separate kolonner for X, Y og Z
GIF_unfolded_rows <- GIF_unfolded_rows %>%
  mutate(
    ball_xyz = lapply(ball$xyz, unlist),  # Konverterer hver liste i 'ball$xyz' til en vektor
    ball_x = sapply(ball_xyz, `[`, 1),  # Ekstraher X-koordinat (første værdi)
    ball_y = sapply(ball_xyz, `[`, 2),  # Ekstraher Y-koordinat (anden værdi)
    ball_z = sapply(ball_xyz, `[`, 3)   # Ekstraher Z-koordinat (tredje værdi)
  )

# Bind dataframes sammen
GIF_samlet <- cbind(GIF_unfolded_rows, GIF_unfolded_rows_home)

# Slet ubrugelige kolonner
GIF_samlet <- GIF_samlet[,-c(2, 6, 7, 8, 9, 10, 13, 14, 15, 19, 20, 21, 22)]

# Ændring af kolonnenavne så det giver mere mening
colnames(GIF_samlet) <- c(
  "frameIdx",           # Behold frameIdx
  "awayPlayers_number", # Behold awayPlayers_number
  "away_x",             # Ændrer awayPlayers_xyz_1 til away_x
  "away_y",             # Ændrer awayPlayers_xyz_2 til away_y
  "ball_x",             # Behold ball_x
  "ball_y",             # Behold ball_y
  "homePlayers_number", # Behold homePlayers_number
  "home_x",             # Ændrer homePlayers_xyz_1 til home_x
  "home_y"              # Ændrer homePlayers_xyz_2 til home_y
)

# Juster koordinater
GIF_samlet$away_x <- GIF_samlet$away_x + 52.5
GIF_samlet$away_y <- GIF_samlet$away_y + 34    
GIF_samlet$home_x <- GIF_samlet$home_x + 52.5
GIF_samlet$home_y <- GIF_samlet$home_y + 34  

# Kunne ikke få lov til at ændre ball til numerisk, så endte med at skulle køre denne kode?
GIF_samlet$ball_x <- map(GIF_samlet$ball_x, ~ if (length(.x) > 0) .x[1] else NA_real_)
GIF_samlet$ball_y <- map(GIF_samlet$ball_y, ~ if (length(.x) > 0) .x[1] else NA_real_)

GIF_samlet$ball_x <- as.numeric(GIF_samlet$ball_x)
GIF_samlet$ball_y <- as.numeric(GIF_samlet$ball_y)

GIF_samlet$ball_x <- GIF_samlet$ball_x + 52.5 
GIF_samlet$ball_y <- GIF_samlet$ball_y + 34 

# Slet NA-værdier, ellers kan plottet ikke virke
GIF_samlet <- na.omit(GIF_samlet)

GIF_samlet <- GIF_samlet[-c(1:913),]

# Lav plot
p <- ggplot(GIF_samlet, aes(x = ball_x, y = ball_y)) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "black", fill = "darkgreen") +  # Brug annotate_pitch til at tegne fodboldbanen
  geom_point(aes(color = "Ball"), size = 5) +  # Plot boldens position
  geom_point(aes(x = away_x, y = away_y, color = "Away Players"), size = 3) +  # Plot ude-spillernes positioner
  geom_point(aes(x = home_x, y = home_y, color = "Home Players"), size = 3) +  # Plot hjemme-spillernes positioner
  labs(title = 'Frame: {frame_time}', x = 'X Coordinate', y = 'Y Coordinate') +
  theme_minimal() +
  scale_color_manual(values = c("Ball" = "red", "Away Players" = "blue", "Home Players" = "green")) +
  transition_time(frameIdx) +  # Animation over tid baseret på frameIdx
  ease_aes('linear')  # Brug lineær bevægelse for glat overgang

# Generér animation
anim <- animate(p, nframes = length(unique(GIF_samlet$frameIdx)), width = 1000, height = 600, fps = 25)


# Gem animationen og send til google drive, så vi kan få et link der virker
anim_save("game_motion.gif", animation = anim)

anim









