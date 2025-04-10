#Feautere Creation
field_length_m <- 105
field_width_m <- 68

#Stolpeafstand i meter (mål er 7.32 m bredt også i Superliga)
goal_width <- 7.32

#Målets midte
goal_y_center_m <- 50 / 100 * field_width_m

#Stolper i meter
left_post_y <- goal_y_center_m - goal_width / 2
right_post_y <- goal_y_center_m + goal_width / 2
goal_x_m <- 100 / 100 * field_length_m  # altid 100%

#Beregning
xg_data <- xg_data %>%
  mutate(
    x_m = LOCATIONX / 100 * field_length_m,
    y_m = LOCATIONY / 100 * field_width_m,
    
    distance_to_goal = sqrt((x_m - goal_x_m)^2 + (y_m - goal_y_center_m)^2),
    
    angle_to_goal = atan2(right_post_y - y_m, goal_x_m - x_m) -
      atan2(left_post_y - y_m, goal_x_m - x_m),
    angle_to_goal = angle_to_goal * (180 / pi)  # konverter til grader
  )