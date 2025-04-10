calculate_angle_degrees <- function(ax, ay, bx, by, ox, oy) {
  # Calculate vectors AO and AB
  theta_degrees = 360
  tryCatch({
    
    AO <- c(ox - ax, oy - ay)
    AB <- c(bx - ax, by - ay)
    #print(AO)
    #print(AB)
    #print("SS")
    
    # Dot product of AO and AB
    dot_product <- sum(AO * AB)
    
    #print(dot_product)
    # Magnitudes of AO and AB
    magnitude_AO <- sqrt(sum(AO^2))
    magnitude_AB <- sqrt(sum(AB^2))
    #print(magnitude_AB)
    
    # Cosine of  angle
    cos_theta <- dot_product / (magnitude_AO * magnitude_AB)
    
    # Angle in radians, then converting to degrees
    theta_radians <- acos(cos_theta)
    theta_degrees <- theta_radians * (180 / pi)
    
    # Return the angle in degrees
  }, error = function(e) { print(e) } )
  return(theta_degrees)
}
