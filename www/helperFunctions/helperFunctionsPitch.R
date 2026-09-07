# Libraries -------
library(png)

# Load the pitch image -------
pitch_image <- readPNG("./www/images/pitchv2.png")
gaa_ball_icon <- readPNG("./www/images/gaa_ball_icon.png")

# Constant Values -------
center1 <- c(-75, 0)
center2 <- c(75, 0)

# Color map of shot outcomes -------
outcome_colors <- c("Point" = "blue",   
                    "Wide" = "red",    
                    "Post" = "purple", 
                    "Short" = "orange", 
                    "Blocked" = "grey", 
                    "Goal" = "green",  
                    "Saved" = "yellow")

# Shape map of set plays -------
set_play_shapes <- c(
  "Open Play"              = 16,  
  "Free Kick from Hands"   = 4,   
  "Free Kick from Ground"  = 17,  
  "Penalty"                = 18,  
  "45m Kick"               = 8,   
  "Mark"                   = 4)

# Function Definitions -------
cartesian_to_polar <- function(x, y, center) {
  # Adjust coordinates based on the center
  x_adj <- x - center[1]
  y_adj <- y - center[2]
  
  # Calculate the radius
  radius <- sqrt(x_adj^2 + y_adj^2)
  
  # Calculate the angle in radians
  angle_rad <- atan2(y_adj, x_adj)
  
  # Convert radians to degrees
  angle_deg <- angle_rad * 180 / pi
  
  # Adjust angle to be between 0 and 360 degrees
  if (angle_deg < 0) {
    angle_deg <- angle_deg + 360
  }
  
  # Determine the side and adjust angle to be within 0-90 degrees
  if (angle_deg <= 90) {
    side <- "Left"
  } else if (angle_deg <= 180) {
    angle_deg <- angle_deg - 90
    side <- "Right"
  } else if (angle_deg <= 270) {
    angle_deg <- angle_deg - 180
    side <- "Left"
  } else {
    angle_deg <- angle_deg - 270
    side <- "Right"
  }
  
  return(data.frame(radius = radius, angle = angle_deg, side = side))
}

# Function to convert polar coordinates to Cartesian coordinates
polar_to_cartesian <- function(distance, angle, center) {
  x <- center[1] + (distance * cos(angle * pi / 180))
  y <- center[2] + (distance * sin(angle * pi / 180))
  return(data.frame(x = x, y = y))
}

# Function to determine which center to use based on game half and team
get_center <- function(gamehalf, team, home_team, center1, center2) {
  # Convert gamehalf to numeric
  gamehalf <- tryCatch({
    as.numeric(gamehalf)
  }, warning = function(w) {
    stop("Warning: ", w$message)
  }, error = function(e) {
    stop("Error converting 'gamehalf' to numeric: ", e$message)
  })
  
  # Argument checks
  if (!is.numeric(gamehalf) || length(gamehalf) != 1 || !gamehalf %in% 1:2) stop("Argument 'gamehalf' must be 1 or 2.")
  if (!is.character(team) || length(team) != 1) stop("Argument 'team' must be a single character string.")
  if (!is.character(home_team) || length(home_team) != 1) stop("Argument 'home_team' must be a single character string.")
  if (!is.numeric(center1) || length(center1) != 2 || !is.numeric(center2) || length(center2) != 2) {
    stop("Arguments 'center1' and 'center2' must be numeric vectors of length 2.")
  }
  
  if (gamehalf == 1) {
    if (team == home_team) {
      return(center2)
    } else {
      return(center1)
    }
  } else if (gamehalf == 2) {
    if (team == home_team) {
      return(center1)
    } else {
      return(center2)
    }
  } else {
    stop("Invalid 'gamehalf' value. Must be 1 or 2.")
  }
}

get_distance_sign <- function(gamehalf, team, home_team) {
  # Convert gamehalf to numeric
  gamehalf <- tryCatch({
    as.numeric(gamehalf)
  }, warning = function(w) {
    stop("Warning: ", w$message)
  }, error = function(e) {
    stop("Error converting 'gamehalf' to numeric: ", e$message)
  })
  
  # Argument checks
  if (!is.numeric(gamehalf) || length(gamehalf) != 1 || !gamehalf %in% 1:2) stop("Argument 'gamehalf' must be 1 or 2.")
  if (!is.character(team) || length(team) != 1) stop("Argument 'team' must be a single character string.")
  if (!is.character(home_team) || length(home_team) != 1) stop("Argument 'home_team' must be a single character string.")
  
  if (gamehalf == 1) {
    if (team == home_team) {
      return(-1)  # Negate distance for home team in the first half
    } else {
      return(1)   # No negation for away team in the first half
    }
  } else if (gamehalf == 2) {
    if (team == home_team) {
      return(1)   # No negation for home team in the second half
    } else {
      return(-1)  # Negate distance for away team in the second half
    }
  } else {
    stop("Invalid 'gamehalf' value. Must be 1 or 2.")
  }
}

adjust_y_for_side <- function(data, side) {
  data$y[side == "Right"] <- -data$y[side == "Right"]
  return(data)
}

adjust_for_left_side <- function(data, converted_data) {
  converted_data <- adjust_y_for_side(converted_data, data$Side)
  return(converted_data)
}

convert_and_adjust_coordinates <- function(data, gamehalf, team, home_team, center1, center2) {
  # Argument checks
  if (!is.data.frame(data)) stop("Argument 'data' must be a data frame.")
  if (!all(c("Distance", "Angle", "Side") %in% names(data))) stop("Data frame 'data' must contain 'Distance', 'Angle', and 'Side' columns.")
  
  # Convert gamehalf to numeric
  gamehalf <- tryCatch({
    as.numeric(gamehalf)
  }, warning = function(w) {
    stop("Warning: ", w$message)
  }, error = function(e) {
    stop("Error converting 'gamehalf' to numeric: ", e$message)
  })
  
  # Convert team and home_team to character
  team <- tryCatch({
    as.character(team)
  }, warning = function(w) {
    stop("Warning: ", w$message)
  }, error = function(e) {
    stop("Error converting 'team' to character: ", e$message)
  })
  
  home_team <- tryCatch({
    as.character(home_team)
  }, warning = function(w) {
    stop("Warning: ", w$message)
  }, error = function(e) {
    stop("Error converting 'home_team' to character: ", e$message)
  })
  
  # Argument checks
  if (!is.numeric(gamehalf) || length(gamehalf) != 1 || !gamehalf %in% 1:2) stop("Argument 'gamehalf' must be 1 or 2.")
  if (!is.character(team) || length(team) != 1) stop("Argument 'team' must be a single character string.")
  if (!is.character(home_team) || length(home_team) != 1) stop("Argument 'home_team' must be a single character string.")
  if (!is.numeric(center1) || length(center1) != 2 || !is.numeric(center2) || length(center2) != 2) {
    stop("Arguments 'center1' and 'center2' must be numeric vectors of length 2.")
  }
  
  tryCatch({
    # Determine the center based on game half and team
    center <- get_center(gamehalf, team, home_team, center1, center2)

    # Determine the distance sign
    distance_sign <- get_distance_sign(gamehalf, team, home_team)
    
    # Convert polar coordinates to Cartesian coordinates
    converted_data <- polar_to_cartesian(distance_sign * data$Distance, data$Angle, center = center)

    # Adjust coordinates for left side shots
    adjusted_data <- adjust_for_left_side(data, converted_data)
    
    # Combine the data
    final_data <- cbind(data, adjusted_data)
    
    return(final_data)
  }, error = function(e) {
    warning("Error occurred in convert_and_adjust_coordinates:\n", e$message, "\n")
    return(NULL)
  })
}
