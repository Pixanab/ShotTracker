# helperFunctions.R

# Define the messages
message_style <- "text-align: center; font-size: 20px; color: #ADD8E6;"

MESSAGE_NO_GAME_SELECTED_shotstab <- tags$p("Please select a game from the table above to be able to insert, edit, or delete shots from that game.", style = message_style)

MESSAGE_NO_GAME_SELECTED_summarytab <- tags$p("Please select a game from the table above to see the details.", style = message_style)

# Date --------
# Helper function to convert 'days since 1/1/1900' to Date
convert_to_date <- function(days_since_1900) {
  tryCatch({
    if (!is.numeric(days_since_1900)) stop("Input must be numeric.")
    base_date <- as.Date("1900-01-01")
    target_date <- base_date + days_since_1900
    return(target_date)
  }, error = function(e) {
    warning(paste("Error in convert_to_date:", e$message))
    return(NA)  # Return NA if there is an error
  })
}

# Helper function to convert Date to 'days since 1/1/1900'
convert_to_days <- function(date_str) {
  tryCatch({
    date <- as.Date(date_str)
    if (is.na(date)) stop("Invalid date format.")
    as.numeric(date - as.Date("1900-01-01"))
  }, error = function(e) {
    warning(paste("Error in convert_to_days:", e$message))
    return(NA)  # Return NA if there is an error
  })
}

# Input is filled --------
# Helper function to check for NULL, empty strings, and NA
is_filled <- function(x) {
  !is.null(x) && x != "" && !is.na(x)
}

# Special helper function to check if a date is filled and valid
is_valid_date <- function(date) {
  !is.null(date) && !is.na(date) && class(date) == "Date"
}

# Helper function to check if all required fields are filled
are_all_filled <- function(fields) {
  tryCatch({
    all(sapply(fields, function(field) !is.null(field) && field != ""))
  }, error = function(e) {
    warning(paste("Error in are_all_filled:", e$message))
    return(FALSE)  # Return FALSE if there is an error
  })
}

# Helper function to get pre-filled value or default
get_prefilled_value <- function(edit_saved_shot, field_name, default_value) {
  tryCatch({
    if (!is.null(edit_saved_shot) && !is.null(edit_saved_shot[[field_name]])) {
      return(edit_saved_shot[[field_name]])
    } else {
      return(default_value)
    }
  }, error = function(e) {
    warning(paste("Error in get_prefilled_value:", e$message))
    return(default_value)  # Return default_value if there is an error
  })
}

# Helper function to get pre-filled data or default
get_prefilled_data <- function(edit_saved_shot, field_name, default_data) {
  tryCatch({
    if (!is.null(edit_saved_shot) && !is.null(edit_saved_shot[[field_name]])) {
      return(edit_saved_shot[[field_name]])
    } else {
      return(default_data)
    }
  }, error = function(e) {
    warning(paste("Error in get_prefilled_data:", e$message))
    return(default_data)  # Return default_data if there is an error
  })
}


# Helper function to add TeamShotSelect column
add_team_shot_select <- function(data, team_home, team_away) {
  tryCatch({
    if (!("TeamShot" %in% names(data))) stop("Data does not contain 'TeamShot' column.")
    
    # Apply team shot selection
    data <- data %>%
      mutate(TeamShotSelect = ifelse(TeamShot == team_home, "Home",
                                     ifelse(TeamShot == team_away, "Away", NA)))
    
    return(data)
  }, error = function(e) {
    warning(paste("Error in add_team_shot_select:", e$message))
    return(data)  # Return original data if there is an error
  })
}

# Calculate result --------
calculate_result <- function(home_team_goals, home_team_points, away_team_goals, away_team_points) {
  tryCatch({
    home_team_score <- (home_team_goals * 3) + home_team_points
    away_team_score <- (away_team_goals * 3) + away_team_points
    
    if (home_team_score > away_team_score) {
      return("Home Win")
    } else if (home_team_score < away_team_score) {
      return("Away Win")
    } else {
      return("Draw")
    }
  }, error = function(e) {
    warning(paste("Error in calculate_result:", e$message))
    return("Error")  # Return "Error" if there is an error
  })
}

displayScore <- function(home_team_goals, home_team_points, away_team_goals, away_team_points) {
  tryCatch({
    paste0(home_team_goals, "-", home_team_points, " : ", away_team_goals, "-", away_team_points)
  }, error = function(e) {
    warning(paste("Error in displayScore:", e$message))
    return("Error")  # Return "Error" if there is an error
  })
}

# Expected Score --------
calculate_shot_expected_score <- function(Intercept = 2.82,
                                          Distance = -0.072,
                                          Angle = -0.003,
                                          MediumPressure = -0.49,
                                          HighPressure = -0.657,
                                          SetPlay = 3.547,
                                          Division2 = -0.125,
                                          Division34 = -0.258,
                                          LeftFoot = -0.1,
                                          Hand = 0.138,
                                          LeftSide = 0.021,
                                          Distance_SetPlay = -0.053,
                                          Angle_SetPlay = -0.018,
                                          LeftFoot_LeftSide = -0.044,
                                          Hand_LeftSide = -0.017,
                                          distance,
                                          angle,
                                          shot_pressure,
                                          set_play,
                                          shot_method,
                                          side) {
  
  tryCatch({
    # Check for required inputs
    if (!is.numeric(distance) || !is.numeric(angle)) stop("Distance and angle must be numeric.")
    
    # Calculate the ExpectedScoreOdds based on the inputs
    ExpectedScoreOdds <- Intercept +
      distance * (Distance + ifelse(set_play == "Open Play", 0, Distance_SetPlay)) +
      angle * (Angle + ifelse(set_play == "Open Play", 0, Angle_SetPlay)) +
      ifelse(shot_pressure == "Medium", MediumPressure, 0) +
      ifelse(shot_pressure == "High", HighPressure, 0) +
      Division34 +
      ifelse(set_play == "Open Play", 0, SetPlay) +
      ifelse(shot_method == "Left Foot", LeftFoot, 0) +
      ifelse(shot_method == "Hand", Hand, 0) +
      ifelse(side == "Left", LeftSide, 0) +
      ifelse(shot_method == "Left Foot" & side == "Left", LeftFoot_LeftSide, 0) +
      ifelse(shot_method == "Hand" & side == "Left", Hand_LeftSide, 0)
    
    # Calculate the ExpectedScoreCalculation
    ExpectedScoreCalculation <- 1 / (1 + exp(-ExpectedScoreOdds))
    
    # Return the result as a named list
    result <- list(ExpectedScoreOdds = ExpectedScoreOdds, ExpectedScoreCalculation = ExpectedScoreCalculation)
    
    return(result)
  }, error = function(e) {
    warning(paste("Error in calculate_shot_expected_score:", e$message))
    return(list(ExpectedScoreOdds = NA, ExpectedScoreCalculation = NA))  # Return NA values if there is an error
  })
}
