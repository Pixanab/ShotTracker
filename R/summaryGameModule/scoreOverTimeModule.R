# scoreOverTimeModule.R

# Define a function to process and compute scores for each team
process_team_data_score <- function(team_data, team_name) {
  team_data$Points <- ifelse(team_data$ShotOutcome == "Point", 1, 
                             ifelse(team_data$ShotOutcome == "Goal", 3, 0))
  team_data <- team_data[order(team_data$GameTime), ]
  team_data$CumulativeScore <- cumsum(team_data$Points)
  team_data$Team <- team_name
  return(team_data)
}

# Define a function to process and compute scores for each team
process_team_data_expected <- function(team_data, team_name) {
  team_data <- team_data[order(team_data$GameTime), ]
  team_data$CumulativeScore <- cumsum(team_data$ExpectedScore)
  team_data$Team <- team_name
  return(team_data)
}

# scoreOverTimeModule UI -------
scoreOverTimeModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 10, offset = 1,
               class = "custom-panel",
               plotlyOutput(ns("scorePlot"))
               )
      )
    )
  )
}

# scoreOverTimeModule Server -------
scoreOverTimeModuleServer <- function(id, shots_data, home_team, away_team) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$scorePlot <- renderPlotly({
      # Get home team and away team data
      home_team_data <- data_apply_filter(shots_data, "TeamShot", home_team)
      away_team_data <- data_apply_filter(shots_data, "TeamShot", away_team)
      
      # Process the data into cumulative data of the actual scores
      home_shots_cumulative_score <- process_team_data_score(home_team_data, home_team)
      away_shots_cumulative_score <- process_team_data_score(away_team_data, away_team)
      
      # Process the data into cumulative data of the expected scores
      home_shots_cumulative_expected <- process_team_data_expected(home_team_data, home_team)
      away_shots_cumulative_expected <- process_team_data_expected(away_team_data, away_team)
      
      # Combine both teams' actual score data and expected score data
      combined_score_data <- rbind(home_shots_cumulative_score, away_shots_cumulative_score)
      combined_expected_data <- rbind(home_shots_cumulative_expected, away_shots_cumulative_expected)
      
      # Create the plot
      p <- ggplot() +
        # Plot actual scores
        geom_line(data = combined_score_data, aes(x = GameTime, y = CumulativeScore, color = Team, linetype = "Actual Score"), linewidth = 1) +
        geom_point(data = combined_score_data, aes(x = GameTime, y = CumulativeScore, color = Team, text = paste("Player: ", PlayerName, "<br>",
                                                                                                                 "Expected Score: ", paste0(round(ExpectedScore * 100, 2), "%"), "<br>",
                                                                                                                 "Shot Method: ", ShotMethod, "<br>",
                                                                                                                 "Shot Outcome: ", ShotOutcome))) +
        # Plot expected scores with shadowy appearance (alpha)
        geom_line(data = combined_expected_data, aes(x = GameTime, y = CumulativeScore, color = Team, linetype = "Expected Score"), linewidth = 1, alpha = 0.5) +
        geom_point(data = combined_expected_data, aes(x = GameTime, y = CumulativeScore, color = Team, text = paste("Player: ", PlayerName, "<br>",
                                                                                                                    "Expected Score: ", paste0(round(ExpectedScore * 100, 2), "%"), "<br>",
                                                                                                                    "Shot Method: ", ShotMethod, "<br>",
                                                                                                                    "Shot Outcome: ", ShotOutcome)), alpha = 0.5) +
        # Facet the plot based on GameHalf, display horizontally with custom labels
        facet_wrap(~GameHalf, nrow = 1, labeller = labeller(GameHalf = c("1" = "Game Half = 1", "2" = "Game Half = 2"))) +
        labs(
          title = "Score and Expected Score Over Time",
          x = "Game Time",
          y = "Score",
          color = "Team",
          linetype = "Score Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5)  # Center the plot title
        )
      
      # Convert ggplot to plotly for interactivity
      plotly_obj <- ggplotly(p, tooltip = "text")
      
      plotly_obj <- plotly_obj %>%
        layout(
          legend = list(
            orientation = "h",       # Horizontal orientation
            x = 0.5,                 # Center the legend
            xanchor = "center",
            y = -0.5,                # Position the legend below the plot
            title = list(
              text = ""             # Empty legend title (we use annotation instead)
            )
          ),
          annotations = list(
            list(
              text = "<b>Legend:</b> (Team / Score Type)",  # HTML tags for bold text
              xref = "paper",
              yref = "paper",
              x = 0.5,                # Center horizontally with respect to the plot area
              y = -0.4,              # Move annotation further up to avoid overlap
              showarrow = FALSE,
              font = list(size = 16, color = "black"),
              align = "center"
            )
          )
        )
      
      # Display the plot
      plotly_obj
    })
  })
}
