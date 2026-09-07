# summaryGamesModule.R

# SummaryGames UI -------
SummaryGamesUI <- function(id) {
  ns <- NS(id)
  tagList(
    gamesTableUI(ns("games_table")),
    uiOutput(ns("message")),
    fluidRow(
      column(12, uiOutput(ns("games_table_ui"))),
      column(12, uiOutput(ns("scorePlotContainer"))),
      column(12, uiOutput(ns("plotPitchContainer")))
    )
  )
}

# SummaryGames Server -------
SummaryGamesServer <- function(id, dataUser, databaseInfo = NULL, selected_game) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call the games table module and pass the required arguments
    selected_game <- gamesTableServer("games_table", dataUser = dataUser)
    
    # Handle rendering based on the selected game
    observe({
      if (is.null(selected_game())) {
        output$message <- renderUI({ MESSAGE_NO_GAME_SELECTED_summarytab })
        output$scorePlotContainer <- renderUI(NULL)
        output$plotPitchContainer <- renderUI(NULL)
      } else {
        output$message <- renderUI(NULL)
        
        game_data <- selected_game()
        if (!is.null(game_data)) {
          # Extract necessary information from selected_game
          game_id <- game_data[["_id"]]
          home_team <- game_data[["HomeTeam"]]
          away_team <- game_data[["AwayTeam"]]
          
          # Extract the shots data of that game_id
          shots_data <- data_apply_filter(dataUser$shots, "Game_id", game_id)
          
          # Render the score plot and pitch plot
          output$scorePlotContainer <- renderUI({
            scoreOverTimeModuleUI(ns("score_over_time_module"))
          })
          scoreOverTimeModuleServer("score_over_time_module", shots_data, home_team, away_team)
          
          output$plotPitchContainer <- renderUI({
            plotPitchUI(ns("pitch_plot_module"),home_team, away_team)
          })
          plotPitchServer("pitch_plot_module", shots_data, home_team)
        } else {
          output$scorePlotContainer <- renderUI(NULL)
          output$plotPitchContainer <- renderUI({ MESSAGE_NO_GAME_SELECTED })
        }
      }
    })
  })
}
