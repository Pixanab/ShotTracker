# manageShotsModule.R

# Manage Shots Module UI -------
manageShotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    gamesTableUI(ns("games_table")),
    shotsContainerUI(ns("shots_container"))
  )
}

# Manage Shots Module Server -------
manageShotsServer <- function(id, dataUser, databaseInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call the games table module and pass the required arguments
    selected_game <- gamesTableServer("games_table", dataUser = dataUser)
    
    # Call the shots container module and pass the required arguments
    shotsContainerServer("shots_container", dataUser, databaseInfo, selected_game)
  })
}
