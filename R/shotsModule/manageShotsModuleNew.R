# manageShotsModule.R

# Manage Shots Module UI -------
manageShotsNewUI <- function(id) {
  ns <- NS(id)
  tagList(
    gamesTableUI(ns("games_table_newShots")), #From GamesTable module,
    shotsNewUI(ns("shotsNew_module"))
  )
}

# Manage Shots Module Server -------
manageShotsNewServer <- function(id, dataUser, databaseInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call the games table module and pass the required arguments
    selected_game <- gamesTableServer("games_table_newShots", dataUser = dataUser)
    
    # Call the shots container module and pass the required arguments
    shotsNewServer("shotsNew_module", dataUser, databaseInfo, selected_game)
  })
}
