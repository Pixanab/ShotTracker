# seasonModule.R

# Helper function to create the modal for adding or editing a game
createModal_season <- function(ns, action = c("add", "edit"), gameData = list()) {
  action <- match.arg(action)
  
  modal_title <- if (action == "add") "Add Game" else "Edit Game"
  button_id <- if (action == "add") ns("submitGame") else ns("saveEdit")
  button_label <- if (action == "add") "Add Game" else "Save"
  
  modalDialog(
    title = modal_title,
    easyClose = TRUE,
    fluidRow(
      column(6, dateInput(ns("gameDate"), "Game Date", value = ifelse(!is.null(gameData$GameDate), convert_to_date(as.numeric(gameData$GameDate)), Sys.Date()), format = "dd/mm/yyyy")),
      column(6, selectInput(ns("competition"), "Competition", choices = c("League", "Championship", "Challenge", "Other"), selected = gameData$Competition)),
      column(12, ""), # Empty space for better formating of the modal
      column(6, textInput(ns("homeTeam"), "Home Team", value = gameData$HomeTeam)),
      column(6, textInput(ns("awayTeam"), "Away Team", value = gameData$AwayTeam)),
      column(12, ""), # Empty space for better formating of the modal
      column(6, numericInput(ns("homeTeamGoals"), "Home Team Goals", value = gameData$HomeTeamGoals)),
      column(6, numericInput(ns("awayTeamGoals"), "Away Team Goals", value = gameData$AwayTeamGoals)),
      column(6, numericInput(ns("homeTeamPoints"), "Home Team Points", value = gameData$HomeTeamPoints)),
      column(6, numericInput(ns("awayTeamPoints"), "Away Team Points", value = gameData$AwayTeamPoints))
    ),
    footer = tagList(
      actionButton(button_id, button_label)
    )
  )
}

# Helper function to check if all fields are filled
are_all_fields_filled_season <- function(date, homeTeam, awayTeam, competition, homeTeamPoints, homeTeamGoals, awayTeamPoints, awayTeamGoals) {
  is_valid_date(date) &&
    is_filled(homeTeam) &&
    is_filled(awayTeam) &&
    is_filled(competition) &&
    is_filled(homeTeamPoints) &&
    is_filled(homeTeamGoals) &&
    is_filled(awayTeamPoints) &&
    is_filled(awayTeamGoals)
}

# Helper function to render the season table
table_season <- function(data) {
  datatable(data, selection = 'multiple', rownames = FALSE,
            options = list(columnDefs = list(
              list(visible = FALSE, targets = c(0, 1, 2, 3, 4, 5, 6))  # Hide the specified columns
            )))
}

# Season UI -------
SeasonUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("actionPanel_season")),
    mainPanel(
      fluidRow(
        column(12, offset = 1,
               class = "custom-panel",
               dataTableOutput(ns("seasonTable"))
        )
      )
    )
  )
}

# Season Server -------
SeasonServer <- function(id, dataUser, databaseInfo = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render actionPanel
    output$actionPanel_season <- renderUI({
      renderActionPanel(
        ns,
        add_label = "Add Game",
        edit_label = "Edit Game",
        delete_label = "Delete Game(s)",
        unselect_label = "Unselect All Rows"
      )
    })
    
    # Reactive expression for dataShow
    dataShow <- reactive({
      req(dataUser$games)  # Ensure dataUser$games is available
      
      dataUser$games %>%
        select("_id", "UserName", "User_id", "HomeTeamGoals", "HomeTeamPoints", "AwayTeamGoals", "AwayTeamPoints", everything()) %>%
        mutate(Score = mapply(displayScore, HomeTeamGoals, HomeTeamPoints, AwayTeamGoals, AwayTeamPoints))
    })
    
    # Render games_data in a table
    output$seasonTable <- renderDataTable({
      table_season(dataShow())
    })
    
    # Unselect All Rows button functionality
    observeEvent(input$unselect_rows, {
      # Re-render the table to clear selections
      output$seasonTable <- renderDataTable({
        table_season(dataShow())
      })
    })
    
    # Add game functionality
    observeEvent(input$add_row, {
      showModal(createModal_season(ns, action = "add"))
    })
    
    observeEvent(input$submitGame, {
      if (are_all_fields_filled_season(input$gameDate, input$homeTeam, input$awayTeam, input$competition, input$homeTeamPoints, input$homeTeamGoals, input$awayTeamPoints, input$awayTeamGoals)) {
        new_game <- data.frame(
          GameDate = convert_to_days(as.Date(input$gameDate)),
          HomeTeam = input$homeTeam,
          AwayTeam = input$awayTeam,
          Competition = input$competition,
          HomeTeamPoints = input$homeTeamPoints,
          HomeTeamGoals = input$homeTeamGoals,
          AwayTeamPoints = input$awayTeamPoints,
          AwayTeamGoals = input$awayTeamGoals,
          Result = calculate_result(input$homeTeamGoals, input$homeTeamPoints, input$awayTeamGoals, input$awayTeamPoints),
          stringsAsFactors = FALSE
        )
        
        if (!is.null(new_game)) {
          # Write to database and reload data to data object
          dataNew <- new_game %>% mutate(UserName = dataUser$userName, User_id = dataUser$user_id)
          db_insert(databaseInfo$connection_string, databaseInfo$collection_games, dataNew)
          dataUser$season <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_games, "User_id", dataUser$user_id)
          
          # Trigger a reactive update
          dataUser$games <- dataUser$season
          
          removeModal()
          shinyalert("Game Added!", "New Game has been added.", type = "success")
        }
      } else {
        shinyalert("Error", "Please fill in all the required fields.", type = "error")
      }
    })
    
    # Delete functionality
    observeEvent(input$delete_rows, {
      selected_rows <- input$seasonTable_rows_selected
      if (length(selected_rows) > 0) {
        shinyalert(
          title = "Are you sure?",
          text = "Do you want to delete the selected games?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, delete them!",
          cancelButtonText = "No, cancel",
          callbackR = function(x) {
            if (x) {
              deleteIDs <- dataShow() %>%
                slice(selected_rows) %>%
                pull(`_id`)
              db_delete_many(databaseInfo$connection_string, databaseInfo$collection_games, deleteIDs)
              dataUser$season <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_games, "User_id", dataUser$user_id)
              
              # Trigger a reactive update
              dataUser$games <- dataUser$season
              
              shinyalert("Data Deleted!", "Data has been deleted.", type = "success")
            }
          }
        )
      } else {
        shinyalert("No rows selected", "Please select rows to delete.", type = "warning")
      }
    })
    
    # Edit functionality
    observeEvent(input$edit_row, {
      selected_row <- input$seasonTable_rows_selected
      if (length(selected_row) == 1) {
        selected_data <- dataUser$season[selected_row, ] %>% as.list()
        
        showModal(createModal_season(ns, action = "edit", gameData = selected_data))
      } else {
        shinyalert("Please select a single row to edit", type = "warning")
      }
    })
    
    observeEvent(input$saveEdit, {
      if (are_all_fields_filled_season(input$gameDate, input$homeTeam, input$awayTeam, input$competition, input$homeTeamPoints, input$homeTeamGoals, input$awayTeamPoints, input$awayTeamGoals)) {
        selected_row <- input$seasonTable_rows_selected
        
        if (length(selected_row) == 1) {
          queryIn <- dataUser$season$`_id`[selected_row[1]]
          dataIn <- list(
            GameDate = convert_to_days(input$gameDate),
            HomeTeam = input$homeTeam,
            AwayTeam = input$awayTeam,
            Competition = input$competition,
            HomeTeamPoints = input$homeTeamPoints,
            HomeTeamGoals = input$homeTeamGoals,
            AwayTeamPoints = input$awayTeamPoints,
            AwayTeamGoals = input$awayTeamGoals,
            Result = calculate_result(input$homeTeamGoals, input$homeTeamPoints, input$awayTeamGoals, input$awayTeamPoints)
          )
          
          db_update_id(databaseInfo$connection_string, databaseInfo$collection_games, queryIn, dataIn)
          dataUser$season <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_games, "User_id", dataUser$user_id)
          
          # Trigger a reactive update
          dataUser$games <- dataUser$season
          
          removeModal()
          shinyalert("Saved!", "Changes have been saved.", type = "success")
        }
      } else {
        shinyalert("Error", "Please fill in all the required fields.", type = "error")
      }
    })
  })
}