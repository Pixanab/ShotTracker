# manageSquadsModule.R

# Helper function to create the modal for adding or editing a player
createModal_squads <- function(ns, action = c("add", "edit"), team = "", playerName = "", position = "Forward") {
  action <- match.arg(action)
  
  modal_title <- if (action == "add") "Add Player" else "Edit Player"
  button_id <- if (action == "add") ns("submitPlayer") else ns("saveEdit")
  button_label <- if (action == "add") "Add Player" else "Save"
  
  modalDialog(
    title = modal_title,
    easyClose = TRUE,
    fluidRow(
      column(6, textInput(ns("teamInput"), "Team", value = team)),
      column(6, textInput(ns("playerNameInput"), "Player Name", value = playerName)),
      column(6, offset = 3, selectInput(ns("positionInput"), "Position", choices = c("Forward", "Back", "Goalkeeper"), selected = position))
    ),
    footer = tagList(
      actionButton(button_id, button_label)
    )
  )
}

# Helper function to check if all fields are filled
are_all_fields_filled_squads <- function(team, playerName, position) {
  is_filled(team) && is_filled(playerName) && is_filled(position)
}

# Helper function to render the players table
table_squads <- function(data) {
  datatable(data, selection = 'multiple', rownames = FALSE,
            options = list(columnDefs = list(
              list(visible = FALSE, targets = c(0, 1, 2))  # Hide the specified columns
            )))
}

# manageSquad UI -------
manageSquadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("actionPanel_squads")),
    mainPanel(
      fluidRow(
        column(12, offset = 1,
               class = "custom-panel",
               dataTableOutput(ns("playersTable"))
        )
      )
    )
  )
}

# manageSquad Server -------
manageSquadServer <- function(id, dataUser, databaseInfo = NULL) { 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render actionPanel
    output$actionPanel_squads <- renderUI({
      renderActionPanel(
        ns,
        add_label = "Add Player",
        edit_label = "Edit Player",
        delete_label = "Delete Player(s)",
        unselect_label = "Unselect All Rows"
      )
    })
    
    # Reactive expression for dataShow
    dataShow <- reactive({
      req(dataUser$players)  # Ensure dataUser$players is available
      dataUser$players %>% select("_id", "UserName", "User_id", everything())
    })
    
    # Render players_data in a table and hide certain columns
    output$playersTable <- renderDataTable({
      table_squads(dataShow())
    })
    
    # Unselect All Rows button functionality
    observeEvent(input$unselect_rows, {
      # Re-render the table to clear selections
      output$playersTable <- renderDataTable({
        table_squads(dataShow())
      })
    })
    
    # Add player functionality
    observeEvent(input$add_row, {
      showModal(createModal_squads(ns, action = "add"))
    })
    
    observeEvent(input$submitPlayer, {
      if (are_all_fields_filled_squads(input$teamInput, input$playerNameInput, input$positionInput)) {
        new_player <- data.frame(
          Team = input$teamInput,
          PlayerName = input$playerNameInput,
          Position = input$positionInput,
          stringsAsFactors = FALSE
        )
        
        if (!is.null(new_player)) {
          # Write to database and reload data to data object
          dataNew <- new_player %>% mutate(UserName = dataUser$userName, User_id = dataUser$user_id)
          db_insert(databaseInfo$connection_string, databaseInfo$collection_players, dataNew)
          dataUser$players <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_players, "User_id", dataUser$user_id)
          
          removeModal()
          shinyalert("Player Added!", "New Player has been added.", type = "success")
        }
      } else {
        shinyalert("Error", "Please fill in all the required fields.", type = "error")
      }
    })
    
    # Delete functionality
    observeEvent(input$delete_rows, {
      selected_rows <- input$playersTable_rows_selected
      if (length(selected_rows) > 0) {
        shinyalert(
          title = "Are you sure?",
          text = "Do you want to delete the selected players?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, delete them!",
          cancelButtonText = "No, cancel",
          callbackR = function(x) {
            if (x) {
              deleteIDs <- dataShow() %>%
                slice(selected_rows) %>%
                pull(`_id`)
              db_delete_many(databaseInfo$connection_string, databaseInfo$collection_players, deleteIDs)
              dataUser$players <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_players, "User_id", dataUser$user_id)
              
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
      selected_row <- input$playersTable_rows_selected
      if (length(selected_row) == 1) {
        selected_data <- dataShow() %>% slice(selected_row) %>% as.list()
        showModal(createModal_squads(ns, action = "edit", team = selected_data$Team, playerName = selected_data$PlayerName, position = selected_data$Position))
      } else {
        shinyalert("Please select a single row to edit", type = "warning")
      }
    })
    
    observeEvent(input$saveEdit, {
      if (are_all_fields_filled_squads(input$teamInput, input$playerNameInput, input$positionInput)) {
        selected_rows <- input$playersTable_rows_selected
        
        if (length(selected_rows) == 1) {
          queryIn <- dataShow() %>% slice(selected_rows) %>% pull(`_id`)
          dataIn <- list(
            Team = input$teamInput,
            PlayerName = input$playerNameInput,
            Position = input$positionInput
          )
          
          db_update_id(databaseInfo$connection_string, databaseInfo$collection_players, queryIn, dataIn)
          dataUser$players <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_players, "User_id", dataUser$user_id)
          
          removeModal()
          shinyalert("Saved!", "Changes have been saved.", type = "success")
        }
      } else {
        shinyalert("Error", "Please fill in all the required fields.", type = "error")
      }
    })
  })
}
