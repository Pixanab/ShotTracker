# shotsContainerModule.R

# UI shotsContainer module -------
shotsContainerUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("message")),
    fluidRow(
      uiOutput(ns("actionPanel_shots")),
      column(width = 9 ,uiOutput(ns("shotsTableUI")))
    )
  )
}

# Server shotsContainer module -------
shotsContainerServer <- function(id, dataUser, databaseInfo, selected_game) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper function to generate players data frame
    generate_players_data <- function() {
      home_team <- selected_game()$HomeTeam
      away_team <- selected_game()$AwayTeam
      
      dataUser$players %>%
        filter(Team == home_team | Team == away_team) %>%
        select(PlayerName, Team, `_id`) %>%
        distinct()
    }
    
    # Reactive expression for filtered shots data - change 15/8
    dataShow <- reactive({
      req(selected_game())
      dataUser$shots %>%
        filter(Game_id == selected_game()$`_id`) %>%
        arrange(GameHalf, GameTime) %>% 
        select(-UserName, -User_id, -Player_id, -Game_id)
    })
    
    # Render functions for UI
    observe({
      if (is.null(selected_game())) {
        output$message <-   renderUI({MESSAGE_NO_GAME_SELECTED_shotstab})
        output$shotsTableUI <- renderUI(NULL)
        output$actionPanel_shots <- renderUI(NULL)
      } else {
        output$message <- renderUI(NULL)
        output$shotsTableUI <- renderUI({
          dataTableOutput(ns("shotsTable"))
        })
        output$actionPanel_shots <- renderUI({
          renderActionPanel(ns, add_label = "Add Shot", edit_label = "Edit Shot", delete_label = "Delete Shot(s)", unselect_label = "Unselect All Rows")
        })
        output$shotsTable <- renderDataTable({
          req(dataShow())
          datatable(dataShow(), selection = 'multiple', rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0))))
        })
      }
    })
    
    # Helper function to update shots data - Change 15/8
    updateShotsData <- function() {
      dataUser$shots <- cleanShotData(db_get_filtered_collection_v2(
        databaseInfo$connection_string,
        databaseInfo$collection_shots,
        "User_id",
        dataUser$user_id
      ))
    }
    
    # Unselect All Rows functionality
    observeEvent(input$unselect_rows, {
      # Clear the selected rows
      output$shotsTable <- renderDataTable({
        req(dataShow())
        datatable(dataShow(), selection = list(target = 'row', selected = integer(0)), rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0))))
      })
    })
    
    # Insert functionality
    observeEvent(input$add_row, {
      # Add Modal with custom height and width
      showModal(modalDialog(
        title = "Insert Shot",
        size = "xl",  # Keep the xl size for basic configuration
        insertShotsInput(ns("shot_module"), players = generate_players_data(), game_info = selected_game()),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submitShot"), "Submit")
        ),
        # Add custom CSS to make the modal larger (found that height is hard to set by default) -> (using shinydashboard::box() might be another solution)
        tags$style(HTML("
    .modal-xl {
      width: 80% !important;  /* Adjust the width to 80% of the viewport */
      max-width: 80% !important;  /* Ensure max-width is also overridden */
    }
    .modal-body {
      max-height: 80vh;  /* Adjust the height to 80% of the viewport height */
      overflow-y: auto;  /* Enable scrolling if the content exceeds the modal height */
    }
  "))
      ))

      shot_module <- insertShotsServer("shot_module",
                                       selected_game()$HomeTeam,
                                       selected_game()$AwayTeam,
                                       players = generate_players_data(),
                                       game_info = selected_game())
      
      observeEvent(input$submitShot, {
        if (shot_module$all_fields_filled()) {
          new_shot <- shot_module$new_shot_data()
          if (!is.null(new_shot)) {
            dataNew <- new_shot %>%
              mutate(UserName = dataUser$userName, User_id = dataUser$user_id, Game_id = selected_game()$`_id`)
            db_insert(databaseInfo$connection_string, databaseInfo$collection_shots, dataNew)
            updateShotsData()
            removeModal()
            shinyalert("Shot Added!", "New shot has been added.", type = "success")
          }
        } else {
          shinyalert("Error", "Please fill in all the required fields and double-click on the pitch to select a shot position.", type = "error")
        }
      }, ignoreInit = TRUE)
    })
    
    # Delete functionality
    observeEvent(input$delete_rows, {
      selected_rows <- input$shotsTable_rows_selected
      if (length(selected_rows) > 0) {
        shinyalert(
          title = "Are you sure?",
          text = "Do you want to delete the selected shots?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, delete them!",
          cancelButtonText = "No, cancel",
          callbackR = function(x) {
            if (x) {
              deleteIDs <- dataShow() %>%
                slice(selected_rows) %>%
                pull(`_id`)
              db_delete_many(databaseInfo$connection_string, databaseInfo$collection_shots, deleteIDs)
              updateShotsData()
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
      selected_row <- input$shotsTable_rows_selected
      
      if (length(selected_row) == 1) {
        selected_data <- dataShow()[selected_row, ]
        adjusted_data <- convert_and_adjust_coordinates(
          selected_data, 
          selected_data$GameHalf,
          selected_data$TeamShot,
          selected_data$HomeTeam, 
          center1, 
          center2
        )
        adjusted_data_with_team_shot_select <- add_team_shot_select(
          adjusted_data, 
          selected_data$HomeTeam, 
          selected_data$AwayTeam
        )
        
        # Edit Modal with custom height and width
        showModal(modalDialog(
          title = "Edit Shot",
          size = "xl",  # Keep the xl size for basic configuration
          insertShotsInput(ns("shot_module"), 
                           players = generate_players_data(),
                           edit_saved_shot = adjusted_data_with_team_shot_select,
                           game_info = selected_game()),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("saveEdit"), "Save")
          ),
          # Add custom CSS to make the modal larger (found that height is hard to set by default) -> (using shinydashboard::box() might be another solution)
          tags$style(HTML("
    .modal-xl {
      width: 80% !important;  /* Adjust the width to 80% of the viewport */
      max-width: 80% !important;  /* Ensure max-width is also overridden */
    }
  "))
        ))
        
        edit_shot_module <- insertShotsServer(
          "shot_module", 
          selected_game()$HomeTeam, 
          selected_game()$AwayTeam,
          players = generate_players_data(),
          edit_saved_shot = adjusted_data_with_team_shot_select,
          game_info = selected_game()
        )
        
        observeEvent(input$saveEdit, {
          if (edit_shot_module$all_fields_filled()) {
            edited_shot <- edit_shot_module$new_shot_data()
            if (!is.null(edited_shot)) {
              queryIn <- dataUser$shots %>%
                filter(Game_id == selected_game()$`_id`) %>%
                slice(selected_row[1]) %>%
                pull(`_id`)
              dataIn <- edited_shot %>%
                mutate(UserName = dataUser$userName, User_id = dataUser$user_id, Game_id = selected_game()$`_id`)
              db_update_id(
                databaseInfo$connection_string, 
                databaseInfo$collection_shots, 
                queryIn, 
                dataIn
              )
              updateShotsData()
              shinyalert("Shot Edited!", "The shot details have been updated.", type = "success")
              removeModal()
            }
          } else {
            shinyalert("Error", "Please fill in all the required fields and double-click on the pitch to select a shot position.", type = "error")
          }
        }, ignoreInit = TRUE)
      } else {
        shinyalert("Please select a single row to edit", type = "warning")
      }
    })
  })
}
