# seasonModule.R

# Season UI -------
shotsNewUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    gamesTableUI(ns("games_table_newShots")), # From GamesTable module
    uiOutput(ns("message")),
    uiOutput(ns("actionPanel_shots")),
    mainPanel(
      div(
        class = "custom-panel", 
        style = "margin-bottom: 100px;", # Have to add margin bottom spacing manually since it overlaps the bottom bar 
        dataTableOutput(ns("shotsNewTable"))
      )
    )
  )
}

# Season Server -------
shotsNewServer <- function(id, dataUser, databaseInfo = NULL, selected_game) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #Initiate Games Table
    #selected_game <- gamesTableServer("games_table_newShots", dataUser = dataUser)
    
    
    # Reactive expression for dataShow
    # Reactive expression for filtered shots data - change 15/8
    dataShow <- reactive({
      req(selected_game())
      dataUser$shots %>%
        filter(Game_id == selected_game()$`_id`) %>%
        arrange(GameHalf, GameTime) %>%
        select(GameDate, HomeTeam, AwayTeam, ShotPressure, ShotType, ShotMethod, Distance, Angle, Side, UserName, User_id, Player_id, Game_id, everything()) %>%
        mutate(ExpectedScore = paste0(round(ExpectedScore * 100, 2), "%"))
    })
    
    #Reactive for position information for plotting to update on double click of graph
    shot_position <- reactiveVal({
      data.frame(Distance = numeric(), Angle = numeric(), Side = character(), stringsAsFactors = FALSE)
    })


    #Note issue here with  as this observe not triggering when deselect a row but output is triggered
    #Output to UI
    
    observe({
      output$message <-   renderUI({MESSAGE_NO_GAME_SELECTED_shotstab})
      req(selected_game())

      #Render Message
      output$message <- renderUI(NULL)
      
      # Render actionPanel
      output$actionPanel_shots <- renderUI({
        renderActionPanel(
          ns,
          add_label = "Add Shot",
          edit_label = "Edit Shot",
          delete_label = "Delete Shot(s)",
          unselect_label = "Unselect All Rows"
        )
      })
        
      # Render games_data in a table
      output$shotsNewTable <- renderDataTable({
        datatable(
          dataShow(),
          selection = 'multiple',
          rownames = FALSE,
          options = list(
            pageLength = 10, # Adjust as needed
            columnDefs = list(
              list(visible = FALSE, targets = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))  # Hide the specified columns
            )
          )
        )
      })
      
      
    })
    
    
    
    
    # Unselect All Rows button functionality
    observeEvent(input$unselect_rows, {
      # Re-render the table to clear selections
      output$shotsNewTable <- renderDataTable({
        datatable(
          dataShow(),
          selection = 'multiple',
          rownames = FALSE,
          options = list(
            pageLength = 10, # Adjust as needed
            columnDefs = list(
              list(visible = FALSE, targets = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))  # Hide the specified columns
            )
          )
        )
      })
    })
    
    
    
    # Delete functionality
    observeEvent(input$delete_rows, {
      selected_rows <- input$shotsNewTable_rows_selected
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
              db_delete_many(connection_string, databaseInfo$collection_shots, deleteIDs)
              
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

      selected_row <- input$shotsNewTable_rows_selected
      
      if (length(selected_row) == 1) {
        
        selected_data <- dataShow()[selected_row, ]
        players = generate_players_data()
        
        shot_position(data.frame(Distance = dataShow()[input$shotsNewTable_rows_selected, ]$Distance, Angle = dataShow()[input$shotsNewTable_rows_selected, ]$Angle, Side=dataShow()[input$shotsNewTable_rows_selected, ]$Side,stringsAsFactors = FALSE))

        
        showModal(modalDialog(
          title = "Edit Shot",
          easyClose = TRUE,
          
          tagList(
            sidebarLayout(
              sidebarPanel(
                width = 3,
                
              selectInput(ns("teamShot"), "Team Shooting", choices = c(selected_game()$HomeTeam, selected_game()$AwayTeam),selected = get_prefilled_value(selected_data, "TeamShot", selected_game()$HomeTeam)),
              selectInput(ns("playerName"), "Player Name",
                                    choices = unique(players$PlayerName),
                                    selected = get_prefilled_value(selected_data, "PlayerName", players$PlayerName[1])),
              selectInput(ns("setPlay"), "Set Play", 
                                    choices = c("Open Play", "Free Kick from Hands", "Free Kick from Ground", "Penalty", "45m Kick", "Mark"),
                                    selected = get_prefilled_value(selected_data, "SetPlay", "Open Play")),
              selectInput(ns("shotType"), "Shot Type", 
                                    choices = c("Goal attempt", "Point attempt"),
                                    selected = get_prefilled_value(selected_data, "ShotType", "Goal attempt")),
              selectInput(ns("shotMethod"), "Shot Method", 
                                    choices = c("Left foot", "Right foot", "Hand"),
                                    selected = get_prefilled_value(selected_data, "ShotMethod", "Left foot")),
              selectInput(ns("shotPressure"), "Shot Pressure", 
                                  choices = c("Low", "Medium", "High"),
                                  selected = get_prefilled_value(selected_data, "ShotPressure", "Low")),
              selectInput(ns("shotOutcome"), "Shot Outcome", 
                                         choices = c("Point", "Wide", "Post", "Short", "Blocked", "Goal", "Saved"),
                                         selected = get_prefilled_value(selected_data, "ShotOutcome", "Point")),
              numericInput(ns("gameTime"), "Game Time", 
                                          value = get_prefilled_value(selected_data, "GameTime", 0.1), 
                                          min = 0.1, max = 60, step = 0.1),
              selectInput(ns("gameHalf"), "Game Half", 
                                         choices = c(1, 2), 
                                         selected = get_prefilled_value(selected_data, "GameHalf", 1))
              ),
              mainPanel(
                fluidPage(
                  #uiOutput(ns("shotForm")),
                  plotOutput(ns("pitchPlot"), dblclick = ns("double_click"))
                  
                  #verbatimTextOutput(ns("insert_status"))
                )
              )
            )
          ),
          
          footer = tagList(
            actionButton(ns("saveEdit"), "Save")
          )
          
        ))
      } else {
        shinyalert("Please select a single row to edit", type = "warning")
      }
    })
    
    
    
    
    output$pitchPlot <- renderPlot({
      
      #Don't need req statement here and dont need reactive object
      xMult=1
      if(shot_position()$Side=="Left"){xMult=-1}
      pitchDimensions=c(45,75)

      dataPlot <- data.frame(
        xPos =  xMult * (shot_position()$Distance * sin(shot_position()$Angle * pi / 180)),
        yPos = 75 - (shot_position()$Distance * cos(shot_position()$Angle * pi / 180))
        )
      
      ggplot() +
        annotation_custom(rasterGrob(pitch_image, width = unit(1, "npc"), height = unit(1, "npc")),-45, 45, -75, 75) +
        geom_point(data=dataPlot, aes(x=xPos, y = yPos), color = "red", size = 3) +
        ylim(-75, 75) +
        xlim(-45, 45) +
        theme_minimal()
    })
    
    
    observeEvent(input$double_click, {
        
      #Convert data here - changed to always shoot up into goals
      xDist=abs(input$double_click$x)
      yDist=75 - input$double_click$y
      
      # Calculate the radius
      distance <- sqrt(xDist^2 + yDist^2)
      
      # Calculate the angle in degrees
      angle <- atan2(xDist, yDist)* 180 / pi
        
      side="Right"
      if(input$double_click$x <0){side="Left"}
      
      shot_position(data.frame(Distance = distance, Angle = angle, Side=side, stringsAsFactors = FALSE))
    })
    
    
    observeEvent(input$saveEdit, {
      
      players = generate_players_data()
      
      required_inputs <- list(
        input$teamShot, input$gameTime, input$playerName, input$shotType,
        input$shotMethod, input$shotPressure, input$shotOutcome, input$setPlay,
        input$gameHalf
      )
        
      all_fields_filled = !any(sapply(required_inputs, is.null))
      
      
      if (all_fields_filled) {
        selected_row <- input$shotsNewTable_rows_selected
        
        if (length(selected_row) == 1) {
          
          queryIn <- dataUser$shots$`_id`[selected_row[1]]
          dataIn <- list(
          
          GameDate = selected_game()$GameDate, 
          HomeTeam = selected_game()$HomeTeam,
          AwayTeam = selected_game()$AwayTeam,
          
          GameHalf = input$gameHalf,
          GameTime = input$gameTime,
          TeamShot = input$teamShot,#ifelse(input$teamShot == "Home", team_home, team_away),
          
          PlayerName = input$playerName,
          Player_id = unique(data_apply_filter(players, "PlayerName", input$playerName)$Player_id), 
          
          SetPlay = input$setPlay,
          ShotOutcome = input$shotOutcome,
          ShotPressure = input$shotPressure,
          ShotType = input$shotType,
          ShotMethod = input$shotMethod,
          ExpectedScore=0.5,
          Distance=shot_position()$Distance,
          Angle=shot_position()$Angle,
          Side=shot_position()$Side)
          
          
          db_update_id(connection_string, databaseInfo$collection_shots, queryIn, dataIn)
          updateShotsData()
          
          
          # Trigger a reactive update
          dataUser$games <- dataUser$season
          
          removeModal()
          shinyalert("Saved!", "Changes have been saved.", type = "success")
        }
      } else {
        shinyalert("Error", "Please fill in all the required fields.", type = "error")
      }
    })
    
    
    #############NEW
    
    # Add Shot functionality
    observeEvent(input$add_row, {
      selected_row <- input$shotsNewTable_rows_selected
      

        selected_data <- dataShow()[selected_row, ]
        players = generate_players_data()
        
        shot_position(data.frame(Distance = 100, Angle = 90, Side="Left",stringsAsFactors = FALSE))
        #print(shot_position())
        
        showModal(modalDialog(
          title = "Add Shot",
          easyClose = TRUE,
          
          tagList(
            sidebarLayout(
              sidebarPanel(
                width = 3,
                
                selectInput(ns("teamShot"), "Team Shooting", choices = c(selected_game()$HomeTeam, selected_game()$AwayTeam),selected = get_prefilled_value(selected_data, "TeamShot", selected_game()$HomeTeam)),
                selectInput(ns("playerName"), "Player NameName",
                            choices = unique(players$PlayerName),
                            selected = get_prefilled_value(selected_data, "PlayerName", players$PlayerName[1])),
                selectInput(ns("setPlay"), "Set Play", 
                            choices = c("Open Play", "Free Kick from Hands", "Free Kick from Ground", "Penalty", "45m Kick", "Mark"),
                            selected =  "Open Play"),
                selectInput(ns("shotType"), "Shot Type", 
                            choices = c("Goal attempt", "Point attempt"),
                            selected = "Point attempt"),
                selectInput(ns("shotMethod"), "Shot Method", 
                            choices = c("Left foot", "Right foot", "Hand"),
                            selected = get_prefilled_value(selected_data, "ShotMethod", "Left foot")),
                selectInput(ns("shotPressure"), "Shot Pressure", 
                            choices = c("Low", "Medium", "High"),
                            selected = get_prefilled_value(selected_data, "ShotPressure", "Low")),
                selectInput(ns("shotOutcome"), "Shot Outcome", 
                            choices = c("Point", "Wide", "Post", "Short", "Blocked", "Goal", "Saved"),
                            selected = get_prefilled_value(selected_data, "ShotOutcome", "Point")),
                numericInput(ns("gameTime"), "Game Time", 
                             value = get_prefilled_value(selected_data, "GameTime", 0.1), 
                             min = 0.1, max = 60, step = 0.1),
                selectInput(ns("gameHalf"), "Game Half", 
                            choices = c(1, 2), 
                            selected = get_prefilled_value(selected_data, "GameHalf", 1)),
                
              ),
              mainPanel(
                fluidPage(
                  plotOutput(ns("pitchPlot"), dblclick = ns("double_click"))
                  
                  #verbatimTextOutput(ns("insert_status"))
                )
              )
            )
          ),
          
          footer = tagList(
            actionButton(ns("saveGame"), "Save")
          )
          
        ))

    })
    
    
    
    
    
    observeEvent(input$saveGame, {
      
      players = generate_players_data()
      
      #Include check on position information
      required_inputs <- list(
        input$teamShot, input$gameTime, input$playerName, input$shotType,
        input$shotMethod, input$shotPressure, input$shotOutcome, input$setPlay,
        input$gameHalf
      )
      
      all_fields_filled = !any(sapply(required_inputs, is.null))
      
      
      if (all_fields_filled) {
        selected_row <- input$shotsNewTable_rows_selected
        
          
        queryIn <- dataUser$shots$`_id`[selected_row[1]]
        
        dataIn <- data.frame(
          
        GameDate = selected_game()$GameDate, 
        HomeTeam = selected_game()$HomeTeam,
        AwayTeam = selected_game()$AwayTeam,
        
        GameHalf = input$gameHalf,
        GameTime = input$gameTime,
        TeamShot = input$teamShot,#ifelse(input$teamShot == "Home", team_home, team_away),
        
        PlayerName = input$playerName,
        Player_id = unique(data_apply_filter(players, "PlayerName", input$playerName)$Player_id), 
        
        SetPlay = input$setPlay,
        ShotOutcome = input$shotOutcome,
        ShotPressure = input$shotPressure,
        ShotType = input$shotType,
        ShotMethod = input$shotMethod,
        ExpectedScore=0.5,
        Distance=shot_position()$Distance,
        Angle=shot_position()$Angle,
        Side=shot_position()$Side,
        stringsAsFactors = FALSE
        )
        
        dataIn <- dataIn %>% mutate(UserName = dataUser$userName, User_id = dataUser$user_id, Game_id=selected_game()$`_id`)
        
        print(dataIn)
        
        db_insert(connection_string, databaseInfo$collection_shots, dataIn)
        updateShotsData()
        
        
        # Trigger a reactive update
        dataUser$games <- dataUser$season
        
        removeModal()
        shinyalert("Saved!", "New Shot Created.", type = "success")
        
      } else {
        shinyalert("Error", "Please fill in all the required fields.", type = "error")
      }
    })
    
    

    
    
    
    
    
  })
  
  
  ########### Functions added in
  
  generate_players_data <- function() {
    dataUser$shots %>%
      select(PlayerName, Player_id) %>%
      distinct()
  }
  
  
  # Helper function to update shots data - Change 15/8
  updateShotsData <- function() {
    dataUser$shots <- cleanShotData(db_get_filtered_collection_v2(
      databaseInfo$connection_string,
      databaseInfo$collection_shots,
      "User_id",
      dataUser$user_id
    ))
  }
  
}
