# insertShotsModule.R

# Helper function to get filtered players based on selected team
get_filtered_players <- function(players, selected_team = NULL, game_info = NULL, edit_saved_shot = NULL) {
  # If edit_saved_shot is available, use its team for filtering
  if (!is.null(edit_saved_shot) && !is.null(edit_saved_shot$TeamShot)) {
    selected_team <- edit_saved_shot$TeamShot
  }
  
  # If selected_team is still NULL, use HomeTeam from game_info
  if (is.null(selected_team) && !is.null(game_info)) {
    selected_team <- unique(game_info$HomeTeam)
  }
  
  # Filter players based on the selected team
  dplyr::filter(players, Team == selected_team) %>%
    dplyr::select(PlayerName) %>%
    dplyr::distinct() %>%
    dplyr::pull(PlayerName)
}

# insert Shots Input -------
insertShotsInput <- function(id, players, edit_saved_shot = NULL, game_info = NULL) {
  ns <- NS(id)
  
  # Determine the selected team and player based on edit_saved_shot or game_info
  selected_team <- if (!is.null(edit_saved_shot)) {
    edit_saved_shot$TeamShot
  } else {
    unique(game_info$HomeTeam)
  }
  
  # Get filtered players
  filtered_players <- get_filtered_players(players, selected_team, game_info, edit_saved_shot)
  
  # Determine default values for teamShot and playerName
  default_team <- if (!is.null(edit_saved_shot)) {
    if (edit_saved_shot$TeamShot == unique(game_info$HomeTeam)) "Home" else "Away"
  } else {
    "Home" # Default value if no edit_saved_shot
  }
  
  default_player <- if (!is.null(edit_saved_shot) && !is.null(edit_saved_shot$PlayerName)) {
    edit_saved_shot$PlayerName
  } else {
    filtered_players[1] # Default to the first player if no edit_saved_shot
  }
  
  # CSS to define height of the modal's side bar panel
  tagList(
    tags$style(HTML("
      #sidebar {
        height: 750px !important; /* Set height to 750px */
        overflow-y: auto;         /* Add scroll bar if content overflows */
      }
    ")),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        id = "sidebar",
        selectInput(ns("teamShot"), "Which Team made the shot?", 
                    choices = c("Home", "Away"), 
                    selected = default_team),
        selectInput(ns("playerName"), "Player Name",
                    choices = filtered_players,  # Use the filtered player names
                    selected = default_player),  # Set the default selected player
        selectInput(ns("setPlay"), "Set Play", 
                    choices = c("Open Play", "Free Kick from Hands", "Free Kick from Ground", "Penalty", "45m Kick", "Mark"),
                    selected = get_prefilled_value(edit_saved_shot, "SetPlay", "Open Play")),
        selectInput(ns("shotType"), "Shot Type", 
                    choices = c("Goal attempt", "Point attempt"),
                    selected = get_prefilled_value(edit_saved_shot, "ShotType", "Goal attempt")),
        selectInput(ns("shotMethod"), "Shot Method", 
                    choices = c("Left foot", "Right foot", "Hand"),
                    selected = get_prefilled_value(edit_saved_shot, "ShotMethod", "Left foot")),
        selectInput(ns("shotPressure"), "Shot Pressure", 
                    choices = c("Low", "Medium", "High"),
                    selected = get_prefilled_value(edit_saved_shot, "ShotPressure", "Low")),
        selectInput(ns("shotOutcome"), "Shot Outcome", 
                    choices = c("Point", "Wide", "Post", "Short", "Blocked", "Goal", "Saved"),
                    selected = get_prefilled_value(edit_saved_shot, "ShotOutcome", "Point")),
        numericInput(ns("gameTime"), "Game Time", 
                     value = get_prefilled_value(edit_saved_shot, "GameTime", 0.1), 
                     min = 0.1, max = 50, step = 0.1),
        selectInput(ns("gameHalf"), "Game Half", 
                    choices = c(1, 2), 
                    selected = get_prefilled_value(edit_saved_shot, "GameHalf", 1))
      ),
      mainPanel(
        fluidPage(
          plotOutput(ns("pitchPlot"), 
                     dblclick = ns("double_click"), 
                     height = "790px")
        )
      )
    )
  )
}

# insert Shots Server -------
insertShotsServer <- function(id, team_home, team_away, players, edit_saved_shot = NULL, game_info = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize double click coordinates with edit_saved_shot if available
    double_click_coords <- reactiveVal(
      if (!is.null(edit_saved_shot)) {
        adjusted_shot <- adjust_for_left_side(edit_saved_shot, data.frame(
          x = edit_saved_shot$x, 
          y = edit_saved_shot$y,
          stringsAsFactors = FALSE
        ))
        data.frame(
          x = adjusted_shot$x, 
          y = adjusted_shot$y, 
          radius = edit_saved_shot$Distance, 
          angle = edit_saved_shot$Angle, 
          side = edit_saved_shot$Side, 
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(x = numeric(), y = numeric(), radius = numeric(), angle = numeric(), side = character(), stringsAsFactors = FALSE)
      }
    )
    
    # Render the pitch plot
    output$pitchPlot <- renderPlot({
      ggplot() +
        annotation_custom(rasterGrob(pitch_image, width = unit(1, "npc"), height = unit(1, "npc")),
                          -45, 45, -75, 75) +
        geom_point(data = double_click_coords(), aes(x = y, y = x), color = "red", size = 3) +
        ylim(-75, 75) +
        xlim(-45, 45) +
        labs(title = "Please double click on the field to draw your shot") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#ADD8E6", size = 14, hjust = 0.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        coord_fixed()
    })
    
    # Update coordinates on double-click
    observeEvent(input$double_click, {
      dbl_click <- input$double_click
      
      if (!is.null(dbl_click)) {
        center <- get_center(
          gamehalf = input$gameHalf,
          team = ifelse(input$teamShot == "Home", team_home, team_away),
          home_team = team_home,
          center1 = center1,
          center2 = center2
        )
        
        shot_position <- cartesian_to_polar(dbl_click$y, dbl_click$x, center = center)
        
        double_click_coords(
          data.frame(
            x = dbl_click$y, 
            y = dbl_click$x, 
            radius = shot_position$radius, 
            angle = shot_position$angle, 
            side = shot_position$side,
            stringsAsFactors = FALSE
          )
        )
      }
    })
    
    # Create new shot data for processing
    new_shot_data <- reactive({
      coords <- double_click_coords()
      
      data.frame(
        GameDate = game_info$GameDate, 
        HomeTeam = game_info$HomeTeam,
        AwayTeam = game_info$AwayTeam,
        
        GameHalf = input$gameHalf,
        GameTime = input$gameTime,
        TeamShot = ifelse(input$teamShot == "Home", team_home, team_away),
        
        PlayerName = input$playerName,
        Player_id = unique(data_apply_filter(players, "PlayerName", input$playerName)$`_id`), 
        
        SetPlay = input$setPlay,
        ShotOutcome = input$shotOutcome,
        ShotPressure = input$shotPressure,
        ShotType = input$shotType,
        ShotMethod = input$shotMethod,
        ExpectedScore = calculate_shot_expected_score(
          distance = coords$radius, 
          angle = coords$angle, 
          shot_pressure = input$shotPressure, 
          set_play = input$setPlay, 
          shot_method = input$shotMethod, 
          side = coords$side
        )$ExpectedScoreOdds,
        Distance = coords$radius,
        Angle = coords$angle,
        Side = coords$side
      )
    })
    
    # Check if all fields are filled
    all_fields_filled <- reactive({
      required_inputs <- list(
        input$teamShot, input$gameTime, input$playerName, input$shotType,
        input$shotMethod, input$shotPressure, input$shotOutcome, input$setPlay,
        input$gameHalf
      )
      
      !any(sapply(required_inputs, is.null))
    })
    
    # Observe changes in team selection and update player options
    observeEvent(input$teamShot, ignoreInit = TRUE,{
      selected_team <- if (input$teamShot == "Home") {
        game_info$HomeTeam
      } else {
        game_info$AwayTeam
      }
      
      # Get filtered players
      filtered_players <- get_filtered_players(players, selected_team, game_info)
      
      # Update player select input with req() to ensure default player is only set if available
      req(filtered_players)
      default_player <- if (length(filtered_players) > 0) {
        filtered_players[1]
      } else {
        NULL
      }
      
      updateSelectInput(session, "playerName",
                        choices = filtered_players,
                        selected = default_player)
    })
    
    return(list(
      new_shot_data = reactive({
        isolate({
          new_shot_data()
        })
      }),
      all_fields_filled = all_fields_filled
    ))
  })
}
