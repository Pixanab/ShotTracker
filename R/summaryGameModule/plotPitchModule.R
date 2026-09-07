# plotPitchModule.R

# Function to generate filter UI elements
generate_filter_ui <- function(ns, choices) {
  tagList(
    selectizeInput(ns("plotpitch_setplay"), "Select Set Play", choices = c("All", choices$setplays), selected = "All"),
    selectizeInput(ns("plotpitch_shotoutcome"), "Select Shot Outcome", choices = c("All", choices$shotoutcomes), selected = "All"),
    selectizeInput(ns("plotpitch_playername"), "Select Player Name", choices = c("All", choices$playernames), selected = "All"),
    selectizeInput(ns("plotpitch_shotpressure"), "Select Shot Pressure", choices = c("All", choices$shotpressures), selected = "All"),
    selectizeInput(ns("plotpitch_shottype"), "Select Shot Type", choices = c("All", choices$shottypes), selected = "All"),
    selectizeInput(ns("plotpitch_shotmethod"), "Select Shot Method", choices = c("All", choices$shotmethods), selected = "All")
  )
}

# Function to filter data
filter_shots_data <- function(data, filters) {
  for (filter in names(filters)) {
    if (filters[[filter]] != "All") {
      data <- data[data[[filter]] == filters[[filter]], ]
    }
  }
  return(data)
}

# Function to get filter choices
get_filter_choices <- function(data) {
  list(
    setplays = unique(data$SetPlay),
    shotoutcomes = unique(data$ShotOutcome),
    playernames = unique(data$PlayerName),
    shotpressures = unique(data$ShotPressure),
    shottypes = unique(data$ShotType),
    shotmethods = unique(data$ShotMethod)
  )
}

# plotPitch UI --------
plotPitchUI <- function(id, home_team, away_team) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 3,
      wellPanel(
        selectizeInput(ns("plotpitch_team"), "Select Team", choices = c(home_team, away_team), selected = home_team),
        selectizeInput(ns("plotpitch_gamehalf"), "Select Game Half", choices = c(1, 2), selected = 1),
        uiOutput(ns("plotpitch_filter_ui")),
        actionButton(ns("plotpitch_reset"), "Reset Filters", class = "btn-primary")
      )
    ),
    column(
      width = 9,
      wellPanel(
        plotOutput(ns("plotpitch_shotsplot"), width = "100%", height = "642px")
      )
    )
  )
}

# Plot Pitch Server --------
plotPitchServer <- function(id, shots_data, home_team) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Function to get current filtered data
    get_filtered_data <- function() {
      req(input$plotpitch_gamehalf, input$plotpitch_team, input$plotpitch_setplay, input$plotpitch_shotoutcome, input$plotpitch_playername, input$plotpitch_shotpressure, input$plotpitch_shottype, input$plotpitch_shotmethod)
      
      filters <- list(
        GameHalf = input$plotpitch_gamehalf,
        TeamShot = input$plotpitch_team,
        SetPlay = input$plotpitch_setplay,
        ShotOutcome = input$plotpitch_shotoutcome,
        PlayerName = input$plotpitch_playername,
        ShotPressure = input$plotpitch_shotpressure,
        ShotType = input$plotpitch_shottype,
        ShotMethod = input$plotpitch_shotmethod
      )
      
      filter_shots_data(shots_data, filters)
    }
    
    # Function to update all filter inputs
    update_all_filters <- function() {
      data <- get_filtered_data()
      updated_filter_choices <- get_filter_choices(data)
      
      # Define a helper function to determine choices
      update_choices <- function(filter_choices) {
        if (length(filter_choices) > 1) {
          return(c("All", filter_choices))
        } else {
          return(filter_choices)
        }
      }
      
      # Update each filter input with the new choices
      updateSelectInput(session, "plotpitch_setplay", choices = update_choices(updated_filter_choices$setplays))
      updateSelectInput(session, "plotpitch_shotoutcome", choices = update_choices(updated_filter_choices$shotoutcomes))
      updateSelectInput(session, "plotpitch_playername", choices = update_choices(updated_filter_choices$playernames))
      updateSelectInput(session, "plotpitch_shotpressure", choices = update_choices(updated_filter_choices$shotpressures))
      updateSelectInput(session, "plotpitch_shottype", choices = update_choices(updated_filter_choices$shottypes))
      updateSelectInput(session, "plotpitch_shotmethod", choices = update_choices(updated_filter_choices$shotmethods))
    }
    
    # Render filter UI elements
    output$plotpitch_filter_ui <- renderUI({
      initial_filter_choices <- get_filter_choices(shots_data[shots_data$GameHalf == 1 & shots_data$TeamShot == home_team, ])
      generate_filter_ui(ns, initial_filter_choices)
    })
    
    # Observe changes to each input and update all filters
    observeEvent(c(input$plotpitch_setplay, input$plotpitch_shotoutcome, input$plotpitch_playername, input$plotpitch_shotpressure, input$plotpitch_shottype, input$plotpitch_shotmethod), {
      update_all_filters()
    })
    
    # Observe reset button click to reset all filters
    observeEvent(c(input$plotpitch_team, input$plotpitch_gamehalf, input$plotpitch_reset), {
      initial_filter_choices <- get_filter_choices(shots_data[shots_data$GameHalf == input$plotpitch_gamehalf & shots_data$TeamShot == input$plotpitch_team, ])
      output$plotpitch_filter_ui <- renderUI({
        generate_filter_ui(ns, initial_filter_choices)
      })
    })
    
    # Scatterplot of the shots using ggplot2
    output$plotpitch_shotsplot <- renderPlot({
      data <- get_filtered_data()
      req(nrow(data) > 0)
      
      # Set the levels for ShotOutcome and SetPlay to ensure consistent legend
      data$ShotOutcome <- factor(data$ShotOutcome, levels = names(outcome_colors))
      data$SetPlay <- factor(data$SetPlay, levels = names(set_play_shapes))
      
      # Determine if the center should be left or right based on game half and team
      plot_data <- convert_and_adjust_coordinates(data, input$plotpitch_gamehalf, input$plotpitch_team, home_team, center1, center2)
      
      # Create a ggplot with the pitch image as background
      pitch_plot <- ggplot() +
        annotation_custom(rasterGrob(pitch_image, width = unit(1, "npc"), height = unit(1, "npc")), -45, 45, -75, 75) +
        geom_point(data = plot_data, aes(x = y, y = x, color = ShotOutcome, shape = SetPlay), size = 3) +
        scale_color_manual(values = outcome_colors) +
        scale_shape_manual(values = set_play_shapes) +
        labs(
          title = "Plot of the Shots",
          color = "Shot Outcomes",
          shape = "Set Play"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 1.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom"
        ) +
        ylim(-75, 75) +
        xlim(-45, 45) +
        coord_fixed()
      
      pitch_plot
    })
  })
}
