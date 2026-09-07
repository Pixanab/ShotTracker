# Libraries --------
library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(DT)
library(stringr)
library(XML)
library(rvest)
library(httr)
library(mongolite)
library(tidyr)
library(formattable)
library(shinyFiles)
library(xml2)
library(rjson)
library(survPen)
library(zoo)
library(shinycssloaders)
library(tidyverse)
library(signal)
library(stats)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(magick)
library(cowplot)
library(png)
library(shinyalert)
library(shinyauthr)
library(shinyjs)
library(plotly)
library(rclipboard)
library(shinydashboard)
library(ggimage)

# Options --------
options(scipen = 999)  # Disable scientific notation
options(shiny.maxRequestSize = 500*1024^2)  # Set maximum request size to 500 MB
options(shiny.http.requestTimeout = 300)  # Set timeout to 300 seconds (5 minutes)

# Helper Files --------
source("www/helperFunctions/helperFunctions.R")
source("www/helperFunctions/helperDatabaseFunctions.R")
source("www/helperFunctions/helperFunctionsPitch.R")
source("www/helperFunctions/helperUI.R")
source("www/config.R")

# Tab Modules --------
source("R/contactUsModule.R")
source("R/settingsModule.R")
source("R/gamesTableModule.R")
source("R/seasonModule/seasonModule.R")
source("R/shotsModule/shotsModuleNew.R")
source("R/shotsModule/manageShotsModuleNew.R")
source("R/summaryGameModule/summaryGameModule.R")
source("R/summaryGameModule/plotPitchModule.R")
source("R/summaryGameModule/scoreOverTimeModule.R")
source("R/squadsModule/manageSquadsModule.R")

# Login --------
source("loginModule.R")

# UI --------
ui <- fluidPage(
  theme = "bootstrap.css",
  useShinyjs(),
  rclipboardSetup(),
  includeCSS("www/styles.css"),
  
  # Top bar
  renderTopBar(),
  
  # Main content area for UI, conditionally rendered
  fluidRow(
    column(
      width = 12,
      div(
        class = "content",
        uiOutput("main_ui")
      )
    )
  ),
  
  # Bottom bar with the specified buttons, conditionally rendered
  renderBottomBar(buttons)
)

# Server --------
server <- function(input, output, session) {
  
  # Function to render the main UI with tiles
  renderMainUI <- function() {
    output$main_ui <- renderUI({
      fluidPage(
        div(
          class = "tiles-container",
          fluidRow(
            tileUI("season_module", "calendar-alt", "Season", "Manage Season Games"),
            tileUI("summary_games_module", "chart-bar", "Games", "View a Game's Summary"),
            tileUI("shotsNew_module", "crosshairs", "Shots", "Manage Shots"),
            tileUI("manage_squad_module", "users", "Squads", "Manage Squad's Players")
          )
        )
      )
    })
  }
  
  # Function to render modules based on the selected tab
  renderModule <- function(module_id) {
    output$main_ui <- renderUI({
      switch(module_id,
             "season_module" = SeasonUI("season_module"),
             "summary_games_module" = SummaryGamesUI("summary_games_module"),
             "shotsNew_module" = manageShotsNewUI("shotsNew_module"),
             "manage_squad_module" = manageSquadUI("manage_squad_module"),
             "contact_us_module" = contactUsUI("contact_us_module"),
      )
    })
  }
  
  # Login handling
  login <- loginModuleServer("login_module", user_database)
  user_team <- NULL
  
  output$logged_in <- reactive({
    login$user_logged_in()
  })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  observe({
    if (login$user_logged_in()) {
      user_team <<- login$current_team()
      
      # Initialize the main UI with tiles
      renderMainUI()
      
      # Observe tile clicks
      observeEvent(input$season_module, { renderModule("season_module") })
      observeEvent(input$summary_games_module, { renderModule("summary_games_module") })
      observeEvent(input$shotsNew_module, { renderModule("shotsNew_module") })
      observeEvent(input$manage_squad_module, { renderModule("manage_squad_module") })
      # Observe bottom bar button clicks
      observeEvent(input$home_button, { renderMainUI() })
      observeEvent(input$summary_games_button, { renderModule("summary_games_module") })
      observeEvent(input$contact_us_button, { renderModule("contact_us_module") })
      # For the Settings Module we just call the module to not "Un-render" other Modules
      observeEvent(input$settings_button, { settingsUI("settings_module") })
      
      # Load user data
      user_id <- db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_users, "UserName", user_team)$`_id`[1]
      dataUser <- reactiveValues(
        userName = user_team, 
        user_id = user_id,
        shots = cleanShotData(db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_shots, "UserName", user_team)),
        players = db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_players, "UserName", user_team),
        games = db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_games, "UserName", user_team),
        season = db_get_filtered_collection_v2(databaseInfo$connection_string, databaseInfo$collection_games, "User_id", user_id)
      )
      
      # Initialize module servers
      SeasonServer("season_module", dataUser, databaseInfo)
      manageShotsNewServer("shotsNew_module", dataUser, databaseInfo)
      SummaryGamesServer("summary_games_module", dataUser, databaseInfo)
      manageSquadServer("manage_squad_module", dataUser, databaseInfo)
      contactUsServer("contact_us_module")
      settingsServer("settings_module")
      
    } else {
      output$main_ui <- renderUI({
        fluidPage(
          loginModuleUI("login_module")
        )
      })
    }
  })
}

# Run the application --------
shinyApp(ui = ui, server = server)
