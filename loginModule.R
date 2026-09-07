# loginModule.R

# Temporary user database -------
user_database <- data.frame(
  Teams = c("q", "Dublin", "Kerry", "Down"),
  passwords = c("q", "123Dublin", "k", "d"),
  stringsAsFactors = FALSE
)

# UI definition -------
loginModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(column(6, offset = 3,

            div(
        class = "custom-panel",
        style = "margin-top: 50px; padding: 20px;",
        # Centered title
        div(class = "title",
            h1("ShotTracker")
        ),
        
        # Centered subtitle with <h6> style
        div(class = "subtitle",
            tags$h6("Do your login below")
        ),
        fluidRow(
          column(12,
                 textInput(ns("team"), "Team:", "Kerry", width = '100%')
          )
        ),
        fluidRow(
          column(12,
                 passwordInput(ns("password"), "Password:", "k", width = '100%')
          )
        ),
        fluidRow(
          column(12,
                 actionButton(ns("login"), "Log In", width = '100%')
          )
        ),
        fluidRow(
          column(12,
                 uiOutput(ns("login_message"))
          )
        )
      )
      
      ))
    )
  )
}

# Server logic -------
loginModuleServer <- function(id, user_database) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value for login status
    user_logged_in <- reactiveVal(FALSE)
    current_team <- reactiveVal(NULL)
    
    observeEvent(input$login, {
      team <- input$team
      password <- input$password
      
      if (team %in% user_database$Teams) {
        correct_password <- user_database$passwords[user_database$Teams == team]
        if (password == correct_password) {
          user_logged_in(TRUE)
          current_team(team)
          output$login_message <- renderUI({
            tags$div(style = "color: green;", "Login successful.")
          })
        } else {
          output$login_message <- renderUI({
            tags$div(style = "color: red;", "Invalid password.")
          })
        }
      } else {
        output$login_message <- renderUI({
          tags$div(style = "color: red;", "Team not found.")
        })
      }
    })
    
    return(list(
      user_logged_in = user_logged_in,
      current_team = current_team
    ))
  })
}
