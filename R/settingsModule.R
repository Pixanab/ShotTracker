# settingsModule.R

# Settings UI --------
settingsUI <- function(id) {
  ns <- NS(id)
  showModal(
    modalDialog(
      title = "Settings",
      footer = NULL, # Remove the footer to get rid of the "Dismiss" button
      size = "m", # medium size modal
      easyClose = TRUE,
      
      div(
        # class = "settings-content",
        # p("Adjust your settings below:"),
        # # Dark Mode Toggle
        # switchInput(
        #   inputId = ns("dark_mode"),
        #   label = "Dark Mode",
        #   value = FALSE,
        #   onLabel = "On",
        #   offLabel = "Off"
        # ),
        actionButton(
          inputId = ns("logout_button"), 
          label = "Log Out", 
          class = "btn",
          style = "background-color: red; color: white;"  # Inline CSS for red background and white text
        )
      )
    )
  )
}

# Settings Server --------
settingsServer <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    
    # Ensure shinyjs is initialized
    shinyjs::useShinyjs()
    
    # observeEvent(input$dark_mode, {
    #   if (input$dark_mode) {
    #     shinyjs::addClass(selector = "body", class = "dark-mode")
    #   } else {
    #     shinyjs::removeClass(selector = "body", class = "dark-mode")
    #   }
    # })
    
    observeEvent(input$logout_button, {
      removeModal()  # Close the settings modal
      session$reload()  # Reload the session to simulate logout
    })
  })
}
