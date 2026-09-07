# contactUsModule.R

# List email addresses
emails <- list(
  "contact_email_1" = "d00261751@student.dkit.ie",
  "contact_email_2" = "example2@example.com"
)

# Contact Us UI --------
contactUsUI <- function(id) {
  ns <- NS(id)
  
  # Use bslib's page_fluid instead of fluidPage
  page_fluid(
    titlePanel(
      HTML("<b>Contact Us</b>")),
    sidebarLayout(
      sidebarPanel(
        class = "custom-panel",
        p("If you have any questions or need further assistance, please reach out to us at the email addresses.")
      ),
      mainPanel(
        class = "custom-panel",
        column(
          width = 12,
          p("Emails:"),
          column(
            width = 6,
            rclipButton(
              inputId = ns("clip_email_1"),
              label = emails[["contact_email_1"]],
              clipText = emails[["contact_email_1"]],
              icon = icon("clipboard"),
              tooltip = "Click to copy email",
              placement = "top"
            )
          )
        )
      )
    )
  )
}

# Contact Us Server --------
contactUsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Function to show shinyalert notification
    showAlert <- function(email_address) {
      shinyalert::shinyalert(
        title = "Email Copied",
        text = paste("Email:", email_address, "copied to clipboard"),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 2000,
        showConfirmButton = FALSE
      )
    }
    
    # Observe clipboard button clicks
    observeEvent(input$clip_email_1, {
      showAlert(emails[["contact_email_1"]])
    })
    
    observeEvent(input$clip_email_2, {
      showAlert(emails[["contact_email_2"]])
    })
  })
}