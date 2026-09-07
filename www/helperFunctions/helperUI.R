# helperUI.R

# =========================================================
# MAIN UI ELEMENTS:

# Button configuration --------
buttons <- list(
  list(id = "home_button", icon = "home", label = "Home"),
  list(id = "summary_games_button", icon = "chart-bar", label = "Games"),
  list(id = "contact_us_button", icon = "envelope", label = "Contact Us"),
  list(id = "settings_button", icon = "cogs", label = "Settings")
)

# =========================================================
# UI Elements: Tiles
# =========================================================

# Helper function to generate a clickable tile as a button
tileUI <- function(tab_id, icon_name, label, subtitle = NULL) {
  column(
    width = 6, # Default width for tiles
    class = "tile-column",
    actionButton(
      inputId = tab_id,
      label = div(
        icon(icon_name, class = paste0("tile-icon ", icon_name, "-icon")), # Apply tile icon styling
        tags$div(label, class = "tile-title"), # Apply tile title styling
        if (!is.null(subtitle)) {
          tags$div(subtitle, class = "tile-subtitle") # Apply tile subtitle styling
        },
        tags$div("Click to View", class = "tile-footer") # Apply tile footer styling
      ),
      class = "tile-button",
      style = "background-color: #f9f9f9; color: #000000;" # Whitish background and black text color
    )
  )
}

# Wrapper function to generate a clickable tile with a specified column width
tileColumnUI <- function(tab_id, icon_name, label) {
  fluidRow(
    tileUI(tab_id, icon_name, label)
  )
}

# =========================================================
# UI Elements: Top Bar
# =========================================================

# Function to render the top bar with fixed positioning
renderTopBar <- function() {
  conditionalPanel(
    condition = "output.logged_in == true", # Condition to show the bottom bar
    fluidPage(
      div(
        class = "navbar-custom",
        div(
          class = "title",
          tags$h1("ShotTracker") # Title for the top bar
        )
      )
    )
  )
}

# =========================================================
# UI Elements: Bottom Bar
# =========================================================

# Helper function to create a footer button
createFooterButton <- function(button_id, icon_class, button_label, button_width) {
  column(
    width = button_width, # Set the width for the button column
    align = "center",     # Center the column content
    actionButton(
      inputId = button_id,
      label = div(
        icon(icon_class, class = paste0("footer-icon ", icon_class, "-icon")), # Apply footer button styling
        tags$br(),
        tags$span(button_label) # Keep the default label color
      ),
      style = "background-color: #007bff; color: white; border: none; border-radius: 8px; padding: 10px 20px;" # Blue background with white text for the button
    )
  )
}

# Helper function to render the bottom bar with specified buttons
renderBottomBar <- function(buttons) {
  num_buttons <- length(buttons)
  
  # Calculate the width percentage for each button
  button_width <- 12 / num_buttons
  
  conditionalPanel(
    condition = "output.logged_in == true", # Condition to show the bottom bar
    fluidPage(
      fluidRow(
        class = "navbar-custom-bottom",
        div(
          class = "footer-buttons-container",
          lapply(buttons, function(btn) {
            createFooterButton(
              button_id = btn$id,
              icon_class = btn$icon,
              button_label = btn$label,
              button_width = button_width
            )
          })
        )
      )
    )
  )
}

# =========================================================
# APP ELEMENTS:

# =========================================================
# UI Elements: Action Panels
# =========================================================

# Function to render an action panel with customizable button labels
renderActionPanel <- function(ns, add_label = "Add Shot", edit_label = "Edit Shot", delete_label = "Delete Shot(s)", unselect_label = "Unselect All Rows") {
  sidebarPanel(
    class = "custom-panel",
    width = 2,
    fluidRow(
      column(width = 12, actionButton(ns("add_row"), label = add_label, class = "action-button"))
    ),
    fluidRow(
      column(width = 12, actionButton(ns("edit_row"), label = edit_label, class = "action-button"))
    ),
    fluidRow(
      column(width = 12, actionButton(ns("delete_rows"), label = delete_label, class = "action-button"))
    ),
    fluidRow(
      column(width = 12, actionButton(ns("unselect_rows"), label = unselect_label, class = "action-button"))
    )
  )
}

# =========================================================
# UI Elements: Data Tables
# =========================================================

# Function to render a data table
renderShotsTableUI <- function(ns) {
  renderUI({
    dataTableOutput(ns("shotsTable"))
  })
}
