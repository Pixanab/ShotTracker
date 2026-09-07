# gamesTableModule.R

# gamesTable UI -------
gamesTableUI <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(
        column(10, offset = 1,
               class = "custom-panel",
               dataTableOutput(ns("gamesTable"))
               )
        )
      )
}

# gamesTable Server -------
gamesTableServer <- function(id, dataUser, selection_mode = 'single') {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$gamesTable <- renderDataTable({
      dataShow <- dataUser$games %>%
        select("_id", "UserName", "User_id", "HomeTeamGoals", "HomeTeamPoints", "AwayTeamGoals", "AwayTeamPoints", everything()) %>%
        mutate(Score = mapply(displayScore, HomeTeamGoals, HomeTeamPoints, AwayTeamGoals, AwayTeamPoints))
      
      datatable(dataShow, selection = selection_mode, rownames = FALSE,
                options = list(
                  pageLength = 5, # Adjust as needed
                  columnDefs = list(
                    list(visible = FALSE, targets = c(0, 1, 2, 3, 4, 5, 6))  # Hide the specified columns (those numbers refer back to the column indices)
                  )
                ))
    })
    
    selected_game <- reactive({
      selected_rows <- input$gamesTable_rows_selected
      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        dataUser$games[selected_rows, ]
      } else {
        NULL
      }
    })
    
    return(selected_game)
  })
}
