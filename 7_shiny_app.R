library(shiny)
#shiny::runExample("02_text")

ui <- fluidPage(
  titlePanel("My first shiny app"),
  sidebarPanel(
    selectInput("shift", "Choose a shift:",
                choices = unique(tracking_df$shift_day))
  ),
  mainPanel(
    tableOutput("view")
  )
)

server <- function(input, output) {
  
}