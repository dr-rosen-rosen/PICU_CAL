#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

tracking_df <- read_excel(paste0(config$tracking_file_loc, config$tracking_file))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Collective Allostatic Load in the PICU"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "shift",label = "Select a shift",
                  choices = c('',unique(tracking_df$shift_day)),
                  selected = NULL,
                  multiple = F), width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("edaPlot"),
      plotlyOutput("hrPlot"),
      plotOutput('accPlot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$edaPlot <- renderPlotly({
    get_E4_plot(measure = 'EDA', shift = input$shift, tracking_df = tracking_df)
  })
  output$hrPlot <- renderPlotly({
    get_E4_plot(measure = 'HR', shift = input$shift, tracking_df = tracking_df)
  })
  output$accPlot <- renderPlot({
    get_E4_plot(measure = 'ACC', shift = input$shift, tracking_df = tracking_df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
