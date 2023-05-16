#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Recreational Fisheries Decision Support Tool"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          shinyWidgets::awesomeCheckboxGroup(
            inputId = "state",
            label = "State", 
            choices = c("NJ", "CT"),
            inline = TRUE,
            status = "danger"
          ),
            sliderInput("baglimit",
                        "Choose Bag Limit for Summer FLounder",
                        min = 1,
                        max = 10,
                        value = 3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput(outputId = "tableout")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$state, {
    output$tableout<- renderTable({
      state <- input$state
      
      source(here::here("setup.R"))
      
      source(here::here(paste0("model_run_",state,".R")))
  })})
}

# Run the application 
shinyApp(ui = ui, server = server)
