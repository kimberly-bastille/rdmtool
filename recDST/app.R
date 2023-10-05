#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


  library(shiny)
  library(shinyjs)
  library(dplyr)
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Recreational Fisheries Decision Support Tool"),
    
    tabsetPanel(
      tabPanel( "Regulation Selection",
                shinyWidgets::awesomeCheckboxGroup(
                  inputId = "state",
                  label = "State", 
                  choices = c("MA", "RI", "CT", "NY", "NJ", "DE",  "MD", "VA", "NC"),
                  inline = TRUE,
                  status = "danger"),
                
                fluidRow( 
                  column(4,
                         titlePanel("Summer Flounder"),
                         sliderInput(inputId = "SFnjFH_seas1", label ="For Hire Open Season 1", # New Jersey for hire season 1
                                     min = as.Date("01-01","%m-%d"),
                                     max = as.Date("12-31","%m-%d"),
                                     value =c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), ###########Testing
                                     timeFormat = "%m-%d", ticks = FALSE),
                         fluidRow(
                           column(5, 
                                  numericInput(inputId = "SFnjFH_1_smbag", label ="Small Bag Limit", ###### Testing
                                               min = 0, max = 7, value = 2), 
                                  sliderInput(inputId = "SFnjFH_1_smlen", label ="Small Min Length",
                                              min = 5, max = 50, value = c(17,18), step = .5)),
                           column(5,
                                  numericInput(inputId = "SFnjFH_1_lgbag", label = "Large Bag Limit",
                                               min = 0, max = 7, value = 1), 
                                  sliderInput(inputId = "SFnjFH_1_lglen", label ="Large Min Length",
                                              min = 5, max = 50, value = c(18,50), step = .5))), 
                         sliderInput(inputId = "SFnjPR_seas1", label ="Private/Rental Open Season 1",  # New Jersey private season 1
                                     min = as.Date("01-01","%m-%d"),
                                     max = as.Date("12-31","%m-%d"),
                                     value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
                                     timeFormat = "%m-%d", ticks = FALSE),
                         fluidRow(
                           column(5, 
                                  numericInput(inputId = "SFnjPR_1_smbag", label ="Small Bag Limit",
                                               min = 0, max = 7, value = 2), 
                                  sliderInput(inputId = "SFnjPR_1_smlen", label ="Small Min Length",
                                              min = 5, max = 50, value = c(17, 18), step = .5)),
                           column(5,
                                  numericInput(inputId = "SFnjPR_1_lgbag", label = "Large Bag Limit",
                                               min = 0, max = 7, value = 1), 
                                  sliderInput(inputId = "SFnjPR_1_lglen", label ="Large Min Length",
                                              min = 5, max = 50, value = c(18, 50), step = .5))),
                         sliderInput(inputId = "SFnjSH_seas1", label ="Shore Open Season 1",  # New Jersey Shore season 1
                                     min = as.Date("01-01","%m-%d"),
                                     max = as.Date("12-31","%m-%d"),
                                     value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
                                     timeFormat = "%m-%d", ticks = FALSE),
                         fluidRow(
                           column(5, 
                                  numericInput(inputId = "SFnjSH_1_smbag", label ="Small Bag Limit",
                                               min = 0, max = 7, value = 2), 
                                  sliderInput(inputId = "SFnjSH_1_smlen", label ="Small Min Length",
                                              min = 5, max = 50, value = c(17, 18), step = .5)),
                           column(5,
                                  numericInput(inputId = "SFnjSH_1_lgbag", label = "Large Bag Limit",
                                               min = 0, max = 7, value = 1), 
                                  sliderInput(inputId = "SFnjSH_1_lglen", label ="Large Min Length",
                                              min = 5, max = 50, value = c(18, 50), step = .5))),
                         actionButton("SFNJaddSeason", "Add Season"), 
                         shinyjs::hidden( div(ID = "SFnjSeason2",
                                              sliderInput(inputId = "SFnjFH_seas2", label ="For Hire Open Season 2", # New Jersey for hire season 2
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),#)),
                                              fluidRow(
                                                column(5,
                                                       numericInput(inputId = "SFnjFH_2_smbag", label ="Small Bag Limit",
                                                                    min = 0, max = 7, value = 0),
                                                       sliderInput(inputId = "SFnjFH_2_smlen", label ="Small Min Length",
                                                                   min = 5, max = 50, value = c(17,18), step = .5)),
                                                column(5,
                                                       numericInput(inputId = "SFnjFH_2_lgbag", label = "Large Bag Limit",
                                                                    min = 0, max = 7, value = 0),
                                                       sliderInput(inputId = "SFnjFH_1_lglen", label ="Large Min Length",
                                                                   min = 5, max = 50, value = c(18,50), step = .5))),
                                              sliderInput(inputId = "SFnjPR_seas2", label ="Private/Rental Open Season 2",  # New Jersey private season 2
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(5,
                                                       numericInput(inputId = "SFnjPR_2_smbag", label ="Small Bag Limit",
                                                                    min = 0, max = 7, value = 0),
                                                       sliderInput(inputId = "SFnjPR_2_smlen", label ="Small Min Length",
                                                                   min = 5, max = 50, value = c(17, 18), step = .5)),
                                                column(5,
                                                       numericInput(inputId = "SFnjPR_2_lgbag", label = "Large Bag Limit",
                                                                    min = 0, max = 7, value = 0),
                                                       sliderInput(inputId = "SFnjPR_2_lglen", label ="Large Min Length",
                                                                   min = 5, max = 50, value = c(18, 50), step = .5))),
                                              sliderInput(inputId = "SFnjSH_seas2", label ="Shore Open Season 2",  # New Jersey Shore season 2
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(5,
                                                       numericInput(inputId = "SFnjSH_2_smbag", label ="Small Bag Limit",
                                                                    min = 0, max = 7, value = 0),
                                                       sliderInput(inputId = "SFnjSH_2_smlen", label ="Small Min Length",
                                                                   min = 5, max = 50, value = c(17, 18), step = .5)),
                                                column(5,
                                                       numericInput(inputId = "SFnjSH_2_lgbag", label = "Large Bag Limit",
                                                                    min = 0, max = 7, value = 0),
                                                       sliderInput(inputId = "SFnjSH_2_lglen", label ="Large Min Length",
                                                                   min = 5, max = 50, value = c(18, 50), step = .5)))))),
                  
                  column(4, 
                         titlePanel("Black Sea Bass"),
                         
                         selectInput("input_type", "Modes to choose from:",
                                     c("Single", "AllModes")),
                         uiOutput("BSBnjMode"),
                         
                         actionButton("BSBNJaddSeason", "Add Season"), 
                         #Season 5
                         shinyjs::hidden( div(ID = "BSBnjSeason5",
                                              sliderInput(inputId = "BSBnjFH_seas5", label =" For Hire Open Season 5", 
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "BSBnjFH_5_bag", label ="Bag Limit",
                                                                    min = 0, max = 20, value = 0)), 
                                                column(6,
                                                       sliderInput(inputId = "BSBnjFH_5_len", label ="Min Length",
                                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                                              sliderInput(inputId = "BSBnjPR_seas5", label ="Private/Rental Open Season 5",
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "BSBnjPR_5_bag", label ="Bag Limit",
                                                                    min = 0, max = 20, value = 0)), 
                                                column(6,
                                                       sliderInput(inputId = "BSBnjPR_5_len", label ="Min Length",
                                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                                              sliderInput(inputId = "BSBnjSH_seas5", label ="Shore Open Season 5",
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "BSBnjSH_5_bag", label ="Bag Limit",
                                                                    min = 0, max = 20, value = 0)), 
                                                column(6,
                                                       sliderInput(inputId = "BSBnjSH_5_len", label ="Min Length",
                                                                   min = 3, max = 28.5, value = 12.5, step = .5)))))),
                  
                  
                  
                  
                  column(4, 
                         titlePanel("Scup"),
                         sliderInput(inputId = "SCUPnj_seas1", label ="Open Season 1",
                                     min = as.Date("01-01","%m-%d"),
                                     max = as.Date("12-31","%m-%d"),
                                     value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                     timeFormat = "%m-%d", ticks = FALSE),
                         fluidRow(
                           column(4,
                                  numericInput(inputId = "SCUPnj_1_bag", label ="Bag Limit",
                                               min = 0, max = 100, value = 50)),
                           column(6,
                                  sliderInput(inputId = "SCUPnj_1_len", label ="Min Length",
                                              min = 5, max = 15, value = 10, step = .5))), 
                         actionButton("SCUPNJaddSeason", "Add Season"), 
                         shinyjs::hidden( div(ID = "SCUPnjSeason2",
                                              sliderInput(inputId = "SCUPnj_seas2", label ="Open Season 2", 
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-30","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "SCUPnj_2_bag", label ="Bag Limit",
                                                                    min = 0, max = 20, value = 0)), 
                                                column(6,
                                                       sliderInput(inputId = "SCUPnj_2_len", label ="Min Length",
                                                                   min = 3, max = 28.5, value = 12.5, step = .5))))))),
                
                
                
                actionButton("runmeplease", "Run Me")),
      
      tabPanel("Results", 
               downloadButton(outputId = "downloadData", "Download"),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Calculating...This may take a minute.",id="loadmessage")),
               fluidRow(
                 column(9, tableOutput(outputId = "keep_release_tableout")),
                 column(3, tableOutput(outputId = "regtableout")),
                 tableOutput(outputId = "welfare_trips_tableout"), 
                 tableOutput(outputId = "mortalityout"),
                 tableOutput(outputId = "futureplansout"))), 
      
      
      tabPanel("Documentation")
    ))
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    library(magrittr) 
    
    
    ############### Toggle extra seasons on front end #############
    shinyjs::onclick("SFNJaddSeason",
                     shinyjs::toggle(id = "SFnjSeason2", anim = TRUE))
    shinyjs::onclick("BSBNJaddSeason",
                     shinyjs::toggle(id = "BSBnjSeason5", anim = TRUE))
    shinyjs::onclick("SCUPNJaddSeason",
                     shinyjs::toggle(id = "SCUPnjSeason2", anim = TRUE))
    #################################################################
    
    
    ############# Breakout by mode ######################################
    output$BSBnjMode <- renderUI({
      if (is.null(input$input_type))
        return()
      
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$input_type,
             
             "Single" = div(sliderInput(inputId = "BSBnj_seas1", label ="Open Season 1", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBnj_1_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 10)), 
                              column(6,
                                     sliderInput(inputId = "BSBnj_1_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 13, step = .5))),
                            
                            #Season 2
                            sliderInput(inputId = "BSBnj_seas2", label ="Open Season 2", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 2)), 
                              column(6,
                                     sliderInput(inputId = "BSBnj_2_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 13, step = .5))),
                            
                            #Season 3
                            sliderInput(inputId = "BSBnj_seas3", label ="Open Season 3", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 10)), 
                              column(6,
                                     sliderInput(inputId = "BSBnj_3_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 13, step = .5))),
                            
                            #Season 4
                            sliderInput(inputId = "BSBnj_seas4", label ="Open Season 4", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBnj_4_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 15)), 
                              column(6,
                                     sliderInput(inputId = "BSBnj_4_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 13, step = .5)))),
             
             "AllModes" = div(sliderInput(inputId = "BSBnjFH_seas1", label =" For Hire Open Season 1", 
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjFH_1_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjPR_seas1", label ="Private/Rental Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjPR_1_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjSH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjSH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjSH_1_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              #Season 2
                              sliderInput(inputId = "BSBnjFH_seas2", label =" For Hire Open Season 2", 
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjFH_2_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjFH_2_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjPR_seas2", label ="Private/Rental Open Season 2",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjPR_2_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjPR_2_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjSH_seas2", label ="Shore Open Season 2",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjSH_2_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjSH_2_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              #Season 3
                              sliderInput(inputId = "BSBnjFH_seas3", label =" For Hire Open Season 3", 
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjFH_3_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjFH_3_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjPR_seas3", label ="Private/Rental Open Season 3",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjPR_3_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjPR_3_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjSH_seas3", label ="Shore Open Season 3",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjSH_3_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjSH_3_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              #Season 4
                              sliderInput(inputId = "BSBnjFH_seas4", label =" For Hire Open Season 4", 
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjFH_4_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjFH_4_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjPR_seas4", label ="Private/Rental Open Season 4",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjPR_4_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjPR_4_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))),
                              sliderInput(inputId = "BSBnjSH_seas4", label ="Shore Open Season 4",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnjSH_4_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 10)), 
                                column(6,
                                       sliderInput(inputId = "BSBnjSH_4_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 12.5, step = .5))))
             
      )
    })
    output$input_type_text <- renderText({
      input$input_type
    })
    
    output$dynamic_value <- renderPrint({
      str(input$dynamic)
    })
    
    ######################################################################
    
    observeEvent(input$runmeplease, {
      
      state <- input$state
      
      ################ Summary Outputs ######################################
      ######################################################################
      source(here::here(paste0("model_run_",state,".R")), local = TRUE)
      
      keep_release <- reactive({
        keep_release_output<- read.csv(here::here(paste0("output_", state, "_1.csv")))  %>% 
          dplyr::left_join(predictions, by = c("Category", "mode", "keep_release", "param", "number_weight", "state")) %>% 
          dplyr::filter(Category %in% c("bsb", "scup","sf"), 
                        param == "Total") %>% 
          dplyr::mutate(#Category = paste(Category, "(lbs)"), 
            StatusQuo = round(StatusQuo, digits = 0), 
            Alternative = round(Value, digits = 0),
            Percent_Change = paste(round(((Alternative/StatusQuo) - 1) * 100, digits = 0), "%" )) %>% 
          dplyr::select(c(Category, mode, keep_release, number_weight, StatusQuo, Alternative, Percent_Change)) %>% 
          tidyr::pivot_wider(names_from = number_weight, values_from = c("StatusQuo", "Alternative", "Percent_Change")) %>% 
          dplyr::rename("StatusQuo_Weight (lbs)" = StatusQuo_Weight, 
                        "Alternative_Weight (lbs)" = Alternative_Weight, 
                        "Species" = Category)
        return(keep_release_output)
      })
      
      
      welfare_ntrips<- reactive({
        welfare_output<- read.csv(here::here(paste0("output_", state, "_1.csv"))) %>% 
          dplyr::left_join(predictions, by = c("Category", "mode", "keep_release", "number_weight", "state")) %>% 
          dplyr::filter(Category %in% c("CV", "ntrips")) %>% 
          dplyr::mutate(Category = dplyr::case_when(stringr::str_detect(Category, "CV") ~ paste(Category, "($)"), 
                                                    stringr::str_detect(Category, "ntrips") ~ Category),
                        StatusQuo = round(StatusQuo, digits = 2), 
                        Alternative = round(Value, digits = 2),
                        Percent_Change = paste(round(((Alternative/StatusQuo) - 1) * 100, digits = 0), "%" )) %>% 
          dplyr::select(c(Category, mode, StatusQuo, Alternative, Percent_Change)) 
        return(welfare_output)
      })
      
      regulations <- reactive({
        SFnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnjFH_seas1[1], "-", input$SFnjFH_seas1[2]),
                                    BagLimit = paste(input$SFnjFH_1_smbag,",", input$SFnjFH_1_lgbag),
                                    Length = paste(input$SFnjFH_1_smlen[1],"-", input$SFnjFH_1_smlen[2],",",input$SFnjFH_1_lglen[1],"-",input$SFnjFH_1_lglen[2]))
        SFnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnjFH_seas2[1], "-", input$SFnjFH_seas2[2]),
                                    BagLimit = paste(input$SFnjFH_2_smbag,",", input$SFnjFH_2_lgbag),
                                    Length = paste(input$SFnjFH_2_smlen[1],"-", input$SFnjFH_2_smlen[2],",",input$SFnjFH_2_lglen[1],"-",input$SFnjFH_2_lglen[2]))
        SFnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnjPR_seas1[1], "-", input$SFnjPR_seas1[2]),
                                    BagLimit = paste(input$SFnjPR_1_smbag,",", input$SFnjPR_1_lgbag),
                                    Length = paste(input$SFnjPR_1_smlen[1],"-", input$SFnjPR_1_smlen[2],",",input$SFnjPR_1_lglen[1],"-",input$SFnjPR_1_lglen[2]))
        SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]),
                                    BagLimit = paste(input$SFnjPR_2_smbag,",", input$SFnjPR_2_lgbag),
                                    Length = paste(input$SFnjPR_2_smlen[1],"-", input$SFnjPR_2_smlen[2],",",input$SFnjPR_2_lglen[1],"-",input$SFnjPR_2_lglen[2]))
        SFnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas1[1], "-", input$SFnjPR_seas1[2]),
                                    BagLimit = paste(input$SFnjSH_1_smbag,",", input$SFnjPR_1_lgbag),
                                    Length = paste(input$SFnjSH_1_smlen[1],"-", input$SFnjPR_1_smlen[2],",",input$SFnjSH_1_lglen[1],"-",input$SFnjSH_1_lglen[2]))
        SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
                                    BagLimit = paste(input$SFnjSH_2_smbag,",", input$SFnjSH_2_lgbag),
                                    Length = paste(input$SFnjSH_2_smlen[1],"-", input$SFnjSH_2_smlen[2],",",input$SFnjSH_2_lglen[1],"-",input$SFnjSH_2_lglen[2]))
        
        if(input$input_type == "Single"){
          BSBnjseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                     Season = paste(input$BSBnj_seas1[1], "-", input$BSBnj_seas1[2]),
                                     BagLimit = paste(input$BSBnj_1_bag),
                                     Length = paste(input$BSBnj_1_len))
          BSBnjseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                     Season = paste(input$BSBnj_seas2[1], "-", input$BSBnj_seas2[2]),
                                     BagLimit = paste(input$BSBnj_2_bag),
                                     Length = paste(input$BSBnj_2_len))
          BSBnjseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                     Season = paste(input$BSBnj_seas3[1], "-", input$BSBnj_seas3[2]),
                                     BagLimit = paste(input$BSBnj_3_bag),
                                     Length = paste(input$BSBnj_3_len))
          BSBnjseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                     Season = paste(input$BSBnj_seas4[1], "-", input$BSBnj_seas4[2]),
                                     BagLimit = paste(input$BSBnj_4_bag),
                                     Length = paste(input$BSBnj_4_len))
          
          
          
          BSBnj<- rbind(BSBnjseason1, BSBnjseason2, BSBnjseason3, BSBnjseason4)
        }else{
          
          BSBnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                       Season = paste(input$BSBnjFH_seas1[1], "-", input$BSBnjFH_seas1[2]),
                                       BagLimit = paste(input$BSBnjFH_1_bag),
                                       Length = paste(input$BSBnjFH_1_len))
          BSBnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                       Season = paste(input$BSBnjFH_seas2[1], "-", input$BSBnjFH_seas2[2]),
                                       BagLimit = paste(input$BSBnjFH_2_bag),
                                       Length = paste(input$BSBnjFH_2_len))
          BSBnjFHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                       Season = paste(input$BSBnjFH_seas3[1], "-", input$BSBnjFH_seas3[2]),
                                       BagLimit = paste(input$BSBnjFH_3_bag),
                                       Length = paste(input$BSBnjFH_3_len))
          BSBnjFHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                       Season = paste(input$BSBnjFH_seas4[1], "-", input$BSBnjFH_seas4[2]),
                                       BagLimit = paste(input$BSBnjFH_4_bag),
                                       Length = paste(input$BSBnjFH_4_len))
          BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                       Season = paste(input$BSBnjFH_seas5[1], "-", input$BSBnjFH_seas5[2]),
                                       BagLimit = paste(input$BSBnjFH_5_bag),
                                       Length = paste(input$BSBnjFH_5_len))
          
          BSBnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                       Season = paste(input$BSBnjPR_seas1[1], "-", input$BSBnjPR_seas1[2]),
                                       BagLimit = paste(input$BSBnjPR_1_bag),
                                       Length = paste(input$BSBnjPR_1_len))
          BSBnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                       Season = paste(input$BSBnjPR_seas2[1], "-", input$BSBnjPR_seas2[2]),
                                       BagLimit = paste(input$BSBnjPR_2_bag),
                                       Length = paste(input$BSBnjPR_2_len))
          BSBnjPRseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                       Season = paste(input$BSBnjPR_seas3[1], "-", input$BSBnjPR_seas3[2]),
                                       BagLimit = paste(input$BSBnjPR_3_bag),
                                       Length = paste(input$BSBnjPR_3_len))
          BSBnjPRseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                       Season = paste(input$BSBnjPR_seas4[1], "-", input$BSBnjPR_seas4[2]),
                                       BagLimit = paste(input$BSBnjPR_4_bag),
                                       Length = paste(input$BSBnjPR_4_len))
          BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                       Season = paste(input$BSBnjPR_seas5[1], "-", input$BSBnjPR_seas5[2]),
                                       BagLimit = paste(input$BSBnjPR_5_bag),
                                       Length = paste(input$BSBnjPR_5_len))
          
          BSBnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                       Season = paste(input$BSBnjSH_seas1[1], "-", input$BSBnjSH_seas1[2]),
                                       BagLimit = paste(input$BSBnjSH_1_bag),
                                       Length = paste(input$BSBnjSH_1_len))
          BSBnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                       Season = paste(input$BSBnjSH_seas2[1], "-", input$BSBnjSH_seas2[2]),
                                       BagLimit = paste(input$BSBnjSH_2_bag),
                                       Length = paste(input$BSBnjSH_2_len))
          BSBnjSHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                       Season = paste(input$BSBnjSH_seas3[1], "-", input$BSBnjSH_seas3[2]),
                                       BagLimit = paste(input$BSBnjSH_3_bag),
                                       Length = paste(input$BSBnjSH_3_len))
          BSBnjSHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                       Season = paste(input$BSBnjSH_seas4[1], "-", input$BSBnjSH_seas4[2]),
                                       BagLimit = paste(input$BSBnjSH_4_bag),
                                       Length = paste(input$BSBnjSH_4_len))
          BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                       Season = paste(input$BSBnjSH_seas5[1], "-", input$BSBnjSH_seas5[2]),
                                       BagLimit = paste(input$BSBnjSH_5_bag),
                                       Length = paste(input$BSBnjSH_5_len))
          
          
          BSBnj<- rbind(BSBnjFHseason1, BSBnjFHseason2, BSBnjFHseason3, BSBnjFHseason4, BSBnjFHseason5,
                        BSBnjPRseason1, BSBnjPRseason2, BSBnjPRseason3, BSBnjPRseason4, BSBnjPRseason5,
                        BSBnjSHseason1, BSBnjSHseason2, BSBnjSHseason3, BSBnjSHseason4, BSBnjSHseason5)
        }
        
        
        
        
        SCUPnjseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPnj_seas1[1], "-", input$SCUPnj_seas1[2]),
                                    BagLimit = paste(input$SCUPnj_1_bag),
                                    Length = paste(input$SCUPnj_1_len))
        
        
        
        regs_output<- rbind(SFnjPRseason1, SFnjPRseason2, SFnjFHseason1, SFnjFHseason2, SFnjSHseason1, SFnjSHseason2,
                            BSBnj, SCUPnjseason1) %>%
          dplyr::filter(!BagLimit == "0",
                        !BagLimit == "0 , 0") %>%
          dplyr::mutate(Season = stringr::str_remove(Season, pattern = "2023-"),
                        Season = stringr::str_remove(Season, pattern = "2023-"))
        return(regs_output)
      })
      
      mortality<- reactive({
        mortality_output<- read.csv(here::here(paste0("output_", state, "_1.csv"))) %>% 
          dplyr::left_join(predictions, by = c("Category", "mode", "keep_release", "param", "number_weight", "state")) %>% 
          dplyr::filter(Category %in% c("bsb", "scup","sf"), 
                        param == "Mortality") %>% 
          dplyr::mutate(#Category = paste(Category, "(lbs)"), 
            StatusQuo = round(StatusQuo, digits = 0), 
            Alternative = round(Value, digits = 0),
            Percent_Change = paste(round(((Alternative/StatusQuo) - 1) * 100, digits = 0), "%" )) %>% 
          dplyr::select(c(Category, mode, number_weight, StatusQuo, Alternative, Percent_Change)) %>% 
          tidyr::pivot_wider(names_from = number_weight, values_from = c("StatusQuo", "Alternative", "Percent_Change")) %>% 
          dplyr::rename("StatusQuo_Mort_Weight (lbs)" = StatusQuo_Weight, 
                        "Alternative_Mort_Weight (lbs)" = Alternative_Weight, 
                        "StatusQuo_Mort_Number" = StatusQuo_Number, 
                        "Alternative_Mort_Number" = Alternative_Number, 
                        "Species" = Category)
        return(mortality_output)
      })
     
      
      ## Output Tables 
      output$keep_release_tableout<- renderTable({
       keep_release()
      })
      
      
      output$welfare_trips_tableout<- renderTable({
       welfare_ntrips()
      })
      
      
      output$regtableout <- renderTable({
        regulations()
       
      })
      
      output$mortalityout <- renderTable({
        mortality()
      })
      
      output$futureplansout <- renderTable({
        futureout <- data.frame(Variable =c("Total Mortality", "Discard Mortality", 
                                            "% of runs that result in desired outcome", 
                                            "Catch by weight", "Incorporating Avidity and Angler Age"), 
                                Notes = c("These are topics we are currently working to incorporate in the model and/or outputs. We just aren't quite there yet to share.", 
                                          "Done", "", "Done", "Done"))
        
        
      })
      
      
      
      
     
      
      output$downloadData <- downloadHandler(
        filename = function(){"RecDSToutput.xlsx"},
        content = function(filename) {
          
          df_list <- list(Regulations=regulations(), Keep_Release=keep_release(), 
                          Angler_Welfare = welfare_ntrips(), Discard_Mortality = mortality())
          openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
        })
      
      })#})
    
    
    
    
  }

#Message of where the model is in the run. Don't know where to put this
#showNotification(strong(paste("Running model", x,"/100 for", state)),duration=NULL,id="running",type="message")

shiny::shinyApp(ui = ui, server = server)
