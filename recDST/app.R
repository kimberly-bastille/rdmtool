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
                strong(div("REMINDER: (1) select state(s) - Just New Jersey included for now. (2) Make selections below (3) click run me and then the `Results` tab to run model", style = "color:blue")),
                shinyWidgets::awesomeCheckboxGroup(
                  inputId = "state",
                  label = "State", 
                  choices = c("MA", "RI", "CT", "NY", "NJ", "DE",  "MD", "VA", "NC"),
                  inline = TRUE,
                  status = "danger"),
                
                fluidRow( 
                  column(4,
                         titlePanel("Summer Flounder"),
                         
                         
                         selectInput("SF_NJ_input_type", "Regulations combined or seperated by mode?",
                                     c("All Modes Combined", "Seperated By Mode")),
                         uiOutput("SFnjMode"),
                         
                         
                         actionButton("SFNJaddSeason", "Add Season"), 
                         shinyjs::hidden( div(ID = "SFnjSeason2",
                                              sliderInput(inputId = "SFnjFH_seas2", label ="For Hire Open Season 2", # New Jersey for hire season 2
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
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
                                                       sliderInput(inputId = "SFnjFH_2_lglen", label ="Large Min Length",
                                                                   min = 5, max = 50, value = c(18,50), step = .5))),
                                              sliderInput(inputId = "SFnjPR_seas2", label ="Private/Rental Open Season 2",  # New Jersey private season 2
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
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
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
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
                         
                         selectInput("BSB_NJ_input_type", "Regulations combined or seperated by mode?",
                                     c("All Modes Combined", "Seperated By Mode")),
                         uiOutput("BSBnjMode"),
                         
                         actionButton("BSBNJaddSeason", "Add Season"), 
                         #Season 5
                         shinyjs::hidden( div(ID = "BSBnjSeason5",
                                              sliderInput(inputId = "BSBnjFH_seas5", label =" For Hire Open Season 5", 
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
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
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
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
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
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
                          
                         selectInput("SCUP_NJ_input_type", "Regulations combined or seperated by mode?",
                                     c("All Modes Combined", "Seperated By Mode")),
                         uiOutput("SCUPnjMode"),
                         
                         actionButton("SCUPNJaddSeason", "Add Season"), 
                         shinyjs::hidden( div(ID = "SCUPnjSeason2",
                                              sliderInput(inputId = "SCUPnjFH_seas2", label ="For Hire Open Season 2",
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "SCUPnjFH_2_bag", label ="Bag Limit",
                                                                    min = 0, max = 100, value = 0)),
                                                column(6,
                                                       sliderInput(inputId = "SCUPnjFH_2_len", label ="Min Length",
                                                                   min = 5, max = 15, value = 10, step = .5))), 
                                              sliderInput(inputId = "SCUPnjPR_seas2", label ="Private Open Season 2",
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "SCUPnjPR_2_bag", label ="Bag Limit",
                                                                    min = 0, max = 100, value = 0)),
                                                column(6,
                                                       sliderInput(inputId = "SCUPnjPR_2_len", label ="Min Length",
                                                                   min = 5, max = 15, value = 10, step = .5))), 
                                              sliderInput(inputId = "SCUPnjSH_seas2", label ="Shore Open Season 2",
                                                          min = as.Date("01-01","%m-%d"),
                                                          max = as.Date("12-31","%m-%d"),
                                                          value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                          timeFormat = "%m-%d", ticks = FALSE),
                                              fluidRow(
                                                column(4,
                                                       numericInput(inputId = "SCUPnjSH_2_bag", label ="Bag Limit",
                                                                    min = 0, max = 100, value = 0)),
                                                column(6,
                                                       sliderInput(inputId = "SCUPnjSH_2_len", label ="Min Length",
                                                                   min = 5, max = 15, value = 10, step = .5))))))),
                
                
                
                actionButton("runmeplease", "Run Me")),
      
      tabPanel("Results", 
               strong(div("REMINDER: (1) Do not click any buttons in this tool once while it says `Calculating`! (2) Be sure to download data (3) When finished with tool, click `Stop App` and close out of the window. ", style = "color:blue")),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Calculating...This will take ~15 min.",id="loadmessage")),
               
                 downloadButton(outputId = "downloadData", "Download"),
                 tableOutput(outputId = "regtableout"),
                 tableOutput(outputId = "welfare_tableout"), 
                 tableOutput(outputId = "keep_tableout"),
                 tableOutput(outputId = "mortalityout"),
                 tableOutput(outputId = "ntrips_tableout")),
      
      
      tabPanel("Documentation")#, 
               #htmlOutput("markdown"))
    ))
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    if (!interactive()) sink(stderr(), type = "output")
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
    output$SFnjMode <- renderUI({
      if (is.null(input$SF_NJ_input_type))
        return()
      
      switch(input$SF_NJ_input_type, 
             "All Modes Combined" = div(sliderInput(inputId = "SFnj_seas1", label ="Open Season 1", # New Jersey for hire season 1
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value =c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(5, 
                                     numericInput(inputId = "SFnj_1_smbag", label ="Small Bag Limit", 
                                                  min = 0, max = 7, value = 2), 
                                     sliderInput(inputId = "SFnj_1_smlen", label ="Small Min Length",
                                                 min = 5, max = 50, value = c(17,18), step = .5)),
                              column(5,
                                     numericInput(inputId = "SFnj_1_lgbag", label = "Large Bag Limit",
                                                  min = 0, max = 7, value = 1), 
                                     sliderInput(inputId = "SFnj_1_lglen", label ="Large Min Length",
                                                 min = 5, max = 50, value = c(18,50), step = .5)))), 
             "Seperated By Mode" = div(sliderInput(inputId = "SFnjFH_seas1", label ="For Hire Open Season 1", # New Jersey for hire season 1
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value =c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(5, 
                                       numericInput(inputId = "SFnjFH_1_smbag", label ="Small Bag Limit", 
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
                                                   min = 5, max = 50, value = c(18, 50), step = .5)))))
    })
    
    
    
    
    
    
    
    output$BSBnjMode <- renderUI({
      if (is.null(input$BSB_NJ_input_type))
        return()
      
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$BSB_NJ_input_type,
             
             "All Modes Combined" = div(sliderInput(inputId = "BSBnj_seas1", label ="Open Season 1", 
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
                                                 min = 3, max = 28.5, value = 12.5, step = .5))),
                            
                            #Season 2
                            sliderInput(inputId = "BSBnj_seas2", label ="Open Season 2", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 1)), 
                              column(6,
                                     sliderInput(inputId = "BSBnj_2_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 12.5, step = .5))),
                            
                            #Season 3
                            sliderInput(inputId = "BSBnj_seas3", label ="Open Season 3", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("10-01","%m-%d"),as.Date("10-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 10)), 
                              column(6,
                                     sliderInput(inputId = "BSBnj_3_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 12.5, step = .5))),
                            
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
                                                 min = 3, max = 28.5, value = 12.5, step = .5)))),
             
             "Seperated By Mode" = div(sliderInput(inputId = "BSBnjFH_seas1", label =" For Hire Open Season 1", 
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
                                                   min = 3, max = 28.5, value = 12.5, step = .5)))))
    })
      
      output$SCUPnjMode <- renderUI({
        if (is.null(input$SCUP_NJ_input_type))
          return()
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$SCUP_NJ_input_type,
               
               "All Modes Combined" = div(sliderInput(inputId = "SCUPnj_seas1", label ="Open Season 1",
                                           min = as.Date("01-01","%m-%d"),
                                           max = as.Date("12-31","%m-%d"),
                                           value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                           timeFormat = "%m-%d", ticks = FALSE),
                               fluidRow(
                                 column(4,
                                        numericInput(inputId = "SCUPnj_1_bag", label ="Bag Limit",
                                                     min = 0, max = 100, value = 30)),
                                 column(6,
                                        sliderInput(inputId = "SCUPnj_1_len", label ="Min Length",
                                                    min = 5, max = 15, value = 10, step = .5)))),
               "Seperated By Mode" = div(sliderInput(inputId = "SCUPnjFH_seas1", label ="For Hire Open Season 1",
                                            min = as.Date("01-01","%m-%d"),
                                            max = as.Date("12-31","%m-%d"),
                                            value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                            timeFormat = "%m-%d", ticks = FALSE),
                                fluidRow(
                                  column(4,
                                         numericInput(inputId = "SCUPnjFH_1_bag", label ="Bag Limit",
                                                      min = 0, max = 100, value = 30)),
                                  column(6,
                                         sliderInput(inputId = "SCUPnjFH_1_len", label ="Min Length",
                                                     min = 5, max = 15, value = 10, step = .5))), 
                                sliderInput(inputId = "SCUPnjPR_seas1", label ="Private Open Season 1",
                                            min = as.Date("01-01","%m-%d"),
                                            max = as.Date("12-31","%m-%d"),
                                            value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                            timeFormat = "%m-%d", ticks = FALSE),
                                fluidRow(
                                  column(4,
                                         numericInput(inputId = "SCUPnjPR_1_bag", label ="Bag Limit",
                                                      min = 0, max = 100, value = 30)),
                                  column(6,
                                         sliderInput(inputId = "SCUPnjPR_1_len", label ="Min Length",
                                                     min = 5, max = 15, value = 10, step = .5))), 
                                sliderInput(inputId = "SCUPnjSH_seas1", label ="Shore Open Season 1",
                                            min = as.Date("01-01","%m-%d"),
                                            max = as.Date("12-31","%m-%d"),
                                            value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                            timeFormat = "%m-%d", ticks = FALSE),
                                fluidRow(
                                  column(4,
                                         numericInput(inputId = "SCUPnjSH_1_bag", label ="Bag Limit",
                                                      min = 0, max = 100, value = 30)),
                                  column(6,
                                         sliderInput(inputId = "SCUPnjSH_1_len", label ="Min Length",
                                                     min = 5, max = 15, value = 10, step = .5)))))
        })
    
    ### New Jersey Output
    output$SF_NJ_input_type_text <- renderText({
      input$SF_NJ_input_type
    })
    output$BSB_NJ_input_type_text <- renderText({
      input$BSB_NJ_input_type
    })
    output$SCUP_NJ_input_type_text <- renderText({
      input$SCUP_NJ_input_type
    })
    
    output$dynamic_value <- renderPrint({
      str(input$dynamic)
    })
    
    ##################################################################
 
    predictions_1 <- eventReactive(input$runmeplease,{
      
      predictions_1 <- NULL
      for(i in 1:length(input$state)){
        state_name <- input$state[i]
        
        source(here::here(paste0("model_run_",state_name,".R")), local = TRUE)
        
        predictions_1 <- predictions_1 %>% rbind(predictions)
        return(predictions_1)
      }
    })
    
    
    keep <- reactive({
      keep_output<- predictions_1() %>% 
        dplyr::filter(Statistic %in% c("harvest pounds", "harvest numbers")) %>% 
        dplyr::arrange(factor(Statistic, levels = c("harvest pounds", "harvest numbers")))
      return(keep_output)
    })
    
    mortality<- reactive({
      mortality_output<- predictions_1() %>% 
        dplyr::filter(Statistic %in% c("release pounds", "dead release pounds", 
                                       "release numbers", "dead release numbers")) %>% 
        dplyr::select(! "% under harvest target (out of 100 simulations)") %>% 
        dplyr::arrange(factor(Statistic, levels = c("release pounds", "release numbers", "dead release pounds", "dead release numbers")))
      return(mortality_output)
    })
    
    welfare<- reactive({
      welfare_output<- predictions_1() %>% 
        dplyr::filter(Statistic %in% c("CV")) %>% 
        dplyr::mutate(Statistic = dplyr::recode(Statistic, "CV" = "Change in angler satisfaction ($)")) %>% 
        dplyr::select(! c("% under harvest target (out of 100 simulations)","Status-quo value (median)","% difference from status-quo outcome (median)"))
      return(welfare_output)
    })
    
    ntrips<- reactive({
      ntrips_output<- predictions_1() %>% 
        dplyr::filter(Statistic %in% c( "ntrips")) %>% 
        dplyr::mutate(Statistic = dplyr::recode(Statistic, "ntrips" = "Total estimate trips")) %>% 
        dplyr::select(! c("% under harvest target (out of 100 simulations)","Status-quo value (median)","% difference from status-quo outcome (median)"))
      return(ntrips_output)
    })
    
    
    regulations <- reactive({
      
      if(input$state == "NJ"){  
      if(input$SF_NJ_input_type == "All Modes Combined"){
        
        SFnjseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFnj_seas1[1], "-", input$SFnj_seas1[2]),
                                  BagLimit = paste(input$SFnj_1_smbag,",", input$SFnj_1_lgbag),
                                  Length = paste(input$SFnj_1_smlen[1],"-", input$SFnj_1_smlen[2],",",input$SFnj_1_lglen[1],"-",input$SFnj_1_lglen[2]))
        SFnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnjFH_seas2[1], "-", input$SFnjFH_seas2[2]),
                                    BagLimit = paste(input$SFnjFH_2_smbag,",", input$SFnjFH_2_lgbag),
                                    Length = paste(input$SFnjFH_2_smlen[1],"-", input$SFnjFH_2_smlen[2],",",input$SFnjFH_2_lglen[1],"-",input$SFnjFH_2_lglen[2]))
        SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]),
                                    BagLimit = paste(input$SFnjPR_2_smbag,",", input$SFnjPR_2_lgbag),
                                    Length = paste(input$SFnjPR_2_smlen[1],"-", input$SFnjPR_2_smlen[2],",",input$SFnjPR_2_lglen[1],"-",input$SFnjPR_2_lglen[2]))
        SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
                                    BagLimit = paste(input$SFnjSH_2_smbag,",", input$SFnjSH_2_lgbag),
                                    Length = paste(input$SFnjSH_2_smlen[1],"-", input$SFnjSH_2_smlen[2],",",input$SFnjSH_2_lglen[1],"-",input$SFnjSH_2_lglen[2]))
        SFnj <- rbind(SFnjseason1, SFnjFHseason2, SFnjPRseason2, SFnjSHseason2)
      } else {
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
                                    Season = paste(input$SFnjSH_seas1[1], "-", input$SFnjSH_seas1[2]),
                                    BagLimit = paste(input$SFnjSH_1_smbag,",", input$SFnjSH_1_lgbag),
                                    Length = paste(input$SFnjSH_1_smlen[1],"-", input$SFnjSH_1_smlen[2],",",input$SFnjSH_1_lglen[1],"-",input$SFnjSH_1_lglen[2]))
        SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
                                    BagLimit = paste(input$SFnjSH_2_smbag,",", input$SFnjSH_2_lgbag),
                                    Length = paste(input$SFnjSH_2_smlen[1],"-", input$SFnjSH_2_smlen[2],",",input$SFnjSH_2_lglen[1],"-",input$SFnjSH_2_lglen[2]))
        
        SFnj <- rbind(SFnjFHseason1,SFnjFHseason2, SFnjPRseason1,SFnjPRseason2, SFnjSHseason1,SFnjSHseason2)
      }
      
      if(input$BSB_NJ_input_type == "All Modes Combined"){
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
        
        BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas5[1], "-", input$BSBnjFH_seas5[2]),
                                     BagLimit = paste(input$BSBnjFH_5_bag),
                                     Length = paste(input$BSBnjFH_5_len))
        BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas5[1], "-", input$BSBnjPR_seas5[2]),
                                     BagLimit = paste(input$BSBnjPR_5_bag),
                                     Length = paste(input$BSBnjPR_5_len))
        BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas5[1], "-", input$BSBnjSH_seas5[2]),
                                     BagLimit = paste(input$BSBnjSH_5_bag),
                                     Length = paste(input$BSBnjSH_5_len))
        
        BSBnj<- rbind(BSBnjseason1, BSBnjseason2, BSBnjseason3, BSBnjseason4, BSBnjFHseason5, BSBnjPRseason5, BSBnjSHseason5)
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
      
      
      
      if(input$SCUP_NJ_input_type == "All Modes Combined"){
        SCUPnjseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPnj_seas1[1], "-", input$SCUPnj_seas1[2]),
                                    BagLimit = paste(input$SCUPnj_1_bag),
                                    Length = paste(input$SCUPnj_1_len))
        
        SCUPnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPnjFH_seas2[1], "-", input$SCUPnjFH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjFH_2_bag),
                                      Length = paste(input$SCUPnjFH_2_len))
        SCUPnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPnjPR_seas2[1], "-", input$SCUPnjPR_seas2[2]),
                                      BagLimit = paste(input$SCUPnjPR_2_bag),
                                      Length = paste(input$SCUPnjPR_2_len))
        SCUPnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPnjSH_seas2[1], "-", input$SCUPnjSH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjSH_2_bag),
                                      Length = paste(input$SCUPnjSH_2_len))
        
        SCUPnj <- rbind(SCUPnjseason1, SCUPnjFHseason2, SCUPnjPRseason2, SCUPnjSHseason2)
      } else {
        SCUPnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPnjFH_seas1[1], "-", input$SCUPnjFH_seas1[2]),
                                      BagLimit = paste(input$SCUPnjFH_1_bag),
                                      Length = paste(input$SCUPnjFH_1_len))
        SCUPnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPnjPR_seas1[1], "-", input$SCUPnjPR_seas1[2]),
                                      BagLimit = paste(input$SCUPnjPR_1_bag),
                                      Length = paste(input$SCUPnjPR_1_len))
        SCUPnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPnjSH_seas1[1], "-", input$SCUPnjSH_seas1[2]),
                                      BagLimit = paste(input$SCUPnjSH_1_bag),
                                      Length = paste(input$SCUPnjSH_1_len))
        
        SCUPnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPnjFH_seas2[1], "-", input$SCUPnjFH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjFH_2_bag),
                                      Length = paste(input$SCUPnjFH_2_len))
        SCUPnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPnjPR_seas2[1], "-", input$SCUPnjPR_seas2[2]),
                                      BagLimit = paste(input$SCUPnjPR_2_bag),
                                      Length = paste(input$SCUPnjPR_2_len))
        SCUPnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPnjSH_seas2[1], "-", input$SCUPnjSH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjSH_2_bag),
                                      Length = paste(input$SCUPnjSH_2_len))
        
        SCUPnj <- rbind(SCUPnjFHseason1, SCUPnjPRseason1, SCUPnjSHseason1, SCUPnjFHseason2, SCUPnjPRseason2, SCUPnjSHseason2)
      }}
      
      
      regs_output<- rbind(SFnj, BSBnj, SCUPnj) %>%
        dplyr::filter(!BagLimit == "0",
                      !BagLimit == "0 , 0") %>%
        dplyr::mutate(Season = stringr::str_remove(Season, pattern = "2023-"),
                      Season = stringr::str_remove(Season, pattern = "2023-")) %>% 
        dplyr::rename("Bag Limit" = BagLimit)
      return(regs_output)
    })
    
    
    ## Output Tables 
    output$keep_tableout<- renderTable({
      keep()
    })
    
    output$welfare_tableout<- renderTable({
      welfare()
    })
    
    output$ntrips_tableout<- renderTable({
      ntrips()
    })
    
    output$regtableout <- renderTable({
      regulations()
    })
    
    output$mortalityout <- renderTable({
      mortality()
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function(){"RecDSToutput.xlsx"},
      content = function(filename) {
        
        df_list <- list(Regulations=regulations(), Keep_Release=keep(), 
                        Change_Angler_Satisfaction = welfare(),
                        Estimated_Trips = ntrips(),
                        Discard_Mortality = mortality())
        openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
      })
    
    # output$markdown <- renderUI({
    #   HTML(markdown::markdownToHTML(knitr::knit(here::here('docs/documentation.Rmd'), quiet = TRUE)))
    # })

}

shiny::shinyApp(ui = ui, server = server)
