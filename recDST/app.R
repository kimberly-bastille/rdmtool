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
                
                uiOutput("addMA"),
                uiOutput("addRI"),
                uiOutput("addCT"), 
                uiOutput("addNY"),
                uiOutput("addNJ"), 
                uiOutput("addDE"),
                uiOutput("addMD"),
                uiOutput("addVA"), 
                uiOutput("addNC"),
                
                actionButton("runmeplease", "Run Me")),
      
      tabPanel("Results", 
               
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Calculating...This may take a minute.",id="loadmessage")),
               
                 downloadButton(outputId = "downloadData", "Download"),
                 tableOutput(outputId = "regtableout"),
                 tableOutput(outputId = "keep_release_tableout"),
                 tableOutput(outputId = "welfare_trips_tableout"), 
                 tableOutput(outputId = "mortalityout"),
                 tableOutput(outputId = "futureplansout")), 
      
      
      tabPanel("Documentation")#, 
               #htmlOutput("markdown"))
    ))
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    library(magrittr) 
    
    
    ############### Toggle extra seasons on front end #############
    shinyjs::onclick("SFMAaddSeason",
                     shinyjs::toggle(id = "SFmaSeason2", anim = TRUE))
    shinyjs::onclick("BSBMAaddSeason",
                     shinyjs::toggle(id = "BSBmaSeason2", anim = TRUE))
    shinyjs::onclick("SCUPMAaddSeason",
                     shinyjs::toggle(id = "SCUPmaSeason2", anim = TRUE))
    
    shinyjs::onclick("SFRIaddSeason",
                     shinyjs::toggle(id = "SFriSeason2", anim = TRUE))
    shinyjs::onclick("BSBRIaddSeason",
                     shinyjs::toggle(id = "BSBriSeason3", anim = TRUE))
    shinyjs::onclick("SCUPRIaddSeason",
                     shinyjs::toggle(id = "SCUPriSeason2", anim = TRUE))
    
    shinyjs::onclick("SFCTaddSeason",
                     shinyjs::toggle(id = "SFctSeason2", anim = TRUE))
    shinyjs::onclick("BSBCTaddSeason",
                     shinyjs::toggle(id = "BSBctSeason2", anim = TRUE))
    shinyjs::onclick("SCUPCTaddSeason",
                     shinyjs::toggle(id = "SCUPctSeason2", anim = TRUE))
    
    shinyjs::onclick("SFNYaddSeason",
                     shinyjs::toggle(id = "SFnySeason2", anim = TRUE))
    shinyjs::onclick("BSBNYaddSeason",
                     shinyjs::toggle(id = "BSBnySeason3", anim = TRUE))
    shinyjs::onclick("SCUPNYaddSeason",
                     shinyjs::toggle(id = "SCUPnySeason2", anim = TRUE))
    
    shinyjs::onclick("SFNJaddSeason",
                     shinyjs::toggle(id = "SFnjSeason2", anim = TRUE))
    shinyjs::onclick("BSBNJaddSeason",
                     shinyjs::toggle(id = "BSBnjSeason5", anim = TRUE))
    shinyjs::onclick("SCUPNJaddSeason",
                     shinyjs::toggle(id = "SCUPnjSeason2", anim = TRUE))
    
    shinyjs::onclick("SFDEaddSeason",
                     shinyjs::toggle(id = "SFdeSeason2", anim = TRUE))
    shinyjs::onclick("BSBDEaddSeason",
                     shinyjs::toggle(id = "BSBdeSeason3", anim = TRUE))
    shinyjs::onclick("SCUPDEaddSeason",
                     shinyjs::toggle(id = "SCUPdeSeason2", anim = TRUE))
    
    shinyjs::onclick("SFMDaddSeason",
                     shinyjs::toggle(id = "SFmdSeason2", anim = TRUE))
    shinyjs::onclick("BSBMDaddSeason",
                     shinyjs::toggle(id = "BSBmdSeason3", anim = TRUE))
    shinyjs::onclick("SCUPMDaddSeason",
                     shinyjs::toggle(id = "SCUPmdSeason2", anim = TRUE))
    
    shinyjs::onclick("SFVAaddSeason",
                     shinyjs::toggle(id = "SFvaSeason2", anim = TRUE))
    shinyjs::onclick("BSBVAaddSeason",
                     shinyjs::toggle(id = "BSBvaSeason3", anim = TRUE))
    shinyjs::onclick("SCUPVAaddSeason",
                     shinyjs::toggle(id = "SCUPvaSeason2", anim = TRUE))
    
    shinyjs::onclick("SFNCaddSeason",
                     shinyjs::toggle(id = "SFncSeason2", anim = TRUE))
    shinyjs::onclick("BSBNCaddSeason",
                     shinyjs::toggle(id = "BSBncSeason3", anim = TRUE))
    shinyjs::onclick("SCUPNCaddSeason",
                     shinyjs::toggle(id = "SCUPncSeason2", anim = TRUE))
    #################################################################
    
    ##############MASSACHUSETTS ###########################################################
    output$addMA <- renderUI({
      if(any("MA" == input$state)){
        fluidRow( 
          column(4,
                 titlePanel("Summer Flounder - MA"),
                 
                 selectInput("SF_MA_input_type", "Modes to choose from:",
                             c("Single", "AllModes")),
                 uiOutput("SFmaMode"),
                 
                 actionButton("SFMAaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SFmaSeason2",
                                      sliderInput(inputId = "SFmaFH_seas2", label ="For Hire Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmaFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFmaFH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5))), 
                                      sliderInput(inputId = "SFmaPR_seas2", label ="Private Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmaPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFmaPR_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5))), 
                                      sliderInput(inputId = "SFmaSH_seas2", label ="Shore Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmaSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFmaSH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5)))))),
          
          column(4, 
                 titlePanel("Black Sea Bass - MA"),
                 
                 selectInput("BSB_MA_input_type", "Modes to choose from:",
                             c("Single", "AllModes")),
                 uiOutput("BSBmaMode"),
          
                 
                 actionButton("BSBMAaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "BSBmaSeason2",
                                      sliderInput(inputId = "BSBmaFH_seas2", label ="For Hire Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmaFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBmaFH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5))),
                                  sliderInput(inputId = "BSBmaPR_seas2", label ="Private Open Season 2",
                                              min = as.Date("01-01","%m-%d"),
                                              max = as.Date("12-31","%m-%d"),
                                              value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                              timeFormat = "%m-%d", ticks = FALSE),
                                  fluidRow(
                                    column(4,
                                           numericInput(inputId = "BSBmaPR_2_bag", label ="Bag Limit",
                                                        min = 0, max = 100, value = 0)),
                                    column(6,
                                           sliderInput(inputId = "BSBmaPR_2_len", label ="Min Length",
                                                       min = 5, max = 30, value = 16.5, step = .5))),
                                      sliderInput(inputId = "BSBmaSH_seas2", label ="Shore Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmaSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBmaSH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5)))))),
          
          
          
          
          column(4, #### SCUP 
                 titlePanel("Scup - MA"),
                 sliderInput(inputId = "SCUPmaFH_seas1", label ="For Hire Open Season 1", # Mass for hire season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("06-30","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPmaFH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 40)),
                   column(5, 
                          sliderInput(inputId = "SCUPmaFH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 
                 sliderInput(inputId = "SCUPmaFH_seas2", label ="For Hire Open Season 2", # Mass for hire season 2
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("07-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPmaFH_2_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPmaFH_2_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 
                 
                 sliderInput(inputId = "SCUPmaPR_seas1", label ="Private Open Season 1", # Massprivate  season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPmaPR_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPmaPR_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 sliderInput(inputId = "SCUPmaSH_seas1", label ="Shore Open Season 1", # Massp shore season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPmaSH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPmaSH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 9.5, step = .5))),
                 actionButton("SCUPMAaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SCUPmaSeason2",
                                      sliderInput(inputId = "SCUPmaFH_seas3", label ="For Hire Open Season 3", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmaFH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPmaFH_3_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPmaPR_seas2", label ="Private Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmaPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPmaPR_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPmaSH_seas2", label ="Shore Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmaSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPmaSH_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5)))))))
      }})

    
    ############# MA Breakout by mode ######################################
    output$SFmaMode <- renderUI({
      if (is.null(input$SF_MA_input_type))
        return()
      
      switch(input$SF_MA_input_type, 
             "Single" = div(sliderInput(inputId = "SFma_seas1", label ="Open Season 1",
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "SFma_1_bag", label ="Bag Limit",
                                                  min = 0, max = 100, value = 5)),
                              column(6,
                                     sliderInput(inputId = "SFma_1_len", label ="Min Length",
                                                 min = 5, max = 30, value = 16.5, step = .5)))), 
             "AllModes" = div(sliderInput(inputId = "SFmaFH_seas1", label ="For Hire Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFmaFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 5)),
                                column(6,
                                       sliderInput(inputId = "SFmaFH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))) ,
                              sliderInput(inputId = "SFmaPR_seas1", label ="Private Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFmaPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 5)),
                                column(6,
                                       sliderInput(inputId = "SFmaPR_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))) ,
                              sliderInput(inputId = "SFmaSH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFmaSH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 5)),
                                column(6,
                                       sliderInput(inputId = "SFmaSH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5)))))
    })
    
    
    output$BSBmaMode <- renderUI({
      if (is.null(input$BSB_MA_input_type))
        return()
      
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$BSB_MA_input_type,
             
             "Single" = div(sliderInput(inputId = "BSBma_seas1", label ="Open Season 1", 
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("05-20","%m-%d"),as.Date("09-07","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBma_1_bag", label ="Bag Limit",
                                                  min = 0, max = 20, value = 4)), 
                              column(6,
                                     sliderInput(inputId = "BSBma_1_len", label ="Min Length",
                                                 min = 3, max = 28.5, value = 16.5, step = .5)))),
                            
             
             "AllModes" = div(sliderInput(inputId = "BSBmaFH_seas1", label =" For Hire Open Season 1", 
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-20","%m-%d"),as.Date("09-07","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBmaFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 4)), 
                                column(6,
                                       sliderInput(inputId = "BSBmaFH_1_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 16.5, step = .5))),
                              sliderInput(inputId = "BSBmaPR_seas1", label ="Private/Rental Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-20","%m-%d"),as.Date("09-07","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBmaPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 4)), 
                                column(6,
                                       sliderInput(inputId = "BSBmaPR_1_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 16.5, step = .5))),
                              sliderInput(inputId = "BSBmaSH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-20","%m-%d"),as.Date("09-07","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBmaSH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 20, value = 4)), 
                                column(6,
                                       sliderInput(inputId = "BSBmaSH_1_len", label ="Min Length",
                                                   min = 3, max = 28.5, value = 16.5, step = .5)))))
                             
    })
    
    
    
    
    ############## RHODE ISLAND ###########################################################
    output$addRI <- renderUI({
      if(any("RI" == input$state)){
        fluidRow( 
          column(4,
                 titlePanel("Summer Flounder - RI"),
                 
                 selectInput("SF_RI_input_type", "Modes to choose from:",
                             c("Single", "AllModes")),
                 uiOutput("SFriMode"),
                 
                 actionButton("SFRIaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SFriSeason2",
                                      sliderInput(inputId = "SFriFH_seas2", label ="For Hire Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFriFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFriFH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18, step = .5))), 
                                      sliderInput(inputId = "SFriPR_seas2", label ="Private Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFriPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFriPR_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18, step = .5))), 
                                      sliderInput(inputId = "SFriSH_seas2", label ="Shore Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFriSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFriSH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18, step = .5)))))),
          
          column(4, 
                 titlePanel("Black Sea Bass - RI"),
                 
                 sliderInput(inputId = "BSBriFH_seas1", label ="For Hire Open Season 1",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("06-18","%m-%d"),as.Date("08-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBriFH_1_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 2)),
                   column(6,
                          sliderInput(inputId = "BSBriFH_1_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 sliderInput(inputId = "BSBriFH_seas2", label ="For Hire Open Season 2",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBriFH_2_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 6)),
                   column(6,
                          sliderInput(inputId = "BSBriFH_2_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 
                 sliderInput(inputId = "BSBriPR_seas1", label ="Private Open Season 1",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBriPR_1_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 2)),
                   column(6,
                          sliderInput(inputId = "BSBriPR_1_len", label ="Min Length",
                                      min = 5, max = 30, value = 16.5, step = .5))),
                 
                 sliderInput(inputId = "BSBriPR_seas2", label ="Private Open Season 2",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBriPR_2_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 3)),
                   column(6,
                          sliderInput(inputId = "BSBriPR_2_len", label ="Min Length",
                                      min = 5, max = 30, value = 16.5, step = .5))),
                 
                 sliderInput(inputId = "BSBriSH_seas1", label ="Shore Open Season 1",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBriSH_1_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 2)),
                   column(6,
                          sliderInput(inputId = "BSBriSH_1_len", label ="Min Length",
                                      min = 5, max = 30, value = 16.5, step = .5))),
                 
                 sliderInput(inputId = "BSBriSH_seas2", label ="Shore Open Season 2",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBriSH_2_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 3)),
                   column(6,
                          sliderInput(inputId = "BSBriSH_2_len", label ="Min Length",
                                      min = 5, max = 30, value = 16.5, step = .5))),
          
                 actionButton("BSBRIaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "BSBriSeason2",
                                      sliderInput(inputId = "BSBriFH_seas3", label ="For Hire Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBriFH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBriFH_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16, step = .5))),
                                      sliderInput(inputId = "BSBriPR_seas3", label ="Private Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBriPR_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBriPR_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5))),
                                      sliderInput(inputId = "BSBriSH_seas3", label ="Shore Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBriSH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBriSH_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5)))))),
          
          
          
          
          column(4, #### SCUP 
                 titlePanel("Scup - RI"),
                 sliderInput(inputId = "SCUPriFH_seas1", label ="For Hire Open Season 1", # Mass for hire season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPriFH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPriFH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 
                 sliderInput(inputId = "SCUPriFH_seas2", label ="For Hire Open Season 2", # Mass for hire season 2
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPriFH_2_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 40)),
                   column(5, 
                          sliderInput(inputId = "SCUPriFH_2_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 sliderInput(inputId = "SCUPriFH_seas3", label ="For Hire Open Season 3", # Mass for hire season 2
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPriFH_3_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPriFH_3_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 
                 sliderInput(inputId = "SCUPriPR_seas1", label ="Private Open Season 1", # Massprivate  season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPriPR_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPriPR_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 sliderInput(inputId = "SCUPriSH_seas1", label ="Shore Open Season 1", # Massp shore season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPriSH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPriSH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 9.5, step = .5))),
                 
                 actionButton("SCUPRIaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SCUPriSeason2",
                                      sliderInput(inputId = "SCUPriFH_seas4", label ="For Hire Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPriFH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPriFH_4_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPriPR_seas2", label ="Private Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPriPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPriPR_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPriSH_seas2", label ="Shore Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPriSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPriSH_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5)))))))
      }})
    
    
    
    
    ############# RI Breakout by mode ######################################
    output$SFriMode <- renderUI({
      if (is.null(input$SF_RI_input_type))
        return()
      
      switch(input$SF_RI_input_type, 
             "Single" = div(sliderInput(inputId = "SFri_seas1", label ="Open Season 1",
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("05-03","%m-%d"),as.Date("12-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "SFri_1_bag", label ="Bag Limit",
                                                  min = 0, max = 100, value = 4)),
                              column(6,
                                     sliderInput(inputId = "SFri_1_len", label ="Min Length",
                                                 min = 5, max = 30, value = 18, step = .5)))), 
             "AllModes" = div(sliderInput(inputId = "SFriFH_seas1", label ="For Hire Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-03","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFriFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFriFH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18, step = .5))) ,
                              sliderInput(inputId = "SFriPR_seas1", label ="Private Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-03","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFriPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFriPR_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18, step = .5))) ,
                              sliderInput(inputId = "SFriSH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-03","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFriSH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFriSH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18, step = .5)))))
    })
    
    ############## Connecticut ###########################################################
    output$addCT <- renderUI({
      if(any("CT" == input$state)){
        fluidRow( 
          column(4,
                 titlePanel("Summer Flounder - CT"),
                 
                 selectInput("SF_CT_input_type", "Modes to choose from:",
                             c("Single", "AllModes")),
                 uiOutput("SFctMode"),
                 
                 actionButton("SFCTaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SFctSeason2",
                                      sliderInput(inputId = "SFctFH_seas2", label ="For Hire Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFctFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFctFH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18.5, step = .5))), 
                                      sliderInput(inputId = "SFctPR_seas2", label ="Private Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFctPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFctPR_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18.5, step = .5))), 
                                      sliderInput(inputId = "SFctSH_seas2", label ="Shore Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFctSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFctSH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18.5, step = .5)))))),
          
          column(4, 
                 titlePanel("Black Sea Bass - CT"),
                 
                 sliderInput(inputId = "BSBctFH_seas1", label ="For Hire Open Season 1",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("05-19","%m-%d"),as.Date("08-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBctFH_1_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 5)),
                   column(6,
                          sliderInput(inputId = "BSBctFH_1_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 sliderInput(inputId = "BSBctFH_seas2", label ="For Hire Open Season 2",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBctFH_2_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 7)),
                   column(6,
                          sliderInput(inputId = "BSBctFH_2_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 
                 sliderInput(inputId = "BSBctPR_seas1", label ="Private Open Season 1",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("05-19","%m-%d"),as.Date("06-23","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBctPR_1_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 5)),
                   column(6,
                          sliderInput(inputId = "BSBctPR_1_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 
                 sliderInput(inputId = "BSBctPR_seas2", label ="Private Open Season 2",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("07-08","%m-%d"),as.Date("12-01","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBctPR_2_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 5)),
                   column(6,
                          sliderInput(inputId = "BSBctPR_2_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 
                 sliderInput(inputId = "BSBctSH_seas1", label ="Shore Open Season 1",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("05-19","%m-%d"),as.Date("06-23","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBctSH_1_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 5)),
                   column(6,
                          sliderInput(inputId = "BSBctSH_1_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 
                 sliderInput(inputId = "BSBctSH_seas2", label ="Shore Open Season 2",
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value=c(as.Date("07-09","%m-%d"),as.Date("12-01","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "BSBctSH_2_bag", label ="Bag Limit",
                                       min = 0, max = 20, value = 5)),
                   column(6,
                          sliderInput(inputId = "BSBctSH_2_len", label ="Min Length",
                                      min = 5, max = 30, value = 16, step = .5))),
                 
                 actionButton("BSBCTaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "BSBctSeason2",
                                      sliderInput(inputId = "BSBctFH_seas3", label ="For Hire Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBctFH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBctFH_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16, step = .5))),
                                      sliderInput(inputId = "BSBctPR_seas3", label ="Private Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBctPR_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBctPR_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16, step = .5))),
                                      sliderInput(inputId = "BSBctSH_seas3", label ="Shore Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBctSH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBctSH_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16, step = .5)))))),
          
          
          
          
          column(4, #### SCUP 
                 titlePanel("Scup - CT"),
                 sliderInput(inputId = "SCUPctFH_seas1", label ="For Hire Open Season 1", # Mass for hire season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPctFH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPctFH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 
                 sliderInput(inputId = "SCUPctFH_seas2", label ="For Hire Open Season 2", # Mass for hire season 2
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPctFH_2_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 40)),
                   column(5, 
                          sliderInput(inputId = "SCUPctFH_2_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 sliderInput(inputId = "SCUPctFH_seas3", label ="For Hire Open Season 3", # Mass for hire season 2
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPctFH_3_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPctFH_3_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 
                 sliderInput(inputId = "SCUPctPR_seas1", label ="Private Open Season 1", # Massprivate  season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPctPR_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPctPR_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 sliderInput(inputId = "SCUPctSH_seas1", label ="Shore Open Season 1", # Massp shore season 1
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPctSH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPctSH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 9.5, step = .5))),
                 
                 actionButton("SCUPCTaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SCUPctSeason2",
                                      sliderInput(inputId = "SCUPctFH_seas4", label ="For Hire Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPctFH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPctFH_4_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPctPR_seas2", label ="Private Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPctPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPctPR_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPctSH_seas2", label ="Shore Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPctSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPctSH_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5)))))))
      }})
    
    
    
    
    ############# CT Breakout by mode ######################################
    output$SFctMode <- renderUI({
      if (is.null(input$SF_CT_input_type))
        return()
      
      switch(input$SF_CT_input_type, 
             "Single" = div(sliderInput(inputId = "SFct_seas1", label ="Open Season 1",
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "SFct_1_bag", label ="Bag Limit",
                                                  min = 0, max = 100, value = 4)),
                              column(6,
                                     sliderInput(inputId = "SFct_1_len", label ="Min Length",
                                                 min = 5, max = 30, value = 18.5, step = .5)))), 
             "AllModes" = div(sliderInput(inputId = "SFctFH_seas1", label ="For Hire Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFctFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFctFH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18.5, step = .5))) ,
                              sliderInput(inputId = "SFctPR_seas1", label ="Private Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFctPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFctPR_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18.5, step = .5))) ,
                              sliderInput(inputId = "SFctSH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFctSH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFctSH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18.5, step = .5)))))
    })
    
    
    
    ############# NEW YORK #######################
    output$addNY <- renderUI({
      if(any("NY" == input$state)){
        fluidRow( 
          column(4,
                 titlePanel("Summer Flounder - NY"),
                 
                 selectInput("SF_NY_input_type", "Modes to choose from:",
                             c("Single", "AllModes")),
                 uiOutput("SFnyMode"),
                 
                 actionButton("SFNYaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SFnySeason2",
                                      sliderInput(inputId = "SFnyFH_seas2", label ="For Hire Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnyFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFnyFH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18.5, step = .5))), 
                                      sliderInput(inputId = "SFnyPR_seas2", label ="Private Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnyPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFnyPR_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18.5, step = .5))), 
                                      sliderInput(inputId = "SFnySH_seas2", label ="Shore Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnySH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "SFnySH_2_len", label ="Min Length",
                                                           min = 5, max = 30, value = 18.5, step = .5)))))),
          
          column(4, 
                 titlePanel("Black Sea Bass - NY"),
                 
                 selectInput("BSB_NY_input_type", "Modes to choose from:",
                             c("Single", "AllModes")),
                 uiOutput("BSBnyMode"),
                 
                 
                 actionButton("BSBNYaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "BSBnySeason3",
                                      sliderInput(inputId = "BSBnyFH_seas3", label ="For Hire Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnyFH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBnyFH_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5))),
                                      sliderInput(inputId = "BSBnyPR_seas3", label ="Private Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnyPR_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBnyPR_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5))),
                                      sliderInput(inputId = "BSBnySH_seas3", label ="Shore Open Season 3",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnySH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 0)),
                                        column(6,
                                               sliderInput(inputId = "BSBnySH_3_len", label ="Min Length",
                                                           min = 5, max = 30, value = 16.5, step = .5)))))),
          
          
          
          
          column(4, #### SCUP 
                 titlePanel("Scup - NY"),
                 sliderInput(inputId = "SCUPnyFH_seas1", label ="For Hire Open Season 1", 
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPnyFH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPnyFH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 
                 sliderInput(inputId = "SCUPnyFH_seas2", label ="For Hire Open Season 2", 
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPnyFH_2_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 40)),
                   column(5, 
                          sliderInput(inputId = "SCUPnyFH_2_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 sliderInput(inputId = "SCUPnyFH_seas3", label ="For Hire Open Season 3", 
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPnyFH_3_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPnyFH_3_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))), 
                 
                 sliderInput(inputId = "SCUPnyPR_seas1", label ="Private Open Season 1", 
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPnyPR_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPnyPR_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 10.5, step = .5))),
                 sliderInput(inputId = "SCUPnySH_seas1", label ="Shore Open Season 1", 
                             min = as.Date("01-01","%m-%d"),
                             max = as.Date("12-31","%m-%d"),
                             value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                             timeFormat = "%m-%d", ticks = FALSE),
                 fluidRow(
                   column(4,
                          numericInput(inputId = "SCUPnySH_1_bag", label = "Bag Limit",
                                       min = 0, max = 100, value = 30)),
                   column(5, 
                          sliderInput(inputId = "SCUPnySH_1_len", label = "Min Length",
                                      min = 5, max = 50, value = 9.5, step = .5))),
                 
                 actionButton("SCUPNYaddSeason", "Add Season"), 
                 shinyjs::hidden( div(ID = "SCUPnySeason2",
                                      sliderInput(inputId = "SCUPnyFH_seas4", label ="For Hire Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnyFH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPnyFH_4_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPnyPR_seas2", label ="Private Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnyPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPnyPR_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5))), 
                                      sliderInput(inputId = "SCUPnySH_seas2", label ="Shore Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnySH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 0)), 
                                        column(6,
                                               sliderInput(inputId = "SCUPnySH_2_len", label ="Min Length",
                                                           min = 3, max = 28.5, value = 10, step = .5)))))))
      }})
    
    ############# NY Breakout by mode ######################################
    output$SFnyMode <- renderUI({
      if (is.null(input$SF_NY_input_type))
        return()
      
      switch(input$SF_NY_input_type, 
             "Single" = div(sliderInput(inputId = "SFny_seas1", label ="Open Season 1",
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "SFny_1_bag", label ="Bag Limit",
                                                  min = 0, max = 100, value = 4)),
                              column(6,
                                     sliderInput(inputId = "SFny_1_len", label ="Min Length",
                                                 min = 5, max = 30, value = 18.5, step = .5)))), 
             "AllModes" = div(sliderInput(inputId = "SFnyFH_seas1", label ="For Hire Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFnyFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFnyFH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18.5, step = .5))) ,
                              sliderInput(inputId = "SFnyPR_seas1", label ="Private Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFnyPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFnyPR_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18.5, step = .5))) ,
                              sliderInput(inputId = "SFnySH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("05-01","%m-%d"),as.Date("10-09","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "SFnySH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 4)),
                                column(6,
                                       sliderInput(inputId = "SFnySH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 18.5, step = .5)))))
    })
    
    
    output$BSBnyMode <- renderUI({
      if (is.null(input$BSB_NY_input_type))
        return()
      
      switch(input$BSB_NY_input_type, 
             "Single" = div(sliderInput(inputId = "BSBny_seas1", label ="Open Season 1",
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("06-23","%m-%d"),as.Date("8-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBny_1_bag", label ="Bag Limit",
                                                  min = 0, max = 100, value = 3)),
                              column(6,
                                     sliderInput(inputId = "BSBny_1_len", label ="Min Length",
                                                 min = 5, max = 30, value = 16.5, step = .5))), 
                            
                            sliderInput(inputId = "BSBny_seas2", label ="Open Season 2",
                                        min = as.Date("01-01","%m-%d"),
                                        max = as.Date("12-31","%m-%d"),
                                        value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                        timeFormat = "%m-%d", ticks = FALSE),
                            fluidRow(
                              column(4,
                                     numericInput(inputId = "BSBny_2_bag", label ="Bag Limit",
                                                  min = 0, max = 100, value = 6)),
                              column(6,
                                     sliderInput(inputId = "BSBny_2_len", label ="Min Length",
                                                 min = 5, max = 30, value = 16.5, step = .5)))), 
             "AllModes" = div(sliderInput(inputId = "BSBnyFH_seas1", label ="For Hire Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnyFH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 3)),
                                column(6,
                                       sliderInput(inputId = "BSBnyFH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))) ,
                              sliderInput(inputId = "BSBnyPR_seas1", label ="Private Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnyPR_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 3)),
                                column(6,
                                       sliderInput(inputId = "BSBnyPR_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))) ,
                              sliderInput(inputId = "BSBnySH_seas1", label ="Shore Open Season 1",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnySH_1_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 3)),
                                column(6,
                                       sliderInput(inputId = "BSBnySH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))), 
                              
                              
                              sliderInput(inputId = "BSBnyFH_seas2", label ="For Hire Open Season 2",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnyFH_2_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 6)),
                                column(6,
                                       sliderInput(inputId = "BSBnyFH_2_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))) ,
                              sliderInput(inputId = "BSBnyPR_seas2", label ="Private Open Season 2",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnyPR_2_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 6)),
                                column(6,
                                       sliderInput(inputId = "BSBnyPR_2_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5))) ,
                              sliderInput(inputId = "BSBnySH_seas2", label ="Shore Open Season 2",
                                          min = as.Date("01-01","%m-%d"),
                                          max = as.Date("12-31","%m-%d"),
                                          value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                          timeFormat = "%m-%d", ticks = FALSE),
                              fluidRow(
                                column(4,
                                       numericInput(inputId = "BSBnySH_2_bag", label ="Bag Limit",
                                                    min = 0, max = 100, value = 6)),
                                column(6,
                                       sliderInput(inputId = "BSBnySH_1_len", label ="Min Length",
                                                   min = 5, max = 30, value = 16.5, step = .5)))))
    })
    
    ############## NEW JERSEY ############################################################
    output$addNJ <- renderUI({
        if(any("NJ" == input$state)){
      fluidRow( 
        column(4,
               titlePanel("Summer Flounder - NJ"),
               
               selectInput("SF_NJ_input_type", "Modes to choose from:",
                           c("Single", "AllModes")),
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
               titlePanel("Black Sea Bass - NJ"),
               
               selectInput("BSB_NJ_input_type", "Modes to choose from:",
                           c("Single", "AllModes")),
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
               titlePanel("Scup - NJ"),
               
               selectInput("SCUP_NJ_input_type", "Modes to choose from:",
                           c("Single", "AllModes")),
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
                                                         min = 5, max = 15, value = 10, step = .5)))))))
      }
      
    })
    ############# NJ Breakout by mode ######################################
    output$SFnjMode <- renderUI({
      if (is.null(input$SF_NJ_input_type))
        return()
      
      switch(input$SF_NJ_input_type, 
             "Single" = div(sliderInput(inputId = "SFnj_seas1", label ="Open Season 1", # New Jersey for hire season 1
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
             "AllModes" = div(sliderInput(inputId = "SFnjFH_seas1", label ="For Hire Open Season 1", # New Jersey for hire season 1
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
                                                   min = 3, max = 28.5, value = 12.5, step = .5)))))
    })
      
      output$SCUPnjMode <- renderUI({
        if (is.null(input$SCUP_NJ_input_type))
          return()
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$SCUP_NJ_input_type,
               
               "Single" = div(sliderInput(inputId = "SCUPnj_seas1", label ="Open Season 1",
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
               "AllModes" = div(sliderInput(inputId = "SCUPnjFH_seas1", label ="For Hire Open Season 1",
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
    
    
    
    
    
    
    
    ############## DELEWARE ###########################################################
    output$addDE <- renderUI({
      if(any("DE" == input$state)){
        print("Deleware")
      }})
    
    
    
    
    
    
    
    
    
    
    
    ##################################################################
 
    predictions_1 <- eventReactive(input$runmeplease,{
      
      predictions_1 <- NULL
      for(i in 1:length(input$state)){
        state_name <- input$state[i]
        
        ################ Summary Outputs ######################################
        ######################################################################
        
        source(here::here(paste0("model_run_",state_name,".R")), local = TRUE)
        
        predictions_1 <- predictions_1 %>% rbind(predictions)
        return(predictions_1)
      }
    })
    
    
    keep_release <- reactive({
      keep_release_output<- predictions_1() %>% 
        dplyr::filter(Category %in% c("bsb", "scup","sf"), 
                      number_weight %in% c("Weight", "Number"), 
                      keep_release %in% c("keep", "release")) %>% 
        dplyr::mutate(#Category = paste(Category, "(lbs)"), 
          StatusQuo = round(StatusQuo, digits = 0), 
          Alternative = round(Value, digits = 0),
          Percent_Change = paste(round(((Alternative/StatusQuo) - 1) * 100, digits = 0), "%" )) %>% 
        dplyr::select(c(state, Category, mode, keep_release, number_weight, StatusQuo, Alternative, Percent_Change, MeetsChange)) %>% 
        tidyr::pivot_wider(names_from = number_weight, values_from = c("StatusQuo", "Alternative", "Percent_Change", "MeetsChange")) %>% 
        tidyr::replace_na(list(mode = "All")) %>% 
        dplyr::select(state, Category, mode, keep_release, 
                      StatusQuo_Number, Alternative_Number, Percent_Change_Number,
                      Percent_Change_Weight, MeetsChange_Weight) %>% 
        dplyr::rename("StatusQuo Number" = StatusQuo_Number, 
                      "Alternative Number" = Alternative_Number,
                      "Percent Change Number" = Percent_Change_Number, 
                      "Percent Change Weight" = Percent_Change_Weight,
                      "Species" = Category, 
                      "Mode" = mode, 
                      "Keep/Release" = keep_release, 
                      "Percent of runs that meet 10% change" = MeetsChange_Weight) %>% 
        dplyr::arrange(factor(Species, levels = c("sf", "bsb", "scup")))
      
      
      return(keep_release_output)
    })
    
    
    welfare_ntrips<- reactive({
      welfare_output<- predictions_1() %>% 
        dplyr::filter(Category %in% c("CV", "ntrips")) %>% 
        dplyr::arrange(factor(Category, levels = c("CV", "ntrips"))) %>% 
        dplyr::mutate(Category = dplyr::recode(Category, "CV" = "Angler Welfare"), 
                      Category = dplyr::recode(Category, "ntrips" = "Estimated Trips"),
                      Category = dplyr::case_when(stringr::str_detect(Category, "Angler Welfare") ~ paste(Category, "($)"), 
                                                  stringr::str_detect(Category, "Estimated Trips") ~ Category),
                      StatusQuo = round(StatusQuo, digits = 2), 
                      Alternative = round(Value, digits = 2),
                      Percent_Change = paste(round(((Alternative/StatusQuo) - 1) * 100, digits = 0), "%" )) %>% 
        tidyr::replace_na(list(mode = "All")) %>% 
        dplyr::select(c(state, Category, mode, StatusQuo, Alternative, Percent_Change)) %>% 
        dplyr::rename("Percent Change" = Percent_Change, 
                      "Mode" = mode) 
      
      return(welfare_output)
    })
    
    mortality<- reactive({
      mortality_output<- predictions_1() %>% 
        dplyr::filter(Category %in% c("bsb", "scup","sf"), 
                      keep_release == "Discmortality") %>% 
        dplyr::mutate(#Category = paste(Category, "(lbs)"), 
          StatusQuo = round(StatusQuo, digits = 0), 
          Alternative = round(Value, digits = 0),
          Percent_Change = paste(round(((Alternative/StatusQuo) - 1) * 100, digits = 0), "%" )) %>% 
        dplyr::select(c(state, Category, mode, number_weight, StatusQuo, Alternative, Percent_Change)) %>% 
        tidyr::pivot_wider(names_from = number_weight, values_from = c("StatusQuo", "Alternative", "Percent_Change")) %>%
        dplyr::select(state, Category, mode, 
                      StatusQuo_Number, Alternative_Number, Percent_Change_Number, 
                      StatusQuo_Weight, Alternative_Weight, Percent_Change_Weight) %>% 
        dplyr::arrange(factor(Category, levels = c("sf", "bsb", "scup"))) %>% 
        dplyr::rename("StatusQuo Discard Mortality Weight (lbs)" = StatusQuo_Weight, 
                      "Alternative Discard Mortality Weight (lbs)" = Alternative_Weight, 
                      "StatusQuo Discard Mortality Number" = StatusQuo_Number, 
                      "Alternative Discard Mortality Number" = Alternative_Number, 
                      "Percent Change Number" = Percent_Change_Number, 
                      "Percent Change Weight" = Percent_Change_Weight, 
                      "Species" = Category, 
                      "Mode" = mode)
      return(mortality_output)
    })
    
    regulations <- reactive({
      
      if(input$state == "NJ"){  
      if(input$SF_NJ_input_type == "Single"){
        
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
      
      if(input$BSB_NJ_input_type == "Single"){
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
      
      
      
      if(input$SCUP_NJ_input_type == "Single"){
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
                      Season = stringr::str_remove(Season, pattern = "2023-"))
      return(regs_output)
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
    
    # output$markdown <- renderUI({
    #   HTML(markdown::markdownToHTML(knitr::knit(here::here('docs/documentation.Rmd'), quiet = TRUE)))
    # })

}

shiny::shinyApp(ui = ui, server = server)
