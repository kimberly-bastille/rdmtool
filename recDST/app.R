#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (interactive()) {
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
                choices = c("CT", "DE", "MA", "MD", "NJ", "NY", "RI", "VA"),
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
                                                    value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
                                                    value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
                                                    value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
                       #Season 1
                       sliderInput(inputId = "BSBnjFH_seas1", label =" For Hire Open Season 1", 
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
                                            min = 3, max = 28.5, value = 12.5, step = .5))),
                       
                       
                       actionButton("BSBNJaddSeason", "Add Season"), 
                       #Season 5
                       shinyjs::hidden( div(ID = "BSBnjSeason5",
                                            sliderInput(inputId = "BSBnjFH_seas5", label =" For Hire Open Season 5", 
                                                        min = as.Date("01-01","%m-%d"),
                                                        max = as.Date("12-31","%m-%d"),
                                                        value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
                                                        value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
                                                        value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
                                            min = 5, max = 15, value = 9, step = .5))), 
                       actionButton("SCUPaddSeason", "Add Season"))),
              
              
              actionButton("runmeplease", "Run Me")),
    
    tabPanel("Results", 
             tableOutput(outputId = "tableout"), 
             tableOutput(outputId = "regtableout")), 
    tabPanel("Documentation")
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  library(magrittr) 
  #once the p_star values are found, run the calibrations and save the output files
  p_star_sf_MA_variable<- 0.85
  p_star_sf_RI_variable<- 0.875
  p_star_sf_CT_variable<- 0.9
  p_star_sf_NY_variable<- 0.91
  p_star_sf_NJ_variable<- 0.89
  p_star_sf_DE_variable<- 0.725
  p_star_sf_MD_variable<- 0.91
  p_star_sf_VA_variable<- 0.9
  #p_star_sf_NC_variable<- 0
  
  p_star_bsb_MA_variable<- 0.82
  p_star_bsb_RI_variable<- 0.81
  p_star_bsb_CT_variable<- 0.82
  p_star_bsb_NY_variable<- 0.86
  p_star_bsb_NJ_variable<- 0.885
  p_star_bsb_DE_variable<- 0.885
  p_star_bsb_MD_variable<- 0.94
  p_star_bsb_VA_variable<- 0.87
  #p_star_bsb_NC_variable<- 0.975
  
  p_star_scup_MA_variable<- 0.67
  p_star_scup_RI_variable<- 0.595
  p_star_scup_CT_variable<- 0.56
  p_star_scup_NY_variable<- 0.535
  p_star_scup_NJ_variable<- 0.045
  #No scup p-stars for DE MD VA NC
  p_star_scup_DE_variable<- 0.65
  p_star_scup_MD_variable<- 0.65
  p_star_scup_VA_variable<- 0.65
  #p_star_scup_NC_variable<- 0.65
  
                  
  observeEvent(input$SFNJaddSeason, {
    show("SFnjSeason2")
  })
  observeEvent(input$BSBNJaddSeason, {
    show("BSBnjSeason5")
  })
  
  
  
  sf_size_data <- readr::read_csv(file.path(here::here("data-raw/sf_length_distn_2020.csv")),  show_col_types = FALSE) %>% 
    dplyr::filter(state!="NC") %>% 
    dplyr::select(c(state, fitted_length, prob_star))%>% 
    dplyr::rename(fitted_prob = prob_star)
  sf_size_data_read_base <- split(sf_size_data, sf_size_data$state)
  
  
  bsb_size_data <- readr::read_csv(file.path(here::here("data-raw/bsb_length_distn_2020.csv")),  show_col_types = FALSE) %>% 
    dplyr::filter(state!="NC") %>% 
    dplyr::select(c(state, fitted_length, prob_star))%>% 
    dplyr::rename(fitted_prob = prob_star)
  bsb_size_data_read_base <- split(bsb_size_data, bsb_size_data$state)
  
  
  scup_size_data <- readr::read_csv(file.path(here::here("data-raw/scup_length_distn_2020.csv")),  show_col_types = FALSE) %>% 
    dplyr::filter(state!="NC") %>% 
    dplyr::select(c(state, fitted_length, prob_star))%>% 
    dplyr::rename(fitted_prob = prob_star)
  scup_size_data_read_base <- split(scup_size_data, scup_size_data$state)
  
  
  shinyjs::onclick("SFaddseason",
                   shinyjs::toggle(id = "SFnjFH_seas2", anim = TRUE))
  # observe({
  # shinyjs::toggle(id = "SFnjFH_seas2", condition = input$SFaddSeason)
  # })
  #observeEvent(input$runmeplease, {
  observeEvent(input$runmeplease, {
    state <- input$state
    
    
    output$tableout<- renderTable({
      source(here::here(paste0("model_run_",state,".R")), local = TRUE)
      
      
    })
    
    output$regtableout <- renderTable({
      SFnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                  Season = paste(input$SFnjPR_seas1[1], "-", input$SFnjPR_seas1[2]), 
                                  BagLimit = paste(input$SFnjPR_1_smbag,",", input$SFnjPR_1_lgbag), 
                                  Length = paste(input$SFnjPR_1_smlen[1],"-", input$SFnjPR_1_smlen[2],",",input$SFnjPR_1_lglen[1],"-",input$SFnjPR_1_lglen[2]))
      SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                  Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]), 
                                  BagLimit = paste(input$SFnjPR_2_smbag,",", input$SFnjPR_2_lgbag), 
                                  Length = paste(input$SFnjPR_2_smlen[1],"-", input$SFnjPR_2_smlen[2],",",input$SFnjPR_2_lglen[1],"-",input$SFnjPR_2_lglen[2])) 
      SCUPnjseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("All"),
                                 Season = paste(input$SCUPnj_seas1[1], "-", input$SCUPnj_seas1[2]), 
                                 BagLimit = paste(input$SCUPnj_1_bag), 
                                 Length = paste(input$SCUPnj_1_len))

      
      
      
      regsout<- rbind(SFnjPRseason1, SFnjPRseason2, 
                      SCUPnjseason1) %>% 
        dplyr::filter(!BagLimit == "0", 
                      !BagLimit == "0 , 0")
      
      #print(regsout)

    })})

}
}

#Message of where the model is in the run. Don't know where to put this
#showNotification(strong(paste("Running model", x,"/100 for", state)),duration=NULL,id="running",type="message")

shinyApp(ui = ui, server = server)
