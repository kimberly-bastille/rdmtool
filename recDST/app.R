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
                                   value=c(as.Date("05-02","%m-%d"),as.Date("09-27","%m-%d")), 
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
  
  
  
  sf_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/sf_length_distn_2020.csv")),  show_col_types = FALSE),
                         state!="NC",select=c(state, fitted_length, prob_star)) %>% 
    dplyr::rename(fitted_prob = prob_star)
  sf_size_data_read_base <- split(sf_size_data, sf_size_data$state)
  
  
  bsb_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/bsb_length_distn_2020.csv")),  show_col_types = FALSE),
                          state!="NC", select=c(state, fitted_length, prob_star))%>% 
    dplyr::rename(fitted_prob = prob_star)
  bsb_size_data_read_base <- split(bsb_size_data, bsb_size_data$state)
  
  
  scup_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/scup_length_distn_2020.csv")),  show_col_types = FALSE),
                           state!="NC",select=c(state, fitted_length, prob_star))%>% 
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
    # Summer Flounder
    SFnjFH_seas1 <- input$SFnjPR_seas1
    SFnjFH_1_smbag <- input$SFnjPR_1_smbag
    SFnjFH_1_smlen <- input$SFnjPR_1_smlen
    SFnjFH_1_lgbag <- input$SFnjPR_1_lgbag
    SFnjFH_1_lglen <- input$SFnjPR_1_lglen
    SFnjPR_seas1 <- input$SFnjPR_seas1
    SFnjPR_1_smbag <- input$SFnjPR_1_smbag
    SFnjPR_1_smlen <- input$SFnjPR_1_smlen
    SFnjPR_1_lgbag <- input$SFnjPR_1_lgbag
    SFnjPR_1_lglen <- input$SFnjPR_1_lglen
    SFnjSH_seas1 <- input$SFnjSH_seas1
    SFnjSH_1_smbag <- input$SFnjSH_1_smbag
    SFnjSH_1_smlen <- input$SFnjSH_1_smlen
    SFnjSH_1_lgbag <- input$SFnjSH_1_lgbag
    SFnjSH_1_lglen <- input$SFnjSH_1_lglen
    SFnjFH_seas2 <- input$SFnjFH_seas2
    SFnjFH_2_smbag <- input$SFnjFH_2_smbag
    SFnjFH_2_smlen <- input$SFnjFH_2_smlen
    SFnjFH_2_lgbag <- input$SFnjFH_2_lgbag
    SFnjFH_2_lglen <- input$SFnjFH_2_lglen
    SFnjPR_seas2 <- input$SFnjPR_seas2
    SFnjPR_2_smbag <- input$SFnjPR_2_smbag
    SFnjPR_2_smlen <- input$SFnjPR_2_smlen
    SFnjPR_2_lgbag <- input$SFnjPR_2_lgbag
    SFnjPR_2_lglen <- input$SFnjPR_2_lglen
    SFnjSH_seas2 <- input$SFnjSH_seas2
    SFnjSH_2_smbag <- input$SFnjSH_2_smbag
    SFnjSH_2_smlen <- input$SFnjSH_2_smlen
    SFnjSH_2_lgbag <- input$SFnjSH_2_lgbag
    SFnjSH_2_lglen <- input$SFnjSH_2_lglen
    # Black Sea Bass
    BSBnjFH_seas1 <- input$BSBnjFH_seas1
    BSBnjFH_1_bag <- input$BSBnjFH_1_bag
    BSBnjFH_1_len <- input$BSBnjFH_1_len
    BSBnjPR_seas1 <- input$BSBnjPR_seas1
    BSBnjPR_1_bag <- input$BSBnjPR_1_bag
    BSBnjPR_1_len <- input$BSBnjPR_1_len
    BSBnjSH_seas1 <- input$BSBnjSH_seas1
    BSBnjSH_1_bag <- input$BSBnjSH_1_bag
    BSBnjSH_1_len <- input$BSBnjSH_1_len
    BSBnjFH_seas2 <- input$BSBnjFH_seas2
    BSBnjFH_2_bag <- input$BSBnjFH_2_bag
    BSBnjFH_2_len <- input$BSBnjFH_2_len
    BSBnjPR_seas2 <- input$BSBnjPR_seas2
    BSBnjPR_2_bag <- input$BSBnjPR_2_bag
    BSBnjPR_2_len <- input$BSBnjPR_2_len
    BSBnjSH_seas2 <- input$BSBnjSH_seas2
    BSBnjSH_2_bag <- input$BSBnjSH_2_bag
    BSBnjSH_2_len <- input$BSBnjSH_2_len
    BSBnjFH_seas3 <- input$BSBnjFH_seas3
    BSBnjFH_3_bag <- input$BSBnjFH_3_bag
    BSBnjFH_3_len <- input$BSBnjFH_3_len
    BSBnjPR_seas3 <- input$BSBnjPR_seas3
    BSBnjPR_3_bag <- input$BSBnjPR_3_bag
    BSBnjPR_3_len <- input$BSBnjPR_3_len
    BSBnjSH_seas3 <- input$BSBnjSH_seas3
    BSBnjSH_3_bag <- input$BSBnjSH_3_bag
    BSBnjSH_3_len <- input$BSBnjSH_3_len
    BSBnjFH_seas4 <- input$BSBnjFH_seas4
    BSBnjFH_4_bag <- input$BSBnjFH_4_bag
    BSBnjFH_4_len <- input$BSBnjFH_4_len
    BSBnjPR_seas4 <- input$BSBnjPR_seas4
    BSBnjPR_4_bag <- input$BSBnjPR_4_bag
    BSBnjPR_4_len <- input$BSBnjPR_4_len
    BSBnjSH_seas4 <- input$BSBnjSH_seas4
    BSBnjSH_4_bag <- input$BSBnjSH_4_bag
    BSBnjSH_4_len <- input$BSBnjSH_4_len
    BSBnjFH_seas5 <- input$BSBnjFH_seas5
    BSBnjFH_5_bag <- input$BSBnjFH_5_bag
    BSBnjFH_5_len <- input$BSBnjFH_5_len
    BSBnjPR_seas5 <- input$BSBnjPR_seas5
    BSBnjPR_5_bag <- input$BSBnjPR_5_bag
    BSBnjPR_5_len <- input$BSBnjPR_5_len
    BSBnjSH_seas5 <- input$BSBnjSH_seas5
    BSBnjSH_5_bag <- input$BSBnjSH_5_bag
    BSBnjSH_5_len <- input$BSBnjSH_5_len
    # Scup
    SCUPnj_seas1 <- input$SCUPnj_seas1
    SCUPnj_1_bag <- input$SCUPnj_1_bag
    SCUPnj_1_len <- input$SCUPnj_1_len
    
    
    
    ####### Directed trips moved to Model Run scripts
    # directed_trips_table<-data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2022.csv")))) %>% 
    #   dplyr::mutate(fluke_bag1= dplyr::case_when(state == "NJ" & mode == "fh" & day >= SFnjFH_seas1[1] & day <= SFnjFH_seas1[2] ~ c(SFnjFH_1_smbag)), #NJ forhire season 1
    #                 fluke_bag1= dplyr::case_when(state == "NJ" & mode == "pr" & day >= SFnjPR_seas1[1] & day <= SFnjPR_seas1[2] ~ c(SFnjPR_1_smbag)), #NJ pr season 1
    #                 fluke_bag1= dplyr::case_when(state == "NJ" & mode == "sh" & day >= SFnjSH_seas1[1] & day <= SFnjSH_seas1[2] ~ c(SFnjSH_1_smbag)), #NJ shore season 1
    #                 #fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "bt" & period >= SFnjBT_seas2[1] & period <= SFnjBT_seas2[2] ~ c(SFnjBT_2_smbag)), #NJ boat season 2
    #                 #fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "sh" & period >= SFnjSH_seas2[1] & period <= SFnjSH_seas2[2] ~ c(SFnjSH_2_smbag)), #NJ shore season 2
    #                 #fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "sh" & period <= SFnjSH_seas1[1] & period >= SFnjSH_seas1[2] & period >= SFnjSH_seas2[1] & period <= SFnjSH_seas2[2] ~ c(0)), #NJ shore closed season
    #                 #fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "bt" & period <= SFnjBT_seas1[1] & period >= SFnjBT_seas1[2] & period >= SFnjBT_seas2[1] & period <= SFnjBT_seas2[2] ~ c(0)), #NJ boat closed season
    #                 bsb_bag = dplyr::case_when(state == "NJ" & day >= BSBnj_seas1[1] & day <= BSBnj_seas1[2] ~ c(BSBnj_1_bag)),
    #                 bsb_bag = dplyr::case_when(state == "NJ" & day >= BSBnj_seas2[1] & day <= BSBnj_seas2[2] ~ c(BSBnj_2_bag)),
    #                 bsb_bag = dplyr::case_when(state == "NJ" & day >= BSBnj_seas3[1] & day <= BSBnj_seas3[2] ~ c(BSBnj_3_bag)),
    #                 bsb_bag = dplyr::case_when(state == "NJ" & day >= BSBnj_seas4[1] & day <= BSBnj_seas4[2] ~ c(BSBnj_4_bag)),
    #                 #fluke_bag2= dplyr::case_when(state == "NJ" ~ c(NJ_SFlg_baglimit1)), 
    #                 #bsb_bag= dplyr::case_when(state == "NJ" ~ c(NJ_BSB_baglimit1)), 
    #                 scup_bag= dplyr::case_when(state == "NJ" & day >= SCUPnj_seas1[1] & day <= SCUPnj_seas1[2] ~ c(SCUPnj_1_bag)))
    # directed_trips_table<-subset(directed_trips_table, state!="NC")
    # directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)
    
    
    
    output$tableout<- renderTable({
      source(here::here(paste0("model_run_",state,".R")), local = TRUE)
    })
    
    output$regtableout <- renderTable({
      SFnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                              Season = paste(SFnjPR_seas1[1], "-", SFnjPR_seas1[2]), 
                              BagLimit = paste(SFnjPR_1_smbag,",", SFnjPR_1_lgbag), 
                              Length = paste(SFnjPR_1_smlen,",", SFnjPR_1_lglen))
      SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                  Season = paste(SFnjPR_seas2[1], "-", SFnjPR_seas2[2]), 
                                  BagLimit = paste(SFnjPR_2_smbag,",", SFnjPR_2_lgbag), 
                                  Length = paste(SFnjPR_2_smlen,",", SFnjPR_2_lglen)) 
      SFnjFHseason1 <- data.frame(State = c("NJ"),Species = c("Summer Flounder"), Mode = c("For Hire"),
                                  Season = paste(SFnjFH_seas1[1], "-", SFnjFH_seas1[2]), 
                                  BagLimit = paste(SFnjFH_1_smbag,",", SFnjFH_1_lgbag), 
                                  Length = paste(SFnjFH_1_smlen,",", SFnjFH_1_lglen))
      SFnjFHseason2 <- data.frame(State = c("NJ"),Species = c("Summer Flounder"), Mode = c("For Hire"),
                                  Season = paste(SFnjFH_seas2[1], "-", SFnjFH_seas2[2]), 
                                  BagLimit = paste(SFnjFH_2_smbag,",", SFnjFH_2_lgbag), 
                                  Length = paste(SFnjFH_2_smlen,",", SFnjFH_2_lglen))
      SFnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                  Season = paste(SFnjSH_seas1[1], "-", SFnjSH_seas1[2]), 
                                  BagLimit = paste(SFnjSH_1_smbag,",", SFnjSH_1_lgbag), 
                                  Length = paste(SFnjSH_1_smlen,",", SFnjSH_1_lglen)) 
      SFnjSHseason2 <- data.frame(State = c("NJ"),Species = c("Summer Flounder"), Mode = c("Shore"),
                                  Season = paste(SFnjSH_seas2[1], "-", SFnjSH_seas2[2]), 
                                  BagLimit = paste(SFnjSH_2_smbag,",", SFnjSH_2_lgbag), 
                                  Length = paste(SFnjSH_2_smlen,",", SFnjSH_2_lglen))
      BSBnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                  Season = paste(BSBnjPR_seas1[1], "-", BSBnjPR_seas1[2]), 
                                  BagLimit = paste(BSBnjPR_1_bag), 
                                  Length = paste(BSBnjPR_1_len)) 
      BSBnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                 Season = paste(BSBnjPR_seas2[1], "-", BSBnjPR_seas2[2]), 
                                 BagLimit = paste(BSBnjPR_2_bag), 
                                 Length = paste(BSBnjPR_2_len))
      BSBnjPRseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                 Season = paste(BSBnjPR_seas3[1], "-", BSBnjPR_seas3[2]), 
                                 BagLimit = paste(BSBnjPR_3_bag), 
                                 Length = paste(BSBnjPR_3_len))
      BSBnjPRseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                 Season = paste(BSBnjPR_seas4[1], "-", BSBnjPR_seas4[2]), 
                                 BagLimit = paste(BSBnjPR_4_bag), 
                                 Length = paste(BSBnjPR_4_len))
      BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(BSBnjPR_seas5[1], "-", BSBnjPR_seas5[2]), 
                                   BagLimit = paste(BSBnjPR_5_bag), 
                                   Length = paste(BSBnjPR_5_len))
      BSBnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(BSBnjFH_seas1[1], "-", BSBnjFH_seas1[2]), 
                                   BagLimit = paste(BSBnjFH_1_bag), 
                                   Length = paste(BSBnjFH_1_len)) 
      BSBnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(BSBnjFH_seas2[1], "-", BSBnjFH_seas2[2]), 
                                   BagLimit = paste(BSBnjFH_2_bag), 
                                   Length = paste(BSBnjFH_2_len))
      BSBnjFHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(BSBnjFH_seas3[1], "-", BSBnjFH_seas3[2]), 
                                   BagLimit = paste(BSBnjFH_3_bag), 
                                   Length = paste(BSBnjFH_3_len))
      BSBnjFHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(BSBnjFH_seas4[1], "-", BSBnjFH_seas4[2]), 
                                   BagLimit = paste(BSBnjFH_4_bag), 
                                   Length = paste(BSBnjFH_4_len))
      BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(BSBnjFH_seas5[1], "-", BSBnjFH_seas5[2]), 
                                   BagLimit = paste(BSBnjFH_5_bag), 
                                   Length = paste(BSBnjFH_5_len))
      BSBnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(BSBnjSH_seas1[1], "-", BSBnjSH_seas1[2]), 
                                   BagLimit = paste(BSBnjSH_1_bag), 
                                   Length = paste(BSBnjSH_1_len)) 
      BSBnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(BSBnjSH_seas2[1], "-", BSBnjSH_seas2[2]), 
                                   BagLimit = paste(BSBnjSH_2_bag), 
                                   Length = paste(BSBnjSH_2_len))
      BSBnjSHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(BSBnjSH_seas3[1], "-", BSBnjSH_seas3[2]), 
                                   BagLimit = paste(BSBnjSH_3_bag), 
                                   Length = paste(BSBnjSH_3_len))
      BSBnjSHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(BSBnjSH_seas4[1], "-", BSBnjSH_seas4[2]), 
                                   BagLimit = paste(BSBnjSH_4_bag), 
                                   Length = paste(BSBnjSH_4_len))
      BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(BSBnjSH_seas5[1], "-", BSBnjSH_seas5[2]), 
                                   BagLimit = paste(BSBnjSH_5_bag), 
                                   Length = paste(BSBnjSH_5_len))
      SCUPnjseason1 <- data.frame(State = c("NJ"), Species = c("SCUP"), Mode = c("All"),
                                 Season = paste(SCUPnj_seas1[1], "-", SCUPnj_seas1[2]), 
                                 BagLimit = paste(SCUPnj_1_bag), 
                                 Length = paste(SCUPnj_1_len))

      
      
      
      regsout<- rbind(SFnjPRseason1, SFnjPRseason2,
                      SFnjFHseason1, SFnjFHseason2, 
                      SFnjSHseason1, SFnjSHseason2, 
                      BSBnjPRseason1, BSBnjPRseason2, BSBnjPRseason3, BSBnjPRseason4, 
                      BSBnjFHseason1, BSBnjFHseason2, BSBnjFHseason3, BSBnjFHseason4, 
                      BSBnjSHseason1, BSBnjSHseason2, BSBnjSHseason3, BSBnjSHseason4, 
                      SCUPnjseason1) %>% 
        dplyr::filter(!BagLimit == "0", 
                      !BagLimit == "0 , 0")
      
      print(regsout)

    })})

}
}

#Message of where the model is in the run. Don't know where to put this
#showNotification(strong(paste("Running model", x,"/100 for", state)),duration=NULL,id="running",type="message")

shinyApp(ui = ui, server = server)
