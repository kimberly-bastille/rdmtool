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
                       sliderInput(inputId = "SFnjBT_seas1", label ="Boat Open Season 1",
                                   min = 0, max = 24, value = c(10,17)),
                       fluidRow(
                         column(5, 
                                numericInput(inputId = "SFnjBT_1_smbag", label ="Small Bag Limit",
                                             min = 0, max = 7, value = 2), 
                                sliderInput(inputId = "SFnjBT_1_smlen", label ="Small Min Length",
                                            min = 5, max = 34, value = 17, step = .5)),
                         column(5,
                                numericInput(inputId = "SFnjBT_1_lgbag", label = "Large Bag Limit",
                                             min = 0, max = 7, value = 1), 
                                sliderInput(inputId = "SFnjBT_1_lglen", label ="Large Min Length",
                                            min = 5, max = 34, value = 18, step = .5))), 
                       sliderInput(inputId = "SFnjSH_seas1", label ="Shore Open Season 1",
                                   min = 0, max = 24, value = c(10,17)),
                       fluidRow(
                         column(5, 
                                numericInput(inputId = "SFnjSH_1_smbag", label ="Small Bag Limit",
                                             min = 0, max = 7, value = 2), 
                                sliderInput(inputId = "SFnjSH_1_smlen", label ="Small Min Length",
                                            min = 5, max = 34, value = 17, step = .5)),
                         column(5,
                                numericInput(inputId = "SFnjSH_1_lgbag", label = "Large Bag Limit",
                                             min = 0, max = 7, value = 1), 
                                sliderInput(inputId = "SFnjSH_1_lglen", label ="Large Min Length",
                                            min = 5, max = 34, value = 18, step = .5))),
                       actionButton("SFaddSeason", "Add Season"), 
                       shinyjs::hidden(tags$div(id = "SF_add_season2",
                                                sliderInput(inputId = "SFnjBT_seas2", label ="Boat Open Season 2",
                                                            min = 0, max = 24, value = c(0,0)),
                                                fluidRow(
                                                  column(5, 
                                                         numericInput(inputId = "SFnjBT_2_smbag", label ="Small Bag Limit",
                                                                      min = 0, max = 7, value = 0), 
                                                         sliderInput(inputId = "SFnjBT_2_smlen", label ="Small Min Length",
                                                                     min = 5, max = 34, value = 17, step = .5)),
                                                  column(5,
                                                         numericInput(inputId = "SFnjBT_2_lgbag", label = "Large Bag Limit",
                                                                      min = 0, max = 7, value = 0), 
                                                         sliderInput(inputId = "SFnjBT_2_lglen", label ="Large Min Length",
                                                                     min = 5, max = 34, value = 18, step = .5))), 
                                                sliderInput(inputId = "SFnjSH_seas2", label ="Shore Open Season 2",
                                                            min = 0, max = 24, value = c(0,0)),
                                                fluidRow(
                                                  column(5, 
                                                         numericInput(inputId = "SFnjSH_2_smbag", label ="Small Bag Limit",
                                                                      min = 0, max = 7, value = 0), 
                                                         sliderInput(inputId = "SFnjSH_2_smlen", label ="Small Min Length",
                                                                     min = 5, max = 34, value = 17, step = .5)),
                                                  column(5,
                                                         numericInput(inputId = "SFnjSH_2_lgbag", label = "Large Bag Limit",
                                                                      min = 0, max = 7, value = 0), 
                                                         sliderInput(inputId = "SFnjSH_2_lglen", label ="Large Min Length",
                                                                     min = 5, max = 34, value = 18, step = .5)))))),
                column(4, 
                       titlePanel("Black Sea Bass"),
                       sliderInput(inputId = "BSBnj_seas1", label ="Open Season 1",
                                   min = 0, max = 24, value = c(5,6)),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "BSBnj_1_bag", label ="Bag Limit",
                                             min = 0, max = 20, value = 10)), 
                         column(6,
                                sliderInput(inputId = "BSBnj_1_len", label ="Min Length",
                                            min = 3, max = 28.5, value = 12.5, step = .5))),
                       sliderInput(inputId = "BSBnj_seas2", label ="Open Season 2",
                                   min = 0, max = 24, value = c(7,8)),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                             min = 0, max = 20, value = 2)),
                         column(6,
                                sliderInput(inputId = "BSBnj_2_len", label ="Min Length",
                                            min = 3, max = 28.5, value = 12.5, step = .5))),
                       sliderInput(inputId = "BSBnj_seas3", label ="Open Season 3",
                                   min = 0, max = 24, value = c(19,20)),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                             min = 0, max = 20, value = 10)),
                         column(6,
                                sliderInput(inputId = "BSBnj_3_len", label ="Min Length",
                                            min = 3, max = 28.5, value = 12.5, step = .5))),
                       sliderInput(inputId = "BSBnj_seas4", label ="OpenSeason 4",
                                   min = 0, max = 24, value = c(21,22)),
                       fluidRow(
                         column(4,
                                numericInput(inputId = "BSBnj_4_bag", label ="Bag Limit",
                                             min = 0, max = 20, value = 15)),
                         column(6,
                                sliderInput(inputId = "BSBnj_4_len", label ="Min Length",
                                            min = 3, max = 28.5, value = 13, step = .5))), 
                       actionButton("BSBaddSeason", "Add Season")),
                
                
                column(4, 
                       titlePanel("Scup"),
                       sliderInput(inputId = "SCUPnj_seas1", label ="Open Season 1",
                                   min = 0, max = 24, value = c(0,24)),
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
             #imageOutput(here::here("images/SF2022Regs.png")), 
             #imageOutput(here::here("images/SF2022Regs.png")), 
             #imageOutput(here::here("images/SF2022Regs.png")),
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
  
  
  
  
  
  
  
  # For the baseline (no correlation) scenario, use catch-per-trip distributions from the baseline year (observed_catch_2020_SS_MM_md.csv)
  # For all scenarios, use the adjusted catch-at-length distribution and no stock adjustments
  
  
  sf_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/sf_length_distn_2020.csv")),  show_col_types = FALSE),
                         state!="NC",select=c(state, fitted_length, prob_star))
  sf_size_data_read_base <- split(sf_size_data, sf_size_data$state)
  
  
  bsb_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/bsb_length_distn_2020.csv")),  show_col_types = FALSE),
                          state!="NC", select=c(state, fitted_length, prob_star))
  bsb_size_data_read_base <- split(bsb_size_data, bsb_size_data$state)
  
  
  scup_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/scup_length_distn_2020.csv")),  show_col_types = FALSE),
                           state!="NC",select=c(state, fitted_length, prob_star))
  scup_size_data_read_base <- split(scup_size_data, scup_size_data$state)
  
  
  #shinyjs::hide("SF_add_season2")
  
  observeEvent(input$SFaddSeason, {
    shinyjs::hide("SF_add_season2", anim = FALSE)
  })
  
  
  
  #observeEvent(input$runmeplease, {
  observeEvent(input$runmeplease, {
    state <- input$state
    # Summer Flounder
    SFnjBT_seas1 <- input$SFnjBT_seas1
    SFnjBT_1_smbag <- input$SFnjBT_1_smbag
    SFnjBT_1_smlen <- input$SFnjBT_1_smlen
    SFnjBT_1_lgbag <- input$SFnjBT_1_lgbag
    SFnjBT_1_lglen <- input$SFnjBT_1_lglen
    SFnjSH_seas1 <- input$SFnjSH_seas1
    SFnjSH_1_smbag <- input$SFnjSH_1_smbag
    SFnjSH_1_smlen <- input$SFnjSH_1_smlen
    SFnjSH_1_lgbag <- input$SFnjSH_1_lgbag
    SFnjSH_1_lglen <- input$SFnjSH_1_lglen
    SFnjBT_seas2 <- input$SFnjBT_seas2
    SFnjBT_2_smbag <- input$SFnjBT_2_smbag
    SFnjBT_2_smlen <- input$SFnjBT_2_smlen
    SFnjBT_2_lgbag <- input$SFnjBT_2_lgbag
    SFnjBT_2_lglen <- input$SFnjBT_2_lglen
    SFnjSH_seas2 <- input$SFnjSH_seas2
    SFnjSH_2_smbag <- input$SFnjSH_2_smbag
    SFnjSH_2_smlen <- input$SFnjSH_2_smlen
    SFnjSH_2_lgbag <- input$SFnjSH_2_lgbag
    SFnjSH_2_lglen <- input$SFnjSH_2_lglen
    # Black Sea Bass
    BSBnj_seas1 <- input$BSBnj_seas1
    BSBnj_1_bag <- input$BSBnj_1_bag
    BSBnj_1_len <- input$BSBnj_1_len
    BSBnj_seas2 <- input$BSBnj_seas2
    BSBnj_2_bag <- input$BSBnj_2_bag
    BSBnj_2_len <- input$BSBnj_2_len
    BSBnj_seas3 <- input$BSBnj_seas3
    BSBnj_3_bag <- input$BSBnj_3_bag
    BSBnj_3_len <- input$BSBnj_3_len
    BSBnj_seas4 <- input$BSBnj_seas4
    BSBnj_4_bag <- input$BSBnj_4_bag
    BSBnj_4_len <- input$BSBnj_4_len
    # Scup
    SCUPnj_seas1 <- input$SCUPnj_seas1
    SCUPnj_1_bag <- input$SCUPnj_1_bag
    SCUPnj_1_len <- input$SCUPnj_1_len
    
    
    
    
    directed_trips_table<-data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2020.csv")))) %>% 
      dplyr::mutate(fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "bt" & period >= SFnjBT_seas1[1] & period <= SFnjBT_seas1[2] ~ c(SFnjBT_1_smbag)), #NJ boat season 1
                    fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "sh" & period >= SFnjSH_seas1[1] & period <= SFnjSH_seas1[2] ~ c(SFnjSH_1_smbag)), #NJ shore season 1
                    fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "bt" & period >= SFnjBT_seas2[1] & period <= SFnjBT_seas2[2] ~ c(SFnjBT_2_smbag)), #NJ boat season 2
                    #fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "sh" & period >= SFnjSH_seas2[1] & period <= SFnjSH_seas2[2] ~ c(SFnjSH_2_smbag)), #NJ shore season 2
                    #fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "sh" & period <= SFnjSH_seas1[1] & period >= SFnjSH_seas1[2] & period >= SFnjSH_seas2[1] & period <= SFnjSH_seas2[2] ~ c(0)), #NJ shore closed season
                    fluke_bag1= dplyr::case_when(state == "NJ" & mode1 == "bt" & period <= SFnjBT_seas1[1] & period >= SFnjBT_seas1[2] & period >= SFnjBT_seas2[1] & period <= SFnjBT_seas2[2] ~ c(0)), #NJ boat closed season
                    bsb_bag = dplyr::case_when(state == "NJ" & period >= BSBnj_seas1[1] & period <= BSBnj_seas1[2] ~ c(BSBnj_1_bag)),
                    bsb_bag = dplyr::case_when(state == "NJ" & period >= BSBnj_seas2[1] & period <= BSBnj_seas2[2] ~ c(BSBnj_2_bag)),
                    bsb_bag = dplyr::case_when(state == "NJ" & period >= BSBnj_seas3[1] & period <= BSBnj_seas3[2] ~ c(BSBnj_3_bag)),
                    bsb_bag = dplyr::case_when(state == "NJ" & period >= BSBnj_seas4[1] & period <= BSBnj_seas4[2] ~ c(BSBnj_4_bag)),
                    #fluke_bag2= dplyr::case_when(state == "NJ" ~ c(NJ_SFlg_baglimit1)), 
                    #bsb_bag= dplyr::case_when(state == "NJ" ~ c(NJ_BSB_baglimit1)), 
                    scup_bag= dplyr::case_when(state == "NJ" & period >= SCUPnj_seas1[1] & period <= SCUPnj_seas1[2] ~ c(SCUPnj_1_bag)))
    directed_trips_table<-subset(directed_trips_table, state!="NC")
    directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)
    
    
    
    output$tableout<- renderTable({
      source(here::here(paste0("model_run_",state,".R")), local = TRUE)
    })
    
    output$regtableout <- renderTable({
      SFnjBTseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Boat"),
                              Season = paste(SFnjBT_seas1[1], "-", SFnjBT_seas1[2]), 
                              BagLimit = paste(SFnjBT_1_smbag,",", SFnjBT_1_lgbag), 
                              Length = paste(SFnjBT_1_smlen,",", SFnjBT_1_lglen)) 
      SFnjBTseason2 <- data.frame(State = c("NJ"),Species = c("Summer Flounder"), Mode = c("Boat"),
                                  Season = paste(SFnjBT_seas2[1], "-", SFnjBT_seas2[2]), 
                                  BagLimit = paste(SFnjBT_2_smbag,",", SFnjBT_2_lgbag), 
                                  Length = paste(SFnjSH_2_smlen,",", SFnjBT_2_lglen))
      SFnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                  Season = paste(SFnjSH_seas1[1], "-", SFnjSH_seas1[2]), 
                                  BagLimit = paste(SFnjSH_1_smbag,",", SFnjSH_1_lgbag), 
                                  Length = paste(SFnjSH_1_smlen,",", SFnjSH_1_lglen)) 
      SFnjSHseason2 <- data.frame(State = c("NJ"),Species = c("Summer Flounder"), Mode = c("Shore"),
                                  Season = paste(SFnjSH_seas2[1], "-", SFnjSH_seas2[2]), 
                                  BagLimit = paste(SFnjSH_2_smbag,",", SFnjSH_2_lgbag), 
                                  Length = paste(SFnjSH_2_smlen,",", SFnjSH_2_lglen))
      BSBnjseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("NA"),
                                  Season = paste(BSBnj_seas1[1], "-", BSBnj_seas1[2]), 
                                  BagLimit = paste(BSBnj_1_bag), 
                                  Length = paste(BSBnj_1_len)) 
      BSBnjseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("NA"),
                                 Season = paste(BSBnj_seas2[1], "-", BSBnj_seas2[2]), 
                                 BagLimit = paste(BSBnj_2_bag), 
                                 Length = paste(BSBnj_2_len))
      
      
      
      regsout<- rbind(SFnjBTseason1, SFnjBTseason2, SFnjSHseason1, SFnjSHseason2, 
                      BSBnjseason1, BSBnjseason2) %>% 
        dplyr::filter(!BagLimit == "0", 
                      !BagLimit == "0 , 0")
      
      print(regsout)

    })})

}


#Message of where the model is in the run. Don't know where to put this
#showNotification(strong(paste("Running model", x,"/100 for", state)),duration=NULL,id="running",type="message")

shinyApp(ui = ui, server = server)
