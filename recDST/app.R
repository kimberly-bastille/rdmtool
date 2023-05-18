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
        status = "danger"),
      sliderInput(inputId = "range1_SFpd", label ="SF Open Season 1:",
                  min = 0, max = 24, value = c(0,24)),
      sliderInput(inputId = "NJ_SFsm_baglimit1",
                  label ="SF Open Season 1 Small Bag Limit NJ",
                  min = 1, max = 10, value = 3),
      sliderInput(inputId = "range2_SFpd", label ="SF Open Season 2:",
                  min = 0, max = 24, value = c(0,24)),
      sliderInput(inputId = "NJ_SFsm_baglimit2",
                  label ="SF Open Season 2 Small Bag Limit NJ",
                  min = 1, max = 10, value = 3),
      sliderInput("NJ_SFlg_baglimit",
                  "Summer Flounder Large Bag Limit NJ",
                  min = 1, max = 10, value = 3),
      sliderInput("NJ_BSB_baglimit",
                  "Black Sea Bass Bag Limit NJ",
                  min = 1, max = 15, value = 10),
      sliderInput("NJ_SCUP_baglimit",
                  "Scup Bag Limit NJ",
                  min = 1, max = 75, value = 50), 
      actionButton("runmeplease", "Run Me")),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput(outputId = "tableout")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
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
  #observeEvent(input$runmeplease, {
    observeEvent(input$runmeplease, {
      state <- input$state
      range1_SFpd <- input$range1_SFpd
      NJ_SFsm_baglimit1 <- input$NJ_SFsm_baglimit1
      range2_SFpd <- input$range2_SFpd
      NJ_SFsm_baglimit2 <- input$NJ_SFsm_baglimit2
      NJ_SFlg_baglimit <- input$NJ_SFlg_baglimit
      NJ_BSB_baglimit <- input$NJ_BSB_baglimit
      NJ_SCUP_baglimit <- input$NJ_SCUP_baglimit
      
      directed_trips_table<-data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2020.csv")))) %>% 
        dplyr::mutate(fluke_bag1= dplyr::case_when(state == "NJ" & period >= range1_SFpd[1] & period <= range1_SFpd[2] ~ c(NJ_SFsm_baglimit1)),
                      fluke_bag1= dplyr::case_when(state == "NJ" & period >= range2_SFpd[1] & period <= range2_SFpd[2] ~ c(NJ_SFsm_baglimit2)), 
                      fluke_bag2= dplyr::case_when(state == "NJ" ~ c(NJ_SFlg_baglimit)), 
                      bsb_bag= dplyr::case_when(state == "NJ" ~ c(NJ_BSB_baglimit)), 
                      scup_bag= dplyr::case_when(state == "NJ" ~ c(NJ_SCUP_baglimit)))
      directed_trips_table<-subset(directed_trips_table, state!="NC")
      directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)
      
      output$tableout<- renderTable({
      source(here::here(paste0("model_run_",state,".R")), local = TRUE)
      
    })})
}


# Running button
#showNotification(strong(" Running... "),duration=NULL,id="running",type="message")
# Run the application 
shinyApp(ui = ui, server = server)
