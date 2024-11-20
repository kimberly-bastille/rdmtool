### Test run prediction using f_l from catch at length data

predictions_out<- data.frame()
for(k in 1:10){
  for(i in 0:1){

    predictions<- predict_rec_catch_season(
      x = k, select_season = i,
      calibration_data_table =  readr::read_rds(file.path(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/pds_new_all_", i, "_", k, ".rds"))),
      directed_trips_table =  read.csv("C:/Users/kimberly.bastille/Desktop/codhad_data/directed_trips/directed_trips_calib_150draws.csv"),
      size_data_read = read.csv("C:/Users/kimberly.bastille/Desktop/codhad_data/projected_CaL_cod_hadd_open_season_test.csv"),
      costs_new_all =  readr::read_rds(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/cost_new_all_",i, "_", k, ".rds")),
      catch_data_all = read.csv(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/catch_draws/catch_draws", k, ".csv")),
      #l_w_conversion =
      n_drawz = 50, n_catch_draws = 30, eff_seed=130
    )

    predictions_out <- predictions_out %>% rbind(predictions)
  }
}

test_plot<- predictions_out %>%
  dplyr::filter(#!number_weight == "Total",
    Category == c("had"),
    number_weight == "Weight") %>%
  ggplot2::ggplot(ggplot2::aes(x = keep_release, y = as.numeric(Value), fill=mode))+
  ggplot2::geom_col(position="dodge")+
  ggplot2::facet_grid(run~season)

test_plot

test_plot2<- predictions_out %>%
  dplyr::filter(Category == c("CV", "ntrips"))%>%
  ggplot2::ggplot(ggplot2::aes(x = Category, y = as.numeric(Value), fill=mode))+
  ggplot2::geom_col(position="dodge")+
  ggplot2::facet_grid(season~run)

test_plot2




























