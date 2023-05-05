# Plot output to check

test<- prediction_output_by_period1 %>%
  ggplot2::ggplot(ggplot2::aes(x = tripid, y = cv_sum, color = state))+
  ggplot2::geom_point()

dt <- data.table::rbindlist( lapply( prediction_output_by_period1, as.data.frame ) ) %>%
  dplyr::filter( period2 == "10_bt") %>%
  ggplot2::ggplot(ggplot2::aes(x = tripid, y = probA, color = state))+
  ggplot2::geom_point()
