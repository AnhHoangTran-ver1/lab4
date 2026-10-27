#' Visualize mean arrival delay by destination airport
#'
#' @return A ggplot object showing mean arrival delay by airport location.
#' @importFrom rlang .data
#' @export
visualize_airport_delays <- function() {
  delays <- nycflights13::flights |>
    dplyr::filter(!is.na(.data$arr_delay)) |>
    dplyr::group_by(dest = .data$dest) |>
    dplyr::summarise(mean_delay = mean(.data$arr_delay)) |>
    dplyr::inner_join(nycflights13::airports, by = c("dest" = "faa"))
  
  ggplot2::ggplot(delays, ggplot2::aes(x = .data$lon, y = .data$lat,
                                       color = .data$mean_delay)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_gradient2(low = "blue", mid = "grey", high = "red") +
    ggplot2::labs(x = "Longitude", y = "Latitude", color = "Mean delay (min)",
                  title = "Mean arrival delay by destination airport")
}