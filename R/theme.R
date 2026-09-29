#'
#' LiU base colors
#' @export
liu_base <- c(blue = "#00b9e7", turquoise = "#17c7d2", green = "#00cfb5")

#' LiU complement colours. For small details
#' @export
liu_comp <- c(orange = "#ff6442", purple = "#8981d3",
              yellow = "#fdef5d", grey = "#6a7e91")

#' LiU colour palette
#'
#' Base colours first, then complement colours.
#' @param n Number of colours (max 7).
#' @export
liu_palette <- function(n = 7) {
  unname(c(liu_base, liu_comp))[seq_len(min(n, 7))]
}

#' LiU colours
#'
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @import ggplot2
#' @export
scale_colour_liu <- function(...) {
  discrete_scale("colour", palette = liu_palette, ...)
}

#' Theme for LiU
#'
#' From the LiU graphical manual: LiU colours,
#' Korolev LiU for headings and Miller for body text (Calibri and
#' Georgia).
#'
#' @param base_size Base font size.
#' @param heading_family Font for title, subtitle, axis and legend titles.
#' @param body_family Font for all other text.
#' @import ggplot2
#' @export
theme_liu <- function(base_size = 12,
                      heading_family = "Calibri",
                      body_family = "Georgia") {
  theme_bw(base_size = base_size, base_family = body_family) %+replace%
    theme(
      panel.background = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      
      text = element_text(colour = "black"),
    )
}

#View(iris)
#head(iris)
#ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#  geom_point(size=3) + theme_dark()

#df <- data.frame(x = factor(rep(letters[1:3], each = 10)), y = rnorm(30), color=(rep(c("A", "B"), each=5)))
#plot <- ggplot(df, aes(x = x, y = y, color=color)) + geom_point()

#plot + ggtitle("No theme")

#ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#  +     geom_point(size = 3) +
#  +     scale_colour_liu() +
#  +     labs(title = "Iris sepals", subtitle = "LiU theme") +
#  +     theme_liu()
