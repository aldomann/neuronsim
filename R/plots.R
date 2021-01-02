hide_x_axis <- function(gg) {
  gg <- gg +
    ggplot2::theme(
      # x-Axis
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  return(gg)
}

plot_rt <- function(data, hide_x = FALSE) {
  gg <- ggplot(data) +
    aes(x = time, y = r) +
    geom_line(
      colour = "darkorange"
    ) +
    labs(x = "Time (s)", y = "r(t)")

  if (hide_x) {
    gg <- gg %>% hide_x_axis()
  }

  return(gg)
}

plot_vt <- function(data, hide_x = FALSE) {
  gg <- ggplot(data) +
    aes(x = time, y = v) +
    geom_line(
      colour = "darkorange"
    ) +
    labs(x = "Time (s)", y = "v(t)")

  if (hide_x) {
    gg <- gg %>% hide_x_axis()
  }

  return(gg)
}

plot_curr <- function(data, hide_x = FALSE) {
  gg <- ggplot(data) +
    aes(x = time, y = current) +
    geom_line() +
    labs(x = "Time (s)", y = "I(t)")

  if (hide_x) {
    gg <- gg %>% hide_x_axis()
  }

  return(gg)
}
