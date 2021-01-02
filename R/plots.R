#' @noRd
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

plot_firing_rate <- function(data, hide_x = FALSE) {
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

plot_membrane_potential <- function(data, hide_x = FALSE) {
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

plot_input_current <- function(data, hide_x = FALSE) {
  gg <- ggplot(data) +
    aes(x = time, y = current) +
    geom_line() +
    labs(x = "Time (s)", y = "I(t)")

  if (hide_x) {
    gg <- gg %>% hide_x_axis()
  }

  return(gg)
}


plot_macro_dynamics <- function(data) {
  plot_r <- plot_firing_rate(data, hide_x = TRUE)
  plot_v <- plot_membrane_potential(data, hide_x = TRUE)
  plot_I <- plot_input_current(data, hide_x = FALSE)

  patch <- list(plot_r, plot_v, plot_I) %>%
    patchwork::wrap_plots(nrow = 3)

  return(patch)
}

plot_macro_trajectory <- function(data) {
  gg <- ggplot(data) +
    aes(x = r, y = v) +
    geom_path(
      colour = "darkorange"
    ) +
    labs(x = "r", y = "v")

  return(gg)
}
