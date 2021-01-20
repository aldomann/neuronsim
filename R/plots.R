#' @noRd
hide_x_axis <- function(gg) {
  gg <- gg +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  return(gg)
}

#' Plot Firing Rate
#'
#' @param data FREs solution, result of \code{solve_fre()}.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_firing_rate <- function(data, hide_x = FALSE) {
  gg <- ggplot2::ggplot(data) +
    ggplot2::aes(x = .data$time, y = .data$r, colour = .data$type) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = c("qif" = "black", "fre" = "darkorange")) +
    ggplot2::labs(x = "Time (s)", y = "r(t)")

  if (hide_x) {
    gg <- hide_x_axis(gg)
  }

  return(gg)
}

#' Plot Membrane Potential
#'
#' @param data FREs solution, result of \code{solve_fre()}.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_membrane_potential <- function(data, hide_x = FALSE) {
  gg <- ggplot2::ggplot(data) +
    ggplot2::aes(x = .data$time, y = .data$v, colour = .data$type) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = c("qif" = "black", "fre" = "darkorange")) +
    ggplot2::labs(x = "Time (s)", y = "v(t)")

  if (hide_x) {
    gg <- hide_x_axis(gg)
  }

  return(gg)
}

#' Plot Input Current
#'
#' @param data FREs solution, result of \code{solve_fre()}.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_input_current <- function(data, hide_x = FALSE) {
  gg <- data %>%
    dplyr::select(.data$time, .data$current) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$time, y = .data$current) +
    ggplot2::geom_line(colour = "black") +
    ggplot2::labs(x = "Time (s)", y = "I(t)")

  if (hide_x) {
    gg <- hide_x_axis(gg)
  }

  return(gg)
}

#' Plot Neuronal Ensemble Dynamics
#'
#' @param data FREs solution, result of \code{solve_fre()}.
#'
#' @return A \code{gg} object.
#' @export
plot_dynamics <- function(data, raster_data = NULL) {
  plot_r <- plot_firing_rate(data, hide_x = TRUE)
  plot_v <- plot_membrane_potential(data, hide_x = TRUE)
  plot_I <- plot_input_current(data, hide_x = FALSE)

  if (is.null(raster_data)) {
    plot_list <- list(plot_r, plot_v, plot_I)
  } else {
    plot_raster <- NULL
    plot_list <- list(plot_r, plot_v, plot_raster, plot_I)
  }

  patch <- plot_list %>%
    patchwork::wrap_plots(
      nrow = lenth(.),
      guides = "collect"
    )

  return(patch)
}

#' Plot Firing Rate and Membrane Potential Trajectory
#'
#' Plot chaotic trajectory of the firing rate and membrane potential.
#'
#' @param data FREs solution, result of \code{solve_fre()}.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_trajectory <- function(data) {
  gg <- ggplot2::ggplot(data) +
    ggplot2::aes(x = .data$r, y = .data$v) +
    ggplot2::geom_path(colour = "darkorange") +
    ggplot2::labs(x = "r", y = "v")

  return(gg)
}
