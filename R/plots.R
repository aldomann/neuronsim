#' @noRd
bind_data_list <- function(data_list) {
  # Allow data frames as input
  if ("data.frame" %in% class(data_list)) {
    data_list <- list(data_list)
  }

  # Parse type attribute
  names(data_list) <- unlist(lapply(data_list, function(x) attr(x, "type")))

  # Bind data and factor type variable
  data_bind <- dplyr::bind_rows(
    data_list,
    .id = "type"
  ) %>%
    dplyr::mutate(
      type = factor(.data$type, levels = c("qif", "fre"))
    )

  return(data_bind)
}

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
#' @param data Data frame or list of data frames from FREs solution or QIF neurons simulation, result of \code{solve_fre()} and \code{simulate_qif()} respectively.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_firing_rate <- function(data, hide_x = FALSE) {
  gg <- data %>%
    bind_data_list() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$time, y = .data$r, colour = .data$type) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(
      values = c("qif" = "black", "fre" = "darkorange"),
      labels = c("qif" = "QIF", "fre" = "FREs")
    ) +
    ggplot2::labs(x = "Time (s)", y = "r(t)", colour = "Model")

  if (hide_x) {
    gg <- hide_x_axis(gg)
  }

  return(gg)
}

#' Plot Membrane Potential
#'
#' @param data Data frame or list of data frames from FREs solution or QIF neurons simulation, result of \code{solve_fre()} and \code{simulate_qif()} respectively.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_membrane_potential <- function(data, hide_x = FALSE) {
  gg <- data %>%
    bind_data_list() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$time, y = .data$v, colour = .data$type) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(
      values = c("qif" = "black", "fre" = "darkorange"),
      labels = c("qif" = "QIF", "fre" = "FREs")
    ) +
    ggplot2::labs(x = "Time (s)", y = "v(t)", colour = "Model")

  if (hide_x) {
    gg <- hide_x_axis(gg)
  }

  return(gg)
}

#' Plot Input Current
#'
#' @param data Data frame or list of data frames from FREs solution or QIF neurons simulation, result of \code{solve_fre()} and \code{simulate_qif()} respectively.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_input_current <- function(data, hide_x = FALSE) {
  gg <- data %>%
    bind_data_list() %>%
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

#' Plot Raster Data
#'
#' @param data Raster data frame from QIF neurons simulation, result of \code{simulate_qif()}.
#' @param hide_x If TRUE, the \code{x} axis will not be shown.
#'
#' @return A \code{gg} object.
#' @export
#'
#' @importFrom rlang .data
plot_raster_data <- function(data, hide_x = FALSE) {
  gg <- data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$time, y = .data$neuron) +
    ggplot2::geom_point(colour = "black", size = 0.1, alpha = 0.2) +
    ggplot2::expand_limits(x = 0) +
    ggplot2::labs(x = "Time (s)", y = "Neuron index")

  if (hide_x) {
    gg <- hide_x_axis(gg)
  }

  return(gg)
}

#' Plot Neuronal Ensemble Dynamics
#'
#' @param data Data frame or list of data frames from FREs solution or QIF neurons simulation, result of \code{solve_fre()} and \code{simulate_qif()} respectively.
#' @param raster_data Raster data from QIF neurons simulation.
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
    plot_raster <- plot_raster_data(raster_data, hide_x = TRUE)
    plot_list <- list(plot_r, plot_v, plot_raster, plot_I)
  }

  patch <- plot_list %>%
    patchwork::wrap_plots(
      nrow = length(.),
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
