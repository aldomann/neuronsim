#' System of Firing-Rate Equations
#'
#' This nonlinear system describes the macroscopic dynamics of the population of
#' QIF neurons in terms ofthe population firing rate r and mean membrane potential v.
#'
#' @param t The current time point in the integration.
#' @param state State variables and their initial conditions.
#' @param params Parameters to pass to the FREs (delta, etabar, J).
#' @param input Input stimulus function of the system.
#'
#' @return List of rates of changes dr and dv.
mean_field_model_system <- function(t, state, params, input) {
  with(as.list(c(params, state)), {
    # Input stimulus
    I <- input(t)
    # Firing ratio
    dr <- delta / pi + 2 * r * v
    # Membrane potential
    dv <- v^2 + etabar + J * r + I - pi^2 * r^2

    # Return the rates of change
    list(c(dr, dv))
  })
}

#' Solve Firing-Rate Equations
#'
#' @param params Named vector of parameters (delta, etabar, J) to pass to the FREs.
#' @param init_state Named vector of the initial state (r, v) for the ODE system.
#' @param times Time sequence for which output is wanted; the first value of times must be the initial time.
#' @param stimulus Input stimulus function of the system.
#' @param method The integrator to use. The default integrator used is rk4.
#'
#' @return Solution of FRE equations as a data frame
#' @export
solve_fre <- function(params, init_state, times, stimulus, method = c("rk4", "euler")) {
  method <- match.arg(method)

  fre_output <- dplyr::as_tibble(
    deSolve::ode(
      y = init_state,
      times = times,
      func = mean_field_model_system,
      parms = params,
      input = stimulus,
      method = "rk4"
    )
  ) %>%
    dplyr::bind_cols(
      stimulus = stimulus(times)
    ) %>%
    dplyr::mutate(
      dplyr::across(.cols = dplyr::everything(), .f = as.numeric)
    )

  return(fre_output)
}
