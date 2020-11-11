#' Title
#'
#' @param fun Function that computes the values of the derivatives in the ODE system (the model definition) at time t.
#' @param params Parameters to pass to fun.
#' @param current Input current of the system.
#' @param init_r The initial (state) value of r(t) for the ODE system.
#' @param init_v The initial (state) value of r(t) for the ODE system.
#' @param times Time sequence for which output is wanted; the first value of times must be the initial time.
#' @param method The integrator to use. The default integrator used is rk4.
#'
#' @return Solution of FRE equations as a data frame
#' @export
solve_fre <- function(fun, params, current, init_r, init_v, times, method = c("rk4", "euler")) {
  method <- match.arg(method)

  fre_output <- dplyr::as_tibble(
    deSolve::ode(
      y = c(r = init_r, v = init_v),
      times = times,
      func = fun,
      parms = params,
      input = current,
      method = "rk4"
    )
  ) %>%
    dplyr::mutate(
      dplyr::across(.cols = dplyr::everything(), .f = as.numeric)
    )

  return(fre_output)
}
