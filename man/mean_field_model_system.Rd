% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/firing_rate_equations.R
\name{mean_field_model_system}
\alias{mean_field_model_system}
\title{System of Firing-Rate Equations}
\usage{
mean_field_model_system(t, state, params, input)
}
\arguments{
\item{t}{The current time point in the integration.}

\item{state}{State variables and their initial conditions.}

\item{params}{Parameters to pass to the FREs (delta, etabar, J).}

\item{input}{Input current or stimulus function of the system.}
}
\value{
List of rates of changes dr and dv.
}
\description{
This nonlinear system describes the macroscopic dynamics of the population of
QIF neurons in terms of the population firing rate r and mean membrane potential v.
}
