% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_firing_rate}
\alias{plot_firing_rate}
\title{Plot Firing Rate}
\usage{
plot_firing_rate(data, hide_x = FALSE)
}
\arguments{
\item{data}{Data frame or list of data frames from FREs solution or QIF neurons simulation, result of \code{solve_fre()} and \code{simulate_qif()} respectively.}

\item{hide_x}{If TRUE, the \code{x} axis will not be shown.}
}
\value{
A \code{gg} object.
}
\description{
Plot Firing Rate
}
