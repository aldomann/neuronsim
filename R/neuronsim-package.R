#' \code{neuronsim} package
#'
#' Simulate the dynamics of neuronal ensembles using the
#' model of FREs and QIF neurons.
#'
#' See the README on
#' \href{https://github.com/aldomann/neuronsim/}{GitHub}
#'
#' @docType package
#' @name neuronsim
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

## usethis namespace: start
#' @useDynLib neuronsim, .registration = TRUE
## usethis namespace: end
NULL
