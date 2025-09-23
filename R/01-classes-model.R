#' @include 00-class-unions.R
NULL

#' Base cognitive model (abstract)
#' @slot parameter_map list
#' @slot accumulators character|NULL
#' @slot factors list
#' @slot match_map list|NULL
#' @slot constants numeric|NULL
#' @slot cell_names character
#' @slot parameter_x_condition_names character
#' @slot model_boolean ANY
#' @slot pnames character
#' @slot npar integer
#' @exportClass model
setClass("model",
  slots = c(
    parameter_map               = "list",
    accumulators                = "characterOrNULL",
    factors                     = "listOrNULL",
    match_map                   = "listOrNULL",
    constants                   = "numericOrNULL",
    cell_names                  = "character",
    parameter_x_condition_names = "character",
    model_boolean               = "ANY",
    pnames                      = "character",
    npar                        = "integer"
  ),
  prototype = list(
    parameter_map = list(),
    accumulators = NULL,
    factors = list(),
    match_map = NULL,
    constants = NULL,
    cell_names = character(),
    parameter_x_condition_names = character(),
    model_boolean = NULL,
    pnames = character(),
    npar = 0L
  )
)

setValidity("model", function(object) {
  x <- object@constants
  if (!is.null(x) && !(is.numeric(x) && length(x) >= 1L)) {
    return("`constants` must be NULL or a numeric vector (length >= 1).")
  }
  TRUE
})

# Subclasses for polymorphism
#' @exportClass model_lba
setClass("model_lba", contains = "model")
#' @exportClass model_fastdm
setClass("model_fastdm", contains = "model")
#' @exportClass model_hyper
setClass("model_hyper", contains = "model")
#' @exportClass model_cdm
setClass("model_cdm", contains = "model")
