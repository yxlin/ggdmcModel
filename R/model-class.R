### Model class -----------------------------------
#' The S4 class for a model object.
#'
#' model class represents how the model parameter associates with the
#' experimental factor.
#'
#' @slot parameter_map indicates how model parameters associate with  experimental factors.
#' @slot accumulators A character vector representing the name of the model accumulator (in the case of the accumulator model).
#' @slot factors A list of factors defined in a design.
#' @slot match_map defines what constitutes a correct and an incorrect response.
#' @slot constants A list that informs what parameters the user sets to a fixed value.
#' @slot cell_names A character vector representing the name of the condition.
#' @slot parameter_x_condition_names A character vector representing the association of the design and the model.
#' @slot model_boolean A 3-D TRUE/FALSE array for allocating parameters to conditions.
#' @slot pnames a string vector specifying the names of the free paraemters
#' @slot npar the number of the free parameter
#' @slot type a string sepcifying which model to fit
#' @export
setClass("model", slots = c(
  parameter_map = "list",
  accumulators = "character",
  factors = "list",
  match_map = "list",
  constants = "numeric",
  cell_names = "character",
  parameter_x_condition_names = "character",
  model_boolean = "ANY",
  pnames = "ANY",
  npar = "ANY",
  type = "ANY"
))


## Data-model Instance  ------------------------------------------------------
#' The S4 class for the Data Model Instance
#'
#' The class is to represent a data-model instance, joining a model object
#' with a data list.
#'
#' @slot model A model object
#' @slot data A data list
#' @slot node_1_index A LBA specific parameter, indicating the node 1 index
#' @slot is_positive_drift Whether the drift rate is positive
#' @export
setClass("dmi",
  slots = c(
    model = "model",
    data = "ANY",
    node_1_index = "ANY",
    is_positive_drift = "ANY"
  ),
  prototype = list(
    model = NULL,
    data = NULL,
    node_1_index = NULL,
    is_positive_drift = NULL
  )
)
