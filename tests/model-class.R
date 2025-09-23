# Model class -----------------------------------

#' An S4 class Representing a Cognitive Model Object.
#'
#' The 'model' class stores information that defines how parameters in a cognitive model
#' are associated with experimental conditions, responses, and other design factors.
#' This object is typically created as part of the model specification process and is
#' used as input to fitting functions or simulation routines.
#'
#' @section Structure:
#' An object of class 'model' contains the following slots:
#'
#' @slot parameter_map A named list or structure indicating how each model parameter
#'   varies with experimental factors (e.g., which parameters depend on which conditions).
#'
#' @slot accumulators A character vector naming the accumulators in the model (e.g., for
#'   racing models or diffusion models with multiple response alternatives).
#'
#' @slot factors A named list where each element is a factor in the experimental design,
#'   and each value is a vector of levels for that factor.
#'
#' @slot match_map A list specifying which responses are considered correct or incorrect
#'   for each condition. Typically used in decision models to differentiate match/non-match.
#'
#' @slot constants A named list of model parameters that are fixed to user-defined values,
#'   rather than estimated.
#'
#' @slot cell_names A character vector giving the names of each condition cell in the design
#'   Boolean array (e.g., 's1.d1.r1', 's1.d1.r2', 's1.d2.r1', etc.), derived from crossing
#'   factor levels.
#'
#' @slot parameter_x_condition_names A character vector naming how each parameter is associated
#'   with particular condition cells.
#'
#' @slot model_boolean A 3D logical array. Its dimensions are:
#' \itemize{
#' \item slice: accumulators,
#' \item row: cells (i.e., conditions),
#' \item column: free parameters
#' }, indicating whether a parameter is free to vary for a given accumulator and condition.
#'
#' @slot pnames A character vector listing the names of all free parameters in the model.
#'
#' @slot npar An integer giving the total number of free parameters in the model.
#'
#' @slot type A character string indicating the type of model (e.g., 'fastdm' for the diffusion
#' model described in Voss, Rothermund, and Voss (2004) <doi:10.3758/BF03196893>.)
#'
#' @section Purpose:
#' This class object encapsulates all necessary mappings and constraints required for model fitting.
#' It is used by the fitting engine to determine which parameters vary, what parameters are fixed,
#' and how each condition affects the model structure.
#'
#' @return An object of class 'model', used to configure and fit cognitive decision models
#'   to experimental data.
#' @export
setClass("model",
  slots = c(
    parameter_map = "list",
    accumulators = "characterOrNULL",
    factors = "list",
    match_map = "listOrNULL",
    constants = "numericOrNULL",
    cell_names = "character",
    parameter_x_condition_names = "character",
    model_boolean = "ANY",
    pnames = "ANY",
    npar = "ANY",
    type = "ANY"
  ),
  prototype = list(constants = NULL)
)

setValidity("model", function(object) {
  x <- object@constants
  if (!is.null(x) && !(is.numeric(x) && length(x) >= 1L)) {
    return("`constants` must be NULL or a numeric vector (length >= 1).")
  }
  TRUE
})


## Data-model Instance  ------------------------------------------------------
#' An S4 Class Representing a Data-Model Instance
#'
#' The Data-Model Instance, 'dmi', class binds a model specification object with
#' a corresponding experimental dataset. This structure provides a unified container
#' used for fitting cognitive models, simulating responses, or conducting posterior
#' predictive checks.
#'
#' Unlike the previous version that used data frames, the `dmi` class expects the
#' data input as a **list** of trial-level records, optimised for internal modelling
#' functions.
#'
#' @section Structure:
#' An object of class 'dmi' has the following slots:
#'
#' @slot model An object of class 'model' that defines the structure of the cognitive model,
#'   including parameter mappings, accumulators, and condition associations.
#'
#' @slot data A list representing the observed dataset. Each element typically corresponds to
#'   a condition or trial grouping, containing relevant variables (e.g., RTs, responses).
#'
#' @slot node_1_index An internal integer matrix used in LBA-type models to indicate the
#'   accumulator or node associated with the correct response (e.g., index of node 1).
#'
#' @slot is_positive_drift A logical flag used in models where drift direction matters,
#'   indicating whether the modelled drift rate is constrained to be positive (in the LBA model)
#'   or is going to the positive direction (in the DDM). This can be important for proper
#'  interpretation of parameters.
#'
#' @section Purpose:
#' This class provides a complete representation of the modelling context by combining the experimental
#' data and model structure. It serves as the standard input to fitting algorithms, allowing for
#' parameter estimation, simulation, and model checking in accumulator-based cognitive models
#' (e.g., LBA, DDM).
#'
#' @return An object of class 'dmi' to be passed to functions for model fitting, likelihood
#' evaluation, simulation, or diagnostics.
#'
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
