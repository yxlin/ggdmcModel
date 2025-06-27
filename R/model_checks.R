#' Parameter Mapping and Condition Processing Utilities
#'
#' A set of helper functions for processing parameter mappings across
#' experimental conditions. These functions are used internally for
#' model specification and simulation.
#'
#' @name parameter_mapping_functions
#' @aliases is_core_parameter_x_condition
#'          is_parameter_x_condition get_stimulus_level_r get_factor_cells_r
#'
#' @param parameter_map_r A named list mapping parameters to conditions/factors.
#'        Example structure:
#'        \code{list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1")}
#'        Where:
#'        \itemize{
#'          \item Numeric values ("1") indicate constant parameters
#'          \item "M" indicates mapping to stimulus levels
#'          \item Other strings indicate factor dependencies
#'        }
#' @param factors_r A named list of experimental factors and their levels.
#'        Example: \code{list(S = c("red", "blue"))}
#' @param accumulators_r A character vector of accumulator names.
#'        Example: \code{c("r1", "r2")}
#'
#' @return
#' \describe{
#'   \item{is_core_parameter_x_condition}{Logical vector indicating which parameters are core (non-factor)}
#'   \item{is_parameter_x_condition}{Logical vector indicating which parameters are factor-dependent}
#'   \item{get_stimulus_level_r}{Character vector of stimulus levels for each accumulator}
#'   \item{get_factor_cells_r}{List of factor combinations for each accumulator}
#' }
#'
#' @details
#' These functions work together to:
#' \itemize{
#'   \item Analyze parameter mappings across experimental conditions
#'   \item Identify which parameters vary by condition
#'   \item Generate appropriate stimulus levels and factor combinations
#'   \item Support model specification and simulation functions
#' }
#'
#' @examples
#' \dontrun{
#' p_map <- list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1")
#' factors <- list(S = c("red", "blue"))
#' accumulators <- c("r1", "r2")
#'
#' # Check which parameters are core (not condition-dependent)
#' is_core_parameter_x_condition(p_map, factors)
#'
#' # Get stimulus levels for each accumulator
#' get_stimulus_level_r(p_map, factors, accumulators)
#' }
NULL

#' Tabulate Model Parameter
#'
#' Functions for inspecting and displaying parameter structures in models
#' built with `ggdmcModel`.
#'
#' @name model_parameter_utils
#' @aliases table_parameters print_parameter_map
#'
#' @param model_r An S4 model object created by \code{BuildModel}.
#' @param parameters_r Numeric vector of parameter values (for `table_parameters` only)
#'
#' @return
#' \describe{
#'   \item{table_parameters}{Returns a List in matrix form showing how parameters
#' map to model parameters}
#'   \item{print_parameter_map}{Prints the parameter mapping structure and
#' returns invisibly as integer status (0 for success)}
#' }
#'
#' @details
#' These functions help analyse whether the parameter and the factor are
#' constructed as \code{BuildModel} specified:
#'
#' \itemize{
#'   \item `table_parameters()` creates a tabular representation showing how
#'         parameters map to stimuli, responses, and other model components
#'   \item `print_parameter_map()` displays the model's parameter mapping.
#' }
#'
#' @examples
#' \dontrun{
#' # Build a model first
#' model <- ggdmcModel::BuildModel(
#'     p_map = list(a = "1", v = "M", z = "1", t0 = "1"),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     accumulators = c("r1", "r2"),
#'     type = "fastdm"
#' )
#'
#' # Tabulate parameter mapping
#' pop_mean <- c(a = 1, sz = 0.25, t0 = 0.15, v = 2.5, z = 0.38)
#' param_table <- table_parameters(model, pop_mean)
#' # $s1.r1
#' #             r1   r2
#' # a         1.00 1.00
#' # d         0.00 0.00
#' # precision 3.00 3.00
#' # s         1.00 1.00
#' # st0       0.00 0.00
#' # sv        0.00 0.00
#' # sz        0.25 0.25
#' # t0        0.15 0.15
#' # v         2.50 2.50
#' # z         0.38 0.38
#' # $s1.r2
#' # ...
#'
#' # Print parameter map structure
#' print_parameter_map(model)
#' }
#'
#' @rdname model_parameter_utils
NULL
