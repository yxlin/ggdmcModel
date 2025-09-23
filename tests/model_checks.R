#' Parameter Mapping and Condition Processing Utilities
#'
#' A set of helper functions for processing parameter mappings across
#' experimental conditions. These functions are used internally for
#' building the model Boolean array.
#'
#' @name parameter_mapping_functions
#' @aliases is_core_parameter_x_condition
#'          is_parameter_x_condition get_stimulus_level_r get_factor_cells_r
#'
#' @param parameter_map_r A named list mapping parameters to conditions and factors.
#'        Example structure:
#'        \code{list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1")}
#'        Where:
#'        \itemize{
#'          \item '1' indicates this parameter is constant across conditions
#'          \item "M" indicates this parameter is associated with the internal
#' matching factor. It changes depends on whether it is a match (i.e., correct)
#' response or a mismatched (i.e., incorrect) response.
#'          \item Other strings indicate factor dependencies
#'        }
#' @param factors_r A named list of experimental factors and their levels.
#'        Example: \code{list(S = c("red", "blue"))}
#' @param accumulators_r A character vector of accumulator names.
#'        Example: \code{c("r1", "r2")}
#'
#' @return
#' \describe{
#'   \item{is_core_parameter_x_condition}{Logical vector indicating whether 
#' core parameters (before associating with any conditions) are factor-dependent}
#'   \item{is_parameter_x_condition}{Logical vector indicating whether 
#' parameters are factor-dependent}
#'   \item{get_stimulus_level_r}{Character vector of stimulus levels for each 
#' accumulator}
#'   \item{get_factor_cells_r}{List of factor combinations for each accumulator}
#' }
#'
#' @details
#' These functions work together to:
#' \itemize{
#'   \item Analyse parameter mappings across experimental conditions
#'   \item Identify which parameters vary by conditions
#'   \item Generate appropriate stimulus levels and factor combinations
#' }
#'
#' @examples
#' p_map <- list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1")
#' factors <- list(S = c("red", "blue"))
#' accumulators <- c("r1", "r2")
#'
#' # Check which parameters are core (not condition-dependent)
#' is_core_parameter_x_condition(p_map, factors)
#'
#' # Get stimulus levels for each accumulator
#' get_stimulus_level_r(p_map, factors, accumulators)
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
#' # Build a model first
#' model <- BuildModel(
#'     p_map = list(a = "1", v = "S", z = "1", d = "1", sz = "1", sv = "1", t0 = "1", 
#'                  st0 = "1", s = "1"),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(d = 1, s = 1, sv = 1, sz = 0.5, st0 = 0),
#'     accumulators = c("r1", "r2"),
#'     type = "fastdm"
#' )
#' 
#' # Tabulate a parameter vector to examine how the factor-dependent 
#' # drift rate maps to the condition, s1 and s2.
#' p_vector <- c(a = 1, sv = 0.2, sz = 0.25, t0 = 0.15, v.s1 = 4, v.s2 = 2, z = .38)
#' 
#' pmat <- table_parameters(model, p_vector)
#' # Transpose the result to get a more readable format
#' result <- lapply(pmat, function(x) {
#'     t(x)
#' })
#' 
#' print(result)
#' # $s1.r1
#' #    a d s st0 sv  sz  t0    v z
#' # r1 1 1 1   0  1 0.5 0.2 0.25 4
#' # r2 1 1 1   0  1 0.5 0.2 0.25 4
#' # 
#' # $s1.r2
#' #    a d s st0 sv  sz  t0    v z
#' # r1 1 1 1   0  1 0.5 0.2 0.25 4
#' # r2 1 1 1   0  1 0.5 0.2 0.25 4
#' # 
#' # $s2.r1
#' #    a d s st0 sv  sz  t0    v z
#' # r1 1 1 1   0  1 0.5 0.2 0.15 4
#' # r2 1 1 1   0  1 0.5 0.2 0.15 4
#' # 
#' # $s2.r2
#' #    a d s st0 sv  sz  t0    v z
#' # r1 1 1 1   0  1 0.5 0.2 0.15 4
#' # r2 1 1 1   0  1 0.5 0.2 0.15 4
#'
#' # Print the parameter map 
#' tmp <- print_parameter_map(model)
#' # All parameters: a       d       s       st0     sv      sz      t0
#' #                 v.s1    v.s2    z
#' # Core parameters: a      d       s       st0     sv      sz      t0     
#' #                  v       z
#' # Free parameters: a      t0      v.s1    v.s2    z
#' # Constant values: d: 1   s: 1    st0: 0  sv: 1   sz: 0.5
#' 
#' # Parameter map: 
#' # 
#' # 1. When the second row is 1, it indicates that the parameter is fixed.
#' # The internal machinery goes to the 'constant' to find its value. Note
#' # the constant will be sorted alphabetically.
#' # 2. When the second row is 0, it indicates that the parameter is free.
#' # The internal machinery goes to the p_vector to find its value.
#' # When doing MCMC sampling, a new p_vector is proposed by the sampler at 
#' # every iteration.
#' 
#' # Cell, s1.r1:
#' # Acc 0: 0 0 1 2 3 4 1 2 4 <- C++ index 
#' #        1 0 0 0 0 0 1 1 1 <- Whether the parameter is fixed
#' # Acc 1: 0 0 1 2 3 4 1 2 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # 
#' # Cell, s1.r2:
#' # Acc 0: 0 0 1 2 3 4 1 2 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # Acc 1: 0 0 1 2 3 4 1 2 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # 
#' # Cell, s2.r1:
#' # Acc 0: 0 0 1 2 3 4 1 3 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # Acc 1: 0 0 1 2 3 4 1 3 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # 
#' # Cell, s2.r2:
#' # Acc 0: 0 0 1 2 3 4 1 3 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # Acc 1: 0 0 1 2 3 4 1 3 4 
#' #        1 0 0 0 0 0 1 1 1 
#' # 
#' # Cell (ncell =  4): s1.r1        s1.r2   s2.r1   s2.r2
#'
#' @rdname model_parameter_utils
NULL
