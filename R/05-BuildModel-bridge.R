#' Build a model (returns subclassed S4 model)
#'
#' The function performs a series of syntax checks to ensure the user enters
#' strings/values conforming the C++ internal setting.
#'
#' @param p_map Descibes the association between the parameter and the
#' experimental factor.
#' @param accumulators Specifies the response names and their levels.
#' @param factors Specifies a list of factors along with their levels or conditions.
#' @param match_map Maps stimulus conditions to response levels, indicating correctness.
#' @param constants Allows the user to fix certain model parameters at constant values.
#' @param type one of "lba","fastdm","hyper","cdm"
#' @param print_method a string indicating how you want the function to print model
#' information. \itemize{
#' \item \code{head} prints the first few elements.
#' \item \code{sample} samples and prints a handful of elements.
#' \item \code{all} prints all elements.
#' }. Default to \code{head} method.
#' @param verbose Logical; if \code{TRUE}, prints design information.
#' @return A S4 'model' object containing the following slots:
#' \itemize{
#'      \item \code{parameter_map} Stores the assocation between model parameters and the factors.
#'      \item \code{accumulators} Names of internal accumulators or manifested responses.
#'      \item \code{factors} Names of the factors.
#'      \item \code{match_map} Mapping between stimuli and responses.
#'      \item \code{constants} Specifies which model parameters are fixed to constant values.
#'      \item \code{cell_names} Names of the experimental conditions aora a cells.
#'      \item \code{parameter_x_condition_names} Parameter names after associated with conditions.
#'      \item \code{model_boolean} A 3D Boolean array guiding the allocation of model parameters to conditions.
#'      \item \code{pnames} Names of the model parameter associated with conditons.
#'      \item \code{npar} Numbers of parameters.
#'      \item \code{type} a string indicating the model type.
#' }
#' @examples
#' ## A diffusion decision model
#' model <- BuildModel(
#'   p_map = list(
#'     a = c("S", "COLOUR"), v = c("NOISE"), z = "1", d = "1", sz = "1", sv = "1",
#'     t0 = "1", st0 = "1", s = "1", precision = "1"
#'   ),
#'   match_map = list(M = list(left = "z_key", right = "x_key")),
#'   factors = list(
#'     S = c("left", "right"), COLOUR = c("red", "blue"),
#'     NOISE = c("high", "moderate", "low")
#'   ),
#'   constants = c(d = 0, s = 1, st0 = 0, sv = 0, precision = 3),
#'   accumulators = c("z_key", "x_key"),
#'   type = "fastdm"
#' )
#'
#' ## A LBA model
#' model <- BuildModel(
#'   p_map = list(
#'     A = "1", B = c("S", "COLOR"), t0 = "1", mean_v = c("NOISE", "M"),
#'     sd_v = "M", st0 = "1"
#'   ),
#'   match_map = list(M = list(left = "z_key", right = "x_key")),
#'   factors = list(
#'     S = c("left", "right"),
#'     COLOR = c("red", "blue"),
#'     NOISE = c("high", "moderate", "low")
#'   ),
#'   constants = c(st0 = 0, sd_v.false = 1),
#'   accumulators = c("z_key", "x_key"),
#'   type = "lba"
#' )
#'
#' @importFrom methods new
#' @export
BuildModel <- function(p_map, accumulators, factors, match_map, constants,
                       type = c("lba", "fastdm", "hyper", "cdm"),
                       print_method = "head", verbose = TRUE) {
  type <- match.arg(type)

  .check_factors(factors)
  .check_p_map(p_map)

  if (type %in% c("lba", "fastdm")) {
    .check_accumulators(accumulators)
    .check_match_map(accumulators, factors, match_map)
  }

  cell_and_factor_names <- build_cell_names_r(p_map, factors, accumulators)
  parameter_x_condition_names <- bind_condition2parameters_r(p_map, factors)
  model_boolean <- build_model_boolean_r(p_map, factors, accumulators, match_map)

  sorted_constant_names <- sort(names(constants))
  sorted_p_map_names <- sort(names(p_map))


  base_slots <- list(
    parameter_map = p_map[sorted_p_map_names],
    accumulators = accumulators,
    factors = factors,
    match_map = match_map,
    constants = constants[sorted_constant_names],
    cell_names = cell_and_factor_names[[1]],
    parameter_x_condition_names = parameter_x_condition_names,
    model_boolean = model_boolean,
    pnames = character(),
    npar = 0L
  )

  cls <- switch(type,
    lba    = "model_lba",
    fastdm = "model_fastdm",
    hyper  = "model_hyper",
    cdm    = "model_cdm"
  )
  out <- do.call(methods::new, c(list(Class = cls), base_slots))

  # compute pnames/npar using your existing helpers *after* construction
  out@pnames <- get_pnames(out, FALSE)
  out@npar <- length(out@pnames)

  if (isTRUE(verbose)) {
    .print_names(out@pnames, print_method = print_method)
    .print_names(out@cell_names, what_info = " cell names ", print_method = print_method)
  }
  out
}
