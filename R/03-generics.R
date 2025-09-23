#' Map Experimental Conditions to Model Parameters
#'
#' Binds experimental conditions to model parameters by combining parameter
#' mappings and experimental factors, automatically handling the \code{M}
#' (matching) factor.
#'
#' @name bind_condition2parameters_r
#' @title Map Experimental Conditions to Model Parameters
#'
#' @param parameter_map_r A named list. Names are parameter names; each element
#'   is a character vector of factor tags (e.g., \code{"1"}, \code{"S"}, \code{"M"}).
#' @param factors_r A named list of experimental factors. Names are factor names;
#'   elements are character vectors of factor levels (e.g., \code{S = c("s1","s2")}).
#'
#' @return A character vector where each element is a parameter–condition binding,
#'   e.g., \code{"mean_v.s1.true"}.
#'
#' @details The function converts the inputs to C++ maps, expands conditions,
#'   and handles the LBA matching factor \code{M} (true/false) when present.
#'
#' @examples
#' p_map <- list(A = "1", B = "1", t0 = "1", mean_v = c("M", "S"), sd_v = "1", st0 = "1")
#' factors <- list(S = c("s1", "s2"))
#' result1 <- bind_condition2parameters_r(p_map, factors)
#' result1
#'
#' result2 <- split_parameter_x_condition(parameter_M)
#' # [[1]]
#' # [1] "A"
#' #
#' # [[2]]
#' # [1] "B"
#' #
#' # [[3]]
#' # [1] "mean_v" "s1"     "false"
#' #
#' # [[4]]
#' # [1] "mean_v" "s1"     "true"
#' #
#' # [[5]]
#' # [1] "mean_v" "s2"     "false"
#' #
#' # [[6]]
#' # [1] "mean_v" "s2"     "true"
#' #
#' # [[7]]
#' # [1] "sd_v"
#' #
#' # [[8]]
#' # [1] "st0"
#' #
#' # [[9]]
#' # [1] "t0"
#'
#' @export
NULL

#' Find All Possible Conditions
#'
#' @name build_cell_names_r
#' @title Find All Possible Conditions
#'
#' @description
#' Constructs all possible condition combinations (i.e., cells)
#' based on experimental factors, parameter mappings, and response
#' definitions. Returns both cell names and sorted factor definitions.
#'
#' @param parameter_map_r An Rcpp::List where each element is a character
#' vector mapping parameters to conditions. Names should correspond to
#'        parameters.
#' @param factors_r An Rcpp::List where each element is a character vector of
#'        factor levels. Names should correspond to factor names.
#' @param responses_r A character vector (std::vector<std::string>) of
#' response/accumulator names.
#'
#' @return An Rcpp::List with two elements:
#' \itemize{
#'   \item \code{cell_names}: Character vector of all possible condition
#' combinations
#'   \item \code{sortedFactors}: The processed factor structure used to
#'   generate cells
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts R lists to 'C++' maps for efficient processing
#'   \item Generates all condition combinations via Cartesian product
#'   \item Handles special parameter mappings (like mapping accumulators to
#'  conditions)
#'   \item Returns both cell names and the factor structure used
#' }
#'
#' @section Typical Workflow:
#' This function is typically used to:
#' \enumerate{
#'   \item Establish the full experimental design space
#'   \item Verify factor/parameter compatibility
#'   \item Generate condition labels for model specification
#' }
#' This function primarily is to debug the internal process of model building.
#'
#' @examples
#' # A simple example
#' p_map <- list(
#'     A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1",
#'     st0 = "1"
#' )
#' factors <- list(S = c("s1", "s2"))
#' responses <- c("r1", "r2")
#' result <- build_cell_names_r(p_map, factors, responses)
#'
#' # cat("B (2 factors), t0, mean_v (3 factors), sd_v (2 factors)")
#' p_map <- list(
#'     A = "H", B = c("S", "G"), t0 = "E", mean_v = c("D", "H", "M"),
#'     sd_v = c("D", "M"), st0 = "1"
#' )
#' factors <- list(
#'     S = c("s1", "s2", "s3"), D = c("d1", "d2"), E = c("e1", "e2"),
#'     G = c("g1", "g2", "g3"), H = c("h1", "h2", "h3", "h4", "h5")
#' )
#' responses <- c("r1", "r2", "r3")
#' result <- build_cell_names_r(p_map, factors, responses)
#' @export
NULL

#' Build Model Boolean
#'
#' @name build_model_boolean_r
#' @title Build Model Boolean
#' @description
#' Constructs a 3D boolean array indicating
#' parameter-condition-response association to represent the experimental
#' design.
#'
#' @param parameter_map_r An Rcpp::List where each element maps parameters
#'        to conditions (character vector). The element names indicates
#'        the model parameter. The element content is the factor name that
#'        assocaites with a model parameter.  \code{1} represents no
#'        assocation.
#' @param factors_r An Rcpp::List where each element defines factor levels
#'        (character vector). Names should be factor names.
#' @param accumulators_r A character vector (std::vector<std::string>)
#'        of accumulator names. I use `accumulator` to remind the
#'        difference of the implicit accumulator and the manifested
#'        response. Mostly, you may mix the two; however, sometimes,
#'        merging the two concepts may result in conceptual errors.
#' @param match_map_r An Rcpp::List that defines the mapping between
#' stimuli and responses, specifying which response are considered correct
#' or incorrect. (This is a nested list structure).
#'
#' @return An R logical array with dimensions:
#' \itemize{
#'   \item 1st dimension: Parameters (column)
#'   \item 2nd dimension: Conditions (row)
#'   \item 3rd dimension: Responses  (slice)
#' }
#' Where `TRUE` indicates the model assumes that a model parameter (1st
#' dimension) affects a condition (2nd dimension) at a particular response
#' (3rd dimension).
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts all R inputs to C++ maps for efficient processing
#'   \item Builds experimental design cells using \code{build_cell_names}
#'   \item Processes parameter-condition mappings with \code{add_M}
#'   \item Applies match map constraints to determine valid combinations
#'   \item Returns results as a 3D logical array compatible with R
#' }
#'
#' @section Typical Use Case:
#' Used when you need to:
#' \itemize{
#'   \item Validate experimental design completeness
#'   \item Generate design matrices for model fitting
#'   \item Check response-condition constraints
#' }
#'
#' @examples
#' p_map <- list(
#'     A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1",
#'     t0 = "1"
#' )
#' match_map <- list(M = list(s1 = "r1", s2 = "r2"))
#' factors <- list(S = c("s1", "s2"))
#' accumulators <- c("r1", "r2")
#' result <- build_model_boolean_r(p_map, factors, accumulators, match_map)
#'
#' @export
NULL

#' Get Free Parameter Names from Model
#'
#' @name get_pnames
#' @title Get Free Parameter Names from Model
#'
#' @description
#' Extracts the names of free parameters from an S4 model object, with optional
#' debugging output to inspect both free and constant parameters.
#'
#' @param model_r An S4 object containing the model specification and design
#' @param debug Logical flag indicating whether to print debugging information
#'        about both free and fixed parameters (default: FALSE)
#'
#' @return A character vector of free parameter names in the model
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Creates a new design object from the model
#'   \item Optionally prints debugging information about all parameters
#'   \item Returns only the names of free (non-constant) parameters
#' }
#'
#' @section Debugging Output:
#' When `debug = TRUE`, the function prints:
#' \itemize{
#'   \item Free parameters (those being estimated)
#'   \item Constants (fixed parameters)
#' }
#'
#' @examples
#' model <- BuildModel(
#'     p_map = list(
#'         A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1",
#'         t0 = "1"
#'     ),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(A = 0.75, mean_v.false = 1.5, sd_v = 1, st0 = 0),
#'     accumulators = c("r1", "r2"),
#'     type = "lba"
#' )
#'
#' pnames <- get_pnames(model)
#'
#' @export
NULL

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
#'     p_map = list(
#'         a = "1", v = "S", z = "1", d = "1", sz = "1", sv = "1", t0 = "1",
#'         st0 = "1", s = "1"
#'     ),
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

#' Build Data Model Instance (generic)
#'
#' Constructs a Data Model Instance (DMI) from data and model
#' specifications. The DMI builder can handle different model types including
#' the Linear Ballistic Accumulator, the Diffusion Decision and hyperparameter.
#' The process of building a 'hyperparameter' DMI amounts to constructing a
#' joint distribution over conventional statistical models.
#'
#' @param data A data frame to be converted to a DMI object.
#' @param model A model specification object of class \code{model} containing
#'       parameters, and other model-specific information. This is typically
#'       created using the `BuildModel` function.
#'
#' @return A 'dmi' object or a list of 'dmi' objects (multiple subjects),
#' with structure:
#' \itemize{
#'   \item For choice RT models: Returns a named list of 'dmi' objects
#'         (one per subject)
#'   \item For hyperparameter models: Returns a single 'dmi' object
#' }
#' Each 'dmi' object contains:
#' \itemize{
#'   \item 'model' - The model specification
#'   \item 'data' - The processed data (a list)
#'   \item 'node_1_index` - Index mapping for first nodes (LBA only)
#'   \item 'is_positive_drift` - A logical vector indicating drift
#' directions. For the LBA model, each element corresponds to an
#' accumulator. For the DDM, each element represents a condition.
#' In the DDM, a positive drift direction corresponds to a correct
#' response (i.e., the accumulator reaches the upper bound), and vice versa.
#' }
#'
#' @section Model Types Supported:
#' \describe{
#'   \item{`"lba"`}{Linear Ballistic Accumulator model}
#'   \item{`"hyper"`}{Hyperparameter model}
#'   \item{`"fastdm"`}{Diffusion Decision model}
#' }
#'
#' @examples
#' # Hyperparameter model example
#' hyper_model <- BuildModel(
#'     p_map = list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1", t0 = "1"),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(sd_v = 1, st0 = 0),
#'     accumulators = c("r1", "r2"),
#'     type = "hyper",
#'     verbose = FALSE
#' )
#'
#' # LBA model example
#' model <- BuildModel(
#'     p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(st0 = 0, sd_v = 1),
#'     accumulators = c("r1", "r2"),
#'     type = "lba"
#' )
#'
#' dat <- data.frame(
#'     RT = c(0.7802726, 0.7890208, 1.3222672, 0.8376305, 0.7144698),
#'     R = c("r1", "r1", "r2", "r1", "r1"),
#'     s = c(1, 1, 1, 1, 1),
#'     S = c("s1", "s1", "s1", "s1", "s1"),
#'     stringsAsFactors = FALSE
#' )
#'
#' sub_dmis <- BuildDMI(dat, model)
#'
#' @export
setGeneric("BuildDMI", function(data, model, ...) standardGeneric("BuildDMI"))


.check_match_map <- function(accumulators, factors, match_map) {
    if (is.null(match_map)) {
        return(invisible(NULL))
    }

    # Check structure
    if (length(match_map) < 1 || !is.list(match_map[[1]])) {
        stop("match_map must be a list of lists")
    }

    # Check match_map contains at least name M
    if (!any(names(match_map) %in% "M")) {
        stop("match_map must have a list named M")
    }
    map_names <- names(match_map)[names(match_map) != "M"]
    map_levels <- sapply(match_map[names(match_map) != "M"], levels)

    # convert match.map$M to accumulators and check
    if (is.numeric(unlist(match_map$M))) {
        message("You seems to be using numeric (i.e., numbers) representing response names. We try to guess what you meant.")
        match_map$M <- lapply(match_map$M, function(x) {
            accumulators[x]
        })
    }

    if (!all(unlist(match_map$M) %in% accumulators)) {
        message("accumulators:\n", paste(accumulators, collapse = "\t"))
        message("unlist match_map$M:\n", paste(unlist(match_map$M), collapse = "\t"))
        stop("match_map$M has index or name not in response names")
    }
    if (!(all(sort(accumulators) == sort(unique(unlist(match_map$M)))))) {
        stop("Not all response names are scored by match.map$M")
    }
    if (!all(sapply(match_map[names(match_map) != "M"], is.factor))) {
        stop("Entries in match_map besides M must be factors")
    }
    if (length(unlist(map_levels)) != length(unique(unlist(map_levels)))) {
        stop("All match_map levels must be unqiue")
    }
    # Check factors
    if (any(names(factors) == "M")) {
        stop("Do not use M as a factor name")
    }
    if (any(names(factors) %in% names(match_map))) {
        stop(paste(match_map, "used in match_map, can not use as a factor name"))
    }
    if (any(unlist(factors) %in% c("true", "false"))) {
        stop("\"true\" and \"false\" cannot be used as factor levels")
    }
    if (any(map_levels %in% c("true", "false"))) {
        stop("\"true\" and \"false\" cannot be used as match_map levels")
    }
    if (length(unlist(c(factors, map_levels))) !=
        length(unique(unlist(c(factors, map_levels))))) {
        stop("Factor levels cannot overlap match_map levels")
    }

    invisible(NULL)
}

.print_names <- function(pnames, what_info = " parameters ", max_print = 10, print_method = c("head", "sample", "all")) {
    method <- match.arg(print_method)
    n <- length(pnames)

    if (n > max_print) {
        if (method == "head") {
            # Print first 'max_print' and indicate truncation
            truncated <- pnames[1:max_print]

            message("First ", max_print, " of ", n, what_info, " (use print_method = 'sample' or 'all' for more):")
            message(paste(truncated, collapse = "\t"))
            message("... (", n - max_print, " more omitted)")
        } else if (method == "sample") {
            # Print a random sample
            sampled <- sample(pnames, size = max_print)
            message("Random sample of ", max_print, what_info)
            message(paste(sampled, collapse = "\t"))
            message("... (total: ", n, ")")
        } else {
            message("All ", n, " parameter names:")
            message(paste(pnames, collapse = "\t"))
        }
    } else {
        # Short list: print everything
        message(n, what_info, "in total:")
        message(paste(pnames, collapse = "\t"))
    }
}

.check_factors <- function(factors) {
    keywords <- c("1", "s", "R", "M")
    if (length(unlist(factors)) != length(unique(unlist(factors)))) {
        stop("All factors levels must be unqiue")
    }
    if (any(names(factors) %in% keywords)) {
        stop("'1', 's' and 'R' are reserved keywords")
    }
    invisible(NULL)
}

.check_p_map <- function(p_map) {
    has_dot <- sapply(strsplit(names(p_map), "[.]"), length) > 1
    if (any(has_dot)) {
        stop(paste(
            "Dots not allowed in p_map names, please fix:",
            paste(names(p_map)[has_dot]), "\n"
        ))
    }
    invisible(NULL)
}

.check_accumulators <- function(accumulators) {
    ## Check accumulators
    if (length(accumulators) < 2) {
        stop("Must supply at least two or more responses/accumulators")
    }
    invisible(NULL)
}

.old_convert2datalist <- function(data) {
    # Split by subject
    by_subject <- split(data, data$s)

    # Sort subjects numerically if names are numbers
    subject_names <- names(by_subject)
    subject_names <- subject_names[order(as.numeric(subject_names))]
    by_subject <- by_subject[subject_names]

    # For each subject, create nested list by condition and response
    data_list <- lapply(by_subject, function(subj_df) {
        # Get all factor columns except 's' and 'RT'
        factor_cols <- setdiff(names(subj_df), c("s", "RT"))

        # Ensure S is first and R is last
        factor_cols <- c("S", setdiff(factor_cols, c("S", "R")), "R")
        factor_cols <- factor_cols[factor_cols %in% names(subj_df)]

        # Create condition strings following the specified order
        conditions <- apply(subj_df[, factor_cols, drop = FALSE], 1, function(row) {
            paste(row, collapse = ".")
        })

        # Split RTs by these combinations
        rt_list <- split(subj_df$RT, conditions)

        # Sort condition names lexicographically
        rt_list <- rt_list[order(names(rt_list))]
        rt_list
    })
    # Assign subject names explicitly as names of data_list
    # names(data_list) <- subject_names
    data_list
}

.convert2datalist <- function(data) {
    # Split by subject
    by_subject <- split(data, data$s)

    # Sort subjects numerically if names are numbers
    subject_names <- names(by_subject)
    subject_names <- subject_names[order(as.numeric(subject_names))]
    by_subject <- by_subject[subject_names]

    n_subject <- length(by_subject)

    data_list <- list()
    C_list <- list()
    has_C <- "C" %in% names(data)

    # For each subject, create nested list by condition and response
    for (subj_name in subject_names) {
        subj_df <- by_subject[[subj_name]]

        # Get all factor columns except 's', 'RT', and (if present) 'C'
        factor_cols <- if (has_C) {
            setdiff(names(subj_df), c("s", "RT", "C"))
        } else {
            setdiff(names(subj_df), c("s", "RT"))
        }


        # Get all factor columns except 's', 'RT', and 'C'
        # Ensure S is first and R is last
        factor_cols <- c("S", setdiff(factor_cols, c("S", "R")), "R")
        factor_cols <- factor_cols[factor_cols %in% names(subj_df)]

        # Create condition strings following the specified order (without C)
        conditions <- apply(subj_df[, factor_cols, drop = FALSE], 1, function(row) {
            paste(row, collapse = ".")
        })

        # Split RTs by these combinations
        rt_list <- split(subj_df$RT, conditions)

        if (has_C) {
            # Split C values by the same combinations (for tracking)
            c_values <- split(subj_df$C, conditions)
            # Get unique C value for each condition???
            c_unique <- lapply(c_values, function(x) x[1])
            sequence <- order(names(rt_list))
            C_list[[subj_name]] <- c_unique[sequence]
        }

        sequence <- order(names(rt_list))
        data_list[[subj_name]] <- rt_list[sequence]
    }

    if (has_C) {
        return(list(data = data_list, c_values = C_list))
    } else {
        return(list(data = data_list))
    }
}

.convert2datalist_nonacc <- function(data) {
    by_subject <- split(data, data$s)

    subject_names <- names(by_subject)
    subject_names <- subject_names[order(as.numeric(subject_names))]
    by_subject <- by_subject[subject_names]

    n_subject <- length(by_subject)
    data_list <- list()

    for (subj_name in subject_names) {
        subj_df <- by_subject[[subj_name]]
        factor_cols <- setdiff(names(subj_df), c("s", "student", "item", "C"))


        # factor_cols <- c("S", setdiff(factor_cols, c("S", "R")), "R")
        factor_cols <- factor_cols[factor_cols %in% names(subj_df)]


        conditions <- apply(subj_df[, factor_cols, drop = FALSE], 1, function(row) {
            paste(row, collapse = ".")
        })

        # Split "dependent variables" by these combinations
        dv_list <- split(subj_df$C, conditions)

        if (length(factor_cols) == 0) {
            ## message("No factorial conditon was detected for subject: ", subj_name)
            names(dv_list) <- "Cell"
        }

        sequence <- order(names(dv_list))
        data_list[[subj_name]] <- dv_list[sequence]
    }
    list(data = data_list)
}
