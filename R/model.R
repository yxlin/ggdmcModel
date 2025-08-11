#' Model Builders for 'ggdmc' Package
#'
#' A suite of tools for specifying and examining experimental
#' designs related to choice response time models (e.g.,
#' the Diffusion Decision Model). This package allows users to
#' define how experimental factors influence one or more model
#' parameters using R-style formula syntax, while also
#' checking the logical consistency of these associations.
#' Additionally, it integrates with the 'ggdmc' package, which
#' employs Differential Evolution Markov Chain Monte Carlo
#' (DE-MCMC) sampling to optimise model parameters. For
#' further details on the model-building approach, see
#' Heathcote, Lin, Reynolds, Strickland, Gretton, and
#' Matzke (2019) <doi:10.3758/s13428-018-1067-y>.
#'
#' @keywords package
#'
#' @name ggdmcModel
#' @keywords internal
#' @author  Yi-Shin Lin <yishinlin001@gmail.com>
#' @references
#' Heathcote, A., Lin, Y.-S., Reynolds, A., Strickland, L., Gretton, M. &
#' Matzke, D., (2019). Dynamic model of choice.
#' \emph{Behavior Research Methods}.
#' https://doi.org/10.3758/s13428-018-1067-y.
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib ggdmcModel
"_PACKAGE"
NULL


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
        # factor_cols <- setdiff(names(subj_df), c("s", "RT", "C"))

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
            # Get unique C value for each condition
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



#' Build a model object
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
#' @param type The model type used in the package, "fastdm", "hyper", or "lba".
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
#'     p_map = list(
#'         a = c("S", "COLOUR"), v = c("NOISE"), z = "1", d = "1", sz = "1", sv = "1",
#'         t0 = "1", st0 = "1", s = "1", precision = "1"
#'     ),
#'     match_map = list(M = list(left = "z_key", right = "x_key")),
#'     factors = list(
#'         S = c("left", "right"), COLOUR = c("red", "blue"),
#'         NOISE = c("high", "moderate", "low")
#'     ),
#'     constants = c(d = 0, s = 1, st0 = 0, sv = 0, precision = 3),
#'     accumulators = c("z_key", "x_key"),
#'     type = "fastdm"
#' )
#'
#' ## A LBA model
#' model <- BuildModel(
#'     p_map = list(
#'         A = "1", B = c("S", "COLOR"), t0 = "1", mean_v = c("NOISE", "M"),
#'         sd_v = "M", st0 = "1"
#'     ),
#'     match_map = list(M = list(left = "z_key", right = "x_key")),
#'     factors = list(
#'         S = c("left", "right"),
#'         COLOR = c("red", "blue"),
#'         NOISE = c("high", "moderate", "low")
#'     ),
#'     constants = c(st0 = 0, sd_v.false = 1),
#'     accumulators = c("z_key", "x_key"),
#'     type = "lba"
#' )
#'
#' @importFrom methods new
#' @export
BuildModel <- function(
    p_map = list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1", t0 = "1"),
    accumulators = c("r1", "r2"),
    factors = list(S = c("s1", "s2")),
    match_map = list(M = list("s1" = "r1", "s2" = "r2")),
    constants = c(sd_v = 1, st0 = 0),
    type = "lba",
    print_method =  "head",
    verbose = TRUE) 
{
    .check_factors(factors)
    .check_p_map(p_map)
    .check_accumulators(accumulators)
    .check_match_map(accumulators, factors, match_map)

    cell_and_factor_names <- build_cell_names_r(p_map, factors, accumulators)
    parameter_x_condition_names <- bind_condition2parameters_r(p_map, factors)
    model_boolean <- build_model_boolean_r(p_map, factors, accumulators, match_map)

    sort_constant_names <- sort(names(constants))
    sort_p_map_names <- sort(names(p_map))

    out <- new("model",
        parameter_map = p_map[sort_p_map_names],
        accumulators = accumulators,
        factors = factors,
        match_map = match_map,
        constants = constants[sort_constant_names],
        cell_names = cell_and_factor_names[[1]],
        parameter_x_condition_names = parameter_x_condition_names,
        model_boolean = model_boolean,
        pnames = NULL,
        npar = NULL,
        type = type
    )
    out@pnames <- get_pnames(out, FALSE)
    out@npar <- length(out@pnames)

    if (verbose) {
        .print_names(out@pnames, print_method = print_method)
        .print_names(out@cell_names, what_info = " cell names ", print_method = print_method)
    }

    out
}

#' Build Data Model Instance
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
#'   RT = c(0.7802726, 0.7890208, 1.3222672, 0.8376305, 0.7144698),
#'   R  = c("r1", "r1", "r2", "r1", "r1"),
#'   s  = c(1, 1, 1, 1, 1),
#'   S  = c("s1", "s1", "s1", "s1", "s1"),
#'   stringsAsFactors = FALSE
#' )
#'
#' sub_dmis <- BuildDMI(dat, model)
#'
#' @export
BuildDMI <- function(data, model) {
    if (isS4(data)) {
        stop("Did you enter the model as the 1st argument?")
    }

    data_and_c_list <- .convert2datalist(data)
    data_list <- data_and_c_list[[1]]

    nsubject <- length(data_list)

    if (model@type == "lba") {
        out <- lapply(seq_len(nsubject), function(i) {
            new("dmi",
                model = model,
                data = data_list[[i]],
                node_1_index = get_node_1_index_r(model@parameter_map, model@factors, model@accumulators),
                is_positive_drift = rep(TRUE, length(model@accumulators))
            )
        })
        names(out) <- names(data_list)
    } else if (model@type == "hyper") {
        data <- attr(data, "parameters")

        out <- new("dmi",
            model = model,
            data = data[, model@pnames],
            node_1_index = NULL,
            is_positive_drift = NULL
        )
    } else if (model@type == "norm") {
        stop("Please use 'lba' for the standard LBA model")
    } else if (model@type == "rd") {
        stop("Please use 'fastdm' for the standard DD model")
    } else if (model@type == "fastdm") {
        out <- lapply(seq_len(nsubject), function(i) {
            new("dmi",
                model = model,
                data = data_list[[i]],
                # unlist(sub_dmis[[1]]@node_1_index)
                is_positive_drift = unlist(data_and_c_list[[2]][[i]])
            )
        })
        names(out) <- names(data_list)
    } else {
        stop("Model type not built yet.")
    }

    out
}
