######### Helper functions---------------------------------------------
#' Model Builders for 'ggdmc' Package
#'
#' \pkg{ggdmcModel} provides tools for specifying and examining experimental
#' design associated with cognitive models (e.g., diffusion decision models)
#' for use with 'ggdmc' package.
#'
#' @keywords package
#'
#' @name ggdmcModel
#' @keywords internal
#' @author  Yi-Shin Lin <yishinlin001@gmail.com> \cr
#' @references
#' Lin, Y.-S. & Strickland, L., (2019). Evidence accumulation models with R: A
#' practical guide to hierarchical Bayesian methods.
#' \emph{The Quantitative Method in Psychology}. \cr
#'
#' Heathcote, A., Lin, Y.-S., Reynolds, A., Strickland, L., Gretton, M. &
#' Matzke, D., (2018). Dynamic model of choice.
#' \emph{Behavior Research Methods}.
#' https://doi.org/10.3758/s13428-018-1067-y. \cr
#'
#' Turner, B. M., & Sederberg P. B. (2012). Approximate Bayesian computation
#' with differential evolution, \emph{Journal of Mathematical Psychology}, 56,
#' 375--385. \cr
#'
#' Ter Braak (2006). A Markov Chain Monte Carlo version of the genetic
#' algorithm Differential Evolution: easy Bayesian computing for real
#' parameter spaces. \emph{Statistics and Computing}, 16, 239-249.
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
        cat("You seems to be using numbers representing response names. We try to guess what you meant.")
        match_map$M <- lapply(match_map$M, function(x) {
            accumulators[x]
        })
    }

    if (!all(unlist(match_map$M) %in% accumulators)) {
        cat("accumulators:\n")
        print(accumulators)

        cat("unlist match_map$M:\n")
        print(unlist(match_map$M))

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


.print_names <- function(pnames, what_info = " parameters (", max_print = 10, method = c("head", "sample", "all")) {
    method <- match.arg(method)

    if (length(pnames) > max_print) {
        if (method == "head") {
            # Print first 'max_print' and indicate truncation
            truncated <- pnames[1:max_print]

            message("First ", max_print, " of ", length(pnames), what_info, "use method = 'sample' or 'all' for more):")
            message(paste(truncated, collapse = "\t"))
            message("... (", length(pnames) - max_print, " more omitted)")
        } else if (method == "sample") {
            # Print a random sample
            sampled <- sample(pnames, size = max_print)
            message("Random sample of ", max_print, " parameter names:")
            message(paste(sampled, collapse = "\t"))
            message("... (total: ", length(pnames), ")")
        } else {
            # Print all (paginated if needed)
            message("All ", length(pnames), " parameter names:")
            print(pnames) # Uses R's default pagination
        }
    } else {
        # Short list: print everything
        message(what_info, length(pnames), " total):")
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


        # Split C values by the same combinations (for tracking)
        # c_values <- split(subj_df$C, conditions)

        # Get unique C value for each condition (assuming all values are same per condition)
        # c_unique <- lapply(c_values, function(x) x[1])

        # sequence <- order(names(rt_list))

        # data_list[[subj_name]] <- rt_list[sequence]
        # C_list[[subj_name]] <- c_unique[sequence]
    }

    if (has_C) {
        return(list(data = data_list, c_values = C_list))
    } else {
        return(list(data = data_list))
    }

    # list(data = data_list, c_values = C_list)
}

#' Build a model object
#'
#' The function performs a series of syntax checks to ensure the user enters
#' strings/values conforming the C++ internal setting.
#'
#' @param p_map descibes the association between the parameter and the
#' experimental factor.
#' @param accumulators specifies the response names and its levels.
#' @param factors specifies a list of factors and their levels/conditions.
#' @param match_map describes which the stimulus condition matches which response
#' level, resulting in a correct or an incorrect response.
#' @param constants the argument allows the user to decide which parameter is set to a
#' constant value.
#' @param type the model type defined in the package, "fastdm", "hyper", or "lba".
#' @param verbose print design information
#' @examples
#' ## A diffusion decision model
#' \dontrun{
#' model <- ggdmcModel::BuildModel(
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
#' }
#' \dontrun{
#' ## A LBA model
#' model <- ggdmcModel::BuildModel(
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
#' }
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
    verbose = TRUE) {
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
        .print_names(out@pnames)
        message(paste("", collapse = "\n"))
        .print_names(out@cell_names, " cell names (")
    }

    out
}

#' Build Data Model Instance
#'
#' Constructs a Data Model Instance (DMI) object from data and model
#' specifications, handling different model types including
#' Linear Ballistic Accumulator, Diffusion Decision and
#' hyperparameters (i.e., common statiatical models).
#'
#' @param data A dataset to be converted to a DMI object. It must be a data
#'        frame.
#' @param model A model specification object of class `model` containing type,
#'        parameters, and other model-specific information.
#'
#' @return A `dmi` object or list of `dmi` objects (multiple subjects),
#' with structure:
#' \itemize{
#'   \item For LBA models: Returns a named list of `dmi` objects (one per subject)
#'   \item For hyperparameter models: Returns a single `dmi` object
#' }
#' Each `dmi` object contains:
#' \itemize{
#'   \item `model` - The model specification
#'   \item `data` - The processed data (a list)
#'   \item `node_1_index` - Index mapping for first nodes (LBA only)
#'   \item `is_positive_drift` - Logical vector for drift directions. For LBA,
#' each element corresponds to an accumulator. In the DDM, each element
#' represents a condition. Additionally, in the DDM, the positive direction
#' corresponds to a correct response (i.e., upper bound), and vice versa.
#' }
#'
#' @section Model Types Supported:
#' \describe{
#'   \item{`"lba"`}{Linear Ballistic Accumulator model}
#'   \item{`"hyper"`}{Hyperparameter model (single subject)}
#'   \item{`"fastdm"`}{Diffusion Decision model}
#' }
#'
#' @section Errors:
#' Throws errors for:
#' \itemize{
#'   \item S4 objects passed as `data` (suggesting reversed arguments)
#'   \item Unsupported model types
#'   \item Requests for norm model type (directs user to use 'lba')
#' }
#'
#' @examples
#' \dontrun{
#'
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
#' model <- ggdmcModel::BuildModel(
#'     p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
#'     match_map = list(M = list(s1 = "r1", s2 = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(st0 = 0, sd_v = 1),
#'     accumulators = c("r1", "r2"),
#'     type = "lba"
#' )
#'
#' sub_model <- lbaModel::setLBA(model)
#' pop_model <- lbaModel::setLBA(model, population_distribution = pop_dist)
#' p_vector <- c(A = .75, B = 1.25, mean_v.false = 1.5, mean_v.true = 2.5, t0 = .15)
#' dat <- lbaModel::simulate(sub_model, nsim = 256, parameter_vector = p_vector, n_subject = 1)
#' hdat <- lbaModel::simulate(pop_model, nsim = 256, n_subject = 32)
#'
#' sub_dmis <- ggdmcModel::BuildDMI(dat, model)
#' pop_dmis <- ggdmcModel::BuildDMI(hdat, model)
#' hyper_dmi <- ggdmcModel::BuildDMI(hdat, hyper_model)
#' }
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
