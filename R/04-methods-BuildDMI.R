#' @include 01-classes-model.R 02-classes-dmi.R 03-generics.R
NULL


`%||%` <- function(a, b) if (!is.null(a)) a else b

.parse_cdm_args <- function(dots) {
  # Keep only the args we support
  keep <- intersect(names(dots), c("q_matrix", "prior_pi", "rule"))
  out <- dots[keep]

  # Normalize rule if present
  if (!is.null(out$rule)) {
    r <- toupper(as.character(out$rule)[1L])
    if (!r %in% c("DINA", "DINO")) {
      stop("`rule` must be 'DINA' or 'DINO'")
    }
    out$rule <- r
  }

  if (is.null(out$q_matrix)) out$q_matrix <- NULL
  if (is.null(out$prior_pi)) out$prior_pi <- NULL
  if (is.null(out$rule)) out$rule <- NULL

  out
}

# .parse_cdm_args <- function(dots) {
#   q <- dots$Q_matrix %||% dots$q_matrix %||%
#     stop("For CDM, pass `q_matrix` (or `Q_matrix`) via `...`.")
#   if (is.logical(q)) q <- q * 1L
#   if (!is.matrix(q) && !inherits(q, "Matrix")) stop("`q_matrix` must be a matrix.")
#   K <- ncol(q)
#   if (is.null(K) || K < 1L) stop("`q_matrix` must have ≥ 1 column.")
#   L <- 2L^K
#   pi <- dots$pi_prior %||% dots$pi
#   if (is.null(pi)) {
#     pi <- rep(1 / L, L)
#   } else {
#     pi <- as.numeric(pi)
#     if (length(pi) != L) stop(sprintf("`pi_prior` length (%d) must equal 2^K = %d.", length(pi), L))
#     s <- sum(pi)
#     if (!isTRUE(all.equal(s, 1))) pi <- pi / s
#   }
#   list(q_matrix = q, prior_pi = pi)
# }

# LBA
#' @export
setMethod(
  "BuildDMI", signature(model = "model_lba"),
  function(data, model, ...) {
    dl <- .convert2datalist(data)
    data_list <- dl[[1]]
    out <- lapply(seq_len(length(data_list)), function(i) {
      new("dmi",
        model = model,
        data = data_list[[i]],
        node_1_index = get_node_1_index_r(model@parameter_map, model@factors, model@accumulators),
        is_positive_drift = rep(TRUE, length(model@accumulators))
      )
    })
    names(out) <- names(data_list)
    out
  }
)

# fastdm (DDM)
#' @export
setMethod(
  "BuildDMI", signature(model = "model_fastdm"),
  function(data, model, ...) {
    dl <- .convert2datalist(data)
    data_list <- dl[[1]]
    out <- lapply(seq_len(length(data_list)), function(i) {
      new("dmi",
        model = model,
        data = data_list[[i]],
        is_positive_drift = unlist(dl[[2]][[i]])
      )
    })
    names(out) <- names(data_list)
    out
  }
)


#' @export
setMethod(
  "BuildDMI", signature(model = "model_hyper"),
  function(data, model, ...) {
    pars <- attr(data, "parameters")
    new("dmi",
      model = model,
      data  = pars[, model@pnames, drop = FALSE]
    )
  }
)

#' @importFrom methods is slot slotNames
.get_rule_from_model <- function(model) {
  if (methods::is(model, "model_cdm") && base::isS4(model)) {
    if ("rule" %in% methods::slotNames(model)) {
      r <- methods::slot(model, "rule")
      if (!is.null(r)) toupper(as.character(r)[1L]) else NULL
    } else {
      NULL
    }
  } else {
    NULL
  }
}

.normalize_rule <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  r <- toupper(as.character(x)[1L])
  if (!r %in% c("DINA", "DINO")) {
    stop("`rule` must be 'DINA' or 'DINO' (got: ", r, ")")
  }
  r
}

# CDM
#' @export
setMethod(
  "BuildDMI", signature(model = "model_cdm"),
  function(data, model, ...) {
    message("Experimental BuildDMI for CDM.")
    dots <- list(...)
    data_list <- .convert2datalist_nonacc(data)[[1]]

    # parse known args from ...
    q_matrix <- dots$q_matrix %||% NULL
    prior_pi <- dots$prior_pi %||% NULL
    rule_in <- .normalize_rule(dots$rule %||% NULL)

    # resolve rule: ... > model@rule > default
    rule_final <- rule_in %||% .get_rule_from_model(model) %||% "DINA"

    if (is.null(rule_in) && is.null(.get_rule_from_model(model))) {
      # user didn’t supply rule anywhere; be explicit but non-fatal
      packageStartupMessage(
        "CDM `rule` not provided; defaulting to 'DINA'. ",
        "To choose DINO, call e.g. setCDM(..., rule = 'DINO')."
      )
    }

    out <- lapply(seq_along(data_list), function(i) {
      new("dmi",
        model    = model,
        data     = data_list[[i]],
        q_matrix = q_matrix,
        prior_pi = prior_pi,
        rule     = rule_final
      )
    })
    names(out) <- names(data_list)
    out
  }
)


