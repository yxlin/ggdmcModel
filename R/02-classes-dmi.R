#' @include 00-class-unions.R 01-classes-model.R
NULL


#' Data-Model Instance (per subject or per group)
#' @slot model model
#' @slot data ANY
#' @slot node_1_index ANY|NULL         # LBA/fastdm routing (OK NULL otherwise)
#' @slot is_positive_drift ANY|NULL    # LBA/fastdm legacy flag/vector (NULL otherwise)
#' @slot q_matrix matrix|NULL          # CDM: J x K
#' @slot prior_pi numeric|NULL         # CDM: length 2^K, sums to 1
#' @exportClass dmi
setClass("dmi",
  slots = c(
    model = "model",
    data = "ANY",
    node_1_index = "matrixOrNULL",
    is_positive_drift = "logicalOrNULL",
    q_matrix = "matrixOrNULL",
    prior_pi = "numericOrNULL",
    rule = "characterOrNULL"
  ),
  prototype = list(
    model = NULL, data = NULL,
    node_1_index = NULL, is_positive_drift = NULL,
    q_matrix = NULL, prior_pi = NULL, rule = NULL
  )
)

setValidity("dmi", function(object) {
  if (!is.null(object@q_matrix)) {
    q <- object@q_matrix
    if (!is.matrix(q)) {
      return("`q_matrix` must be a matrix.")
    }
    K <- ncol(q)
    if (is.null(K) || K < 1L) {
      return("`q_matrix` must have ≥ 1 column (skills).")
    }
    if (!is.null(object@prior_pi)) {
      L <- 2L^K
      pi <- object@prior_pi
      if (!is.numeric(pi)) {
        return("`prior_pi` must be numeric.")
      }
      if (length(pi) != L) {
        return(sprintf("`prior_pi` length (%d) must equal 2^K = %d.", length(pi), L))
      }
      if (any(!is.finite(pi)) || any(pi < 0)) {
        return("`prior_pi` must be nonnegative and finite.")
      }
      if (!isTRUE(all.equal(sum(pi), 1, tolerance = 1e-8))) {
        return("`prior_pi` must sum to 1 (within tolerance).")
      }
    }
  }
  TRUE
})
