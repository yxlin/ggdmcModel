#' @keywords internal
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("ANYOrNULL", c("ANY", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
