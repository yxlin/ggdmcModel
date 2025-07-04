% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{BuildModel}
\alias{BuildModel}
\title{Build a model object}
\usage{
BuildModel(
  p_map = list(A = "1", B = "1", mean_v = "M", sd_v = "1", st0 = "1", t0 = "1"),
  accumulators = c("r1", "r2"),
  factors = list(S = c("s1", "s2")),
  match_map = list(M = list(s1 = "r1", s2 = "r2")),
  constants = c(sd_v = 1, st0 = 0),
  type = "lba",
  verbose = TRUE
)
}
\arguments{
\item{p_map}{descibes the association between the parameter and the
experimental factor.}

\item{accumulators}{specifies the response names and its levels.}

\item{factors}{specifies a list of factors and their levels/conditions.}

\item{match_map}{describes which the stimulus condition matches which response
level, resulting in a correct or an incorrect response.}

\item{constants}{the argument allows the user to decide which parameter is set to a
constant value.}

\item{type}{the model type defined in the package, "fastdm", "hyper", or "lba".}

\item{verbose}{print design information}
}
\description{
The function performs a series of syntax checks to ensure the user enters
strings/values conforming the C++ internal setting.
}
\examples{
## A diffusion decision model
\dontrun{
model <- ggdmcModel::BuildModel(
    p_map = list(
        a = c("S", "COLOUR"), v = c("NOISE"), z = "1", d = "1", sz = "1", sv = "1",
        t0 = "1", st0 = "1", s = "1", precision = "1"
    ),
    match_map = list(M = list(left = "z_key", right = "x_key")),
    factors = list(
        S = c("left", "right"), COLOUR = c("red", "blue"),
        NOISE = c("high", "moderate", "low")
    ),
    constants = c(d = 0, s = 1, st0 = 0, sv = 0, precision = 3),
    accumulators = c("z_key", "x_key"),
    type = "fastdm"
)
}
\dontrun{
## A LBA model
model <- ggdmcModel::BuildModel(
    p_map = list(
        A = "1", B = c("S", "COLOR"), t0 = "1", mean_v = c("NOISE", "M"),
        sd_v = "M", st0 = "1"
    ),
    match_map = list(M = list(left = "z_key", right = "x_key")),
    factors = list(
        S = c("left", "right"),
        COLOR = c("red", "blue"),
        NOISE = c("high", "moderate", "low")
    ),
    constants = c(st0 = 0, sd_v.false = 1),
    accumulators = c("z_key", "x_key"),
    type = "lba"
)
}

}
