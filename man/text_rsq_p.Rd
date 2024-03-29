% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_rsq_p.R
\name{text_rsq_p}
\alias{text_rsq_p}
\alias{text_rsq_p.default}
\alias{text_rsq_p.lm}
\title{The P-Value of the F Test of R-square as Text}
\usage{
text_rsq_p(x, ...)

\method{text_rsq_p}{default}(
  x,
  digits = getOption("out2text_test_p_digits", 3),
  format = getOption("out2text_test_p_format", "f"),
  italics = FALSE,
  ...
)

\method{text_rsq_p}{lm}(
  x,
  digits = getOption("out2text_test_p_digits", 3),
  format = getOption("out2text_test_p_format", "f"),
  italics = FALSE,
  ...
)
}
\arguments{
\item{x}{A supported object with a F statistic the results.}

\item{...}{Optional arguments. Ignored for now.}

\item{digits}{The number of decimal places to keep. To be
passed to \code{\link[=formatC]{formatC()}}.}

\item{format}{The format string used in \code{\link[=formatC]{formatC()}}. Default
is \code{"f"}.}

\item{italics}{Logical. Whether the statistics should be
italicized (using markdown). Default is \code{FALSE}.}
}
\value{
A one-element character vector.
}
\description{
Extracts the p-value of the
F statistics of
the R-square from an object
and returns it as a text string.
}
\details{
Details
(Include subjects for verbs.)
(Use 3rd person forms for verbs.)
}
\examples{

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)
out <- lm(y ~ x)
text_rsq_p(out)

}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
