#' @title A R-square as Text
#'
#' @description Extracts the R-square from an object
#' and returns it as a text string.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return A one-element character vector.
#'
#' @param x A supported object with an R-square in the results.
#'
#' @param digits The number of decimal places to keep. To be
#' passed to [formatC()].
#'
#' @param format The format string used in [formatC()]. Default
#' is `"f"`.
#'
#' @param type The type of R-squared to be extracted.
#' Either `"unadjusted"`, the default, or `"adjusted"`,
#' which returns the adjusted R-squared.
#'
#'
#' @param ...  Optional arguments. Ignored for now.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' set.seed(894252)
#' n <- 10
#' x <- rnorm(n)
#' y <- .6 * x + rnorm(n, 0, .8)
#' out <- lm(y ~ x)
#' text_rsq(out)
#'
#' @export

text_rsq <- function(x, ...) UseMethod("text_rsq")

#' @export
#' @rdname text_rsq

text_rsq.default <- function(x,
                             digits = getOption("out2text_coef_digits", 3),
                             format = getOption("out2text_coef_format", "f"),
                             ...) {
    NULL
  }


#' @export
#' @rdname text_rsq

text_rsq.lm <- function(x,
                        digits = getOption("out2text_coef_digits", 3),
                        format = getOption("out2text_coef_format", "f"),
                        type = c("unadjusted", "adjusted"),
                        ...) {
    type <- match.arg(type)
    out <- switch(type,
                  unadjusted = summary(x)$r.squared,
                  adjusted = summary(x)$adj.r.squared)
    if (is.null(out)) {
        stop("The summary of the object does not have the requested R-squared.")
      }
    out <- formatC(out,
                   digits = digits,
                   format = format,
                   ...)
    names(out) <- NULL
    out
  }
