#' @title The P-Value of the F Test of R-square as Text
#'
#' @description Extracts the p-value of the
#' F statistics of
#' the R-square from an object
#' and returns it as a text string.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return A one-element character vector.
#'
#' @param x A supported object with a F statistic the results.
#'
#' @param digits The number of decimal places to keep. To be
#' passed to [formatC()].
#'
#' @param format The format string used in [formatC()]. Default
#' is `"f"`.
#'
#' @param italics Logical. Whether the statistics should be
#' italicized (using markdown). Default is `FALSE`.
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
#' text_rsq_p(out)
#'
#' @export

text_rsq_p <- function(x, ...) UseMethod("text_rsq_p")

#' @export
#' @rdname text_rsq_p

text_rsq_p.default <- function(x,
                             digits = getOption("out2text_test_p_digits", 3),
                             format = getOption("out2text_test_p_format", "f"),
                             italics = FALSE,
                             ...) {
    NULL
  }


#' @export
#' @rdname text_rsq_p

text_rsq_p.lm <- function(x,
                        digits = getOption("out2text_test_p_digits", 3),
                        format = getOption("out2text_test_p_format", "f"),
                        italics = FALSE,
                        ...) {
    fstat <- summary(x)$fstatistic
    if (is.null(fstat)) {
        stop("The summary of the object does not have the F statistic.")
      }
    fstatp <- stats::pf(q = fstat["value"],
                 df1 = fstat["numdf"],
                 df2 = fstat["dendf"],
                 lower.tail = FALSE)
    out <- format_pval(fstatp)
    names(out) <- NULL
    out
  }
