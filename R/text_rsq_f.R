#' @title The F Statistics of R-square as Text
#'
#' @description Extracts the F statistics of
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
#' text_rsq_f(out)
#'
#' @export

text_rsq_f <- function(x, ...) UseMethod("text_rsq_f")

#' @export
#' @rdname text_rsq_f

text_rsq_f.default <- function(x,
                             digits = getOption("out2text_coef_digits", 3),
                             format = getOption("out2text_coef_format", "f"),
                             italics = FALSE,
                             ...) {
    NULL
  }


#' @export
#' @rdname text_rsq_f

text_rsq_f.lm <- function(x,
                        digits = getOption("out2text_coef_digits", 3),
                        format = getOption("out2text_coef_format", "f"),
                        italics = FALSE,
                        ...) {
    fstat <- summary(x)$fstatistic
    if (is.null(fstat)) {
        stop("The summary of the object does not have the F statistic.")
      }
    out <- text_f(fstat,
                  digits = digits,
                  format = format,
                  italics = italics,
                   ...)
    names(out) <- NULL
    out
  }

#' @noRd

text_f <- function(fstat,
                   df1,
                   df2,
                   digits = getOption("out2text_test_digits", 3),
                   format = getOption("out2text_test_format", "f"),
                   italics = FALSE,
                   ...) {
    if (length(fstat == 3)) {
        fstat_old <- fstat
        df2 <- fstat[3]
        df1 <- fstat[2]
        fstat <- fstat[1]
      }
    out <- paste0(ifelse(italics, "*F*", "F"),
                  "(",
                  df1,
                  ", ",
                  df2,
                  ")",
                  " = ",
                  formatC(fstat,
                          digits = digits,
                          format = format,
                          ...)
                  )
    out
  }