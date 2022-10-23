#' @title A Confidence Interval as Text
#'
#' @description Extracts a confidence interval from an object
#' and returns the interval as a text string.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return A one-element character vector of the interval.
#'
#' @param x A supported object with coefficients in the results.
#' @param param The name of the term or parameter for which
#' the coefficients will be returned.
#' @param level The level of confidence. Default is .95.
#' @param digits The number of decimal places to keep. To be
#' passed to [formatC()].
#' @param format The format string used in [formatC()]. Default
#' is `"f"`.
#' @param sep The separator. Default is `", "`.
#' @param brackets The brackets. A character vector with
#' two elements. Default is `c("[", "]")`.
#' @param ...  Optional arguments. To be passed to
#' [stats::confint()].
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
#' text_confint(out, "x")
#'
#' @export

text_confint <- function(x, ...) UseMethod("text_confint")

#' @export

text_confint.default <- function(x,
                          param = NULL,
                          level = .95,
                          digits = 3,
                          format = "f",
                          sep = ", ",
                          brackets = c("[", "]"),
                          ...) {
    if (is.null(param) || length(param) != 1 ||
        !is.character(param)) {
        stop("param must be a length 1 character.")
      }
    ci0 <- tryCatch(suppressMessages(stats::confint(object = x,
                                   parm = param,
                                   level = level,
                                   ...)),
                    error = function(e) e)
    if (inherits(ci0, "error")) {
        stop("confint() failed to extract coefficients.")
      }
    if (is.null(dim(ci0))) {
        out0 <- ci0
      } else {
        if (nrow(ci0) == 1) {
            out0 <- ci0[1, , drop = TRUE]
          } else {
            out0 <- ci0[param, , drop = TRUE]
          }
      }
    out1 <- formatC(out0,
                    digits = digits,
                    format = format)
    out <- paste0(brackets[1],
                  paste0(out1, collapse = sep),
                  brackets[2])
    out
  }


#' @export

text_confint.lm <- function(x,
                          param = NULL,
                          level = .95,
                          digits = 3,
                          format = "f",
                          sep = ", ",
                          brackets = c("[", "]"),
                          ...) {
    NextMethod()
  }

#' @export

text_confint.glm <- function(x,
                          param = NULL,
                          level = .95,
                          digits = 3,
                          format = "f",
                          sep = ", ",
                          brackets = c("[", "]"),
                          ...) {
    NextMethod()
  }