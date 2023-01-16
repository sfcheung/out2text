#' @title The P-Value of a Coefficient as Text
#'
#' @description Extracts the p-value of a coefficient from
#' an object
#' and returns it as a text string.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return A one-element character vector.
#'
#' @param x A supported object with coefficients in the
#' results.
#' @param param The name of the term or parameter for
#' which
#' the coefficients will be returned.
#' @param digits The number of decimal places to keep.
#' To be
#' passed to [formatC()].
#' @param format The format string used in [formatC()].
#' Default
#' is `"f"`.
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
#' text_coef_p_p(out, "x")
#'
#' @export

text_coef_p <- function(x, ...) UseMethod("text_coef_p")

#' @export

text_coef_p.default <- function(x,
                                param = NULL,
                                digits = getOption("out2text_coef_p_digits", 3),
                                format = getOption("out2text_coef_p_format", "f"),
                                ...) {
    NULL
  }


#' @export

text_coef_p.lm <- function(x,
                           param = NULL,
                           digits = getOption("out2text_coef_p_digits", 3),
                           format = getOption("out2text_coef_p_format", "f"),
                          ...) {
    if (is.null(param) || length(param) != 1 ||
        !is.character(param)) {
        stop("param must be a length 1 character.")
      }
    coefm <- summary(x)$coefficients
    out0 <- coefm[param, "Pr(>|t|)"]
    out <- format_pval(out0, digits = digits,
                       format = format)
    out
  }

#' @export

text_coef_p.glm <- function(x,
                            param = NULL,
                            digits = getOption("out2text_coef_p_digits", 3),
                            format = getOption("out2text_coef_p_format", "f"),
                            ...) {
    NextMethod()
  }

#' @export

text_coef_p.lavaan <- function(x,
                               param = NULL,
                               digits = getOption("out2text_coef_p_digits", 3),
                               format = getOption("out2text_coef_p_format", "f"),
                               standardized = FALSE,
                               group = 1L,
                               ...) {
    param_str <- lavaan::lavParseModelString(
                    model.syntax = param,
                    as.data.frame. = TRUE,
                    warn = FALSE)
    if (standardized) {
        ct <- lavaan::standardizedSolution(x,
                                           se = TRUE,
                                           zstat = TRUE,
                                           pvalue = TRUE,
                                           ci = FALSE,
                                           ...)
        ct$est <- ct$est.std
      } else {
        ct <- lavaan::parameterEstimates(x, ...)
      }
    if (lavInspect(x, "ngroups") == 1) {
        ct$group <- 1
      }
    coef1 <- ct[(ct$lhs == param_str$lhs) &
                (ct$op == param_str$op) &
                (ct$rhs == param_str$rhs) &
                (ct$group == group), "pvalue"]
    out <- format_pval(coef1, digits = digits,
                       format = format)
    out
  }

#' @noRd

format_pval <- function(x,
                        digits = 3,
                        format = "f") {
    pmin <- 10^(-digits)
    if (x < pmin) {
        out <- paste0("p < ",
                      formatC(pmin, digits = digits,
                              format = format))
      } else {
        out <- paste0("p = ",
                      formatC(x, digits = digits,
                              format = format))
      }
    return(out)
  }