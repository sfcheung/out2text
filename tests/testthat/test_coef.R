library(testthat)
library(out2text)

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)

lm_out <- lm(y ~ x)

glm_out <- glm(y ~ x)

test_that("lm and glm", {
    expect_identical(text_coef(lm_out, param = "x"),
                     "0.433")
    expect_identical(text_coef(glm_out, param = "x"),
                     "0.433")
  })


library(lavaan)
mod <-
"
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
textual ~ c(a1, a2) * visual
speed ~ c(b1, b2) * textual
"
fit <- sem(mod, data = HolzingerSwineford1939, group = "school")
est <- parameterEstimates(fit)
std <- standardizedSolution(fit)

test_that("lavaan", {
    expect_identical(text_coef(fit, param = "visual=~x3"),
                     "0.543")
    expect_identical(text_coef(fit, param = "textual~visual", standardized = TRUE),
                     "0.485")
    expect_identical(text_coef(fit, param = "visual=~x3", group = 2, standardized = TRUE),
                     "0.729")
    expect_identical(text_coef(fit, param = "x6 ~1", group = 2),
                     "2.469")
  })

# text_coef(n)
