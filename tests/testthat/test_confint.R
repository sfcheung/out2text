library(testthat)
library(out2text)

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)

lm_out <- lm(y ~ x)

glm_out <- glm(y ~ x)

test_that("lm and glm", {
    expect_identical(text_confint(lm_out, param = "x"),
                     "[0.159, 0.708]")
    expect_identical(text_confint(glm_out, param = "x"),
                     "[0.200, 0.666]")
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
set.seed(870452)
id <- sample(nrow(HolzingerSwineford1939), 100)
fit <- sem(mod, data = HolzingerSwineford1939[id, ], group = "school")
est <- parameterEstimates(fit)
std <- standardizedSolution(fit)

test_that("lavaan", {
    expect_identical(text_confint(fit, param = "visual=~x2"),
                     "[-0.151, 0.685]")
    expect_identical(text_confint(fit, param = "x6~~x6", standardized = TRUE),
                     "[0.036, 0.371]")
    expect_identical(text_confint(fit, param = "x7~~x7", group = 2, standardized = TRUE),
                     "[0.124, 0.595]")
    expect_identical(text_confint(fit, param = "x6 ~1", group = 2),
                     "[2.375, 2.963]")
  })

# text_confint(n)
