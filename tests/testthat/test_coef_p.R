library(testthat)
library(out2text)

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)
lm_out <- lm(y ~ x)
summary(lm_out)

set.seed(53424321)
n2 <- 15
x2 <- rnorm(n2)
y2 <- .6 * x2 + rnorm(n2, 0, .8)
lm_out2 <- lm(y2 ~ x2)
summary(lm_out2)

set.seed(135345)
n3 <- 25
x3 <- rnorm(n3)
y3 <- .6 * x3 + rnorm(n3, 0, .8)
lm_out3 <- lm(y3 ~ x3)
summary(lm_out3)

glm_out <- glm(y ~ x)

test_that("lm and glm", {
    expect_identical(text_coef_p(lm_out, param = "x"),
                     "p = 0.007")
    expect_identical(text_coef_p(lm_out2, param = "x2"),
                     "p = 0.002")
    expect_identical(text_coef_p(lm_out3, param = "x3"),
                     "p = 0.010")
    expect_identical(text_coef_p(glm_out, param = "x"),
                     "p = 0.007")
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
    expect_identical(text_coef_p(fit, param = "visual=~x2"),
                     "p = 0.211")
    expect_identical(text_coef_p(fit, param = "x6~~x6", standardized = TRUE),
                     "p = 0.017")
    expect_identical(text_coef_p(fit, param = "x7~~x7", group = 2, standardized = TRUE),
                     "p = 0.003")
    expect_identical(text_coef_p(fit, param = "x6 ~1", group = 2),
                     "p < 0.001")
  })

# text_coef(n)
