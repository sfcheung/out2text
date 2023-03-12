library(testthat)
library(out2text)

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)

lm_out <- lm(y ~ x)

text_rsq(lm_out)
test_that("lm", {
    expect_identical(text_rsq(lm_out),
                     "0.624")
  })

glm_out <- glm(y ~ x)

test_that("glm", {
    expect_error(text_rsq(glm_out))
  })

test_that("NULL", {
    expect_identical(text_rsq(n), NULL)
  })


test_that("F statistics", {
    expect_identical(text_rsq_f(lm_out),
                     "F(1, 8) = 13.254")
  })

test_that("F statistics", {
    expect_identical(text_rsq_p(lm_out),
                     "p = 0.007")
  })
