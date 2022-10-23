library(testthat)
library(out2text)

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)

lm_out <- lm(y ~ x)
text_coef(lm_out, param = "x")

glm_out <- glm(y ~ x)
text_coef(glm_out, param = "x")

text_coef(n)
