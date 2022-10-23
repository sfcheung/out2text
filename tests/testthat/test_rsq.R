library(testthat)
library(out2text)

set.seed(894252)
n <- 10
x <- rnorm(n)
y <- .6 * x + rnorm(n, 0, .8)

lm_out <- lm(y ~ x)
text_rsq(lm_out)

glm_out <- glm(y ~ x)
text_rsq(glm_out)

text_rsq(n)
