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
y2 <- .6 * x2 + rnorm(n, 0, .8)
lm_out2 <- lm(y2 ~ x2)
summary(lm_out2)

set.seed(135345)
n3 <- 25
x3 <- rnorm(n3)
y3 <- .6 * x3 + rnorm(n, 0, .8)
lm_out3 <- lm(y3 ~ x3)
summary(lm_out3)


text_coef_p(lm_out, param = "x")
text_coef_p(lm_out2, param = "x2")
text_coef_p(lm_out3, param = "x3")


glm_out <- glm(y ~ x)
text_coef_p(glm_out, param = "x")

# text_coef(n)
