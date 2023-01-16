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
text_coef(fit, param = "visual=~x3")
est[3, ]
text_coef(fit, param = "textual~visual", standardized = TRUE)
std[10, ]
text_coef(fit, param = "visual=~x3", group = 2, standardized = TRUE)
std[38, ]
text_coef(fit, param = "x6 ~1", group = 2)
est[64, ]

# text_coef(n)
