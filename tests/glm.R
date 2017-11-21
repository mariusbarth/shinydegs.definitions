library(shinydegs.definitions)

I <- 200

x1 <- rnorm(I)
x2 <- rnorm(I)
y <- rbinom(n = I, size = 1, prob = pnorm(x1 * .4 + x2 * .3 + rnorm(I), sd = 2))

data <- data.frame(
  x1 = x
  , y = y
)

m1 <- glm(formula = y ~ x1 + x2, data = data, family = binomial(link = "logit"), x = TRUE, y = TRUE)

plot_marginal(m1, x1 = "x1")
str(m1)

library(rockchalk)
plot(x = x1, y = y)
plotCurves(m1, plotx = "x2")
plot(residuals(m1))


plot(x = m1$x[, "x1"], y = m1$y)

