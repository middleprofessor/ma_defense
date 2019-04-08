n <- 10^2
z <- rnorm(n)
rho <- 0.7
alpha <- sqrt(rho)
x1 <- alpha*z + sqrt(1-alpha^2)*rnorm(n)
x2 <- alpha*z + sqrt(1-alpha^2)*rnorm(n)
epsilon1 <- sqrt(1-0.4^2-0.8^2-2*(0.4)*(-0.8)*rho)*rnorm(n)
y1 <- 0.4*x1 - 0.8*x2 + epsilon1
epsilon2 <- -0.8*x2 + epsilon1
y2 <- 0.4*x1 + epsilon2
coefficients(summary(lm(y~x1+x2)))

fit <- lm(y2~x1)
e <- residuals(fit)
head(data.frame(y1=y1, y2=y2))
cor(x1,e)
cor(x1, epsilon1)
cor(x1, epsilon2)

s2 <- var(y2)
sst <- s2*(n-1)
ssm <- sum((fitted(fit) - mean(y2))^2)
sse <- sum((y2 - fitted(fit))^2)
sst2 <- sum((y2 - mean(y2))^2)
sst
sst2
ssm + sse