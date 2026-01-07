#set.seed(123)
# No sé qué pasó
A <- factor(rep(c(1, 0), each = 112))
B <- factor(rep(c(1, 0), each = 112))
C <- factor(rep(c(1, 0), each = 112))
e <- runif(112, 4, 10)

param <-  c(-10, 50)

y <- 2 * (A==1) + param[1] * (B==1) + param[2] * (C==1) + e

datos <- data.frame(y, A, B, C)
m0 <-  summary(lm(y ~ A, datos))


# Muestra aleatoria de población con efectos B y C desconocidos
# No quiero saber que pasó
muestra <- sample(param[1] * (B==1), size = 112, replace = F) +
           sample(param[2] * (C==1), size = 112, replace = F)

y1 <- 2 * (A==1) + muestra + e

datos1 <- data.frame(y1, A, muestra)
m1 <-  summary(lm(y1 ~ A , datos1))

m0$coefficients
mean(y1)
m1$coefficients

library(dagitty)

no_se <- dagitty("dag{B[unobserved] -> A ;
                      C[unobserved] -> A ;
                      e[unobserved] -> A[exposure]
                      A -> y[outcome]}")

plot(no_se)
