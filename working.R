# New script for MA simulation
# June 8, 2018
# Jeffrey A. Walker
# Motivation - not much simulation on efficacy of MA to estimate generating coefficients.

library(ggplot2)
library(data.table)



fake.eigenvectors <- function(u){
  a <- matrix(rnorm(u*u),u,u)
  a <- t(a)%*%a
  E <- eigen(a)$vectors
  return(E)
}

fake.eigenvalues.fun <- function(u, f){
  # the lth eigenvalue is 1/f the previous eigenvalue
  L <- f^(seq(1:u))
  L <- L/sum(L)*u
  L
  L/sum(L)
}