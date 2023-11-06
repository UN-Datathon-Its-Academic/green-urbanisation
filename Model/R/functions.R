# function 

## 95% coverage
coverage95 <- function(z, lower, upper) {
  sum((z < upper) & (z > lower)) / length(z)
}

## 95% interval score
IS95 <- function(true, lower, upper) {
  alpha = 0.1
  pred95l <- lower 
  pred95u <- upper
  ISs <- (pred95u - pred95l) + 2/alpha * (pred95l - true) * (true < pred95l) +
    2/alpha * (true - pred95u) * (true > pred95u)
  mean(ISs)
}

## Root-mean-squared prediction error
RMSPE <- function(z,pred) {
  Y <- (z - pred)^2
  sqrt(mean(Y))
}

