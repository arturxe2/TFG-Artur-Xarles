library(plyr)
library(readxl)
library(extraDistr)


glmbpois <- function(w1, w2, w3, x, y, max_iter, pres = 1e-8){
  
  formula1 <- paste0(names(w1), collapse = " + ")
  formula2 <- paste0(names(w2), collapse = " + ")
  formula3 <- paste0(names(w3), collapse = " + ")
  
  coef_inicial1 <- glm(as.formula(paste0("x ~ ", formula1)), data = w1, family = "poisson")$coefficients
  coef_inicial2 <- glm(as.formula(paste0("y ~ ", formula2)), data = w2, family = "poisson")$coefficients
  lambda1 <- exp(as.matrix(w1) %*% as.matrix(coef_inicial1[2: length(coef_inicial1)]) + coef_inicial1[1]) * 0.5
  lambda2 <- exp(as.matrix(w2) %*% as.matrix(coef_inicial2[2: length(coef_inicial2)]) + coef_inicial2[1]) * 0.5
  lambda3 <- exp(as.matrix(w1) %*% as.matrix(coef_inicial1[2: length(coef_inicial1)]) + coef_inicial1[1]) * 0.25 + exp(as.matrix(w2) %*% as.matrix(coef_inicial2[2: length(coef_inicial2)]) + coef_inicial2[1]) * 0.25
  
  r = 1
  n_iter = 0
  loglike_prev <- 0
  loglike <- 0
  difllike <- 1000
  while(n_iter < max_iter && difllike > pres){
    n_iter = n_iter + 1
    loglike_prev <- loglike
    pre_lambda1 <- lambda1
    pre_lambda2 <- lambda2
    pre_lambda3 <- lambda3
    
    #E-step
    s <- sapply(1:nrow(w1), FUN = function(i){
      if(x[i] * y[i] != 0){lambda3[i] * dbvpois(x[i] - 1, y[i] - 1, lambda1[i], lambda2[i], lambda3[i]) / 
          dbvpois(x[i], y[i], lambda1[i], lambda2[i], lambda3[i])}
      else {0}
    })
    loglike <- sum(sapply(1:nrow(w1), FUN = function(i){
      dbvpois(x[i], y[i], lambda1[i], lambda2[i], lambda3[i], log = T)
    }))
    
    
    #M-step
    coef1 <- glm(as.formula(paste0("(x - s) ~ ", formula1)), data = w1, family = "poisson")$coefficients
    coef2 <- glm(as.formula(paste0("(y - s) ~ ", formula2)), data = w2, family = "poisson")$coefficients
    coef3 <- glm(as.formula(paste0("(s) ~ ", formula3)), data = w3, family = "poisson")$coefficients
    
    lambda1 <- exp(as.matrix(w1) %*% as.matrix(coef1[2: length(coef1)]) + coef1[1])
    lambda2 <- exp(as.matrix(w2) %*% as.matrix(coef2[2: length(coef2)]) + coef2[1])
    lambda3 <- exp(as.matrix(w3) %*% as.matrix(coef3[2: length(coef3)]) + coef3[1])
    
    difllike<-abs((loglike_prev - loglike) / loglike_prev)
  }
  
  params = length(coef1) + length(coef2) + length(coef3)
  AIC = -2 * loglike + 2 * params
  BIC = -2 * loglike + params * log(nrow(w1))
  return(list(coefficients = list(coef1, coef2, coef3), 
              fitted.values = data.frame(x = (lambda1 + lambda3), y = (lambda2 + lambda3)), 
              residuals = data.frame(resx = (x - (lambda1 + lambda3)), resy = (y - (lambda2 + lambda3))), lambda1 = lambda1,
              lambda2 = lambda2, lambda3 = lambda3, loglikelihood = loglike, AIC = AIC, BIC = BIC, parameters = params, 
              iterations = n_iter))
}