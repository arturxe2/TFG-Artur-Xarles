####################################################################
##FUNCIÓ PER PREDIR RESULTAT SENSE DIAGONAL INFLADA
####################################################################

pred_result_bpois <- function(lambda1, lambda2, lambda3 = NULL, type = "prob"){
  library(extraDistr)
  probs <- t(sapply(1:length(lambda1), FUN = function(x){
    return((c(sum(dskellam(1:100, lambda1[x], lambda2[x])), dskellam(0, lambda1[x], 
                      lambda2[x]), sum(dskellam(-100:-1, lambda1[x], lambda2[x])))))
  }))
  colnames(probs) <- c("1", "X", "2")
  if(type == "prob") return(probs)
  else if(type == "class") return(colnames(probs)[apply(probs, 1, which.max)])
  else if(type == "expected"){
    if(is.null(lambda3)) lambda3 <- 0
    return(data.frame(x = lambda1 + lambda3, y = lambda2 + lambda3))
  }
}

####################################################################
##FUNCIÓ PER PREDIR RESULTAT AMB DIAGONAL INFLADA
####################################################################

pred_result_bpoisDI <- function(lambda1, lambda2, lambda3 = NULL, p, 
                                theta = NULL, type = "prob"){
  library(extraDistr)
  lambda1 <- as.vector(lambda1)
  lambda2 <- as.vector(lambda2)
  lambda3 <- as.vector(lambda3)
  probs <- t(sapply(1:length(lambda1), FUN = function(x){
      return((c((1 - p[x]) * sum(dskellam(1:100, lambda1[x], lambda2[x])), 
            (1 - p[x]) * dskellam(0, lambda1[x], lambda2[x]) + p[x], 
            (1 - p[x]) * sum(dskellam(-100:-1, lambda1[x], lambda2[x])))))
  }))
  colnames(probs) <- c("1", "X", "2")
  if(type == "prob") return(probs)
  else if(type == "class") return(colnames(probs)[apply(probs, 1, which.max)])
  else if(type == "expected"){
    if(is.null(lambda3)) lambda3 <- 0
    return(data.frame(x = ((1 - p) * (lambda1 + lambda3) + p * theta), 
                      y = ((1 - p) * (lambda2 + lambda3) + p * theta)))
  }
}
