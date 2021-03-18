#Model

glmbpois <- function(formula1, formula2, formula3, data, max_iter, pres = 1e-5, var = F, B = 100, epsilon = 0.07){
  #Llibreries necessitem
  library(plyr)
  library(extraDistr)
  library(speedglm)
  library(parallel)
  
  #Funció trobar paràmetres
  funcio1 <- function(formula1, formula2, formula3, data, max_iter, pres = pres, epsilon = epsilon){
    
    form1_char <- as.character(formula1)
    form2_char <- as.character(formula2)
    form3_char <- as.character(formula3)
    formula3 <- as.formula(paste0("s ~ ", form3_char[2]))
  
  
    x <- data[form1_char[2]][[1]]
    y <- data[form2_char[2]][[1]]
  
    #Valors inicials (seleccionar epsilon mirant rmse (o absolut) i crossvalidation)
    lambda1 <- exp(speedglm(formula1, data = data, family = poisson(), fitted = T)$linear.predictors) * epsilon
    lambda2 <- exp(speedglm(formula2, data = data, family = poisson(), fitted = T)$linear.predictors) * epsilon
    lambda3 <- ((1 - epsilon) * lambda1 + (1 - epsilon) * lambda2) / 2
  
  
    n_iter = 0
    loglike_prev <- 1
    loglike <- 1
    difllike <- 1000
    while(n_iter < max_iter && difllike > pres){
      n_iter = n_iter + 1
      loglike_prev <- loglike
    
      #E-step
      a <- dbvpois(x, y, lambda1, lambda2, lambda3)
      s <-  ((x * y) != 0) * lambda3 * dbvpois(x - 1, y - 1, lambda1, lambda2, lambda3) / a
    
      loglike <- sum(log(a))
    
      data[, form1_char[2]] <- x - s
      data[, form2_char[2]] <- y - s
      data["s"] <- s
      
      #M-step
      lambda1 <- exp(speedglm(formula1, data = data, family = poisson(), fitted = T)$linear.predictors)
      lambda2 <- exp(speedglm(formula2, data = data, family = poisson(), fitted = T)$linear.predictors)
      lambda3 <- exp(speedglm(formula3, data = data, family = poisson(), fitted = T)$linear.predictors)
      lambda3[lambda3 == 0] <- 1e-50
    
      difllike <- abs((loglike_prev - loglike) / loglike_prev)
      print(n_iter)
    }
    mod1 <- speedglm(formula1, data = data, family = poisson())
    mod2 <- speedglm(formula2, data = data, family = poisson())
    mod3 <- speedglm(formula3, data = data, family = poisson())
  
    params = length(mod1$coefficients) + length(mod2$coefficients) + length(mod3$coefficients)
    AIC <- -2 * loglike + 2 * params
    BIC <- -2 * loglike + params * log(nrow(data))
    loglike_sat <- sum(log(dpois(x, x) * dpois(y, y)))
    deviance <- -2 * (loglike - loglike_sat)

    return(list(coefficients = list(mod1$coefficients, mod2$coefficients, mod3$coefficients), 
                fitted.values = data.frame(x = (lambda1 + lambda3), y = (lambda2 + lambda3)), 
                residuals = data.frame(resx = (x - (lambda1 + lambda3)), resy = (y - (lambda2 + lambda3))), lambda1 = lambda1,
                lambda2 = lambda2, lambda3 = lambda3, loglikelihood = loglike, AIC = AIC, BIC = BIC, parameters = params, 
                iterations = n_iter, deviance = deviance))
  }
  
  #Funció a l'environment global
  assign("funcio1", funcio1, envir = .GlobalEnv)
  
  #Si no volem variància tornem sense sd
  if(!var){
    return(funcio1(formula1, formula2, formula3, data, max_iter, pres, epsilon))
  }
  
  #Si volem variància trobem per bootstrap i en paral·lel
  if(var){
    
    #Model
    mod <- funcio1(formula1, formula2, formula3, data, max_iter, pres, epsilon)
    #Bootstrap en paral·lel
    cl<-makeCluster(detectCores())
    clusterExport(cl, "data")
    clusterExport(cl, "funcio1")
    clusterEvalQ(cl, {
      library(speedglm)
      library(plyr)
      library(extraDistr)
    })
    a <- parSapply(cl, 1:B, FUN = function(x){try(funcio1(formula1 = formula1, formula2 = formula2, formula3 = formula3, 
                  data = data[sample(1:nrow(data), replace = T, size = nrow(data)), ], max_iter = max_iter, pres = pres, epsilon = epsilon
    )$coefficients)}, simplify = F)
    stopCluster(cl)
    
    Summary = list()
    for(l in 1:length(mod$coefficients)){
      coefs_aux <- as.data.frame(t(sapply(1:B, FUN = function(x){
        if(class(a[[x]][[l]]) != "character") return(a[[x]][[l]])      
        })))
      sd_aux <- apply(coefs_aux, 2, sd)
      mod[['Desv. Std.']][[paste0("Coef", as.character(l))]] <- sd_aux

      d_aux <- data.frame(Coefficients = mod$coefficients[[l]])
      d_aux['Desv. Std.'] <- mod$`Desv. Std.`[[l]]
      d_aux['p-value'] <- round(1 - pchisq(d_aux$Coefficients^2 / d_aux$`Desv. Std.`^2, df = 1), 4)
      d_aux['sign'] <- "-"
      d_aux$sign[d_aux$`p-value` < 0.1] <- "."
      d_aux$sign[d_aux$`p-value` < 0.05] <- "*"
      d_aux$sign[d_aux$`p-value` < 0.01] <- "**"
      d_aux$sign[d_aux$`p-value` < 0.001] <- "***"
      Summary[[paste0("coef", as.character(l))]] <- d_aux
    }
    
    
    mod[['Summary']] <- Summary
    
    #Tornem model
    return(mod)
  }
  
}

#Prediccions

pred_glmbpois <- function(mod, newdata = NULL, type = "link"){
  if(is.null(newdata)){
    if(type == "link") return(data.frame(lambda1 = log(mod$lambda1), lambda2 = log(mod$lambda2), lambda3 = log(mod$lambda3)))
    else if(type == "response") return(data.frame(lambda1 = mod$lambda1, lambda2 = mod$lambda2, lambda3 = mod$lambda3))
  }
  
  else{
    newdata["(Intercept)"] <- 1
    lambda1 <- exp(as.matrix(newdata[names(mod$coefficients[[1]])]) %*% as.matrix(mod$coefficients[[1]]))
    lambda2 <- exp(as.matrix(newdata[names(mod$coefficients[[2]])]) %*% as.matrix(mod$coefficients[[2]]))
    lambda3 <- exp(as.matrix(newdata[names(mod$coefficients[[3]])]) %*% as.matrix(mod$coefficients[[3]]))
    lambda3[lambda3 == 0] <- 1e-200
    if(type == "link") return(data.frame(lambda1 = log(lambda1), lambda2 = log(lambda2), lambda3 = log(lambda3)))
    else if(type == "response") return(data.frame(lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3))
  }
}



