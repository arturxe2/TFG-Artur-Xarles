##############################################
###FUNCIÓ DE DISTRIBUCIÓ DE PROBABILITAT BPDI
##############################################

dbvpoisDI <- function(x, y, lambda1, lambda2, lambda3, p, theta, log = F){
  if(!log){
    return((1 - p) * dbvpois(x, y, lambda1, lambda2, lambda3) + 
             (x == y) * p * dpois(x, theta))
  }
  if(log){
    return(log((1 - p) * dbvpois(x, y, lambda1, lambda2, lambda3) + 
                 (x == y) * p * dpois(x, theta)))
  }
}

##############################################
###FUNCIÓ PER FER REGRESSIÓ 2PDI
##############################################

glmbpoisDI <- function(formula1, formula2, formula3, formula4 = NULL, formula5 = NULL, 
                  data, max_iter = 100, pres = 1e-5, bootstrap = F, B = 100, epsilon = 0.5){
  #Llibreries necessitem
  library(plyr)
  library(extraDistr)
  library(speedglm)
  library(parallel)
  
  #Funció trobar paràmetres amb l'algoritme EM
  funcio1 <- function(formula1, formula2, formula3, formula4, formula5, data, max_iter, 
                      pres = pres, epsilon = epsilon){
    
    form1_char <- as.character(formula1)
    form2_char <- as.character(formula2)
    form3_char <- as.character(formula3)
    formula3 <- as.formula(paste0("s ~ ", form3_char[2]))
    if(!is.null(formula4)) formula4_aux <- as.formula(paste0("v ~ ", 
                                                             as.character(formula4)[2]))
    if(!is.null(formula5)) formula5_aux <- as.formula(paste0("theta ~ ", 
                                                             as.character(formula5)[2]))
  
  
    x <- data[form1_char[2]][[1]]
    y <- data[form2_char[2]][[1]]
  
    #Valors inicials
    lambda1 <- exp(speedglm(formula1, data = data, family = poisson(), 
                            fitted = T)$linear.predictors) * epsilon
    lambda2 <- exp(speedglm(formula2, data = data, family = poisson(), 
                            fitted = T)$linear.predictors) * epsilon
    lambda3 <- ((1 - epsilon) * lambda1 + (1 - epsilon) * lambda2) / 2
    p <- 0.1
    theta <- 1
  
    n_iter = 0
    loglike_prev <- 1
    loglike <- 1
    difllike <- 1000
    while(n_iter < max_iter && difllike > pres){
      n_iter = n_iter + 1
      loglike_prev <- loglike
    
      #E-step
      a <- dbvpois(x, y, lambda1, lambda2, lambda3)
      b <- dbvpoisDI(x, y, lambda1, lambda2, lambda3, p, theta)
      s <-  ((x * y) != 0) * lambda3 * dbvpois(x - 1, y - 1, lambda1, lambda2, 
                                               lambda3) / a
      v <- p * dpois(x, lambda = theta) / b * (x == y)
    
      loglike <- sum(log(b))
      difllike <- abs((loglike_prev - loglike) / loglike_prev)
      if(difllike < pres) break;
    
      data[, form1_char[2]] <- x - s
      data[, form2_char[2]] <- y - s
      data["s"] <- s
      data["v"] <- v
      data["theta"] <- x
      
      #M-step
      lambda1 <- exp(speedglm(formula1, data = data, family = poisson(), fitted = T, 
                              weights = (1 - v))$linear.predictors)
      lambda2 <- exp(speedglm(formula2, data = data, family = poisson(), fitted = T, 
                              weights = (1 - v))$linear.predictors)
      lambda3 <- exp(speedglm(formula3, data = data, family = poisson(), fitted = T, 
                              weights = (1 - v))$linear.predictors)
      lambda3[lambda3 == 0] <- 1e-50
      if(!is.null(formula4)){p <- 1 / (1 + exp(-speedglm(formula4_aux, data = data, 
                              family = binomial(), fitted = T)$linear.predictors))}
      else{p <- rep(mean(v), nrow(data))}
      if(!is.null(formula5)){theta <- exp(speedglm(formula5_aux, data = data, 
                    family = poisson(), fitted = T, weights = (v))$linear.predictors)}
      else{theta <- rep(weighted.mean(x, v), nrow(data))}
    
      print(n_iter)
    }
    mod1 <- speedglm(formula1, data = data, family = poisson())
    mod2 <- speedglm(formula2, data = data, family = poisson())
    mod3 <- speedglm(formula3, data = data, family = poisson())
    params4 <- 1
    params5 <- 1
    if(!is.null(formula4)){
      mod4 <- speedglm(formula4_aux, data = data, family = binomial(), fitted = T)
      params4 <- length(mod4$coefficients)
    }
    if(!is.null(formula5)){
      mod5 <- speedglm(formula5_aux, data = data, family = poisson(), fitted = T)
      params5 <- length(mod5$coefficients)
    }
  
    #Calculem alguens mètriques dels models
    params = length(mod1$coefficients) + length(mod2$coefficients) + 
                        length(mod3$coefficients) + params4 + params5 
    AIC <- -2 * loglike + 2 * params
    BIC <- -2 * loglike + params * log(nrow(data))
  
    #Model a retornar
    mod <- list(coefficients = list(coef1 = mod1$coefficients, coef2 = mod2$coefficients, 
                                    coef3 = mod3$coefficients),
                  fitted.values = list(lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3, 
                                       p = p, theta = theta), 
                  loglikelihood = loglike, AIC = AIC, BIC = BIC, parameters = params, 
                  iterations = n_iter, mod1 = mod1, mod2 = mod2, mod3 = mod3)
    if(!is.null(formula4)){
      mod$coefficients$coef4 = mod4$coefficients
      mod$mod4 = mod4
      }
    if(!is.null(formula5)){
      mod$coefficients$coef5 = mod5$coefficients
      mod$mod5 = mod5
    }
    
    #Summary amb desv. típica coeficients i p_valor
    Summary = list()
    for(l in 1:length(mod$coefficients)){
      sd_aux <- summary(get(paste0("mod", l)))$coefficients['Std. Error']
      mod[['Desv. Std.']][[paste0("Coef", as.character(l))]] <- sd_aux
      
      d_aux <- data.frame(Coefficients = mod$coefficients[[l]])
      d_aux['Desv. Std.'] <- mod$`Desv. Std.`[[l]]
      d_aux['p-value'] <- round(1 - pchisq(d_aux$Coefficients^2 / d_aux$`Desv. Std.`^2, 
                                           df = 1), 4)
      d_aux['sign'] <- "-"
      d_aux$sign[d_aux$`p-value` < 0.1] <- "."
      d_aux$sign[d_aux$`p-value` < 0.05] <- "*"
      d_aux$sign[d_aux$`p-value` < 0.01] <- "**"
      d_aux$sign[d_aux$`p-value` < 0.001] <- "***"
      Summary[[paste0("coef", as.character(l))]] <- d_aux
    }
    
    mod[['Summary']] <- Summary
    
    #Retornem el model
    return(mod)
  }
  
  #Funció a l'environment global
  assign("funcio1", funcio1, envir = .GlobalEnv)
  
  #Retornem amb les desviacions típiques del GLM
  if(!bootstrap){
    return(funcio1(formula1, formula2, formula3, formula4, formula5, data, max_iter, 
                   pres, epsilon))
  }
  
  #Calculem desviacions típiques amb bootstrap
  if(bootstrap){
    
    #Model
    mod <- funcio1(formula1, formula2, formula3, formula4, formula5, data, max_iter, 
                   pres, epsilon)
    #Bootstrap paral·lelitzant
    cl<-makeCluster(detectCores())
    clusterExport(cl, "data")
    clusterExport(cl, "funcio1")
    clusterExport(cl, "dbvpoisDI")
    clusterEvalQ(cl, {
      library(speedglm)
      library(plyr)
      library(extraDistr)
    })
    a <- parSapply(cl, 1:B, FUN = function(x){try(funcio1(formula1 = formula1, 
                  formula2 = formula2, formula3 = formula3, formula4, formula5, 
                  data = data[sample(1:nrow(data), replace = T, size = nrow(data)), ], 
                  max_iter = max_iter, pres, epsilon)$coefficients)}, simplify = F)
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
      d_aux['p-value'] <- round(1 - pchisq(d_aux$Coefficients^2 / d_aux$`Desv. Std.`^2, 
                                           df = 1), 4)
      d_aux['sign'] <- "-"
      d_aux$sign[d_aux$`p-value` < 0.1] <- "."
      d_aux$sign[d_aux$`p-value` < 0.05] <- "*"
      d_aux$sign[d_aux$`p-value` < 0.01] <- "**"
      d_aux$sign[d_aux$`p-value` < 0.001] <- "***"
      Summary[[names(mod$coefficients)[l]]] <- d_aux
    }
    
    
    mod[['Summary']] <- Summary
    
    #Retornem el model
    return(mod)
  }
  
}

################################################
###FUNCIÓ PER FER PREDICCIONS EN REGRESSIÓ BPDI
################################################

pred_glmbpoisDI <- function(mod, newdata = NULL, type = "link"){
  #Cas que no hi hagi newdata, retornem prediccions per les dades amb les que s'ha 
  #ajustat el model 
  if(is.null(newdata)){
    if(type == "link") return(list(lambda1 = log(mod$lambda1), lambda2 = log(mod$lambda2), 
        lambda3 = log(mod$lambda3), p = mod$coefficients$p, theta = mod$coefficients$theta))
    else if(type == "response") return(list(lambda1 = mod$lambda1, lambda2 = mod$lambda2, 
        lambda3 = mod$lambda3, p = mod$coefficients$p, theta = mod$coefficients$theta))
  }
  
  #Prediccions per dades noves
  else{
    lambda1 <- predict(mod$mod1, newdata, type = "response")
    lambda2 <- predict(mod$mod2, newdata, type = "response")
    lambda3 <- predict(mod$mod3, newdata, type = "response")
    lambda3[lambda3 == 0] <- 1e-200
    p <- rep(mod$fitted.values$p[1], nrow(newdata))
    if(length(names(mod$coefficients)) == 4){
      if(names(mod$coefficients)[4] == "coef5")  theta <- predict(mod$mod5, newdata, 
                                                                  type = "response")
      else p <- predict(mod$mod4, newdata, type = "response")
    }
    if(length(names(mod$coefficients)) == 5){
      p <- predict(mod$mod4, newdata, type = "response")
      theta <- predict(mod$mod5, newdata, type = "response")
    }
    else theta <- rep(mod$fitted.values$theta[1], nrow(newdata))
    
    if(type == "link") return(list(lambda1 = log(lambda1), lambda2 = log(lambda2), 
                              lambda3 = log(lambda3), p = p, theta = theta))
    else if(type == "response") return(list(lambda1 = lambda1, lambda2 = lambda2, 
                              lambda3 = lambda3, p = p, theta = theta))
  }
}



