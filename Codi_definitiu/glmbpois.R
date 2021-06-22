###################################
###FUNCIÓ PER AJUSTAR REGRESSIÓ BP
###################################

glmbpois <- function(formula1, formula2, formula3, data, max_iter = 100, pres = 1e-5, 
                     bootstrap = F, B = 100, epsilon = 0.5){
  #Llibreries necessàries
  library(plyr)
  library(extraDistr)
  library(speedglm)
  library(parallel)
  
  #Funció trobar paràmetres amb l'algoritme EM
  funcio1 <- function(formula1, formula2, formula3, data, max_iter, pres = pres, 
                      epsilon = epsilon){
    
    form1_char <- as.character(formula1)
    form2_char <- as.character(formula2)
    form3_char <- as.character(formula3)
    formula3 <- as.formula(paste0("s ~ ", form3_char[2]))
  
  
    x <- data[form1_char[2]][[1]]
    y <- data[form2_char[2]][[1]]
  
    #Valors inicials 
    lambda1 <- exp(speedglm(formula1, data = data, family = poisson(), 
                            fitted = T)$linear.predictors) * epsilon
    lambda2 <- exp(speedglm(formula2, data = data, family = poisson(), 
                            fitted = T)$linear.predictors) * epsilon
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
      s <-  ((x * y) != 0) * lambda3 * dbvpois(x - 1, y - 1, lambda1, lambda2, 
                                               lambda3) / a
    
      loglike <- sum(log(a))
      difllike <- abs((loglike_prev - loglike) / loglike_prev)
      if(difllike < pres) break;
      
      data[, form1_char[2]] <- x - s
      data[, form2_char[2]] <- y - s
      data["s"] <- s
      
      #M-step
      lambda1 <- exp(speedglm(formula1, data = data, family = poisson(), 
                              fitted = T)$linear.predictors)
      lambda2 <- exp(speedglm(formula2, data = data, family = poisson(), 
                              fitted = T)$linear.predictors)
      lambda3 <- exp(speedglm(formula3, data = data, family = poisson(), 
                              fitted = T)$linear.predictors)
      lambda3[lambda3 == 0] <- 1e-50
    
      print(n_iter)
    }
    mod1 <- speedglm(formula1, data = data, family = poisson())
    mod2 <- speedglm(formula2, data = data, family = poisson())
    mod3 <- speedglm(formula3, data = data, family = poisson())
  
    #Càlcul algunes mètriques
    params = length(mod1$coefficients) + length(mod2$coefficients) + 
      length(mod3$coefficients)
    AIC <- -2 * loglike + 2 * params
    BIC <- -2 * loglike + params * log(nrow(data))

    #Llista a retornar
    mod <- list(coefficients = list(mod1$coefficients, mod2$coefficients, 
                mod3$coefficients), fitted.values = data.frame(x = (lambda1 + 
                lambda3), y = (lambda2 + lambda3)), 
                residuals = data.frame(resx = (x - (lambda1 + lambda3)), 
                resy = (y - (lambda2 + lambda3))), lambda1 = lambda1,
                lambda2 = lambda2, lambda3 = lambda3, loglikelihood = loglike, 
                AIC = AIC, BIC = BIC, parameters = params, iterations = n_iter, 
                mod1 = mod1, mod2 = mod2, mod3 = mod3)
    
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
    
    #Retornem model
    return(mod)
  }
  

  #Retornem amb les desviacions típiques del GLM
  if(!bootstrap){
    return(funcio1(formula1, formula2, formula3, data, max_iter, pres, epsilon))
  }
  
  #Calculem desviacions típiques amb bootstrap
  if(bootstrap){
    
    #Model
    mod <- funcio1(formula1, formula2, formula3, data, max_iter, pres, epsilon)
    
    #Fem bootstrap paral·lelitzant
    cl<-makeCluster(detectCores())
    clusterExport(cl, c("data", "funcio1"), envir = environment())
    clusterEvalQ(cl, {
      library(speedglm)
      library(plyr)
      library(extraDistr)
    })
    a <- parSapply(cl, 1:B, FUN = function(x){try(funcio1(formula1 = formula1, 
                  formula2 = formula2, formula3 = formula3, data = data[sample(1:nrow(data), 
                  replace = T, size = nrow(data)), ], max_iter = max_iter, pres = pres, 
                  epsilon = epsilon)$coefficients)}, simplify = F)
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
      Summary[[paste0("coef", as.character(l))]] <- d_aux
    }
    
    
    mod[['Summary']] <- Summary
    
    #Retornem model amb les desviacions típiques calculades amb bootstrap
    return(mod)
  }
  
}

##############################################
###FUNCIÓ PER FER PREDICCIONS EN REGRESSIÓ BP
##############################################

pred_glmbpois <- function(mod, newdata = NULL, type = "link"){
  #Cas que no hi hagi newdata, retornem prediccions per les dades amb les que s'ha 
  #ajustat el model  
  if(is.null(newdata)){
    if(type == "link") return(data.frame(lambda1 = log(mod$lambda1), 
                  lambda2 = log(mod$lambda2), lambda3 = log(mod$lambda3)))
    else if(type == "response") return(data.frame(lambda1 = mod$lambda1, 
                  lambda2 = mod$lambda2, lambda3 = mod$lambda3))
  }
  
  #Prediccions per dades noves
  else{
    lambda1 <- predict(mod$mod1, newdata, type = "response")
    lambda2 <- predict(mod$mod2, newdata, type = "response")
    lambda3 <- predict(mod$mod3, newdata, type = "response")
    lambda3[lambda3 == 0] <- 1e-200
    if(type == "link") return(data.frame(lambda1 = log(lambda1), lambda2 = log(lambda2), 
                                         lambda3 = log(lambda3)))
    else if(type == "response") return(data.frame(lambda1 = lambda1, lambda2 = lambda2, 
                                                  lambda3 = lambda3))
  }
}



