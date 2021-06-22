###################################
###FUNCIÓ PER AJUSTAR REGRESSIÓ 2P
###################################

glm2pois <- function(formula1, formula2, data){
  
  #Extraiem variables resposta
  form1_char <- as.character(formula1)
  form2_char <- as.character(formula2)
  x <- data[form1_char[2]][[1]]
  y <- data[form2_char[2]][[1]]
  
  #Fem regressions de Poisson
  mod1 <- glm(formula1, data, family = "poisson")
  mod2 <- glm(formula2, data, family = "poisson")
  
  #Obtenció algunes mètriques
  params <- length(mod1$coefficients) + length(mod2$coefficients)
  loglike <- sum(log(dpois(x, mod1$fitted.values) * dpois(y, mod2$fitted.values)))
  AIC <- -2 * loglike + 2 * params
  BIC <- -2 * loglike + params * log(nrow(data))

  #Creem dataframes amb coeficient, desviació típica i pvalor
  d1 <- as.data.frame(summary(mod1)$coefficients)
  names(d1)[4] <- "p-value"
  d1['sign'] <- "-"
  d1$sign[d1[,"p-value"] < 0.1] <- "."
  d1$sign[d1[,"p-value"] < 0.05] <- "*"
  d1$sign[d1[,"p-value"] < 0.01] <- "**"
  d1$sign[d1[,"p-value"] < 0.001] <- "***"
  
  d2 <- as.data.frame(summary(mod2)$coefficients)
  names(d2)[4] <- "p-value"
  d2['sign'] <- "-"
  d2$sign[d2[,"p-value"] < 0.1] <- "."
  d2$sign[d2[,"p-value"] < 0.05] <- "*"
  d2$sign[d2[,"p-value"] < 0.01] <- "**"
  d2$sign[d2[,"p-value"] < 0.001] <- "***"
  
  #Tornem model
  return(list(coefficients = list(mod1$coefficients, mod2$coefficients), 
              fitted.values = data.frame(x = mod1$fitted.values, 
                                         y = mod2$fitted.values), 
              residuals = data.frame(resx = (x - mod1$fitted.values), 
                                     resy = (y - mod2$fitted.values)), 
              loglikelihood = loglike, AIC = AIC, BIC = BIC, parameters = params,
              Summary = list(coef1 = d1, coef2 = d2), mod1 = mod1, mod2 = mod2))
}

##############################################
###FUNCIÓ PER FER PREDICCIONS EN REGRESSIÓ 2P
##############################################

pred_glm2pois <- function(mod, newdata = NULL, type = "link"){
  #Cas que no hi hagi newdata, retornem prediccions per les dades amb les que s'ha 
  #ajustat el model
  if(is.null(newdata)){
    if(type == "link") return(data.frame(lambda1 = log(mod$fitted.values[, 1]), 
                                         lambda2 = log(mod$fitted.values[, 2])))
    else if(type == "response") return(data.frame(lambda1 = mod$fitted.values[, 1], 
                                                  lambda2 = mod$fitted.values[, 2]))
  }
  
  #Prediccions per dades noves
  else{
    lambda1 <- predict(mod$mod1, newdata, type = "response")
    lambda2 <- predict(mod$mod2, newdata, type = "response")
    if(type == "link") return(data.frame(lambda1 = log(lambda1), 
                                         lambda2 = log(lambda2)))
    else if(type == "response") return(data.frame(lambda1 = lambda1, 
                                                  lambda2 = lambda2))
  }
}
