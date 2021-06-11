####################################################################
##FUNCIÓ PER FORWARD STEPWISE
####################################################################

forward_stepwise <- function(model_fun, data, vars1, vars2, vars3 = NULL, vars4 = NULL, 
                    vars5 = NULL, method = "AIC", treshold_p = 0.15, treshold_AIC = 10){
  
  #Llibreries que necessitem i funcions d'ajustar els diferents models
  library(parallel)
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glmbpois.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2pois.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glmbpoisDI.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2poisDI.R")
  
  p <- length(vars1) + length(vars2) + length(vars3) + length(vars4) + length(vars5)
  covars1 <- "1"
  covars2 <- "1"
  covars3 <- "1"
  covars4 <- "1"
  covars5 <- "1"

  n_vars = 0
  
  #Funció per agafar AIC, BIC o p-value
  get_metric <- function(model_fun, llista, method, n_formula){
    mod <- do.call(model_fun, llista)
    if(method != "p-value") return(mod[[method]])
    else{return(tail(mod$Summary[[n_formula]][, "p-value"], 1))}
  }
  
  #Model nul
  llista <- list(formula1 = as.formula(paste0("Gols_local ~ ", covars1)), 
                 formula2 = as.formula(paste0("Gols_visitant ~ ", covars2)),
                 data = as.data.frame(data))
  if(!is.null(vars3)) llista[['formula3']] <- as.formula(paste0("~ ", covars3))
  if(!is.null(vars4)) llista[['formula4']] <- as.formula(paste0("~ ", covars4))
  if(!is.null(vars5)) llista[['formula5']] <- as.formula(paste0("~ ", covars5))
  metric <- try(get_metric(model_fun = model_fun, llista = llista, method = method, 
                           n_formula = 1))
  min_metric <- metric
  
  tornar <- data.frame(n_vars = n_vars, formula1 = "1", formula2 = "1", formula3 = "1", 
                       formula4 = "1", formula5 = "1", metric = metric)
  
  #Anar afegint variables
  while(p > 0){
    n_vars <- n_vars + 1
    
    assign("model_fun", model_fun, envir = .GlobalEnv)
    
    cl<-makeCluster(detectCores())
    clusterExport(cl, c("data", "glmbpois", "glm2pois", "glmbpoisDI", "dpois2DI", 
              "dbvpoisDI", "glm2poisDI", "model_fun", "vars1", "vars2", "vars3", 
              "vars4", "vars5", "covars1", "covars2", "covars3", "covars4", "covars5"), 
              envir = environment())
    clusterEvalQ(cl, {
      library(speedglm)
      library(plyr)
      library(extraDistr)
    })
    estads <- as.vector(parSapply(cl, 1:p, FUN = function(x){
            if(x <= length(vars1)){
              covars1 <- paste0(covars1, " + ", vars1[x])
              n_formula = 1
            }
            else if(x <= (length(vars1) + length(vars2))){
              covars2 <- paste0(covars2, " + ", vars2[x - length(vars1)])
              n_formula = 2
            }
            else if(x <= (length(vars1) + length(vars2) + length(vars3))){
              covars3 <- paste0(covars3, " + ", vars3[x - length(vars1) - length(vars2)])
              n_formula = 3
            }
            else if(x <= (length(vars1) + length(vars2) + length(vars3) + length(vars4))){
              covars4 <- paste0(covars4, " + ", vars4[x - length(vars1) - length(vars2) - 
                                                        length(vars3)])
              n_formula = 4
            }
            else{
              covars5 <- paste0(covars5, " + ", vars5[x - length(vars1) - length(vars2) - 
                                                        length(vars3) - length(vars4)])
              n_formula = 5
            }
            llista <- list(formula1 = as.formula(paste0("Gols_local ~ ", covars1)), 
                           formula2 = as.formula(paste0("Gols_visitant ~ ", covars2)),
                           data = data)
            if(!is.null(vars3)) llista[['formula3']] <- as.formula(paste0("~ ", covars3))
            if(!is.null(vars4)) llista[['formula4']] <- as.formula(paste0("~ ", covars4))
            if(!is.null(vars5)) llista[['formula5']] <- as.formula(paste0("~ ", covars5))
            metric <- try(get_metric(model_fun = model_fun, llista = llista, method = method, 
                                     n_formula = n_formula))
            if(typeof(metric) == "character"){metric = Inf}
            return(metric)
            
    }))
    stopCluster(cl)
    
    #Comparem que no augmenti més i en aquest cas parem
    if((min(estads) - min_metric) > treshold_AIC && method != "p-value") break;
    if((min(estads) > treshold_p) && method == "p-value") break;
    min_metric <- min(c(min_metric, min(estads)))
    
    #Afegim millor variable
    afegir <- which.min(estads)
    if(afegir <= length(vars1)){
      covars1 <- paste0(covars1, " + ", vars1[afegir])
      vars1 <- vars1[-afegir]
    }
    else if(afegir <= (length(vars1) + length(vars2))){
      covars2 <- paste0(covars2, " + ", vars2[afegir - length(vars1)])
      vars2 <- vars2[- (afegir - length(vars1))]
    }
    else if(afegir <= (length(vars1) + length(vars2) + length(vars3))){
      covars3 <- paste0(covars3, " + ", vars3[afegir - length(vars1) - length(vars2)])
      vars3 <- vars3[- (afegir - length(vars1) - length(vars2))]
    }
    else if(afegir <= (length(vars1) + length(vars2) + length(vars3) + length(vars4))){
      covars4 <- paste0(covars4, " + ", vars4[afegir - length(vars1) - length(vars2) - 
                                                length(vars3)])
      vars4 <- vars4[- (afegir - length(vars1) - length(vars2) - length(vars3))]
    }
    else{
      covars5 <- paste0(covars5, " + ", vars5[afegir - length(vars1) - length(vars2) - 
                                                length(vars3) - length(vars4)])
      vars5 <- vars5[- (afegir - length(vars1) - length(vars2) - length(vars3) - length(vars4))]
    }
    
    #Afegim al data.frame la variable que hem afegit
    tornar <- rbind(tornar, data.frame(n_vars = n_vars, formula1 = covars1, formula2 = covars2, 
                      formula3 = covars3, formula4 = covars4, formula5 = covars5, 
                      metric = min(estads)))
    p <- p - 1
    print(p)
  }
  
  #Tornem el millor conjunt de variables segons el mètode utilitzat
  if(method != "p-value") return(tornar[which.min(tornar$metric), ])
  else if(method == "p-value") return(tornar[nrow(tornar), ])
  
}

