####################################################################
##FUNCIÓ PER BACKWARD STEPWISE
####################################################################

backward_stepwise <- function(model_fun, data, vars1, vars2, vars3 = NULL, vars4 = NULL, 
                      vars5 = NULL, method = "AIC", treshold_p = 0.15, treshold_AIC = 10){
  
  #Llibreries que necessitem i funcions d'ajustar els diferents models
  library(parallel)
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glmbpois.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2pois.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glmbpoisDI.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2poisDI.R")
  
  p <- length(vars1) + length(vars2) + length(vars3) + length(vars4) + length(vars5)
  covars1 <- paste0(c("1", vars1), collapse = " + ")
  covars2 <- paste0(c("1", vars2), collapse = " + ")
  covars3 <- paste0(c("1", vars3), collapse = " + ")
  covars4 <- paste0(c("1", vars4), collapse = " + ")
  covars5 <- paste0(c("1", vars5), collapse = " + ")
  
  n_vars = p
  n_forms = (!is.null(vars1)) + (!is.null(vars2)) + (!is.null(vars3)) + (!is.null(vars4)) + 
    (!is.null(vars5))
  
  #Funció per agafar AIC, BIC o p-value
  get_metric <- function(model_fun, llista, method, n_formules){
    mod <- do.call(model_fun, llista)
    if(method != "p-value") return(mod[[method]])
    else{
      pvals <- unlist(sapply(1:n_formules, FUN = function(x){
        return(as.vector(mod$Summary[[x]][-1, "p-value"]))
      }, simplify = T))
      if(max(pvals) > treshold_p) return(which.max(pvals))
      else return(-1)
      }
  }
  
  #Model complet
  llista <- list(formula1 = as.formula(paste0("Gols_local ~ ", covars1)), 
                 formula2 = as.formula(paste0("Gols_visitant ~ ", covars2)))
  if(!is.null(vars3)) llista[['formula3']] <- as.formula(paste0("~ ", covars3))
  if(!is.null(vars4)) llista[['formula4']] <- as.formula(paste0("~ ", covars4))
  if(!is.null(vars5)) llista[['formula5']] <- as.formula(paste0("~ ", covars5))
  llista[['data']] <- as.data.frame(data)
  metric <- try(get_metric(model_fun = model_fun, llista = llista, method = method, 
                           n_formules = n_forms))
  min_max_metric <- metric
  
  tornar <- data.frame(n_vars = n_vars, formula1 = covars1, formula2 = covars2, 
          formula3 = covars3, formula4 = covars4, formula5 = covars5, metric = metric)
  
  #Iteració per anar eliminant variables
  while(p > 0){
    n_vars <- n_vars - 1
    
    if(method != "p-value"){
      
    assign("model_fun", model_fun, envir = .GlobalEnv)
    
    cl<-makeCluster(detectCores())
    clusterExport(cl, c("data", "glmbpois", "glm2pois", "glmbpoisDI", "dpois2DI", 
                    "dbvpoisDI", "glm2poisDI", "model_fun", "vars1", "vars2", "vars3", 
                    "vars4", "vars5", "covars1", "covars2", "covars3", "covars4", 
                    "covars5"), envir = environment())
    clusterEvalQ(cl, {
      library(speedglm)
      library(plyr)
      library(extraDistr)
    })
    estads <- as.vector(parSapply(cl, 1:p, FUN = function(x){
      if(x <= length(vars1)){
        covars1 <- paste0(c("1", vars1[-x]), collapse = " + ")
        n_forms = 1
      }
      else if(x <= (length(vars1) + length(vars2))){
        covars2 <- paste0(c("1", vars2[- (x - length(vars1))]), collapse = " + ")
        n_forms = 2
      }
      else if(x <= (length(vars1) + length(vars2) + length(vars3))){
        covars3 <- paste0(c("1", vars3[- (x - length(vars1) - length(vars2))]), 
                          collapse = " + ")
        n_forms = 3
      }
      else if(x <= (length(vars1) + length(vars2) + length(vars3) + length(vars4))){
        covars4 <- paste0(c("1", vars4[- (x - length(vars1) - length(vars2) - 
                                            length(vars3))]), collapse = " + ")
        n_forms = 4
      }
      else{
        covars5 <- paste0(c("1", vars5[- (x - length(vars1) - length(vars2) - length(vars3) - 
                                            length(vars4))]), collapse = " + ")
        n_forms = 5
      }
      llista <- list(formula1 = as.formula(paste0("Gols_local ~ ", covars1)), 
                     formula2 = as.formula(paste0("Gols_visitant ~ ", covars2)),
                     data = data)
      if(!is.null(vars3)) llista[['formula3']] <- as.formula(paste0("~ ", covars3))
      if(!is.null(vars4)) llista[['formula4']] <- as.formula(paste0("~ ", covars4))
      if(!is.null(vars5)) llista[['formula5']] <- as.formula(paste0("~ ", covars5))
      metric <- try(get_metric(model_fun = model_fun, llista = llista, method = method, 
                               n_formules = n_forms))
      if(typeof(metric) == "character"){metric = Inf}
      return(metric)
      
    }))
    stopCluster(cl)

    #Comparem que no augmenti més i en aquest cas parem
    if((min(estads) - min_max_metric) > treshold_AIC) break;
    metric <- min(estads)
    min_max_metric <- min(c(min_max_metric, min(estads)))
    treure <- which.min(estads)
    }
    
    else{
      llista <- list(formula1 = as.formula(paste0("Gols_local ~ ", covars1)), 
                        formula2 = as.formula(paste0("Gols_visitant ~ ", covars2)),
                        data = data)
      if(!is.null(vars3)) llista[['formula3']] <- as.formula(paste0("~ ", covars3))
      if(!is.null(vars4)) llista[['formula4']] <- as.formula(paste0("~ ", covars4))
      if(!is.null(vars5)) llista[['formula5']] <- as.formula(paste0("~ ", covars5))
      metric <- try(get_metric(model_fun = model_fun, llista = llista, method = method, 
                               n_formules = n_forms))
      if(metric == -1) break;
      treure <- metric
    }
    
    #Eliminem variable
    if(treure <= length(vars1)){
      vars1 <- vars1[-treure]
      covars1 <- paste0(c("1", vars1), collapse = " + ")
    }
    if(treure <= (length(vars1) + length(vars2)) && treure > length(vars1)){
      vars2 <- vars2[- (treure - length(vars1))]
      covars2 <- paste0(c("1", vars2), collapse = " + ")
    }
    if(treure <= (length(vars1) + length(vars2) + length(vars3)) && treure > (length(vars1) +
                                                                              length(vars2))){
      vars3 <- vars3[- (treure - length(vars1) - length(vars2))]
      covars3 <- paste0(c("1", vars3), collapse = " + ")
    }
    if(treure <= (length(vars1) + length(vars2) + length(vars3) + length(vars4)) && treure > 
       (length(vars1) + length(vars2) + length(vars3))){
      vars4 <- vars4[- (treure - length(vars1) - length(vars2) - length(vars3))]
      covars4 <- paste0(c("1", vars4), collapse = " + ")
    }
    if(treure <= (length(vars1) + length(vars2) + length(vars3) + length(vars4)) && treure > 
       (length(vars1) + length(vars2) + length(vars3) + length(vars4))){
      vars5 <- vars5[- (treure - length(vars1) - length(vars2) - length(vars3) - length(vars4))]
      covars5 <- paste0(c("1", vars5), collapse = " + ")
    }
    
    #Afegim conjunt de variables a un data.frame
    tornar <- rbind(tornar, data.frame(n_vars = n_vars, formula1 = paste0(c("1", vars1), 
                                       collapse = " + "), 
                                       formula2 = paste0(c("1", vars2), collapse = " + "), 
                                       formula3 = paste0(c("1", vars3), collapse = " + "), 
                                       formula4 = paste0(c("1", vars4), collapse = " + "),
                                       formula5 = paste0(c("1", vars5), collapse = " + "), 
                                       metric = metric))
    p <- p - 1
    print(p)
  }
  
  #Retornem el millor conjunt de variables
  if(method != "p-value") return(tornar[which.min(tornar$metric), ])
  else if(method == "p-value") return(tornar[nrow(tornar), ])
  
}
