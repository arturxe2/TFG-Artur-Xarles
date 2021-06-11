#########################################################
####CODI PRINCIPAL DEL TREBALL
#########################################################



##########################################################
############# LECTURA BASE DE DADES
##########################################################


library(readxl)
library(dplyr)

#Lectura de la base de dades
clase <- c("text", "text", "text", rep("numeric", 65), "text", "numeric", "numeric")
dir <- "C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Resultats.xlsx"
Resultats <- read_excel(dir, col_types = clase)

#Afegim temporada a la qual pertany cada partit
Resultats$Temporada <- ""
Resultats$Temporada[1:257] <- "19/20"
Resultats$Temporada[258:nrow(Resultats)] <- "20/21"

##########################################################
############# PREPROCESSAMENT BASE DE DADES
##########################################################

#Algunes variables com a factor
Resultats$Lliga <- as.factor(Resultats$Lliga)
Resultats$Resultat <- as.factor(Resultats$Resultat)

#### Afegim cuotes B365 dels partits i transformem a probabilitat
cuotes <- read.csv("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/cuotes.csv", header = T)[, -1]
colnames(cuotes) <- c("1", "X", "2")
cuotes <- 1/cuotes
cuotes <- cuotes / apply(cuotes, 1, sum)
Resultats <- cbind(Resultats, cuotes)

#### Creem variable amb la importància del partit
imp_partit <- function(data, pos, locvis = "loc"){
  punts_jugar <- (data$Lliga == "Bundesliga") * 34 * 3 + (data$Lliga != "Bundesliga") * 38 * 3 - (data$Jornada - 1) * 3
  punts_total <- (data$Lliga == "Bundesliga") * 34 * 3 + (data$Lliga != "Bundesliga") * 38 * 3 
  difpos <- as.numeric(t(as.matrix(data[, paste0("dif", pos, locvis)])))
  difpos1 <- as.numeric(t(as.matrix(data[, paste0("dif", (pos + 1), locvis)])))
  return(1 - (sapply(1:nrow(data), FUN = function(x){
    if(difpos[x] < 0 ){
      if(abs(difpos[x]) <= punts_jugar[x]){
        return(min(abs(difpos[x]) / (punts_jugar[x] + 5) + punts_jugar[x] / punts_total[x], 1))
      }
      else{
        return(1)
      }
    }
    else{
      if(abs(difpos1[x]) <= punts_jugar[x]){
        return(min(abs(difpos1[x]) / (punts_jugar[x] + 5) + punts_jugar[x] / punts_total[x], 1))
      }
      else{
        return(1)
      }
    }
  }, simplify = T)))
}

## Posicions importants a cada lliga
laliga_pos <- c(1, 4, 6, 7, 17)
bundesliga_pos <- c(1, 4, 6, 7, 17, 18)
ligue1_pos <- c(1, 2, 3, 5, 6, 17, 18)
premier_pos <- c(1, 4, 5, 6,  17)
seriea_pos <- c(1, 4, 5, 6, 17)

#Inicialitzem les variables a 0
Resultats$imp_partit_loc <- 0
Resultats$imp_partit_vis <- 0

## Partits corresponents a LaLiga
Resultats$imp_partit_loc[Resultats$Lliga == "Laliga"] <- apply(sapply(1:length(laliga_pos), FUN = function(x){
    imp_partit(Resultats[Resultats$Lliga == "Laliga",], laliga_pos[x], "loc")
  }), 1, max)
Resultats$imp_partit_vis[Resultats$Lliga == "Laliga"] <- apply(sapply(1:length(laliga_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Laliga",], laliga_pos[x], "vis")
}), 1, max)

## Partits corresponents a la Bundesliga
Resultats$imp_partit_loc[Resultats$Lliga == "Bundesliga"] <- apply(sapply(1:length(bundesliga_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Bundesliga",], bundesliga_pos[x], "loc")
}), 1, max)
Resultats$imp_partit_vis[Resultats$Lliga == "Bundesliga"] <- apply(sapply(1:length(bundesliga_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Bundesliga",], bundesliga_pos[x], "vis")
}), 1, max)

## Partits corresponents a la Ligue1
Resultats$imp_partit_loc[Resultats$Lliga == "Ligue1"] <- apply(sapply(1:length(ligue1_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Ligue1",], ligue1_pos[x], "loc")
}), 1, max)
Resultats$imp_partit_vis[Resultats$Lliga == "Ligue1"] <- apply(sapply(1:length(ligue1_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Ligue1",], ligue1_pos[x], "vis")
}), 1, max)

## Partits corresponents a la Premier
Resultats$imp_partit_loc[Resultats$Lliga == "Premier"] <- apply(sapply(1:length(premier_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Premier",], premier_pos[x], "loc")
}), 1, max)
Resultats$imp_partit_vis[Resultats$Lliga == "Premier"] <- apply(sapply(1:length(premier_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "Premier",], premier_pos[x], "vis")
}), 1, max)

## Partits corresponents a la SerieA
Resultats$imp_partit_loc[Resultats$Lliga == "SerieA"] <- apply(sapply(1:length(seriea_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "SerieA",], seriea_pos[x], "loc")
}), 1, max)
Resultats$imp_partit_vis[Resultats$Lliga == "SerieA"] <- apply(sapply(1:length(seriea_pos), FUN = function(x){
  imp_partit(Resultats[Resultats$Lliga == "SerieA",], seriea_pos[x], "vis")
}), 1, max)

#Eliminem variables corresponents a la diferència de punts
Resultats <- Resultats[, -(26:65)]

#### Analitzem multicol·línealitat

variables <- Resultats[c("Jornada", "Camp_local", "ga901", "ga902", "save1", "save2", "gols1", "gols2", "as1", "as2", "taklesint1", "taklesint2", "ong1", "ong2", "onga1", "onga2", "clas1", "clas2", "last51", "last52", "aslocvis1", "aslocvis2", "Gol1_last", "Gol2_last", "h2h", "imp_partit_loc", "imp_partit_vis")]
cor(variables)
R <- cor(variables)
Rinv <- solve(R)
vif <- as.data.frame(matrix(diag(Rinv), nrow = 1))
colnames(vif) <- colnames(variables)
round(vif, 2)

#### Modifiquem algunes variables d'acord a l'anàlisi de multicol·linealitat

## Eliminar variable Jornada
variables <-variables[, -(1:length(colnames(variables)))[colnames(variables) == "Jornada"]]

## Combinar onga amb ga90 (porters + jugadors)
variables$onga1 <- (variables$onga1 * 10 + variables$ga901) / 11
variables <- variables[, -(1:length(colnames(variables)))[colnames(variables) == "ga901"]]
variables$onga2 <- (variables$onga2 * 10 + variables$ga902) / 11
variables <- variables[, -(1:length(colnames(variables)))[colnames(variables) == "ga902"]]

## Combinar ong i gols (gols directes + indirectes)
variables$gols1 <- (variables$gols1 + variables$ong1) / 2
variables <- variables[, -(1:length(colnames(variables)))[colnames(variables) == "ong1"]]
variables$gols2 <- (variables$gols2 + variables$ong2) / 2
variables <- variables[, -(1:length(colnames(variables)))[colnames(variables) == "ong2"]]

#Comprovem VIF després de la modificació
R <- cor(variables)
Rinv <- solve(R)
vif <- as.data.frame(matrix(diag(Rinv), nrow = 1))
colnames(vif) <- colnames(variables)
round(vif, 2)

## Guardem base de dades resultant a data
vars_modificades <- c("Camp_local", "ga901", "ga902", "save1", "save2", "gols1", "gols2", "as1", "as2", 
                      "taklesint1", "taklesint2", "ong1", "ong2", "onga1", "onga2", "clas1", "clas2", "last51", 
                      "last52", "aslocvis1", "aslocvis2", "Gol1_last", "Gol2_last", "h2h", "imp_partit_loc", "imp_partit_vis")
Resultats <- Resultats[names(Resultats)[sapply(1:length(names(Resultats)), FUN = function(x){!any(names(Resultats)[x] == vars_modificades)})]]

Resultats <- cbind(Resultats[, 1:4], variables, Resultats[, 4:ncol(Resultats)])

data <- Resultats


#### Separem dades per a l'entrenament dels models i l'avaluació
set.seed(1)
eval <- sample(1:nrow(data), size = ceiling(0.20 * nrow(data)))
train <- data[-eval, ]
test <- data[eval, ]


##########################################################
############# ANÀLISI BASE DE DADES
##########################################################


#### Carreguem funcions necessàries per a l'ajust dels models i la selecció de variables
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glmbpois.R")
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2pois.R")
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glmbpoisDI.R")
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2poisDI.R")
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/pred_result.R")
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/forward_stepwise.R")
source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/backward_stepwise.R")

#### Creem funcions per avaluar els models

## Funció Accuracy
get_accuracy <- function(prediction, observed){
  return(mean(as.character(prediction) == as.character(observed)))
}

## Funció entropia creuada
get_cross_entropy <- function(prediction, observed){
  aux <- data.frame("res_1" = rep(0, length(observed)), "res_X" = rep(0, length(observed)), "res_2" = rep(0, length(observed)))
  aux$res_1[observed == "1"] <- 1
  aux$res_X[observed == "X"] <- 1
  aux$res_2[observed == "2"] <- 1
  return(- sum(log(prediction) * aux) / length(observed))
}

## Funció MSE
get_MSE <- function(prediction, observed){
  return(sum((observed - prediction) ^ 2) / nrow(prediction))
}

## Funció validació encreuada
cross_validation <- function(model_fun, formula1, formula2, formula3 = NULL, formula4 = NULL, formula5 = NULL, data, folds){
  library(parallel)
  k <- length(table(folds))
  
  llista <- list(formula1 = formula1, formula2 = formula2)
  if(!is.null(formula3)) llista[['formula3']] <- formula3
  if(!is.null(formula4)) llista[['formula4']] <- formula4
  if(!is.null(formula5)) llista[['formula5']] <- formula5
  
  #Fem models en paral·lel
  cl<-makeCluster(detectCores())
  clusterExport(cl, c("data", "glmbpois", "glm2pois", "glmbpoisDI", "dpois2DI", "dbvpoisDI", "glm2poisDI", "model_fun", "llista",
                      "folds", "get_accuracy", "get_cross_entropy", "get_MSE", "pred_glm2poisDI", "pred_glm2pois", "pred_glmbpois",
                      "pred_glmbpoisDI", "pred_result_bpois", "pred_result_bpoisDI"), envir = environment())
  clusterEvalQ(cl, {
    library(speedglm)
    library(plyr)
    library(extraDistr)
  })
  
  metrics <- parSapply(cl, 1:k, FUN = function(x){
        train_aux <- data[folds != x, ]
        test_aux <- data[folds == x, ]
        llista[['data']] <- train_aux
        mod_aux <- do.call(model_fun, llista)
    
        #Predim segons el model que utilitzem
        if(model_fun == "glmbpois"){
         preds <- pred_glmbpois(mod_aux, newdata = test_aux, type = "response")
         pred_class <- pred_result_bpois(preds$lambda1, preds$lambda2, preds$lambda3, type = "class")
         pred_prob <- pred_result_bpois(preds$lambda1, preds$lambda2, preds$lambda3, type = "prob")
         pred_expected <- pred_result_bpois(preds$lambda1, preds$lambda2, preds$lambda3, type = "expected")
        }
        else if(model_fun == "glmbpoisDI"){
          preds <- pred_glmbpoisDI(mod_aux, newdata = test_aux, type = "response")
          pred_class <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, type = "class")
          pred_prob <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, type = "prob")
          pred_expected <- pred_result_bpoisDI(preds$lambda1, preds$lambda2, preds$lambda3, preds$p, preds$theta, type = "expected")
        }
        else if(model_fun == "glm2pois"){
          preds <- pred_glm2pois(mod_aux, newdata = test_aux, type = "response")
          pred_class <- pred_result_bpois(preds$lambda1, preds$lambda2, type = "class")
          pred_prob <- pred_result_bpois(preds$lambda1, preds$lambda2, type = "prob")
          pred_expected <- pred_result_bpois(preds$lambda1, preds$lambda2, type = "expected")
        }
        else if(model_fun == "glm2poisDI"){
          preds <- pred_glm2poisDI(mod_aux, newdata = test_aux, type = "response")
          pred_class <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p , type = "class")
          pred_prob <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, type = "prob")
          pred_expected <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, theta = preds$theta, type = "expected")
        }
        return(c(get_accuracy(pred_class, test_aux$Resultat), get_cross_entropy(pred_prob, test_aux$Resultat), 
                 get_MSE(pred_expected, data.frame(x = test_aux$Gols_local, y = test_aux$Gols_visitant))))
  })
  stopCluster(cl)
  
  return(metrics)
}


## Variables que considerem en els diferents models
vars <- names(variables)

#### Selecció de variables en GLMBPOIS (d'acord a diferents metodologies)

#Temps d'execució bastant elevat
vars_glmbpois1 <- forward_stepwise("glmbpois", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glmbpois2 <- forward_stepwise("glmbpois", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glmbpois3 <- forward_stepwise("glmbpois", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)
vars_glmbpois4 <- backward_stepwise("glmbpois", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glmbpois5 <- backward_stepwise("glmbpois", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                    vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glmbpois6 <- backward_stepwise("glmbpois", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                    vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)

## Selecció del conjunt de variables a través de validació encreuada
#Les variables dels diferents models son les que s'obtenen amb els mètodes anteriors
set.seed(1)
n <- nrow(train)
k = 20
groups <- sample(rep(1:k, ceiling(n / k))[1:n], size = n, replace = F)
metrics1 <- cross_validation(model_fun = "glmbpois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1 + aslocvis1 + clas1 + Gol1_last + aslocvis2 + Gol2_last,
                              formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1 + save1 + taklesint1 + imp_partit_vis,
                              formula3 = ~ taklesint2 + imp_partit_vis + last51 + onga2 + aslocvis2 + taklesint1 + Gol2_last + clas2 + imp_partit_loc + last52,
                              data = train, folds = groups)
metrics2 <- cross_validation(model_fun = "glmbpois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis,
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc,
                             formula3 = ~ 1,
                             data = train, folds = groups)
metrics3 <- cross_validation(model_fun = "glmbpois", formula1 = Gols_local ~ gols1 + gols2 + onga2 + imp_partit_vis + as1 + Gol1_last + as2,
                             formula2 = Gols_visitant ~ save1 + gols2 + gols1 + onga1 + onga2 + taklesint2 + imp_partit_loc,
                             formula3 = ~ Camp_local + clas2 + save2 + Gol2_last + Gol1_last + gols1 + onga1 + h2h + onga2 + clas1 + gols2 + save1 + last51 + imp_partit_loc + aslocvis1 + as2 + taklesint2 + imp_partit_vis + aslocvis2 + last52 + taklesint1,
                             data = train, folds = groups)
metrics4 <- cross_validation(model_fun = "glmbpois", formula1 = Gols_local ~ gols1 + as1 + as2 + onga2 + clas1 + aslocvis1 + Gol1_last + h2h + imp_partit_vis,
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + aslocvis1 + imp_partit_loc,
                             formula3 = ~ save2 + gols2 + as2 + taklesint1 + taklesint2 + onga1 + onga2 + clas1 + clas2 + last51 + aslocvis1 + Gol1_last + Gol2_last + h2h + imp_partit_loc + imp_partit_vis,
                             data = train, folds = groups)
metrics5 <- cross_validation(model_fun = "glmbpois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis,
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc,
                             formula3 = ~ 1,
                             data = train, folds = groups)
metrics6 <- cross_validation(model_fun = "glmbpois", formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + Gol1_last + imp_partit_vis,
                             formula2 = Gols_visitant ~ gols2 + as1 + taklesint2 + onga1 + onga2 + imp_partit_loc,
                             formula3 = ~ gols1 + gols2 + as1 + as2 + taklesint1 + taklesint2 + onga1 + onga2 + clas1 + last52 + aslocvis1 + aslocvis2 + Gol1_last + Gol2_last + h2h + imp_partit_loc + imp_partit_vis,
                             data = train, folds = groups)

metriques <- sapply(1:6, FUN = function(x){
  apply(get(paste0("metrics", x)), 1, mean)
})
rownames(metriques) <- c("Accuracy", "Cross-entropy", "MSE")
colnames(metriques) <- paste0("Variables ", 1:6)
ranks <- apply(metriques, 1, rank)
ranks[, "Accuracy"] <- 7 - ranks[, "Accuracy"]
apply(ranks, 1, mean)

## Seleccionem model amb variables 1
form1 <- "gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1 + aslocvis1 + clas1 + Gol1_last + aslocvis2 + Gol2_last"
form2 <- "gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1 + save1 + taklesint1 + imp_partit_vis"
form3 <- "taklesint2 + imp_partit_vis + last51 + onga2 + aslocvis2 + taklesint1 + Gol2_last + clas2 + imp_partit_loc + last52"

vars1_glmbpois <- unlist(strsplit(form1, split = " + ", fixed = T))
vars2_glmbpois <- unlist(strsplit(form2, split = " + ", fixed = T))
vars3_glmbpois <- unlist(strsplit(form3, split = " + ", fixed = T))

#### Selecció de variables en GLMBPOISDI (d'acord a diferents metodologies)

#Temps d'execució bastant elevat
vars_glmbpoisDI1 <- forward_stepwise("glmbpoisDI", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glmbpoisDI2 <- forward_stepwise("glmbpoisDI", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glmbpoisDI3 <- forward_stepwise("glmbpoisDI", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                   vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)
vars_glmbpoisDI4 <- backward_stepwise("glmbpoisDI", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                    vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glmbpoisDI5 <- backward_stepwise("glmbpoisDI", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                    vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glmbpoisDI6 <- backward_stepwise("glmbpoisDI", data = train, vars1 = vars, vars2 = vars, vars3 = vars,
                                    vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)

## Selecció del conjunt de variables a través de validació encreuada
#Els conjunts de variables dels diferents models son els obtinguts amb els mètodes proposats
metrics1 <- cross_validation(model_fun = "glmbpoisDI", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1 + aslocvis1 + clas1,
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1,
                             formula3 = ~ 1,
                             data = train, folds = groups)
metrics2 <- cross_validation(model_fun = "glmbpoisDI", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis,
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc,
                             formula3 = ~ 1,
                             data = train, folds = groups)
metrics3 <- cross_validation(model_fun = "glmbpoisDI", formula1 = Gols_local ~ gols1 + gols2 + onga2 + imp_partit_vis + as1 + onga1 + as2, 
                             formula2 = Gols_visitant ~ save1 + gols2 + gols1 + onga1 + onga2 + taklesint2 + imp_partit_loc, 
                             formula3 = ~ Camp_local + clas2 + save2 + Gol2_last + Gol1_last + gols1 + h2h + onga2 + last51 + aslocvis2 + taklesint2 + imp_partit_loc + clas1 + onga1,
                             data = train, folds = groups)
metrics4 <- cross_validation(model_fun = "glmbpoisDI", formula1 = Gols_local ~ gols1 + as1 + as2 + onga2 + clas1 + aslocvis1 + Gol1_last + h2h + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint1 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             formula3 = ~ save2 + gols2 + as2 + taklesint1 + taklesint2 + onga1 + onga2 + clas1 + clas2 + last51 + aslocvis1 + Gol1_last + Gol2_last + h2h + imp_partit_loc + imp_partit_vis, 
                             data = train, folds = groups)
metrics5 <- cross_validation(model_fun = "glmbpoisDI", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc, 
                             formula3 = ~ 1,
                             data = train, folds = groups)
metrics6 <- cross_validation(model_fun = "glmbpoisDI", formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + Gol1_last + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols2 + as1 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             formula3 = ~ gols1 + gols2 + as1 + as2 + taklesint1 + taklesint2 + onga1 + onga2 + clas1 + last52 + aslocvis1 + aslocvis2 + Gol1_last + Gol2_last + h2h + imp_partit_loc + imp_partit_vis, 
                             data = train, folds = groups)

metriques <- sapply(1:6, FUN = function(x){
  apply(get(paste0("metrics", x)), 1, mean)
})
rownames(metriques) <- c("Accuracy", "Cross-entropy", "MSE")
colnames(metriques) <- paste0("Variables ", 1:6)
ranks <- apply(metriques, 1, rank)
ranks[, "Accuracy"] <- 7 - ranks[, "Accuracy"]
apply(ranks, 1, mean)

## Seleccionem model amb variables 1 
form1 <- "gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1 + aslocvis1 + clas1"
form2 <- "gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1"
form3 <- "1"

vars1_glmbpoisDI <- unlist(strsplit(form1, split = " + ", fixed = T))
vars2_glmbpoisDI <- unlist(strsplit(form2, split = " + ", fixed = T))
vars3_glmbpoisDI <- unlist(strsplit(form3, split = " + ", fixed = T))


#### Selecció de variables en GLM2POIS (d'acord a diferents metodologies)

#Temps d'execució bastant elevat
vars_glm2pois1 <- forward_stepwise("glm2pois", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                     vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glm2pois2 <- forward_stepwise("glm2pois", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                     vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glm2pois3 <- forward_stepwise("glm2pois", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                     vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)
vars_glm2pois4 <- backward_stepwise("glm2pois", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                      vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glm2pois5 <- backward_stepwise("glm2pois", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                      vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glm2pois6 <- backward_stepwise("glm2pois", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                      vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)

## Selecció del conjunt de variables a través de validació encreuada
#Els conjunts de variables son els obtinguts amb els mètodes proposats
metrics1 <- cross_validation(model_fun = "glm2pois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1 + aslocvis1 + clas1, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1,
                             data = train, folds = groups)
metrics2 <- cross_validation(model_fun = "glm2pois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)
metrics3 <- cross_validation(model_fun = "glm2pois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1, 
                             data = train, folds = groups)
metrics4 <- cross_validation(model_fun = "glm2pois", formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)
metrics5 <- cross_validation(model_fun = "glm2pois", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)
metrics6 <- cross_validation(model_fun = "glm2pois", formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)

metriques <- sapply(1:6, FUN = function(x){
  apply(get(paste0("metrics", x)), 1, mean)
})
rownames(metriques) <- c("Accuracy", "Cross-entropy", "MSE")
colnames(metriques) <- paste0("Variables ", 1:6)
ranks <- apply(metriques, 1, rank)
ranks[, "Accuracy"] <- 7 - ranks[, "Accuracy"]
apply(ranks, 1, mean)

## Seleccionem model amb variables 3
form1 <- "gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1"
form2 <- "gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1"

vars1_glm2pois <- unlist(strsplit(form1, split = " + ", fixed = T))
vars2_glm2pois <- unlist(strsplit(form2, split = " + ", fixed = T))


#### Selecció de variables en GLM2POISDI (d'acord a diferents metodologies)

#Temps d'execució bastant elevat
vars_glm2poisDI1 <- forward_stepwise("glm2poisDI", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                   vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glm2poisDI2 <- forward_stepwise("glm2poisDI", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                   vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glm2poisDI3 <- forward_stepwise("glm2poisDI", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                   vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)
vars_glm2poisDI4 <- backward_stepwise("glm2poisDI", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                    vars4 = NULL, vars5 = NULL, method = "AIC", treshold_AIC = 10)
vars_glm2poisDI5 <- backward_stepwise("glm2poisDI", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                    vars4 = NULL, vars5 = NULL, method = "BIC", treshold_AIC = 10)
vars_glm2poisDI6 <- backward_stepwise("glm2poisDI", data = train, vars1 = vars, vars2 = vars, vars3 = NULL,
                                    vars4 = NULL, vars5 = NULL, method = "p-value", treshold_p = 0.10)

## Selecció del conjunt de variables a través de validació encreuada
#Els conjunts de variables son els obtinguts amb els mètodes anteriors
metrics1 <- cross_validation(model_fun = "glm2poisDI", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis + as1 + onga1 + aslocvis1 + clas1, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc + taklesint2 + gols1, 
                             data = train, folds = groups)
metrics2 <- cross_validation(model_fun = "glm2poisDI", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)
metrics3 <- cross_validation(model_fun = "glm2poisDI", formula1 = Gols_local ~ gols1 + gols2 + onga2 + imp_partit_vis + as1 + as2 + onga1, 
                             formula2 = Gols_visitant ~ save1 + gols2 + gols1 + onga1 + onga2 + imp_partit_loc + taklesint2, 
                             data = train, folds = groups)
metrics4 <- cross_validation(model_fun = "glm2poisDI", formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)
metrics5 <- cross_validation(model_fun = "glm2poisDI", formula1 = Gols_local ~ gols1 + as2 + onga2 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)
metrics6 <- cross_validation(model_fun = "glm2poisDI", formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = train, folds = groups)

metriques <- sapply(1:6, FUN = function(x){
  apply(get(paste0("metrics", x)), 1, mean)
})
rownames(metriques) <- c("Accuracy", "Cross-entropy", "MSE")
colnames(metriques) <- paste0("Variables ", 1:6)
ranks <- apply(metriques, 1, rank)
ranks[, "Accuracy"] <- 7 - ranks[, "Accuracy"]
apply(ranks, 1, mean)

## Seleccionem model amb variables 4 o 6, veiem que han sortit mateixes variables
form1 <- "gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis"
form2 <- "gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc"

vars1_glm2poisDI <- unlist(strsplit(form1, split = " + ", fixed = T))
vars2_glm2poisDI <- unlist(strsplit(form2, split = " + ", fixed = T))



#### COMPARACIÓ DIFERENTS MODELS DISTRIBUCIONALS

#Validació encreuada amb 40 iteracions
set.seed(1) 
n <- nrow(train)
k = 40
groups <- sample(rep(1:k, ceiling(n / k))[1:n], size = n, replace = F)
metrics1 <- cross_validation(model_fun = "glm2pois", formula1 = as.formula(paste0("Gols_local ~ ", paste(vars1_glm2pois, collapse = " + "))),
                             formula2 = as.formula(paste0("Gols_visitant ~ ", paste(vars2_glm2pois, collapse = " + "))),
                             data = train, folds = groups)
metrics2 <- cross_validation(model_fun = "glm2poisDI", formula1 = as.formula(paste0("Gols_local ~ ", paste(vars1_glm2poisDI, collapse = " + "))),
                             formula2 = as.formula(paste0("Gols_visitant ~ ", paste(vars2_glm2poisDI, collapse = " + "))),
                             data = train, folds = groups)
metrics3 <- cross_validation(model_fun = "glmbpois", formula1 = as.formula(paste0("Gols_local ~ ", paste(vars1_glmbpois, collapse = " + "))),
                             formula2 = as.formula(paste0("Gols_visitant ~ ", paste(vars2_glmbpois, collapse = " + "))),
                             formula3 = as.formula(paste0("~ ", paste(vars3_glmbpois, collapse = " + "))),
                             data = train, folds = groups)
metrics4 <- cross_validation(model_fun = "glmbpoisDI", formula1 = as.formula(paste0("Gols_local ~ ", paste(vars1_glmbpoisDI, collapse = " + "))),
                             formula2 = as.formula(paste0("Gols_visitant ~ ", paste(vars2_glmbpoisDI, collapse = " + "))),
                             formula3 = as.formula(paste0("~ ", paste(vars3_glmbpoisDI, collapse = " + "))),
                             data = train, folds = groups)

metriques <- sapply(1:4, FUN = function(x){
  apply(get(paste0("metrics", x)), 1, mean)
})
rownames(metriques) <- c("Accuracy", "Cross-entropy", "MSE")
colnames(metriques) <- paste0("Model ", 1:4)
ranks <- apply(metriques, 1, rank)
ranks[, "Accuracy"] <- 5 - ranks[, "Accuracy"]
apply(ranks, 1, mean)

#Seleccionem model 2 --> GLM2POISDI

#################################################
############ VALIDACIÓ MODEL
#################################################

#Analitzem amb dades de test i comparem amb B365
mod_final <- glm2poisDI(formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                   formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc,
                   data = train, max_iter = 100, bootstrap = F, epsilon = 0.5)

preds <- pred_glm2poisDI(mod_final, newdata = test, type = "response")
pred_class <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, lambda3 = preds$lambda3, p = preds$p, theta = preds$theta, type = "class")
pred_prob <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, lambda3 = preds$lambda3, p = preds$p, theta = preds$theta, type = "prob")
pred_expected <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, lambda3 = preds$lambda3, p = preds$p, theta = preds$theta, type = "expected")

get_accuracy(pred_class, test$Resultat)
get_cross_entropy(pred_prob, test$Resultat)
get_MSE(pred_expected, data.frame(x = test$Gols_local, y = test$Gols_visitant))

#Mirem mètriques amb probabilitats cases d'apostes
test_2 <- test[!is.na(test$`1`), ]
get_accuracy(colnames(cuotes)[apply(data.frame(test_2$`1`, test_2$X, test_2$`2`), 1, which.max)], test_2$Resultat)
get_cross_entropy(data.frame(test_2$`1`, test_2$X, test_2$`2`), test_2$Resultat)


### Anàlisi per jornades
data$Jornada2 <- cut(data$Jornada, breaks = c(0, 13, 26, 39))

set.seed(1)
k = 10
nJ1 <- nrow(data[data$Jornada2 == "(0,13]", ])
nJ2 <- nrow(data[data$Jornada2 == "(13,26]", ])
nJ3 <- nrow(data[data$Jornada2 == "(26,39]", ])
data$groupsJ1 <- k + 1
data$groupsJ2 <- k + 1
data$groupsJ3 <- k + 1
data$groupsJ1[data$Jornada2 == "(0,13]"] <- sample(rep(1:k, ceiling(nJ1 / k))[1:nJ1], size = nJ1, replace = F)
data$groupsJ2[data$Jornada2 == "(13,26]"] <- sample(rep(1:k, ceiling(nJ2 / k))[1:nJ2], size = nJ2, replace = F)
data$groupsJ3[data$Jornada2 == "(26,39]"] <- sample(rep(1:k, ceiling(nJ3 / k))[1:nJ3], size = nJ3, replace = F)


metrics1 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsJ1)
metrics2 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsJ2)
metrics3 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsJ3)

metriques <- sapply(1:3, FUN = function(x){
  apply(get(paste0("metrics", x))[, 1:k], 1, mean)
})
rownames(metriques) <- c("Accuracy", "CrossEntropy", "MSE")
colnames(metriques) <- paste0("Grups Jornades ", 1:3)
metriques <- as.data.frame(metriques)

dataJ1 <- data[data$Jornada2 == "(0,13]" & !is.na(data$`1`), ]
metriques[4:6, "Grups Jornades 1"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataJ1$`1`, 
                                  dataJ1$X, dataJ1$`2`), 1, which.max))], dataJ1$Resultat), get_cross_entropy(data.frame(dataJ1$`1`, 
                                  dataJ1$X, dataJ1$`2`), dataJ1$Resultat), NA)

dataJ2 <- data[data$Jornada2 == "(13,26]" & !is.na(data$`1`), ]
metriques[4:6, "Grups Jornades 2"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataJ2$`1`, 
                                  dataJ2$X, dataJ2$`2`), 1, which.max))], dataJ2$Resultat), get_cross_entropy(data.frame(dataJ2$`1`, 
                                  dataJ2$X, dataJ2$`2`), dataJ2$Resultat), NA)

dataJ3 <- data[data$Jornada2 == "(26,39]" & !is.na(data$`1`), ]
metriques[4:6, "Grups Jornades 3"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataJ3$`1`, 
                                  dataJ3$X, dataJ3$`2`), 1, which.max))], dataJ3$Resultat), get_cross_entropy(data.frame(dataJ3$`1`, 
                                  dataJ3$X, dataJ3$`2`), dataJ3$Resultat), NA)

rownames(metriques) <- c("Accuracy", "CrossEntropy", "MSE", "Accuracy2", "CrossEntropy2", "MSE2")
metriques <- as.data.frame(t(metriques))

#Mostrem gràfic de l'anàlisi per jornades
library(ggplot2)
p <- ggplot(metriques, aes(x = c(1, 2, 3))) +  xlab("Jornades") + scale_x_continuous(breaks = c(1, 2, 3), labels = c("1-13", "14-26", "27-38"))
  p <- p + geom_line(aes(y = Accuracy, colour = "Accuracy Model"), size = 1.3) + geom_point(aes(y = Accuracy, colour = "Accuracy Model"), size = 1.3)
  p <- p + geom_line(aes(y = Accuracy2, colour = "Accuracy bet365"), size = 1.3) + geom_point(aes(y = Accuracy2, colour = "Accuracy bet365"), size = 1.3)
  p <- p + geom_line(aes(y = CrossEntropy/2, colour = "Cross-Entropy Model"), linetype = "dashed", size = 1.3) + geom_point(aes(y = CrossEntropy/2, colour = "Cross-Entropy Model"), size = 1.3)
  p <- p + geom_line(aes(y = CrossEntropy2/2, colour = "Cross-Entropy bet365"), linetype = "dashed", size = 1.3) + geom_point(aes(y = CrossEntropy2/2, colour = "Cross-Entropy bet365"), size = 1.3)
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Cross-Entropy Loss"))
  p <- p + theme(legend.title = element_blank(), legend.position = "top") +
    scale_color_manual("Metrica", values = c("green","red", "darkgreen",  "darkred"))
p

### Anàlisi per lligues

set.seed(1)
k = 10
nL1 <- nrow(data[data$Lliga == "Bundesliga", ])
nL2 <- nrow(data[data$Lliga == "Laliga", ])
nL3 <- nrow(data[data$Lliga == "Ligue1", ])
nL4 <- nrow(data[data$Lliga == "Premier", ])
nL5 <- nrow(data[data$Lliga == "SerieA", ])
data$groupsL1 <- k + 1
data$groupsL2 <- k + 1
data$groupsL3 <- k + 1
data$groupsL4 <- k + 1
data$groupsL5 <- k + 1
data$groupsL1[data$Lliga == "Bundesliga"] <- sample(rep(1:k, ceiling(nL1 / k))[1:nL1], size = nL1, replace = F)
data$groupsL2[data$Lliga == "Laliga"] <- sample(rep(1:k, ceiling(nL2 / k))[1:nL2], size = nL2, replace = F)
data$groupsL3[data$Lliga == "Ligue1"] <- sample(rep(1:k, ceiling(nL3 / k))[1:nL3], size = nL3, replace = F)
data$groupsL4[data$Lliga == "Premier"] <- sample(rep(1:k, ceiling(nL4 / k))[1:nL4], size = nL4, replace = F)
data$groupsL5[data$Lliga == "SerieA"] <- sample(rep(1:k, ceiling(nL5 / k))[1:nL5], size = nL5, replace = F)


metrics1 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsL1)
metrics2 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsL2)
metrics3 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsL3)
metrics4 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsL4)
metrics5 <- cross_validation(model_fun = "glm2poisDI", 
                             formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                             formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc, 
                             data = data, folds = data$groupsL5)

metriques <- sapply(1:5, FUN = function(x){
  apply(get(paste0("metrics", x))[, 1:k], 1, mean)
})
rownames(metriques) <- c("Accuracy", "Cross-entropy", "MSE")
colnames(metriques) <- names(table(data$Lliga))
metriques <- as.data.frame(metriques)

dataL1 <- data[data$Lliga == "Bundesliga" & !is.na(data$`1`), ]
metriques[, "Bundesliga B365"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataL1$`1`, 
                              dataL1$X, dataL1$`2`), 1, which.max))], dataL1$Resultat), get_cross_entropy(data.frame(dataL1$`1`, 
                              dataL1$X, dataL1$`2`), dataL1$Resultat), NA)

dataL2 <- data[data$Lliga == "Laliga" & !is.na(data$`1`), ]
metriques[, "Laliga B365"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataL2$`1`, 
                              dataL2$X, dataL2$`2`), 1, which.max))], dataL2$Resultat), get_cross_entropy(data.frame(dataL2$`1`, 
                              dataL2$X, dataL2$`2`),dataL2$Resultat), NA)                                                                                                                                                                                       

dataL3 <- data[data$Lliga == "Ligue1" & !is.na(data$`1`), ]
metriques[, "Ligue1 B365"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataL3$`1`, 
                              dataL3$X, dataL3$`2`), 1, which.max))], dataL3$Resultat), get_cross_entropy(data.frame(dataL3$`1`, 
                              dataL3$X, dataL3$`2`), dataL3$Resultat), NA)

dataL4 <- data[data$Lliga == "Premier" & !is.na(data$`1`), ]
metriques[, "Premier B365"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataL4$`1`, 
                              dataL4$X, dataL4$`2`), 1, which.max))], dataL4$Resultat), get_cross_entropy(data.frame(dataL4$`1`, 
                              dataL4$X, dataL4$`2`), dataL4$Resultat), NA)
dataL5 <- data[data$Lliga == "SerieA" & !is.na(data$`1`), ]
metriques[, "SerieA B365"] <- c(get_accuracy(colnames(cuotes)[unlist(apply(data.frame(dataL5$`1`, 
                              dataL5$X, dataL5$`2`), 1, which.max))], dataL5$Resultat), get_cross_entropy(data.frame(dataL5$`1`, 
                              dataL5$X, dataL5$`2`), dataL5$Resultat), NA)

#Mostrem mètriques per les diferents lligues

##########################################################
############# FER PREDICCIONS AMB MODEL FINAL
##########################################################

#Exemple de predicció per a una dada nova

data$Resultat <- factor(Resultats$Resultat, levels = c("1", "X", "2"))
train <- data[1:1682, ]
test <- data[1683:nrow(data), ]

mod_final <- glm2poisDI(formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                        formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc,
                        data = train, max_iter = 100, bootstrap = F, epsilon = 0.5)

preds <- pred_glm2poisDI(mod_final, newdata = test, type = "response")
(pred_class <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, theta = preds$theta, type = "class"))
(pred_prob <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, theta = preds$theta, type = "prob"))
(pred_expected <- pred_result_bpoisDI(lambda1 = preds$lambda1, lambda2 = preds$lambda2, p = preds$p, theta = preds$theta, type = "expected"))

preds2 <- as.data.frame((matrix(unlist(preds), ncol = 4)))
colnames(preds2) <- c("lambda1", "lambda2", "p", "theta")
preds2$Local <- test$Local
preds2$Visitant <- test$Visitant

#Guardem les prediccions fetes en un fitxer csv, que després s'utilitza pel dashboard
write.csv(preds2, "C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/prediccions.csv")
