###############################################################
#####Funció per a fer la simulació del tram final de temporada
###############################################################

simulacio_temporada <- function(lliga, n_sim = 1000){
  
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/glm2poisDI.R")
  source("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/pred_result.R")
  library(stringr)
  library(readxl)
  library(parallel)

  #Funció per modificar les dades i tindre-les com al model
  mod_dades <- function(data){
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
    
    ## Posicions que marquen l'importància a cada lliga
    laliga_pos <- c(1, 4, 6, 7, 17)
    bundesliga_pos <- c(1, 4, 6, 7, 17, 18)
    ligue1_pos <- c(1, 2, 3, 5, 6, 17, 18)
    premier_pos <- c(1, 4, 5, 6, 17)
    seriea_pos <- c(1, 4, 5, 6, 17)
    
    data$imp_partit_loc <- 0
    data$imp_partit_vis <- 0
    
    ## Partits corresponents a LaLiga
    if(nrow(data[data$Lliga == "Laliga", ]) > 0){
    data$imp_partit_loc[data$Lliga == "Laliga"] <- apply(matrix(sapply(1:length(laliga_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Laliga",], laliga_pos[x], "loc")
    }), nrow = nrow(data)), 1, max)
    data$imp_partit_vis[data$Lliga == "Laliga"] <- apply(matrix(sapply(1:length(laliga_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Laliga",], laliga_pos[x], "vis")
    }), nrow = nrow(data)), 1, max)
    }
    ## Partits corresponents a la Bundesliga
    if(nrow(data[data$Lliga == "Bundesliga", ]) > 0){
    data$imp_partit_loc[data$Lliga == "Bundesliga"] <- apply(matrix(sapply(1:length(bundesliga_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Bundesliga",], bundesliga_pos[x], "loc")
    }), nrow = nrow(data)), 1, max)
    data$imp_partit_vis[data$Lliga == "Bundesliga"] <- apply(matrix(sapply(1:length(bundesliga_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Bundesliga",], bundesliga_pos[x], "vis")
    }), nrow = nrow(data)), 1, max)
    }
    ## Partits corresponents a la Ligue1
    if(nrow(data[data$Lliga == "Ligue1", ]) > 0){
    data$imp_partit_loc[data$Lliga == "Ligue1"] <- apply(matrix(sapply(1:length(ligue1_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Ligue1",], ligue1_pos[x], "loc")
    }), nrow = nrow(data)), 1, max)
    data$imp_partit_vis[data$Lliga == "Ligue1"] <- apply(matrix(sapply(1:length(ligue1_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Ligue1",], ligue1_pos[x], "vis")
    }), nrow = nrow(data)), 1, max)
    }
    ## Partits corresponents a la Premier
    if(nrow(data[data$Lliga == "Premier", ]) > 0){
    data$imp_partit_loc[data$Lliga == "Premier"] <- apply(matrix(sapply(1:length(premier_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Premier",], premier_pos[x], "loc")
    }), nrow = nrow(data)), 1, max)
    data$imp_partit_vis[data$Lliga == "Premier"] <- apply(matrix(sapply(1:length(premier_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "Premier",], premier_pos[x], "vis")
    }), nrow = nrow(data)), 1, max)
    }
    ## Partits corresponents a la SerieA
    if(nrow(data[data$Lliga == "SerieA", ]) > 0){
    data$imp_partit_loc[data$Lliga == "SerieA"] <- apply(matrix(sapply(1:length(seriea_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "SerieA",], seriea_pos[x], "loc")
    }), nrow = nrow(data)), 1, max)
    data$imp_partit_vis[data$Lliga == "SerieA"] <- apply(matrix(sapply(1:length(seriea_pos), FUN = function(x){
      imp_partit(data[data$Lliga == "SerieA",], seriea_pos[x], "vis")
    }), nrow = nrow(data)), 1, max)
    }
    #Eliminem variables diferència punts
    data <- data[, -(26:65)]
    
    variables <- data[c("Jornada", "Camp_local", "ga901", "ga902", "save1", "save2", "gols1", "gols2", "as1", "as2", "taklesint1", "taklesint2", "ong1", "ong2", "onga1", "onga2", "clas1", "clas2", "last51", "last52", "aslocvis1", "aslocvis2", "Gol1_last", "Gol2_last", "h2h", "imp_partit_loc", "imp_partit_vis")]
    
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
    
    ## Guardem base de dades resultant a data i afegim les variables corresponents al resultat
    vars_modificades <- c("Jornada", "Camp_local", "ga901", "ga902", "save1", "save2", "gols1", "gols2", "as1", "as2", 
                          "taklesint1", "taklesint2", "ong1", "ong2", "onga1", "onga2", "clas1", "clas2", "last51", 
                          "last52", "aslocvis1", "aslocvis2", "Gol1_last", "Gol2_last", "h2h", "imp_partit_loc", "imp_partit_vis")
    data <- data[names(data)[sapply(1:length(names(data)), FUN = function(x){!any(names(data)[x] == vars_modificades)})]]
    
    data <- cbind(data[, 1:3], variables, data[, 4:ncol(data)])
    
    return(data)
  }

  #Llegim base de dades partits
  clase <- c("text", "text", "text", rep("numeric", 171), "text", "numeric", "numeric", "text", "numeric", "numeric")
  Resultats <- read_excel("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/Resultats_simulacio.xlsx", col_types = clase)
  Resultats$Lliga <- as.factor(Resultats$Lliga)
  Resultats$Resultat <- as.factor(Resultats$Resultat)
  Resultats$Resultat_descans <- as.factor(Resultats$Resultat_descans)

  ##### Eliminem casos que no contenen algunes variables i variables innecessàries
  Resultats <- Resultats[315:nrow(Resultats), ]
  Resultats <- Resultats[, -(66:171)]
  Resultats2 <- mod_dades(Resultats) #Modifiquem la base de dades

  #Ajustem model
  mod <- glm2poisDI(formula1 = Gols_local ~ gols1 + as1 + as2 + onga1 + onga2 + clas1 + aslocvis1 + imp_partit_vis, 
                    formula2 = Gols_visitant ~ gols1 + gols2 + taklesint2 + onga1 + onga2 + imp_partit_loc,
                    data = Resultats2, max_iter = 100, bootstrap = F, epsilon = 0.5)

  #########################################################
  ######### SELECCIÓ PARÀMETRES SIMULACIÓ
  #########################################################


  #Sel·leccionar lliga de la qual volem fer la simulació
  lliga_value <- ifelse(lliga == "Bundesliga", 1, ifelse(lliga == "Laliga", 2, ifelse(lliga == "Ligue1", 3, ifelse(lliga == "Premier", 4, 5))))

  #Llegim bases de dades necessàries
  clas <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/", as.character(lliga_value), "clas.txt"), stringsAsFactors = F)
  claslocvis <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/", as.character(lliga_value), "claslocvis.txt"), stringsAsFactors = F)
  h2h <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/", as.character(lliga_value), "h2h.txt"))
  h2h <- h2h[!is.na(h2h$Wk), ]
  h2h20 <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/h2hs/", as.character(lliga_value), "h2h20.txt"), stringsAsFactors = F)
  h2h19 <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/h2hs/", as.character(lliga_value), "h2h19.txt"), stringsAsFactors = F)
  h2h18 <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/h2hs/", as.character(lliga_value), "h2h18.txt"), stringsAsFactors = F)
  h2h17 <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/h2hs/", as.character(lliga_value), "h2h17.txt"), stringsAsFactors = F)
  h2h16 <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/h2hs/", as.character(lliga_value), "h2h16.txt"), stringsAsFactors = F)
  h2h15 <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/h2hs/", as.character(lliga_value), "h2h15.txt"), stringsAsFactors = F)
  h2h <- h2h[names(h2h15)]
  h2h20 <- h2h20[names(h2h15)]
  h2h19 <- h2h19[names(h2h15)]
  h2h18 <- h2h18[names(h2h15)]
  h2hfull <- rbind(h2h15, h2h16, h2h17, h2h18, h2h19, h2h20, h2h)
  h2hfull <- h2hfull[!is.na(h2hfull$Wk), ]
  h2hfull <- h2hfull[!is.na(h2hfull$Score1), ]

  #Canviem noms equips perquè concordin amb els de clas
  actual <- c("Augsburgo", "Bayer Leverkusen", "Bielefeld", "Colonia", "Eintracht Frankfurt", "Friburgo", "Hertha Berlin", "Mainz", 
              "Monchengladbach", "Schalke", "Wolfsburgo", "Alaves", "Atletico de Madrid", "Cadiz", "Celta de Vigo", "Real Betis", 
              "Real Valladolid", "Estrasburgo", "Girondins", "Marsella", "Nimes", "Niza", "PSG", "Saint-Etienne", "Stade Rennais", 
              "Leeds Utd", "Leicester", "Newcastle", "AC Milan", "Bolonia", "Napoles", "Verona")

  canviar <- c("Augsburg", "Leverkusen", "Arminia", "KÃ¶ln", "Eint Frankfurt", "Freiburg", "Hertha BSC", "Mainz 05", 
               "MGladbach", "Schalke 04", "Wolfsburg", "AlavÃ©s", "AtlÃ©tico Madrid", "CÃ¡diz", "Celta Vigo", "Betis", 
               "Valladolid", "Strasbourg", "Bordeaux", "Marseille", "NÃ®mes", "Nice", "Paris S-G", "Saint-Ã‰tienne", "Rennes", 
               "Leeds United", "Leicester City", "Newcastle Utd", "Milan", "Bologna", "Napoli", "Hellas Verona")

  equips <- as.character(clas$Squad)
  for(x in 1:length(actual)){
    Resultats$Local[Resultats$Local == actual[x]] <- canviar[x]
    Resultats$Visitant[Resultats$Visitant == actual[x]] <- canviar[x]
  }

  #Assumim juguen mateixa alineació darrer partit, i per tant agafem les estadístiques basades en els jugadors de l'últim partit
  alineacions <- data.frame(t(sapply(1:length(equips), FUN = function(x){
    dades_aux <- Resultats[(Resultats$Local == equips[x] | Resultats$Visitant == equips[x]), ]
    dades_aux <- dades_aux[nrow(dades_aux), ]
    if(dades_aux$Local == equips[x]){
      return(as.matrix(dades_aux[c('ga901', 'save1', 'gols1', 'as1', 'taklesint1', 'ong1', 'onga1')]))
    }
    else if(dades_aux$Visitant == equips[x]){
      return(as.matrix(dades_aux[c('ga902', 'save2', 'gols2', 'as2', 'taklesint2', 'ong2', 'onga2')]))
    }
  }, simplify = T)))


  vars_jugadors <- c("ga90", "save", "gols", "as", "taklesint", "ong", "onga")
  colnames(alineacions) <- vars_jugadors
  alineacions$Equips <- equips

  #Fem la simulació
  cl<-makeCluster(detectCores())
  clusterExport(cl, c("lliga", "alineacions", "pred_glm2poisDI", "mod", "mod_dades", "equips", "vars_jugadors", "h2hfull"), envir = environment())
  clusterEvalQ(cl, {
    library(readxl)
    library(stringr)
    library(speedglm)
    library(extraDistr)
  })
  res_sim <- parSapply(cl, 1:n_sim, FUN = function(s){

  lliga_value <- ifelse(lliga == "Bundesliga", 1, ifelse(lliga == "Laliga", 2, ifelse(lliga == "Ligue1", 3, ifelse(lliga == "Premier", 4, 5))))

  clas <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/", as.character(lliga_value), "clas.txt"), stringsAsFactors = F)
  claslocvis <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/", as.character(lliga_value), "claslocvis.txt"), stringsAsFactors = F)
  h2h <- read.csv(paste0("C:/Users/artur/OneDrive/Escriptori/4t GEA/TFG/Simulacio/", as.character(lliga_value), "h2h.txt"))
  h2h <- h2h[!is.na(h2h$Wk), ]
  partits_jugar <- h2h[is.na(h2h$Score1), ]
  partits_jugats <- h2h[!is.na(h2h$Score1), ]
  jornades_jugar <- names(table(partits_jugar$Wk))


  for(i in 1:length(jornades_jugar)){
    partits <- data.frame(Local = partits_jugar$Home[partits_jugar$Wk == jornades_jugar[i]], Visitant = partits_jugar$Away[partits_jugar$Wk == jornades_jugar[i]], Lliga = lliga, 
                          Jornada = as.numeric(jornades_jugar[i]), Camp_local = 3)
    local_stats <- data.frame(t(sapply(1:nrow(partits), FUN = function(x){
      as.matrix(alineacions[alineacions$Equips == partits$Local[x], ][, -ncol(alineacions)])
    })))
    colnames(local_stats) <- paste0(vars_jugadors, "1")
    visitant_stats <- data.frame(t(sapply(1:nrow(partits), FUN = function(x){
      as.matrix(alineacions[alineacions$Equips == partits$Visitant[x], ][, -ncol(alineacions)])
    })))
    colnames(visitant_stats) <- paste0(vars_jugadors, "2")
    
    partits <- cbind(partits, local_stats, visitant_stats)
    
    local_clas <- t(sapply(1:nrow(partits), FUN = function(x){
      aux <- clas[as.character(clas$Squad) == as.character(partits$Local[x]), ]
      aux2 <- claslocvis[as.character(clas$Squad) == as.character(partits$Local[x]), ]
      return(c((aux$W * 3 + aux$D) / (aux$MP * 3), 
             (str_count(aux$Last.5,"W") * 3 + str_count(aux$Last.5,"D")) / (str_count(aux$Last.5,"W") * 3 + 
              str_count(aux$Last.5,"D") * 3 + str_count(aux$Last.5,"L")*3), 
             (aux2$W * 3 + aux2$D) / (aux2$MP * 3)))
    }))
    colnames(local_clas) <- c("clas1", "last51", "aslocvis1")
    visitant_clas <- t(sapply(1:nrow(partits), FUN = function(x){
      aux <- clas[as.character(clas$Squad) == as.character(partits$Visitant[x]), ]
      aux2 <- claslocvis[as.character(clas$Squad) == as.character(partits$Visitant[x]), ]
      return(c((aux$W * 3 + aux$D) / (aux$MP * 3), 
               (str_count(aux$Last.5,"W") * 3 + str_count(aux$Last.5,"D")) / (str_count(aux$Last.5,"W") * 3 + 
                                                                                str_count(aux$Last.5,"D") * 3 + str_count(aux$Last.5,"L")*3), 
               (aux2$W.1 * 3 + aux2$D.1) / (aux2$MP.1 * 3)))
    }))
    colnames(visitant_clas) <- c("clas2", "last52", "aslocvis2")
    
    partits <- cbind(partits, local_clas, visitant_clas)
    
    dif_local <- t(sapply(1:nrow(partits), FUN = function(x){
      sapply(1:nrow(clas), FUN = function(x2){
        clas$Pts[which(as.character(clas$Squad) == partits$Local[x])] - clas$Pts[x2]
      })
    }))
    if(lliga == "Bundesliga") dif_local <- data.frame(dif_local[, 1:9], rep(0, nrow(partits)), rep(0, nrow(partits)), dif_local[, 10:18])
    colnames(dif_local) <- paste0("dif", as.character(1:20), "loc")
    
    dif_visitant <- t(sapply(1:nrow(partits), FUN = function(x){
      sapply(1:nrow(clas), FUN = function(x2){
        clas$Pts[which(as.character(clas$Squad) == partits$Visitant[x])] - clas$Pts[x2]
      })
    }))
    if(lliga == "Bundesliga") dif_visitant <- data.frame(dif_visitant[, 1:9], rep(0, nrow(partits)), rep(0, nrow(partits)), dif_visitant[, 10:18])
    colnames(dif_visitant) <- paste0("dif", as.character(1:20), "vis")
    
    partits <- cbind(partits, dif_local, dif_visitant)
    
    last <- t(sapply(1:nrow(partits), FUN = function(x){
      last_aux <- h2hfull[(h2hfull$Home == partits$Local[x] & h2hfull$Away == partits$Visitant[x]) | (h2hfull$Home == partits$Visitant[x] & h2hfull$Away == partits$Local[x]), ]
      last_aux <- last_aux[!is.na(last_aux$Score1), ]
      last_aux <- tail(last_aux, 1)
      g1 <- ifelse(last_aux$Home == partits$Local[x], last_aux$Score1, last_aux$Score2)
      g2 <- ifelse(last_aux$Home == partits$Local[x], last_aux$Score2, last_aux$Score1)
      return(c(g1, g2))
    }))
    
    partits$Gol1_last <- last[, 1]
    partits$Gol2_last <- last[, 2]
    
    h2h_val <- sapply(1:nrow(partits), FUN = function(x){
      h2h_aux <- h2hfull[(h2hfull$Home == partits$Local[x] & h2hfull$Away == partits$Visitant[x]) | (h2hfull$Home == partits$Visitant[x] & h2hfull$Away == partits$Local[x]), ]
      if(nrow(h2h_aux) == 0) return(0.5)
      if(nrow(h2h_aux) > 5) h2h_aux <- tail(h2h_aux, 5)
      pts1 <- 0
      ptstot <- 0
      pts1 <- pts1 + nrow(h2h_aux[h2h_aux$Home == partits$Local[x] & (h2h_aux$Score1 > h2h_aux$Score2), ]) * 3
      pts1 <- pts1 + nrow(h2h_aux[h2h_aux$Home == partits$Visitant[x] & (h2h_aux$Score1 < h2h_aux$Score2), ]) * 3
      pts1 <- pts1 + nrow(h2h_aux[h2h_aux$Score1 == h2h_aux$Score2, ]) * 1
      ptstot <- ptstot + nrow(h2h_aux[h2h_aux$Score1 == h2h_aux$Score2, ]) * 2 + nrow(h2h_aux[h2h_aux$Score1 != h2h_aux$Score2, ]) * 3
      return(pts1 / ptstot)
    })
    partits$h2h <- h2h_val
    
    partits$Resultat <- " "
    
    partits <- mod_dades(partits)
    
    preds <- pred_glm2poisDI(mod, newdata = partits, type = "response")
    
    ## Simulem resultat
    
    x1 <- as.vector(unlist(lapply(preds$lambda1, rpois, n = 1)))
    x2 <- as.vector(unlist(lapply(preds$lambda2, rpois, n = 1)))
    p <- as.vector(unlist(lapply(preds$p, rbern, n = 1)))
    theta <- as.vector(unlist(lapply(preds$theta, rpois, n = 1)))
  
    partits$Gols_local <- (x1) * (1 - p) + (theta) * p
    partits$Gols_visitant <- (x2) * (1 - p) + (theta) * p
    partits$Resultat[partits$Gols_local > partits$Gols_visitant] <- "1"
    partits$Resultat[partits$Gols_local < partits$Gols_visitant] <- "2"
    partits$Resultat[partits$Gols_local == partits$Gols_visitant] <- "X"
  
    ## Actualitzar taules
    resultat_loc <- ifelse(partits$Resultat == "1", "W", ifelse(partits$Resultat == "2", "L", "D"))
    resultat_vis <- ifelse(partits$Resultat == "1", "L", ifelse(partits$Resultat == "2", "W", "D"))
  
    for(x in 1:nrow(partits)){
      #Clas local
      clas_aux <- clas[clas$Squad == as.character(partits$Local[x]), ]
      claslocvis_aux <- claslocvis[claslocvis$Squad == as.character(partits$Local[x]), ]
      clas_aux$Last.5 <- paste0(c(unlist(strsplit(as.character(clas$Last.5)[1], " "))[2:5], resultat_loc[x]), collapse = " ")
      clas_aux$MP <- clas_aux$MP + 1
      claslocvis_aux$MP <- claslocvis_aux$MP + 1
      clas_aux$GF <- clas_aux$GF + partits$Gols_local[x]
      clas_aux$GA <- clas_aux$GA + partits$Gols_visitant[x]
      if(partits$Resultat[x] == "1"){
        clas_aux$W <- clas_aux$W + 1
        clas_aux$Pts <- clas_aux$Pts + 3
        claslocvis_aux$W <- claslocvis_aux$W + 1
      }
      else if(partits$Resultat[x] == "2"){
        clas_aux$L <- clas_aux$L + 1
        claslocvis_aux$L <- claslocvis_aux$L + 1
      }
      else{
        clas_aux$D <- clas_aux$D + 1
        clas_aux$Pts <- clas_aux$Pts + 1
        claslocvis_aux$D <- claslocvis_aux$D + 1
      }
      clas[clas$Squad == as.character(partits$Local[x]), ] = clas_aux
      claslocvis[claslocvis$Squad == as.character(partits$Local[x]), ] = claslocvis_aux
      
      #Clas visitant
      clas_aux <- clas[clas$Squad == as.character(partits$Visitant[x]), ]
      claslocvis_aux <- claslocvis[claslocvis$Squad == as.character(partits$Visitant[x]), ]
      clas_aux$Last.5 <- paste0(c(unlist(strsplit(as.character(clas$Last.5)[1], " "))[2:5], resultat_vis[x]), collapse = " ")
      clas_aux$MP <- clas_aux$MP + 1
      claslocvis_aux$MP.1 <- claslocvis_aux$MP.1 + 1
      clas_aux$GF <- clas_aux$GF + partits$Gols_visitant[x]
      clas_aux$GA <- clas_aux$GA + partits$Gols_local[x]
      if(partits$Resultat[x] == "1"){
        clas_aux$L <- clas_aux$L + 1
        claslocvis_aux$L.1 <- claslocvis_aux$L.1 + 1
      }
      else if(partits$Resultat[x] == "2"){
        clas_aux$W <- clas_aux$W + 1
        clas_aux$Pts <- clas_aux$Pts + 3
        claslocvis_aux$W.1 <- claslocvis_aux$W.1 + 1
      }
      else{
        clas_aux$D <- clas_aux$D + 1
        clas_aux$Pts <- clas_aux$Pts + 1
        claslocvis_aux$D.1 <- claslocvis_aux$D.1 + 1
      }
      clas[clas$Squad == as.character(partits$Visitant[x]), ] = clas_aux
      claslocvis[claslocvis$Squad == as.character(partits$Visitant[x]), ] = claslocvis_aux
    
      #H2h
    
      h2h_aux <- h2h[h2h$Home == partits$Local[x] & h2h$Away == partits$Visitant[x], ]
      h2h_aux$Score1 = partits$Gols_local[x]
      h2h_aux$Score2 = partits$Gols_visitant[x]
      h2h[h2h$Home == partits$Local[x] & h2h$Away == partits$Visitant[x], ] <- h2h_aux
    }
    clas$GD <- clas$GF - clas$GA
    clas <- clas[order(clas$Pts, clas$GD, decreasing = T), ]
    clas$Rk <- 1:nrow(clas)
  }
  a <- matrix(1:length(equips), nrow = 1)
  colnames(a) <- clas$Squad
  print(s)
  return(a[, equips])
  })
  stopCluster(cl)

  prob_pos <- t(sapply(1:nrow(res_sim), FUN = function(x){
    aux <- factor(res_sim[x, ], levels = 1:nrow(res_sim))
    table(aux)
  })) / n_sim * 100

  rownames(prob_pos) <- equips
  return(prob_pos)
}

###Fem les simulacions per les diferents lligues
Laliga_sim <- simulacio_temporada(lliga = "Laliga", n_sim = 1000)
Bundesliga_sim <- simulacio_temporada(lliga = "Bundesliga", n_sim = 1000)
SerieA_sim <- simulacio_temporada(lliga = "SerieA", n_sim = 1000)
Ligue1_sim <- simulacio_temporada(lliga = "Ligue1", n_sim = 1000)
Premier_sim <- simulacio_temporada(lliga = "Premier", n_sim = 1000)
