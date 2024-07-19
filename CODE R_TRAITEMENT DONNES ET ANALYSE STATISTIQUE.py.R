#---------------------------------------------------------------------#
# 1ere partie  - ICHARGEMENT DU PACKAGE D'ANALYSE
#---------------------------------------------------------------------#

library(anliou)

#---------------------------------------------------------------------------------------------#
# anliou.importation()   # importation de données
# anliou.qt.tableau()   # tableau statistique de variable quantitative
# anliou.qt.graph()     # graphiques de variable quantitative
# anliou.qt.resume()    # resume numerique de variable quantitative
# anliou.ql.tableau()   # tableau statistique de variable qualitative
# anliou.ql.graph()     # graphique de variable qualitative

#---------------------------------------------------------------------------------------------#

# anliou.2qt.liaison()  # liaison entre deux variables quantitatives
# anliou.2ql.tableau()  # tableaux statistiques de deux variables qualitatives
# anliou.2ql.graph()    # graphiques de deux variables qualitatives
# anliou.2ql.liaison()  # liaison entre deux variables qualitatives
# anliou.qtql.liaison() # liaison entre une variable quantitative et une variable qualitative
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# test_normalite_anliou()  # teste la normalité de la distribution
#---------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
traitement_doublons <- function(dataset) {
  # Identification des lignes en double
  lignes_doublons <- duplicated(dataset)
  
  # Calcul du nombre de doublons
  nombre_de_doublons <- sum(lignes_doublons)
  
  # Suppression des doublons
  dataset_sans_doublons <- unique(dataset)
  
  # Affichage du nombre de doublons
  cat("Nombre de doublons : ", nombre_de_doublons, "\n")
  
  return(dataset_sans_doublons)
}



#-------------------------------------------------------------------------------#
traitement_donnees_manquantes <- function(dataset, seuil = 0.05, k = 10) {
  # Calcul du pourcentage de données manquantes
  pourcentage <- sum(!complete.cases(dataset)) / nrow(dataset)
  
  if (pourcentage < seuil) {
    # Suppression des individus avec des données manquantes si le pourcentage est inférieur au seuil
    dataset <- na.omit(dataset)
  } else {
    # Imputation par k plus proches voisins si le pourcentage est supérieur au seuil
    library(DMwR2)
    dataset <- knnImputation(dataset, k = k, meth = "median")
  }
  
  return(dataset)
}


#-------------------------------------------------------------------------------#
afficher_boites_a_moustache <- function(dataframe) {
  # Sélectionner uniquement les colonnes numériques
  colonnes_numeriques <- sapply(dataframe, is.numeric)
  
  if (sum(colonnes_numeriques) > 0) {
    # Créer un graphique de boîtes à moustache pour chaque colonne numérique
    boxplot(dataframe[, colonnes_numeriques], main="Boîtes à moustache", col="lightblue", border="black")
  } else {
    cat("Aucune colonne numérique à afficher.\n")
  }
}

#-------------------------------------------------------------------------------#
traitement_donnees_extremes <- function(dataset) {
  # Liste des noms de colonnes quantitatives
  library(DescTools)
  colonnes_quantitatives <- sapply(dataset, is.numeric)
  
  # Winsorisation des variables quantitatives
  for (colonne in names(dataset)[colonnes_quantitatives]) {
    dataset[[colonne]] <- Winsorize(dataset[[colonne]])
  }
  
  return(dataset)
}

#-------------------------------------------------------------------------------#
extraire_variables_quantitatives <- function(data) {
  # Identifier les colonnes quantitatives (de type numérique)
  colonnes_quantitatives <- sapply(data, is.numeric)
  
  # Extraire les colonnes quantitatives
  variables_quantitatives <- data[, colonnes_quantitatives]
  
  return(variables_quantitatives)
}

#-------------------------------------------------------------------------------#
extraire_variables_qualitatives <- function(data) {
  # Identifier les colonnes qualitatives (de type factor ou caractère)
  colonnes_qualitatives <- sapply(data, function(col) is.factor(col) || is.character(col))
  
  # Extraire les colonnes qualitatives
  variables_qualitatives <- data[, colonnes_qualitatives]
  
  return(variables_qualitatives)
}


#-------------------------------------------------------------------------------#
transformer_en_tableau_disjonctif_complet <- function(data) {
  # Trouver les colonnes catégorielles
  colonnes_catégorielles <- sapply(data, function(col) is.factor(col) || is.character(col))
  
  # Sélectionner les colonnes non catégorielles (quantitatives)
  colonnes_quantitatives <- !colonnes_catégorielles
  
  # Transformer les colonnes catégorielles en tableaux disjonctifs complets
  data_dummies <- data[, colonnes_quantitatives]  # Copier les colonnes quantitatives
  
  for (colonne in names(data)[colonnes_catégorielles]) {
    categories <- levels(data[[colonne]])
    for (catégorie in categories) {
      nom_colonne_dummie <- paste(colonne, catégorie, sep = "_")
      data_dummies[nom_colonne_dummie] <- as.numeric(data[[colonne]] == catégorie)
    }
  }
  
  return(data_dummies)
}


#-------------------------------------------------------------------------------#
# 1eme partie :  - ANALYSE STATISTIQUE UNIVARIEE
#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 01.anliou.importation()   # importation de données

anliou.importation=function(x,y){ #la fonction prend trois arguments x,y
  #chemin=setwd(x)
  S="C:/Users/Anliou.Meite/Documents/DOCUMENTS MEITE/FORMATION INGENIEUR STATISTICIEN"
  chemin=setwd(paste(S,x,sep="/"))
  base<-read.table(y,header=TRUE, check.names = FALSE, stringsAsFactors =
                     TRUE, sep = ";", dec = ".")
  return(base)
}


#-------------------------------------------------------------------------------
#01.anliou.qt.tableau()   # tableau statistique de variable quantitative

anliou.qt.tableau<-function(vecteur){ #Creation d'une fonction pour paramettre un vecteur#
  T<-table(vecteur) # transformer les tables en vecteur#
  Tc<-c(T) # Cr?ation de variable T #
  tab<-data.frame(
    Effectifs=Tc,
    Eff_Cum_crois = cumsum(Tc),
    Eff_Cum_dcrois = sort(cumsum(Tc),decreasing = TRUE),
    Frequence=Tc/sum(Tc),
    Freq_Cum_crois=cumsum(Tc/sum(Tc)), Freq_Cum_dcrois=sort(cumsum(Tc/sum(Tc)),decreasing = TRUE))
  tab
  return(tab)
}



#-------------------------------------------------------------------------------
# O2.anliou.qt.graph()/anliou.qt.graph_ggplot     # graphiques de variable quantitative

anliou.qt.graph<-function(vecteur){ #Creation d'une fonction pour paramettre un vecteur#
  par(mfrow=c(2,2), mar=c(3,3,3,3))
  res1<- plot(table(vecteur),main="Diagramme en Baton") # renplacer l'argurment par vecteurs
  res2<- plot(ecdf(vecteur),main="Diagramme en Escalier") # renplacer l'argurment par vecteurs
  res3<-hist(vecteur,main="Histogramme",col="green") # renplacer l'argurment par vecteurs
  res4<-boxplot(vecteur,main="Boîte à moustache",col="green") # renplacer l'argurment par vecteurs
  par(mfrow=c(1,1), mar=c(0,0,0,0))
}
anliou.qt.graph(bd$Age)

anliou.qt.graph_ggplot<- function(vecteur){
  require(ggplot2)
  require(gridExtra)
  
  # Diagramme en bâtons
  res1 <- ggplot(data.frame(vecteur), aes(x = vecteur)) +
    geom_bar(stat = "count") +
    labs(title = "Diagramme en bâtons", x = "Valeurs", y = "Effectifs")
  
  # Polygone des fréquences cumulées
  res2 <- ggplot(data.frame(vecteur), aes(x = vecteur)) +
    stat_ecdf() +
    labs(title = "Polygone des fréquences cumulées", x = "Valeurs", y = "Fréquences
cumulées")
  
  # Histogramme
  res3 <- ggplot(data.frame(vecteur), aes(x = vecteur)) +
    geom_histogram(aes(y=..density..), bins = 30, color = "black", fill = "green") +
    geom_density(alpha = .2, fill = "red") +
    labs(title = "Histogramme", x = "Valeurs", y = "Densité")
  
  # Boîte à moustaches
  res4 <- ggplot(data.frame(vecteur), aes(x = "", y = vecteur)) +
    geom_boxplot(fill = "green") +
    labs(title = "Boîte à moustaches", x = "", y = "Valeurs")
  
  grid.arrange(res1, res2, res3, res4, ncol = 2)
}


#-------------------------------------------------------------------------------
# 03.anliou.qt.resume()    # resume numerique de variable quantitative

anliou.qt.resume<-function(vecteur){
  res1<-min(vecteur)
  res2<-max(vecteur)
  library(RVAideMemoire)
  res3<-mod(vecteur)
  res4<-median(vecteur,na.rm=TRUE)
  res5<-mean(vecteur,na.rm=TRUE)
  res6<-quantile(vecteur,na.rm=TRUE)
  res7<-cv(vecteur)
  res8<-var(vecteur,na.rm=TRUE)
  res9<-sd(vecteur,na.rm=TRUE)
  library(moments)
  res10<-skewness(vecteur)
  interpskew<- ifelse(res10<0,'distribution étalée à gauche','distribution
étalée à droite')
  res11<-kurtosis(vecteur)
  interpkurt<- ifelse(res11<3,'distribution platikurtique','distribution
leptokurtique')
  return(list(minimum=res1,
              maximum=res2,
              mode=res3,
              mediane=res4,moyenne=res5,quantile=res6,
              coefficient_variation=res7,variance=res8,ecart_type=res9,
              coefficient_assymetrie=res10,interprétation_skewness=interpskew,
              coefficent_applatissement=res11,interprétation_kurtosis=interpkurt))
}



#-------------------------------------------------------------------------------
# 04.anliou.ql.tableau()   # tableau statistique de variable qualitative

anliou.ql.tableau<-function(facteur){#Creation d'une fonction pour paramettre un facteur#
  T<-table(facteur) # transformer les tables en vecteur#
  Tc=c(T) # Création de variable Tc #
  tab<-data.frame(Effectif=Tc,Frequence= prop.table(Tc))
  tab
  return(tab)
}


#-------------------------------------------------------------------------------
# 05.anliou.ql.graph()/anliou.ql.graph_ggplot     # graphique de variable qualitative

anliou.ql.graph01 <- function(facteur) {
  # Créer un dataframe à partir du facteur
  data <- as.data.frame(table(facteur))
  colnames(data) <- c("Category", "Count")
  
  # Créer un diagramme à barres
  bar_plot <- ggplot(data, aes(x = Category, y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    ggtitle("Diagramme en barre")
  
  # Créer un diagramme circulaire
  pie_plot <- ggplot(data, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "right") +
    ggtitle("Diagramme en secteur")
  
  # Afficher les deux graphiques côte à côte
  grid.arrange(bar_plot, pie_plot, ncol = 1)
}


anliou.ql.graph_ggplot <- function(facteur) {
  # Cr?ation d'un data frame contenant les fr?quences absolues et relatives de chaque modalit?
  df <- data.frame(table(facteur))
  df$freq_relatives <- round(100 * df$Freq / sum(df$Freq), 2)
  
  # Diagramme en barre vertical avec les fr?quences absolues
  p1 <- ggplot(df, aes(x = facteur, y = Freq, fill = facteur)) +
    geom_bar(stat = "identity") +
    labs(title = "Diagramme en barre", x = "", y = "Frequences absolues") +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color = "white")
  
  # Diagramme en barre horizontal avec les fr?quences absolues
  p2 <- ggplot(df, aes(x = facteur, y = freq_relatives, fill = facteur)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Diagramme en barre", x = "", y = "Frequences Relatives") +
    geom_text(aes(label = freq_relatives ), position = position_stack(vjust = 0.5), color = "white")
  
  # Diagramme en secteur avec les fr?quences absolues
  p3 <- ggplot(df, aes(x = "", y = Freq, fill = facteur)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    labs(title = "Diagramme en secteur", x = "", y = "") +
    theme_void() +
    geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color = "white")
  
  # Diagramme en secteur avec les fr?quences relatives
  p4 <- ggplot(df, aes(x = "", y = freq_relatives, fill = facteur)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    labs(title = "Diagramme en secteur", x = "", y = "") +
    theme_void() +
    scale_fill_discrete(labels = paste0(levels(facteur), " (", df$freq_relatives, "%)")) +
    geom_text(aes(label = freq_relatives ), position = position_stack(vjust = 0.5), color = "white")
  
  # Affichage des graphiques en grille
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}


#-------------------------------------------------------------------------------
# 05.test_normalite_anliou ()     # Teste de normalité

test_normalite_anliou = function(vecteur){
  par(mfrow=c(2,1), mar=c(3,3,3,3))
  histogramme = hist(vecteur, main="Histogramme",col="green")
  library(car)
  qqplot = qqPlot(vecteur, main="QQplot")
  par(mfrow=c(1,1), mar=c(3,3,3,3))
  library(tseries)
  par(mfrow=c(1,1), mar=c(3,3,3,3))
  
  shaipro_wilk = shapiro.test(vecteur)
  interpret_shapiro = ifelse(shaipro_wilk$p.value<0.05,'La distribution ne suit pas une loi normale','La distribution suit la loi normale')
  
  jarque_bera = jarque.bera.test(vecteur)
  interpret_jarque_bera = ifelse(jarque_bera$p.value<0.05,'La distribution ne suit pas une loi normale','La distribution suit la loi normale')
  
  library(moments)
  agostino = agostino.test(vecteur)
  interpret_agostino = ifelse(agostino$p.value<0.05,'La distribution ne suit pas une loi normale','La distribution suit la loi normale')
  
  kolmogonov_smirnov = ks.test(vecteur, pnorm, mean(vecteur, na.rm=TRUE), sd(vecteur, na.rm=TRUE))
  interpret_kolmogonov_smirnov = ifelse(kolmogonov_smirnov$p.value<0.05,'La distribution ne suit pas une loi normale','La distribution suit la loi normale')
  
  return(list(test_shaipro_wilk = shaipro_wilk, Interpretation_shaipro_wilk = interpret_shapiro,
              test_jarque_bera = jarque_bera, interpret_arque_bera = interpret_jarque_bera,
              test_agostino = agostino, interpret_agostino = interpret_agostino,
              test_kolmogonov_smirnov = kolmogonov_smirnov, Interpretation_kolmogonov_smirnov = interpret_kolmogonov_smirnov))
}



#-------------------------------------------------------------------------------#
# 5eme partie  - ANALYSE STATISTIQUE BIVARIEE
#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 01.anliou.2qt.liaison()  # liaison entre deux variables quantitatives

anliou.2qt.liaison<-function(vecteur1,vecteur2){
  res1<- cor(vecteur1,vecteur2,method="pearson")
  res2<- cor(vecteur1,vecteur2,method="spearman")
  res3<- cor(vecteur1,vecteur2,method="kendall")
  res4<- cor(vecteur1,vecteur2,method="pearson")^2
  interp1<- ifelse(res4<0.10,'liaison très faible',ifelse(res4<0.40,'liaison
faible',ifelse(res4<0.60,'liaison moyenne',ifelse(res4<0.80,'liaison
forte','liaison très forte'))))
  res5<- lm(vecteur1~vecteur2)$coefficients
  res6<- cor.test(vecteur2, vecteur1)
  interp2<- ifelse(res6$p.value<0.05,'liaison significative','liaison non
significative')
  rem<-"Si la liaison n’est pas significative, Ne pas tenir compte de son intensité"
  return(list(Corrélation_Pearson=res1, Corrélation_Spearman=res2,
              Corrélation_Kendall=res3, Coefficient_Détermination=res4,
              Interprétation_Intensité_Liaison=interp1,
              Coefficents_Droite_Régression=res5,Résultat_Test_Liaison=res6,
              p.value=res6$p.value, Significacité_Liaison=interp2, Remarque=rem))
}

#-------------------------------------------------------------------------------
# 02.anliou.2ql.tableau()  # tableaux statistiques de deux variables qualitatives

anliou.2ql.tableau<-function(facteur1,facteur2){
  res1<- table(facteur1,facteur2)
  res2<- round(prop.table(table(facteur1,facteur2)),2)
  res3<- round(prop.table(table(facteur1,facteur2),1),2)
  res4<- round(prop.table(table(facteur1, facteur2),2),2)
  return(list(Tableau_Contingence=res1, Tableau_Fréquence=res2,
              Tableau_Profil_Ligne=res3, Tableau_Profil_Colonne=res4))
}


#-------------------------------------------------------------------------------
# 03.anliou.2ql.graph()/anliou.2ql.graph_ggplot    # graphiques de deux variables qualitatives

anliou.2ql.graph<-function(facteur1, facteur2){
  par(mfrow=c(3,2), mar=c(3,3,3,3))
  barplot(table(facteur1, facteur2),main="diagramme en barres empilés",legend.text=F)
  barplot(table(facteur2, facteur1),main="diagramme en barres empilés",legend.text=F)
  barplot(table(facteur1, facteur2),main="diagramme en bâtons groupés",beside=TRUE, legend.text=F)
  barplot(table(facteur2, facteur1),main="diagramme en bâtons groupés",beside=TRUE, legend.text=F)
  tabc1<-table(facteur1, facteur2)
  fi1<-apply(tabc1,1,sum)
  plignes1<-sweep(tabc1,1,fi1,"/")
  barplot(t(plignes1),horiz=TRUE, main="Profil ligne",legend.text=F)
  tabc2<-table(facteur2, facteur1)
  fi2<-apply(tabc2,1,sum)
  plignes2<-sweep(tabc2,1,fi2,"/")
  barplot(t(plignes2),horiz=TRUE, main=" Profil colonne",legend.text=F)
  par(mfrow=c(1,1), mar=c(0,0,0,0))
}
anliou.2ql.graph()

anliou.2ql.graph_ggplot <- function(facteur1, facteur2) {
  library(gridExtra)
  # Diagramme en barres empilées
  p1 <- ggplot(data.frame(table(facteur1, facteur2)), aes(x = facteur1, fill = facteur2)) +
    geom_bar() +
    labs(title = "Diagramme en barres empilées", x = "", y = "") +
    theme(legend.position = "bottom")
  
  # Diagramme en barres empilées inversé
  p2 <- ggplot(data.frame(table(facteur2, facteur1)), aes(x = facteur2, fill = facteur1)) +
    geom_bar() +
    labs(title = "Diagramme en barres empilées", x = "", y = "") +
    theme(legend.position = "bottom")
  
  # Diagramme en bâtons groupés
  p3 <- ggplot(data.frame(table(facteur1, facteur2)), aes(x = facteur1, y = Freq, fill = facteur2)) +
    geom_col(position = position_dodge()) +
    labs(title = "Diagramme en bâtons groupés", x = "", y = "") +
    theme(legend.position = "bottom")
  
  # Diagramme en bâtons groupés inversé
  p4 <- ggplot(data.frame(table(facteur2, facteur1)), aes(x = facteur2, y = Freq, fill = facteur1)) +
    geom_col(position = position_dodge()) +
    labs(title = "Diagramme en bâtons groupés", x = "", y = "") +
    theme(legend.position = "bottom")
  
  # Profil ligne
  df1 <- as.data.frame(t(apply(table(facteur1, facteur2), 1, function(x) x / sum(x))))
  df1$facteur1 <- row.names(df1)
  df1 <- reshape2::melt(df1, id.vars = "facteur1")
  
  p5 <- ggplot(df1, aes(x = value, y = facteur1, fill = variable)) +
    geom_bar(stat = "identity") +
    labs(title = "Profil ligne", x = "", y = "") +
    theme(legend.position = "bottom")
  
  # Profil colonne
  df2 <- as.data.frame(t(apply(table(facteur2, facteur1), 1, function(x) x / sum(x))))
  df2$facteur2 <- row.names(df2)
  df2 <- reshape2::melt(df2, id.vars = "facteur2")
  
  p6 <- ggplot(df2, aes(x = value, y = facteur2, fill = variable)) +
    geom_bar(stat = "identity") +
    labs(title = "Profil colonne", x = "", y = "") +
    theme(legend.position = "bottom")
  
  # Affichage des graphiques en grille
  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
  
}

#-------------------------------------------------------------------------------
# 04.anliou.2ql.liaison()  # liaison entre deux variables qualitatives



anliou.2ql.liaison<-function(vecteur1,vecteur2){
  library(questionr)
  res1<- chisq.test(table(vecteur1,vecteur2))$expected
  res2<- fisher.test(table(vecteur1,vecteur2))
  res3<- chisq.test(table(vecteur1,vecteur2))
  res4<- cramer.v(table(vecteur1,vecteur2))
  interp1<- ifelse(res2$p.value<0.05, 'liaison significative, les deux variables sont
liées','liaison non significative, les deux variables ne sont pas liées')
  interp2<- ifelse(res3$p.value<0.05, 'liaison significative, les deux variables sont
liées','liaison non significative, les deux variables ne sont pas liées')
  interp3<- ifelse(res4<0.10,'liaison très faible',ifelse(res4<0.40,'liaison
faible',ifelse(res4<0.60,'liaison moyenne',ifelse(res4<0.80,'liaison forte','liaison très
forte'))))
  rem<-"Si la liaison n’est pas significative, Ne pas tenir compte de son intensité"
  return(list(Effectif_Théorique=res1, Résultat_Test_KhiDeux=res3,
              Résultat_Test_Fisher=res2,Khi_Deux=res3$statistic,
              V_Cramer=res4,Khi2.P.value=res3$p.value,Significativité_TestKhi2=interp2,
              Fisher.P.value=res2$p.value, Significativité_TestFisher=interp1, Intensité_liaison=interp3,
              Remarque=rem))
}

#-------------------------------------------------------------------------------
# 05.anliou.qtql.liaison() # liaison entre une variable quantitative et une variable qualitative

anliou.qtql.liaison<-function(vecteur,facteur){
  library(BioStatR)
  res1<- eta2(vecteur,facteur)
  res2<- anova(lm(vecteur~facteur))
  a<-data.frame(res2)
  res3<-a$Pr..F.[1]
  interp1 <- ifelse(res1<0.10,'liaison tr?s faible',ifelse(res1<0.40,'liaison faible',ifelse(res1<0.60,'liaison moyenne',ifelse(res1<0.80,'liaison forte','liaison tres forte'))))
  interp2 <- ifelse(res3<0.05, 'liaison significative, les deux variables sont liees','liaison non significative, les deux variables ne sont pas liees')
  rem<-"Si la liaison n'est pas significative, Ne pas tenir compte de son intensite"
  return(list(Rapport_Correlation=res1, Resultat_Test_Anova=res2,Anova.P.value=res3,Significativite_TestAnova=interp2, Intensite_liaison=interp1, Remarque=rem))
}
