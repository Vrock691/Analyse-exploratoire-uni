#Plusieurs packages pour faire de l'ADD sous R
#Choix d'utiliser deux packages : FactoMineR (pour l'analyse) et factoextra (pour la visualisation, des donn?es, bas?e sur ggplot2)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")


#
# Importation des données et sélection des éléments -----------------------
#



#Désignation des éléments (individus et variables) actifs pour l'ACP



#
# Analyse en Composantes Principales (ACP) --------------------------------
#
PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)
#Avec X=jeu de données de type data frame ; scale.unit=valeur logique si TRUE alors les données sont standardisées ; ncp: nbre de dimensions affichées ; graph: valeur logique si TRUE un graphique est affiché


#Visualisation des valeurs propres
eig.val <- get_eigenvalue(res.pca) 
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) 

#
#Résultats pour les variables ------------------------------------------
#
var <- get_pca_var(res.pca) #Création d'une variable "var" avec tous les résultats concernants les variables

fviz_cos2(res.pca, choice = "var")# graphique du Cos2 des variables sur le 1 premier axe


install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)#Graphique du cos2 des variables sur toutes les dimensions avec le package corrplot

fviz_contrib(res.pca, choice = "var")# Contributions des variables sur l'axe 1 avec la fonction fviz_contrib() du package factoextra
corrplot(var$contrib, is.corr=FALSE) #Pour chaque axe, graphique des contributions des variables avec le package corrplot


fviz_pca_var(res.pca)#graphique des variables selon leurs coordonnées



#Description des dimensions et mise en évidence des variables qui contribuent le plus aux composantes principales
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)#fonction dimdesc() [dans FactoMineR]

#
#Résultats pour les individus ---------------------------------
#
ind <- get_pca_ind(res.pca) #Cr?ation d'une variable "ind" avec tous les résultats concernants les variables

fviz_pca_ind(res.pca, col.ind = "red")#graphique des individus selon leurs coordonnées

fviz_cos2(res.pca, choice = "ind")# graphique du Cos2 des individus sur le 1 premier axe

fviz_contrib(res.pca, choice = "ind", axes = 1)# Contributions des individus sur l'axe 1 avec la fonction fviz_contrib() du package factoextra. 


#
# Elements supplémentaires ou illustratifs --------------------------------
#
res.pca <- PCA(d, ind.sup = ??:??, quanti.sup = ??:??, quali.sup = ??, graph=FALSE) 

res.pca$quanti.sup #résultats pour les quantis sup avec leurs coordonnées, corrélations et cos?
fviz_pca_var(res.pca)#Visualiser toutes les variables (actives et suplémentaires)
fviz_pca_var(res.pca, invisible = "var")# Cacher les variables actives sur le graphique, ne montrer que des variables suppl?mentaires
fviz_pca_var(res.pca, invisible = "quanti.sup")# Cacher les variables supplémentaires

res.pca$ind.sup #Résultats pour les individus supplémentaires
fviz_pca_ind(res.pca, col.ind.sup = "blue", repel = TRUE)#Visualiser tous les individus (actifs et supl?mentaires)

res.pca$quali #Résultats pour les vars qualis supplémentaires

#
# Exportation des résultats -----------------------------------------------
#
#Exporter les graphiques en PDF / PNG : le package factoextra produit des graphiques de type ggplot2
scree.plot <- fviz_eig (res.pca) #Création des graphiques que l'on veut enregistrer. Celui des valeurs propres
ind.plot <- fviz_pca_ind (res.pca)#Celui des individus
var.plot <- fviz_pca_var (res.pca)#Celui des variables

setwd("?????????")#Pour spécifier le dossier dans lequel le fichier des graphiques va être cr??.
pdf("PCA.pdf") # Exportation des graphiques ggplot2 dans un seul fichier pdf.
print (scree.plot)
print (ind.plot)
print (var.plot)
dev.off () 

# Pour enregistrer séparemment  chaque graphique dans un fichier png
setwd("????????????")
png ("pca-scree-plot.png")
print(scree.plot)
dev.off ()
png ("pca-variables.png")
print(var.plot)
dev.off ()
png ("pca-individuals.png")
print(ind.plot)
dev.off ()

#Une alternative, pour l'exportation de ggplots, est d'utiliser la fonction ggexport() de l'extension ggpubr
library (ggpubr)
ggexport (plotlist = list(scree.plot, ind.plot, var.plot), #Exportez plusieurs graphiques dans un seul fichier pdf (un graphique par page)
          filename = "PCA.pdf")
ggexport (plotlist = list(scree.plot, ind.plot, var.plot), #Exporter plusieurs graphiques dans un seul fichier mais en mettant plusieurs graphiques par page
            nrow = 2, ncol = 2, filename = "PCA.pdf")

ggexport (plotlist = list(scree.plot, ind.plot, var.plot), # Exporter plusieurs graphiques vers plusieurs fichiers png
          filename = "PCA.png")

#Exporter les résultats de l'ACP (coordonnées, contributions des indivs/variables) vers des fichiers txt/csv
write.infile(res.pca, "pca.txt", sep = "\t") # Exporter vers un fichier TXT
write.infile(res.pca, "pca.csv", sep = ";")  # Exporter vers un fichier CSV