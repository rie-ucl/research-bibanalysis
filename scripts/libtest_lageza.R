# Sample R script via Lazega
# http://elazega.fr/wp-content/uploads/2021/01/Script-R-QSJ-Reseaux-sociaux-Chapitre-2.txt

# initialisation du workspace
library(tcltk)
setwd(tk_choose.dir(caption="Choisissez le repertoire de travail (l'endroit ou les outputs seront stockes)"))
detach(package:tcltk)

#========================================= ETUDE STATISTIQUES =======================================

# on charge la bibliothÃ¨que statnet
library(statnet)
# attention, statnet et igraph peuvent rentrer en conflit, la derniÃ¨re bibliothÃ¨que chargÃ©e est la prioritaire,
# si vous voulez utiliser une prÃ©cÃ©dente, il faut dÃ©charger la bibliothÃ¨que :
# detach(package: nom du package)

# on lit le fichier qui sera sÃ©lectionnÃ©
# et on importe les donnÃ©es dans la liste advice (is.list(advice) = TRUE)
# il existe une fonction similaire, read.csv(file = nom du fichier), mais le sÃ©parateur par dÃ©faut est la virgule et non le point-virgule
# si vous voulez lire un fichier dont vous voulez spÃ©cifier le sÃ©parateur, utilisez read.table() (voir la doc pour plus de details) 
fichier <- file.choose()
advice <- read.csv2(fichier, header=TRUE, row.names=1)

# extraction du nom du fichier d'oÃ¹ proviens les donnÃ©es selectionnÃ©es
noms <- strsplit(fichier, "[\\]")
fic <- noms[[1]][length(noms[[1]])]
fic <- strsplit(fic, "[.]")[[1]][1]

# sink permet d'Ã©crire et de sauvegarder les informations passÃ©es dans les print() suivants, dans un fichier texte
# si le fichier n'existe pas, il sera crÃ©Ã©
sink(paste("output_ScriptCentralitÃ©_",fic,".txt", sep =""))
options(width = 500)

cat(paste("OUTPUT SCRIPT CENTRALITE (" ,fic, ") : \n \n \n"))
cat("MATRICE D'ADJACENCE : \n \n")
print(advice)
cat("\n \n")

# rÃ©cupÃ©ration des demis degrÃ©s intÃ©rieurs (indegree) et extÃ©rieurs (outdegree)
inDegree <- degree(advice, cmode="indegree")
inDegree <- round(inDegree, 2)
# print(inDegree)
outDegree <- degree(advice, cmode="outdegree")
outDegree <- round(outDegree, 2)
# print(outDegree)

# calcul des demi degrÃ©s normalisÃ©s 
# (on divise par le nombre de sommets du graphe -1 et on multiplie par 100, cf "Que sais je ?")
inNormDegree <- (inDegree/(length(advice) - 1)) * 100
inNormDegree <- round(inNormDegree, 2)
# print(inNormDegree)
outNormDegree <- (outDegree/(length(advice) - 1)) * 100
outNormDegree <- round(outNormDegree, 2)
# print(outNormDegree)

# La centralitÃ© de proximitÃ© :
# la mÃ©thode closeness(graph g) existe, cependant elle ne fonctionne pas si le graphe n'est pas complet (ne renvoit que des 0)
# il semble lorsque l'on calcule les distances gÃ©odÃ©siques que certains sommets comme le 6 sont considÃ©rÃ©s comme isolÃ©s 
# car il ne "choisit" personne comme conseillÃ©

# la centralitÃ© d'intermÃ©diaritÃ© :
betweenness <- betweenness(advice)
# print(betweenness)

# la centralitÃ© d'intermÃ©diaritÃ© normalisÃ©e :
# selon le "Que sais je ?" il faut diviser la centralitÃ© d'intermÃ©diaritÃ© par :
# (nombre de sommet du graphe -1)(nombre de sommet du graphe -2)/2
# cependant les rÃ©sultats obtenue avec cette formule ne correspondent pas aux resultats du tableau page 46

# la centralitÃ© de Bonacich :
# il existe plusieurs mÃ©thode semblant s'apparenter Ã  cette centralitÃ© :
# infocent(graph g)
# (igraph) alpha_centrality(graph g)
# (igraph) power_centrality(graph g)
# (igraph) eigen_centrality(graph g)
# bonpow(graph g)

# on crÃ©e un dataFrame (tableau de donnÃ©es) contenant toutes les valeurs obtenues afin d'avoir un affichage propre
# les variables de gauches (in_degree, out_degre, ...) sont le nom des colonnes 
# et celles de droites, les donnÃ©es Ã  stocker
dataFrame <- data.frame( in_degree = inDegree,
                         out_degree = outDegree,
                         Normalized_in_degree = inNormDegree,
                         Normalized_out_degree = outNormDegree,
                         betweenness = betweenness)
cat("DATAFRAME RECAPITULATIF : \n \n")
print(dataFrame)

write.csv2(dataFrame, file = paste("output_dataFrame_Centralite_",fic,".csv", sep =""))

# on ferme le fichier de sauvegarde
sink()

detach(package:statnet, unload=TRUE)
#========================================= ETUDE DES CLIQUES =======================================

# chargement de la bibliothÃ¨que igraph
library(igraph)
# chargement de la bibliothÃ¨que statnet
library(statnet)
# ou, si l'on veut se concentrer sur les arcs entrant ou sortant
library(RBGL)


#===================================================================================================

# definition des fonctions nÃ©cessaires aux calculs

# remplissage de la matrice de co-appartenance
rempMat <- function(len,cli){ #len est le nombre d'acteurs et cli la liste des cliques du rÃ©seau renvoyÃ©e par max_cliques
  retMat <- matrix(0, nrow = len, ncol=len) #Initialisation d'une matrice carrÃ©e de taille len
  for(i in 1:len){ #On va remplir la matrice pour chaque sommet
    for(j in 1:length(cli)){ #Pour chacun, on parcourt la liste des cliques
      if((paste("X",i, sep="") %in% names(cli[[j]]))[1]){ #Si X1, X2, X3... est contenu dans la liste
        retMat[i,i] <- retMat[i,i] + 1#On incrÃ©mente la diagonale
        for(k in i+1:len){ #On va complÃ©ter la matrice pour tous les autres sommets appartenants Ã  la clique actuelle
          if((paste("X",k, sep="") %in% names(cli[[j]]))[1]){
            retMat[i,k] <- retMat[i,k]+1 #On remplit les cases de la matrice Ã  la ligne i
            retMat[k,i] <- retMat[k,i]+1 #On fait pareil pour la colonne car la matrice est symÃ©trique
          }
        }
      }
    }   
  }
  return(retMat) #retour de la matrice remplie
}

# n-cliques (tous les membres d'une clique sont liÃ©s Ã  tous les autres mais pas necessairement directement,
# ils ont droit Ã  des chemins de taille n) : 

# source de la fonction : Alexander Montgomery/Stackoverflow
nCliques <- function(g,n){
  g <- as.undirected(g)
  E(g)$weight <- 1 #just in case g has weights - does not modify original graph
  ncliques <- kCliques(ugraph(igraph.to.graphNEL(g))) #get cliques
  n.cand <- ncliques[[n]] #n-clique candidates to be an n-clan
  n.clan <- list() #initializes a list to store the n-clans
  n.clan.i <- 1 #initializes a list pointer
  for (n.cand.i in 1:length(n.cand)){ #loop over all of the candidates
    g.n.cand <- induced_subgraph(g,n.cand[[n.cand.i]]) #get the subgraph
    if (diameter(g.n.cand)<=n){ #check diameter of the subgraph
      n.clan[[n.clan.i]] <- n.cand[[n.cand.i]] #add n-clan to the list
      n.clan.i <- n.clan.i+1 #increment list pointer
    }
  }
  return(n.clan) #return the entire list
}

#===================================================================================================

sink(paste("output_ScriptClique_",fic,".txt", sep =""))
options(width = 500)
cat(paste("OUTPUT SCRIPT CLIQUES (" ,fic, ") : \n \n \n"))
cat("MATRICE D'ADJACENCE : \n \n")
print(advice)
cat("\n \n")

# igraph utilise marjoritairement des graphes, on convertit donc advice en graph
# Ã  l'aide de la fonction graph_from_adjacency_matrix(matrix m)
# la fonction as.matrix(object o) permet de convertir une liste (advice) en matrice
graph <- graph_from_adjacency_matrix(as.matrix(advice))

# cliques (tous les membres d'une clique sont liÃ©s directement Ã  tous les autres membres) :
#cliques <- cliques(graph, min = NULL, max = NULL) # fonctionne mais ne correspond pas Ã  UCINET

print("N-CLIQUES DU RÃ‰SEAU : ")
cat("\n")
cliques <- max_cliques(graph, min = NULL, max = NULL) # ignore la direction du graphe et renvoie les mÃªme output que UCINET
print(cliques)
cat("\n \n")


print("PLUS GRANDES CLIQUES DU RÃ‰SEAU : ")
cat("\n")
largestCliques <- largest_cliques(graph) # donne les cliques les plus grandes( =contenant le plus de sommets)
print(largestCliques)
cat("\n \n")
# nous n'avons rien trouvÃ© qui permettait de faire les k-plex de maniÃ¨re simple et efficace, nous ne les avons donc pas faits

# k-cores (il doit y avoir minimum k liens entre tous sommets de la cliques et les autres) :
print("K-CORES DU RÃ‰SEAU : ")
cat("\n")
kCores <- kcores(advice)
print(kCores)
cat("\n")

V(graph)$size <- (kCores/4)^2
#kCoresIn <- kCores(graph, "in")
#kCoresOut <- kCores(graph, "out")

sink()

appMatrice <- rempMat(length(advice), cliques) # appMatrice est la matrice de co-appartenance correspondant Ã  advice
d <- dist(appMatrice) # calcul de la distance euclidienne entre tous les sommets 
# (pour utiliser d'autres mÃ©tohdes de calculs de distance, il faut ajouter un paramÃ¨tre, plus d'informations sur: 
# https://www.rdocumentation.org/packages/stats/versions/3.5.3/topics/dist)

V(graph)$label.cex = 0.8

# https://stackoverflow.com/questions/38999656/increasing-spaces-between-vertices-for-r-igraph/39023025
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

l <- layout_with_graphopt(graph)

plot(graph, main = "variation des tailles en fonction du nombre de kcores dans lequel un sommet se trouve", layout = l, edge.arrow.size = 0.3)

write_graph(graph, paste("output_dendrogramme_Cliques_",fic,".graphml", sep =""), format="graphml")

hc <- hclust(d) # dendrogramme
pdf(paste("output_dendrogramme_Cliques_",fic,".pdf", sep =""), width=15, height=10) #adapter la width et la height en fonction de la taille des donnÃ©es
plot(graph, main = "variation des tailles en fonction du nombre de kcores dans lequel un sommet se trouve")
plot(hc, hang = -1, main="Dendrogramme d'equivalence structurale") # affichage du dendrogramme, hang = -1 permet d'aligner tous les acteurs sur une meme ligne dans le graphe
dev.off()

detach(package:igraph, unload=TRUE)
detach(package:statnet, unload=TRUE)
detach(package:RBGL, unload=TRUE)

#========================================= BLOCK MODELLING =======================================

library(statnet)
library(igraph)
library(concoR)


sink(paste("output_ScriptCONCOR_",fic,".txt", sep =""))
cat(paste("OUTPUT SCRIPT CONCOR (" ,fic, ") : \n \n \n"))
cat("MATRICE D'ADJACENCE : \n \n")
print(advice)
cat("\n \n")

# conversion en matrice
matrice <- as.matrix(advice)
# conversion en graphe
g <- graph_from_adjacency_matrix(matrice)
# conversion en rÃ©seau
net<-network(matrice, directed=TRUE, matrix.type="adjacency")


# recupÃ©ration de la matrice d'adjacence du graphe
mat <- as.matrix(get.adjacency(g))
# calcul des coeficient de correlations
m0 <- cor(mat)
m0[is.na(m0)] <- 0 #remplace tous les NA par des 0
# arrondit des coef au centiÃ¨me
round(m0, 2)

cat("COEFFICIENTS DE CORRELATION : \n \n")
print(m0)
cat("\n \n")
mat[1,44] <- 1 # on modifie la matrice car la colonne 44 n'a que des 0
sink()

print("Veuillez ecrire un nombre de blocs a creer : ")
# on lit la densitÃ© choisie par l'utilisateur
nb <- scan(nmax=1, what=double())
# crÃ©ation d'un vecteur d'appartenance necessaire Ã  la fonction blockmodel
blks <- concor_hca(list(mat), p = nb)

# crÃ©ation du graphe 
blk_mod <- blockmodel(net, blks$block)
V(g)$blocks <- blks$block


bloques <- vector("numeric", max(blks$block))
for(i in 1:max(blks$block)){
  bloques[i] <- sum(blks$block == i)
}

# block -> blks$block
# dens -> blk_mod[5]
adjMat <- function(block, dens, seuil){
  retMat <- matrix(0, nrow = max(block), ncol=max(block))
  dens <- as.matrix(dens[[1]])
  for(i in 1:length(dens[1,])){
    for(j in 1:length(dens[,1])){
      if(dens[i,j] >= seuil){
        retMat[i,j]<- 1
      }
    }
  }
  return(retMat)
}

matrice <- adjMat(blks$block, blk_mod[5], 0.302)
matrice
gBlock <- graph_from_adjacency_matrix(matrice)
V(gBlock)$size <- bloques*2

pdf(paste("output_ScriptCONCOR_matrice_graphes_",fic,".pdf", sep =""), width=20, height=20)
plot(blk_mod, main = "")
title("matrice CONCOR")
plot.igraph(g, vertex.color = V(g)$blocks, main = "graphes des sommets et de leurs appartenances aux blocks")
plot(gBlock, main = "graphe du block model")
dev.off()

detach(package:statnet, unload=TRUE)
detach(package:igraph, unload=TRUE)
detach(package:concoR, unload=TRUE)


#========================================= ETUDE DENSITE =======================================

library(igraph)

# on rÃ©cupÃ¨re les donnÃ©es enregistrÃ©es dans un tableur excel (qui sera choisi par l'intÃ©rmÃ©diaire d'une boÃ®te de dialogue)
fichier <- file.choose()
advice <- read.csv2(fichier, header=TRUE, row.names=1)  

# extraction du nom du fichier d'oÃ¹ proviens les donnÃ©es selectionnÃ©es
noms <- strsplit(fichier, "[\\]")
fic <- noms[[1]][length(noms[[1]])]
fic <- strsplit(fic, "[.]")[[1]][1]

# on convertit la liste des donnÃ©es obtenue en matrice d'incidence (qui met en relation deux classes d'objets)
data_matrix <- as.matrix(noms)

print("Veuillez ecrire un seuil pour votre densite")
# on lit la densitÃ© choisie par l'utilisateur
seuil <- scan(nmax=1, what=double())

# CrÃ©ation de la matrice d'adjacence de la taille de la table de densitÃ© sÃ©lectionnÃ©e, remplie de 0 
imgMat <- matrix(0, nrow=length(data_matrix[1,])-1, ncol=length(data_matrix[,1])-1)

# parcours de la table de densitÃ©
for(i in 1:(length(data_matrix[1,])-1)){
  for(j in 1:(length(data_matrix[,1])-1)){
    # Si la donnÃ©e en i,j est supÃ©rieure au seuil, alors on met un 1 dans la matrice d'adjacence
    if(data_matrix[i,j] >= seuil){
      imgMat[i,j]<- 1
    }
  }
}

sink(paste("output_scriptDensite_",fic,".txt", sep =""))
cat(paste("OUTPUT SCRIPT DENSITE (" ,fic, ") : \n \n \n"))
cat("MATRICE D'ADJACENCE : \n \n")
print(advice)
cat("\n \n")
cat("MATRICE IMAGE : \n \n")
print(imgMat)
cat("\n \n")
sink()

write.csv2(imgMat, file = paste("output_matriceImageDensite_",fic,".csv", sep =""))

detach(package:igraph, unload=TRUE)


#========================================= FIN =======================================