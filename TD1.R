library(ggplot2)
library(FactoMineR)
library(cluster) # Pour le clustering
library(xtable)

red_wine <- read.csv("wineQualityReds.csv",header=TRUE,sep=",",dec=".")
white_wine <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",", dec=".")


Xmax = max(red_wine$X)
white_wine$X = white_wine$X + Xmax

red_wine$color = 1
white_wine$color = 2

all_wine = rbind(red_wine, white_wine)

all_wine_bis <- all_wine[,-14]
all_wine_bis <- all_wine_bis[,-1]

#Question 1

var = var(all_wine_bis)
diag = diag(var)
barplot(diag)
boxplot(all_wine_bis)

#Question 2

boxplot(all_wine_bis)

all_wine_bis_sans_sulfure <- all_wine_bis[-7]
all_wine_bis_sans_sulfure <- all_wine_bis_sans_sulfure[-6]

boxplot(all_wine_bis_sans_sulfure)

#Question 3

plot(all_wine_bis)

#Question 4

mat_cor = round(cor(all_wine_bis),3)
upper<-mat_cor
upper[upper.tri(mat_cor)]<-""
upper<-as.data.frame(upper)
upper
mat_cor_style <- xtable(upper)
print.xtable(mat_cor_style, type="latex", file="Matrice_correlation_all_wine.tex")


#Question 5

#Choix d'une ACP normée justifiée par le fait que total.sulfure.dioide est prédominant, et que toutes les vraibles mesurées n'ont pas les mêmes unités 

#Question 6
out_acp <- PCA(all_wine[1:14],scale.unit=TRUE,ncp=5,quali.sup=1,quanti.sup=14,graph=FALSE)
summary(out_acp)

val_prop <- out_acp$eig[,"eigenvalue"]
val_prop_cum <- cumsum(val_prop)/sum(val_prop)
cp <- 1:length(val_prop)
vp <- data.frame(cp=cp,val_prop=val_prop)
vp_cum <- data.frame(cp=cp,val_prop_cum=val_prop_cum)
ggplot(data=vp,aes(x=cp,y=val_prop))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Eboulis des valeurs propres")+
  xlab("Nombre de composantes principales")+
  ylab("Valeurs propres")+
  scale_x_continuous(breaks=cp)


#Question 7
ggplot(data=vp_cum,aes(x=cp,y=val_prop_cum))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Part d'inertie expliquée en fonction du nombre de CP")+
  xlab("Nombre de composantes principales")+
  ylab("Part d'inertie expliquée")+
  scale_x_continuous(breaks=cp)

#Question 8
plot.PCA(out_acp,shadow=TRUE,cex=0.8,axes=c(1,2),choix="var",new.plot=TRUE,
         title="Cercle des corrélations")


#Question 9
plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(1,2),choix="ind",label="none",new.plot=TRUE,
         title="Projection des individus", habillage="color")


#Question 16
cah_ward <- agnes(all_wine,metric="euclidean",stand=TRUE,method="ward")
plot(as.dendrogram(cah_ward),main="Ward")

plot(as.dendrogram(cah_ward),main="Ward")
rect.hclust(cah_ward,k=2)


ei <- data.frame(k=2:dim(all_wine)[1],height=sort(cah_ward$height,decreasing=TRUE))

ggplot(data=ei,aes(x=k,y=height))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Gain d'inertie inter-classes lors du passage de (k-1) à k classes")+
  xlab("k")+
  ylab("Indice d'aggrégation")+
  scale_x_continuous(breaks=2:dim(all_wine)[1]) 

cluster_ward_3cl <- data.frame(nom=rownames(all_wine),classe=cutree(cah_ward,k=2),color=all_wine$color)
cluster_ward_3cl[order(cluster_ward_3cl$classe),]
plot(cluster_ward_3cl$nom,cluster_ward_3cl$classe,col=cluster_ward_3cl$color,
     main="Ward CAH clusters",sub="Noir: Rembrandt, Rouge: Van Gogh",
     xlab="Vins",ylab="Cluster")
################################################
cah_ward <- agnes(all_wine_nor,metric="euclidean",stand=TRUE,method="ward")
plot(as.dendrogram(cah_ward),main="Ward (norm?)")

plot(as.dendrogram(cah_ward),main="Ward (norm?)")
rect.hclust(cah_ward,k=2)


ei <- data.frame(k=2:dim(all_wine)[1],height=sort(cah_ward$height,decreasing=TRUE))

ggplot(data=ei,aes(x=k,y=height))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Gain d'inertie inter-classes lors du passage de (k-1) à k classes (norm?)")+
  xlab("k")+
  ylab("Indice d'aggr?gation")+
  scale_x_continuous(breaks=2:dim(all_wine)[1]) 

cluster_ward_3cl <- data.frame(nom=rownames(all_wine),classe=cutree(cah_ward,k=2),color=all_wine$color)
cluster_ward_3cl[order(cluster_ward_3cl$classe),]
plot(cluster_ward_3cl$nom,cluster_ward_3cl$classe,col=cluster_ward_3cl$color,
     main="Ward CAH clusters (norm?)",sub="Noir: Rembrandt, Rouge: Van Gogh",
     xlab="Peinture",ylab="Cluster")


#Question 17

all_wine_nor <- scale(all_wine[2:13],center=TRUE,scale=TRUE)
cluster_kmeans_3cl <- kmeans(all_wine_nor,centers=2,nstart=100)
print(cluster_kmeans_3cl)
plot(rownames(all_wine),cluster_kmeans_3cl$cluster,col=all_wine$color,
     main="K-means Clustering (norm?)",sub="Noir: Rouge, Rouge: Blanc",
     xlab="Peinture",ylab="Cluster")
##############################
cluster_kmeans_3cl <- kmeans(all_wine[2:13],centers=2,nstart=100)
print(cluster_kmeans_3cl)
plot(rownames(all_wine),cluster_kmeans_3cl$cluster,col=all_wine$color,
     main="K-means Clustering",sub="Noir: Rembrandt, Rouge: Van Gogh",
     xlab="Peinture",ylab="Cluster")
