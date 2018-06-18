library(factoextra)
library(fpc)
library(dbscan)
library(cluster)
library(wskm)
library(pdfCluster)
library(ggplot2)
library(ggsci)

data("multishapes")
multishapes$shape<-as.factor(multishapes$shape)
names(multishapes)[3]<-"labels"
ggplot(multishapes,aes(x=x,y=y,colour=labels))+geom_point()+scale_colour_npg()+theme_minimal()
points<-multishapes[,1:2]

set.seed(10)
x1<-seq(0,pi,length.out=100)
y1<-sin(x1)+0.1*rnorm(100)
x2<-1.5+ seq(0,pi,length.out=100)
y2<-cos(x2)+0.1*rnorm(100)
s2a<-data.frame(c(x1,x2),c(y1,y2),factor(c((rep(1,100)),rep(2,100))))
names(s2a)<-c("x","y","labels")  
ggplot(s2a,aes(x=x,y=y,colour=labels))+geom_point()+scale_colour_npg()+theme_minimal()
s2<-s2a[,1:2]

set.seed(10)
r1<-runif(300,0,9.5)
r3<-runif(100,0,5)
theta1<-pi*runif(300,0,10)
theta3<-pi*runif(100,0,10)
x1<-r1*cos(theta1);y1<-r1*sin(theta1)
x2<-r1*cos(theta1);y2<-r1*sin(theta1)
x3<-r3*cos(theta3);y3<-r3*sin(theta3)
x<-c(x1,x2+15,x3+27)
y<-c(y1,y2+15,y3+5)
labels<-factor(c(rep(1,300),rep(2,300),rep(3,100)))
g3a<-data.frame(x,y,labels)
ggplot(g3a,aes(x=x,y=y,colour=labels))+geom_point()+scale_colour_npg()+theme_minimal()
g3<-g3a[,1:2]

set.seed(10)
x1<-y1<-seq(1,3,0.005)
x2<-x3<-y2<-y3<-seq(2.8,4.8,0.005)
x<-c(x1+rnorm(401,-0.2,0.15),x2+rnorm(401,0.6,0.15),x3+rnorm(401,0.5,0.15))
y<-c(y1+rnorm(401,0.3,0.15),y2+rnorm(401,-1,0.1),y3+rnorm(401,0,0.1))
labels<-factor(c(rep(1,401),rep(2,401),rep(3,401)))
l3a<-data.frame(x,y,labels)
ggplot(l3a,aes(x=x,y=y,colour=labels))+geom_point()+scale_colour_npg()+theme_minimal()
l3<-l3a[,1:2]

## K-means ##
km1<-kmeans(points, centers = 5)
km2<-kmeans(s2, centers = 2)
km3<-kmeans(g3, centers = 3)
km4<-kmeans(l3, centers = 3)
fviz_cluster(km1, data=points, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(km2, data=s2, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(km3, data=g3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(km4, data=l3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())

## Fuzzy Clustering ##
fuz1<-fanny(points, k = 5)
fuz2<-fanny(s2, k = 2)
fuz3<-fanny(g3, k = 3)
fuz4<-fanny(l3, k = 3)

fviz_cluster(fuz1, data=points, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(fuz2, data=s2, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(fuz3, data=g3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(fuz4, data=l3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())

## ewkm ##
ewkm1<-ewkm(points, centers = 5)
ewkm2<-ewkm(s2, centers = 2)
ewkm3<-ewkm(g3, centers = 3)
ewkm4<-ewkm(l3, centers = 3)

fviz_cluster(ewkm1, data=points, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())
fviz_cluster(ewkm2, data=s2, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())
fviz_cluster(ewkm3, data=g3, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())
fviz_cluster(ewkm4, data=l3, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())

## PAM ##
pam1<-pam(points, k = 5)
pam2<-pam(s2, k = 2)
pam3<-pam(g3, k = 3)
pam4<-pam(l3, k = 3)

fviz_cluster(pam1, data=points, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(pam2, data=s2, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(pam3, data=g3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(pam4, data=l3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())

## Clara ##
cla1<-clara(points, k = 5)
cla2<-clara(s2, k = 2)
cla3<-clara(g3, k = 3)
cla4<-clara(l3, k = 3)

fviz_cluster(cla1, data=points, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(cla2, data=s2, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(cla3, data=g3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(cla4, data=l3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())

## Hierarchical Clustering ##
hc1<-eclust(points, k = 5, "hclust")
hc2<-eclust(s2, k = 2, "hclust")
hc3<-eclust(g3, k = 3, "hclust")
hc4<-eclust(l3, k = 3, "hclust")

fviz_cluster(hc1, data=points, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(hc2, data=s2, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(hc3, data=g3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())
fviz_cluster(hc4, data=l3, geom = "point", pointsize = 2,
             ellipse.type = "convex", palette = "npg",
             ggtheme = theme_minimal())

## DBSCAN ##
db1<-fpc::dbscan(points,eps = 0.15, MinPts = 5)
db2<-fpc::dbscan(s2, eps = 0.5, MinPts = 5)
db3<-fpc::dbscan(g3, eps = 2, MinPts = 5)
db4<-fpc::dbscan(l3, eps = 0.17, MinPts = 5)

fviz_cluster(db1, data=points, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())
fviz_cluster(db2, data=s2, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())
fviz_cluster(db3, data=g3, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())
fviz_cluster(db4, data=l3, geom = 'point', pointsize = 2, 
             ellipse.type = 'convex', palette = 'npg',
             ggtheme = theme_minimal())


