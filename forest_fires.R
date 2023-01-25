require(dplyr)
require(ggplot2)
require(funModeling)
require(ltm)
require(corrplot)
require(plotly)
require(caret)
require(nnet)
require(NeuralNetTools)
require(randomForest)
require(rpart)
require(rpart.plot)
require(e1071)

#lectura datos
df <- read.csv("forest_fires_data.csv",sep=";")
df <- df[1:122,5:ncol(df)-1]

colnames(df) <- c("temp","RH","Ws","Rain","FFMC","DMC","DC","ISI","BUI","FWI","incendio" )

#limpieza
df$temp <- as.numeric(df$temp)
df$RH <- as.numeric(df$RH)
df$Ws <- as.numeric(df$Ws)
df$Rain <- as.numeric(df$Rain)
df$FFMC <- as.numeric(df$FFMC)
df$DMC <- as.numeric(df$DMC)
df$DC <- as.numeric(df$DC)
df$ISI <- as.numeric(df$ISI)
df$BUI <- as.numeric(df$BUI)
df$FWI <- as.numeric(df$FWI)
df$incendio <- ifelse(df$incendio=="fire   ",1,0)

funModeling::df_status(df)

# df <- df %>% filter(!is.na(DC),!is.na(FWI))

#EDA
df %>% ggplot(aes(x=temp,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=RH,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=Ws,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=log(Rain),fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=FFMC,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=DMC,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=ISI,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=BUI,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% ggplot(aes(x=FWI,fill=factor(incendio)))+geom_density(alpha=0.8)
df %>% summary()

#ensayos para la diferencia de medias. nivel de significación=0.05
t.test(df[df$incendio==1,]$temp,df[df$incendio==0,]$temp) #significativo
t.test(df[df$incendio==1,]$RH,df[df$incendio==0,]$RH) #significativo
t.test(df[df$incendio==1,]$Ws,df[df$incendio==0,]$Ws) #no significativo
t.test(df[df$incendio==1,]$Rain,df[df$incendio==0,]$Rain) #significativo
t.test(df[df$incendio==1,]$FFMC,df[df$incendio==0,]$FFMC) #significativo
t.test(df[df$incendio==1,]$DMC,df[df$incendio==0,]$DMC) #significativo
t.test(df[df$incendio==1,]$ISI,df[df$incendio==0,]$ISI) #significativo
t.test(df[df$incendio==1,]$BUI,df[df$incendio==0,]$BUI) #significativo
t.test(df[df$incendio==1,]$FWI,df[df$incendio==0,]$FWI) #significativo


df_num <- df %>% select(temp,RH,Ws,Rain,FFMC,DMC,DC,ISI,BUI,FWI)
plot(df_num)
df_num
cor(df_num)
corrplot(cor(df_num),method="number")

heatmap(cor(df_num),method="number",symm=T,Colv=NA,Rowv=NA)

biserial.cor(df$temp,df$incendio)
biserial.cor(df$RH,df$incendio)
biserial.cor(df$Rain,df$incendio)
biserial.cor(df$FFMC,df$incendio)
biserial.cor(df$DMC,df$incendio)
biserial.cor(df$DC,df$incendio)
biserial.cor(df$ISI,df$incendio)
biserial.cor(df$BUI,df$incendio)
biserial.cor(df$FWI,df$incendio)

df %>% ggplot(aes(y=temp,x=ISI,color=factor(incendio)))+geom_point()+ggtitle("Temperatura vs ISI")


#reducción de dimensión
df
df_num <- df %>% dplyr::select(temp,RH,Ws,Rain,FFMC,DMC,ISI,FWI)
df_num

X <- scale(df_num)
X
S <- cov(X)
S
eig <- eigen(S)

lambda <- eig$values
sum(eig$values[1:2])/sum(eig$values) #explican 75.4% de la varianza, bien

V <- X %*% eig$vectors[,1:2]
V <- data.frame(V)
V

plot(V$X1,V$X2,col=factor(df$incendio),pch=19,main = "PCA",xlab = "Componente 1",ylab="Componente 2")

j <- rbind(
  eig$vectors[,1],
  eig$vectors[,2])

j <- as.data.frame(j)
colnames(j) <- colnames(df_num)
rownames(j) <- c("eig1","eig2")

j

sum(eig$values[1:3])/sum(eig$values) #explican 83.8% de la varianza, bien

eig$vectors
V3 <- X %*% eig$vectors[,1:3]
V3 <- data.frame(V3)
V3

plot_ly(V3, type='scatter3d', mode='markers', color = as.factor(df$incendio),
        x = V$X1, y = V$X2, z = V$X3,
        marker = list(size=2))

j3 <- rbind(
  eig$vectors[,1],
  eig$vectors[,2],
  eig$vectors[,3])

j3 <- as.data.frame(j3)
colnames(j3) <- colnames(df_num)
rownames(j3) <- c("PC1","PC2","PC3")

j3

V3
#mahalanobis y MDS
cov1 <- function(x){
  x <- as.matrix(scale(x,scale = F))
  xt <- t(x)
  cov <- 1/(nrow(x)-1) * xt %*% x
  
  return(cov)
}

mahalanobis_dist <- function(x){
  x <- as.matrix(x)
  S <- cov1(x)
  S_1 <- solve(S)
  matriz <- matrix(NA,ncol=nrow(x),nrow=nrow(x))
  
  for(i in 1:nrow(x)){
    for(j in i:nrow(x)){
      a <- as.numeric(x[i,])-as.numeric(x[j,])
      matriz[i,j] = sqrt(t(a) %*% S_1 %*% a)
      b <- as.numeric(x[j,])-as.numeric(x[i,])
      matriz[j,i] = sqrt(t(b) %*% S_1 %*% b)
    }
  }
  return(matriz)
}


incendios <- df_num[df$incendio==1,]
incendios

m <- mahalanobis_dist(incendios)
row.names(m) <- row.names(incendios)
colnames(m) = row.names(incendios)
heatmap(m, Colv = NA, Rowv = NA, symm = T,main = "Matriz de distancia (mahalanobis) entre incendios",ylab="obs.",xlab="obs.")
#los incendios 76, 77 Y 90 son los que menos se parecen a los demás, de hecho, son parecidos a otros días donde no hubo incendios

df[65:90,]
hist(df[df$incendio==1,]$Rain)
hist(df[df$incendio==0,]$Rain)
#dichos días registraron volúmenes atípicos de lluvia para los días en que hay incendios
hist(df[df$incendio==1,]$FWI)
hist(df[df$incendio==0,]$FWI)


fit <- cmdscale(m,eig=TRUE, k=2)
fit # view results

x <- fit$points[,1]
y <- fit$points[,2]

df$incendio <- factor(df$incendio)
plot(x, y, xlab="Coordenada 1", ylab="Coordenada 2",
     type="n",pch=19)
text(x, y, labels = colnames(m), cex=.8)

#no incendios
no_incendios <- df_num[df$incendio==0,]
no_incendios

m2 <- mahalanobis_dist(no_incendios)
row.names(m2) <- row.names(no_incendios)
colnames(m2) = row.names(no_incendios)
heatmap(m2, Colv = NA, Rowv = NA, symm = T,ylab="obs.",xlab="obs.")

#los registros 84 y 92 son los más distintos con respecto de otros días de no incendio
df[80:95,]
hist(df[df$incendio==0,]$ISI) #el registro 84 presenta un ISI elevado (outlier), similar al que se ve en los días de incendio.
hist(df[df$incendio==1,]$ISI)

hist(df$Rain) #el día 92 registró valores atípicos de lluvia.

fit <- cmdscale(m2,eig=TRUE, k=2)
fit

x <- fit$points[,1]
y <- fit$points[,2]

df$incendio <- factor(df$incendio)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n",pch=19,xlim = c(-5,5),ylim=c(-6,6))
text(x, y, labels = colnames(m2), cex=.8)

### todo

m3 <- mahalanobis_dist(df_num)
fit <- cmdscale(m3,eig=TRUE, k=2)
fit

x <- fit$points[,1]
y <- fit$points[,2]
x
y

p <- cbind(x,y,df$incendio)
colnames(p) <- c("x","y","fire")
p <- as.data.frame(p)
p
plot(p$x, p$y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS",pch=19,col=factor(p$fire))

#no parecen diferenciarse demasiado los grupos con 2 dimensiones, veremos qué tal añadiendo una dimensión más

fit <- cmdscale(m3,eig=TRUE, k=3)
fit

x <- fit$points[,1]
y <- fit$points[,2]
z <- fit$points[,3]
x
y
z
p <- cbind(x,y,z,df$incendio)
colnames(p) <- c("x","y","z","fire")
p <- as.data.frame(p)
p
glimpse(p)
plot_ly(p, type='scatter3d', mode='markers',
        x = x, y = y, z = z,color=factor(p$fire),size=5)

###
#MDS matriz de correlación (variables)
cor <- cor(df_num)
cor <- 1-cor
cor
fit <- cmdscale(cor, k=2)
fit
x <- fit[,1]
y <- fit[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = colnames(df_num), cex=.8)

cor <- cor(df_num)
cor <- 1-cor
cor
fit <- cmdscale(cor, k=3)
fit
x <- fit[,1]
y <- fit[,2]
z <- fit[,3]

dd <- cbind(x,y,z)
dd <- as.data.frame(dd)
colnames(dd) <- c("x1","x2","x3")
rownames(dd) <- rownames(fit)
dd
plot_ly(dd, type='scatter3d', mode='markers',
        x = x, y = y, z = z) #hmmmm

#modelos predictivos

#conjunto de entrenamiento y testeo
data <- df %>% dplyr::select(temp,RH,Ws,Rain,FFMC,DMC,DC,ISI,BUI,FWI,incendio)
data <- data[-c(76,77,84),] #removemos aquellos días que son atípicos en su respectiva categoría
data$incendio <- factor(data$incendio)
set.seed(8);part = createDataPartition(y=data$incendio, p=0.7,list=F)
entreno = data[part,]
testeo = data[-part,]

#modelo I: árbol
set.seed(8);arbol = rpart(incendio~.,entreno,method="class")
# rpart.plot(arbol)
pred=predict(arbol,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

#modelo II: rf
set.seed(8);RF = randomForest(incendio~.,entreno,ntree=100,mtry=1)
pred=predict(RF,testeo,type='class')
confusionMatrix(pred,testeo$incendio)

#modelo III: red neuronal
set.seed(8);modelo=train(incendio~.,data,maxit=1000,MaxNWts=2000,method="nnet",
                         trControl=trainControl(method="cv",10), #method = "cv", cross-validation, folds=10
                         tuneGrid=expand.grid(size=c(5,10,20),decay=c(0.001,0.01,0.1))) #tune grid for hyper-parameters
plotnet(modelo)
modelo
mean(modelo$resample[,"Accuracy"]) #accuracies de los modelos a los que tomo la media

#modelo IV: svm
svm = svm(incendio~.,entreno,kernel="linear")
svm
pred=predict(svm,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

##svm - ajuste de hiperparametros
set.seed(123);tune_l = tune.svm(incendio~., data=entreno, kernel="linear",
                                cost = c(0.001, 0.01, 0.1, 0.5, 1))
summary(tune_l)
tune_l$best.model

svm <- tune_l$best.model
pred=predict(svm,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

###utilizando únicamente los componentes principales

data <- cbind(V3,df$incendio)
V3
data <- as.data.frame(data)
data
colnames(data) <- c("X1","X2","X3","incendio")
data <- data[-c(76,77,84),] #removemos aquellos días que son atípicos en su respectiva categoría
data$incendio <- factor(data$incendio)
set.seed(8);part = createDataPartition(y=data$incendio, p=0.7,list=F)
entreno = data[part,]
testeo = data[-part,]

#modelo I: árbol
set.seed(8);arbol = rpart(incendio~.,entreno,method="class")
# rpart.plot(arbol)
pred=predict(arbol,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

#modelo II: rf
set.seed(8);RF = randomForest(incendio~.,entreno,ntree=100,mtry=1)
pred=predict(RF,testeo,type='class')
confusionMatrix(pred,testeo$incendio)

#modelo III: red neuronal
set.seed(8);modelo=train(incendio~.,data,maxit=1000,MaxNWts=2000,method="nnet",
                         trControl=trainControl(method="cv",10), #method = "cv", cross-validation, folds=10
                         tuneGrid=expand.grid(size=c(5,10,20),decay=c(0.001,0.01,0.1))) #tune grid for hyper-parameters
plotnet(modelo)
modelo
mean(modelo$resample[,"Accuracy"]) #accuracies de los modelos a los que tomo la media

#modelo IV: svm
svm = svm(incendio~.,entreno,kernel="linear")
svm
pred=predict(svm,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

##svm - ajuste de hiperparametros
set.seed(123);tune_l = tune.svm(incendio~., data=entreno, kernel="linear",
                                cost = c(0.001, 0.01, 0.1, 0.5, 1))
summary(tune_l)
tune_l$best.model

svm <- tune_l$best.model
pred=predict(svm,testeo,type="class")
confusionMatrix(pred,testeo$incendio)


results <- princomp(df_num)

#visualize results of PCA in biplot
biplot(results)


###utilizando sólo factores climáticos y FWI

data <- df %>% dplyr::select(temp,RH,Ws,Rain,FWI,incendio)
data <- data[-c(76,77,84),] #removemos aquellos días que son atípicos en su respectiva categoría
data$incendio <- factor(data$incendio)
set.seed(8);part = createDataPartition(y=data$incendio, p=0.7,list=F)
entreno = data[part,]
testeo = data[-part,]

#modelo I: árbol
set.seed(8);arbol = rpart(incendio~.,entreno,method="class")
# rpart.plot(arbol)
pred=predict(arbol,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

#modelo II: rf
set.seed(8);RF = randomForest(incendio~.,entreno,ntree=100,mtry=1)
pred=predict(RF,testeo,type='class')
confusionMatrix(pred,testeo$incendio)

#modelo III: red neuronal
set.seed(8);modelo=train(incendio~.,data,maxit=1000,MaxNWts=2000,method="nnet",
                         trControl=trainControl(method="cv",10), #method = "cv", cross-validation, folds=10
                         tuneGrid=expand.grid(size=c(5,10,20),decay=c(0.001,0.01,0.1))) #tune grid for hyper-parameters
plotnet(modelo)
modelo
mean(modelo$resample[,"Accuracy"]) #accuracies de los modelos a los que tomo la media

#modelo IV: svm
svm = svm(incendio~.,entreno,kernel="linear")
svm
pred=predict(svm,testeo,type="class")
confusionMatrix(pred,testeo$incendio)

##svm - ajuste de hiperparametros
set.seed(123);tune_l = tune.svm(incendio~., data=entreno, kernel="linear",
                                cost = c(0.001, 0.01, 0.1, 0.5, 1))
summary(tune_l)
tune_l$best.model

svm <- tune_l$best.model
pred=predict(svm,testeo,type="class")
confusionMatrix(pred,testeo$incendio)


