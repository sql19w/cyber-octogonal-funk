library(caret)
library(rpart)
library(rpart.plot)
library(jpeg)
library(dplyr)
library(nnet)
library(NeuralNetTools)

#Data Set Information:
# We used preprocessing programs made available by NIST to extract normalized bitmaps
# of handwritten digits from a preprinted form.
# From a total of 43 people, 30 contributed to the training set and different 13 to the
# test set. 32x32 bitmaps are divided into nonoverlapping blocks of 4x4 and the number of
# on pixels are counted in each block.
# This generates an input matrix of 8x8 where each element is an integer in the range
# 0..16. This reduces dimensionality and gives invariance to small distortions.
# For info on NIST preprocessing routines, see M. D. Garris, J. L. Blue, G. T. Candela,
# D. L. Dimmick, J. Geist, P. J. Grother, S. A. Janet, and C. L. Wilson, NIST Form-Based
# Handprint Recognition System, NISTIR 5469, 1994.

base=read.table("optdigits.tra",sep=",")
str(base)
dim(base)
base

vector=base[15,] #tomamos la fila 15 por separado
vector #visualizamos que la fila sigue siendo un data.frame con títulos de columnas

#vemos qué número se va a dibujar
vector=vector[-65] #sacamos la columna V65 (variable a predecir – valor)
vector=as.numeric(vector) #transformamos el data.frame a vector numérico
vector #visualizamos que la fila ahora es un vector sin títulos de columnas
vector=vector/16 #transformamos los valores de (1:16) a (0:1)
vector

imagen=array(vector,dim=c(8,8)) #creamos la imagen de 8x8
imagen
imagen=t(imagen) #rotamos la imagen

plot(as.raster(imagen)) #número 3
# writeJPEG(imagen,"other/num15.jpg")

names(base)[names(base)=="V65"]="NumeroCorrespondiente"
base$NumeroCorrespondiente=as.factor(base$NumeroCorrespondiente)
glimpse(base)

plot(base$NumeroCorrespondiente,main="distribución ~ de la variable a predecir",xlab='número',ylim=c(0,400),col="#80cab3") #uniforme, bien.
#base %>% ggplot(aes(x=NumeroCorrespondiente))+geom_bar(fill="#80cab3")+ggtitle('distribución ~ de la variable a predecir')+theme_classic()

##
summary(base$NumeroCorrespondiente)
#o
base %>% group_by(NumeroCorrespondiente) %>% summarise('cantidad'=n())

##

set.seed(478);particion=createDataPartition(y=base$NumeroCorrespondiente,p=0.8,list=FALSE)
entreno=base[particion,]
testeo=base[-particion,]

dim(entreno)
dim(testeo)

#composición de los conjuntos de entrenamiento y testeo
a <- rbind(table(entreno$NumeroCorrespondiente),table(testeo$NumeroCorrespondiente))
rownames(a) <- c('entreno','testeo')
a


arbol=rpart(NumeroCorrespondiente ~.,entreno,method="class")
rpart.plot(arbol,extra=1,type=5,cex=0.5) #18 hojas

pred=predict(arbol,testeo,type="class")
confusionMatrix(pred,testeo$NumeroCorrespondiente) 

#tomamos un número aleatorio 
numero=base[478,]
numero
predict(arbol,numero,type="class") #si coincide, aunque el registro pertenece al conjunto de entrenamiento.
predict(arbol,numero) #devuelve la probabilidad de cada clase
rpart.predict(arbol, numero, rules=TRUE) #además, muestra las reglas de decisión que influyeron en la clasificación

arbol$control #muestra parámetros del árbol
#minsplit= 20

#vamos a armar un árbol con menor minsplit
arbolGrande=rpart(NumeroCorrespondiente~.,entreno,method="class",cp=0,minsplit=5)
rpart.plot(arbolGrande,extra=1,type=5,cex=0.5)
#el arbol que resulta obviamente es ininteligible

plotcp(arbolGrande)
arbolPodado=prune(arbolGrande, cp=0.00042) #lo podamos
rpart.plot(arbolPodado) #sigue siendo una monstruosidad, aunque más sencilla


#probemos con una red neuronal
pred=predict(arbolPodado,testeo,type="class")
confusionMatrix(pred,testeo$NumeroCorrespondiente) #este arbol desempeña considerablemente mejor

#veamos cómo desempeña un perceptrón simple.
#vamos a realizar un ajuste de hiperparámetros y cross-validation con k=10
set.seed(8);modelo=train(NumeroCorrespondiente~.,base,maxit=1000,MaxNWts=2000,method="nnet",
                         trControl=trainControl(method="cv",10),
                         tuneGrid=expand.grid(size=c(5,10,20),decay=c(0.001,0.01,0.1)))
plotnet(modelo)
modelo
mean(modelo$resample[,"Accuracy"]) #accuracies de los modelos a los que tomo la media

###

#segmentación de imágenes con K-means

imagen=readJPEG("cangrejo.jpg")
imagen

rojo=as.vector(imagen[,,1])
verde=as.vector(imagen[,,2])
azul=as.vector(imagen[,,3])
base=data.frame(rojo,verde,azul)
set.seed(123);km=kmeans(base,2)

#reconstruimos la imagen segmentada
segmR=rojo
segmV=verde
segmA=azul
segmR[km$cluster==1]=km$center[1,1]
segmV[km$cluster==1]=km$center[1,2]
segmA[km$cluster==1]=km$center[1,3]
segmR[km$cluster==2]=km$center[2,1]
segmV[km$cluster==2]=km$center[2,2]
segmA[km$cluster==2]=km$center[2,3]
segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
plot(as.raster(segmentada))
writeJPEG(segmentada,"segmentada2g.jpg")

imagen=readJPEG("data/cangrejo.jpg")
rojo=as.vector(imagen[,,1])
verde=as.vector(imagen[,,2])
azul=as.vector(imagen[,,3])
base=data.frame(rojo,verde,azul)
set.seed(123);km=kmeans(base,3)

#reconstruimos la imagen segmentada
segmR=rojo
segmV=verde
segmA=azul
segmR[km$cluster==1]=km$center[1,1]
segmV[km$cluster==1]=km$center[1,2]
segmA[km$cluster==1]=km$center[1,3]
segmR[km$cluster==2]=km$center[2,1]
segmV[km$cluster==2]=km$center[2,2]
segmA[km$cluster==2]=km$center[2,3]
segmR[km$cluster==3]=km$center[3,1]
segmV[km$cluster==3]=km$center[3,2]
segmA[km$cluster==3]=km$center[3,3]
segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
plot(as.raster(segmentada))
writeJPEG(segmentada,"segmentada3g.jpg")

#reconstruimos la imagen segmentada
segmR=rojo
segmV=verde
segmA=azul
segmR[km$cluster==1]=1
# segmV[km$cluster==1]=0
# segmA[km$cluster==1]=0
segmR[km$cluster==2]=0
segmV[km$cluster==2]=1
segmA[km$cluster==2]=0
# segmR[km$cluster==3]=0
# segmV[km$cluster==3]=0
# segmA[km$cluster==3]=1
segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
plot(as.raster(segmentada))
writeJPEG(segmentada,"segmentada3col.jpg")

#Reconstruimos la imagen segmentada
segmR=rojo
segmV=verde
segmA=azul
# segmR[km$cluster==1]=1
segmV[km$cluster==1]=0
# segmA[km$cluster==1]=0
#segmR[km$cluster==2]=0
#segmV[km$cluster==2]=1
segmA[km$cluster==2]=0
segmR[km$cluster==3]=0
segmV[km$cluster==3]=0
segmA[km$cluster==3]=1
segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
plot(as.raster(segmentada))
writeJPEG(segmentada,"segmentada4.jpg")

#Reconstruir la imagen segmentada
# segmR=rojo
segmV=verde
segmA=azul
# segmR[km$cluster==1]=1
# segmV[km$cluster==1]=0
# segmA[km$cluster==1]=0
# segmR[km$cluster==2]=0
# segmV[km$cluster==2]=1
# segmA[km$cluster==2]=0
segmR[km$cluster==3]=1
segmV[km$cluster==3]=1
segmA[km$cluster==3]=1
segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
plot(as.raster(segmentada))
writeJPEG(segmentada,"segmentada5.jpg")
