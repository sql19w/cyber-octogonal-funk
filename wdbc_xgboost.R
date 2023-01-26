library(ggplot2)
library(corrplot)
library(caTools)
library(funModeling)
library(dplyr)
library(factoextra)
library(nFactors)
library(semPlot)

wdbc<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")

features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))

colnames(wdbc)
glimpse(wdbc)

# ensayos chi cuadrado contra variable de respuesta. debería automatizarlo luego

chisq.test(wdbc$diagnosis,wdbc$radius_mean)
chisq.test(wdbc$diagnosis,wdbc$texture_mean)
chisq.test(wdbc$diagnosis,wdbc$perimeter_mean)
chisq.test(wdbc$diagnosis,wdbc$area_mean)
chisq.test(wdbc$diagnosis,wdbc$smoothness_mean)
chisq.test(wdbc$diagnosis,wdbc$compactness_mean)
chisq.test(wdbc$diagnosis,wdbc$concavity_mean)
chisq.test(wdbc$diagnosis,wdbc$concave_points_mean)
chisq.test(wdbc$diagnosis,wdbc$symmetry_mean)
chisq.test(wdbc$diagnosis,wdbc$fractal_dimension_mean)
chisq.test(wdbc$diagnosis,wdbc$radius_se)
chisq.test(wdbc$diagnosis,wdbc$texture_se)
chisq.test(wdbc$diagnosis,wdbc$perimeter_se)
chisq.test(wdbc$diagnosis,wdbc$area_se)
chisq.test(wdbc$diagnosis,wdbc$smoothness_se)
chisq.test(wdbc$diagnosis,wdbc$compactness_se)
chisq.test(wdbc$diagnosis,wdbc$concavity_se)
chisq.test(wdbc$diagnosis,wdbc$concave_points_se)
chisq.test(wdbc$diagnosis,wdbc$symmetry_se)
chisq.test(wdbc$diagnosis,wdbc$fractal_dimension_se)
chisq.test(wdbc$diagnosis,wdbc$radius_worst)
chisq.test(wdbc$diagnosis,wdbc$texture_worst)
chisq.test(wdbc$diagnosis,wdbc$perimeter_worst)
chisq.test(wdbc$diagnosis,wdbc$area_worst)
chisq.test(wdbc$diagnosis,wdbc$smoothness_worst)
chisq.test(wdbc$diagnosis,wdbc$compactness_worst)
chisq.test(wdbc$diagnosis,wdbc$concavity_worst)
chisq.test(wdbc$diagnosis,wdbc$concave_points_worst)
chisq.test(wdbc$diagnosis,wdbc$symmetry_worst)
chisq.test(wdbc$diagnosis,wdbc$fractal_dimension_worst)

wdbc$diagnosis <- ifelse(wdbc$diagnosis =='M',1,0)
summary(wdbc)
glimpse(wdbc)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)

corrplot(cor(wdbc %>% select(-id)),method="color",type="lower",col=COL2("PRGn",200),tl.col="black") #addCoef.col = 'black'

#revisamos las distribuciones de las variables cuyos test chi2 dieron significativos
wdbc %>% ggplot(aes(x=(radius_mean),fill=as.factor(diagnosis)))+geom_density(alpha=0.8)+ggtitle('distribuciones del radio medio contra diagn?stico')
wdbc %>% ggplot(aes(x=radius_worst,fill=as.factor(diagnosis)))+geom_density(alpha=0.8)+ggtitle('distribuciones del radio worst contra diagn?stico')
wdbc %>% ggplot(aes(x=concave_points_worst,fill=as.factor(diagnosis)))+geom_density(alpha=0.8)+ggtitle('distribuciones del concave_points_worst contra diagn?stico')

wdbc %>% ggplot(aes(x=radius_mean))+geom_boxplot()

#vamos a imputar outliers
outliersReplace <- function(data, low, high){
    data[data < low] <- mean(data)
    data[data > high] <- mean(data)
    data       
}

wdbc %>% ggplot(aes(y=fractal_dimension_worst,x=as.factor(diagnosis)))+geom_boxplot()
summary(wdbc$fractal_dimension_worst)[2] -IQR(wdbc$fractal_dimension_worst)

glimpse(wdbc)

#los valores atípicos de los atributos que no se distribuyan de forma significativamente distinta contra la clase diagnóstico los imputamos directamente con la media del atributo
wdbc$fractal_dimension_worst <- outliersReplace(wdbc$fractal_dimension_worst,summary(wdbc$fractal_dimension_worst)[2] -IQR(wdbc$fractal_dimension_worst),summary(wdbc$fractal_dimension_worst)[5] +IQR(wdbc$fractal_dimension_worst))
wdbc$symmetry_worst <- outliersReplace(wdbc$symmetry_worst,summary(wdbc$symmetry_worst)[2] -IQR(wdbc$symmetry_worst),summary(wdbc$symmetry_worst)[5] +IQR(wdbc$symmetry_worst))
wdbc$concavity_worst <- outliersReplace(wdbc$concavity_worst,summary(wdbc$concavity_worst)[2] -IQR(wdbc$concavity_worst),summary(wdbc$concavity_worst)[5] +IQR(wdbc$concavity_worst))
wdbc$compactness_worst <- outliersReplace(wdbc$compactness_worst,summary(wdbc$compactness_worst)[2] -IQR(wdbc$compactness_worst),summary(wdbc$compactness_worst)[5] +IQR(wdbc$compactness_worst))
wdbc$smoothness_worst <- outliersReplace(wdbc$smoothness_worst,summary(wdbc$smoothness_worst)[2] -IQR(wdbc$smoothness_worst),summary(wdbc$smoothness_worst)[5] +IQR(wdbc$smoothness_worst))
wdbc$area_worst <- outliersReplace(wdbc$area_worst,summary(wdbc$area_worst)[2] -IQR(wdbc$area_worst),summary(wdbc$area_worst)[5] +IQR(wdbc$area_worst))
wdbc$perimeter_worst <- outliersReplace(wdbc$perimeter_worst,summary(wdbc$perimeter_worst)[2] -IQR(wdbc$perimeter_worst),summary(wdbc$perimeter_worst)[5] +IQR(wdbc$perimeter_worst))
wdbc$texture_worst <- outliersReplace(wdbc$texture_worst,summary(wdbc$texture_worst)[2] -IQR(wdbc$texture_worst),summary(wdbc$texture_worst)[5] +IQR(wdbc$texture_worst))
wdbc$fractal_dimension_se <- outliersReplace(wdbc$fractal_dimension_se,summary(wdbc$fractal_dimension_se)[2] -IQR(wdbc$fractal_dimension_se),summary(wdbc$fractal_dimension_se)[5] +IQR(wdbc$fractal_dimension_se))
wdbc$symmetry_se <- outliersReplace(wdbc$symmetry_se,summary(wdbc$symmetry_se)[2] -IQR(wdbc$symmetry_se),summary(wdbc$symmetry_se)[5] +IQR(wdbc$symmetry_se))
wdbc$concave_points_se <- outliersReplace(wdbc$concave_points_se,summary(wdbc$concave_points_se)[2] -IQR(wdbc$concave_points_se),summary(wdbc$concave_points_se)[5] +IQR(wdbc$concave_points_se))
wdbc$concavity_se <- outliersReplace(wdbc$concavity_se,summary(wdbc$concavity_se)[2] -IQR(wdbc$concavity_se),summary(wdbc$concavity_se)[5] +IQR(wdbc$concavity_se))
wdbc$compactness_se <- outliersReplace(wdbc$compactness_se,summary(wdbc$compactness_se)[2] -IQR(wdbc$compactness_se),summary(wdbc$compactness_se)[5] +IQR(wdbc$compactness_se))
wdbc$smoothness_se <- outliersReplace(wdbc$smoothness_se,summary(wdbc$smoothness_se)[2] -IQR(wdbc$smoothness_se),summary(wdbc$smoothness_se)[5] +IQR(wdbc$smoothness_se))
wdbc$area_se <- outliersReplace(wdbc$area_se,summary(wdbc$area_se)[2] -IQR(wdbc$area_se),summary(wdbc$area_se)[5] +IQR(wdbc$area_se))
wdbc$perimeter_se <- outliersReplace(wdbc$perimeter_se,summary(wdbc$perimeter_se)[2] -IQR(wdbc$perimeter_se),summary(wdbc$perimeter_se)[5] +IQR(wdbc$perimeter_se))
wdbc$texture_se <- outliersReplace(wdbc$texture_se,summary(wdbc$texture_se)[2] -IQR(wdbc$texture_se),summary(wdbc$texture_se)[5] +IQR(wdbc$texture_se))
wdbc$radius_se <- outliersReplace(wdbc$radius_se,summary(wdbc$radius_se)[2] -IQR(wdbc$radius_se),summary(wdbc$radius_se)[5] +IQR(wdbc$radius_se))
wdbc$fractal_dimension_mean <- outliersReplace(wdbc$fractal_dimension_mean,summary(wdbc$fractal_dimension_mean)[2] -IQR(wdbc$fractal_dimension_mean),summary(wdbc$fractal_dimension_mean)[5] +IQR(wdbc$fractal_dimension_mean))
wdbc$symmetry_mean <- outliersReplace(wdbc$symmetry_mean,summary(wdbc$symmetry_mean)[2] -IQR(wdbc$symmetry_mean),summary(wdbc$symmetry_mean)[5] +IQR(wdbc$symmetry_mean))
wdbc$concavity_mean <- outliersReplace(wdbc$concavity_mean,summary(wdbc$concavity_mean)[2] -IQR(wdbc$concavity_mean),summary(wdbc$concavity_mean)[5] +IQR(wdbc$concavity_mean))
wdbc$compactness_mean <- outliersReplace(wdbc$compactness_mean,summary(wdbc$compactness_mean)[2] -IQR(wdbc$compactness_mean),summary(wdbc$compactness_mean)[5] +IQR(wdbc$compactness_mean))
wdbc$smoothness_mean <- outliersReplace(wdbc$smoothness_mean,summary(wdbc$smoothness_mean)[2] -IQR(wdbc$smoothness_mean),summary(wdbc$smoothness_mean)[5] +IQR(wdbc$smoothness_mean))
wdbc$area_mean <- outliersReplace(wdbc$area_mean,summary(wdbc$area_mean)[2] -IQR(wdbc$area_mean),summary(wdbc$area_mean)[5] +IQR(wdbc$area_mean))
wdbc$perimeter_mean <- outliersReplace(wdbc$perimeter_mean,summary(wdbc$perimeter_mean)[2] -IQR(wdbc$perimeter_mean),summary(wdbc$perimeter_mean)[5] +IQR(wdbc$perimeter_mean))
wdbc$texture_mean <- outliersReplace(wdbc$texture_worst,summary(wdbc$texture_mean)[2] -IQR(wdbc$texture_mean),summary(wdbc$texture_mean)[5] +IQR(wdbc$texture_mean))

#los atributos que si resultaron significativamente distintos con el test de chi2, son imputados por la media de la clase correspondiente
wdbc$concave_points_worst[wdbc$diagnosis==0] <- outliersReplace(wdbc$concave_points_worst[wdbc$diagnosis==0],summary(wdbc$concave_points_worst[wdbc$diagnosis==0])[2] -IQR(wdbc$concave_points_worst[wdbc$diagnosis==0]),summary(wdbc$concave_points_worst[wdbc$diagnosis==0])[5] +IQR(wdbc$concave_points_worst[wdbc$diagnosis==0]))
wdbc$concave_points_worst[wdbc$diagnosis==1] <- outliersReplace(wdbc$concave_points_worst[wdbc$diagnosis==1],summary(wdbc$concave_points_worst[wdbc$diagnosis==1])[2] -IQR(wdbc$concave_points_worst[wdbc$diagnosis==1]),summary(wdbc$concave_points_worst[wdbc$diagnosis==1])[5] +IQR(wdbc$concave_points_worst[wdbc$diagnosis==1]))

wdbc %>% ggplot(aes(x=concave_points_worst,y=as.factor(diagnosis)))+geom_boxplot() #chequeamos

wdbc$radius_mean[wdbc$diagnosis==0] <- outliersReplace(wdbc$radius_mean[wdbc$diagnosis==0],summary(wdbc$radius_mean[wdbc$diagnosis==0])[2] -IQR(wdbc$radius_mean[wdbc$diagnosis==0]),summary(wdbc$radius_mean[wdbc$diagnosis==0])[5] +IQR(wdbc$radius_mean[wdbc$diagnosis==0]))
wdbc$radius_mean[wdbc$diagnosis==1] <- outliersReplace(wdbc$radius_mean[wdbc$diagnosis==1],summary(wdbc$radius_mean[wdbc$diagnosis==1])[2] -IQR(wdbc$radius_mean[wdbc$diagnosis==1]),summary(wdbc$radius_mean[wdbc$diagnosis==1])[5] +IQR(wdbc$radius_mean[wdbc$diagnosis==1]))

wdbc %>% ggplot(aes(x=radius_mean,y=as.factor(diagnosis)))+geom_boxplot()

wdbc$radius_worst[wdbc$diagnosis==0] <- outliersReplace(wdbc$radius_worst[wdbc$diagnosis==0],summary(wdbc$radius_worst[wdbc$diagnosis==0])[2] -IQR(wdbc$radius_worst[wdbc$diagnosis==0]),summary(wdbc$radius_worst[wdbc$diagnosis==0])[5] +IQR(wdbc$radius_worst[wdbc$diagnosis==0]))
wdbc$radius_worst[wdbc$diagnosis==1] <- outliersReplace(wdbc$radius_worst[wdbc$diagnosis==1],summary(wdbc$radius_worst[wdbc$diagnosis==1])[2] -IQR(wdbc$radius_worst[wdbc$diagnosis==1]),summary(wdbc$radius_worst[wdbc$diagnosis==1])[5] +IQR(wdbc$radius_worst[wdbc$diagnosis==1]))

wdbc %>% ggplot(aes(x=radius_worst,y=as.factor(diagnosis)))+geom_boxplot()

summary(wdbc)
glimpse(wdbc)


#PCA

wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
summary(wdbc.pr)


screeplot(wdbc.pr, type = "l", npcs = 20, main = "10 primeros PC")
abline(h = 1, col="black", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("black"), lty=5, cex=0.6)

cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))

plot(cumpro[0:15], xlab = "PC", ylab = "varianza explicada (acumulada)")
abline(v = 10, col="blue", lty=5)
abline(h = 0.88176, col="blue", lty=5)

plot(wdbc.pr$x[,1],wdbc.pr$x[,2], xlab="PC1 (40.76%)", ylab = "PC2 (15.46%)", main = "PC1 / PC2 - plot")

wdbc_all <- cbind(wdbc,wdbc.pr$x[,1:10]) #con 10 explicamos 88% de la varianza
plot(wdbc_all$PC1,wdbc_all$PC2,col=factor(wdbc_all$diagnosis))

fviz_pca_ind(wdbc.pr, axes = c(1, 2), geom.ind='point',
             pointshape = 25,
             label = "all", invisible = "none", labelsize = 4,
             pointsize = 2, habillage = "none",
             addEllipses = T, ellipse.level = 0.95, 
             col.ind = "black", col.ind.sup = "blue", alpha.ind = 1,
             fill.ind=as.factor(wdbc$diagnosis),legend.title='diagnosis')




wdbc_all

corrplot(cor(wdbc_all %>% select(diagnosis,PC1,PC2,PC3,PC4,PC5,PC6,
                                 PC8,PC9)),method="color",type="lower",col=COL2("PRGn",200),addCoef.col = 'black',tl.col="black")
#obviamente los componentes principales no se encuentran correlacionados entre sí

modelo <- glm(diagnosis~PC1+PC2+PC3+PC4+PC5+PC6+
                  PC8+PC9,data=wdbc_all,family='binomial') #ajustamos un modelito sencillo
summary(modelo)


r.mat = cor(wdbc)
corrplot(r.mat,method='square',tl.col="black")

wdbc

nS = nScree(r.mat) #los tests sugieren 7 factores
plotnScree(nS) 
f = factanal(wdbc[3:32], factors=10,scores="Bartlett") #bartlett usa mínimos cuadrados
semPaths(f, what="est", residuals=FALSE,
         cut=0.85, posCol=c("white", "black"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)
f

scores <- f$scores %>% as.data.frame() %>% select(Factor1,Factor4,Factor5,Factor7) #tomamos factores que tengan incidencias fuertes

data <- wdbc_all %>% cbind(scores)

glimpse(data)

corrplot(cor(data %>% select(diagnosis,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,Factor1,Factor4,Factor5,Factor7)),method="color",col=COL2("PRGn",200),tl.col="black")

corrplot(cor(data %>% select(diagnosis,contains('Factor'))),method="color",col=COL2("PRGn",200),tl.col="black",addCoef.col = "black")
corrplot(cor(data %>% select(diagnosis,contains('PC'))),method="color",col=COL2("PRGn",200),tl.col="black")
corrplot(cor(data))


glimpse(data)
summary(data)

#conjuntos de entrenamiento y testeo

data

data$split <- sample.split(data$id,SplitRatio =0.80)

train_data <- subset(data,split==TRUE)
test_data <- subset(data,split==FALSE)
glimpse(test_data)


train_data<- train_data %>% select(-c(split))
test_data <- test_data %>% select(-split)

glimpse(train_data)
glimpse(test_data)


train_data_soloPC_factores <- train_data %>% select(diagnosis,contains('PC'),contains('Factor'))
test_data_soloPC_factores <- test_data %>% select(diagnosis,contains('PC'),contains('Factor'))

train_data_original <- train_data %>% select(diagnosis,contains('mean'),contains('se'),contains("worst"))
test_data_original <- test_data %>% select(diagnosis,contains('mean'),contains('se'),contains("worst"))

# glimpse(train_data_soloPC_factores)
# glimpse(test_data_soloPC_factores)
# write.csv(train,'train.csv')
# write.csv(test,'test.csv')

# Separamos un conjunto de validación
val_index <- sample(1:nrow(train_data_original), 114)
val_data <- train_data_original[val_index,]
train_data_original <- train_data_original[-val_index,]

library(xgboost)

# xgboost come sólo lo que le gusta
dtrain <- xgb.DMatrix(data = as.matrix(train_data_original[,setdiff(colnames(train_data_original), "diagnosis")]),
                      label = as.matrix(train_data_original[,"diagnosis"]))

dvalid <- xgb.DMatrix(data = as.matrix(val_data[,setdiff(colnames(val_data), "diagnosis")]),
                      label = as.matrix(val_data[,"diagnosis"]))

# ajuste de hiperparámetros

random_grid <- function(size,
                        min_nrounds, max_nrounds,
                        min_max_depth, max_max_depth,
                        min_eta, max_eta,
                        min_gamma, max_gamma,
                        min_colsample_bytree, max_colsample_bytree,
                        min_min_child_weight, max_min_child_weight,
                        min_subsample, max_subsample) {
    
    rgrid <- data.frame(nrounds = if (min_nrounds == max_nrounds) {
        rep(min_nrounds, size)
    } else {
        sample(c(min_nrounds:max_nrounds),
               size = size, replace = TRUE)
    },
    max_depth = if (min_max_depth == max_max_depth) {
        rep(min_max_depth, size)
    } else {
        sample(c(min_max_depth:max_max_depth),
               size = size, replace = TRUE)
    },
    eta = if (min_eta == max_eta) {
        rep(min_eta, size)
    } else {
        round(runif(size, min_eta, max_eta), 5)
    },
    gamma = if (min_gamma == max_gamma) {
        rep(min_gamma, size)
    } else {
        round(runif(size, min_gamma, max_gamma), 5)
    },
    colsample_bytree = if (min_colsample_bytree == max_colsample_bytree) {
        rep(min_colsample_bytree, size)
    } else {
        round(runif(size, min_colsample_bytree, max_colsample_bytree), 5)
    },
    min_child_weight = if (min_min_child_weight == max_min_child_weight) {
        rep(min_min_child_weight, size)
    } else {
        round(runif(size, min_min_child_weight, max_min_child_weight), 5)
    },
    subsample = if (min_subsample == max_subsample) {
        rep(min_subsample, size)
    } else {
        round(runif(size, min_subsample, max_subsample), 5)
    })
    
    return(rgrid)
}


rgrid <- random_grid(size = 5,
                     min_nrounds = 50, max_nrounds = 300,
                     min_max_depth = 2, max_max_depth = 12,
                     min_eta = 0.001, max_eta = 0.125,
                     min_gamma = 0, max_gamma = 1,
                     min_colsample_bytree = 0.5, max_colsample_bytree = 1,
                     min_min_child_weight = 0, max_min_child_weight = 2,
                     min_subsample = 0.5, max_subsample = 1)


train_xgboost <- function(data_train, data_val, rgrid) {
    
    watchlist <- list(train = data_train, valid = data_val)
    
    predicted_models <- list()
    
    for (i in seq_len(nrow(rgrid))) {
        print(i)
        print(rgrid[i,])
        trained_model <- xgb.train(data = data_train,
                                   params=as.list(rgrid[i, c("max_depth",
                                                             "eta",
                                                             "gamma",
                                                             "colsample_bytree",
                                                             "subsample",
                                                             "min_child_weight")]),
                                   nrounds = rgrid[i, "nrounds"],
                                   watchlist = watchlist,
                                   objective = "binary:logistic",
                                   eval.metric = "auc",
                                   print_every_n = 10)
        
        perf_tr <- tail(trained_model$evaluation_log, 1)$train_auc
        perf_vd <- tail(trained_model$evaluation_log, 1)$valid_auc
        print(c(perf_tr, perf_vd))
        
        predicted_models[[i]] <- list(results = data.frame(rgrid[i,],
                                                           perf_tr = perf_tr,
                                                           perf_vd = perf_vd),
                                      model = trained_model)
        rm(trained_model)
        gc()
    }
    
    return(predicted_models)
}

set.seed(123);predicted_models <- train_xgboost(dtrain, dvalid, rgrid) #como dios manda

result_table <- function(pred_models, higher_is_better = TRUE) {
    res_table <- data.frame()
    i <- 1
    for (m in pred_models) {
        res_table <- rbind(res_table, data.frame(i = i, m$results))
        i <- i + 1
    }
    
    hib <- if (higher_is_better) -1 else 1
    
    res_table <- res_table[order(hib *res_table$perf_vd),]
    return(res_table)
}
results <- result_table(predicted_models)
results
#siendo
predicted_models[[results[1, "i"]]]$model #el mejor modelo

#para ver que features son mejores para el mejor modelo
mejor_modelo=predicted_models[[results[1, "i"]]]$model
mejor_modelo
xgb.importance(feature_names = mejor_modelo$feature_names,
               model = mejor_modelo)

dtest <- xgb.DMatrix(data = as.matrix(test_data_original[,setdiff(colnames(test_data_original), "diagnosis")]),
                     label = as.matrix(test_data_original[,"diagnosis"]))

preds <- predict(mejor_modelo, dtest)

test_data_original <- test_data_original %>% cbind(preds)

test_data_original$preds_bin <- ifelse(test_data_original$preds>=0.5,1,0)
glimpse(test_data_original)


table(test_data_original$diagnosis,test_data_original$preds_bin)
proportions(table(test_data_original$diagnosis,test_data_original$preds_bin),1)


library(caret)

confusionMatrix(as.factor(test_data_original$preds_bin),as.factor(test_data_original$diagnosis))
#.9912 accuracy, .9730 especificidad

# 1. Title: Wisconsin Diagnostic Breast Cancer (WDBC)
# 
# 2. Source Information
# 
# a) Creators: 
#     
#     Dr. William H. Wolberg, General Surgery Dept., University of
# Wisconsin,  Clinical Sciences Center, Madison, WI 53792
# wolberg@eagle.surgery.wisc.edu
# 
# W. Nick Street, Computer Sciences Dept., University of
# Wisconsin, 1210 West Dayton St., Madison, WI 53706
# street@cs.wisc.edu  608-262-6619
# 
# Olvi L. Mangasarian, Computer Sciences Dept., University of
# Wisconsin, 1210 West Dayton St., Madison, WI 53706
# olvi@cs.wisc.edu 
# 
# b) Donor: Nick Street
# 
# c) Date: November 1995
# 
# 3. Past Usage:
#     
#     first usage:
#     
#     W.N. Street, W.H. Wolberg and O.L. Mangasarian 
# Nuclear feature extraction for breast tumor diagnosis.
# IS&T/SPIE 1993 International Symposium on Electronic Imaging: Science
# and Technology, volume 1905, pages 861-870, San Jose, CA, 1993.
# 
# OR literature:
#     
#     O.L. Mangasarian, W.N. Street and W.H. Wolberg. 
# Breast cancer diagnosis and prognosis via linear programming. 
# Operations Research, 43(4), pages 570-577, July-August 1995.
# 
# Medical literature:
#     
#     W.H. Wolberg, W.N. Street, and O.L. Mangasarian. 
# Machine learning techniques to diagnose breast cancer from
# fine-needle aspirates.  
# Cancer Letters 77 (1994) 163-171.
# 
# W.H. Wolberg, W.N. Street, and O.L. Mangasarian. 
# Image analysis and machine learning applied to breast cancer
# diagnosis and prognosis.  
# Analytical and Quantitative Cytology and Histology, Vol. 17
# No. 2, pages 77-87, April 1995. 
# 
# W.H. Wolberg, W.N. Street, D.M. Heisey, and O.L. Mangasarian. 
# Computerized breast cancer diagnosis and prognosis from fine
# needle aspirates.  
# Archives of Surgery 1995;130:511-516.
# 
# W.H. Wolberg, W.N. Street, D.M. Heisey, and O.L. Mangasarian. 
# Computer-derived nuclear features distinguish malignant from
# benign breast cytology.  
# Human Pathology, 26:792--796, 1995.
# 
# See also:
#     http://www.cs.wisc.edu/~olvi/uwmp/mpml.html
# http://www.cs.wisc.edu/~olvi/uwmp/cancer.html
# 
# Results:
#     
#     - predicting field 2, diagnosis: B = benign, M = malignant
# - sets are linearly separable using all 30 input features
# - best predictive accuracy obtained using one separating plane
# in the 3-D space of Worst Area, Worst Smoothness and
# Mean Texture.  Estimated accuracy 97.5% using repeated
# 10-fold crossvalidations.  Classifier has correctly
# diagnosed 176 consecutive new patients as of November
# 1995. 
# 
# 4. Relevant information
# 
# Features are computed from a digitized image of a fine needle
# aspirate (FNA) of a breast mass.  They describe
# characteristics of the cell nuclei present in the image.
# A few of the images can be found at
# http://www.cs.wisc.edu/~street/images/
#     
#     Separating plane described above was obtained using
# Multisurface Method-Tree (MSM-T) [K. P. Bennett, "Decision Tree
# 	Construction Via Linear Programming." Proceedings of the 4th
#                                   Midwest Artificial Intelligence and Cognitive Science Society,
#                                   pp. 97-101, 1992], a classification method which uses linear
# programming to construct a decision tree.  Relevant features
# were selected using an exhaustive search in the space of 1-4
# features and 1-3 separating planes.
# 
# The actual linear program used to obtain the separating plane
# in the 3-dimensional space is that described in:
#     [K. P. Bennett and O. L. Mangasarian: "Robust Linear
# 	Programming Discrimination of Two Linearly Inseparable Sets",
#      Optimization Methods and Software 1, 1992, 23-34].
# 
# 
# This database is also available through the UW CS ftp server:
#     
#     ftp ftp.cs.wisc.edu
# cd math-prog/cpo-dataset/machine-learn/WDBC/
#     
#     5. Number of instances: 569 
# 
# 6. Number of attributes: 32 (ID, diagnosis, 30 real-valued input features)
# 
# 7. Attribute information
# 
# 1) ID number
# 2) Diagnosis (M = malignant, B = benign)
# 3-32)
# 
# Ten real-valued features are computed for each cell nucleus:
#     
#     a) radius (mean of distances from center to points on the perimeter)
# b) texture (standard deviation of gray-scale values)
# c) perimeter
# d) area
# e) smoothness (local variation in radius lengths)
# f) compactness (perimeter^2 / area - 1.0)
# g) concavity (severity of concave portions of the contour)
# h) concave points (number of concave portions of the contour)
# i) symmetry 
# j) fractal dimension ("coastline approximation" - 1)
# 
# Several of the papers listed above contain detailed descriptions of
# how these features are computed. 
# 
# The mean, standard error, and "worst" or largest (mean of the three
#                                                   largest values) of these features were computed for each image,
# resulting in 30 features.  For instance, field 3 is Mean Radius, field
# 13 is Radius SE, field 23 is Worst Radius.
# 
# All feature values are recoded with four significant digits.
# 
# 8. Missing attribute values: none
# 
# 9. Class distribution: 357 benign, 212 malignant


