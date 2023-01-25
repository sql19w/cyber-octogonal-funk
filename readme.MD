# cyber-octogonal-funk

## Contents
[Forest fires: Multidimensional scaling on Mahalanobis distance matrix](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#forest-fires-multidimensional-scaling-on-mahalanobis-distance-matrix)

[XGBoost on Wisconsin diagnostic breast cancer data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#xgboost-on-wisconsin-diagnostic-breast-cancer-data)

### Forest fires: Multidimensional scaling on Mahalanobis distance matrix

The dataset describes forest fires from climatic variables such as maximum temperature, windspeed and relative humidity as well as FWI indices, observed over a six month period.

To better comprehend the fires, I performed a dissimilarity analysis between them. For this, I built a Mahalanobis distance matrix and performed multidimensional scaling on it. Mahalanobis distance was computed between each observation regarding every other observation in the dataset, resulting in an nxn matrix. I do not recommend doing this on larger datasets. The function I used to construct such matrix goes as follows:


```
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
```
Additionally, I utilized multidimensional scaling on the features (climatic + FWI) correlation matrix, in order to gain a clearer understanding of how the indices were composed. To further supplement this analysis, I also conducted PCA.

Conclusively, I trained various models such as support vector machine, decision tree, random forest, and single-layer perceptron to predict the forest fires and evaluated their performance, with a strong emphasis on model specificity.

I'll be working on a time series approach which I hope I'll be able to share shortly.

#### Resources

* [Dataset](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_data.csv)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires.R)
* [Full report (please translate it using AI)](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_report.pdf)



### XGBoost on Wisconsin diagnostic breast cancer data
