# cyber-octogonal-funk




### Mahalanobis distance matrix for quick visual detection of outliers and some ML algos on forest fire classification

The dataset describes forest fires from climatic variables such as maximum temperature, windspeed and relative humidity as well as FWI indices, observed over a six month period.

To better comprehend the fires, I performed a dissimilarity analysis between them. For this, I built a Mahalanobis distance matrix and performed multidimensional scaling from it. Mahalanobis distance was computed between each observation regarding every other observation in the dataset, resulting in an nxn matrix. I do not recommend doing this on larger datasets. The function I used to construct such matrix goes as follows:


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

I also ran mds on the features correlation matrix, to better understand how the FWI indices were composed. For this purpose, I performed principal components analysis too.

Finally, I trained some models (support vector machine, decision tree, random forest, single-layer perceptron) and evaluated their performance, prioritizing model specificity.

I'll be working on a time series approach which I hope I'll be able to share shortly.

For more info on the dataset, please visit: https://archive.ics.uci.edu/ml/datasets/Algerian+Forest+Fires+Dataset++

* [data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_data.csv)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires.R)
* [report (please translate it using AI)](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_report.pdf)
