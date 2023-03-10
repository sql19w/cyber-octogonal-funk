### Hey there!
My name is Ignacio and I am a Buenos Aires based data scientist, currently working in the field of logistics. With a strong background in statistical analysis and machine learning, I am passionate about uncovering insights from data and utilizing them to drive business decisions. In addition to my professional work, I also enjoy delving into side projects to stay current on the latest technologies and techniques. I'm particulary interested in medical data, life and social sciences. Below you'll find short readings about some of my side projects that showcase my skills and interests. Whether it's experimenting with new technologies or simply showcasing my skills, I am always looking for new challenges and opportunities to grow as a data scientist. So have a look around and feel free to reach out if you have any questions or want to collaborate on a project 😎.


&nbsp;
&nbsp;

### Contents
[Forest fires: Multidimensional scaling on Mahalanobis distance matrix](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#forest-fires-multidimensional-scaling-on-mahalanobis-distance-matrix)

[Edge detection using k-means & classifying hand written numbers with decision tree and neural network](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#edge-detection-using-k-means--classifying-handwritten-numbers-with-decision-tree-and-neural-network)

[XGBoost on Wisconsin diagnostic breast cancer data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#xgboost-on-wisconsin-diagnostic-breast-cancer-data)

&nbsp;
&nbsp;
&nbsp;
&nbsp;
&nbsp;
&nbsp;
### Forest fires: Multidimensional scaling on Mahalanobis distance matrix

#pca #mds #mahalanobis-distance #decision-tree #random-forest #svm #neural-net #R

The dataset describes forest fires from climatic variables such as maximum temperature, windspeed and relative humidity as well as FWI indices, observed over a six month period. The data includes observations from two different regions in Argelia and are time-stamped, so the problem could be addressed with a time-series approach, which I'll probably do in future posts. For simplification, I worked on one of the regions only.

To better comprehend the fires, I performed a dissimilarity analysis between them. For this, I built a Mahalanobis distance matrix and performed multidimensional scaling on it. Mahalanobis distance was computed between each observation regarding every other one in the dataset, resulting in an nxn matrix. I do not recommend doing this on larger datasets. The function I used to construct such matrix goes as follows:


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

#pca #mds #mahalanobis-distance #decision-tree #random-forest #svm #neural-net #R

#### Resources

* [Dataset](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_data.csv)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires.R)
* [Full report (please translate it using AI)](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_report.pdf)

&nbsp;
&nbsp;
&nbsp;

### Edge detection using k-means & classifying handwritten numbers with decision tree and neural network

#k-means #decision-trees #R

#### Resources

* [Dataset: numbers](https://github.com/sql19w/cyber-octogonal-funk/blob/main/optdigits.tra)
* [Dataset: crab jpeg](https://github.com/sql19w/cyber-octogonal-funk/blob/main/cangrejo.jpg)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/images.R)

&nbsp;
&nbsp;
&nbsp;

### XGBoost on Wisconsin diagnostic breast cancer data

#pca #factor-analysis #classification #logistic-regression #svm #xgboost #R

WDBC data consists of pre-processed image data on potential cancer cell biopsy. Given the nature of this case study, if we were to rely upon diagnostics based on information systems, accuracy itself sheds little light on the matter: both specificity and sensibility have to be taken in account.

After performing a brief exploratory analysis including pca and factor analysis, I mounted a state of the art xgboost implementation (validation set, k-fold, hyperparameters tuning via grid search) and compared them against baseline models (namely logistic regression and support vector machine). 

#pca #factor-analysis #classification #logistic-regression #svm #xgboost #R

#### Resources

* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/wdbc_xgboost.R)

&nbsp;
&nbsp;
&nbsp;


