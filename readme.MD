# cyber-octogonal-funk

### Hey there!
My name is Ignacio and I am a Buenos Aires based data scientist, currently working in the field of logistics. With a strong background in statistical analysis and machine learning, I am passionate about uncovering insights from data and utilizing them to drive business decisions. In addition to my professional work, I also enjoy delving into side projects to stay current on the latest technologies and techniques. I'm particulary interested in medical data, life and social sciences. Below you'll find short readings about some of my side projects that showcase my skills and interests, from fun personal projects to more complex data-driven applications. Whether it's experimenting with new technologies or simply showcasing my skills, I am always looking for new challenges and opportunities to grow as a data scientist. So have a look around and feel free to reach out if you have any questions or want to collaborate on a project 😎.


&nbsp;
&nbsp;

### Contents
[Forest fires: Multidimensional scaling on Mahalanobis distance matrix](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#forest-fires-multidimensional-scaling-on-mahalanobis-distance-matrix)

[Image pattern recognition using K-means](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#exploratory-analysis-on-pandemic-data)

[XGBoost on Wisconsin diagnostic breast cancer data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#xgboost-on-wisconsin-diagnostic-breast-cancer-data)

[Exploratory analysis on pandemic data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/readme.MD#exploratory-analysis-on-pandemic-data)

&nbsp;
&nbsp;
&nbsp;
&nbsp;
&nbsp;
&nbsp;
### Forest fires: Multidimensional scaling on Mahalanobis distance matrix

The dataset describes forest fires from climatic variables such as maximum temperature, windspeed and relative humidity as well as FWI indices, observed over a six month period.

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

#### Resources

* [Dataset](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_data.csv)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires.R)
* [Full report (please translate it using AI)](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_report.pdf)

&nbsp;
&nbsp;
&nbsp;

### Image pattern recognition using K-means

#### Resources

* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/wdbc_xgboost.R)

&nbsp;
&nbsp;
&nbsp;

### XGBoost on Wisconsin diagnostic breast cancer data

<img src=https://github.com/sql19w/cyber-octogonal-funk/blob/main/xgboostmeme.png width="30%" height="30%">

So I decided to test which seems to be one of the hottest ML algorithms nowadays.

WDBC data consists of pre-processed image data on potential cancer cell biopsy. Given the nature of this case study, if we were to rely upon diagnostics based on information systems, accuracy itself sheds little light on the matter: both specificity and sensibility have to be taken in account.

#### Resources

* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/wdbc_xgboost.R)

&nbsp;
&nbsp;
&nbsp;

### Exploratory analysis on pandemic data

#### Resources

* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/wdbc_xgboost.R)

&nbsp;
&nbsp;
&nbsp;
