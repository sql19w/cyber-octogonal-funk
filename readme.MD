# cyber-octogonal-funk

### Mahalanobis distance matrix for quick visual detection of outliers and some ML algos on forest fire classification

The dataset describes forest fires from climatic variables such as maximum temperature, windspeed and relative humidity as well as FWI indices observed on a six month period. I performed a dissimilarity analysis between the fires. For this, I built a mahalanobis distance matrix and performed multidimensional scaling from it. I also ran mds from the feature correlation matrix, to better understand how the FWI indices were composed. For this purpose, I also performed a principal component analysis. 
Finally, I trained some models (SVM, decision tree, random forest, single-layer perceptron)

For more info on the dataset, please visit: https://archive.ics.uci.edu/ml/datasets/Algerian+Forest+Fires+Dataset++

* [data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_data.csv)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires.R)
* [report (please translate it using AI)](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_report.pdf)
