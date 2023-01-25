# cyber-octogonal-funk




### Mahalanobis distance matrix for quick visual detection of outliers and some ML algos on forest fire classification

The dataset describes forest fires from climatic variables such as maximum temperature, windspeed and relative humidity as well as FWI indices observed over a six month period. To better comprehend the fires, I performed a dissimilarity analysis between them. For this, I built a mahalanobis distance matrix and performed multidimensional scaling from it. I also ran mds on the features correlation matrix, to better understand how the FWI indices were composed. For this purpose, I performed principal components analysis too.

Finally, I trained some models (support vector machine, decision tree, random forest, single-layer perceptron) and evaluated their performance, prioritizing model specificity.

For more info on the dataset, please visit: https://archive.ics.uci.edu/ml/datasets/Algerian+Forest+Fires+Dataset++

* [data](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_data.csv)
* [R script](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires.R)
* [report (please translate it using AI)](https://github.com/sql19w/cyber-octogonal-funk/blob/main/forest_fires_report.pdf)
