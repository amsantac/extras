---
title       : Imbalanced classification problem
subtitle    : A remote sensing example
author      : Ali Santacruz 
job         : R-Spatialist
logo        : r-collection-icon-transp.png
framework   : io2012        
highlighter : highlight.js  
hitheme     : tomorrow   
url:
  lib       : librariesNew   
  assets    : assets
widgets     : [mathjax]            
mode        : selfcontained 
knit        : slidify::knit2slides
--- 



<style>
.title-slide {
  background-color: #FFFFFF 
}

.title-slide hgroup > h1{
 font-family: 'Oswald', 'Helvetica', sanserif; 
}

.title-slide hgroup > h1, 
.title-slide hgroup > h2 {
  color: #1E6BB8  
}

.title-slide hgroup p {
  font-weight: bold;
}
</style>

## Key ideas

* You can combine classifiers by averaging/voting
* Combining classifiers improves accuracy
* Combining classifiers reduces interpretability
* Boosting, bagging, and random forests are variants on this theme

--- 

## Import image and training data


```r
library(rgdal)
library(raster)
library(caret)

set.seed(123)

img <- brick(stack(as.list(list.files("data/", "sr_band", full.names = TRUE))))
names(img) <- c(paste0("B", 1:5, coll = ""), "B7") 

trainData <- shapefile("data/training_15.shp")
responseCol <- "class"
```

---

## Extract data from image bands


```r
dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   
for (i in 1:length(unique(trainData[[responseCol]]))){                          
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  dataSet <- sapply(dataSet, function(x){cbind(x, class = rep(category, nrow(x)))})
  df <- do.call("rbind", dataSet)
  dfAll <- rbind(dfAll, df)  
}
```


```r
dim(dfAll)
```

```
[1] 80943     7
```

---

## Create training, test and validation sets


```r
inBuild <- createDataPartition(y = dfAll$class, p = 0.7, list = FALSE)
training <- dfAll[inBuild,]
testing <- dfAll[-inBuild,]
```


```r
dim(training)
```

```
[1] 56662     7
```

```r
dim(testing)
```

```
[1] 24281     7
```

```r
table(training$class)
```

```

    1     2     3     5     6     7 
 4753 21626 14866  8093  3535  3789 
```

---

## Model using imbalanced dataset


```r
training_ub <- training[sample(1:nrow(training), 2400), ]
table(training_ub$class)
```

```

  1   2   3   5   6   7 
211 900 611 357 149 172 
```

```r
mod1_ub <- train(as.factor(class) ~ B3 + B4 + B5, method = "rf", data = training_ub)
```

```
note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .
```

```r
mod1_ub$results[, 1:2]
```

```
  mtry  Accuracy
1    2 0.9782595
2    3 0.9764377
```

---

## Function for creating a balanced dataset by undersampling


```r
undersample_ds <- function(x, classCol, nsamples_class){
  for (i in 1:length(unique(x[, classCol]))){
    class.i <- unique(x[, classCol])[i]
    if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
      x <- x[-sample(which(x[, classCol] == class.i), 
                     sum(x[, classCol] == class.i) - nsamples_class), ]
      }
  }
  return(x)
}
```

---

## Balance training dataset


```r
(nsamples_class <- 400) 
```

```
[1] 400
```

```r
training_bc <- undersample_ds(training, "class", nsamples_class)
table(training_bc$class)
```

```

  1   2   3   5   6   7 
400 400 400 400 400 400 
```

---

## Model using balanced dataset


```r
mod1_bc <- train(as.factor(class) ~ B3 + B4 + B5, method = "rf", data = training_bc)
```

```
note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .
```

```r
mod1_bc$results[, 1:2]
```

```
  mtry  Accuracy
1    2 0.9797371
2    3 0.9766507
```

---
## Predict with imbalanced model on testing set


```r
pred1_ub <- predict(mod1_ub, testing)
confusionMatrix(pred1_ub, testing$class)$overall[1]
```

```
Accuracy 
0.984597 
```

```r
confusionMatrix(pred1_ub, testing$class)$byClass[, 1]
```

```
 Class: 1  Class: 2  Class: 3  Class: 5  Class: 6  Class: 7 
0.9951644 0.9793201 0.9806938 0.9945213 1.0000000 0.9809816 
```

---
## Predict with balanced model on testing set


```r
pred1_bc <- predict(mod1_bc, testing)
confusionMatrix(pred1_bc, testing$class)$overall[1]
```

```
 Accuracy 
0.9788312 
```

```r
confusionMatrix(pred1_bc, testing$class)$byClass[, 1]
```

```
 Class: 1  Class: 2  Class: 3  Class: 5  Class: 6  Class: 7 
0.9941973 0.9662191 0.9759849 0.9904844 1.0000000 0.9975460 
```

---
## Further resources

* For a detailed explanation please see:
  * This [post in my blog](http://amsantac.co/blog/en/2016/09/18/balanced-image-classification-r.html)
  * And this [video on my YouTube channel](https://www.youtube.com/watch?v=EbbSY6EJ4js)  
* Also check out these useful resources:
  * [Practical guide to deal with imbalanced classification problems in R](https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/)
  * [8 tactics to combat imbalanced classes in your machine learning dataset](http://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/)
