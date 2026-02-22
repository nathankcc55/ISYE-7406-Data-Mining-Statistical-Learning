## Below assume that you save the datasets in the folder ``C://Temp" in your laptop
## 1. Read Training data
ziptrain <- read.table(file="zip.train.csv", sep = ",");
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7);
## some sample Exploratory Data Analysis
dim(ziptrain27); ## 1376 257
sum(ziptrain27[,1] == 2);
sum(ziptrain27[,1] == 7);
summary(ziptrain27);
round(cor(ziptrain27),2);
## To see the letter picture of the 5-th row by changing the row observation to a matrix
rowindex = 18; ## You can try other "rowindex" values to see other rows
ziptrain27[rowindex,1];
Xval = t(matrix(data.matrix(ziptrain27[,-1])[rowindex,],byrow=TRUE,16,16)[16:1,]);
image(Xval,col=gray(0:1),axes=FALSE) ## Also try "col=gray(0:32/32)"
### 2. Build Classification Rules
### linear Regression
mod1 <- lm( V1 ~ . , data= ziptrain27);
pred1.train <- predict.lm(mod1, ziptrain27[,-1]);
y1pred.train <- 2 + 5*(pred1.train >= 4.5);
## Note that we predict Y1 to $2$ and $7$,
## depending on the indicator variable whether pred1.train >= 4.5 = (2+7)/2.
mean( y1pred.train != ziptrain27[,1]);
## KNN
library(class);
kk <- 7; # set to various levels of k. Start with 1.
xnew <- ziptrain27[,-1];
ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=kk);
mean( ypred2.train != ziptrain27[,1])
### 3. Testing Error
### read testing data

ziptest <- read.table(file="zip.test.csv", sep = ",");
ziptest27 <- subset(ziptest, ziptest[,1]==2 | ziptest[,1]==7);
dim(ziptest27) ##345 257
model1pred <- ziptest27[,1]
pred1.test <- predict.lm(mod1, ziptest27[,-1]);
y1pred.test <- 2 + 5*(pred1.test >= 4.5);
mean(y1pred.test != ziptest27[,1]);
## Testing error of KNN, and you can change the k values.
xnew2 <- ziptest27[,-1]; ## xnew2 is the X variables of the "testing" data
kk <- 1; ## below we use the training data "ziptrain27" to predict xnew2 via KNN
ypred2.test <- knn(ziptrain27[,-1], xnew2, ziptrain27[,1], k=kk);
mean( ypred2.test != ziptest27[,1]) ## Here "ziptest27[,1]" is the Y response of the "testing" data
### 4. Cross-Validation
### The following R code might be useful, but you need to modify it.
zip27full = rbind(ziptrain27, ziptest27) ### combine to a full data set
n1 = 1376; # training set sample size
n2= 345; # testing set sample size
n = dim(zip27full)[1]; ## the total sample size
set.seed(7406); ### set the seed for randomization
### Initialize the TE values for all models in all $B=100$ loops
B= 100; ### number of loops
TEALL = NULL; ### Final TE values

kk <- c(1, 3, 5, 7, 9, 11, 13, 15)  # Define KNN neighbors

for (b in 1:B) {
  te <- NULL
  flag <- sort(sample(1:n, n1)) 
  zip27traintempset <- zip27full[flag, ]
  zip27testtempset <- zip27full[-flag, ]
  
  # Linear regression
  modLinear <- lm(V1 ~ ., data = zip27traintempset)
  predLTest <- predict(modLinear, zip27testtempset[,-1])
  ypredLTest <- 2 + 5 * (predLTest >= 4.5)
  tempLte <- mean(ypredLTest != zip27testtempset[,1])
  te <- c(te, tempLte)
  
  # KNN Models
  for (k in kk) {
    ypredKTest <- knn(zip27traintempset[,-1], zip27testtempset[,-1], zip27traintempset[,1], k = k)
    tempKte <- mean(ypredKTest != zip27testtempset[,1])  # Misclassification rate
    te <- c(te, tempKte)
  }
  
  TEALL <- rbind(TEALL, te)
}

colnames(TEALL) <- c("linearRegression", paste0("KNN", kk))

head(TEALL, 4)
summary(TEALL)

var(TEALL[, "linearRegression"])
var(TEALL[, "KNN1"])
var(TEALL[, "KNN3"])
var(TEALL[, "KNN5"])
var(TEALL[, "KNN7"])
var(TEALL[, "KNN9"])
var(TEALL[, "KNN11"])
var(TEALL[, "KNN13"])
var(TEALL[, "KNN15"])

library(ggplot2)
library(reshape2)

rowindex = 9
Xval = t(matrix(data.matrix(ziptrain27[,-1])[rowindex,], byrow = TRUE, 16, 16)[16:1,])

image(Xval, col = gray(0:1), axes = FALSE)