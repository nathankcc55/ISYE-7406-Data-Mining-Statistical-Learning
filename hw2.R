# Setup

fat <- read.table(file = "fat.csv", sep=',', header=TRUE)
n = dim(fat)[1]
n1 = round(n/10)
flag = c(1, 21, 22, 57, 70, 88, 91, 94, 121, 127, 149, 151, 159, 162,
         164, 177, 179, 194, 206, 214, 215, 221, 240, 241, 243);
fat1train = fat[-flag,]; fat1test = fat[flag,]

# EDA
summary(fat1train)
boxplot(fat1train,
        main = "Boxplot of All Variables",
        col = rainbow(ncol(fat1train)),
        las = 1,
        cex.axis = 0.7,
        horizontal = TRUE)
hist(fat1train$height, 
     main = "Distribution of Height", 
     xlab = "Height (inches)", 
     col = "lightblue", 
     border = "black", 
     breaks = 30)
hist(fat1train$weight, 
     main = "Distribution of Height", 
     xlab = "Height (inches)", 
     col = "lightblue", 
     border = "black", 
     breaks = 30)
hist(fat1train$age, 
     main = "Distribution of Age", 
     xlab = "Years", 
     col = "lightblue", 
     border = "black", 
     breaks = 30)
boxplot(fat1train$abdom,
        main = "Boxplot of Abdominal Circumference",
        ylab = "Abdominal Circumference (cm)",
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE)
cor(fat1train)

# Model 1. Linear Regression
model1 <- lm(brozek ~ ., data=fat1train)
summary(model1)

# VIF for EDA
library(car)
vif(model1)

# Model 1 Testing Error
model1_pred <- predict(model1, newdata=fat1test[,-1])
model1_te <- mean((model1_pred - fat1test[,1])^2)
print(model1_te)

# Model 2. Linear Regression using k = 5 predictors variables
library(leaps)
five_subset <- regsubsets(brozek ~., data=fat1train, nvmax=5)
subset_summary <- summary(five_subset)
print(subset_summary$which)
model2 <- lm(brozek ~ siri + density + thigh + knee + wrist, data=fat1train)
summary(model2)

# Model 2 Testing Error
model2_pred <- predict(model2, newdata=fat1test[,-1])
model2_te <- mean((model2_pred - fat1test[,1])^2)
print(model2_te)

# Model 3. Linear regression with variables (stepwise) selected using AIC;
model3 <- step(model1, direction="both")
summary(model3)

# Model 3 Testing Error
model3_pred <- predict(model3, newdata=fat1test[,-1])
model3_te <- mean((model3_pred - fat1test[,1])^2)
print(model3_te)

# Model 4. Ridge Regression
library(MASS)
model4 <- lm.ridge(brozek ~., data=fat1train, lambda = seq(0, 100, 0.001))
plot(model4)
matplot(model4$lambda, t(model4$coef), type = "l", lty = 1, 
        xlab = expression(lambda), ylab = expression(hat(beta)))
indexopt <- which.min(model4$GCV)
ridge_coeff <- model4$coef[, indexopt] / model4$scales
intercept <- -sum(ridge_coeff * colMeans(fat1train[, -1])) + mean(fat1train$brozek)
ridge_coeffs <- c(intercept, ridge_coeff)

# Model 4 Testing Error
model4_pred <- as.matrix(fat1test[, -1]) %*% as.vector(ridge_coeff) + intercept
model4_te <- mean((model4_pred - fat1test[, 1])^2)
print(model4_te)

# Model 5. LASSO
library(lars)
fat_lasso <- lars(as.matrix(fat1train[,-1]), fat1train$brozek, type="lasso", trace=TRUE)
plot(fat_lasso)
Cp_lasso <- summary(fat_lasso)$Cp
index_lasso <- which.min(Cp_lasso)
lasso_lambda <- fat_lasso$lambda[index_lasso]
coef_lasso <- predict(fat_lasso, s = lasso_lambda, type = "coef", mode = "lambda")$coef
lasso_intercept <- mean(fat1train$brozek) - sum(coef_lasso * colMeans(fat1train[, -1]))
lasso_coeffs <- c(lasso_intercept, coef_lasso)

# Model 5 Testing Error
model5_pred <- predict(fat_lasso, as.matrix(fat1test[, -1]), s = lasso_lambda, type = "fit", mode = "lambda")
model5_te <- mean((model5_pred$fit - fat1test[,1])^2)
print(model5_te)

# Model 6. PCR
library(pls)
model6 <- pcr(brozek ~., data=fat1train, scale=TRUE, validation="CV")
summary(model6)
validationplot(model6, val.type = "MSEP")
opt_pcs_pcr <- which.min(model6$validation$adj)

# Model 6 Testing Error
model6_pred <- predict(model6, newdata=fat1test[-1], ncomp=opt_pcs_pcr)
model6_te <- mean((model6_pred - fat1test[,1])^2)
print(model6_te)

# Model 7. Partial Least Squares
model7 <- plsr(brozek ~., data=fat1train, validation="CV")
summary(model7)
opt_pcs_pls <- which.min(model7$validation$adj)

# Model 7 Testing Error
model7_pred <- predict(model7, newdata=fat1test[,-1], ncomp=opt_pcs_pls)
model7_te <- mean((model7_pred - fat1test[,1])^2)
print(model7_te)

# All TE's
model_names <- c("LinReg", "LinRegk=5", "Stepwise", "Ridge", "LASSO", "PCR", "PLS")
te_vals <- c(model1_te, model2_te, model3_te, model4_te, model5_te, model6_te, model7_te)
names(te_vals) <- model_names
print(te_vals)

# MCCV
set.seed(123)
B = 100
TEALL = NULL
library(glmnet)
model1_te_mccv <- numeric(B)
for (i in 1:B) {
  flag <- sort(sample(1:n, n1));
  fattraintemp <- fat[-flag,]
  fattesttemp <- fat[flag,]
  
  # Model 1
  model1 <- lm(brozek ~ ., data=fattraintemp)
  model1_pred <- predict(model1, newdata=fattesttemp[,-1])
  te0 <- mean((model1_pred - fattesttemp[,1])^2)
  
  # Model 2
  five_subset <- regsubsets(brozek ~ ., data = fattraintemp, nvmax = 5)
  selected_vars <- names(which(summary(five_subset)$which[5,]))
  model2 <- lm(as.formula(paste("brozek ~", paste(selected_vars[-1], collapse = "+"))), data = fattraintemp)
  model2_pred <- predict(model2, newdata = fattesttemp)
  te1 <- mean((model2_pred - fattesttemp$brozek)^2)
  
  # Model 3
  model3 <- step(model1, direction="both")
  model3_pred <- predict(model3, newdata=fattesttemp[,-1])
  te2 <- mean((model3_pred - fattesttemp[,1])^2)
  
  # Model 4
  model4 <- lm.ridge(brozek ~., data=fattraintemp, lambda = seq(0, 100, 0.001))
  indexopt <- which.min(model4$GCV)
  ridge_coeff <- model4$coef[, indexopt] / model4$scales
  intercept <- -sum(ridge_coeff * colMeans(fattraintemp[, -1])) + mean(fattraintemp$brozek)
  ridge_coeffs <- c(intercept, ridge_coeff)
  model4_pred <- as.matrix(fattesttemp[, -1]) %*% as.vector(ridge_coeff) + intercept
  te3 <- mean((model4_pred - fattesttemp[, 1])^2)
  
  # Model 5
  lasso_model <- cv.glmnet(as.matrix(fattraintemp[, -1]), fattraintemp$brozek, alpha = 1)
  model5_pred <- predict(lasso_model, s = "lambda.min", newx = as.matrix(fattesttemp[, -1]))
  te4 <- mean((model5_pred - fattesttemp[, 1])^2)
  
  # Model 6
  model6 <- pcr(brozek ~., data=fattraintemp, scale=TRUE, validation="CV")
  opt_pcs_pcr <- which.min(model6$validation$adj)
  model6_pred <- predict(model6, newdata=fattesttemp[-1], ncomp=opt_pcs_pcr)
  te5 <- mean((model6_pred - fattesttemp[,1])^2)
  
  # Model 7
  model7 <- plsr(brozek ~., data=fattraintemp, validation="CV")
  opt_pcs_pls <- which.min(model7$validation$adj)
  model7_pred <- predict(model7, newdata=fattesttemp[,-1], ncomp=opt_pcs_pls)
  te6 <- mean((model7_pred - fattesttemp[,1])^2)
  
  TEALL = rbind(TEALL, cbind(te0, te1, te2, te3, te4, te5, te6))
}
apply(TEALL, 2, mean)
apply(TEALL, 2, var)
