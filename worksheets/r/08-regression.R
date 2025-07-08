library(broom)
library(mice)
library(tidyverse)

df <- airquality
imp <- mice(df, m=5, method="pmm")
df_imp <- complete(imp)

## Univariate linear regression
model <- lm(Ozone ~ Solar.R, df_imp)
model$coefficients
summary(model)

model_df <- augment(model)
# Residuals
ggplot(model_df, aes(x=.resid)) + geom_histogram()
# Predicted vs true
ggplot(model_df, aes(x=.fitted, y=Ozone)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0, color="blue", size=1)
# Manually plotting the fitted model
ggplot(df_imp, aes(x=Solar.R, y=Ozone)) + 
  geom_point() + 
  geom_abline(
    slope=model$coefficients["Solar.R"], 
    intercept=model$coefficients["(Intercept)"],
    color="Red"
  )
# Using ggplot to do fitting and plot with confidence intervals
ggplot(df_imp, aes(x=Solar.R, y=Ozone)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

## Multivariate regression
#> Two variables
model <- lm(Ozone ~ Solar.R + Wind, df_imp)
summary(model)
#> Three variables
model <- lm(Ozone ~ Solar.R + Wind + Temp, df_imp)
summary(model)
#> All variables we use a .
model <- lm(Ozone ~ ., df_imp)
summary(model)
#> Note: Using day and month encoded as numbers is not really appropriate
df_imp$Day <- factor(df_imp$Day)
df_imp$Month <- factor(df_imp$Month)
model <- lm(Ozone ~ ., df_imp)
summary(model)
#> Let's get rid of day
model <- lm(Ozone ~ ., subset(df_imp, select=-c(Day)))
summary(model)
#> Interactions
model <- lm(Ozone ~ Solar.R + Wind + Temp + Solar.R * Temp, df_imp)
summary(model)


## Logistic regression
#> Let's clean up the workspace before we continue
rm(list=ls())
#> Data from https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset
file.name <- "/home/andrew/Desktop/path_548L/breast-cancer.csv"
df <- read.csv(file.name)
df$diagnosis <- factor(df$diagnosis)

## Univariate logistic regression
model <- glm(diagnosis ~ radius_mean, family="binomial", data=df)
summary(model)

## Multivariate
model <- glm(diagnosis ~ radius_mean + texture_worst, family="binomial", data=df)
summary(model)
#> Let's try everything except id
model <- glm(diagnosis ~ ., family="binomial", data=subset(df, select=-c(id)))
summary(model)
anova(model, test="Chisq")
#> Complaining about not converging. Likely because we have highly correlated variables
#> Let's focus on the mean values
df_mean <- select(df, contains("mean"))
df_mean$diagnosis <- df$diagnosis
model <- glm(diagnosis ~ ., family="binomial", data=df_mean)
summary(model)
#> Let's looks at the fitted values
ggplot(augment(model), aes(x=.fitted)) + geom_histogram()
#> We can predict! 
#> Here we use the training data but in principle new data could be used
pred <- predict(model, df_mean)
pred <- factor(ifelse(pred > 0.5, "M", "B"))
sum(df_mean$diagnosis == pred) / nrow(df_mean)

## Model selection
#> A lot of non-significant features
#> Lets remove compactness_mean
model <- glm(diagnosis ~ ., family="binomial", data=subset(df_mean, select=-c(compactness_mean)))
summary(model)
#> Let's try keeping the four significant features
feats <- c("texture_mean", "area_mean", "smoothness_mean", "concave.points_mean", "diagnosis")
model <- glm(diagnosis ~ ., family="binomial", data=subset(df_mean, select=feats))
summary(model)

#> One common approach to finding features is "lasso" regression
library(glmnet)
x <- as.matrix(subset(df, select=-c(diagnosis)))
y <- df$diagnosis
cv.lasso <- cv.glmnet(x, y, alpha=1, family="binomial")
model <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
coef(model)