library(broom)
library(car)
library(ggfortify)
library(sjPlot)
library(survminer)
library(tidyverse)

# Loading data
file_name <- "/home/andrew/Desktop/path_548L/Diabetes Full.csv"
df <- read.csv(file_name)
head(df)

# Cleaning data
df$Random.Blood.Glucose.Binary <- factor(
  df$Random.Blood.Glucose.Binary,
  levels=c("Low", "High")
)
df$Random.Blood.Glucose.Ordinal <- factor(
  df$Random.Blood.Glucose.Ordinal,
  levels=c("Low", "Medium", "High")
)
df$Sex <- factor(df$Sex)

# Fitting the model
model <- lm(Random.Blood.Glucose.mg.dL ~ BMI, df)
summary(model)
confint(model)

# Plotting the results from broom
augment(model)

## Residuals
ggplot(augment(model), aes(x=.resid)) +
  geom_histogram()

## True vs predicted values
ggplot(augment(model), aes(x=Random.Blood.Glucose.mg.dL, y=.fitted)) +
  geom_point()

## Fitted model
ggplot(augment(model), aes(x=BMI, y=Random.Blood.Glucose.mg.dL)) +
  geom_point() +
  geom_abline(
    slope=model$coefficients["BMI"],
    intercept=model$coefficients["(Intercept)"],
    color="Red"
    )

## Fitted model using ggplot
ggplot(df, aes(x=Age, y=Random.Blood.Glucose.mg.dL)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  xlim(0, 100) +
  ylim(0, 100) +
  theme_bw()

## Various statistics from ggfortify
autoplot(model)

# Multivariable modelling
model <- lm(Random.Blood.Glucose.mg.dL ~ Age + HDL, df)
summary(model)
anova(model)

## All variables
df_no_cat <- subset(
  df, 
  select=-c(Random.Blood.Glucose.Binary, Random.Blood.Glucose.Ordinal)
)
model <- lm(Random.Blood.Glucose.mg.dL ~ ., df_no_cat)
summary(model)
autoplot(model)
# From car
vif(model)

# Logistic regression
model <- glm(
  Random.Blood.Glucose.Binary ~ BMI, 
  family="binomial",
  data=df
)
summary(model)
autoplot(model)

df_bin <- subset(
  df, 
  select=-c(Random.Blood.Glucose.mg.dL, Random.Blood.Glucose.Ordinal)
)
model <- glm(
  Random.Blood.Glucose.Binary ~ ., 
  family="binomial",
  data=df_bin
)
summary(model)
plot_model(model)
vif(model)

model <- glm(
  Random.Blood.Glucose.Binary ~ ., 
  family="binomial",
  data=subset(df_bin, select=-c(Total.Cholesterol))
)
vif(model)

model <- glm(
  Random.Blood.Glucose.Binary ~ ., 
  family="binomial",
  data=subset(df_bin, select=-c(Total.Cholesterol, TCH))
)
vif(model)
# plot_model from sjPlot
plot_model(model) + xlab("Covariate") + theme_bw()
summary(model)

model <- glm(
  Random.Blood.Glucose.Binary ~ Age + BMI + HDL, 
  family="binomial",
  data=df
)
summary(model)

model <- glm(
  Random.Blood.Glucose.Binary ~ Age + BMI + HDL + Age * HDL, 
  family="binomial",
  data=df
)
summary(model)

# Multinomial logisitic regression
library(nnet)
model <- multinom(Random.Blood.Glucose.Binary ~ BMI, data=df)
summary(model)

model <- multinom(Random.Blood.Glucose.Binary ~ Age + BMI, data=df)
summary(model)

library(VGAM)

model <- vglm(
  Random.Blood.Glucose.Binary ~ Age + BMI, 
  data=df,
  family="multinomial"
) 
summary(fit)
plot_model(model)

# Lasso
#> One common approach to finding features is "lasso" regression
library(glmnet)
x <- as.matrix(
  subset(
    df, 
    select=-c(
      Random.Blood.Glucose.mg.dL,
      Random.Blood.Glucose.Binary,
      Random.Blood.Glucose.Ordinal, 
      Total.Cholesterol
      )
    )
  )
y <- df$Random.Blood.Glucose.Binary
cv.lasso <- cv.glmnet(x, y, alpha=1, family="binomial")
model <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
coef(model)

summary(df)
