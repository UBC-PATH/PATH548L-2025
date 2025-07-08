library(tidyverse)

## Simple imputation
df <- tibble(airquality)
summary(df)
### Set the value of Solar.R to the mean
df$Solar.R[is.na(df$Solar.R)] <- mean(df$Solar.R, na.rm=TRUE)
summary(df)
### For discerete data it would be more appropriate to use the median or mode
df <- tibble(airquality)
df$Solar.R[is.na(df$Solar.R)] <- median(df$Solar.R, na.rm=TRUE)
summary(df)
### Remove the Ozone column
df <- subset(df, select=-c(Ozone))
summary(df)

## Multivariate imputations 
### R has lots of possible packages for imputation we will use a common one called mice
library(mice)
df <- tibble(airquality)
summary(df)
ggplot(df) + geom_histogram(aes(x=Ozone), bins=10)
imp <- mice(df, m=5, method="pmm")
df_imp <- complete(imp)
summary(df_imp)
df_plot <- data.frame(
  value=c(df$Ozone, df_imp$Ozone), 
  imputed=factor(c(rep(0, nrow(df)), rep(1, nrow(df_imp))))
)
ggplot(df_plot, aes(x=value, fill=imputed)) + geom_density(alpha=0.4)