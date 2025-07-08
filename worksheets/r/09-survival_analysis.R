library(ggplot2)
library(ggfortify)
library(ranger)
library(survminer)
library(survival)


df <- veteran
km <- with(df, Surv(time, status))
km

## Univariate survival

# First let's look at the overall survival curve
km_fit <- survfit(Surv(time, status) ~ 1, data=df)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit) + 
  xlab("Time") + 
  ylab("Survival") + 
  theme_bw()

# Now lets consider by treatment group
km_trt_fit <- survfit(Surv(time, status) ~ trt, data=df)
autoplot(km_trt_fit) + 
  xlab("Time") + 
  ylab("Survival") + 
  guides(fill=FALSE) +
  labs(color = "Treatment") +
  theme_bw()

# We can also binarize on age
df$age_grp <- factor(ifelse(df$age < 60, "LT60", "OV60"))
km_AG_fit <- survfit(Surv(time, status) ~ age_grp, data=df)
autoplot(km_AG_fit) + 
  xlab("Time") + 
  ylab("Survival") + 
  guides(fill=FALSE) +
  labs(color = "Age group") +
  theme_bw()

## Multivariate survival

# We can do a Cox proportional hazard analysis
cox <- coxph(
  Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , 
  data=df
  )
summary(cox)

# We can also get the survival function
cox_fit <- survfit(cox)
autoplot(cox_fit) +
  xlab("Time") + 
  ylab("Survival")

# Finally we can viusalize the coefficients
ggforest(cox, data=df)

# The Cox model assumes the covariates are time independent
# We can consider an alternative model the Aalen model
aa_fit <-aareg(
  Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , 
  data=df
  )
summary(aa_fit)
autoplot(aa_fit)
