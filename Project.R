data = read.csv("day.csv")
length(data)

# remove unnecessary variables - registered and cnt
data = data[, -c(15,16)]

# Stats of target variable
stats = function(var_)
{
  print(paste("Mean: ", mean(var_)))
  print(paste("Variance: ", var(var_)))
  print(paste("S.D.: ", sd(var_)))
  print(paste("Median: ", median(var_)))
  print(paste("Max Value: ", max(var_)))
  print(paste("Min Value: ", min(var_)))
}

stats(data$casual)

# Analyse casual variable
hist(data$casual,
     main="Distribution for casual",
     xlab="Casual",
     ylab = "Distribution")

boxplot(data$casual,
        main="Distribution for casual",
        xlab="Casual",
        ylab = "Distribution")

boxplot(temp ~ season,
        data = hour,
        xlab = "Season",
        ylab = "Temperature",
        main = "Temperature by Season",
        col = "skyblue")

# Check missing data
rowSums(is.na(data))
colSums(is.na(data)) 

# Analysis of Predictor Variables
str(data)

cat_var_list = c(3,4,5,6,7,8,9)
num_var_list = c(10,11,12,13)
target_var = c(14)

# Analysis of categorical variables

plot_cat = function(c, label){
  cat_ = unique(c)
  max_val = max(data$casual)
  min_val = min(data$casual)
  len = length(cat_)
  par(mfrow=c(ceiling(len/2), 2), oma=c(0,0,2,0))
  for (x in 1:len){
    print(x)
    data_filt = subset(data, c==cat_[x])
    x_val = seq(1, nrow(data_filt), 1)
    plot(x_val, data_filt$casual,
         xlab="Index", ylab="Casual",
         main=paste("Category = ", cat_[x]),
         ylim=c(min_val, max_val + 10))
  }
  mtext(paste("Categorical Analysis of ", label), side = 3, line = 0, outer = TRUE, cex=1.5)
}


# plot_cat(data$season, "Season")
# plot_cat(data$yr, "Year")
# plot_cat(data$holiday, "Holidays")
# plot_cat(data$weekday, "Weekday")
# plot_cat(data$workingday, "Working Day")
# plot_cat(data$weathersit, "Weather Situation")
# 

hist(data$temp,
     main="Distribution for Temperature",
     xlab="Temperature",
     ylab = "Distribution")

hist(data$atemp,
     main="Distribution for Temperature (Feels-like)",
     xlab="Temperature",
     ylab = "Distribution")

hist(data$hum,
     main="Distribution for Humidity",
     xlab="Temperature",
     ylab = "Distribution")

hist(data$windspeed,
     main="Distribution for Wind Speed",
     xlab="Temperature",
     ylab = "Distribution")

# pairplot
pairs(data[append(num_var_list, target_var)],
      main="Pair Plot for Numerical Variables",
)

corr_ = cor(data[num_var_list])
install.packages('corrplot')
library(corrplot)
corrplot(corr_, method="color")

# Remove atemp
data_filt = data[, -c(1, 2, 11)]
str(data_filt)

# Convert Categorical data to columns
?as.factor
data_filt$season = as.factor(data_filt$season)
data_filt$yr = as.factor(data_filt$yr)
data_filt$mnth = as.factor(data_filt$mnth)
data_filt$holiday = as.factor(data_filt$holiday)
data_filt$weekday = as.factor(data_filt$weekday)
data_filt$workingday = as.factor(data_filt$workingday)
data_filt$weathersit = as.factor(data_filt$weathersit)

str(data_filt)

fit_null = lm(casual~1, data=data_filt)
fit_step_aic = step(fit_null, scope=casual~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed, direction="both")

plot(fitted(fit_step_aic), resid(fit_step_aic), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")

abline(h = 0, col = "darkorange", lwd = 2)

bptest(fit_step_aic) 

## Linearity violated, Equal variances violated

qqnorm(resid(fit_step_aic))
qqline(resid(fit_step_aic), col = "dodgerblue", lwd = 2)

shapiro.test(resid(fit_step_aic))

# Check for reasons for assumption violation
dist = cooks.distance(fit_step_aic)
inf_i = which(dist > 4/length(dist))
data_inf = data_filt[-inf_i,]

fit_inf = lm(casual~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,data=data_inf)
summary(fit_inf)

plot(fitted(fit_inf), resid(fit_inf), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")

abline(h = 0, col = "darkorange", lwd = 2)

bptest(fit_inf) 

## Linearity violated, Equal variances violated

qqnorm(resid(fit_inf))
qqline(resid(fit_inf), col = "dodgerblue", lwd = 2)

shapiro.test(resid(fit_inf))

library(MASS)
boxcox(fit_step_aic)

# Specify the range of lambda
boxcox(fit_step_aic, lambda = seq(0, 0.5, by = 0.05))

lambda = 0.32
fit_tr_y <- lm(((casual^(lambda)-1)/(lambda))~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,data=data_inf)
summary(fit_tr_y)

plot(fitted(fit_tr_y), resid(fit_tr_y), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")

abline(h = 0, col = "darkorange", lwd = 2)

bptest(fit_tr_y) 

## Linearity violated, Equal variances violated

qqnorm(resid(fit_tr_y))
qqline(resid(fit_tr_y), col = "dodgerblue", lwd = 2)

shapiro.test(resid(fit_tr_y))

# Multi-collinearity Check
library(faraway)
vif(fit_tr_y)

lambda = 0.32
fit_int_y <- lm(((casual^(lambda)-1)/(lambda))~season+yr+mnth+holiday+weekday+workingday+weathersit+temp*hum*windspeed,data=data_inf)
summary(fit_int_y)

plot(fitted(fit_int_y), resid(fit_int_y), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")

abline(h = 0, col = "darkorange", lwd = 2)

bptest(fit_int_y) 

## Linearity violated, Equal variances violated

qqnorm(resid(fit_int_y))
qqline(resid(fit_int_y), col = "dodgerblue", lwd = 2)

shapiro.test(resid(fit_int_y))


lambda = 0.32
fit_int_y <- lm(((casual^(lambda)-1)/(lambda))~season+yr+holiday+weekday+workingday+weathersit+temp*hum*windspeed-temp,data=data_inf)
summary(fit_int_y)

plot(fitted(fit_int_y), resid(fit_int_y), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")

abline(h = 0, col = "darkorange", lwd = 2)

bptest(fit_int_y) 

## Linearity violated, Equal variances violated

qqnorm(resid(fit_int_y))
qqline(resid(fit_int_y), col = "dodgerblue", lwd = 2)

shapiro.test(resid(fit_int_y))



get_model_assumptions = function(model){
    par(mfrow=c(1, 2))
    plot(fitted(model), resid(model), col = "grey", pch = 20,
         xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")
    
    abline(h = 0, col = "darkorange", lwd = 2)
    
    bp_val = bptest(model)
    print(paste("BP Test p-value: ", bp_val$p.value))
    
    qqnorm(resid(model))
    qqline(resid(model), col = "dodgerblue", lwd = 2)
    
    shapiro_val = shapiro.test(resid(fit_int_y))
    print(paste("Shapiro Test p-value: ", shapiro_val$p.value))
    
  }


get_influential_points = function(model, data, rem=FALSE){
  data_inf = data
  dist = cooks.distance(model)
  inf_index = which(dist > 4/length(dist))
  print(paste("Found ",length(inf_index)," influential points."))
  if(rem){
    data_inf = data[-inf_i,]
  }
  return(c(inf_index, data_inf))
}

get_influential_points(fit_int_y, data_filt)

library(mgcv)
lambda = 0.32
fit_spline <- gam(((casual^(lambda)-1)/(lambda))~season+yr+holiday+weekday+workingday+weathersit+s(temp)+s(hum)+s(windspeed),data=data_filt)
summary(fit_spline)
get_model_assumptions(fit_spline)


fit_poly <- gam(((casual^(lambda)-1)/(lambda))~season+yr+holiday+weekday+workingday+weathersit+poly(temp,3):poly(hum,3):poly(windspeed,3),data=data_filt)
summary(fit_poly)
get_model_assumptions(fit_poly)
