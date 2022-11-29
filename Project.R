#######################################
#######################################
## Bike Sharing Data Analysis #########
#######################################
#######################################

# Library Imports
install.packages('corrplot')
library(corrplot)
library(faraway)
library(MASS)
library(mgcv)
library(lmtest)
library(car)

## Data Exploration
data = read.csv("day.csv")
length(data)

str(data)

# Check missing data
colSums(is.na(data))


## Data Transformation

# remove unnecessary variables - registered and cnt
data_filt = data[, -c(15,16)]

# Change categories to more meaningful labels
data_filt$season <- factor(format(data_filt$season, format="%A"),
                      levels = c("1", "2","3","4") , 
                      labels = c("Springer","Summer","Fall","Winter"))

data_filt$holiday <- factor(format(data_filt$holiday, format="%A"),
                           levels = c("0", "1") , labels = c("Working Day","Holiday"))

data_filt$weathersit <- factor(format(data_filt$weathersit, format="%A"),
                              levels = c("1", "2","3","4") , 
                              labels = c("Good","Moderate","Bad","Worse"))

data_filt$yr <- factor(format(data_filt$yr, format="%A"),
                      levels = c("0", "1") , labels = c("2011","2012"))


# Add columns with De Normalized value
data_filt$actual_temp <- data_filt$temp*41
data_filt$actual_feel_temp <- data_filt$atemp*50
data_filt$actual_windspeed <- data_filt$windspeed*67
data_filt$actual_humidity <- data_filt$hum*100

# Convert Categorical data to columns
data_filt$mnth = as.factor(data_filt$mnth)
data_filt$weekday = as.factor(data_filt$weekday)
data_filt$workingday = as.factor(data_filt$workingday)
data_filt$weathersit = as.factor(data_filt$weathersit)

# View final DataFrame and its datatypes
str(data_filt)

## EDA

# Stats of variable
stats = function(var_)
{
  print(paste("Mean: ", mean(var_)))
  print(paste("Variance: ", var(var_)))
  print(paste("S.D.: ", sd(var_)))
  print(paste("Median: ", median(var_)))
  print(paste("Max Value: ", max(var_)))
  print(paste("Min Value: ", min(var_)))
}

stats(data_filt$actual_temp)
stats(data_filt$actual_feel_temp)
stats(data_filt$actual_humidity)
stats(data_filt$actual_windspeed)
stats(data_filt$casual)

# Analyse response variable
hist(data_filt$casual,
     main="Distribution for casual",
     xlab="Casual",
     ylab = "Distribution")

boxplot(data_filt$casual,
        main="Distribution for casual",
        xlab="Casual",
        ylab = "Distribution")


# list of different categorical / numerical variables
cat_var_list = c(3,4,5,6,7,8,9)
num_var_list = c(10,11,12,13)
target_var = c(14)

# Categorical Variable Analysis
par(mfcol=c(2,2))

boxplot(data_filt$casual ~ data_filt$season,
        data = data_filt,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 


boxplot(data_filt$casual ~ data_filt$holiday,
        data = data_filt,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

boxplot(data_filt$casual ~ data_filt$weathersit,
        data = data_filt,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation", 
        ylab = "Total Bike Rentals",
        col = c("purple", "purple1", "purple2", "purple3")) 


plot(data_filt$casual ~ data_filt$yr,type = "p",
     main = "Total Bike Rentals Vs Year",
     xlab = "Year",
     ylab = "Total Bike Rentals",
     col  = "orange",
     pch  = 19)


# Function - Plot scatter plot of categorical variables vs casual
plot_cat = function(data, c, label){
  cat_ = unique(c)
  max_val = max(c)
  min_val = min(c)
  len = length(cat_)
  par(mfrow=c(ceiling(len/2), 2), oma=c(0,0,2,0))
  for (x in 1:len){
    data_filt = subset(data, c==cat_[x])
    x_val = seq(1, nrow(data_filt), 1)
    plot(x_val, data_filt$casual,
         xlab="Index", ylab="Casual",
         main=paste("Category = ", cat_[x]),
         ylim=c(min_val, max_val + 10))
  }
  mtext(paste("Categorical Analysis of ", label), side = 3, line = 0, outer = TRUE, cex=1.5)
}


# plot_cat(data_filt, data_filt$season, "Season")
# plot_cat(data_filt, data_filt$yr, "Year")
# plot_cat(data_filt, data_filt$holiday, "Holidays")
# plot_cat(data_filt, data_filt$weekday, "Weekday")
# plot_cat(data_filt, data_filt$workingday, "Working Day")
# plot_cat(data_filt, data_filt$weathersit, "Weather Situation")
# 

# Continuous Variable Analysis
par(mfcol=c(2,2))

hist(data_filt$temp,
     main="Distribution for Temperature",
     xlab="Temperature",
     ylab = "Distribution")

hist(data_filt$atemp,
     main="Distribution for Temperature (Feels-like)",
     xlab="Temperature",
     ylab = "Distribution")

hist(data_filt$hum,
     main="Distribution for Humidity",
     xlab="Temperature",
     ylab = "Distribution")

hist(data_filt$windspeed,
     main="Distribution for Wind Speed",
     xlab="Temperature",
     ylab = "Distribution")

# pairplot
par(mfcol=c(1,1))
pairs(data_filt[append(num_var_list, target_var)],
      main="Pair Plot for Numerical Variables",
)

# Correlation Analysis
par(mfcol=c(1,1))
corr_ = cor(data_filt[num_var_list])
corrplot(corr_, method="color")

# Remove index, dates and atemp
data_filt = data_filt[, -c(1, 2, 11)]
str(data_filt)

## Function for Further Use

get_model_assumptions = function(model){
  par(mfrow=c(1, 2))
  plot(fitted(model), resid(model), col = "grey", pch = 20,
       xlab = "Fitted", ylab = "Residuals", main = "Residual Plot")
  
  abline(h = 0, col = "darkorange", lwd = 2)
  
  bp_val = bptest(model)
  print(paste("BP Test p-value: ", bp_val$p.value))
  
  qqnorm(resid(model))
  qqline(resid(model), col = "dodgerblue", lwd = 2)
  
  shapiro_val = shapiro.test(resid(model))
  print(paste("Shapiro Test p-value: ", shapiro_val$p.value))
  
}

get_influential_points = function(model, data, rem=FALSE){
  data_inf = data
  dist = cooks.distance(model)
  inf_index = which(dist > 4/length(dist))
  print(paste("Found ",length(inf_index)," influential points."))
  if(rem){
    data_inf = data[-inf_index,]
  }
  return(list("index"=inf_index, "data"=data_inf))
}

# Questions for Analysis

# 1. Is temperature significant predictor for casual users?

plot(casual~temp, data=data_filt)
lr_temp = lm(casual ~ temp, data=data_filt)
summary(lr_temp)
get_model_assumptions(lr_temp)

# # Correcting model assumptions
# # Y Transformation
# par(mfcol=c(1,1))
# boxcox(lr_temp, plotit=TRUE, lambda = seq(0, 2, by = 0.1))
# lambda = 0.25
# lr_temp_tr = lm(((casual^(lambda)-1)/(lambda)) ~ log(temp) , data=data_filt)
# summary(lr_temp_tr)
# get_model_assumptions(lr_temp_tr)
# 
# # Influential Points
# inf_list = get_influential_points(lr_temp, data_filt, rem=TRUE)
# lr_temp_tr = lm(((casual^(lambda)-1)/(lambda)) ~ temp , data=inf_list$data)
# summary(lr_temp_tr)
# get_model_assumptions(lr_temp_tr)

# 2. Does season influence number of casual users? If yes, which two seasons show significant difference in average casual users?
season_model = aov(casual~season, data=data_filt)
summary(season_model)

# Which two seasons give us the difference in mean?
TukeyHSD(season_model)

# 3. What is the effect of weekday after adjusting for temperature?

## Check assumptions for ANCOVA test
# 1. Linearity amongst weekday and temperature
model = aov(temp ~ weekday, data=data_filt)
summary(model)

# 2. Check variance amongst weekday and temperature
leveneTest(temp ~ weekday, data = data_filt)

# Running ANCOVA test
model = lm(casual ~ weekday + temp, data=data_filt)
Anova(model, type=3)

############## END OF DESCRIPTIVE #######################

# Modelling Analysis
fit_null = lm(casual~1, data=data_filt)
fit_step_aic = step(fit_null, scope=casual~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed, direction="both")
summary(fit_step_aic)

get_model_assumptions(fit_step_aic)

# Check for reasons for assumption violation

## Remove influential points
inf_ob = get_influential_points(fit_step_aic, data_filt, rem=TRUE)
data_inf_rem = inf_ob$data

fit_inf = lm(casual~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+hum+windspeed,data=data_inf_rem)
summary(fit_inf)

get_model_assumptions(fit_inf)

## Remove working day because of NA
fit_final = lm(casual~season+yr+mnth+holiday+weekday+weathersit+temp+hum+windspeed,data=data_filt)
summary(fit_final)

## Transform Y
#### Specify the range of lambda
par(mfcol=c(1,1))
boxcox(fit_step_aic, lambda = seq(0, 0.5, by = 0.05))

lambda = 0.25
fit_tr_y <- lm(((casual^(lambda)-1)/(lambda))~season+yr+mnth+holiday+weekday+weathersit+temp+hum+windspeed,data=data_filt)
summary(fit_tr_y)

get_model_assumptions(fit_tr_y)

## Transform X
################## TO DO ############

# Multi-collinearity Check
vif(fit_tr_y)

# Splines
lambda = 0.25
fit_spline <- gam(((casual^(lambda)-1)/(lambda))~season+yr+holiday+weekday+workingday+weathersit+s(temp)+s(hum)+s(windspeed),data=data_filt)
summary(fit_spline)
get_model_assumptions(fit_spline)

## Cross Validation Method


## Random Forest