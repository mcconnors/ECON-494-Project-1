####### PROJECT II - INTRO TO BUSINESS ANALYTICS #######
# IMPORTING CLEAN DATASET FROM PROJECT I - UNEMPLOYMENT AND RELATED FACTORS BY STATE
df<-read.csv('/Users/madonnaconnors/Desktop/ACADEMICS/FALL\ 2020/INTRO\ TO\ BUS.\ ANALYTICS/IBA\ PROJECT\ II/IBA\ Project\ II\ Dataset.csv')
View(df)

#loading up ggplot2
library(ggplot2)
library(plyr)
library(tseries)
.libPaths()
install.packages("zoo")
library("zoo")
install.packages("quantmod")

# NONLINEAR (POLYNOMIAL) TRANSFORMATIONS 
df$AVG.RESIDENT.POPULATION2<-df$AVG.RESIDENT.POPULATION^2 #Quadratic tranfsormation (2nd Order)
df$AVG.RESIDENT.POPULATION3<-df$AVG.RESIDENT.POPULATION^3 #Quadratic transformation (3rd order)

df$ln_AVG.RESIDENT.POPULATION<-log(df$AVG.RESIDENT.POPULATION) #Logarithmic Transformation
View(df)

p<-.7 #percentage of data that will be used to train the model
obs_count<-dim(df)[1] # number of rows in the dataframe

training_size <- floor(p * obs_count) # number of obs for training data partition and rounding down
training_size #print
set.seed(1234) #goal is to make partition reproducible
train_ind <- sample(obs_count, size = training_size) #creating vector
Training <- df[train_ind, ] #pulling rows at random for training
Testing <- df[-train_ind, ] #pulling rows at random for testing

#checking dimensions for each partition - training/tesing
dim(Training)
dim(Testing)

### BUILDING MODEL - TRAINING DATA ### 
M1 <- lm(AVG..UNEMPLOYMENT.RATE ~ AVG.RESIDENT.POPULATION, Training) #from training data
summary(M1) #summary diagnostic output

M1$coefficients #Beta estimates, intercept and slope
M1$residuals #return residuals
M1$fitted.values
hist(M1$residuals) #histogram of residual values
jarque.bera.test(M1$residuals)

##from project 1
ggplot(df, aes(x = AVG.RESIDENT.POPULATION, y = AVG..UNEMPLOYMENT.RATE)) +
  geom_point() +
  geom_smooth(method = 'lm')
#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values

#PREDICTIONS ON THE TEST DATA - BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #for predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_1_IN))  #in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_1_OUT)) #out-of-sample error

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(AVG.RESIDENT.POPULATION=x_grid))
plot(Training$AVG..UNEMPLOYMENT.RATE ~ Training$AVG.RESIDENT.POPULATION, col='purple')
lines(x_grid, predictions, col='green',lwd=3)
points(Testing$AVG..UNEMPLOYMENT.RATE ~ Testing$AVG.RESIDENT.POPULATION, col='orange', pch=3)

#BUILDING QUADRATIC MODEL (TRAINING)
M2 <- lm(AVG..UNEMPLOYMENT.RATE ~ AVG.RESIDENT.POPULATION + AVG.RESIDENT.POPULATION2, Training)
summary(M2)

M2$coefficients #Beta estimates, intercept and slope
M2$residuals #return residuals
M2$fitted.values
hist(M2$residuals) #histogram of residual values
jarque.bera.test(M2$residuals)

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(AVG.RESIDENT.POPULATION=x_grid, AVG.RESIDENT.POPULATION2=x_grid^2))
plot(Training$AVG..UNEMPLOYMENT.RATE ~ Training$AVG.RESIDENT.POPULATION, col='red')
lines(x_grid, predictions, col='blue', lwd=3)
points(Testing$AVG..UNEMPLOYMENT.RATE ~ Testing$AVG.RESIDENT.POPULATION, col='green', pch=3)

##LINEAR REGRESSION WITH ALL VARIABLES IN DATASET AS REGRESSORS
M3 <- lm(AVG..UNEMPLOYMENT.RATE ~ AVG.RESIDENT.POPULATION + AVG..PER.CAPITA.PERSONAL.INCOME + BACHELOR.S.DEGREE.OR.HIGHER, df)
summary(M3)
M3$coefficients #Beta estimates, intercept and slope
M3$residuals #return residuals
M3$fitted.values
hist(M3$residuals) #histogram of residual values
jarque.bera.test(M3$residuals) #p-value is greater than 0.05, fail to reject and conclude that data is normal

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR


##LOGARITHMIC MODEL
M4 <- lm(AVG..UNEMPLOYMENT.RATE ~ ln_AVG.RESIDENT.POPULATION, Training)
summary(M4)

M4$coefficients #Beta estimates, intercept and slope
M4$residuals #return residuals
M4$fitted.values
hist(M4$residuals) #histogram of residual values
jarque.bera.test(M4$residuals) #p-value is greater than 0.05, fail to reject and conclude that data is normal

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$AVG..UNEMPLOYMENT.RATE)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M4, list(ln_AVG.RESIDENT.POPULATION=log(x_grid)))
plot(Training$AVG..UNEMPLOYMENT.RATE ~ Training$AVG.RESIDENT.POPULATION, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$AVG..UNEMPLOYMENT.RATE ~ Testing$AVG.RESIDENT.POPULATION, col='red', pch=3)


### COMPARING THE MODELS ###

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH MULTIPLE INDEPENDENT VARS.
RMSE_4_IN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH MULT. INDEPENDENT VARS.
RMSE_4_OUT #LOGARITHMIC MODEL

###PLOTTING REGRESSION AGAINST ONE ANOTHER###

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$AVG..UNEMPLOYMENT.RATE ~ Training$AVG.RESIDENT.POPULATION, col='blue')
predictions_1 <- predict(M1, list(AVG.RESIDENT.POPULATION=x_grid))
predictions_2 <- predict(M2, list(AVG.RESIDENT.POPULATION=x_grid, AVG.RESIDENT.POPULATION2=x_grid^2))
predictions_3 <- predict(M3, list(AVG.RESIDENT.POPULATION=x_grid, AVG..PER.CAPITA.PERSONAL.INCOME=x_grid^2, BACHELOR.S.DEGREE.OR.HIGHER=x_grid^3))
predictions_4 <- predict(M4, list(ln_AVG.RESIDENT.POPULATION=log(x_grid)))
lines(x_grid, predictions_1, col='purple', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='blue', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='green', lwd=3) #PLOTS M3
lines(x_grid, predictions_4, col='red', lwd=3) #PLOTS M4
points(Testing$AVG..UNEMPLOYMENT.RATE ~ Testing$AVG.RESIDENT.POPULATION, col='red', pch=3)
