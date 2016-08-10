library(lawstat,  quietly  =  TRUE)  ;  library(forecast,  quietly  =  TRUE)    

################################################################################
#                               DATA EXPLORATION
################################################################################

# Train/Test split
train  <-  read.csv("data/train.csv")
test  <-  read.csv("data/test.csv")
val  <-  train[277:288,]
train  <-  train[1:276,]

# Loading variables and converting them into time series objects
Bankruptcy_Rate_train  <-  ts(train$Bankruptcy_Rate)
House_Price_Index_train  <-  ts(train$House_Price_Index)
Unemployment_Rate_train  <-  ts(train$Unemployment_Rate)
Population_train  <-  ts(train$Population)

# Determine number of times differencing is required.
ndiffs(Bankruptcy_Rate_train,  test  =  "adf")
nsdiffs(Bankruptcy_Rate_train,  m  =  12)

# Exploratory plots on the training data
par(mfrow  =  c(4,  1))
plot(Bankruptcy_Rate_train,  ylab  =  "Bankruptcy Rate")
plot(Unemployment_Rate_train,  ylab  =  "Unemployment Rate")
plot(Population_train,  ylab  =  "Population")
plot(House_Price_Index_train,  ylab  =  "House Price Index")

# We find hetroscedaticity, fixed with log
# Non stationary time series, fixed with one difference
par(mfrow  =  c(2,  1))
plot(Bankruptcy_Rate_train,  main  =  "Original Time Series", 
     ylab  =  "log(Bankruptcy Rate)",  col  =  "light blue")
plot(log(Bankruptcy_Rate_train),  main  =  "Log Transformed Time Series", 
     ylab  =  "Bankruptcy Rate",  col  =  "blue")

# Ordinary differencing
par(mfrow  =  c(2,  1))
plot(log(Bankruptcy_Rate_train), ylab = "log(Bankruptcy Rate)", 
     main  =  "Log Transformed Time Series",  col  =  "light blue")
plot(diff(log(Bankruptcy_Rate_train)), ylab = "diff(log(Bankruptcy Rate))", 
     main  =  "Log Transformed and Difference Time Series",  col  =  "blue")
abline(h  =  mean(diff(log(Bankruptcy_Rate_train))),  col  =  "red")

# t-test to verify the Stationarity of the time series (Mean  =  0)
t.test(diff(log(Bankruptcy_Rate_train)))

# ADF test to check for the stationarity of the time series
library(tseries)
adf.test(log(Bankruptcy_Rate_train))
cat("Thus the ADF test confirms that the above time series is not stationary")

# Intial Guess of p, q, P, Q
log.Bankruptcy_Rate_train.1  <-  diff(log(Bankruptcy_Rate_train))
par(mfrow  =  c(2,  1))
acf(log.Bankruptcy_Rate_train.1,  lag.max  =  48
,  main  =  "Log Transformed and Differenced Bankruptcy Rate")
pacf(log.Bankruptcy_Rate_train.1,  lag.max  =  48,  main  =  "")

# ADF test to check for the stationarity of the time series
adf.test(log.Bankruptcy_Rate_train.1)
cat("Thus the ADF test confirms that the above time series stationary")

################################################################################
#                               MODEL SELECTION
################################################################################
# Fit an ARIMA(1,1,0) model
# q = 2
# p = 2
# Q = 2 (possibly 1)
# P = 1

# Grouping for the Levene test (4 * 69)
group  <-  c(rep(1,69), rep(2,69), rep(3,69), rep(4,69))

# A dataframe to store all metrics of all possible models
df  <-  data.frame(orders  =  character(),  sigma_2  =  numeric(), 
                 loglik  =  numeric(),  pval  =  numeric(), 
                 rmse_val  =  numeric(),  rmse_com  =  numeric())

# Iterate
for  (p  in  seq(0,  2)){
  for  (q  in  seq(0,  2)){
    for  (P  in  seq(0,  1)){
      for  (Q  in  seq(0,  2)){
        model  <-  arima(log(Bankruptcy_Rate_train), 
                       order  =  c(p,  1,  q), 
                       seasonal  =  list(order  =  c(P,  0,  Q),  period  =  12), 
                       method  =  "CSS", 
                       xreg  =  data.frame(House_Price_Index_train))
        
        pval  <-  round(levene.test(model$residuals,group)$p,  3)
        # RMSE in Training set    
        f  <-  forecast(model,  h  =  12,  level  =  0.95,  xreg  =  val$House_Price_Index)
        rmse_c  <-  sqrt(mean((Bankruptcy_Rate_train  -  exp(f$fitted))  ^  2))
        # RMSE in validation set         
        f  <-  predict(model,  n.ahead  =  12,  newxreg  =  val$House_Price_Index)
        rmse_v  <-  sqrt(mean((val$Bankruptcy_Rate  -  exp(f$pred))  ^  2))

        df  <-  rbind(df, data.frame(orders =  paste(p,1,q,P,0,Q,  sep  =  ","), 
                                   sigma_2  =  model$sigma2, 
                                   loglik  =  model$loglik, 
                                   pval  =  pval,
                                   rmse_val  =  rmse_v,
                                   rmse_comp  =  rmse_c))
      }
    } 
  }
}

# Best Value
df
which.max(df$loglik)
which.min(df$sigma_2)
which.min(df$rmse_val)
df[order(df$rmse_val),  ]

# Our Chosen Model
model1  <-  arima(log(Bankruptcy_Rate_train),  order  =  c(2,  1,  0)
                  ,  seasonal  =  list(order  =  c(1,  0,  2),  period  =  12)
                  ,  method  =  "CSS",  xreg  =  data.frame(House_Price_Index_train))
model1
tsdiag(model1,  gof.lag  =  48) #ACF and Ljung-Box test all in one!

# Next Best Model
model2  <-  arima(log(Bankruptcy_Rate_train),  order  =  c(2,  1,  0)
                  ,  seasonal  =  list(order  =  c(0,  0,  2),  period  =  12)
                  ,  method  =  "CSS",  xreg  =  data.frame(House_Price_Index_train))
model2

# Calculating Deviance for the 2 models
D  <-  -2  *  (model2$loglik  -  model1$loglik)
pval  <-  1  -  pchisq(D,  1)
cat("Test Statistic:",D,"P-value:",pval)


################################################################################
#                               RESIDUAL DIAGNOSTICS
################################################################################
model  <-  arima(log(Bankruptcy_Rate_train),  order  =  c(2,  1,  0), 
             seasonal  =  list(order  =  c(0,  0,  2),  period  =  12),
             method  =  "ML",  xreg  =  data.frame(House_Price_Index_train))
model

# Assumption:1 The residuls should have a mean of Zero
par(mfrow  =  c(1,  1))
cat("Null Hypothesis (Ho): The residuals have an expected value of Zero")
cat("Alternate Hypothesis (Ha): The residuals do not have an expected value of Zero")
e  <-  model$residuals
t.test(e)
plot(e,  main  =  "Residuals vs t",  ylab  =  "",  col  =  "blue")
abline(h  =  0, col  =  "red")
cat("As the 95% range contains 0, we reject Ho that the residuals have
    an expected value of Zero \n")
cat("However, we also see some seasonal trend in the pattern of residuals")

# Assumption:2 The residuls should have no heteroskedasticity
cat("Null Hypothesis (Ho): The residuals have no heteroskedasticity")
cat("Alternate Hypothesis (Ha): The residuals do have heteroskedasticity")
group <- c(rep(1,69), rep(2,69), rep(3,69), rep(4,69))
levene.test(e,  group) #Levene
bartlett.test(e,  group) #Bartlett   
cat("Given the above tests & plots, We fail to reject the Null hypothesis that there 
    is no heteroskedasticity is the distribution\n")

# Assumption3: The residuals belong to a normal distribution
cat("Null Hypothesis (Ho): The residuals come from a distribution that is Normal")
cat("Alternate Hypothesis (Ha): The residuals do not come from a distribution 
    that is Normal")
qqnorm(e,  main  =  "QQ-plot of Residuals",  col  =  "light blue",  pch  =  15)
qqline(e,  col  =  "red",  lty  =  2,  lwd  =  2)
shapiro.test(e) #SW test
cat("Given the above tests & plots, We fail to reject the Null hypothesis that 
    the distribution is non-normal \n")

# Assumption4: The residuals have no auto-correlation with each other
cat("Null Hypothesis (Ho): The residuals have no significant auto-correlation 
    with each other")
cat("Alternate Hypothesis (Ha): The residuals have significant auto-correlation 
    with each other")
tsdiag(model) #ACF and Ljung-Box test all in one!
runs.test(e) #Runs test for randomness
cat("Given the high p-value, We fail to reject the Null hypothesis that 
    the distribution has no auto-correlation \n")


################################################################################
#                               FORECASTING
################################################################################

model  <-  arima(log(Bankruptcy_Rate_train),  order  =  c(2,  1,  0), 
             seasonal  =  list(order  =  c(0,  0,  2),  period  =  12),
             method  =  "ML",  xreg  =  data.frame(House_Price_Index_train))

f  <-  forecast(model,  h  =  12,  level  =  0.95,  xreg  =  val$House_Price_Index)
f_pred  <-  predict(model,  n.ahead  =  12,  newxreg  =  val$House_Price_Index)
rmse  <-  sqrt(mean((val$Bankruptcy_Rate  -  exp(f_pred$pred))  ^  2))


pred  <-  f_pred$pred
l  <-  f$lower #95% PI LL
h  <-  f$upper #95% PI UL
par(mfrow  =  c(1,  1))
plot(Bankruptcy_Rate_train,  xlim  =  c(0,  300),  ylim  =  c(0,  0.05)
     ,  ylab = "Bankruptcy Rate")
abline(v  =  276,  lwd  =  2,  col  =  "black")
abline(v  =  288,  lwd  =  2,  col  =  "black")
points(277  :  288,  exp(pred),  type  = "l",  col  =  "blue")
points(277  :  288,  exp(l),  type  =  "l", col  =  "red")
points(277  :  288,  exp(h),  type  =  "l", col  =  "red")
points(1  :  276,  exp(f$fitted),  type  =  "l",  col  =  "green")
points(277  :  288,  val$Bankruptcy_Rate,  type = "l",  col  =  "black")

model_full  <-  arima(log(c(Bankruptcy_Rate_train,  val$Bankruptcy_Rate)), 
                   order  =  c(2,  1,  0), 
                   seasonal  =  list(order  =  c(0,  0,  2),  period  =  12),
                   method  =  "ML", 
                   xreg  =  data.frame(c(House_Price_Index_train, 
                                       val$House_Price_Index)))
model_full
f  <-  forecast(model_full,  h  =  12,  level  =  0.95,  xreg  =  test$House_Price_Index)
f_pred  <-  predict(model_full,  n.ahead  =  12,  newxreg  =  test$House_Price_Index)

pred  <-  f_pred$pred
l  <-  f$lower #95% PI LL
h  <-  f$upper #95% PI UL
points(289  :  300,  exp(pred),  type  =  "l",  col  =  "blue")
points(289  :  300,  exp(l),  type  =  "l",  col  =  "red")
points(289  :  300,  exp(h),  type  =  "l",  col  =  "red")

output  <-  data.frame(exp(pred),  exp(h),  exp(l))
names(output)  <-  c("mean","Upper","Lower")
write.csv(output,  "test2.csv")

