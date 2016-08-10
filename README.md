
## Time Series - Canadian national bankruptcy rate 

Authors: [Chhavi Choudhury](https://github.com/chhavi21), [Jaime Pastor](https://github.com/jaimeps), [Abhishek Singh](https://github.com/Abhishek19895) <br />
*(Report available upon request)*

### Description

This project presents the analysis and construction of a SARIMA time series model to accurately forecast monthly national bankruptcy rates in Canada. 

### Data

The dataset consists of 4 series of monthly data from January 1987 to December 2010:
- Bankruptcy rate
- Population
- Unemployment rate
- House Price Index <br />
<p align="center">
	<img src="https://github.com/jaimeps/time-series-bankruptcy/blob/master/images/original_series.png" width = 600> <br />
</p> <br />

### Exploratory Data Analysis:

After analyzing the stationarity, seasonality, order, mean, covariates and heteroscedasticity, we found appropriate to use one ordinary difference and log transform the series.
<p align="center">
	<img src="https://github.com/jaimeps/time-series-bankruptcy/blob/master/images/log_transformed.png" width="600">
</p> <br />

### Model building process:
We used a two step process, where data for 2010 is used for parameter estimation:
<img src="https://github.com/jaimeps/time-series-bankruptcy/blob/master/images/table.png" width="600"> <br />

After careful consideration of the ACF and PACF plots, we fitted a SARIMA model. 
<p align="center">
	<img src="https://github.com/jaimeps/time-series-bankruptcy/blob/master/images/acf_pacf.png" width="600"> 
</p> <br />

Additionally, we contrasted our preliminary conclusions with an overfitting method. Multiple models were explored and the values of the log-likelihood, σ2, RMSE on the validation set as well as RMSE on the complete set were compared.

### Selected model:
Our final model is a **SARIMA (2, 1, 0) x (1, 0, 2)<sub>[12]</sub>**

### Residuals diagnostics:
Through both visual inspection and appropriate tests, we analyzed the assumptions of the model. In particular, we verified that the residuals have: (1) zero mean, (2) homoscedasticity, (3) lack of autocorrelation and (4) normally distributed.

### Forecast:
We used the model to forecast the bankruptcy rates for January 2011 to December 2011.
<p align="center">
	<img src="https://github.com/jaimeps/time-series-bankruptcy/blob/master/images/forecast.png" width="600"> 
</p> <br />

### References
- Peter Brockwell; Richard Davis - *Introduction to Time Series and Forecasting*
- Peter Brockwell; Richard Davis - *Time Series: Theory and Methods*
- Nathaniel Stevens - *Time Series Analysis: Course notes*