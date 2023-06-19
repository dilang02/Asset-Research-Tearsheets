graphics.off()
rm(list=ls()) # Reset work environment
library(yahoofinancer) # Call packages for financial data API, forecasting, and statistical testing
library(forecast)
library(zoo)
library(lubridate)
library(urca)
product_list <- c("GBP=X","EUR=X","JPY=X","AUD=X","CNY=X") # Adjustable list of equity products
for (i in 1:length(product_list)){ # Iterate through all products
  symbol <- product_list[i]
  product <- Ticker$new(symbol)
  data_mo <- product$get_history(start = Sys.Date()-years(5), end = Sys.Date(), interval = '1mo') # Monthly data from past 5 years
  data_d <- product$get_history(start = Sys.Date()-years(2), end = Sys.Date(), interval = '1d') # Daily data from past 2 years
  data_mo_ts <- ts(data_mo$adj_close,frequency=12)
  data_d_ts <- ts(data_d$adj_close,frequency=1)
  plot(decompose(data_mo_ts))
  plot(ur.df(data_mo_ts,type = "drift",selectlags = "AIC"))
  plot(ur.df(data_d_ts,type = "drift",selectlags = "AIC"))
  
  MA <- rollmean(data_d_ts,k=30,fill=NA,align="right") # Create moving average and ARIMA forecasts
  plot(data_d_ts,main="Moving Average Forecast")
  lines(MA,col="red")
  arima <- auto.arima(data_d_ts,ic="aic")
  #show(summary(arima))
  #show(Box.test(residuals(arima),type="Ljung-Box"))
  plot(forecast(arima,h=100))
  
  ytd_data <- product$get_history(start = as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01")), end = Sys.Date(), interval = '1d')
  m1_data <- product$get_history(start = Sys.Date()-months(1), end = Sys.Date(), interval = '1mo')
  m3_data <- product$get_history(start = Sys.Date()-months(3), end = Sys.Date(), interval = '1mo')
  y1_data <- product$get_history(start = Sys.Date()-years(1), end = Sys.Date(), interval = '1mo')
  y3_data <- product$get_history(start = Sys.Date()-years(3), end = Sys.Date(), interval = '1mo')
  y5_data <- product$get_history(start = Sys.Date()-years(5), end = Sys.Date(), interval = '1mo')
  y10_data <- product$get_history(start = Sys.Date()-years(10), end = Sys.Date(), interval = '1mo')
  p <- function(x){
    perf <- (tail(x$adj_close,1)-head(x$adj_close,1))/head(x$adj_close,1) * 100
    return(perf)
  }
  perf_data <- c(p(ytd_data),p(m1_data),p(m3_data),p(y1_data),p(y3_data),p(y5_data),p(y10_data))
  perf_labels <- c("YTD","1 Month","3 Months","1 Year","3 Years","5 Years","10 Years")
  colors <- ifelse(perf_data >= 0, "green", "red")
  barplot(perf_data,main="USD Exchange Rate Fluctuation Over Time",names.arg=perf_labels,col=colors,las=2)
  
  if (p(ytd_data) <= 0){
    ERF <- paste0('This year, the US dollar has depreciated against ',substr(symbol,1,3),' by ',abs(round(p(ytd_data),2)),'%')
    print(ERF)
  } else{
    ERF <- paste0('This year, the US dollar has apppreciated against ',substr(symbol,1,3),' by ',abs(round(p(ytd_data),2)),'%')
    print(ERF)
  }
  
  library(knitr)
  library(rmarkdown)
  tearsheet <- paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/RMD & HTML Files/",substr(symbol,1,3),"-USD.Tearsheet.rmd") # Create tearsheet template
  content <- "
# FOREX Tearsheet: `r symbol`
The following data is for the `r product$quote$shortName`:

# Currency Performance
`r ERF`

```{r,fig.width=6,fig.height=4,echo=FALSE}
barplot(perf_data,main='USD Exchange Rate Fluctuation Over Time',names.arg=perf_labels,col=colors,las=2)
plot(decompose(data_mo_ts))
plot(ur.df(data_mo_ts,type = 'drift',selectlags = 'AIC'))
plot(ur.df(data_d_ts,type = 'drift',selectlags = 'AIC'))
plot(data_d_ts,main='Moving Average Forecast')
lines(MA,col='red')
plot(forecast(arima,h=100))
```

This is a test document for asset research purposes.
"
  library(pagedown) # Export markdown file to HTML & PDF
  writeLines(content,tearsheet)
  chrome_print(input = tearsheet, output = paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/PDF Files/",substr(symbol,1,3),"-USD.Tearsheet.pdf"))
}
