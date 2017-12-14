library(TSA)
library(quantmod)
library(ggplot2)
########

#get financial data using quantmod
getSymbols('AAPL')
getSymbols('FB')

getSymbols('EA')
getSymbols('ATVI')

#standardize time so that we are looking at similar windows (since 2015)
AAPL = AAPL[c('2015','2016','2017')]
FB = FB[c('2015','2016','2017')]

EA = EA[c('2013','2014','2015','2016','2017')]
ATVI = ATVI[c('2013','2014','2015','2016','2017')]

#plot adjusted close for each Apple and Facebook
plot_stock_movement = function(series1, series2){
  
  mainplot = ggplot() + theme_classic() + theme(legend.position=c(.15,.95)) +
    geom_line(data=series1, aes( x=time(series1), y=series1[,6], color='ATVI' )) + 
    geom_line(data=series2, aes( x=time(series2), y=series2[,6], color='EA' )) +
    labs(title='Adjusted Close Prices', x='Time', y='Adj. Close') +
    scale_colour_manual("", values = c('ATVI'='tomato1', 'EA'='deepskyblue1')) 
  
  return(mainplot)
  
}


#make series stationary requires 1st order differencing

plot_correlograms = function(x, conflevel=.95, lagmax=20, title='Correlograms'){
  # Function creates a prettier ACF/PACF using ggplot2
  # Args: x (xts) --------- your data. Must be data suitable for acf() and pacf(). Quantmod getSymbols() uses xts object.
  #       conflevel (numeric) -- confidence level for interval bands
  #       lagmax (numeric) ------ maximum number of lags to plot
  #       title (string) -------- title of plot
  # Return: ACF/PACF as a ggplot object
  
  CIline = qnorm((1 - conflevel)/2)/sqrt(length(x))
  
  
  tmpacf=acf(x, lag.max=lagmax, plot=FALSE, na.action=na.omit)
  tmppacf=pacf(x, lag.max=lagmax, plot=FALSE, na.action=na.omit)
  pltdf= data.frame(lag=rep(tmpacf$lag,2),vals=c(tmpacf$acf,tmppacf$acf),type=c( rep('ACF',lagmax), rep('PACF',lagmax) ))
  
  siglag = ( abs(pltdf[,2]) > abs(CIline) )
  for(i in 1:length(siglag)){
    if(siglag[i]==TRUE){
      siglag[i]='red'
    }else{
      siglag[i]='black'
    }
  }
  pltdf = cbind(pltdf, siglag)
  
  plt = ggplot(data=pltdf, mapping=aes( x=lag, y=vals )) + theme_classic() +
    geom_hline(yintercept=0) +
    geom_segment(mapping=aes( xend = lag, yend = 0 ), color=siglag) +
    geom_hline(yintercept=CIline, color='blue') + geom_hline(yintercept=-CIline, color='blue') +
    facet_grid(type ~ .) +
    labs(title=title, y='', x='Lag')
  
  return(plt)
  
}

plot_correlograms(diff(FB$FB.Adjusted), conflevel=.95, lagmax=50,title='Correlograms')



#Apple
ggplot(data=AAPL, aes( time(AAPL), (diff(AAPL$AAPL.Adjusted)) )) + geom_line() + labs(title='1st Order Differencing', x='Time Index', y='Difference')

plot_correlograms(diff(AAPL$AAPL.Adjusted), conflevel=.95, lagmax=50,title='Correlograms')

#FB
ggplot(data=FB, aes( time(FB), (diff(FB$FB.Adjusted)) )) + geom_line() + labs(title='1st Order Differencing', x='Time Index', y='Difference')

plot_correlograms(diff(FB$FB.Adjusted), conflevel=.95, lagmax=50,title='Correlograms')


###think about splitting into 2 different rmkd files?

#calc daily return ratio from lognormal dist
daily_return_ratio = function(x){
  # Calculates the daily return ratios
  # Args: x (xts vector) -- your data
  # Returns: a vector of ratios r (length = dim(x)[1] - 1)
  
  r = NULL
  for(i in 2:length(x)){
    ratio = coredata(x)[i]/coredata(x)[i-1]
    r = c(r,ratio)
  }
  return(r)
}

apple_ratios = daily_return_ratio(AAPL[,6])
fb_ratios = daily_return_ratio(FB[,6])

#consider changing colors to beautify plots?
ggplot() +
  geom_histogram(aes(apple_ratios),binwidth=.005) +
  labs(title='Distribution of Apple Daily Return Ratios', y='Frequency', x='Daily Return Ratio')
ggplot() + 
  geom_histogram(aes(fb_ratios),binwidth=.005) +
  labs(title='Distribution of Facebook Daily Return Ratios', y='Frequency', x='Daily Return Ratio')

#Estimate params w/ MLE
library(MASS)

#simulated forecast for fb
#random walk uses current time*logN to predict next time

random_walk_sim = function(data, window, nahead){
  # Forecasts stock prices using a lognormal random walk
  # Args: data (xts vector) ---- your training data
  #       window (numeric) ----- number of trading days to use for MLE of lognormal params
  #       nahead (numeric) ----- number of days to forecast
  # Returns: xts vector of predicted stock prices of length nahead
  
  nTrain = length(data)
  forecasts = NULL
  times = NULL
  dataTrain = data
 
  for(i in 0:nahead){

    dataTrain_fc = dataTrain[(nTrain-window+i):(nTrain+i)]
    
    lnorm.params = fitdistr(daily_return_ratio(dataTrain_fc), 'lognormal')
    forecast = tail(dataTrain_fc,n=1) * rlnorm(n=1, meanlog=lnorm.params$estimate[1], sdlog=lnorm.params$estimate[2])
    
    forecasts = c(forecasts, forecast)
    times = c(times, max(index(dataTrain))+1)
    dataTrain = c(dataTrain, xts( as.numeric(forecast),( max(time(dataTrain))+1 ) ) )
    
  }
  
  return(dataTrain[(nTrain+1):(nTrain+nahead)])
  
}


#plotting

plot_forecasts = function(data, window, nahead, train_on=.8, nsims){
  # Plots simulated forecasts
  # Args: data (xts) ---------- your data
  #       window (numeric) ---- number of trading days to use for MLE of lognormal params
  #       nahead (numeric) ---- number of days to forecast
  #       train_on (numeric) -- % of days to use (from start of series) for training
  #       nsims (numeric) ----- number of simulated forecasts to plot
  # Returns: ggplot2 plot of simulated forecasts
  # implement optional training w/ missing()
  
  training = data[1:round(train_on*dim(data)[1]),6]
        
  forecastsims = data.frame( replicate(nsims,numeric(nahead)) )
  for(simnum in colnames(forecastsims)){
    
    forecastsims[simnum] = random_walk_sim(training, window, nahead)
    
  }
  
  forecastsims = data.frame(forecastsims,time=time(forecastsims[,1]))
  
  simforecastPlot = ggplot() + theme_classic()+ geom_line(data=data, aes( x=time(data), y=data[,6] ), color='lightblue') +
    labs(title='Simulated LogNormal Random Walk Forecasts', x='Time', y='Adj. Close')
  
  for(simnum in colnames(forecastsims[-(nsims+1)])){
    print(simnum)
    simforecastPlot = simforecastPlot + geom_line(data=forecastsims, aes_string(x='time', y=simnum), color='pink', alpha=.7)
    
  }
  
  return(simforecastPlot)

}


plot_forecasts(data=FB, window=100, nahead=200, nsims=20)



#calc k-step ahead expected value, variance
#plot expected forecast with confidence bands


get_expval = function(mu, sigma){
  return( exp(mu + .5 * sigma**2) )
}

get_var = function(mu, sigma){
  return( exp(2*mu - sigma**2) * (exp(sigma**2) - 1) )
}

get_nahead_variance = function(mu, sigma, yt, n){
  # Calculates n-step ahead expected forecast
  # Args: mu ------ parameter mu
  #       sigma --- parameter sigma
  #       yt ------ price of stock at time t
  #       n ------- number of steps ahead
  # Returns: variance
  
  return( yt**2 * (get_var(mu,sigma) + get_expval(mu,sigma)**2)**n - get_expval(mu,sigma)**(2*n) )
}


expected_randwalk = function(data, window_size, nahead){
  # Steps through desired forecast range calculating expected price and variance of that estimate
  # Args: data --------- time series
  #       window_size -- size of window for MLE of lognormal params
  #       nahead ------- number of days to forecast
  # Returns: data frame of estimates. Col1 = ExpVal, Col2 = Var
  
  exp_X = NULL
  var_X = NULL
  window = tail(data[,6], n=window_size)
  last_price = tail(window, n=nahead)[1]
  yt = last_price
  
  for(step in 0:nahead){
    mu = fitdistr( daily_return_ratio(window),'lognormal' )$estimate[1]
    sigma = fitdistr( daily_return_ratio(window),'lognormal' )$estimate[2]
    exp_X[step] = last_price * get_expval(mu, sigma)
    var_X[step] = get_nahead_variance(mu, sigma, last_price, n=step)
    last_price = last_price * get_expval(mu, sigma)
  }
  
  return(cbind(exp_X,sqrt(var_X)))
  
}

plot_exp_forecast = function(data, window, nahead, error=2){
  
  mu = expected_randwalk(data, window, nahead)[,1]
  var = expected_randwalk(data, window, nahead)[,2]
  CI_ll = mu - (error*sqrt(var))
  CI_ul = mu + (error*sqrt(var))

  expforecastPlot = ggplot() + theme_classic() + 
    geom_line(data=data, aes( x=time(data), y=data[,6] ), color='lightblue') +
    labs(title='Simulated LogNormal Random Walk Forecasts', x='Time', y='Adj. Close') +
    geom_line(aes(x=time(tail(FB,nahead)), y=mu), color='red') +
    geom_ribbon(aes(ymin=CI_ll, ymax=CI_ul, x=time(tail(FB,nahead)), fill='band'), alpha=.3)
  
  return(expforecastPlot)
  
  
}
