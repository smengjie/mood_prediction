
load('testmse.rda')
library(glmnet)
library(vars)
library(plyr)
library(zoo)
library(tibble)
library(readr)
library(neuralnet)
load("TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")

#### Order of users
intern_data = full_2015_data[full_2015_data$days_intern >= 0, ]
intern_data$mood_na = is.na(intern_data$mood)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$userid), FUN = sum )
order_user = mood_na_counts$Group.1[order(mood_na_counts$x)]
#lags
lags = c(1,3,5,10,15)
neurons = c(1,5,10,15,20)


### A function gives model matrix
model_df = function(cur_user, cur_p) {
  cur_data = full_2015_data[full_2015_data$userid == cur_user, ]
  #mood
  x1 = ts(cur_data[cur_data$days_intern >= 0, 'mood'])
  x1.5 = ts(is.na(x1))
  #sleep
  sleep_vars = c('TotalMinutesAsleep', 'TotalTimeInBed', 'TotalSleepRecords')
  x2 = ts(cur_data[cur_data$days_intern >= 0, sleep_vars], start = 0)
  x2[,c(1,2)] = x2[,c(1,2)]/60  ## Turns it to minutes
  x2.5 = ts(is.na(x2[ ,'TotalMinutesAsleep']), start = 0)
  x_new = ts.intersect(x1,x1.5, x2, x2.5)
  #distance
  dist_vars = c('TotalDistance', 'VeryActiveDistance', 'ModeratelyActiveDistance', 'LightActiveDistance')
  x3 = sqrt(ts(cur_data[cur_data$days_intern >= 0, dist_vars], start = 0))
  x3.5 = ts(is.na(x3[ ,'TotalDistance']), start = 0)
  #new time series data
  x_new = ts.intersect(x_new, x3, x3.5)
  cur_ts = x_new
  colnames(cur_ts) = c('mood', 'mood_missing', sleep_vars, 'sleep_missing', dist_vars, 'distance_missing')
  
  ## mean imputation
  for(i in colnames(cur_ts)){
    cur_ts[is.na(cur_ts[,i]), i] = mean(cur_ts[,i], na.rm = TRUE)}
  
  cur_fit_big = VAR(cur_ts, type = 'both', p = cur_p) ## you don't need to have both days intern and trend, one or the other
  
  ## Give me a matrix which can be used for regression, with the correct number of lags
  cur_df = cur_fit_big$varresult$mood$model
  return(subset(cur_df, select=-c(const)))
}

R2 = list()


for(i in 1:23){
  cur_r2 = data.frame(matrix(ncol = length(neurons), nrow = length(lags)))
  cur_mse = MSE[[i]]
  for(j in 1:length(lags)){
    y = model_df(order_user[i], lags[j])[,1]
    cur_r2[j,] = 1 - cur_mse[j,] * length(y)/sum((y - mean(y))^2)
  }
  
  R2[[i]] = cur_r2
  
}

r2 = sapply(R2, max)
neuron = sapply(R2, function(df) which.max(apply(df,MARGIN=2,max)))
lag = sapply(R2, function(df) which.max(apply(df,MARGIN=1,max)))
lag = mapvalues(lag, 1:5, lags)
neuron = mapvalues(neuron, 1:5, neurons)

ndf = cbind(order_user,r2, lag, neuron)
write.csv(ndf, file = 'results_nn.csv')
save(ndf,file = 'results_nn.rda')
