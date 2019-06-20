c_v = function(df, cur_p, neurons, folds) {
  foldID = rep(1:folds, length.out = dim(df)[1])
  foldID = sample(foldID)
  err = numeric(length(neurons))
  for(m in 1:length(neurons)) {
    print(m)
    neuron = neurons[m]
    nn = names(df[, -1])
    f = as.formula(paste("y~", paste(nn[!nn %in% "y"], collapse = " + ")))
    # of inputs
    input = length(nn)
    para = (input  + 1)* neuron + neuron + 1 
    error = numeric()
    for(u in 1:folds) {
      train = df[which(foldID != u), ]
      test = df[which(foldID == u),]
      #FIX
      fit = neuralnet(f, data = train, hidden = neuron,
                      stepmax = 1e+08,
                      act.fct = 'logistic', 
                      err.fct = 'sse', algorithm = "rprop+",
                      lifesign = 'none', learningrate.factor = list(minus = 0.5, plus = 1.2),
                      linear.output = T, likelihood = F)
      yhat = neuralnet::compute(fit,test[,-1])
      error[u] = mean((yhat$net.result - test[,'y'])^2)
    }
    err[m] = mean(error)
  }
  return(err)
}


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
folds = 5
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

#Here Sys.getenv('PBS_ARRAYID') gives 1,2,...,23. 
#I used this for cluster computation which submits 23 jobs in parallel
user_i = as.numeric(Sys.getenv('PBS_ARRAYID'))
MSE_testing = data.frame(matrix(ncol = length(lags), nrow = length(neurons)))
for(i in 1:length(lags)){
  MSE_testing[i,] = c_v(df = model_df(cur_user = order_user[user_i], cur_p = lags[i]) , cur_p = lags[i], 
                        neurons = neurons, folds = folds)  
}

  
save(MSE_testing, file = paste('user_',user_i,'.rda',sep=''))
