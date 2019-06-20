library(glmnet)
library(vars)
library(plyr)
library(zoo)
library(tibble)
library(readr)
library(randomForest)
load("TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")

#### Order of users
intern_data = full_2015_data[full_2015_data$days_intern >= 0, ]
intern_data$mood_na = is.na(intern_data$mood)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$userid), FUN = sum )
order_user = mood_na_counts$Group.1[order(mood_na_counts$x)]
#lags
lags = c(1,3,5,10,15)
#cluster computing
cur_p = lags[as.numeric(Sys.getenv('PBS_ARRAYID'))]
#next week
cur_d = 7

### A function gives model matrix
model_df = function(cur_user) {
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
  new_y = change_y(cur_df)
  return(cbind(new_y,cur_df[seq_along(new_y),-1]))
}

#give new y
change_y = function(df) {
  #Regular y
    return(df$y)
  #predict next week
    #return(rollapply(df$y, cur_d, mean))
  
  #predict the difference
    #return((df$y - df$mood.l1))
}

#lasso, test function
cv_test = function(df) {
  folds = 5
  foldID = rep(1:folds, length.out = dim(df)[1])
  foldID = sample(foldID)
  err = numeric()
  fit_df = lapply(1:folds, function(u) cv.glmnet(x = as.matrix(df[foldID != u,-1]), y = df[foldID != u,'new_y']))
  yhat_df = lapply(1:folds,function(u) predict(fit_df[[u]], newx = as.matrix(df[foldID == u,-1]), s = 'lambda.min'))
  err = sapply(1:folds, function(u) sum((yhat_df[[u]] - df[foldID == u,'new_y'])^2))
  mse = sum(err)/dim(df)[1]
  R2 = 1- sum(err)/sum((df[,'new_y'] - mean(df[,'new_y']))^2)
  return(c(mse, R2))
}

separate = function(df){
  foldID = rep(1:folds, length.out = dim(df)[1])
  foldID = sample(foldID)
  return(foldID)
}
