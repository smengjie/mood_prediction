library(glmnet)
library(vars)
library(plyr)

load("TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")
m1 = read.csv('M1.csv',row.names = 1)


#### Order of users, in the same seq as m1
intern_data = full_2015_data[full_2015_data$days_intern >= 0, ]
intern_data$mood_na = is.na(intern_data$mood)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$userid), FUN = sum )
order_user = mood_na_counts$Group.1[order(mood_na_counts$x)]
cur_p = 15

####k-means clustering results
clustering_fun = function(df) {
  nks = dim(df)[1]
  errors = rep(0, nks)
  #each row is a clustering
  clusterings = matrix(nrow = nks, ncol = nrow(df))
  for(j in 1:nks){
    set.seed(j)
    tempKmeans = kmeans(df, centers = j,nstart = 10,iter.max=100,algorithm=c("Lloyd"))
    errors[j] = tempKmeans$tot.withinss
    clusterings[j,] = tempKmeans$cluster
  }
  return(list(errors, clusterings))
} 
errors = clustering_fun(m1)[[1]]
clusterings = clustering_fun(m1)[[2]]

### A function gives model matrix for
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
  return(cur_fit_big$varresult$mood$model)
}

#list with model dfs for all users, corresponding to the same oder
new_dat_big = list()
new_dat_big = lapply(order_user,model_df)

## mse for each cluster
mse = numeric()

cv_fit = function(df) {
  folds = 5
  foldID = rep(1:folds, length.out = dim(df)[1])
  foldID = sample(foldID)
  err = numeric()
  fit_df = lapply(1:folds, function(u) cv.glmnet(x = as.matrix(df[foldID != u,-1]), y = df[foldID != u,'y']))
  yhat_df = lapply(1:folds,function(u) predict(fit_df[[u]], newx = as.matrix(df[foldID == u,-1]), s = 'lambda.min'))
      err = sapply(1:folds, function(u) sum((yhat_df[[u]] - df[foldID == u,'y'])^2))
  return(sum(err)) 
}



####
#for each number of clusters
for(i in 1:23){
  print(i)
  #show users in each cluster
  cur_users = lapply(1:i,function(j) which(clusterings[i,] == j))
  #Now I have a big matrix for each cluster
  cur_df = lapply(1:i, function(j) rbind.fill(new_dat_big[cur_users[[j]]])) ###powerful function!
  y_length = sum(sapply(cur_df, nrow))
  sse = sum(sapply(cur_df, cv_fit))
  mse[i] = sse/y_length
  print(mse[i])
}

write(as.data.frame(cbind(1:23,mse)), file = 'mse_test.csv')
