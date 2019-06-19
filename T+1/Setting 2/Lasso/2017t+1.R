library(tidyverse) 
library(purrr)
library(dplyr)
library(vars)
library(plyr)
library('glmnet')
load('2017_data.rda')
start = as.Date('2017-07-01')
prestart = as.Date('2017-04-01')
end = as.Date("2018-06-30")
diff = end - start + 1
grid = c(10^seq(1,-3,length = 100),10^8)
num_split = 5

########## Order of users
#less to more NA
#restrict to intern period
intern_data = full_2017[full_2017$new_day >= start & full_2017$new_day <= end, ]
intern_data$mood_na = is.na(intern_data$new_daily_mood_score)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$UserID), FUN = sum )
#Drop any users that have no mood data ever.
selected_users = mood_na_counts[mood_na_counts$x != diff,]
order_user = selected_users$Group.1[order(selected_users$x)]
############


##########variable names
mood_var = 'new_daily_mood_score'
sleep_vars = c("INBED","RESTLESS","deepMinutes_new", "remMinutes_new", "lightMinutes_new", "wakeMinutes_new",
               "wakeCount_new", "lightCount_new", "deepCount_new", "remCount_new", "totalMinutes_new")  
dist_vars = c('dailyStepTotal_new', "active_minutes_new")
other = "resting_hr_new" 
############


#############imputation method on mood
#each user should have some value in mood
#right now we impute by previous value
impute_mood = function(md){
  #impute the first entry by mean if it is missing
  if(is.na(md[1])) {
    md[1] = mean(md,na.rm = TRUE)
  }
  
  for(i in 1:length(md)){
    if(is.na(md[i])){
      md[i] = md[i-1]
    }
  } 
  
  return(md)
}
##############


################function: impute predictors
#right now we take the mean of last seven days
days_imputed = 7
impute_predictors = function(pred,var_name){
  #if everything is missing, impute by the data set mean
  if(sum(!is.na(pred))== 0){
    pred = mean(intern_data[,var_name ],na.rm = T)
  }else{
    #in the first seven days, if there is missing, impute by mean
    for(j in 1:days_imputed){
      if(is.na(pred[j])){
        pred[j] = mean(pred,na.rm = T)
      }
    }
    for(k in (days_imputed+1):length(pred)){
      if(is.na(pred[k])){
        pred[k] = mean(pred[(k-days_imputed):(k-1)])
      }
    }
  }
  
  return(pred)
}
############


######## A function gives a full data without NA
impute_df = function(cur_user) {
  cur_data = intern_data[intern_data$UserID == cur_user, ]
  cur_data = cur_data[, c(mood_var,sleep_vars,dist_vars,other)]
  #imputation
  cur_data[,mood_var] = impute_mood(cur_data[,mood_var])
  for(i in c(sleep_vars,dist_vars,other)){
    cur_data[,i] = impute_predictors(cur_data[,i], i)
  }
  return(cur_data)
}  
##########


##### A function gives model matrix for lasso, maybe applicable to other algos too.
cur_p = 7
lasso_df = function(cur_data){
  #mood
  x1 = ts(cur_data[,mood_var])
  #sleep
  x2 = ts(cur_data[, sleep_vars], start = 0)
  x_new = ts.intersect(x1, x2)
  #distance
  x3 = ts(cur_data[, dist_vars], start = 0)
  #new time series data
  x_new = ts.intersect(x_new, x3)
  #other i.e. heart rate
  x4 = ts(cur_data[, other], start = 0)
  x_new = ts.intersect(x_new, x4)
  cur_ts = x_new
  colnames(cur_ts) = c('mood', sleep_vars, dist_vars, other)
  
  
  cur_fit_big = VAR(cur_ts, type = 'both', p = cur_p) ## you don't need to have both days intern and trend, one or the other
  
  ## Give me a matrix which can be used for regression, with the correct number of lags
  cur_df = cur_fit_big$varresult$mood$model
  return(cur_df)
}
###### 

####Add an indicator of whether the y is missing in the original data 
real_y = function(cur_user,cur_df){
  cur_merge = data.frame(cbind(cur_df, intern_data[intern_data$UserID == cur_user,'mood_na'][cur_df$trend]))
  colnames(cur_merge) = c(colnames(cur_merge)[-length(colnames(cur_merge))],'mood_na')
  return(cur_merge)
}
#####

##########fully imputed model matrix for each user with an indicator of whether y is missing
new_dat_full = lapply(order_user, function(user) real_y(user,lasso_df(impute_df(user))))
###############


#####function that splits the data for each user into 5 parts
splits = function(cur_dat) {
  return(split(cur_dat, cut(seq_along(1:nrow(cur_dat)), num_split, labels = FALSE)))
  
}
########


#####function to cross validation for each tuning value
######return avg cv error
lasso_model = function(cur_split,lam){
  s = c();len = c()
  for(j in 2:num_split){
    #####prepare for the testing set
    #pick one split
    #choose non-na observation
    #remove extra cols
    cur_test = cur_split[[j]]
    cur_test = cur_test[cur_test$mood_na == F,]
    cur_test[,c('mood_na','const',".id")] = NULL
    #######prepare for the training set
    cur_train = ldply(cur_split[1:(j-1)])
    #Implement if test data is not empty, and mood in training set is not constant(lasso won't run)
    #Otherwise, impute NA for error and 0 for length for test
    if((nrow(cur_test) != 0) & (sum(abs(diff(cur_train$y))) != 0)) {
      #take all previous splits
      #get rid of extra cols
      cur_train[,c('mood_na','const',".id")] = NULL
      cur_train = as.matrix(cur_train)
      #fit model
      cur_model = glmnet(cur_train[,-1],cur_train[,'y'],alpha = 1, lambda = lam)
      cur_predict =  predict(cur_model,as.matrix(cur_test[,-1]))
      s[j-1] = sum((cur_predict - cur_test$y)^2)
      len[j-1] = nrow(cur_test)
    } else{
      s[j-1] = NA
      len[j-1] = 0
    }
  }
  return(list(s,len))
}
#############

##########function for compute error for each tuning value
cv = function(cur_split){
  err = c()
  #for each tuning value
  for(k in 1:length(grid)){
    ls = lasso_model(cur_split,grid[k])
    err[k] = sum(ls[[1]], na.rm = T)/sum(ls[[2]])
  }
  return(grid[which.min(err)])
}
#################


err_df = data.frame(matrix(nrow = length(order_user), ncol = (num_split+4)))
colnames(err_df) = c('UserID', paste('cv',1:(num_split-1), sep=''), 'avg_cv', 'num', 'den','lambda')
err_df$UserID = order_user


for(i in 1:length(new_dat_full)){
  print(i)
  cur_user = order_user[i]
  cur_dat = new_dat_full[[i]]
  cur_split = splits(cur_dat)
  #if nothing to test, impute -1 for all errors and lambda
  if((sum(ldply(cur_split[-1])$mood_na == F) == 0)){
    err_df[err_df$UserID == cur_user,-1] = -1
  }else{
    lam = cv(cur_split)
    #get the best lambda
    err_df[err_df$UserID == cur_user,-1]['lambda'] = lam
    ls = lasso_model(cur_split, lam)
    ind0 = which(ls[[2]] == 0)
    indn0 = (1:(num_split-1))[!((1:(num_split-1))%in% ind0)]
    #if length is 0, impute -1; otherwise impute mean cv error for each split
    err_df[err_df$UserID == cur_user,-1][ind0] = -1
    err_df[err_df$UserID == cur_user,-1][indn0] = ls[[1]][indn0]/ls[[2]][indn0]
    if(length(ind0) != (num_split-1)){
      err_df[err_df$UserID == cur_user,-1]['avg_cv'] = sum(ls[[1]], na.rm = T)/sum(ls[[2]])
      err_df[err_df$UserID == cur_user,-1]['num'] = sum(ls[[1]], na.rm = T)
      test = ldply(cur_split[(indn0 + 1)])
      err_df[err_df$UserID == cur_user,-1]['den'] = sum((test[test$mood_na == F,'y'] - mean(test[test$mood_na == F,'y']))^2)
    }else{
      err_df[err_df$UserID == cur_user,-1]['avg_cv'] = -1
      err_df[err_df$UserID == cur_user,-1]['num'] = -1
      err_df[err_df$UserID == cur_user,-1]['den'] = -1
    }
  }
}



#############R-sqr session
#compute total rsqr
rsqrtol = rep(NA, nrow(err_df))
ind = which(err_df$num != -1)
rsqrtol[ind] = 1- err_df$num[ind]/err_df$den[ind]
err_df$rsqrtol = rsqrtol

#compute each split r2
#impute R2 as NA if cv is not available
rsqr = function(k){
  r2 = c()
  cv_k = err_df[,c('UserID',paste('cv',k,sep=''))]
  for(i in 1:length(new_dat_full)){
    cur_user = order_user[i]
    cv = cv_k[cv_k$UserID == cur_user, paste('cv',k,sep='')]
    if(cv != -1){
      cur_dat = new_dat_full[[i]]
      cur_split = split(cur_dat, cut(seq_along(1:nrow(cur_dat)), num_split, labels = FALSE))
      cur_test = cur_split[[k+1]]
      observe = cur_test[cur_test$mood_na == F,'y']
      r2[i] = 1- length(observe) * cv/sum((observe-mean(observe))^2) 
    }else{
      r2[i] = NA
    }
  }
  rt = data.frame(cbind(order_user, r2))
  colnames(rt) = c('UserID', paste('rsqr',k, sep=''))
  return(rt)
}

rsqr_ls = lapply(1:(num_split-1), rsqr)
# a list of rsqr for each split
comb_rsqr = rsqr_ls%>%reduce(full_join, by = 'UserID')


err_df =  join(err_df, comb_rsqr)
save(err_df, file = 'err_df.rda')

#Seems like when y is a constant, it is going to be a problem to implement glmnet
#Error in elnet(x, is.sparse, ix, jx, y, weights, offset, type.gaussian,  : 
#y is constant; gaussian glmnet fails at standardization step
