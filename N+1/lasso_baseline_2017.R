library(vars)
load('2017_data.rda')
load('baseline_2017.rda')
start = as.Date('2017-07-01')
prestart = as.Date('2017-04-01')
end = as.Date("2018-06-30")
diff = end - start + 1


##########variable names
mood_var = 'new_daily_mood_score'
sleep_vars = c("INBED","RESTLESS","deepMinutes_new", "remMinutes_new", "lightMinutes_new", "wakeMinutes_new",
               "wakeCount_new", "lightCount_new", "deepCount_new", "remCount_new", "totalMinutes_new")  
dist_vars = c('dailyStepTotal_new', "active_minutes_new")
other = "resting_hr_new" 
##############



####### Order of users
#less to more NA
#restrict to intern period, merge with baseline variables
#507 users
intern_data = merge(full_2017[full_2017$new_day >= start & full_2017$new_day <= end, 
                        c('UserID', mood_var, sleep_vars, dist_vars, other)], baseline_2017, 'UserID')
intern_data$mood_na = is.na(intern_data$new_daily_mood_score)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$UserID), FUN = sum )
#Drop any users that have no mood data ever.
selected_users = mood_na_counts[mood_na_counts$x != diff,]
order_user = selected_users$Group.1[order(selected_users$x)]
length(order_user)
###########

##########
##training users
set.seed(1)
train_size = 3/4

#380 users
train_users = sample(order_user, train_size * length(order_user), replace = F)
##testing users
test_users = order_user[!order_user %in% train_users]
#############



###################imputation method on mood
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

#################function: impute predictors
#right now we take the mean of last seven days
days_imputed = 7
impute_predictors = function(pred, train, var_name){
  #if everything is missing, impute by the data set mean
  if(sum(!is.na(pred))== 0){
    if(train){
      pred = mean(intern_data[intern_data$UserID %in% train_users,var_name ],na.rm = T)
    } else{
      pred = mean(intern_data[intern_data$UserID %in% test_users,var_name ],na.rm = T)
    }
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
  train = cur_user%in%train_users
  #imputation
  cur_data[,mood_var] = impute_mood(cur_data[,mood_var])
  for(i in c(sleep_vars,dist_vars,other)){
    cur_data[,i] = impute_predictors(cur_data[,i], train, i)
  }
  return(cur_data)
}  
##########


##### A function gives model matrix of mood/sleep/activity for lasso, maybe applicable to other algos too.
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


#########Add baseline variables
add_base = function(cur_user,cur_df){
  cur_df$UserID = cur_user
  cur_base = intern_data[intern_data$UserID == cur_user, colnames(baseline_2017)][1,]
  return(merge(cur_df,cur_base, all.x = T))
}
  

####select data points with non-NA y from a model matrix
real_y = function(cur_user,cur_merge){
  ind = which(intern_data[intern_data$UserID == cur_user,'mood_na'][cur_merge$trend] == FALSE)
  return(cur_merge[ind,])
}

 
#####Construct test/train data sets
library(plyr)
# get a complete case lagged data frame with baseline for each user
new_dat = lapply(order_user, function(user) add_base(user, lasso_df(impute_df(user))))
# unlist into a dataframe
new_dat_big = ldply(new_dat)
# change to model matrix, note here we don't have y
model_mtx = model.matrix(lm(y~.,new_dat_big[ , -which(colnames(new_dat_big) %in% c("UserID","const"))]))[,-1]
# Add back y and user id
model_mtx_big = cbind(new_dat_big[,c('UserID','y')],model_mtx)

# a new list where for each user, we have a lagged, factorized dataframe ready for glmnet to train
new_dat_mtx = lapply(order_user, function(user) model_mtx_big[model_mtx_big$UserID == user, ])
new_dat_mtx = lapply(new_dat_mtx, function(dat) dat[ , -which(colnames(dat) %in% c("UserID","const"))])
names(new_dat_mtx) = order_user


#training data
new_dat_train = new_dat_mtx[as.character(train_users)]
#unlist the train datas sets,
train_data = ldply(new_dat_train)[,-1]
#change into model matrix
train_mtx = model.matrix(lm(y~., train_data))[,-1]

#test data
testcomb = list(new_dat_mtx[as.character(test_users)], test_users)
new_dat_test = lapply(1:length(test_users), function(i) real_y(testcomb[[2]][i], testcomb[[1]][[i]]))




#####Train lasso
library(glmnet)
grid = 10^seq(3,-3,length = 1000)
lasso_model = cv.glmnet(train_mtx, train_data[,'y'], alpha = 1,lambda = grid)
#to do change
predicted_mood = lapply(new_dat_test, function(dat) predict(lasso_model,as.matrix(dat[,-1])))

#R^2
err = sapply(1:length(test_users), function(i) sum((predicted_mood[[i]] - new_dat_test[[i]][,1])^2))
r_sqr = sapply(1:length(test_users), function(i) 1 - err[i]/sum((new_dat_test[[i]][,1] - mean(new_dat_test[[i]][,1]))^2))
r_sqr_user = data.frame(test_users, r_sqr)
colnames(r_sqr_user) = c('Group.1','r_sqr')
r_sqr_user = merge(r_sqr_user, selected_users, 'Group.1', all.x = TRUE)

#selected variables
coeff_est = coef(lasso_model, s = 'lambda.min', alpha = 0)
abc = round(coeff_est, 4)
## now I create the heat map
library(reshape2)
melted_cormat <- melt(as.matrix(abc))
melted_cormat[,1] = as.factor(melted_cormat[,1])
coe = melted_cormat[order(abs(melted_cormat$value),decreasing = T),-2]


#save model
save(r_sqr_user,coe, file = 'baseline2017.rda')