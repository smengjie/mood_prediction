library(tibble)
library(readr)
library(tensorflow)
library(keras)
library(tidyverse)
library(plyr)

load('2017_data.rda')
start = as.Date('2017-07-01')
prestart = as.Date('2017-04-01')
end = as.Date("2018-06-30")
diff = end - start + 1
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
#sqrt daily step total
impute_df = function(cur_user) {
  cur_data = intern_data[intern_data$UserID == cur_user, ]
  cur_data = cur_data[, c(mood_var,sleep_vars,dist_vars,other,'mood_na')]
  #imputation
  cur_data[,mood_var] = impute_mood(cur_data[,mood_var])
  for(i in c(sleep_vars,dist_vars,other)){
    cur_data[,i] = impute_predictors(cur_data[,i], i)
  }
  cur_data[,'dailyStepTotal_new'] = sqrt(cur_data[,'dailyStepTotal_new'])
  return(cur_data)
}  
##########



##########fully imputed model matrix for each user with an indicator of whether y is missing
new_dat_full = lapply(order_user, function(user) impute_df(user))

#save(new_dat_full, file = 'new_dat_full.rda')
###############
