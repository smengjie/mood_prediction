load('2017_merged_all_daily.Rdata')
#pretty symmetric
sleeptol_vars = c('ASLEEP','totalMinutes_new')
#skewed, did log(x+1) on 'Awake', sqrt on 'wakeMinutes_new'
wake_vars = c('AWAKE','wakeMinutes_new')
#skewed, did squared root on both
steptol_vars = c('Steps','dailyStepTotal_new')

index_impute = function(df){
  colnames(df) = c('x','y')
  index = (!is.na(df$x))&(is.na(df$y))
  return(index)
}
impute = function(input,impute_df){
  colnames(input) = c('x','y')
  colnames(impute_df) = c('x','y')
  model = lm(y~x, input)
  yhat = predict(model, impute_df)
  return(yhat)
}

####wake
wake = new_data1[,wake_vars]
wake_red = wake[complete.cases(wake),]
wake_input = data.frame(log(wake_red[,wake_vars[1]]+1),sqrt(wake_red[,wake_vars[2]]))
wake_ind = index_impute(wake)
wake_impute = data.frame(log(wake[wake_ind, wake_vars[1]]+1), sqrt(wake[wake_ind, wake_vars[2]]))
wake_output = round((impute(wake_input,wake_impute))^2)
wake[wake_ind, wake_vars[2]] = wake_output



####step
step = new_data1[,steptol_vars]
step_red = step[complete.cases(step),]
step_input = data.frame(sqrt(step_red[,steptol_vars[1]]),sqrt(step_red[,steptol_vars[2]]))
step_ind = index_impute(step)
step_impute = data.frame(sqrt(step[step_ind,steptol_vars[1]]),sqrt(step[step_ind,steptol_vars[2]]))
step_output = round(impute(step_input,step_impute)^2)
step[step_ind, steptol_vars[2]] = step_output


#####sleep
sleep = new_data1[,sleeptol_vars]
sleep_input = sleep[complete.cases(sleep),]
sleep_ind = index_impute(sleep)
sleep_impute = sleep[sleep_ind,]
sleep_output = round(impute(sleep_input,sleep_impute))
sleep[sleep_ind, sleeptol_vars[2]] = sleep_output




  
col_rem = c(sleeptol_vars[1], wake_vars[1], steptol_vars[1],"EFFICIENCY", "INBED_hr", 
            "ASLEEP_hr","RESTLESS_hr", "AWAKE_hr")

col_keep = colnames(new_data1)[!colnames(new_data1) %in% col_rem]


combined17 = new_data1[,col_keep]
combined17[,sleeptol_vars[2]] = sleep[,sleeptol_vars[2]]
combined17[,wake_vars[2]] = wake[,wake_vars[2]]
combined17[,steptol_vars[2]] = step[,steptol_vars[2]]


write.csv(combined17, file = 'combined2017.csv')


which((!is.na(combined17$dailyStepTotal_new)) & is.na(new_data1$dailyStepTotal_new)==T  )
dim(step_impute)
which((!is.na(combined17$totalMinutes_new)) & is.na(new_data1$totalMinutes_new) == T)
dim(sleep_impute)
which((!is.na(combined17$wakeMinutes_new)) & is.na(new_data1$wakeMinutes_new) == T)
dim(wake_impute)

