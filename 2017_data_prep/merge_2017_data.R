full_2017_data = read.csv("NewDailyMood_combined2017.csv")
baseline = read.csv('Data2017_full.csv')
full_2017_data = full_2017_data[,-1]
#change new_day to type Date
full_2017_data$new_day = as.Date(as.character(full_2017_data$new_day))
start = as.Date('2017-07-01')
prestart = as.Date('2017-04-01')
end = as.Date("2018-06-30")
#list all users
#users all appear in baseline
users = unique(full_2017_data$UserID)

#308686
new_data_per_user = function(user){
  data_cur_user = full_2017_data[full_2017_data$UserID == user,]
  #get a vector of complete dates, start from 4/1/2017, end on the last day
  new_day = seq(prestart, max(end,max(data_cur_user$new_day)), by="days")
  #merge two data frames, keep all dates
  new_data_cur_user = merge(x = data_cur_user, y = data.frame(new_day),"new_day", all = TRUE)
  #get days in intern, start on 7/1/2017
  new_data_cur_user$days_intern = new_data_cur_user$new_day - start
  #fill the same user id
  new_data_cur_user$UserID = user
  return(new_data_cur_user)
}
library(plyr)
full_2017 = ldply(lapply(users, new_data_per_user))

full_2017 = merge(x = full_2017, y = baseline, 'UserID', all.x = TRUE)

save(full_2017, file = '2017_data.rda')


