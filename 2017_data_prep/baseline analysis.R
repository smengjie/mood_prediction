library(vars)
load('C:/Users/smengjie/Box Sync/Fitbit 2017-2018 data share/Jessica_Mengjie/Data/2017_data.rda')
baseline = read.csv('C:/Users/smengjie/Box Sync/Fitbit 2017-2018 data share/2017 data/survey data/Data2017_full.csv')
start = as.Date('2017-07-01')
prestart = as.Date('2017-04-01')
end = as.Date("2018-06-30")
diff = end - start + 1

#### Order of users
#less to more NA
#restrict to intern period
intern_data = full_2017[full_2017$new_day >= start & full_2017$new_day <= end, ]
intern_data$mood_na = is.na(intern_data$new_daily_mood_score)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$UserID), FUN = sum )
#Drop any users that have no mood data ever.
selected_users = mood_na_counts[mood_na_counts$x != diff,]
order_user = selected_users$Group.1[order(selected_users$x)]
###########


######baseline variables
base_var = c("Clerk0", "Specialty", "hours0", "depr0", "sleep24h0", "sleepAve0",
             "Sex",  "religious", "sexOrientation", "Marital", "SigOther0", "Child", "Ethnicity", 
             "asian", "Age", "PHQtot0", "PHQ10above0","BirthPlace", "GADtot0", "GAD10above0", "EFE0", "Neu0", "SLE0")
##########


####create a sub baseline data set, with only users appearing in mood/activity/sleep, and selected baseline variables
sub_baseline = baseline[baseline$UserID %in% order_user,c('UserID',base_var)]

##############check if users are the same
#There are missing values
#baseline variables with NA
#8 variables
#"Clerk0"     "hours0"     "religious"  "Ethnicity"  "asian"      "Age"        "BirthPlace" "EFE0"      
na_base_var = base_var[sapply(base_var, function(i) sum(is.na(sub_baseline[,i]))!= 0)]
 
#function gives the users NA, given the variable name
na_user_per_var = function(i){
  cur_df = sub_baseline[,c('UserID',i)]
  users = sub_baseline$UserID[which(is.na(cur_df[,i]) == T)]
  return(users)
}  
#quite different users
#common to see variables overlapping with asian/clerk0/hours0
#variables with only one user missing do not have any overlap
similar_users = data.frame(matrix(nrow= length(na_base_var),ncol = length(na_base_var)))
colnames(similar_users) = na_base_var
rownames(similar_users) = na_base_var
user_lists = lapply(na_base_var, na_user_per_var)
names(user_lists) = na_base_var
for(i in 1:length(user_lists)) {
  for(j in 1:length(user_lists)){
    similar_users[i,j] = sum(user_lists[[i]]%in%user_lists[[j]])
  }
}
#check the number of entries each user is missing
user_counts = as.data.frame(table(unlist(user_lists)))
#max is 3 << 8
unique(user_counts$Freq)
#Thus do not drop any users because of baseline variables missing
#########################

###############check when asian is missing, if ethnicity is not asian
asian_ethnicity = sub_baseline[is.na(sub_baseline$asian) == T,c('UserID', 'Ethnicity', 'asian')]
dim(asian_ethnicity)[1]
sum(asian_ethnicity$Ethnicity != 4, na.rm =T)
unique(asian_ethnicity$Ethnicity)
#Yes. When asian is missing, ethnicity is not asian
#From the meeting, asian is not importnat
#########

######pick baseline variables to train
len_levels = sapply(base_var, function(i) length(unique(sub_baseline[,i])))

#exclude BirthPlace because it has 29 levels, hard to interpret
#exclude asian because it is not important
new_base_var = base_var[!(base_var%in%c('asian','BirthPlace'))]
nsub_baseline = sub_baseline[,c('UserID',new_base_var)]

#separate into categorical/continuous variables
cts_var = c('Neu0', 'EFE0', 'GADtot0', 'PHQtot0', 'Age', 'sleepAve0', 'sleep24h0', 'hours0')
cat_var = new_base_var[!(new_base_var %in% cts_var)]

#convert NA in Clerk0 into 0
nsub_baseline$Clerk0[is.na(nsub_baseline$Clerk0)] = 0

#Put 0 if missing hours0
nsub_baseline$hours0[is.na(nsub_baseline$hours0)] = 0

#Put mean if missing age
nsub_baseline$Age[is.na(nsub_baseline$Age)] = mean(nsub_baseline$Age,na.rm = T)

#now only religious, ethnicity, efe0 has missing values, each one NA
colnames(nsub_baseline)[sapply(colnames(nsub_baseline), function(i) sum(is.na(nsub_baseline[,i])) != 0)]

#remove users with religious, Ethnicity, and EFE0 missing
baseline_2017 = nsub_baseline[complete.cases(nsub_baseline),]


#convert categorical variables into factors
for(i in cat_var){
  baseline_2017[,i] = as.factor(baseline_2017[,i])
}
summary(baseline_2017)

#This is a complete-cased file with only users in mood/activity/sleep, and all selected baseline variables
#Filled in Clerk0, hours0, Age
#Drop users missing in religious, Ethnicity, and EFE0. Since there is only one missing each, total 3
#507 users, 22 covariates
#"UserID"         "Clerk0"         "Specialty"      "hours0"         "depr0"          "sleep24h0"     
# "sleepAve0"      "Sex"            "religious"      "sexOrientation" "Marital"        "SigOther0"     
# "Child"          "Ethnicity"      "Age"            "PHQtot0"        "PHQ10above0"    "GADtot0"       
# "GAD10above0"    "EFE0"           "Neu0"           "SLE0"   
save(baseline_2017,file = 'baseline_2017.rda')
