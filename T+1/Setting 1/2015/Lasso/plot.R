library(glmnet)
library(vars)
library(plyr)
library(ggplot2)

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

pdf('kmeans_on_users.pdf')
ggplot(as.data.frame(cbind(1:23,clustering_fun(m1)[[1]])), aes(x = V1, y = V2)) + geom_point() + 
  xlab('number of clusters') +ylab('total within cluster distance') + ggtitle('K-means on Lasso Coefficients')+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

clusters_m1 = sapply(1:4, function(i) which(clustering_fun(m1)[[2]][4,] == i))
errors = clustering_fun(m1)[[1]]
clusterings = clustering_fun(m1)[[2]]

save(clusters_m1, errors, clusterings, file = 'clustering.rda')

