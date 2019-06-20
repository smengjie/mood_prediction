
load('clustering.rda')
source('functions.R')
new_dat_big = lapply(order_user, model_df)

foldIDs = list()
folds = 5
foldIDs = lapply(new_dat_big, separate)
mse = numeric()
R2 = numeric()

#4 can be changed to any number between 23.In this study so far, we used 1,4,23
#But reconstruct clusters_m1 when using other cluster number.The code should be the last bit of plot.R
for(i in 1:4){
  err= data.frame(matrix(ncol = folds,nrow = length(clusters_m1[[i]])))
  cur_users = clusters_m1[[i]]
  for(u in 1:folds) {
    train = ldply(lapply(cur_users, function(l) new_dat_big[[l]][foldIDs[[l]] != u,]))
    test = lapply(cur_users, function(l) new_dat_big[[l]][foldIDs[[l]] == u,])
    fit_df = cv.glmnet(x = as.matrix(train[,-1]), y = train[,'new_y'])
    yhat = lapply(test, function(df) predict(fit_df, newx = as.matrix(df[,-1]),s = 'lambda.min'))
    err[,u] = sapply(1:length(cur_users), function(l) sum((yhat[[l]] - test[[l]][,1])^2))
  }
  mse[clusters_m1[[i]]] = apply(err, 1, sum)/sapply(new_dat_big[cur_users], nrow)
  R2[clusters_m1[[i]]] = 1- apply(err, 1, sum)/sapply(new_dat_big[cur_users], function(df) sum((df[,'new_y'] - mean(df[,'new_y']))^2))
}
save(R2,file = paste('R2',cur_p, '.rda', sep = ''))
