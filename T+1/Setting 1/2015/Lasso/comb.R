library(plyr)
lags = c(1,3,5,10,15)
r2 = data.frame(matrix(nrow = 23, ncol = 5))
for(i in 1:length(lags)){
 load(paste('R2',lags[i],'.rda',sep = ''))
  r2[,i] = R2
}
save(r2, file = 'results_lasso_cluster.rda')
maxr2 = apply(r2, 1, max)
lag = mapvalues(apply(r2, 1, which.max),1:5, lags)

ndf = cbind(maxr2, lag)
write.csv(ndf, file = 'lasso_cluster.csv')
