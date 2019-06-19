#can change to other file names
load('results_2017.rda')
numsplits = 5

r2 = err_df[,c(paste('rsqr',1:(numsplits-1),sep= ''))]

library(reshape2)
r2_melt = melt(as.matrix(r2))

r2_melt = r2_melt[!is.na(r2_melt$value),]
r2_melt = r2_melt[r2_melt$value != -Inf,]
library(ggplot2)
ggplot(r2_melt, aes(x=Var2, y=value)) + 
  geom_boxplot() + coord_cartesian(ylim = c(-1, 1)) + xlab('cv') + ylab('R-squared') + ggtitle('Boxplot for T+1 on 2017 data in Setting 2')+ 
  theme(plot.title = element_text(hjust = 0.5))



r2_ls = lapply(1:ncol(r2), function(i) r2[!(is.na(r2[,i])),i])
r2_ls = lapply(1:length(r2_ls), function(i) r2_ls[[i]][r2_ls[[i]] != -Inf])
r2_ls = lapply(r2_ls, summary)
names(r2_ls) = c('rsqrtol',paste('rsqr',1:4,sep= ''))
r2_ls

cv = err_df[,c('avg_cv', paste('cv',1:4, sep = ''))]
cv_ls = lapply(1:ncol(cv), function(i) summary(cv[cv[,i]!= -1,i]))
names(cv_ls) = c('avg_cv', paste('cv',1:4, sep = ''))
cv_ls
cv_melt = melt(as.matrix(cv))
cv_melt = cv_melt[cv_melt$value != -1,]
ggplot(cv_melt, aes(x=Var2, y=value)) + 
  geom_boxplot()  + coord_cartesian(ylim = c(0, 10))



###
lambda = err_df$lambda[err_df$lambda != -1]
length(lambda)
hist(lambda)

