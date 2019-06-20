library(ggplot2)
library(reshape2)
df = read.csv('mse_clustering.csv')
ggplot(df,aes(x = clusters)) + geom_point(aes(y = MSE_Train))+geom_line(aes(y = MSE_Train, color = 'MSE_Train')) + 
  geom_point(aes(y = MSE_Test)) + geom_line(aes(y = MSE_Test, color = 'MSE_Test'))  +scale_colour_manual(values=c("blue", "red")) + ylab('MSE')
