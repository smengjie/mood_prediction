load('baseline2017.rda')
load('nobase2017.rda')
r2b = r_sqr_user$r_sqr
dfr2b = data.frame(group = 'baseline',value = r2b[r2b!=-Inf])
###########
r2nb = r_sqr_user_nobase$`r-sqr`
r2nb =  r2nb[r2nb!=-Inf]
r2nb = r2nb[!is.na(r2nb)]
dfr2nb = data.frame(group = 'no_baseline',value = r2nb)
###############
plot.data <- rbind(dfr2b, dfr2nb)

library(ggplot2)
ggplot(plot.data, aes(x=group, y=value)) + geom_boxplot() + xlab('') + ylab('R-squared') + labs(title = "Boxplot for N+1")+
  theme(plot.title = element_text(hjust = 0.5))
################


###feature importance: top 10
coe[1:11,]

