MSE = list()

for(user_i in 1:23) {
  load(paste('user_',user_i,'.rda',sep=''))
  MSE[[user_i]] = MSE_testing
}

save(MSE, file = 'testmse.rda')
