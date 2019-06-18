#lag 7
#One split: train on first 300 days, test on the rest
#Two layers
library(tibble)
library(readr)
library(tensorflow)
library(keras)
load("TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")
#### Order of users
intern_data = full_2015_data[full_2015_data$days_intern >= 0, ]
intern_data$mood_na = is.na(intern_data$mood)
mood_na_counts = aggregate(intern_data$mood_na, by = list(intern_data$userid), FUN = sum )
order_user = mood_na_counts$Group.1[order(mood_na_counts$x)]
val_results = data.frame(matrix(nrow= 23, ncol = 9))
colnames(val_results) = c('r2_gru','mse_gru','epoch_gru','r2_lstm','mse_lstm','epoch_lstm','r2_rnn','mse_rnn','epoch_rnn')

lookback = 7
step = 1
delay = 1
batch_size = 20
maxind = 300
unit = 32
epoch = 20
step_epoch = 50

### A function gives model matrix
lstm_model_matrix = function(cur_user) {
  cur_data = full_2015_data[full_2015_data$userid == cur_user, ]
  #mood
  x1 = cur_data[cur_data$days_intern >= 0, 'mood']
  x1.5 = is.na(x1)
  #sleep
  sleep_vars = c('TotalMinutesAsleep', 'TotalTimeInBed', 'TotalSleepRecords')
  x2 = cur_data[cur_data$days_intern >= 0, sleep_vars]
  x2[,c(1,2)] = x2[,c(1,2)]/60  ## Turns it to minutes
  x2.5 = is.na(x2[ ,'TotalMinutesAsleep'])
  x_new = cbind(x1,x1.5, x2, x2.5)
  #distance
  dist_vars = c('TotalDistance', 'VeryActiveDistance', 'ModeratelyActiveDistance', 'LightActiveDistance')
  x3 = sqrt(cur_data[cur_data$days_intern >= 0, dist_vars])
  x3.5 = is.na(x3[ ,'TotalDistance'])
  #new time series data
  cur_ts = cbind(x_new, x3, x3.5) 
  
  colnames(cur_ts) = c('mood', 'mood_missing', sleep_vars, 'sleep_missing', dist_vars, 'distance_missing')
  ## mean imputation
  for(i in colnames(cur_ts)){
    cur_ts[is.na(cur_ts[,i]), i] = mean(cur_ts[,i], na.rm = TRUE)}
  return(data.matrix(cur_ts))
}




#indexing on the last lag
#generator
generator = function(data, lookback, delay, min_index, max_index, 
                     shuffle = FALSE, batch_size = 50,step = 1){
  #max_index is the index of the last lag of the last data point
  #min_index is the index of the first lag of the first data point
  if (is.null(max_index)) max_index = nrow(data) - delay #no -1
  #i is the window index, last lag
  i = min_index + lookback -1 
  function() {
    if (shuffle) {
      #sample batch size from all last lag indices 
      rows = sample(c((min_index + lookback - 1) : max_index),size = batch_size)
    }else {
      if(i + batch_size-1 > max_index)
        i <<- min_index + lookback - 1
      rows = c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    samples <- array(0,dim = c(length(rows), lookback/step, dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for(j in 1:length(rows)) {
      indices = seq(rows[[j]] - lookback + 1, rows[[j]], length.out = dim(samples)[[2]]) 
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]]  + delay, 1] 
    }
    list(samples, targets)
  }
  
}
lstm_fit = function(data){
  train_gen = generator(data,lookback = lookback, delay = delay, min_index = 1,max_index = maxind,
                        shuffle = TRUE, step = step, batch_size = batch_size)
  val_gen = generator(data,lookback = lookback, delay = delay, min_index = maxind+1,max_index = NULL,
                      step = step, batch_size = batch_size)
  val_steps = (nrow(data)-delay - maxind -lookback + 1)/batch_size
  
  
  model_gru = keras_model_sequential()%>%
    layer_gru(units = unit, input_shape = list(NULL,dim(data)[[-1]])) %>% 
    layer_dense(units = 1)
  
  model_lstm = keras_model_sequential()%>%
    layer_lstm(units = unit, input_shape = list(NULL,dim(data)[[-1]])) %>%
    layer_dense(units = 1)
  
  model_rnn = keras_model_sequential()%>%
    layer_simple_rnn(units = unit, input_shape = list(NULL,dim(data)[[-1]])) %>%
    layer_dense(units = 1)
  
  
  model_gru%>%compile(optimizer = optimizer_rmsprop(),loss = 'mean_squared_error')
  model_lstm%>%compile(optimizer = optimizer_rmsprop(),loss = 'mean_squared_error')
  model_rnn%>%compile(optimizer = optimizer_rmsprop(),loss = 'mean_squared_error')
  
  
  history_gru = model_gru %>% fit_generator(train_gen,steps_per_epoch = step_epoch,
                                        epochs = epoch,
                                        validation_data = val_gen,
                                        validation_steps = val_steps)
  history_lstm = model_lstm %>% fit_generator(train_gen,steps_per_epoch = step_epoch,
                                        epochs = epoch,
                                        validation_data = val_gen,
                                        validation_steps = val_steps)
  history_rnn = model_rnn %>% fit_generator(train_gen,steps_per_epoch = step_epoch,
                                        epochs = epoch,
                                        validation_data = val_gen,
                                        validation_steps = val_steps)
  
  return(list(history_gru, history_lstm, history_rnn))
}



for(cur_user in order_user){
  print(cur_user)
  data = lstm_model_matrix(cur_user)
  #TO DO
  cur_y  = data[(maxind + lookback + delay):
                  (maxind + lookback + delay + 
                     floor((nrow(data)-delay - maxind -lookback + 1)/batch_size)*batch_size-1),1]
  rnn = lstm_fit(data)
  for(i in 1:3){
    val_results[which(order_user == cur_user),c(3*i-1,3*i)] = c(min(unlist(rnn[[i]])[grep('metrics.val_loss', names(unlist(rnn[[i]])), fixed = TRUE)]),
                                                                which.min(unlist(rnn[[i]])[grep('metrics.val_loss', names(unlist(rnn[[i]])), fixed = TRUE)]))
    val_results[which(order_user == cur_user),3*i-2]  = 1 - as.numeric(val_results[which(order_user == cur_user),(3*i-1)]) * length(cur_y)/sum((cur_y - mean(cur_y))^2)
  }
}
save('val_results', file = paste('val_results','.rda', sep= ''))
