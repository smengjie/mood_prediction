source('new_data_full.R')
o = 3
lookback = 7
step = 1
delay = 1
#Tune Parameters
#batch_size*step_epoch better be approximately number of data points
batch_size = 50
step_epoch = 3
unit = 32
epoch = 50

  
val_results = data.frame(matrix(nrow= length(order_user), ncol = 3))
colnames(val_results) = c('r2_gru','mse_gru','epoch_gru')
  
  



#####function that splits the data for each user into 5 parts
splits = function(cur_dat,test_block) {
  blocks = split(cur_dat, cut(seq_along(1:nrow(cur_dat)), num_split, labels = FALSE))
  cur_train = ldply(blocks[1:(test_block-1)])[,-1]
  cur_train$train = TRUE
  cur_test = blocks[[test_block]]
  cur_test$train = FALSE
  cur_ts = rbind(cur_train, cur_test)
  return(cur_ts)
  
}


#indexing on the last lag
#generator
generator = function(data, lookback, delay, min_index, max_index, 
                     shuffle = FALSE, batch_size = batch_size,step = 1){
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

#generate validation dataset
#a list (inputs, targets)
validation_gen = function(data){
  val_steps = dim(data[data$train == FALSE & data$mood_na == FALSE,])[1]
  rows = intersect(which(data$train == FALSE),which(data$mood_na == FALSE))
  
  sub_data = data.matrix(subset(data, select=-c(train,mood_na)))
  
  inputs <- array(0,dim = c(length(rows), lookback/step, dim(sub_data)[[-1]]))
  targets <- array(0, dim = c(length(rows)))
  
  for(m in 1:val_steps) {
    indices = seq(rows[[m]] - lookback - delay + 1 , rows[[m]] - delay, length.out = dim(inputs)[[2]]) 
    inputs[m,,] <- sub_data[indices,]
    targets[[m]] <- sub_data[rows[[m]], mood_var] 
  
  }
  list(inputs, targets)
}


lstm_fit = function(data){
  #maxind the last lag of the last data point
  maxind = length(data$train[data$train == TRUE]) - delay
  #generator for the train data
  #remove train and mood_na which are not predictors
  train_data = data[data$train == TRUE,]
  train_gen = generator(data.matrix(subset(train_data, select=-c(train,mood_na))),lookback = lookback, delay = delay, min_index = 1,max_index = maxind,
                        shuffle = TRUE, step = step, batch_size = batch_size)
 
  val_data = validation_gen(data)
  
  
  
  model_gru = keras_model_sequential()%>%
    layer_gru(units = unit, input_shape = list(NULL,dim(val_data[[1]])[[3]]),
              recurrent_activation = "sigmoid") %>% 
    layer_dense(units = 1)
  
  
  model_gru%>%compile(optimizer = optimizer_rmsprop(),loss = 'mean_squared_error')
  
   
   history_gru = model_gru %>% fit_generator(train_gen,steps_per_epoch = step_epoch,
                                               epochs = epoch,
                                               validation_data = val_data)
   

  
  return(history_gru)
}




for(k in 1:length(order_user)){
  
  print(k)
  data = new_dat_full[[k]]
  
  #Iterating each split

  
   #cur_ts is a fully imputed block dataset with an indicator on train/test and an indicator on mood missing
    cur_ts = splits(data,o)
    #extract observed mood
    cur_y = cur_ts[cur_ts$train == FALSE & cur_ts$mood_na == FALSE, mood_var]
    
    #if no testing data, impute all as NA
    #else if all testing data are the same, calculate epoch and mse as usual, but imput R^2 as -Inf
    #else (i.e., nonempty testing data with non-identical entries), calculate everything as usual
    if(length(cur_y) == 0){
      
      val_results[k,3-2] = NA
      
    }else if (sum(abs(diff(cur_y))) == 0) {
      
      val_results[k,3-2]  = -Inf
      
    } else{
  
      rnn = lstm_fit(cur_ts)
      
      val_results[k,c(3-1,3)] = c(min(unlist(rnn)[grep('metrics.val_loss', names(unlist(rnn)), fixed = TRUE)]),
                                  which.min(unlist(rnn)[grep('metrics.val_loss', names(unlist(rnn)), fixed = TRUE)]))
      val_results[k,3-2]  = 1 - as.numeric(val_results[k,(3-1)]) * length(cur_y)/sum((cur_y - mean(cur_y))^2)
      
    }

 
}

save(val_results, file = 'val_results_gru__cv2.rda')

