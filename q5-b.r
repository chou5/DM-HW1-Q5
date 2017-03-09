attach(u_info)
attach(u)
user = u$V1
item = u$V2
rate = u$V3
data2 = matrix(0, nrow=943, ncol=1682)

for (i in c(1:100000)){
  row = user[i]
  column = item[i]
  data2[row,column] = rate[i]
}



#get gender information
data4 = matrix(0, nrow=943, ncol=1)
for (i in c(1:943)){
    elems <- u_info[i,]
    
    if (grepl("M", elems)){
      data4[i,1] = 1    
    }else{
      data4[i,1] = 2  
    }
  }
  

#get job information
data3 = matrix(0, nrow=943, ncol=1)
for (i in c(1:943)){
    elems <- u_info[i,]
    
    if (grepl("administrator", elems) == TRUE){
      data3[i,1] = 1
    }else if(grepl("artist", elems) == TRUE){
      data3[i,1] = 2    
    }else if(grepl("doctor", elems) == TRUE){
      data3[i,1] = 3    
    }else if(grepl("educator", elems) == TRUE){
      data3[i,1] = 4    
    }else if(grepl("engineer", elems) == TRUE){
      data3[i,1] = 5     
    }else if(grepl("entertainment", elems) == TRUE){
      data3[i,1] = 6    
    }else if(grepl("executive", elems) == TRUE){
      data3[i,1] = 7    
    }else if(grepl("healthcare", elems) == TRUE){
      data3[i,1] = 8    
    }else if(grepl("homemaker", elems) == TRUE){
      data3[i,1] = 9    
    }else if(grepl("lawyer", elems) == TRUE){
      data3[i,1] = 10    
    }else if(grepl("librarian", elems) == TRUE){
      data3[i,1] = 11    
    }else if(grepl("marketing", elems) == TRUE){
      data3[i,1] = 12    
    }else if(grepl("none", elems) == TRUE){
      data3[i,1] = 13    
    }else if(grepl("other", elems) == TRUE){
      data3[i,1] = 14    
    }else if(grepl("programmer", elems) == TRUE){
      data3[i,1] = 15    
    }else if(grepl("retired", elems) == TRUE){
      data3[i,1] = 16    
    }else if(grepl("salesman", elems) == TRUE){
      data3[i,1] = 17    
    }else if(grepl("scientist", elems) == TRUE){
      data3[i,1] = 18    
    }else if(grepl("student", elems) == TRUE){
      data3[i,1] = 19    
    }else if(grepl("technician", elems) == TRUE){
      data3[i,1] = 20    
    }else if(grepl("writer", elems) == TRUE){
      data3[i,1] = 21    
    }
}


#following is finding manhattan distance
get_manha <- function(user1, movie, k, data){
  manha <- list()
  manha_best_k <- list()
  for (user2 in c(1:943)){
    if (user1 != user2){
      if(data[user2, movie] != 0){
        x = data[user1,]
        y = data[user2,]
        manha_distance <- dist(rbind(x,y), method="manhattan")
        manha <- append(manha, list(c(manha_distance, data[user2,movie])))
      }
    }
  }
  manha <- manha[order(sapply(manha, function(x) x[1], simplify = TRUE), decreasing = FALSE)]
  if(k > length(manha)){
    manha_best_k <- manha  
  }else{
    for (idx in c(1:k)){
      manha_best_k[idx] <- manha[[idx]][2]
    }
  }
  return (manha_best_k)
}

cal_predict_for_manha <- function(k,data){
  predict_manha = matrix(0, nrow=943, ncol=1683)
  for (user in c(1:943)){
    for (movie in c(1:1683)){
      if(data[user, movie] != 0){
        predict_manha[user,movie]<- cal_avg_rate(get_manha(user,movie,k, data), k, movie, data)
      }
    }
  }
  return (predict_manha)
}

#following is calculating the average rate
cal_avg_rate <- function(best_k_list,k,movie, data){
  if (length(best_k_list) < k){
    count = 0
    total = 0
    for(i in c(1:943)){
      if (data[i, movie] != 0){
        total = total + data[i,movie]
        count = count + 1
      }
    }
    avg_rate = total/count
  }else{
    avg_rate <- Reduce("+", best_k_list)/length(best_k_list)
  }
  return (avg_rate)
}


#folowing is calcculating the performance
cal_performance <- function(predict_matrix, data){
  sum1 = 0
  for(user in c(1:943)){
    r = 0
    for(movie in c(1:1683)){
      if(data[user,movie] != 0){
        r = 1
      }
      sum1 = sum1 + r
    }
  }
  
  sum2 = 0
  for(user in c(1:943)){
    for(movie in c(1:1683)){
      r = 0
      p = predict_matrix[user,movie]
      t = data[user,movie]
      if(data[user,movie] != 0){
        r = 1
      }
      sum2 = sum2 + r*abs(p-t)
    }
  }
  
  performance = (1/sum1)*sum2
  return (performance)
}

user_info1 <- cbind(data4, data2)
manha_predict_matrix <- cal_predict_for_manha(8,user_info1)
cal_performance(manha_predict_matrix, user_info1)

user_info2 <- cbind(data3, data2)
manha_predict_matrix <- cal_predict_for_manha(8,user_info2)
cal_performance(manha_predict_matrix, user_info2)


