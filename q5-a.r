attach(u)
user = u$V1
item = u$V2
rate = u$V3
data = matrix(0, nrow=943, ncol=1682)


for (i in c(1:100000)){
  row = user[i]
  column = item[i]
  data[row,column] = rate[i]
}

#following is finding euclidean distance
get_eucli <- function(user1, movie, k){
  eucli <- list()
  eucli_best_k <- list()
  for (user2 in c(1:943)){
    if (user1 != user2){
      if(data[user2, movie] != 0){
        x = data[user1,]
        y = data[user2,]
        eucli_distance <- dist(rbind(x,y), method="euclidean")
        eucli <- append(eucli, list(c(eucli_distance, data[user2,movie])))
      }
    }
  }
  eucli <- eucli[order(sapply(eucli, function(x) x[1], simplify = TRUE), decreasing = FALSE)]
  if(k > length(eucli)){
    eucli_best_k <- eucli  
  }else{
    for (idx in c(1:k)){
      eucli_best_k[idx] <- eucli[[idx]][2]
    }
  }  
  return (eucli_best_k)
}

cal_predict_for_eucli <- function(k){
  predict_eucli = matrix(0, nrow=943, ncol=1682)
  for (user in c(1:943)){
    for (movie in c(1:1682)){
      if(data[user, movie] != 0){
        predict_eucli[user,movie]<- cal_avg_rate(get_eucli(user,movie,k), k, movie)
      }
    }
  }
  return (predict_eucli)
}


#following is finding manhattan distance
get_manha <- function(user1, movie, k){
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

cal_predict_for_manha <- function(k){
  predict_manha = matrix(0, nrow=943, ncol=1682)
  for (user in c(1:943)){
    for (movie in c(1:1682)){
      if(data[user, movie] != 0){
        predict_manha[user,movie]<- cal_avg_rate(get_manha(user,movie,k), k, movie)
      }
    }
  }
  return (predict_manha)
}

#following is finding Lmax distance
get_Lmax <- function(user1, movie, k){
  Lmax <- list()
  Lmax_best_k <- list()
  for (user2 in c(1:943)){
    if (user1 != user2){
      if(data[user2, movie] != 0){
        x = data[user1,]
        y = data[user2,]
        Lmax_distance <- dist(rbind(x,y), method="maximum")
        Lmax <- append(Lmax, list(c(Lmax_distance, data[user2,movie])))
      }
    }
  }
  
  Lmax <- Lmax[order(sapply(Lmax, function(x) x[1], simplify = TRUE), decreasing = FALSE)]
  if(k > length(Lmax)){
    Lmax_best_k <- Lmax  
  }else{
    for (idx in c(1:k)){
      Lmax_best_k[idx] <- Lmax[[idx]][2]
    }
  }
  return (Lmax_best_k)
}

cal_predict_for_Lmax <- function(k){
  predict_Lmax = matrix(0, nrow=943, ncol=1682)
  for (user in c(1:943)){
    for (movie in c(1:1682)){
      if(data[user, movie] != 0){
        predict_Lmax[user,movie]<- cal_avg_rate(get_Lmax(user,movie,k), k, movie)
      }
    }
  }
  return (predict_Lmax)
}


#following is calculating the average rate
cal_avg_rate <- function(best_k_list,k,movie){
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


#folowing is calculating the performance
cal_performance <- function(predict_matrix, data){
  sum1 = 0
  for(user in c(1:943)){
    r = 0
    for(movie in c(1:1682)){
      if(data[user,movie] != 0){
        r = 1
      }
      sum1 = sum1 + r
    }
  }
  
  sum2 = 0
  for(user in c(1:943)){
    for(movie in c(1:1682)){
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

#Following can get the Manhattan distance between every user and every other user
get_manha_matrix <- function(){
   manha_matrix = matrix(0, nrow=943, ncol=943)
   for (user1 in c(1:943)){
     for (user2 in c(1:943)){
       x = data[user1,]
       y = data[user2,]
       manha_matrix[user1,user2] <- dist(rbind(x,y), method="manhattan")
     }
   }
   return (manha_matrix)
}

#Following can get the Euclidean distance between every user and every other user
get_eucli_matrix <- function(){
  eucli_matrix = matrix(0, nrow=943, ncol=943)
  for (user1 in c(1:943)){
    for (user2 in c(1:943)){
      x = data[user1,]
      y = data[user2,]
      eucli_matrix[user1,user2] <- dist(rbind(x,y), method="euclidean")
    }
  }
  return (eucli_matrix)
}

#Following can get the Lmax distance between every user and every other user
get_Lmax_matrix <- function(){
  Lmax_matrix = matrix(0, nrow=943, ncol=943)
  for (user1 in c(1:943)){
    for (user2 in c(1:943)){
      x = data[user1,]
      y = data[user2,]
      Lmax_matrix[user1,user2] <- dist(rbind(x,y), method="maximum")
    }
  }
  return (Lmax_matrix)
}

tmp1 <- get_eucli_matrix()
library(MASS)
write.matrix(tmp1, file = "eucli_matrix.txt", sep = " ")

tmp2 <- get_manha_matrix()
library(MASS)
write.matrix(tmp2, file = "manha_matrix.txt", sep = " ")

tmp3 <- get_Lmax_matrix()
library(MASS)
write.matrix(tmp3, file = "Lmax_matrix.txt", sep = " ")

eucli_predict_matrix <- cal_predict_for_eucli(8)
eucli_performance <- cal_performance(eucli_predict_matrix, data)
print (eucli_performance)

manha_predict_matrix <- cal_predict_for_manha(8)
manha_performance <- cal_performance(manha_predict_matrix, data)
print (manha_performance)

Lmax_predict_matrix <- cal_predict_for_Lmax(8)
Lmax_performance <- cal_performance(Lmax_predict_matrix, data)
print (Lmax_performance)