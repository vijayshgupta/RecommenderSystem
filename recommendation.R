#Implement User-based CF
#use Tmall user's behavior log
require('reshape')
# global constant
kBreakDate <- as.Date('2014-7-15')
kUrl <- "G:\\DataMining\\ali_user_data\\data_desc.csv"  # csv file url
kClick <- 0.1
kBuy <- 1.0
kCollect <- 0.2
kBasket <- 0.5
# help function:
BehaviorScore <- function(behaviorcode){
  # Transform the type to score
  return((behaviorcode == '0') * kClick + (behaviorcode == '1') * kBuy
         + (behaviorcode == '2') * kCollect + (behaviorcode == '3') * kBasket
  )
}

MergeRow <-function(data.train){
  # Merge the row in matrix with same user_id and brand_id
  merge.matrix <- matrix(nrow = nrow(data.train), ncol = ncol(data.train))
  merge.matrix <- as.data.frame(merge.matrix)
  merge.matrix[1, 1] <- data.train[1, 1] #  assign first row of data.train to  merge.matrix  
  merge.matrix[1, 2] <- data.train[1, 2]
  merge.matrix[1, 3] <- data.train[1, 3]
  train.idx <- 2
  merge.idx <- 1
  while (train.idx <= nrow(data.train)){
    if (data.train[train.idx, 1] == data.train[train.idx - 1, 1] 
        & data.train[train.idx, 2] == data.train[train.idx-1, 2]){
      merge.matrix[merge.idx, 3] <- merge.matrix[merge.idx, 3] + data.train[train.idx, 3]
      
      train.idx <- train.idx + 1
    }else{
      merge.idx <- merge.idx + 1
      merge.matrix[merge.idx, ] <- data.train[train.idx, ]
      train.idx <- train.idx + 1
    }
  }
  return(merge.matrix[complete.cases(merge.matrix), ])
}
# data process
#

data.user <- read.csv(kUrl, header = TRUE, sep = ',', 
                      colClasses = c('character', 'character', 'character', 'Date'))


data.train <- data.user[data.user$visit_datetime < kBreakDate, ]
data.test <- data.user[data.user$visit_datetime >= kBreakDate, ]
rm('data.user')
score <- BehaviorScore(data.train[,3])
clear.data.train <- cbind(data.train[, c(-3,-4)], score)
data.train <- MergeRow(clear.data.train)
names(data.train) <- c('user_id', 'brand_id', 'score')
brand.user.matrix <-  cast(data.train, V1~V2, value = 'score')
# reshape data frame to brand~user matrix
brand.user.matrix <-  cast(data.train, brand_id~user_id, value = 'score')
brand.user.matrix <- brand.user.matrix[ ,-1] #  remove first cloumn
source('knn.R')
distances <- CalDistance(brand.user.matrix) # calculate distance matrix
# prediction evalution
data.test.buy <- data.test[data.test$type == '1', c(-3, -4)]
user.buy <- unique(data.test.buy$user_id)  #  users shopping during last month
source('eval.R')
CalRecall <- function(user.id, k = 20, n = 20){
  # take in user.id (which is string) and return the recall
  user.idx <- which(user.list == user.id)
  recommendation <- brand.list[BuyRecommendation(user.idx, brand.user.matrix, 
                                                 distances, k = k, n = n)]
  Recall(user.id, recommendation, data.test.buy)
}

result <- vector(mode='numeric', length = length(user.buy))
for (i in length(user.buy)){
  result[i] <- CalRecall(user.buy[i], k = 100, n = 100)
}