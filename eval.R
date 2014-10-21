# Evaluate the recommendation model.
Recall <- function(user.id, recommendation, data.test.buy){
  # 
  item.buy <- data.test.buy[data.test.buy$user_id == user.id, 2]
  
  return (length(intersect(item.buy, recommendation)) / length(item.buy))
}
