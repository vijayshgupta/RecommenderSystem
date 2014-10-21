# k-neareast-algorithm
# user-based CF
CalDistance <- (brand.user.matrix){
  # calculate distance matrix
  similarities <- cor(brand.user.matrix)
  return <- -log((similarities /2) + 0.5)  # transform correlation to distance
}

KNearestNeighbors <- function(i, distances, k = 20){
  # return k nearest neighbors
  # Args:
  # i : target element row number
  # distances: distance matrix
  # k: number of nearest neighbors
  return(order(distances[i, ])[2:(k + 1)])

}
 
BuyRecommendation <- function(user, brand.user.matrix, distances, k = 20, n = 10){
  # calculate the recommendation of k nearest users 
  # and return the recommendation list of n items.
  #Args:
  #user: interger user's 
  #barnd.user.matrix: matrix of brand and user
  #distances: distances matrix
  #k: number of neareast neighbors
  #n: number of recommendation items
  neighbors <- KNearestNeighbors(user, distances, k = k)
  raw.list <- order(apply(brand.user.matrix[, neighbors], 1, sum), decreasing = TRUE)
  recommendation <- raw.list[brand.user.matrix[, user] < 1]
  return(recommendation[1:n])
}
