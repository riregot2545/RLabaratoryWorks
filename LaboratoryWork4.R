irisSet<-0
init<-function(){
  irisSet<<-read.csv("iris.data")
  names(irisSet)<<- c("sepal length in cm", "sepal width in cm", "petal length in cm", "petal width in cm", "class")
}

codingCube <- function(values){
  for(l in 1:(ncol(values) - 1)){
    minValue <- min(values[,l])
    maxValue <- max(values[,l])
    for(k in 1:nrow(values)){
      values[k,l] <- 2*(values[k,l] - minValue)/(maxValue - minValue) - 1
    }
  }
  return(values)
}

mixSet <- function(values){
  mixedSet <- values[sample(nrow(values)),]
  row.names(mixedSet) <- seq(length = nrow(mixedSet))
  return(mixedSet)
}

makeCentroids <- function(){
  centroid1 <- runif(4, min = -1, max = 1)
  centroid2 <- runif(4, min = -1, max = 1)
  centroid3 <- runif(4, min = -1, max = 1)
  centroids <- cbind(centroid1, centroid2, centroid3)
  return(centroids)
}

calcDistance <- function(values, centroids){
  result <- matrix(nrow = nrow(values), ncol = ncol(centroids))
  for(l in 1:ncol(centroids)){
    resultPart1 <- t(unlist(values[1, 1:4], use.names = FALSE) - centroids[,l])
    resultPart2 <- unlist(values[1, 1:4], use.names = FALSE) - centroids[,l]
    result[1,l] <-  sqrt(resultPart1 %*% resultPart2) 
    for(k in 2:nrow(values)){
      resultPart1 <- t(unlist(values[k, 1:4], use.names = FALSE) - centroids[,l])
      resultPart2 <- unlist(values[k, 1:4], use.names = FALSE) - centroids[,l]
      result[k,l] <- sqrt(resultPart1 %*% resultPart2)
    }
  }
  return(result)
}

main <- function(){
  init()
  irisSet2 <- codingCube(irisSet)
  irisSet3 <- mixSet(irisSet2)
  centroids <- makeCentroids()
  distCentroids <- calcDistance(irisSet3, centroids)
  
}
