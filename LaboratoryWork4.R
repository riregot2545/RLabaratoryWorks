irisSet<-0
init<-function(){
  irisSet<<-read.csv("iris.data")
  names(irisSet)<<- c("sepal length in cm", "sepal width in cm", "petal length in cm", "petal width in cm", "class")
  irisSet["class"] <<- NULL
}

#кодирование выборки
codingCube <- function(values){
  for(l in 1:ncol(values)){
    minValue <- min(values[,l])
    maxValue <- max(values[,l])
    for(k in 1:nrow(values)){
      values[k,l] <- 2*(values[k,l] - minValue)/(maxValue - minValue) - 1
    }
  }
  return(values)
}

#перемешивание выборки
mixSet <- function(values){
  mixedSet <- values[sample(nrow(values)),]
  row.names(mixedSet) <- seq(length = nrow(mixedSet))
  return(mixedSet)
}

#генерация центроида
makeCentroids <- function(){
  centroid1 <- runif(4, min = -1, max = 1)
  centroid2 <- runif(4, min = -1, max = 1)
  centroid3 <- runif(4, min = -1, max = 1)
  centroids <- cbind(centroid1, centroid2, centroid3)
  return(centroids)
}

#вычисление расстояния
calcDistance <- function(values, centroids){
  result <- matrix(nrow = nrow(values), ncol = ncol(centroids))
  for(l in 1:ncol(centroids)){
    resultPart1 <- t(unlist(values[1,], use.names = FALSE) - centroids[,l])
    resultPart2 <- unlist(values[1,], use.names = FALSE) - centroids[,l]
    result[1,l] <-  sqrt(resultPart1 %*% resultPart2) 
    for(k in 2:nrow(values)){
      resultPart1 <- t(unlist(values[k,], use.names = FALSE) - centroids[,l])
      resultPart2 <- unlist(values[k,], use.names = FALSE) - centroids[,l]
      result[k,l] <- sqrt(resultPart1 %*% resultPart2)
    }
  }
  return(result)
}

#поиск минимальной дистанции
searchMinDist <- function(values){
  listMin <- which.min(unlist(values[1,],use.names = FALSE))
  for(k in 2:nrow(values)){
    listMin <- c(listMin, which.min(unlist(values[k,],use.names = FALSE)))
  }
  return(listMin)
}

#перерасчёт центроидов
recalcCentroids <- function(dataSet, distSet){
  listMin <- searchMinDist(distSet)
  recalcedCentroids <- matrix(0, nrow = 4, ncol = 3)
  quantity <- c(0,0,0)
  for(k in 1:nrow(dataSet)){
    recalcedCentroids[,listMin[k]] <- recalcedCentroids[,listMin[k]] + unlist(dataSet[k,], use.names = FALSE)
    quantity[listMin[k]] <- quantity[listMin[k]] + 1
  }
  for(k in 1:ncol(recalcedCentroids)){
    recalcedCentroids[,k] <- recalcedCentroids[,k]/quantity[k]
  }
  return(recalcedCentroids)
}

main <- function(){
  init()
  irisSet2 <- codingCube(irisSet)
  irisSet3 <- mixSet(irisSet2)
  centroids <- makeCentroids()
  distCentroids <- calcDistance(irisSet3, centroids)
  recalcedCentroids <- recalcCentroids(irisSet3, distCentroids)
}
