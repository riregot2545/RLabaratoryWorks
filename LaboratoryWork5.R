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
makeCentroids <- function(numOfClusters,dimension){
  centroids <- matrix(ncol = dimension,nrow = numOfClusters)
  centroids[1,]<-runif(dimension, min = -1, max = 1)
  for(i in 2:numOfClusters){
    centroids[i,]<-runif(dimension, min = -1, max = 1)
  }
  return(centroids)
}

#вычисление квадрата расстояния
calcDistance <- function(values, centroids,scaleMatrixAList){
  result <- matrix(nrow = nrow(values), ncol = nrow(centroids))
  for(i in 1:nrow(centroids)){
    resultPart1 <- t(unlist(values[1,], use.names = FALSE) - centroids[i,])
    resultPart2 <- unlist(values[1,], use.names = FALSE) - centroids[i,]
    result[1,i] <-  resultPart1 %*% solve(scaleMatrixAList[[i]]) %*% resultPart2 
    for(k in 2:nrow(values)){
      resultPart1 <- t(unlist(values[k,], use.names = FALSE) - centroids[i,])
      resultPart2 <- unlist(values[k,], use.names = FALSE) - centroids[i,]
      result[k,i] <- resultPart1 %*% solve(scaleMatrixAList[[i]]) %*% resultPart2 
    }
  }
  return(result)
}

calcMembershipRatio<- function(values, distanceMatrix){
  muMatrix<-matrix(ncol = ncol(distanceMatrix),nrow = nrow(values))
  for(i in 1:ncol(distanceMatrix)){
    for(j in 1:nrow(values)){
      sum<-0
      for(k in 1:ncol(distanceMatrix)){
        sum<-sum+distanceMatrix[j,i]/distanceMatrix[j,k]
      }
      muMatrix[j,i]<- sum^-1
    }
  }
  return(muMatrix)
}


#перерасчёт центроидов
recalcCentroids <- function(centroids,values,muMatrix){
  for(i in 1:nrow(centroids)){
    sum<-0
    for(j in 1:nrow(values)){
      sum<-sum + muMatrix[j,i]^2 * unlist(values[j,],use.names = F)
    }
    centroids[i,]<-sum/sum(muMatrix[,i]^2)
  }
  return(centroids)
}

recalcScaleMatrix<- function(centroids,values,muMatrix){
  newAMatrixesList<-list(matrix(nrow =nrow(values),ncol = ncol(values)))
  for(i in 1:nrow(centroids)){
    sumMatrix<-0
    for(j in 1:nrow(values)){
      uValues<-unlist(values[j,],use.names = F)
      sumMatrix<-sumMatrix+muMatrix[j,i]^2*((uValues-centroids[i,]) %*% t(uValues-centroids[i,]))
    }
    newAMatrixesList[[i]]<-((det(sumMatrix))^(1/4))*solve(sumMatrix)
  }
  return(newAMatrixesList)
}

main <- function(){
  init()
  irisSetCoded <- codingCube(irisSet)
  irisSetMixed <- mixSet(irisSetCoded)
  dimension<-ncol(irisSetMixed)
  
  centroids <- makeCentroids(3, dimension)
  scaleMatrixAList<-list(diag(dimension))
  for(i in 2:3){
    scaleMatrixAList[[i]]<-diag(dimension)
  }
  distanceMatrix<- calcDistance(irisSetMixed, centroids,scaleMatrixAList)
  muMatrix<- calcMembershipRatio(irisSetMixed, distanceMatrix)

  recalcedCentroids <- recalcCentroids(centroids,irisSetMixed,muMatrix)
  scaleMatrixAList <- recalcScaleMatrix(centroids,irisSetMixed,muMatrix)
  
  centoidsDelta<-norm((centroids-recalcedCentroids),type="2")^2
  while(centoidsDelta>0.03){
    centroids<-recalcedCentroids
    
    distanceMatrix<- calcDistance(irisSetMixed, centroids,scaleMatrixAList)
    muMatrix<- calcMembershipRatio(irisSetMixed, distanceMatrix)
    
    recalcedCentroids <- recalcCentroids(centroids,irisSetMixed,muMatrix)
    scaleMatrixAList <- recalcScaleMatrix(centroids,irisSetMixed,muMatrix)
    
    centoidsDelta<-norm((centroids-recalcedCentroids),type="2")^2
  }
  centroids<-recalcedCentroids
  classesFiltered<-list("class 1"=matrix(ncol=5),"class 2"=matrix(ncol=5),"class 3"= matrix(ncol=5))
  
  for(k in 1:nrow(irisSetMixed)){
    classIndex <- which.max(muMatrix[k,])
    classesFiltered[[classIndex]] <-rbind(classesFiltered[[classIndex]],c(muMatrix[k,classIndex],unlist(irisSetMixed[k,],use.names = F)) )
  }
  classesFiltered[[1]]<-classesFiltered[[1]][-1:0,]
  classesFiltered[[2]]<-classesFiltered[[2]][-1:0,]
  classesFiltered[[3]]<-classesFiltered[[3]][-1:0,]
  View(centroids)
  View(distCentroids)
  View(classesFiltered)
}
