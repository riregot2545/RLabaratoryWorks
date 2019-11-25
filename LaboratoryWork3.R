irisSet<-0
init <- function(){
  irisSet<<-read.csv("iris.data")
  names(irisSet)<<- c("sepal length in cm", "sepal width in cm", "petal length in cm", "petal width in cm", "class")
  irisSet["class"] <<- NULL
}

codingCube <- function(values){
  minValue <- min(values)
  maxValue <- max(values)
  for(k in 1:length(values)){
    values[k] <- 2*(values[k] - minValue)/(maxValue - minValue) - 1
  }
  return(values)
}

avgValue <- function(values){
  return(sum(values)/length(values))
}

centering <- function(values){
  avgValue <- avgValue(values)
  for(k in 1:length(values)){
    values[k] <- values[k] - avgValue
  }
  return(values)
}

etapOne <- function(values){
  for(k in 1:ncol(values)){
    values[,k] <- codingCube(values[,k])
    values[,k] <- centering(values[,k])
  }
  return(values)
}

normVector <- function(values){
  summa <- 0
  for(k in 1:length(values)){
    summa <- summa + values[k]**2
  }
  return(sqrt(summa))
}

etapTwo <- function(values){
  w0 <- runif(ncol(values), min = -1, max = 1)
  w0 <- w0/normVector(w0)
  matrixW <<- matrix(w0, nrow = ncol(values), ncol = ncol(values))
  matrixY <<- matrix(nrow = nrow(values), ncol = ncol(values))
  for(k in 1:ncol(values)){
    matrixX <- matrix(nrow = nrow(values), ncol = ncol(values))
    for(i in 1:10**k){
      for(j in 1:nrow(values)){
        matrixY[j,k] <<- matrixW[,k] %*% unlist(values[j,], use.names = FALSE)
        matrixW[,k] <<- matrixW[,k] + (1/nrow(values)) * matrixY[j,k] * (unlist(values[j,], use.names = FALSE) - matrixY[j,k] * matrixW[,k])
        matrixW[,k] <<- matrixW[,k]/normVector(matrixW[,k])
      }
      matrixX <- matrixY[,k] %*% t(matrixW[,k])
    }
    values <- values - matrixX
  }
  return(values)
}

etapThree <- function(values){
  matrixX <- matrix(0, nrow = nrow(values), ncol = ncol(values))
  for(j in 1:nrow(values)){
    for(k in 1:ncol(values)){
      matrixX[j,] <- matrixX[j,] + matrixW[,k] * matrixY[j,k]
    }
  }
  return(matrixX)
}
