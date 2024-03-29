dataSet <- 0
init <- function(){
  dataSet<<-read.csv("Data.dat")
  names(dataSet)<<- c("X0","V", "F", "C", "M", "Del")
  dataSet["Del"]<<- NULL
}

createMatrixVar1 <- function(dataSet){
  matr <- matrix(c(dataSet[2,1], dataSet[1, 3], dataSet[2, 5], dataSet[1, 2]), ncol = 4)
  for(k in 3:60){
    matr <- rbind(matr, c(dataSet[k,1], dataSet[k-1, 3], dataSet[k, 5], dataSet[k-1, 2]))
  }
  colnames(matr) <- c("X0", "F", "M", "V")
  return(matr)
}

createMatrixVar2 <- function(dataSet){
  matr <- matrix(c(dataSet[2,1], dataSet[1, 5], dataSet[1, 3], dataSet[2, 4]), ncol = 4)
  for(k in 3:60){
    matr <- rbind(matr, c(dataSet[k,1], dataSet[k-1, 5], dataSet[k-1, 3], dataSet[k, 4]))
  }
  colnames(matr) <- c("X0", "M", "F", "C")
  return(matr)
}

createMatrixVar3 <- function(dataSet){
  matr <- matrix(c(dataSet[2,1], dataSet[1, 4], dataSet[2, 2], dataSet[2, 3]), ncol = 4)
  for(k in 3:60){
    matr <- rbind(matr, c(dataSet[k,1], dataSet[k-1, 4], dataSet[k, 2], dataSet[k, 3]))
  }
  colnames(matr) <- c("X0", "C", "V", "F")
  return(matr)
}

createMatrixVar4 <- function(dataSet){
  matr <- matrix(c(dataSet[2,1], dataSet[1, 5], dataSet[2, 2], dataSet[1, 4]), ncol = 4)
  for(k in 3:60){
    matr <- rbind(matr, c(dataSet[k,1], dataSet[k-1, 5], dataSet[k, 2], dataSet[k-1, 4]))
  }
  colnames(matr) <- c("X0", "M", "V", "C")
  return(matr)
}

createMatrixVar5 <- function(dataSet){
  matr <- matrix(c(dataSet[2,1], dataSet[1, 5], dataSet[2, 3], dataSet[2, 2]), ncol = 4)
  for(k in 3:60){
    matr <- rbind(matr, c(dataSet[k,1], dataSet[k-1, 5], dataSet[k, 3], dataSet[k, 2]))
  }
  colnames(matr) <- c("X0", "M", "F", "V")
  return(matr)
}

createMatrixVar6 <- function(dataSet){
  matr <- matrix(c(dataSet[2,1], dataSet[1, 2], dataSet[2, 4], dataSet[2, 5]), ncol = 4)
  for(k in 3:60){
    matr <- rbind(matr, c(dataSet[k,1], dataSet[k-1, 2], dataSet[k, 4], dataSet[k, 5]))
  }
  colnames(matr) <- c("X0", "V", "C", "M")
  return(matr)
}

SearchKoeff <- function(matri ,dataSet, indexY){
  matri4x4 <- matri[1,] %*% t(matri[1,])
  for(k in 2:59){
    rowi <- matri[k,]
    revRowi <- t(matri[k,])
    matri4x4 <- matri4x4 + (rowi %*% revRowi)
  }
  revMatri4x4 <- solve(matri4x4)
  vector1x4 <- matri[1,] * dataSet[2, indexY]
  for(k in 2:59){
    rowi <- matri[k,]
    elementY <- dataSet[k+1, indexY]
    vector1x4 <- vector1x4 + (rowi*elementY)
  }
  resultMatri <- revMatri4x4 %*% vector1x4
  return(resultMatri)
}



main<-function(){
  init()
  matrix1 <- createMatrixVar1(dataSet)
  matrix2 <- createMatrixVar2(dataSet)
  matrix3 <- createMatrixVar3(dataSet)
  matrix4 <- createMatrixVar4(dataSet)
  matrix5 <- createMatrixVar5(dataSet)
  matrix6 <- createMatrixVar6(dataSet)
  print("������� ������������ ���������:")
  print(paste("������� 1: ",
              paste(unlist(SearchKoeff(matrix1,dataSet,4),use.names = F),collapse = " ")))
  print(paste("������� 2: ",
              paste(unlist(SearchKoeff(matrix2,dataSet,2),use.names = F),collapse = " ")))
  print(paste("������� 3: ",
              paste(unlist(SearchKoeff(matrix3,dataSet,5),use.names = F),collapse = " ")))
  print(paste("������� 4: ",
              paste(unlist(SearchKoeff(matrix4,dataSet,3),use.names = F),collapse = " ")))
  print(paste("������� 5: ",
              paste(unlist(SearchKoeff(matrix5,dataSet,4),use.names = F),collapse = " ")))
  print(paste("������� 6: ",
              paste(unlist(SearchKoeff(matrix6,dataSet,3),use.names = F),collapse = " ")))
}
