wineSet<-0
init <- function(){
  wineSet<<-read.csv("wine.data")
  names(wineSet)<<- c("Type","Alcoholh","Malic_acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins","Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline")
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
  for(k in 2:ncol(values)){
    values[,k] <- codingCube(values[,k])
    values[,k] <- centering(values[,k])
  }
  return(values)
}

etapTwo <- function(values){
  
}

#ÂÍÈÌÀÍÈÅ!!! íå çàâåðøåíî
calcComponent <- function(values){
  w <- runif(ncol(values), min = -1, max = 1)
  w <- w/normVector(w)
  y0 <- t(w) %*% unlist(wineSet[1, 2:14], use.names = FALSE)
  w <- w + (1/nrow(values))*y0
  for(k in 2:nrow(values)){
    w <- w + (1/nrow(values))*y0
  }
}

normVector <- function(values){
  summa <- 0
  for(k in 1:length(values)){
    summa <- summa + values[k]**2
  }
  return(sqrt(summa))
}
