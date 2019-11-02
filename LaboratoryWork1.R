wineSet<-0
init<-function(){
  wineSet<-read.csv("C:/Users/Admin/Downloads/wine.data")
  names(wineSet)<- c("Type","Alcoholh","Malic_acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins","Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline")
}

showData<-function(dataSet){
  print("Atributes:")
  print(names(dataSet))
  print("Ranges:")
  summary(dataSet)
  #boxplot(x = as.list(dataSet))
  print("Boxplots:")
  for(i in 2:ncol(dataSet)){
    boxplot(x = dataSet[][i])
  }
}

#Саша	середнє значення («маточікування»); 
#Миша	медіану; 
#Дима	напівсуму «крайніх» спостережень; --------------------------- 
#Саша	середнє квадратичне відхилення від середнього; 
#Миша	середній модуль відхилень; 
#Дима	розмах;------------------- 
#Саша	дисперсію; 
#Миша	рекурентні співвідношення середнього значення;
#Дима та медіани;--------------------- 
#Саша	максимальне та мінімальне значення; 
#Миша	провести нормування та центрування; 
#Дима	кодування на гіперкулю;------------------------ 
#Саша	кодування на гіперкуб.

#Саша	середнє значення («маточікування»); 
avgValue<-function(values){
  #TODO
  return(mean(values))
}

#Миша	медіану; 
meanValue<-function(x){
  x <- sort(unlist(x, use.names = FALSE))
  if(length(x) %% 2 == 0) {
    m <- (x[length(x)/2] + x[length(x)/2 + 1])/2
  }else {
    m <- x[(length(x)+1)/2]
  }
  return(m)
}

#Дима	напівсуму «крайніх» спостережень;
halfSumEdges<-function(values){
  #TODO
}

#Саша	середнє квадратичне відхилення від середнього; 
avgSqr<-function(values){
  #TODO
  return(sd(values))
}

#Миша	середній модуль відхилень; 
avgMod<-function(x){
  x <- unlist(x, use.names = FALSE)
  m <- meanValue(x)
  s <- sum(Mod(x - m))
  aM <- s / length(x)
  return(aM)
}

#Дима	розмах;
dataRange<-function(values){
  #Todo
}

#Саша	дисперсію; 
dispersion<-function(values){
  #TODO
  return(var(values))
}

#Миша	рекурентні співвідношення середнього значення;
recurentAvg<-function(x){
  x <- unlist(x, use.names = FALSE)
  newX <- vector(length = length(x))
  newX[1] <- x[1]
  for(k in 2:length(x)) newX[k] <- newX[k-1] + (1/k)*(x[k] - newX[k-1])
  return(newX)
}

#Дима рекурентні співвідношення медіани;
recurentMediana<-function(values){
  #TODO
}

#Саша	максимальне та мінімальне значення;
maxMin<-function(values){
  #TODO
  return(c(max(values),min(values)))
}

#Миша	провести нормування та центрування; 
normCenter<-function(x){
  Xsr <- avgValue(x)
  s <- avgSqr(x)
  newX <- (x - Xsr) / s
  return(newX)
}

#Дима	кодування на гіперкулю;
codingSphere<-function(values){
  #TODO
}

#Саша	кодування на гіперкуб.
codingCube<-function(values){
  #TODO
}

main<-function(){
  init()
  showData(wineSet)
  for (i in 2:ncol(wineSet)) {
    
  }
}

