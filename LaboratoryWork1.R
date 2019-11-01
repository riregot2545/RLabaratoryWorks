wineSet<-0
init<-function(){
  wineSet<-read.csv("D:/Programming/IAD/wine.data")
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
  
  return(sum(values)/nrow(values))
}

#Миша	медіану; 
meanValue<-function(values){
  #TODO
  return(median(values))
}

#Дима	напівсуму «крайніх» спостережень;
halfSumEdges<-function(values){
  #TODO
}

#Саша	середнє квадратичне відхилення від середнього; 
avgSqr<-function(values){
  m <- avgValue(values)
  return(sqrt(avgValue((m-values)^2)))
}

#Миша	середній модуль відхилень; 
avgMod<-function(values){
  #TODO
}

#Дима	розмах;
dataRange<-function(values){
  #Todo
}

#Саша	дисперсію; 
dispersion<-function(values){
  m <- avgValue(values)
  return(avgValue((m-values)^2))
}

#Миша	рекурентні співвідношення середнього значення;
recurentAvg<-function(values){
  #TODO
}

#Дима рекурентні співвідношення медіани;
recurentMediana<-function(values){
  #TODO
}

#Саша	максимальне та мінімальне значення;
maxMin<-function(values){
  values1 <- unlist(values)
  max <- 0
  min <- 99999999
  for (j in 1:length(values1)){
    if(values1[j] > max){
      max <- values1[j]
    }
    if(values1[j] < min){
      min <- values1[j]
    }
  }
  return(c(max,min))
}

#Миша	провести нормування та центрування; 
normCenter<-function(values){
  #TODO
}

#Дима	кодування на гіперкулю;
codingSphere<-function(values){
  #TODO
}

#Саша	кодування на гіперкуб.
codingCube<-function(values){
  maxMinValues<-maxMin(values)
  return(sapply(values, cdShapeFunc, maxValue=maxMinValues[1],minValue=maxMinValues[2]))
}
cdShapeFunc<-function(x,minValue,maxValue){
  return((2*(x-minValue)/(maxValue-minValue))-1)
}

main<-function(){
  init()
  showData(wineSet)
  for (i in 1:ncol(wineSet)) {
    a <- avgValue(wineSet[i])
    s <- avgSqr(wineSet[i])
    d <- dispersion(wineSet[i])
    m <- maxMin(wineSet[i])
    c <- codingCube(wineSet[i])
    print(c)
  }
  
  
  
  
}

