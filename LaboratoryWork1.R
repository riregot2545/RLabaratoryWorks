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
meanValue<-function(values){
  #TODO
  return(median(values))
}

#Дима	напівсуму «крайніх» спостережень;
halfSumEdges<-function(values){
  return(sum(range(values))/2)
}

#Саша	середнє квадратичне відхилення від середнього; 
avgSqr<-function(values){
  #TODO
  return(sd(values))
}

#Миша	середній модуль відхилень; 
avgMod<-function(values){
  #TODO
}

#Дима	розмах;
dataRange<-function(values){
  numPair<-range(values)
  return(numPair[2]-numPair[1])
}

#Саша	дисперсію; 
dispersion<-function(values){
  #TODO
  return(var(values))
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
  #TODO
  return(c(max(values),min(values)))
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
  #TODO
}

main<-function(){
  init()
  showData(wineSet)
  for (i in 2:ncol(wineSet)) {
    
  }
}

