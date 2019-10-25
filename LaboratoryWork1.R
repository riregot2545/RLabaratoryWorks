
init<-function(){
  wineSet<-read.csv("C:/Users/Admin/Downloads/wine.data")
  names(wineSet)<- c("Type","Alcoholh","Malic_acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins","Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline")
}

showData<-function(){
  print("Atributes:")
  print(names(wineSet))
  print("Ranges:")
  summary(wineSet)
  #boxplot(x = as.list(wineSet))
  print("Boxplots:")
  for(i in 2:13){
    boxplot(x = wineSet[][i])
  }
}
