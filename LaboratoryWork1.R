wineSet<-0
init<-function(){
  wineSet<<-read.csv("wine.data")
  names(wineSet)<<- c("Type","Alcoholh","Malic_acid","Ash","Alcalinity_of_ash","Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins","Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline")
}

showData<-function(dataSet){
  print("Atributes:")
  print(names(dataSet))
  print("Ranges:")
  print(summary(dataSet[-1]))
  for(i in 2:ncol(dataSet)){
    boxplot(x = dataSet[][i])
  }
}

#����	������� �������� (���������������); 
#����	������; 
#����	�������� ��������� ������������; --------------------------- 
#����	������� ����������� ��������� �� ����������; 
#����	�������� ������ ��������; 
#����	������;------------------- 
#����	��������; 
#����	���������� ������������ ���������� ��������;
#���� �� ������;--------------------- 
#����	����������� �� ��������� ��������; 
#����	�������� ���������� �� �����������; 
#����	��������� �� ��������;------------------------ 
#����	��������� �� �������.

#����	������� �������� (���������������); 
avgValue<-function(values){
  
  return(sum(values)/nrow(values))
}

#����	������; 
meanValue<-function(x){
  x <- sort(unlist(x, use.names = FALSE))
  if(length(x) %% 2 == 0) {
    m <- (x[length(x)/2] + x[length(x)/2 + 1])/2
  }else {
    m <- x[(length(x)+1)/2]
  }
  return(m)
}

#����	�������� ��������� ������������;
halfSumEdges<-function(values){
  return(sum(range(values))/2)
}

#����	������� ����������� ��������� �� ����������; 
avgSqr<-function(values){
  m <- avgValue(values)
  return(sqrt(avgValue((m-values)^2)))
}

#����	�������� ������ ��������; 
avgMod<-function(x){
  x <- unlist(x, use.names = FALSE)
  m <- meanValue(x)
  s <- sum(Mod(x - m))
  aM <- s / length(x)
  return(aM)
}

#����	������;
dataRange<-function(values){
  numPair<-range(values)
  return(numPair[2]-numPair[1])
}

#����	��������; 
dispersion<-function(values){
  m <- avgValue(values)
  return(avgValue((m-values)^2))
}

#����	���������� ������������ ���������� ��������;
recurentAvg<-function(x){
  x <- unlist(x, use.names = FALSE)
  newX <- vector(length = length(x))
  newX[1] <- x[1]
  for(k in 2:length(x)) newX[k] <- newX[k-1] + (1/k)*(x[k] - newX[k-1])
  return(newX)
}

#���� ���������� ������������ ������;
recurentMediana<-function(values){
  rMedValue<-vector(length = length(values))
  values<-unlist(values,use.names = F)
  rMedValue[1]<-values[1]
  for (k in 2:length(values)) {
    rMedValue[k]<-rMedValue[k-1] + (1/k)*(sign(values[k]-rMedValue[k-1]))
  }
  return(rMedValue)
}

#����	����������� �� ��������� ��������;
maxMin<-function(values){
  values1 <- unlist(values, use.names = FALSE)
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

#����	�������� ���������� �� �����������; 
normCenter<-function(x){
  Xsr <- avgValue(x)
  s <- avgSqr(x)
  newX <- (x - Xsr) / s
  return(newX)
}

#����	��������� �� ��������;
codingSphere<-function(values){
  maxMinValues<-maxMin(values)
  return(sapply(values, cdShapeFunc, maxValue=maxMinValues[1],minValue=maxMinValues[2]))
}
cdShapeFunc<-function(x,minValue,maxValue){
  return((x-minValue)/(maxValue-minValue))
}

#����	��������� �� �������.
codingCube<-function(values){
  maxMinValues<-maxMin(values)
  return(sapply(values, cdShapeFunc, maxValue=maxMinValues[1],minValue=maxMinValues[2]))
}
cdShapeFunc2<-function(x,minValue,maxValue){
  return((2*(x-minValue)/(maxValue-minValue))-1)
}

main<-function(){
  init()
  showData(wineSet)
  for (i in 2:ncol(wineSet)) {
    print(paste("-------",names(wineSet[i]),"-----------"))
    print(paste("�������:",avgValue(wineSet[i])))
    print(paste("�������:",meanValue(wineSet[i])))
    print(paste("��������� ������� ����������:",halfSumEdges(wineSet[i])))
    print(paste("�������������������� �����������:",avgSqr(wineSet[i])))
    print(paste("������� ������ ����������:",avgMod(wineSet[i])))
    print(paste("������:",dataRange(wineSet[i])))
    print(paste("���������:",dispersion(wineSet[i])))
    print("�����. �������:")
    tmp<-recurentAvg(wineSet[i]);
    for(j in 1:3){
      print(tmp[length(tmp)-j])
    }
    print("�����. �������:")
    tmp<-recurentMediana(wineSet[i]);
    for(j in 1:3){
      print(tmp[length(tmp)-j])
    }
    print("������������ ����������� ��������:")
    print(maxMin(wineSet[i]))
    
    print("������������ � �������������:")
    tmp<-normCenter(wineSet[i]);
    tmp<-unlist(tmp,use.names = F)
    for(j in 1:5){
      print(tmp[length(tmp)-j])
    }
    print("����������� �� ��������:")
    tmp<-codingSphere(wineSet[i]);
    tmp<-unlist(tmp,use.names = F)
    for(j in 1:5){
      print(tmp[length(tmp)-j])
    }
    
    print("����������� �� ��������:")
    tmp<-codingCube(wineSet[i]);
    tmp<-unlist(tmp,use.names = F)
    for(j in 1:5){
      print(tmp[length(tmp)-j])
    }
  }
}

