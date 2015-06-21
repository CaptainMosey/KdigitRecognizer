library(caret)
library(data.table)
library(randomForest)
set.seed(1492)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

#dataset is 40,000 , so 0.025 is ~1000 samples
inTrain<-createDataPartition(train$label,p=0.1875,list=F)

training<-train[inTrain,]

testingAndValidation<-train[-inTrain,]
print("part 1")
inTest<-createDataPartition(testingAndValidation$label,p=0.1875,list=F)
testing<-testingAndValidation[inTest,]
validation<-testingAndValidation[-inTest,]
print(paste("part2 ",Sys.time()))




#picture is 28x28
#find contiguous pixels
#cutoff=220#for considering full
#lowCutoff=50#for considering empty
cutoff=220#for considering full
lowCutoff=50#for considering empty


#looks for boundaries of dark, light and inbetween
process1<-function(dataSet=training,isTest=FALSE){
  if(isTest==FALSE) colAdd=1
  else colAdd=0
  ans=data.frame(matrix(nrow=0,ncol=477))
  #colnames(ans)=names
  #newMatrix<-matrix(dataSet[i,2:785],nrow=28,ncol=28,byrow=T)
  max=0
  for (i in 1:nrow(dataSet)){
    
    if (i%%100==0){
      print(paste("row number ",toString(i)," of dataSet",Sys.time()))
    }
    
    #make matrix 28x28
    newMatrix<-matrix(dataSet[i,(1+colAdd):(784+colAdd)],nrow=28,ncol=28,byrow=T)
    nextLine=data.frame(matrix(nrow=1,ncol=1))
      for(j in 1:28){
      #nextLine=data.frame(matrix(nrow=0,ncol=20))
      numZero=0
      numBlur=0
      numFull=0
      inZero=1
      inBlur=0
      inFull=0
      sumVector=vector(length=0)
      #scan matrix for adjacent values
      for(k in 1:28){
        a=newMatrix[j,k]
        if(inZero){
          if(a<lowCutoff) numZero=numZero+1
          else {
            inZero=0
            inBlur=1
            sumVector=append(sumVector,numZero)
            numZero=0
            numBlur=1
          }}
        else if (inBlur){
          if (a>lowCutoff & a<cutoff) numBlur=numBlur+1
          else if (a<lowCutoff){
            inZero=1
            inBlur=0
            sumVector=append(sumVector,numBlur)
            numBlur=0
            numZero=1
          } else{#a is full
            
            inFull=1
            inBlur=0
            sumVector=append(sumVector,numBlur)
            numBlur=0
            numFull=1                    
          }}
        else{
          if(a>cutoff) numFull=numFull+1
          else{
            inFull=0
            inBlur=1
            sumVector=append(sumVector,numFull)
            numFull=0
            numBlur=1
            
          }
        }      
        
        
        
      }
      #make vector same length
      #pattern always zero,blur,full,blur,zero...
      #,newdataframe<-rbind(newdataframe,vector
      if (length(sumVector)>max) max=length(sumVector)
      
      for(n in length(sumVector):17){
        sumVector<-append(sumVector,0)
        
        
      }
      #print(sumVector)
      #ans<-rbind(ans,c(dataSet[i,1],j,sumVector))
      
      nextLine[(1+(j-1)*17):((j*17))]<-sumVector[1:17]
      #nextLine<-cbind(nextLine,sumVector)

      #ans<-rbindlist(list(ans,nextLine),use.names=F)
      
    }
    if (isTest==FALSE) label=dataSet[i,1]
    else label=0
    ans<-rbindlist(list(ans,c(label,nextLine)),use.names=F)
    
    #print(max)
  }
  #colnames(ans)=names
  return(ans)
}

trainProData<-function(dataSet=trainSet){

  print("generate random forest")
  rfMod<-train(X1~.,data=dataSet,method="rf")
 
  
  print("model ready")
  return(rfMod)
}

trainProDataGBM<-function(dataSet=trainSet){
  
  print("generate boost")
  gbmMod<-train(X1~.,data=dataSet,method="gbm")

  
  print("model ready")
  return(gbmMod)
}


fitProData<-function(rfMod,testSet=testing){  
  

  rfTest<-predict(rfMod,testSet)
  print("applied model 1")
  testSet$predRight<-rfTest==testSet$X1
  print(table(rfTest,testSet$X1))
  print(paste(toString(sum(testSet$predRight))," of ",toString(nrow(testSet))))
  print((paste(toString(100*sum(testSet$predRight)/(nrow(testSet))),"%")))

  return(rfTest)
}


fitTestData<-function(rfMod,testSet=test){  
  
  rfTest<-predict(rfMod,testSet)
  print("applied model 1")
  
  return(rfTest)
}



runOnTest<-function(model,dataSet){
  
 

  print("processing test partition")
  competeProSet=processTest(dataSet)
  print("fitting test partition data")
  competeFit<-fitProData(model,competeProSet)
  
  
  return(competeFit)
  
  
}

trainSet<-process1()
trainSet$X1<-as.factor(trainSet$X1)
rfMod<-trainProData(trainSet)
print("processing validation partition")
testSet=process1(testing)
print("fitting validation partition data")
testFit<-fitProData(rfMod,testSet)
#competeProSet=process1(test,isTest=TRUE)
competeFit<-fitProData(rfMod,competeProSet)

write.csv(competeFit,paste("sub",Sys.time(),".csv",sep=""))
gbmFit<-fitProData(gbmMod,testSet)


#only dif from process1 is the lack of a label in columm 1
processTest<-function(dataSet=training){
  ans=data.frame(matrix(nrow=0,ncol=477))
  #colnames(ans)=names

  max=0
  for (i in 1:nrow(dataSet)){
  #for (i in 1:5000) {
    if (i%%100==0){
      print(paste("row number ",toString(i)," of dataSet",Sys.time()))
    }
    
    #make matrix 28x28
    newMatrix<-matrix(dataSet[i,1:784],nrow=28,ncol=28,byrow=T)
    nextLine=data.frame(matrix(nrow=1,ncol=1))
    for(j in 1:28){
      #nextLine=data.frame(matrix(nrow=0,ncol=20))
      numZero=0
      numBlur=0
      numFull=0
      inZero=1
      inBlur=0
      inFull=0
      sumVector=vector(length=0)
      #scan matrix for adjacent values
      for(k in 1:28){
        a=newMatrix[j,k]
        if(inZero){
          if(a<lowCutoff) numZero=numZero+1
          else {
            inZero=0
            inBlur=1
            sumVector=append(sumVector,numZero)
            numZero=0
            numBlur=1
          }}
        else if (inBlur){
          if (a>lowCutoff & a<cutoff) numBlur=numBlur+1
          else if (a<lowCutoff){
            inZero=1
            inBlur=0
            sumVector=append(sumVector,numBlur)
            numBlur=0
            numZero=1
          } else{#a is full
            
            inFull=1
            inBlur=0
            sumVector=append(sumVector,numBlur)
            numBlur=0
            numFull=1                    
          }}
        else{
          if(a>cutoff) numFull=numFull+1
          else{
            inFull=0
            inBlur=1
            sumVector=append(sumVector,numFull)
            numFull=0
            numBlur=1
            
          }
        }      
        
        
        
      }
      #make vector same length
      #pattern always zero,blur,full,blur,zero...
      #,newdataframe<-rbind(newdataframe,vector
      if (length(sumVector)>max) max=length(sumVector)
      
      for(n in length(sumVector):17){
        sumVector<-append(sumVector,0)
        
        
      }
      #print(sumVector)
      #ans<-rbind(ans,c(dataSet[i,1],j,sumVector))
      
      nextLine[(1+(j-1)*17):((j*17))]<-sumVector[1:17]
      #nextLine<-cbind(nextLine,sumVector)
      
      #ans<-rbindlist(list(ans,nextLine),use.names=F)
      
    }
    
    ans<-rbindlist(list(ans,c(0,nextLine)),use.names=F)
    
    #print(max)
  }
  #colnames(ans)=names
  return(ans)
}
