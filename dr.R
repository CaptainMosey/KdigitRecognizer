


library(caret)
set.seed(1492)

inTrain<-createDataPartition(train$label,p=0.05,list=F)

training<-train[inTrain,]

testingAndValidation<-train[-inTrain,]
print("part 1")
inTest<-createDataPartition(testingAndValidation$label,p=.025,list=F)
testing<-testingAndValidation[inTest,]
validation<-testingAndValidation[-inTest,]
print("part2")

#RFmodel=train(label~.,training,method="rf")
#print("model done")
#RfFit<-predict(RFmodel,testing)

names=c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")
#picture is 28x28
#find contiguous pixels
cutoff=220#for considering full
lowCutoff=50#for considering empty


#looks for boundaries of dark, light and inbetween
process1<-function(dataSet=training){
  ans=data.frame(matrix(nrow=0,ncol=20))
  colnames(ans)=names

  max=0
  for (i in 1:nrow(dataSet)){
    if (i%%100==0)print(paste("row number ",toString(i)," of dataSet"))
    
    #make matrix 28x28
    newMatrix<-matrix(dataSet[i,2:785],nrow=28,ncol=28,byrow=T)
    
    for(j in 1:28){
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
      
      for(n in length(sumVector):18){
        sumVector<-append(sumVector,0)
        
        
      }
      #print(sumVector)
      ans<-rbind(ans,c(dataSet[i,1],j,sumVector))
      
      
    }
    
    
    
    #print(max)
  }
  colnames(ans)=names
  return(ans)
}

trainProData<-function(dataSet=trainSet){
  #colnames(trainSet)=names
  rfMod<-train(X1~.,data=dataSet,method="rf")
  print("fit model 1")
  return(rfMod)
}


fitProData<-function(rfMod,testSet=testing){  
  
  rfTest<-predict(rfMod,testSet)
  print("applied model 1")
  testSet$predRight<-rfTest==testSet$X1
  print(table(rfTest,testSet$X1))
  return(testSet)
}



trainSet<-process1()
trainSet$X1<-as.factor(trainSet$X1)
rfMod<-trainProData(trainSet)
testSet=process1(testing)
testSet<-fitProData(rfMod,testSet)


#testing$predRight<-RfFit==testing$label

#print(table(RfFit,testing$label))

#tot=sum(testing$predRight)

#print(tot)
#print(tot/nrow(testing))