library(caret)
library(data.table)
library(randomForest)
library(e1071)
library(mgcv)
library(tree)

set.seed(1492)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

#partition of 42,000 samples
# p=0.05 gives ~2000
# p=0.2 gives ~8000  (6 hours to build rf)
# p=0.0025 gives ~100 (40 seconds to build rf)
inTrain<-createDataPartition(train$label,p=0.1875,list=F)

training<-train[inTrain,]

testingAndValidation<-train[-inTrain,]
print("part 1")
inTest<-createDataPartition(testingAndValidation$label,p=0.6,list=F)
testing<-testingAndValidation[inTest,]
validation<-testingAndValidation[-inTest,]
print(paste("part2 ",Sys.time()))



#picture is 28x28
#find contiguous pixels
#no good reason these are global. woulsd probably be useful to vary values and try to optimize
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
    
    if (i%%1000==0){
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
      inBlurUp=0
      inBlurDown=0
      inFull=0
      sumVector=vector(length=0)
      #scan matrix for adjacent values
      for(k in 1:28){
        a=newMatrix[j,k]
        if(inZero){
          if(a<lowCutoff) numZero=numZero+1
          else {
            inZero=0
            inBlurUp=1
            sumVector=append(sumVector,numZero)
            numZero=0
            numBlur=1
          }}
        else if (inBlurUp){
          if (a>lowCutoff & a<cutoff) numBlur=numBlur+1
          else {
            inFull=1
            inBlurUp=0
            sumVector=append(sumVector,numBlur)
            numBlur=0
            numFull=1
          } }
        else if (inBlurDown){
          if (a>lowCutoff & a<cutoff) numBlur=numBlur+1
          else {
            inZero=1
            inBlurDown=0
            sumVector=append(sumVector,numBlur)
            numBlur=0
            numZero=1
          } }
        else{
          if(a>cutoff) numFull=numFull+1
          else{
            inFull=0
            inBlurDown=1
            sumVector=append(sumVector,numFull)
            numFull=0
            numBlur=1
          }
        }        
      }
      
      if (length(sumVector)>max) max=length(sumVector)
      
      for(n in length(sumVector):17){
        sumVector<-append(sumVector,0) 
        nextLine[(1+(j-1)*17):((j*17))]<-sumVector[1:17]
      }}
    if (isTest==FALSE) label=dataSet[i,1]
    else label=0
    ans<-rbindlist(list(ans,c(label,nextLine)),use.names=F)
    
  }
  
  return(ans)
}
trainSet<-process1()
trainSet$X1<-as.factor(trainSet$X1)
testSet=process1(testing)
validSet=process1(validation)

competeSet=process1(dataSet=test,isTest=TRUE)
        
        
        fitProData<-function(rfMod,testSet=testing){  
          
          
          rfTest<-predict(rfMod,testSet)
          print("applied model 1")
          testSet$predRight<-rfTest==testSet$X1
          print(table(rfTest,testSet$X1))
          print(paste(toString(sum(testSet$predRight))," of ",toString(nrow(testSet))))
          print((paste(toString(100*sum(testSet$predRight)/(nrow(testSet))),"%")))
          
          return(rfTest)
        }
        
        #develop on training
        print(Sys.time())
        rfMod<-train(X1~.,data=trainSet,method="rf")
        
        print(Sys.time())
        gbmMod<-train(.outcome~.,data=trainSet,method="gbm")
        print(Sys.time())
        
        
        svmMod<-best.svm(.outcome~.,data=trainSet)
        print(Sys.time())
        
        testRFFit<-fitProData(rfMod,testSet)
        gbmFit<-fitProData(gbmMod,testSet)
        svmFit<-fitProData(svmMod,testSet)
        tFrame=data.frame(matrix(ncol=1,nrow=nrow(testSet)))
        colnames(tFrame)="Label"
        tFrame$Label=testSet$X1
        tFrame$Label=as.factor(tFrame$Label)
        tFrame$rf=testRFFit
        #tFrame$gbm=gbmFit
        tFrame$svm=svmFit
        
        cMod=best.svm(Label~.,data=tFrame)
        
        
        
        
        #check out blend on validation partitioon
        
        rfValidFit<-predict(rfMod,validSet)
        #gbmValidFit<-predict(gbmMod,validSet)
        svmValidFit<-predict(svmMod,validSet)
        vFrame=data.frame(matrix(ncol=1,nrow=nrow(validSet)))
        colnames(vFrame)="Label"
        vFrame$Label=validSet$X1
        vFrame$Label=as.factor(vFrame$Label)
        vFrame$rf=rfValidFit
        #vFrame$gbm=gbmValidFit
        vFrame$svm=svmValidFit
        vFit=predict(cMod,vFrame)
        vFrame$fit=vFit
        print(table(vFit,vFrame$Label))
        sum(vFit==vFrame$Label)
        sum(vFit==vFrame$Label)/length(vFit)
        
        #************************************************
        #run on competition data and save

        
        rfCompFit<-predict(rfMod,competeSet)
        gbmCompFit<-predict(gbmMod,competeSet)
        svmCompFit<-predict(svmMod,competeSet)
        competeFrame=data.frame(matrix(ncol=1,nrow=nrow(competeSet)))
        colnames(competeFrame)="Label"
        competeFrame$Label=-1
        competeFrame$Label=as.factor(competeFrame$Label)
        
        competeFrame$rf=rfCompFit
        competeFrame$gbm=gbmCompFit
        competeFrame$svm=svmCompFit
        
        comFit=predict(cMod,competeFrame)
        competeFrame$fit=comFit
        
        #error in gbm fit. still cant beat svm on its own!
        write.csv(comFit,paste("sub",Sys.time(),".csv",sep=""))
      