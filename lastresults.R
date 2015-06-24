> testRFFit<-fitProData(rfMod,testSet)
[1] "applied model 1"

rfTest    0    1    2    3    4    5    6    7    8    9
0 1971    0    8    6   24   10   15    1   17    7
1    1 2244   10   13    4   15    4   15    5    0
2    2    7 1869   69    2   21    3   33   25    1
3    4    8   28 1808    1   86    0   20   12    7
4    6    4    5    2 1824   11   15   22   14   55
5    5   14   12  104    9 1590   51   10   42   18
6   19    5   20    5   18   15 1931    0    5    0
7    4    4   11   29    6   11    0 2001    2   41
8    8    9   46   29   14   23    5    2 1839   22
9    4    2    4   43   96   36    0   66   40 1870
[1] "18947  of  20474"
[1] "92.5417602813324 %"
> svmMod<-best.svm(.outcome~.,data=trainSet)
There were 11 warnings (use warnings() to see them)
> arguments imply differing number of rows: 0, 2904
Error: unexpected symbol in "arguments imply"
> svmFit<-fitProData(svmMod,testSet)
[1] "applied model 1"

rfTest    0    1    2    3    4    5    6    7    8    9
0 1982    2    9    3    3    8    9    1   11    6
1    1 2240    7    8    3    8    5   14    3    1
2   10   11 1913   69    2   11    7   17   14    3
3    0    6   12 1881    1   76    0   24    3   10
4    1    5    4    2 1862    8    5    9   11   51
5    6    9   11   76    4 1653    8   11   20   14
6   15    6    7    2   14   15 1970    0    5    0
7    0    4    9   19    3    8    0 2012    8   41
8    8    7   37   26    5   17   20    6 1902   16
9    1    7    4   22  101   14    0   76   24 1879
[1] "19294  of  20474"
[1] "94.2365927517828 %"
> svmValidFit<-fitProData(svmMod,validSet)
[1] "applied model 1"

rfTest    0    1    2    3    4    5    6    7    8    9
0 1336    0    5    3    1    5   19    1    8    4
1    0 1470    8   12    2    3    1   10    5    1
2    1    9 1287   30    1   10    9   17   11    0
3    2    2   11 1280    0   48    0   11    1    7
4    1    4    1    1 1205    3    3    3    7   38
5    4    9    2   64    4 1183    4    9   15    8
6    9    2    5    0   12    9 1243    0   10    0
7    0    2   15   23    1    4    0 1328    6   29
8    6    9   15   21    1   10   11    4 1258    8
9    0    6    1    7   69   15    0   45   18 1247
[1] "12837  of  13648"
[1] "94.0577373974209 %"
> gbmFit<-fitProData(gbmMod,testSet)
Error in predict(rfMod, testSet) : object 'gbmMod' not found
> tFrame=data.frame(matrix(ncol=1,nrow=nrow(testSet)))
> colnames(tFrame)="Label"
> tFrame$Label=testSet$X1
> tFrame$Label=as.factor(tFrame$Label)
> tFrame$rf=testRFFit
> #tFrame$gbm=gbmFit
  > tFrame$svm=svmFit
> 
  > cMod=best.svm(Label~.,data=tFrame)
> rfValidFit<-predict(rfMod,validSet)
> #gbmValidFit<-predict(gbmMod,validSet)
  > svmValidFit<-predict(svmMod,validSet)
> vFrame=data.frame(matrix(ncol=1,nrow=nrow(validSet)))
> colnames(vFrame)="Label"
> vFrame$Label=validSet$X1
> vFrame$Label=as.factor(vFrame$Label)
> vFrame$rf=rfValidFit
> #vFrame$gbm=gbmValidFit
  > vFrame$svm=svmValidFit
> vFit=predict(cMod,vFrame)
> vFrame$fit=vFit
> print(table(vFit,vFrame$Label))

vFit    0    1    2    3    4    5    6    7    8    9
0 1317    1    9    1    1    5   12    2    7    3
1    0 1468    5   12    1    3    3    6    4    1
2    1    9 1275   25    1    9    9   18   20    0
3   10    2   28 1290    0   51    5   11    3    7
4    1    4    1    1 1204    3    3    3    7   38
5   12    9    3   64    4 1181    8    9   17    8
6    8    1    4    0   12    9 1239    0    8    0
7    0    5   17   23    3    3    0 1327    3   24
8    7    8    5   18    1   10   11    3 1248    8
9    3    6    3    7   69   16    0   49   22 1253
> sum(vFit==vFrame$Label)
[1] 12802
> sum(vFit==vFrame$Label)/length(vFit)
[1] 0.9380129