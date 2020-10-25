

#### groups for four fold cross validation misclassification rate

g1 = 1:368
g2 = 369:(368*2)
g3 = (368*2+1):(368*3)
g4 = (368*3+1):(368*4)


Groups = data.frame(g1,g2,g3,g4)


## using various models to try and predict what contraceptive 
## method married women in Indonesia use 

nrow(cmc) #1473 rows so removing one to make it divisble by 4
cmc2 = cmc[1:1472,]

1472/4 #368

## going to predict whether or not someone uses any contraceptive method or not
## we have to change the CMC column to binary since now it is 1 (no use), 2(long-term), 3(short-term)

cmc2$CMC[cmc2$CMC == 1] = "NO"
cmc2$CMC[cmc2$CMC == 2 | cmc2$CMC == 3] = "YES"

# now it is a binary variable with "NO" representing
# using no contraceptive methods and "YES" representing
# using contraceptive methods

# shuffling the rows randomly
set.seed(1472)
rows = sample(nrow(cmc2))
cmc2 = cmc2[rows,]

#############################################
#############################################
#            LOGISTIC REGRESSION            #
#############################################
#############################################

# going to predict CMC which is a binary var


  ##############################
  # USING ALL VARIABLES
  ##############################




# model using all the predictor variables to predict CMC
data = cmc2
data$CMC[data$CMC == "NO"] = 0
data$CMC[data$CMC == "YES"] = 1
data$CMC = as.numeric(data$CMC)

mod = glm(CMC~., data = data)
pred = (predict(mod) > .5) * 1

#misclassification rate 
sum(data$CMC != pred)/nrow(data) #0.3165761


err = logical(0)
for(i in 1:4){
  mod=glm(CMC~., data=data[-Groups[,i],]) 
  
  pred = (predict(mod,newdata=data[Groups[,i],]) > .5)*1
  
  err=c(err,data$CMC[Groups[,i]] != pred)
}

#misclassification rate
misclass1 = sum(err) / nrow(data) #0.3254076
misclass1




  ###############################################
  # FINDING THE OPTIMAL NUMBER OF PRED VARIABLES
  ###############################################

# correlations between numeric columns
cor(cmc2[,c(1,4)])
# correlation of 0.5397181

  ###############################################
  # BACKWARDS SELECTION - AIC/BIC
  ###############################################

library(boot)


backStep = step(glm(CMC~.,data=data))
pred = (predict(backStep) > .5) * 1
sum(cmc2$wife_relig != pred)/nrow(cmc2) #0.4110054


backStep$call #lm(formula = wife_relig ~ wife_age + wife_ed + num_children + hus_occupation + SOL_index + CMC, data = cmc2)
  # resulting model
lm(formula = CMC ~ wife_age + wife_ed + num_children + wife_relig + 
     SOL_index + media_exposure, data = data)


# obtaining 4 fold cross validation misclassification rate
err = logical(0)
for(i in 1:4){
  mod=glm(formula = CMC ~ wife_age + wife_ed + num_children + wife_relig + 
           SOL_index + media_exposure, data = data[-Groups[,i],]) 
  # the formula was from backStep$call above
  pred=(predict(mod,newdata=data[Groups[,i],]) > .5)*1 
  
  err=c(err,data$CMC[Groups[,i]] != pred)
}
misclass2 = sum(err) / nrow(cmc2) #0.3199728
misclass2

  ###############################################
  # FORWARD SELECTION - AIC/BIC
  ###############################################


forwardStep = step(lm(CMC~1,data=data),direction="forward",scope=list(lower=lm(CMC~1,data=data),upper=lm(CMC~.,data=data)))
pred = (predict(forwardStep) > .5) * 1
sum(data$CMC != pred)/nrow(data) #0.3179348

forwardStep$call #lm(formula = CMC ~ wife_ed + num_children + wife_age + SOL_index + wife_relig + media_exposure, data = data)
  # model
lm(formula = CMC ~ wife_ed + num_children + wife_age + SOL_index + wife_relig + media_exposure, data = data)


# obtaining 4 fold cross validation misclassification rate
err = logical(0)
for(i in 1:4){
  mod=lm(formula = CMC ~ wife_ed + num_children + wife_age + SOL_index + wife_relig + media_exposure, data = data[-Groups[,i],]) 
  # the formula was from forwardStep$call above
  pred=(predict(mod,newdata=data[Groups[,i],]) > .5)*1 
  
  err=c(err,data$CMC[Groups[,i]] != pred)
}
misclass3 = sum(err) / nrow(data) #0.3199728
misclass3

  ###########################################
  # BACKWARDS SELECTION - CV
  ###########################################

  library(boot)

  
  data = cmc2
  data$CMC[data$CMC == "NO"] = 0
  data$CMC[data$CMC == "YES"] = 1
  data$CMC = as.numeric(data$CMC)

  #start with all the columns
  cv.glm(data,glm(CMC~.,data=data),K=10)$delta[1] #MSE = 0.210553

  res=c()
  for(k in 1:(ncol(data) - 1)){ ## data has 10 columns with one being the response var 
    model=glm(CMC~.,data = data[,-k])
    res=c(res,cv.glm(data,model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest at the 6th iteration when k = 6
      # which corresponds to wife_working is removed with an MSE of 0.2106271
  toRm = (1:length(res))[res==min(res)]
  
  data = data[,-toRm] # removing wife_working
  
  res=c()
  for(k in 1:(ncol(data) - 1)){  
    model=glm(CMC~.,data = data[,-k])
    res=c(res,cv.glm(data,model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest when col 6 (hus_occupation) is removed which has an MSE of 0.2103408
  toRm = (1:length(res))[res==min(res)]
  
  data = data[,-toRm] # removing hus_occ
  
  
  
  res=c()
  for(k in 1:(ncol(data) - 1)){  
    model=glm(CMC~.,data = data[,-k])
    res=c(res,cv.glm(data,model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest when col 3 (hus_ed) is removed which has an MSE of 0.2105919
  # THIS IS A GREATER MSE THAN THE PREVIOUS MODEL 
  
  # final  model 
  mod = lm(formula = CMC ~., data = data) 
  pred = (predict(mod) > .5) * 1
  sum(data$CMC != pred)/nrow(cmc2) #0.3179348
  
  # obtaining 4 fold cross validation misclassification rate
  err = logical(0)
  for(i in 1:4){
    mod=lm(formula = CMC~., data = data[-Groups[,i],]) 
    # using all predictor vars left in data
    pred=(predict(mod,newdata=data[Groups[,i],]) > .5)*1 
    err=c(err,data$CMC[Groups[,i]] != pred)
  }
  misclass4 = sum(err) / nrow(cmc2) #0.3233696
  misclass4
  
  ###########################################
  # FORWARDS SELECTION - CV
  ###########################################

  library(boot)
  
  #resetting data
  data = cmc2
  data$CMC[data$CMC == "NO"] = 0
  data$CMC[data$CMC == "YES"] = 1
  data$CMC = as.numeric(data$CMC)
  
  data = cbind(data[10], data[1:9])
  # getting the data with CMC (our response var)
  # at the beginning so that the loop can ignore it as a variable
  # to be taken out in backward/forward selection
  
  ### STARTING WITH JUST THE RESPONSE VAR CMC
  newData = data[1] #just response var for now
  # start by adding one column at at a time
  
  allMinRes=c() #to keep track of all the min MSEs 
  
  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) # with first col (response) and each k predictor
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the second iteration
      #       where k=3 which corresponds to the third column in data
      #       (wife_ed); MSE is 0.2305309
  allMinRes = c(allMinRes, min(res))
  toAdd = (2:ncol(data))[res==min(res)]
  
  #### ADDING WIFE_ED
  newData = cbind(newData, data[toAdd]) # adding wife_ed
  data = data[-toAdd] 
  #removing the col wife_ed from data so we can go through adding the other options
  
  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) 
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the third iteration
      #       with k=4 which corresponds to the fourth column
      #       of data (num_children); MSE is 0.2241662
  allMinRes = c(allMinRes, min(res))
  toAdd = (2:ncol(data))[res==min(res)] 
  
  #### ADDING NUM_CHILDREN
  newData = cbind(newData, data[toAdd]) 
  data = data[-toAdd] 
  #removing the col num_children from data so we can go through adding the other options

  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) 
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the first iteration
  #       with k=2 which corresponds to the second column
  #       of data (wife_age); MSE is 0.2147407
  allMinRes = c(allMinRes, min(res))
  toAdd = (2:ncol(data))[res==min(res)]
  
  ### ADDING WIFE_AGE
  newData = cbind(newData, data[toAdd]) 
  data = data[-toAdd] 
  #removing the col wife_age from data so we can go through adding the other options
  
  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) 
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the fifth iteration
  #       with k=6 which corresponds to the sixth column
  #       of data (SOL_index); MSE is 0.2110059
  allMinRes = c(allMinRes, min(res))
  toAdd = (2:ncol(data))[res==min(res)]
  
  ### ADDING SOL_INDEX
  newData = cbind(newData, data[toAdd]) # adding SOL_index
  data = data[-toAdd] 
  #removing the col SOL_index from data so we can go through adding the other options
  
  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) 
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the fifth iteration
  #       with k=6 which corresponds to the sixth column
  #       of data (SOL-index); MSE is 0.2108328
  allMinRes = c(allMinRes, min(res))
  toAdd = (2:ncol(data))[res==min(res)]
  
  ### ADDING SOL
  newData = cbind(newData, data[toAdd]) 
  data = data[-toAdd] 
  #removing the col from data so we can go through adding the other options
  
  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) 
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the second iteration
  #       with k=3 which corresponds to the third column
  #       of data (wife_relig); MSE is  0.2104462
  allMinRes = c(allMinRes, min(res))
  toAdd = (2:ncol(data))[res==min(res)]
  
  
  ### ADDING WIFE_RELIG
  newData = cbind(newData, data[toAdd]) 
  data = data[-toAdd] 
  #removing the col from data so we can go through adding the other options
  
  
  
  
  
  res=c()
  for(k in 2:(ncol(data))){ #row one is the response var (wife_relig)
    model=glm(CMC~.,data = cbind(data[k],newData)) 
    res=c(res,cv.glm(cbind(data[k],newData),model,K=10)$delta[1]) 
    ## captures the cross-validation prediction error (MSE) 
    ## each time we remove a different variable.
  }
  res # note: MSE is smallest in the model in the second iteration
  #       with k=3 which corresponds to the third column
  #       of data (wife_working); MSE is  0.2105695
  allMinRes
  
  ## the minimum MSE in res is 0.2105695 which is greater than the MSE from the previous model
  ## of 0.2101886 so our final model is the model with all of the variables stored
  ## in newData
  
  
  ######## model pred variables: wife_ed, wife_age, num_children, SOL_index, hus_occupation, media_exposure, CMC
  
  mod = lm(formula = CMC ~., data = newData) 
  pred = (predict(mod) > .5) * 1
  sum(data$CMC != pred)/nrow(newData) #0.3267663
  
  # obtaining 4 fold cross validation misclassification rate
  err = logical(0)
  for(i in 1:4){
    mod=glm(formula = CMC~., data = newData[-Groups[,i],]) 
    # the formula from above
    pred=(predict(mod,newdata=newData[Groups[,i],]) > .5)*1 
    
    err=c(err,newData$CMC[Groups[,i]] != pred)
  }
  misclass5 = sum(err) / nrow(newData) #0.3226902
  misclass5
  ##################################
  # PCA - transforming the variables
  ##################################
  
  
  #resetting data
  data = cmc2
  data$CMC[data$CMC == "NO"] = 0
  data$CMC[data$CMC == "YES"] = 1
  data$CMC = as.numeric(data$CMC)
  
  
  ## get rid of non-continuous columns
  ## all the cols here are technically numeric type
  ## but some such as wife_relig and wife_ed are really 
  ## binary or categorical
  data2 = cmc2[,c(1,4)] 
  # TOOK OUT: wife_ed, hus_ed, media_exposure, wife_working
  #             hus_occupation, SOL_index, wife_relig, CMC
  #  LEFT WITH: wife_age, num_children



  cor(data2)
  # the correlation between the two quantitative variables
  # is 0.5397181 which isn't very concerning 

  pca = prcomp(data2, scale = TRUE)
  summary(pca)
  pca$x
  screeplot(pca,type="lines")
  #see that the first pca contains most of the variance

  ## we have decided to use the first principle component

  data2 = cbind(data[,c(-1,-4)], pca1 = pca$x[,1])
  # creating new data without the two numerical columns
  # and with our PCA component instead 


  mod=lm(data2$CMC~., data = data2)
  mod
  pred = (predict(mod) > .5) * 1
  sum(data2$wife_relig != pred)/nrow(data2) #0.4076087

  # four fold cross val misclassification rate
  err = logical(0)
  for(i in 1:4){
    mod=lm(CMC~., data=data2[-Groups[,i],]) 
    # Note that we are using all predictor variables first.
    pred=(predict(mod,newdata=data2[Groups[,i],]) > .5)*1 
    # Note the importance of the "predict()" function!
    err=c(err,data2$wife_relig[Groups[,i]] != pred)
  }

  misclass6 = sum(err) / nrow(data2) #0.4035326
  misclass6

  
#############################################
#############################################
#            DECISION TREES                 #
#############################################
#############################################

  library(rpart)
  library(rpart.plot)
  
  
  data = cmc2
  
  #converting all categorical and binary values to factor values
  data$wife_relig = as.factor(cmc2$wife_relig)
  data$wife_ed = as.factor(cmc2$wife_ed)
  data$hus_ed = as.factor(cmc2$hus_ed)
  data$wife_relig = as.factor(cmc2$wife_relig)
  data$wife_working = as.factor(cmc2$wife_working)
  data$hus_occupation = as.factor(cmc2$hus_occupation)
  data$media_exposure = as.factor(cmc2$media_exposure)
  data$SOL_index = as.factor(cmc2$SOL_index)


  
  
  ######## TREE 1: CP = 0.0065

  tree=rpart(CMC~., data=data,control=rpart.control(cp=.0001))
  ### Look at the row where xerror is minimized and set the control parameter slightly below that.
  printcp(tree,digits=5) ## we observe that it's ideal to set cp=.0065 (since xerror is minimized right before this)
  plotcp(tree)
  
  tree=rpart(CMC~., data=data,control=rpart.control(cp=.0065))
  rpart.plot(tree, cex = .5)
  
  # getting predictions
  pred = predict(tree,type="class")
  #misclassification rate
  sum(pred != data$CMC) / nrow(data) #0.2751359
  
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    tree=rpart(CMC~., data=data[-Groups[,i],], control=rpart.control(cp=.0065))
    pred=predict(tree,type="class",newdata=data[Groups[,i],])
    err=c(err,data$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass7 = sum(err) / nrow(data2) #0.3009511
  misclass7
  
  
  ######## TREE 2: CP = 0.004
  
  tree=rpart(CMC~., data=data,control=rpart.control(cp=.004))
  rpart.plot(tree, cex = .4)
  
  # getting predictions
  pred = predict(tree,type="class")
  #misclassification rate
  sum(pred != data$CMC) / nrow(data) #0.2540761
  
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    tree=rpart(CMC~., data=data[-Groups[,i],], control=rpart.control(cp=.001))
    pred=predict(tree,type="class",newdata=data[Groups[,i],])
    err=c(err,data$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass8 = sum(err) / nrow(data) #0.3240489
  misclass8
  
  
  
  ######## TREE 3: CP = 0.01
  
  tree=rpart(CMC~., data=data,control=rpart.control(cp=.01))
  rpart.plot(tree, cex = .5)
  
  # getting predictions
  pred = predict(tree,type="class")
  #misclassification rate
  sum(pred != data$CMC) / nrow(data) #0.2751359
  
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    tree=rpart(CMC~., data=data[-Groups[,i],], control=rpart.control(cp=.01))
    pred=predict(tree,type="class",newdata=data[Groups[,i],])
    err=c(err,data$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass9 = sum(err) / nrow(data) #0.2934783
  misclass9
  
 
  
  
#############################################
#############################################
#          SUPPORT VECTOR MACHINES          #
#############################################
#############################################
  
  
  cmc2$CMC = as.factor(data$CMC)
  
  library(e1071)
  
  #################################################################################
  #### SVM Model 1: kernal = linear, cost = 1
  
  mod=svm(CMC~.,data=cmc2,kernel="linear",scale=TRUE,type="C-classification",cost=1)
  pred = predict(mod)
  #misclassification rate
  sum(pred != cmc2$CMC) / nrow(cmc2) #0.310462
  
  plot(mod,data=cmc2,formula = wife_age~num_children)
  
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    mod=svm(CMC~.,data=cmc2[-Groups[,i],],kernel="linear",scale=TRUE,type="C-classification",cost=1)
    pred=predict(mod,newdata=cmc2[Groups[,i],])
    err=c(err,cmc2$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass10 = sum(err) / nrow(cmc2) #0.3192935
  misclass10

  ##################################################################################
  #### SVM Model 2: kernal = radial, cost = 1
  
  mod=svm(CMC~.,data=cmc2,kernel="radial",scale=TRUE,type="C-classification",cost=1)
  pred = predict(mod)
  #misclassification rate
  sum(pred != cmc2$CMC) / nrow(cmc2) #0.2398098
  
  plot(mod,data=cmc2,formula = wife_age~num_children)
  
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    mod=svm(CMC~.,data=cmc2[-Groups[,i],],kernel="radial",scale=TRUE,type="C-classification",cost=1)
    pred=predict(mod,newdata=cmc2[Groups[,i],])
    err=c(err,cmc2$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass11 = sum(err) / nrow(cmc2) #0.3023098
  misclass11
  
  ############################################################################
  #### SVM Model 3: kernal = linear, cost = 100
  
  mod=svm(CMC~.,data=cmc2,kernel="linear",scale=TRUE,type="C-classification",cost=100)
  pred = predict(mod)
  #misclassification rate
  sum(pred != cmc2$CMC) / nrow(cmc2) #0.3111413
  
  
  plot(mod,data=cmc2,formula = wife_age~num_children)
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    mod=svm(CMC~.,data=cmc2[-Groups[,i],],kernel="linear",scale=TRUE,type="C-classification",cost=100)
    pred=predict(mod,newdata=cmc2[Groups[,i],])
    err=c(err,cmc2$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass12 = sum(err) / nrow(cmc2) #0.3179348
  misclass12
  
  ############################################################################
  #### SVM Model 4: kernal = radial, cost = 100
  
  mod=svm(CMC~.,data=cmc2,kernel="radial",scale=TRUE,type="C-classification",cost=100)
  pred = predict(mod)
  #misclassification rate
  sum(pred != cmc2$CMC) / nrow(cmc2) #0.1487772
  
  
  plot(mod,data=cmc2,formula = wife_age~num_children)
  
  #four fold cross validation
  
  err = logical(0)
  for(i in 1:4){
    mod=svm(CMC~.,data=cmc2[-Groups[,i],],kernel="radial",scale=TRUE,type="C-classification",cost=100)
    pred=predict(mod,newdata=cmc2[Groups[,i],])
    err=c(err,cmc2$CMC[Groups[,i]] != pred)
  }
  
  #misclassification rate
  misclass13 = sum(err) / nrow(cmc2) #0.3288043
  misclass13
  
  
#############################################
#############################################
#            K-NEAREST NEIGHBORS            #
#############################################
#############################################
  
  
  library(class)
  
  ### KNN measures distance so we can only take quantatative predictor values
  data = cmc2[c(10,1,4)] 
    #selecting predictor vars: wife_age and num_children
    #selecting response var  : CMC
 
  
  
  # centering the data
  for(i in 2:(ncol(data))){ # not centering the response var
    data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
  }
  
  
  
  nrow(data) #1472 rows
  nrow(data)/10 # 147.2 
  # so we will use 148 rows as the unknown class and 1324 as the known class
  
  trainSet = data[1:1324,-1]
  testSet = data[1325:1472,-1]
  trainSetYs = data[1:1324,1]
  
  

  
  ##Let's see how changing the value k affects the predictions:
  ## w/out four-fold cross validation
  misclass=c()
  for(k in 5*(1:50)){ 
    Pred=knn(train = trainSet,test = testSet,cl = trainSetYs,k = k) 
    misclass=c(misclass,sum(data$CMC[1325:1472]!=Pred)/148)
  }
  
  
  plot(5*(1:50), misclass,pch=16) 
  min(misclass)
  
  #### WITH FOUR-FOLD CV 
  
  #Train - obs in data used as trainSet
  #Test - obs in data used as testSet
  #Y1 - the response var corresponding to trainSet
  #testPos - the positions in data of testSet
  
  
  Train1 = data[1:331,-1]
  Test1 = data[332:368,-1]
  Y1 = data[1:331,1]
  TestPos1 = 332:368
  
  Train2 = data[369:699,-1]
  Test2 = data[700:736,-1]
  Y2 = data[369:699,1]
  TestPos2 = 700:736
  
  Train3 = data[737:1067,-1]
  Test3 = data[1068:1104,-1]
  Y3 = data[737:1067,1]
  TestPos3 = 1068:1104
  
  Train4 = data[1105:1435,-1]
  Test4 = data[1436:1472,-1]
  Y4 = data[1105:1435,1]
  TestPos4 = 1436:1472
  
  trainSets = data.frame(Train1, Train2, Train3, Train4)
  testSets = data.frame(Test1, Test2, Test3, Test4)
  YSets = data.frame(Y1, Y2, Y3, Y4)
  testPos = data.frame(TestPos1, TestPos2, TestPos3, TestPos4)
  
  nrow(trainSet)/4 # the trainSet w/ four fold misclass will be 331 obs four times
  nrow(testSet)/4 # the testSet w/ four fold misclass will be 37 obs four times
  length(trainSetYs)/4 #  the trainSetYs w/ four fold misclass will be 331 obs four times
  
  
  ##Let's see how changing the value k affects the predictions:
  misclass=c()
  for(k in 5*(1:50)){ 
    err = logical(0)
    j = 1
    for(i in (0:3)*2){
      Pred=knn(train = trainSets[,(1+i):(2+i)],test = testSets[,(1+i):(2+i)],cl = YSets[,j],k = k) 
      # trainSets and testSets come in pairs of the columns to select
      # so we are selecting the columns with the observations in pairs 
      err=c(err,data$CMC[testPos[,j]]!=Pred)
      j = j+1
    }
    misclass = c(misclass, sum(err)/148)
  }
  
  # above loops from 0:250 by 5s through nearest neighbors and does 4
  # fold misclassification rate on each k value
  
  
  plot(5*(1:50),misclass,pch=16) 
  min(misclass) 
  # earliest min(misclass) is at the 6th iteration which is a
  # a k of 30 (6*5)

  misclass14 = min(misclass)
  
#############################################
#############################################
#             CLUSTER ANALYSIS              #
#############################################
#############################################
    
  # types of distances: Euclidean, Manhattan
  # types of linkage: complete, average, single
  
  data = cmc2[,-10] #disregarding our response var (CMC)
  
  # centering our data
  for(k in 1:ncol(data)){
    data[,k]=(data[,k]-mean(data[,k]))/sd(data[,k])
  }
  
  
  Eucl.Distances= dist(data)
  Manh.Distances= dist(data,method="manhattan")
  
  # function to plot effects of using from 1:5 clusters:
  
  
  plotClust <- function(Clusters) {
    ## When using one cluster:
    Gr = cutree(Clusters, k = 1)
    d = dist(data[Gr == 1, ]) ### Compute distances between objects in Group 1 (the only group here)
    mean(d)
    
    ONE.clustAve = mean(d)
    
    ## When using two clusters:
    
    Gr = cutree(Clusters, k = 2)
    d1 = dist(data[Gr == 1, ]) ### Compute distances between objects in Group 1
    d2 = dist(data[Gr == 2, ]) ### Compute distances between objects in Group 2
    
    TWO.clustAve = mean(c(d1, d2)) # average of the distances within the clusters
    TWO.clustAve
    
    ## When using three clusters:
    
    Gr = cutree(Clusters, k = 3)
    d1 = dist(data[Gr == 1, ])
    d2 = dist(data[Gr == 2, ])
    d3 = dist(data[Gr == 3, ])
    
    THREE.clustAve = mean(c(d1, d2, d3))   # average of the distances within the clusters
    THREE.clustAve
    
    ## When using four clusters:
    
    Gr = cutree(Clusters, k = 4)
    d1 = dist(data[Gr == 1, ])
    d2 = dist(data[Gr == 2, ])
    d3 = dist(data[Gr == 3, ])
    d4 = dist(data[Gr == 4, ])
    
    FOUR.clustAve = mean(c(d1, d2, d3, d4)) # average of the distances within the clusters
    FOUR.clustAve
    
    
    ## When using five clusters:
    
    Gr = cutree(Clusters, k = 5)
    d1 = dist(data[Gr == 1, ])
    d2 = dist(data[Gr == 2, ])
    d3 = dist(data[Gr == 3, ])
    d4 = dist(data[Gr == 4, ])
    d5 = dist(data[Gr == 4, ])
    
    FIVE.clustAve = mean(c(d1, d2, d3, d4, d5)) # average of the distances within the clusters
    FIVE.clustAve
    
    ################### THE AVERAGES TOGETHER:
    ONE.clustAve
    TWO.clustAve
    THREE.clustAve
    FOUR.clustAve
    FIVE.clustAve
    
    ## To get a sense for the best grouping we can plot them:
    plot(
      c(1, 2, 3, 4, 5),
      c(
        ONE.clustAve,
        TWO.clustAve,
        THREE.clustAve,
        FOUR.clustAve,
        FIVE.clustAve
      )
    )
  }
  
  #################################################
  ### MODEL 1: Euclidian Distance, Complete Linkage
  
  Clusters=hclust(Eucl.Distances,method="complete")
  plot(as.dendrogram(Clusters))
  
  # DECIDING HOW MANY CLUSTERS TO USE
  plotClust(Clusters)
    # plots avg of the distances within the clusters on the Y axis 
    # plots the number of clusters used on the x axis
  # dropoff is small from 4 to 5 clusters so we will go with 4 clusters
    
  group=cutree(Clusters,k=4)
  
  Pred=rep(NA,1472)

  sum(cmc2$CMC[group==1]== "YES") #96
  sum(cmc2$CMC[group==1]== "NO") #83
  ## Therefore Group 1 should be predicted as "YES"
  Pred[group==1] = "YES"
  
  sum(cmc2$CMC[group==2]== "YES") #707 
  sum(cmc2$CMC[group==2]== "NO")  #469
  ## Therefore Group 2 should be predicted as "YES"
  Pred[group==2]= "YES"
  
  sum(cmc2$CMC[group==3]== "YES") #35 
  sum(cmc2$CMC[group==3]== "NO")  #74
  ## Therefore Group 1 should be predicted as "NO" 
  Pred[group==3]= "NO"
  
  sum(cmc2$CMC[group==4]== "YES") #5
  sum(cmc2$CMC[group==4]== "NO")  #3
  ## Therefore Group 1 should be predicted as "YES"
  Pred[group==4]= "YES"
  
  
  ################misclassification rate can be computed with:
  misclass15 = sum(cmc2$CMC!=Pred)/length(Pred)
  misclass15 #0.4008152
  
  

  
  
  
  #################################################
  ### MODEL 2: Manhattan Distance, Complete Linkage
  
  Clusters=hclust(Manh.Distances,method="complete")
  plot(as.dendrogram(Clusters))

  # DECIDING HOW MANY CLUSTERS TO USE
  plotClust(Clusters)
  # plots avg of the distances within the clusters on the Y axis 
  # plots the number of clusters used on the x axis
  # dropoff is small from 4 to 5 clusters so we will go with 4 clusters
  group=cutree(Clusters,k=4)
  
  Pred=rep(NA,1472)
  
  sum(cmc2$CMC[group==1]== "YES") #170
  sum(cmc2$CMC[group==1]== "NO") #212
  ## Therefore Group 1 should be predicted as "NO"
  Pred[group==1] = "NO"
  
  sum(cmc2$CMC[group==2]== "YES") #478 
  sum(cmc2$CMC[group==2]== "NO")  #228
  ## Therefore Group 2 should be predicted as "YES"
  Pred[group==2]= "YES"
  
  sum(cmc2$CMC[group==3]== "YES") #160 
  sum(cmc2$CMC[group==3]== "NO")  #115
  ## Therefore Group 1 should be predicted as "YES" 
  Pred[group==3]= "YES"
  
  sum(cmc2$CMC[group==4]== "YES") #35
  sum(cmc2$CMC[group==4]== "NO")  #74
  ## Therefore Group 1 should be predicted as "NO"
  Pred[group==4]= "NO"
  

  ################misclassification rate can be computed with:
  misclass16=sum(cmc2$CMC!=Pred)/length(Pred)
  misclass16 #0.3722826
  
  #################################################
  ### MODEL 3: Euclidean Distance, Avg Linkage
  
  Clusters=hclust(Eucl.Distances,method="average")
  plot(as.dendrogram(Clusters))
  # DECIDING HOW MANY CLUSTERS TO USE
  plotClust(Clusters)
  # plots avg of the distances within the clusters on the Y axis 
  # plots the number of clusters used on the x axis
  # dropoff is small from 3 to 4 clusters so we will go with 3 clusters
  group=cutree(Clusters,k=3)
  
  Pred=rep(NA,1472)
  
  sum(cmc2$CMC[group==1]== "YES") #808
  sum(cmc2$CMC[group==1]== "NO") #554
  ## Therefore Group 1 should be predicted as "YES"
  Pred[group==1] = "YES"
  
  sum(cmc2$CMC[group==2]== "YES") #35 
  sum(cmc2$CMC[group==2]== "NO")  #74
  ## Therefore Group 2 should be predicted as "NO"
  Pred[group==2]= "NO"
  
  sum(cmc2$CMC[group==3]== "YES") #0 
  sum(cmc2$CMC[group==3]== "NO")  #1
  ## Therefore Group 1 should be predicted as "NO" 
  Pred[group==3]= "NO"
  
  ################misclassification rate can be computed with:
  misclass17=sum(cmc2$CMC!=Pred)/length(Pred)
  misclass17 #0.4001359
  
  #################################################
  ### MODEL 4: Manhattan Distance, Single Linkage
  
  Clusters=hclust(Manh.Distances,method="single")
  plot(as.dendrogram(Clusters))
  # DECIDING HOW MANY CLUSTERS TO USE
  plotClust(Clusters)
  # going to use 5 clusters
  group=cutree(Clusters,k=2)
  
  
  Pred=rep(NA,1472)
  
  sum(cmc2$CMC[group==1]== "YES") #842
  sum(cmc2$CMC[group==1]== "NO") #626
  ## Therefore Group 1 should be predicted as "YES"
  Pred[group==1] = "YES"
  
  sum(cmc2$CMC[group==2]== "YES") #0 
  sum(cmc2$CMC[group==2]== "NO")  #1
  ## Therefore Group 2 should be predicted as "NO"
  Pred[group==2]= "NO"

  
  
  
  ################misclassification rate can be computed with:
  misclass18=sum(cmc2$CMC!=Pred)/length(Pred)
  misclass18 #0.4266304
  
  
  
#############################################
#############################################
#              RANDOM FORESTS               #
#############################################
#############################################
  
  
  
  library(randomForest)
  
  data = cmc2
  data$CMC = as.factor(data$CMC)
  
  nrow(cmc2) #1472
  ncol(cmc2) #10
  
  
  RF=randomForest(CMC~.,data = data, mtry = 5, ntree = 736)
    # we want 50% of the observations (1472/2)
    # we want 50% of the variables at each step (10/2)
  
  predict(RF)
  
  # misclassification rate
  sum(data$CMC != pred)/nrow(data) #0.4422554
  
  # WITH TESTING AND TRAINING SETS
  
  nrow(data) #1472
  # putting 148 (about 10% of obs) in test set and 1324 in train set
  
  trainObs=sample(1:1472,1324,replace=FALSE)
  testObs=(1:1472)[-trainObs]
  
  RF=randomForest(CMC~.,data=data[trainObs,])
  Pred=predict(RF,newdata=data[testObs,])
  
  # misclassification rate is 0.27702703
  misclass19 = sum(data$CMC[testObs]!= Pred)/length(testObs)
  misclass19
  
#############################################
#############################################
#       ANALYSIS OF MISCLASS RATES          #
#############################################
#############################################
  
  
  
  
  allMisclass <- c(misclass1,misclass2,misclass3,misclass4,
                   misclass5,misclass6,misclass7,misclass8,
                   misclass9,misclass10,misclass11,misclass12,
                   misclass13,misclass14,misclass15,misclass16,
                   misclass17,misclass18,misclass19)
  plot(allMisclass, main = "Misclassification Rates")

  
  
  # trying the final model one last time
  

  ###############################
  # optimal model final assessment
  ###############################
  
  library(randomForest)
  
  data = cmc2
  data$CMC = as.factor(data$CMC)
  
  RF=randomForest(CMC~.,data = data, mtry = 5, ntree = 736)
  # we want 50% of the observations (1472/2)
  # we want 50% of the variables at each step (10/2)
  
  # WITH TESTING AND TRAINING SETS
  
  nrow(data) #1472
  # putting 148 (about 10% of obs) in test set and 1324 in train set
  
  trainObs=sample(1:1472,1324,replace=FALSE)
  testObs=(1:1472)[-trainObs]
  
  RF=randomForest(CMC~.,data=data[trainObs,])
  Pred=predict(RF,newdata=data[testObs,])
  
  # misclassification rate is 0.2905405
  misclass19 = sum(data$CMC[testObs]!= Pred)/length(testObs)
  misclass19
  
###################################
# finding the average misclass 
# of 100 random forests
##################################
  
  misclass = numeric(0)
  for(i in 1:100){
    trainObs=sample(1:1472,1324,replace=FALSE)
    testObs=(1:1472)[-trainObs]
    RF=randomForest(CMC~.,data=data[trainObs,])
    Pred=predict(RF,newdata=data[testObs,])
    misclass = c(misclass,sum(data$CMC[testObs]!= Pred)/length(testObs))
  }
  
   misclass
   mean(misclass)  
   median(misclass)
   