install.packages("forecast")
library(forecast)

install.packages("TSA")
library(TSA)

install.packages("randomForest")
library(randomForest)

install.packages("stringr")
library(stringr)



#reorder columns for readability
# reOrgTree = c('Store','Dept','Week', 'Month', 'Holiday','StoreType','Size','Temperature','Fuel_Price','CPI','Unemployment',
#                 'MarkDown1','MarkDown2','MarkDown3','MarkDown4','MarkDown5')


#Data Prep for random Forest Models
  trainSub = merge(x = trainSub, y = train, all.x = TRUE)
  trainSub = trainSub[with(trainSub, order(Store, Dept, Date)), ]
  #test = test[with(test, order(Store, Dept, Date)), ]
  
  reOrgRF = c('Store','Dept','WeekMonth', 'WeekYear', 'Month', 'Holiday','StoreType','Size','Temperature','Fuel_Price','CPI','Unemployment')
  trainRF = (trainSub[ , c(reOrgRF, 'Weekly_Sales')])
  View(trainRF)
  #testRF = data.frame(test[ , reOrgRF ])
  #View(testRF)


#Build Random Forest Tree for predictions
  #regressor = randomForest(formula = Weekly_Sales ~ ., data = trainRF)
  regressor = randomForest(formula = Weekly_Sales ~ ., data = trainRF, ntree = 250, mtry = 8, importance = TRUE, proximity = TRUE)
  print(regressor)
  plot(regressor)
  
  t = tuneRF(trainRF[ , -13], trainRF[, 13],
         stepFactor = 0.5,
         plot = TRUE,
         ntreeTry = 250,
         trace = TRUE,
         improve = 0.05)
  importance(regressor)
  
  hist(treesize(regressor), main = "Number of nodes")
  varImpPlot(regressor)
  
  #pred = predict(regressor, newdata = testRF)


  
#Data Prep for arima 
  # 143 dates for all departments in training and test sets
  # fill missing weekly sales with 0.
  
    # trainRead$Id = str_c(train$Store, "_", trainRead$Dept)
    # testRead$Id = str_c(testRead$Store, "_", testRead$Dept)
    # 
    # dateAll = data.frame(seq.Date(from = as.Date('2010-02-05', format = "%Y-%m-%d"), to = as.Date('2012-10-26', format = "%Y-%m-%d"), by = 7))
    # dept = data.frame(unique(c(trainRead$Id, trainRead$Dept)))
    # 
    # dateDept =  merge(x = dateAll, y = dept)
    # names(dateDept) = c("Date","Id")
    # 
    # trainArima = merge(dateDept, trainRead[,c("Id","Date","Weekly_Sales")], all.x=T, all.y=T)
    # trainArima[is.na(trainArima)] = 0
    # trainArima = trainArima[, c("Id","Date","Weekly_Sales")]
    # 
    # View(trainArima)
  
  trainArima = trainSub[ , c('Store', 'Dept','Date', 'Weekly_Sales')]
  trainArimaSub = trainArima[trainArima$Store==1 & trainArima$Dept==1, c('Date', 'Weekly_Sales')]
  trainArimaSub = trainArimaSub[with(trainArimaSub, order(Date)), ]
  trainArimaSub$Weekly_Sales = ts(trainArimaSub$Weekly_Sales, start = c(2010, 5), end = c(2012, 30), frequency = 52)
  
  #d = diff(trainArimaSub$Weekly_Sales, 52)
    
  arima(d, order = c(1,0,0)) 
  
  
  
  
  