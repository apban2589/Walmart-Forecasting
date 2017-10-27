#set working directory
  setwd("C:/Users/reddy/Desktop/DataSets/WallmartForecasting")

#load libraries
  install.packages("timeSeries")
  library(timeSeries)

#read files
  storesRead = read.csv(file = "stores.csv", header = TRUE)
  View(storesRead)
  
  featuresRead = read.csv(file = "features.csv", header = TRUE)
  featuresRead$Date = as.Date(featuresRead$Date, format = "%Y-%m-%d")
  View(featuresRead)
  
  trainRead = read.csv(file = "train.csv", header = TRUE)
  trainRead$Date = as.Date(trainRead$Date, format = "%Y-%m-%d")
  View(trainRead)
  
  testRead = read.csv(file = "test.csv", header = TRUE)
  testRead$Date = as.Date(testRead$Date, format = "%Y-%m-%d")
  View(testRead)


#handle missing MarkDown Values
      #features$markDownPresent = features$Date > as.Date("2011-11-04", format = "%Y-%m-%d")
      
      #markdowns = c('MarkDown1','MarkDown2','MarkDown3','MarkDown4','MarkDown5')
      #markDownMatrix = features[features$markDownPresent ==1, markdowns]
      
      #markDownMatrix = interpNA(markDownMatrix, method = "linear")
      #features[features$markDownPresent ==1, markdowns] = round(markDownMatrix, 2)
      
      #features$MarkDown1[is.na(features$MarkDown1)] = round(median(features$MarkDown1, na.rm = TRUE), 2)
      #features$MarkDown2[is.na(features$MarkDown2)] = round(median(features$MarkDown2, na.rm = TRUE), 2)
      #features$MarkDown3[is.na(features$MarkDown3)] = round(median(features$MarkDown3, na.rm = TRUE), 2)
      #features$MarkDown4[is.na(features$MarkDown4)] = round(median(features$MarkDown4, na.rm = TRUE), 2)
      #features$MarkDown5[is.na(features$MarkDown5)] = round(median(features$MarkDown5, na.rm = TRUE), 2)


#change type to integer
  storesRead$StoreType = 0
    for(i in 1:nrow(storesRead)){
      if(storesRead$Type[i] == "A")  
        storesRead$StoreType[i] = 1
      else  if(storesRead$Type[i] == "B") 
        storesRead$StoreType[i] = 2
      else  if(storesRead$Type[i] == "C")  
        storesRead$StoreType[i] = 3
    }


#handle missing CPI(extrapolation) and Unemployment(previous available rate)
  storeUnique = unique(featuresRead$Store)
    for(i in storeUnique){
      cpi = featuresRead[featuresRead$Store==i, ]$CPI
      a = na.omit(cpi)
      inc = mean(a[2:length(a)] - a[1:length(a)-1])
      cpi[is.na(cpi)] = seq(1, length(cpi) - length(a))*inc + a[length(a)]
      featuresRead[featuresRead$Store==i, ]$CPI = round(cpi,2)
      
      
      unemp = featuresRead[featuresRead$Store==i, ]$Unemployment
      for(j in 1:length(unemp)){
        if(is.na(unemp[j])){
          unemp[j] = unemp[j-1]
        }
      }
      featuresRead[featuresRead$Store==i, ]$Unemployment = round(unemp,2)
    }

  
#subset the data for faster results
#choose 5 stores and 5 departments and save last 3 months for forecasting
  
  Date = rep(seq.Date(from = as.Date('2010-02-05', format = "%Y-%m-%d"), to = as.Date('2012-07-27', format = "%Y-%m-%d"), by = 7),25) 
  Store = rep(rep(seq(1,5,by = 1),rep(130,5)),5)
  Dept = rep(seq(1,5,by = 1),rep(650,5))
  trainSub = data.frame(Store, Dept, Date)
  trainSub = trainSub[with(trainSub, order(Store, Dept, Date)), ]


#merge datasets
trainTemp =  merge(x=trainRead, y=storesRead, all.x=TRUE)
train = merge(x = trainTemp, y = featuresRead, all.x = TRUE)

testTemp =  merge(x=testRead, y=storesRead, all.x=TRUE)
test = merge(x = testTemp, y = featuresRead, all.x = TRUE)

#change boolean to number
train$IsHoliday = 1*train$IsHoliday
test$IsHoliday = 1*test$IsHoliday

#sort datasets
train = train[with(train, order(Store, Dept, Date)), ]
test = test[with(test, order(Store, Dept, Date)), ]


#save the datasets
write.table(x = train, file = "trainPreprocess.csv", sep = ",", row.names = FALSE, quote = FALSE)
write.table(x = test, file = "testPreprocess.csv", sep = ",", row.names = FALSE, quote = FALSE)








