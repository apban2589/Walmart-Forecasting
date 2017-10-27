#libraries
install.packages("lubridate")
library(lubridate)


#feature1
#Update Holidays
  holidayDateList = c('2010-02-12', '2011-02-11', '2012-02-10', '2013-02-08', '2010-09-10', '2011-09-09', '2012-09-07', '2013-09-06', '2010-11-26', '2011-11-25', '2012-11-23', '2013-11-29', '2010-12-31', '2011-12-30', '2012-12-28', '2013-12-27')
  holidayDateList = as.Date(holidayDateList, format = "%Y-%m-%d")
  
  holiday = function(inputDate){
      ret = 0 # ret = "No"
    if(inputDate %in% holidayDateList[1:4])
      ret = 1 # ret = "SuperBowl"
    else if(inputDate %in% holidayDateList[5:8])
      ret = 2 # ret = "LaborDay"
    else if(inputDate %in% holidayDateList[9:12])
      ret = 3 # ret = "Thanksgiving"
    else if(inputDate %in% holidayDateList[13:16])
      ret = 4 # ret = "Christmas"
    
    return(ret)
  }
  
  train$Holiday = rep(0, nrow(train))
  n = nrow(train[train$IsHoliday == TRUE, ])
    for(i in 1:n){
      train$Holiday[train$IsHoliday == TRUE][i] = holiday(train$Date[train$IsHoliday == TRUE][i])
    }

  test$Holiday = rep(0, nrow(test))
  m = nrow(test[test$IsHoliday == TRUE, ])
    for(i in 1:m){
      test$Holiday[test$IsHoliday == TRUE][i] = holiday(test$Date[test$IsHoliday == TRUE][i])
    }


#feature2
#Extract Month from Date (to detect seasonality)
  train$Month =  month(train$Date)
  test$Month = month(test$Date)


#feature3
#Extract week number from Date (to detect trend)
  baseDate = as.Date('2010-02-05', format = "%Y-%m-%d")
  
  train$Week = (train$Date - baseDate)/7
  test$Week = (test$Date - baseDate)/7


#feature4
#Extract week number in year
  train$WeekYear = NULL
  base2010 = as.Date('2010-01-01', format = "%Y-%m-%d")
  base2011 = as.Date('2011-01-07', format = "%Y-%m-%d")
  base2012 = as.Date('2012-01-06', format = "%Y-%m-%d")
  for(i in 1:nrow(train)){
    if(year(train$Date[i])==2010){
      train$WeekYear[i] = (train$Date[i] - base2010)/7
    }else if(year(train$Date[i])==2011){
      train$WeekYear[i] = (train$Date[i] - base2011)/7
    }else if(year(train$Date[i])==2012){
      train$WeekYear[i] = (train$Date[i] - base2012)/7
    }
  }
 
  
#feature5
#Extract week number in month
  train$WeekMonth = ceiling(day(train$Date)/7)
  
  
#save the datasets
  write.table(x = train, file = "trainPreprocess.csv", sep = ",", row.names = FALSE, quote = FALSE)
  write.table(x = test, file = "testPreprocess.csv", sep = ",", row.names = FALSE, quote = FALSE)

View(train)
View(test)
