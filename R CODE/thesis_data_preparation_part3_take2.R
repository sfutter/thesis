
#######################################
# Data transformations to Date column
#######################################
processPart1 = function(df){
  df = df[,2:length(names(df))] 
  df$Date         = as.Date(df$Date, '%Y-%m-%d')
  df$Year         = as.factor(df$Year)
  df$Month        = as.factor(df$Month)
  df$Week         = as.factor(df$Week)
  df$Day          = as.factor(df$Day)
  df$TimeHoursNum = as.factor(df$TimeHoursNum)
  
  #dropCols = which(names(df) %in% c('Date','Week')) 
  #df = df[,-dropCols]
  
  return(df)
}
#################################
# Time of day filter for looping
#################################
processPart2 = function(df,hour){
  df <- df %>% filter(TimeHoursNum==hour) 
  return(df)
}

######################################################
# Drop 'current' snapshot values for ex-ante modeling
######################################################
processPart3 = function(df, response){
  
  inCols  = which(names(df) %in% response)
  df1 = df[,inCols]
  
  dropCols = which(names(df) %in% c('NO','NO2','NOX','O3','PM10','PM2.5','SO2','BP','RAIN','RHUM','SOLR','TEMP','WDIR','WSPD',
                                    'TimeHoursNum'))  # also remove TimeHoursNum otherwise it becomes a 1 factor col after filtering on e.g. TimeHoursNum == 9
  df2 = df[,-dropCols]
  
  df  = cbind(df1,df2)
  colnames(df)[1] = response  # rename response column otherwise it reads df1 as column header
  return(df)
}

###################################################
# Drop current hour values for predictors/responses
###################################################
processPart4 = function(df){
  dropCols = which(names(df) %in% c('NO','NO2','NOX','O3','PM10','PM2.5','SO2','BP','RAIN','RHUM','SOLR','TEMP','WDIR','WSPD', 'TimeHoursNum',
                                    'NO.IMP', 'NOX.IMP', 'O3.IMP', 'PM10.IMP','PM2.5.IMP','SO2.IMP','BP.IMP', 'RAIN.IMP','RHUM.IMP','SOLR.IMP',
                                    'WDIR.IMP','WSPD.IMP'))        
  df = df[,-dropCols]
  return(df)
}

####################################
# get future hour response variable
####################################
processPart5 = function(df,response, responseHour){
  df <- df %>% filter(TimeHoursNum==responseHour)
  inCols  = which(names(df) %in% c(response))            
  df = df[,inCols]
  return(df)
}

#################################################################################
# Data transforms to Date column - similar to processPart1 w.out the col remove
#################################################################################
processPart6 = function(df){
  df = df[,2:length(names(df))] 
  df$Date         = as.Date(df$Date, '%Y-%m-%d')
  df$Year         = as.factor(df$Year)
  df$Month        = as.factor(df$Month)
  df$Week         = as.factor(df$Week)
  df$Day          = as.factor(df$Day)
  df$TimeHoursNum = as.factor(df$TimeHoursNum)
  return(df)
}

##################################################################
# Drop TimeHoursNum since this has been made a single factor col
##################################################################
processPart7 = function(df){
  dropCols = which(names(df) %in% c('TimeHoursNum'))        
  df = df[,-dropCols]
  return(df)
}


# ############################################################################
# # Create data frame which includes 1-3 hr lag pollutant + meteo current hr   
# ############################################################################
# # predictions were in range of 70% accuracy in test and 60% accuracy in validation for predicting >40 NO2. 
# createDataFrame = function(df,predictor){
#   dfNew = df %>% select(Time, Year, Month, TimeHoursNum,                  # date Time variables
#                         lag.1.NO:lag.3.NO,
#                         lag.1.NO2:lag.3.NO2,
#                         lag.1.NOX:lag.3.NOX,
#                         lag.1.O3:lag.3.O3,
#                         lag.1.PM10:lag.3.PM10,
#                         lag.1.PM2.5:lag.3.PM2.5,
#                         lag.1.SO2:lag.3.SO2,
#                         
#                         lag.1.BP:lag.3.BP,
#                         lag.1.RAIN:lag.3.RAIN,
#                         lag.1.RHUM:lag.3.RHUM,
#                         lag.1.SOLR:lag.3.SOLR,
#                         lag.1.TEMP:lag.3.TEMP,
#                         lag.1.WDIR:lag.3.WDIR,
#                         lag.1.WSPD:lag.3.WSPD,
#                         
#                         BP.IMP:WSPD.IMP,                                  # use current hour meteorological variable inputs (assumed provided by MET office UK)
#                         
#                         X5amTo8amSlope.NO2.IMP: X2amTo5amSlope.WSPD.IMP)  # arima slopes-proxy
#   dfPred = df[,predictor]
#   dfNew = cbind(dfPred, dfNew)
#   colnames(dfNew)[1] = paste0(predictor)
#   return(dfNew)
# }

############################################################################################
# Create data frame which includes 1-3 hr lag pollutant + meteo current hr + max/min 6-8AM
############################################################################################
createDataFrame = function(df,predictor){
  dfNew = df %>% select(Time, Year, Month, TimeHoursNum,                  # date Time variables
                        lag.1.NO:lag.3.NO,
                        lag.1.NO2:lag.3.NO2,
                        lag.1.NOX:lag.3.NOX,
                        lag.1.O3:lag.3.O3,
                        lag.1.PM10:lag.3.PM10,
                        lag.1.PM2.5:lag.3.PM2.5,
                        lag.1.SO2:lag.3.SO2,
                        
                        lag.1.BP:lag.3.BP,
                        lag.1.RAIN:lag.3.RAIN,
                        lag.1.RHUM:lag.3.RHUM,
                        lag.1.SOLR:lag.3.SOLR,
                        lag.1.TEMP:lag.3.TEMP,
                        lag.1.WDIR:lag.3.WDIR,
                        lag.1.WSPD:lag.3.WSPD,
                        
                        BP.IMP:WSPD.IMP,                                  # use current hour meteorological variable inputs (assumed provided by MET office UK)
                        
                        X5amTo8amSlope.NO2.IMP: X2amTo5amSlope.WSPD.IMP,  # arima slopes-proxy
                        max.6.to.8.AM.NO2.IMP:min.6.to.8.AM.WSPD.IMP)     # max and min values between 6-8AM
  
  dfPred = df[,predictor]
  dfNew = cbind(dfPred, dfNew)
  colnames(dfNew)[1] = paste0(predictor)
  return(dfNew)
}



############################################################################
# FUNCTION THAT REPLACES ACTUAL-LAG-1 WITH PREDICTED-LAG-1
############################################################################
updateDF_tMinus1Lags = function(df, lagPredictor, infile){
  inPath  = file.path("~/Dropbox","NU","THESIS_MAC","thesis","DATASETS","RESULTS", "PREDICTIONS","ONE_HOUR_AHEAD_PREDICTIONS")
  inFile = read.csv(file.path(inPath,infile),na.strings=c("NA"," "))
  inFile = inFile %>% select(predict, TimeKey1)  # note that the TimeKey for the 9AM prdiction will be 10AM... 
  
  inFile$TimeKey1 = as.POSIXct(inFile$TimeKey1)  
  df$Time         = as.POSIXct(df$Time)  
  
  df = full_join(df,inFile,by = c("Time" = "TimeKey1")) 
  df[,lagPredictor] = ifelse(is.na(df$predict),df[,lagPredictor],df$predict)
  
  dfNew = df %>% select(everything(), -predict)
  return(dfNew)
}


# FOR 11AM TIMEKEY1 IN THE 10AM PREDICT FILE IS 1-HOUR LAG. FOR 11AM TIMEKEY2 IN THE 9AM PREDICT FILE IS THE 2-HOUR LAG.
updateDF_tMinus2Lags = function(df, lagPredictor, infile){
  inPath  = file.path("~/Dropbox","NU","THESIS_MAC","thesis","DATASETS","RESULTS", "PREDICTIONS","ONE_HOUR_AHEAD_PREDICTIONS")
  inFile = read.csv(file.path(inPath,infile),na.strings=c("NA"," "))
  inFile = inFile %>% select(predict, TimeKey2)  # note that the TimeKey for the 11AM prdiction will be 9AM TIMEKEY2!
  
  inFile$TimeKey2 = as.POSIXct(inFile$TimeKey2)  
  df$Time         = as.POSIXct(df$Time)  
  
  df = full_join(df,inFile,by = c("Time" = "TimeKey2")) 
  df[,lagPredictor] = ifelse(is.na(df$predict),df[,lagPredictor],df$predict)
  
  dfNew = df %>% select(everything(), -predict)
  return(dfNew)
}


updateDF_tMinus3Lags = function(df, lagPredictor, infile){
  inPath  = file.path("~/Dropbox","NU","THESIS_MAC","thesis","DATASETS","RESULTS", "PREDICTIONS","ONE_HOUR_AHEAD_PREDICTIONS")
  inFile = read.csv(file.path(inPath,infile),na.strings=c("NA"," "))
  inFile = inFile %>% select(predict, TimeKey3)  
  
  inFile$TimeKey3 = as.POSIXct(inFile$TimeKey3)  
  df$Time         = as.POSIXct(df$Time)  
  
  df = full_join(df,inFile,by = c("Time" = "TimeKey3")) 
  df[,lagPredictor] = ifelse(is.na(df$predict),df[,lagPredictor],df$predict)
  
  dfNew = df %>% select(everything(), -predict)
  return(dfNew)
}



updateDF_tMinus4Lags = function(df, lagPredictor, infile){
  inPath  = file.path("~/Dropbox","NU","THESIS_MAC","thesis","DATASETS","RESULTS", "PREDICTIONS","ONE_HOUR_AHEAD_PREDICTIONS")
  inFile = read.csv(file.path(inPath,infile),na.strings=c("NA"," "))
  inFile = inFile %>% select(predict, TimeKey4)  
  
  inFile$TimeKey4 = as.POSIXct(inFile$TimeKey4)  
  df$Time         = as.POSIXct(df$Time)  
  
  df = full_join(df,inFile,by = c("Time" = "TimeKey4")) 
  df[,lagPredictor] = ifelse(is.na(df$predict),df[,lagPredictor],df$predict)
  
  dfNew = df %>% select(everything(), -predict)
  return(dfNew)
}


updateDF_tMinus5Lags = function(df, lagPredictor, infile){
  inPath  = file.path("~/Dropbox","NU","THESIS_MAC","thesis","DATASETS","RESULTS", "PREDICTIONS","ONE_HOUR_AHEAD_PREDICTIONS")
  inFile = read.csv(file.path(inPath,infile),na.strings=c("NA"," "))
  inFile = inFile %>% select(predict, TimeKey5)  
  
  inFile$TimeKey5 = as.POSIXct(inFile$TimeKey5)  
  df$Time         = as.POSIXct(df$Time)  
  
  df = full_join(df,inFile,by = c("Time" = "TimeKey5")) 
  df[,lagPredictor] = ifelse(is.na(df$predict),df[,lagPredictor],df$predict)
  
  dfNew = df %>% select(everything(), -predict)
  return(dfNew)
}




