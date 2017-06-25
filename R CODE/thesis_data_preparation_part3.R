
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
  
  dropCols = which(names(df) %in% c('Date','Week')) 
  df = df[,-dropCols]
  
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
  dropCols = which(names(df) %in% c('NO','NO2','NOX','O3','PM10','PM2.5','SO2','BP','RAIN','RHUM','SOLR','TEMP','WDIR','WSPD', 'TimeHoursNum'))        
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


