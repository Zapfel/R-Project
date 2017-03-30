install.packages('zipcode')

setwd("~/data science")
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(maps)
library(sqldf)
setwd("~/data science")
set.seed(123)
no_cores <- detectCores() - 1
sqlite <- dbDriver("SQLite")
db <- dbConnect(sqlite, dbname='R1_3.SQLite')
setwd("~/data science")

#Load data sets
LoanStats <- read_csv('LoanStats3a.csv', skip = 1, col_names = TRUE)

RejectStats <- read_csv('RejectStatsA.csv',col_types = cols(ApplicationDate = 'c'),
                        skip = 1, col_names = TRUE)

StatesPopData <- data.table(read_csv('StatesPopData.csv', col_names = TRUE))




#LoanStats Initial manipulation to a more readable form


#Collecting the initial data from csv files

LoanStats <- read_csv('LoanStats3a.csv', skip = 1, col_names = TRUE)

#converting to data table for memory optimization

LoanStats <- data.table(LoanStats)

#Stripping unnecessary columns, either too much missing data 
#or provide no information

LoanStats <- LoanStats[,c(2:18,20:50)]

#Data Manipulation Steps
#objectives:
#(i) LoanStats
#(1) collect dates from column 17 add to new column
#(2) Split all dates into a month and year section
#(3) remove '%' sign and convert to decimal in 'int_rate'
# & remove "xx"'s from zip_code

#(1) 
#splitting column 17, (loan description) into
#the actual description and the date it was added
LoanStatsTemp <- filter(LoanStats, is.na(desc) == FALSE)
LoanStatsTemp <- separate(LoanStatsTemp,desc, 
                          c('desc_date', 'desc'), sep = '>', extra = 'merge', fill = 'left')
LoanStatsTemp <- LoanStatsTemp[,c('member_id','desc_date','desc')]
LoanStats <- subset(LoanStats, select = -desc ) 
LoanStats <- left_join(LoanStats, LoanStatsTemp, by = 'member_id')

#(2) 
#Splitting all useful columns with dates into a month and year section
#this will allow for future time series analysis 
#The desired columns are: 
#issue_d,earliest cr line, desc_date (must be converted into dif format first)

cl <- makeCluster(no_cores)

LoanStats[,'desc_date'] <- parSapply(cl = cl, LoanStats[,'desc_date'], function(x) 
  sub('Borrower added on ', '', x))

LoanStats[,'desc_date'] <- parSapply(cl = cl, LoanStats[,'desc_date'], function(x) 
  gsub(' ', '',x))

LoanStats[,'desc_date'] <- parSapply(cl = cl, LoanStats[,'desc_date'], function(x) 
  format(as.Date(as.character(x), '%m/%d/%y'), '%b-%Y'))


stopCluster(cl)

LoanStats <- separate(LoanStats, col = desc_date, c('DescMnth','DescYr'), sep = '-')
LoanStats <- separate(LoanStats, col = issue_d, c('IssueMnth','IssueYr'), sep = '-')
LoanStats <- separate(LoanStats, col = earliest_cr_line, c('1stCrL_Mnth','1stCrL_Yr'),
                      sep = '-')

#(3)
#modifying 'int_rate' col to be in numeric format as a decimal & stripping xx's

cl <- makeCluster(no_cores)

LoanStats[,'int_rate'] <- parSapply(cl = cl, LoanStats[,'int_rate'], function(x) 
  as.numeric(gsub('%', '',x))/100)

LoanStats[,'zip_code'] <- parSapply(cl = cl, LoanStats[,'zip_code'], function(x) 
  as.numeric(gsub('xx', '', x)))

stopCluster(cl)





#RejectStats initial manipulation to a more readable form

#Collecting the initial data from RejectStatsA.csv file
RejectStats <- read_csv('RejectStatsA.csv',col_types = cols(ApplicationDate = 'c'),
                        skip = 1, col_names = TRUE)
#converting RejectStats into a data table, it is to large to do operations
#on it using my computer without the optimized memory
RejectStats <- data.table(RejectStats)
#Stripping unnecessary column
RejectStats <- RejectStats[,-c(9)]
#(ii) RejectStats
#(1) split col 2 into month & year
#(2) remove xx's from zipcodes

#(1) - splitting the application date column into a year, month, and day column.
cl <- makeCluster(no_cores)
RejectStats[,2] <- parSapply(cl = cl, RejectStats[,2], function(x)
  format(as.Date(as.character(x),'%Y-%m-%d'),'%Y-%m-%d-%a'))
stopCluster(cl)


RejectStats <- separate(RejectStats, col = ApplicationDate, 
                        c('AppYr','AppMnth','AppDay','AppWDay'), sep = '-')

#(2) quickly removing the xx's from zipcodes and % from DTI
cl <- makeCluster(no_cores)
RejectStats[,'ZipCode'] <- parSapply(cl = cl, RejectStats[,'ZipCode'], function(x) 
  as.character(gsub('xx', '',x)))
RejectStats[,'DTI'] <- parSapply(cl = cl, RejectStats[,'DTI'], function(x) 
  as.numeric(gsub('%', '',x)))/100
stopCluster(cl)


#######################################################################
#finish this code by writing RejectStats and LoanStats to our database#
#######################################################################

#Creating a table from a .csv file
dbWriteTable(conn = db, name = 'LoanStats', LoanStats, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

dbWriteTable(conn = db, name = 'RejectStats', RejectStats, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbListTables(db)