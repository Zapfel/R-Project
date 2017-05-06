##############################################################################################
#Project 1_4 Pre Processing Data Cleaning
#CSV files can be found at 
#https://www.lendingclub.com/info/download-data.action
#In the current iteration lending club has the following CSV's We need them all.
##########################################################################################
setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4")



library(foreach)
library(doParallel)
library(data.table)
library(tidyr)
library(dplyr)
library(stringi)
library(maps)
library(ggplot2)
library(sqldf)
library(zoo)
library(sqldf)

no_cores <- detectCores() - 1
v <- rbind(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
MapData <- map_data('state')
sqlite    <- dbDriver("SQLite")

#Assigning Number of rows in LS & RS and pre-allocating the variables
LSR <- 1319098
LS <- data.table(nrow(LSR))

RSR <- 11079386
RS <- data.table(nrow(RSR))

#Connecting to DB and accessing the data
con <- dbConnect(sqlite, dbname='R1_4.SQLite')
dbBegin(con)

LS <- data.table(dbReadTable(con, 'LS'))
RS <- data.table(dbReadTable(con, 'RS'))
SPD <- data.table(dbReadTable(con, 'SPD'))

dbDisconnect(con, shutdown=TRUE)

#Iniital cleaning. What do we have to do?
#For LS we need to:
# 1) Convert dti to a decimal
# 2) Split issue_d into months and year
# 3) Convert months to a number
# 4) change purpose into a column of single words
# 5) Change column names so LS and RS have a set of columns they share

#For RS we need to:
# 1) convert Debt-To-Income Ratio into a decimal
# 2) Split App Date into Months and years
# 3) Filter Loan Title to be similiar to purpose column in LS 
# 4) Change column names so LS and RS have a set of columns they share


#########################################################################################


########
#Step 1#
########

# LS: Convert dti to a decimal
# RS: convert Debt-To-Income Ratio into a decimal

#Setting column names to be similar
setnames(LS, 'dti', 'DTI')
setnames(RS, 'Debt.To.Income.Ratio', 'DTI')

#Changing DTI
LS$DTI <- as.numeric(LS$DTI)/100
RS$DTI <- as.numeric(gsub('%', '',RS$DTI))/100
gc()

#Removing extreme values from DTI
RSDTI <- ifelse(RS$DTI > 50 | RS$DTI < 0,FALSE,TRUE)
LSDTI <- ifelse(LS$DTI > 50 | LS$DTI < 0,FALSE,TRUE)
RS <- RS[RSDTI,]
LS <- LS[LSDTI,]
#########################################################################################


########
#Step 2#
########

#Split issue_d into months and year
#Split App Date into Months and years

#Setting column names to be similar
setnames(LS, 'issue_d', 'AppDate')
setnames(RS, 'Application.Date', 'AppDate')

#Formatting dates
#vectorizing
LS$AppDate <- format(as.yearmon(LS$AppDate,'%b-%Y'), '%Y-%m')
LS <- separate(LS, col = AppDate, c('AppYr','AppMo'), sep = '-')

RS$AppDate <- format(as.Date(as.character(RS$AppDate),'%Y-%m-%d'),'%Y-%m')
RS <- separate(RS, col = AppDate, c('AppYr','AppMo'), sep = '-')

#Dropping year 2007 from data due to low number of loans rejected or accepted
LS <- LS %>% arrange(AppYr,AppMo) %>% filter(AppYr != 2007)
RS <- RS %>% arrange(AppYr,AppMo) %>% filter(AppYr != 2007)


#########################################################################################


########
#Step 3#
########

#change purpose into a column of single words
#Filter Loan Title to be similiar to purpose column in LS

#We need a vector of words to filter from LS

LP <- LS[,'purpose']
LP <- na.omit(LP) #only 3 w/ NA
LP <- data.table(table(LP))
LP <- LP[,1]
LP <- as.vector((unlist(strsplit(LP$LP, '_'))))
#dropping unnecessary elements
drop <- c('card','improvement','major','small','renewable','educational','consolidation')
add <- c('student') #changed educational to student since loans are self titled in RS
LP <- as.vector(setdiff(LP,drop))
LP = c(LP,add)

#we now have a vector LP with a set of words we want to add to LS and RS to replace
#loan purpose and loan title with

LSP <- as.vector(LS$purpose)
RSLT <- as.vector(RS[,'Loan.Title'])


#Modifying the purpose and Loan Title columns to give matrix of boolean values
LSP <-  gsub('educational','student',LSP)
LSP <- gsub('credit_card','credit',LSP)
LSP <- sapply(LP, function(x) grepl(x,LSP))
gc()

#need to do in chunks for RS since original is to large
RSLTN <- length(RSLT) #May have to readjust
RSLT1 <- as.vector(unlist(RSLT[1:(RSLTN/3)])) 
RSLT2 <- as.vector(unlist(RSLT[((RSLTN/3)+1):(2*RSLTN/3)]))
RSLT3 <- as.vector(unlist(RSLT[((2*RSLTN/3)+1):(RSLTN)]))

#rm(RSLT)
RSLT1 <- sapply(LP, function(x) stri_detect_fixed(RSLT1,x))
RSLT2 <- sapply(LP, function(x) stri_detect_fixed(RSLT2,x))
RSLT3 <- sapply(LP, function(x) stri_detect_fixed(RSLT3,x))

#binding all to RSLT variable.
RSLT <- rbind(RSLT1,RSLT2,RSLT3)


#creating check variable to eliminate rows where all values are false
RSLTcheck <- apply(RSLT,1,function(x) any(x))


#Row elimination in RS & RSLT
RS <- RS[RSLTcheck,] 
RSLT <- RSLT[RSLTcheck,]

#converting matrix of bools to matrix of 1's and 0's
LSP <- apply(LSP,2, function(x) as.integer(x))
RSLT <- apply(RSLT,2, function(x) as.integer(x))

#Using matrix multiplication with the bool matrix, and vector v above
#to create a column of values that can select elements out of LP.
#these values will become the new purpose & Loan Title columns

LSP <- LSP%*%v
RSLT <- RSLT%*%v

#RSLT may have some values greater than 14, these need to be dropped
RSLTcheck2 <- as.vector(ifelse(RSLT > 14,FALSE,TRUE))
#Remove values from RSLT & RS
RS <- RS[RSLTcheck2,]
RSLT <- RSLT[RSLTcheck2,]

#Assigning characters based off the bool matrices

LSPc <- sapply(LSP,function(x) LP[x])
RSLTc <- sapply(RSLT,function(x) LP[x])

#Assigning new names for purpose and title

setnames(LS, 'purpose', 'Title')
setnames(RS, 'Loan.Title', 'Title')

#Assigning variables to RS & LS
LS$Title <- LSPc
RS$Title <- RSLTc


#########################################################################################


########
#Step 4#
########


#Finalizing Column names changes, adding additional pertinant state data,
#Adding column indicating accepted or rejected to LS & RS, ordering the columns properly 
#and combining the two data sets


#Finalizing remaining column names
setnames(LS, 'loan_amnt', 'LoanSize')
setnames(LS, 'emp_length', 'EmpLen')
setnames(LS, 'addr_state','State')

setnames(RS, 'Amount.Requested', 'LoanSize')
setnames(RS, 'Employment.Length', 'EmpLen')


#Adding Status column to differentiate when data sets are combined
LS <- LS%>%mutate(Status = 'Accepted')
RS <- RS%>%mutate(Status = 'Rejected')

#Re-ordering columns
Col_Order <- c('Status','Title','LoanSize','AppMo','AppYr','DTI','State','EmpLen')
LS <- LS[,Col_Order]
RS <- RS[,Col_Order]

#Joining Data
LD <- data.table(rbind(LS,RS))

#Last filters, clearing out remaining values that will be unused.
LD <- as.vector(LD) %>% filter(State != 'DC') %>% filter(State != 'AK') %>%
                        filter(State != 'HI') %>% filter(EmpLen != 'n/a')
LD <- data.table(LD)

#Combining States and MapData for final touch
colnames(SPD) <- c('region','Abrev.','StatePop')
SPD$StatePop <- as.numeric(gsub(',*', '',SPD$StatePop))
SPD <- arrange(SPD,region)
SPD$region <- tolower(SPD$region)
MD <- merge(MapData,SPD)
MD <- MD[,-6]


#Converting table back to Data.tables and storing to DB
MD <- data.table(MD)

con <- dbConnect(sqlite, dbname='R1_4.SQLite')
dbBegin(con)

dbWriteTable(con = con, name = 'LD', LD, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = FALSE)


dbWriteTable(con = con, name = 'MD', MD, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

dbCommit(con)
dbDisconnect(con, shutdown=TRUE)
rm(list=ls(all=TRUE))

