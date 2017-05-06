##############################################################################################
#Project 1_4 Pre Processing DB initialization
#CSV files can be found at 
#https://www.lendingclub.com/info/download-data.action
#In the current iteration lending club has the following CSV's We need them all.
#For Accepted Loans: 07 - 11, 12-13, 14, 15, 16Q1 - 4 Q1 - 4 are all seperate CSV's
#For Rejected Loans: 07 - 12, 13-14, 15, 16Q1 - 4, again Q1-4 are all seperate CSV's
#
#Our goal with this code is to do data analysis on the following variables, since
#they are shared by both Rejected Loans, and Accepted Loans.
#
#Loan_amt, Application Mo & Yr, Emp Length, DTI, State, & purpose
#For LS this is columns 3,12,16,21,24,25
#For RS this is columns 1,2,3,5,7,8
#
#Extra notes:
#For the monetDBLite, you do not need SQL downloaded. However, you do need to create
#a new folder to hold it
#for future use of monetdb it functions just like a standard SQLDB
#
#You must change the following code for this to work properly
#
#DB storage foloder
#(Ldbdir <- "C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4/Proj1_4_DB"
#
#Accepted Loans CSV Storage folder
#setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4/Data/AcceptedLoans")
#
#Rejected Loans CSV storage folder
#setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4/Data/RejectedLoans")
#
#
##########################################################################################

setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4")
library(foreach)
library(doParallel)
library(data.table)
library(sqldf)

no_cores <- detectCores() - 1

#initializing DB
sqlite    <- dbDriver("SQLite")

#Setting up initial constants
LScol <- c(3,12,16,21,24,25)
RScol <- c(1,2,3,5,7,8)




#Reading in the Data
setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4/Data/AcceptedLoans")
Files <- list.files(pattern = "[.]csv$")
cl <- makeCluster(no_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {library(data.table)})
LS <- foreach(i = Files, .combine = rbind) %dopar% fread(i,header = TRUE, select = LScol)
stopCluster(cl)

setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4/Data/RejectedLoans")
Files <- list.files(pattern = "[.]csv$")
cl <- makeCluster(no_cores)
registerDoParallel(cl)
clusterExport(cl,"Files")
clusterEvalQ(cl, {library(data.table)})
RS <- foreach(i = Files, .combine = rbind) %dopar% fread(i,header = TRUE, select = RScol)
stopCluster(cl)

setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4/Data")
Files <- list.files(pattern = "[.]csv$")
SPD <- fread(Files,header = TRUE, select = c(1,2,3))


#Writing to Database
setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4")
con <- dbConnect(sqlite, dbname='R1_4.SQLite')
dbBegin(con)
dbWriteTable(con = con, name = 'LS', LS, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = FALSE)

dbWriteTable(con = con, name = 'RS', RS, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = FALSE)

dbWriteTable(con = con, name = 'SPD', SPD, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbCommit(con)
dbDisconnect(con, shutdown=TRUE)
rm(list=ls(all=TRUE))





