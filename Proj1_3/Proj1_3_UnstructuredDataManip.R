
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(maps)
library(sqldf)
library(readr)
setwd("~/data science")
set.seed(123)
no_cores <- detectCores() - 1
sqlite    <- dbDriver("SQLite")
db <- dbConnect(sqlite, dbname='R1_3.SQLite')
setwd("~/data science")


#Initial setup
L <- data.table(dbReadTable(conn = db, "LoanStats", header = TRUE))
R <- data.table(dbReadTable(conn = db, "RejectStats", header = TRUE))



### moving to Initial manip in 1_4
#########################################################################################

#Plot 9 Can we determine who will be rejected from getting a loan and who will be accepted
#we will use the following variables that both data sets share
#R AmountRequested, AppYr,AppMnth, DTI, State, EmploymentLength
#Additionally we want to filter through loan.Title to determine if enough for a
#reasonable sample match up with any from purpose 

#L from this data set we will use Loan_amt, emp_length, issue_month and issue_yr and DTI
#Our final goal will be to see how accurately we can predict if someone will be
#Rejected from getting a loan or not


#To start lets clean L's purpose column and R's Loan.Title column

#This code figures out all the major words found in L's Purpose column
#and coverts them to individual strings
LP <- L[,'purpose']
LP <- na.omit(LP) #only 3 w/ NA
LPT <- data.table(table(LP))
LPT <- LPT[,1]
LPT <- as.vector((unlist(strsplit(LPT$LP, '_'))))
#since we are using this list to find data from rejection, we want to change some items
#First we want to change education to student, since its a personal description, 
#remove the word major and delete the word card, small, and improvement. since they're
#redundant with the second words they relate too.
drop <- c('card','improvement','major','small','renewable','educational','consolidation')
add <- c('student')
LPT <- as.vector(setdiff(LPT,drop))
LPT = c(LPT,add)

cl <- makeCluster(no_cores)
clusterExport(cl,"LP")
clusterEvalQ(cl, {library(stringr)})
LPD <- parSapply(cl = cl, LP, function(x) gsub('educational','student',x))
clusterExport(cl,"LPD")
LPD <- parSapply(cl = cl, LPD, function(x) gsub('credit_card','credit',x))
LPD <-  parSapply(cl = cl, LPT, function(y) sapply( LPD, function(x) grepl(y,x)))
stopCluster(cl)

LPD <- apply(LPD,2, function(x) as.integer(x))
v <- rbind(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
LPD <- LPD%*%v
LPD <- data.table(sapply(LPD, function (x) LPT[x]))
LPD <- filter(LPD, is.na(LPD) == FALSE)



#We are going to use the strings from LPT to see if any match from R using the filter
#function

RL <- data.table(sapply(R[,'Loan.Title'], function(x) as.character(tolower(x))))

cl <- makeCluster(no_cores)
clusterExport(cl, "LPT")
clusterExport(cl,"RL")
clusterEvalQ(cl, {library(stringr)})
RLD <-  parSapply(cl = cl, LPT, function(y) sapply( RL, function(x) grepl(y,x)))
stopCluster(cl)

### remove below to *** original developed code including to see improvement
data.table(RL)   
data.table(RLD)
RL <- data.table(cbind(RL,RLD))
RL <- filter(RL, car != FALSE | credit != FALSE | debt != FALSE |
                home != FALSE | house != FALSE | purchase != FALSE |
               medical != FALSE | moving != FALSE | other != FALSE | energy != FALSE |
               business != FALSE | vacation != FALSE | wedding != FALSE | student != FALSE)

#After this step drop all logical rows, since we have filtered out necessary data.
#Need to find a better way to do this
### remove above up to ***
RL2 <- RL[,2:15]
RL3 <- data.table(apply(RL2,1, function(x) any(x)))# use in RL2[RL3,] to filter rows
#Replacement for the above filter now we need a way to convert these rows that
#have the strings we need into a usable form I believe best move will be with a sub
#function


#### regression cleaning section

#With the strings filtered we also need a way to change the 
#Reject values to ones similar to the Loan values to be comparable.

#Step 1 convert our DF of booleans to integers
RL2 <- apply(RL2,2, function(x) as.integer(x)) #student returning NA
v <- rbind(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
RL2 <- RL2%*%v
RL2 <- data.table(sapply(RL2, function (x) LPT[x]))
RL2 <- filter(RL2, is.na(RL2) == FALSE)
RL2 <- data.table(RL2)
### Moving to initial manip in 1_4
