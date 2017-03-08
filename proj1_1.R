install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
set.seed(123)

#Code objective, compare the data sets LoanStats and RejectStats
#Look for any interesting comparisons that can be made

LoanStats = read.csv("LoanStats3a.csv", header=FALSE, stringsAsFactors=FALSE)
RejectStats = read.csv("RejectStatsA.csv",header=FALSE, stringsAsFactors=FALSE)

#Examining the Loan Stats dataset to get an understanding of each variable
names(LoanStats)
headLoanStats <- LoanStats[2,]
headLoanStats2 <- LoanStats[2,]
i = 1
for (i in 1:50)
{
  print(headLoanStats2 <- LoanStats[2,i])
  print(headLoanStats2 <- LoanStats[3,i])
  print(headLoanStats2 <- LoanStats[4,i])
  print(headLoanStats2 <- LoanStats[5,i])
  
  print('######################################')
  print('######################################')
  
}

i = 1
for (i in 1:500)
{
  print(headLoanStats2 <- LoanStats[i,21])
  
}

N <- dim(LoanStats)[1]
N2 <- dim(RejectStats)[1]

LoanStatsNew <- LoanStats[2:42540,1:18]
head(LoanStatsNew,n = 10)
LoanStatsNew <- LoanStats[1:42540,1:18]

#Examining the Reject Stats Dataset to get an understanding of each variable
i = 1
for (i in 1:9)
{
  print(RejectStats[2,i])
  print(RejectStats[3,i])
  print(RejectStats[4,i])
  print(RejectStats[5,i])
  
  print('######################################')
  print('######################################')
  
}

i = 1
for (i in 1:500)
{
  print(RejectStats[i,3])
  
}


#We can see two interesting things to compare from these data sets
#The first is Employment length and loan acceptance
#The second is Debt to income ratio and loan acceptance

#In order to examine the data further we must break it down into consumable peices

#Loan Stats data cleaning

#Step(1)
#Extracting the variables we want
#In this case since debt to income ratio isn't availible we are going to
#use the formula monthly total loan payment / monthly income, 
#which is the loan payment and interest divided by the monthly income of the
#Individual

#Step(1a)
#Extracting Variables from LoanStats dataset
#The variables we will need from this are:
#V8,V12,V14
loan_data <- select(LoanStatsNew,V8,V12,V14)
loan_data <- loan_data[3:N,]
N <- N - 2 # book keeping
colnames(loan_data) <- c('LoanPmt','EmpLength', 'AnnInc')
loan_data <- data.frame(loan_data)
loan_data[,c(1,3)] <- sapply(loan_data[c(1,3)],as.numeric)
head(loan_data, n = 5)



#Step(1b)
#Extracting Variables from RejectStats dataset
#The variables we will need from this are:
#V5,V8

reject_data <- data.frame(select(RejectStats,V5,V8))
reject_data <- reject_data[3:N2,]
N2 <- N2 - 2 #Book keeping
colnames(reject_data) <- c('D2IR','EmpLength')

#Step(2)
#Our data has officially been collected. However, Its still very messy
#Our next step will be ensuring all data is in numeric form 

#Two new data frames to hold these numeric values
LoanData <- data.frame(LoanPmt = numeric(),
                       EmpLength = numeric(),
                       AnnInc = numeric())
RejectData <- data.frame(D2IR = numeric(),
                         EmpLength = numeric())


#Step(2a)
#Cleaning the data of unnecessary characters and converting it to numeric
#Replacing the '%' symbols in reject_data so it can be converted to numeric
# quick peice of test code to ensure logic is correct.
x = reject_data[1,1] 
y <- sub('[[:punct:]]','', x)

x2 = reject_data[2,1]
y2 = sub('<','',y3)

y3 = sub('years*','',x2)

#Before going further we need to create samples, The code in its 
#Current iteration cant processes the loop below without taking 
#Hours or days
N3 = 10000
LoanSample <- sample_n(loan_data,N3,replace = FALSE)
RejectSample <- sample_n(reject_data,N3,replace = FALSE)


#LoanSample Cleaning and numeric conversion loop
i = 1
for (i in 1:N3)
{
  LoanData[i,1] <- as.numeric(LoanSample[i,1])
  
  y <- LoanSample[i,2]
  y <- sub('[[:punct:]]','',y)
  y <- sub('years*','',y)
  y <- sub(' ','',y)
  LoanData[i,2] <- as.numeric(y)
  
  LoanData[i,3] <- as.numeric(LoanSample[i,3])
}


#RejectData Cleaning and numeric conversion loop
j = 1
for (j in 1:N3)
{
  x = RejectSample[j,1]
  x <- sub('%','',x)
  x <- sub(' ','',x)
  RejectData[j,1] <- as.numeric(x)
  
  x2 = RejectSample[j,2]
  x2 <- sub('[[:punct:]]','',x2)
  x2 <- sub('years*','',x2)
  x2 <- sub(' ','',x2)
  RejectData[j,2] <- as.numeric(x2)
}

#Step(2b)
#Removing missing or extremely absurd values from data
LoanData <- filter(LoanData, is.na(LoanPmt) == FALSE & 
                             is.na(EmpLength) == FALSE &
                             is.na(AnnInc) == FALSE)

RejectData <- filter(RejectData, D2IR >= 0 &
                     D2IR <  1000 &
                     is.na(EmpLength) == FALSE)

summary(LoanData)
summary(RejectData)

#Step(2c)
#Adding D2IR variable to LoanData and
#Marking LoanData as accepted loan requests
#Marking RejectData as rejected loan requests
#And finally merging the data sets together

LoanData <- mutate(LoanData, D2IR = LoanPmt/AnnInc)
LoanData <- mutate(LoanData, LoanRequest = 'Accepted')
LoanData <- LoanData[,c(2,4,5)]
RejectData <- mutate(RejectData,D2IR = D2IR / 100)
RejectData <- mutate(RejectData, LoanRequest = 'Rejected')
RejectData <- RejectData[,c(2,1,3)]

LoanRequests <- rbind(LoanData,RejectData)
summary(LoanRequests)

#Step(3)
#Analysis of the data
N1 <- dim(LoanData)[1]
N2 <- dim(RejectData)[1]
N3 <- dim(LoanRequests)[1]
BinNum <- sqrt(N3)
RangeEmpLength <- (10 - 1)
RangeD2IR <- (9.949400)
#Bin Width is just range of data divided by number of bins

#BoxPlot on employment length vs Loan Request
bp <- ggplot(LoanRequests, aes(x= LoanRequest, y=EmpLength, fill = LoanRequest )) 
bp2 = p + geom_boxplot() 
bp3 = p2 + scale_fill_manual(breaks = c("Accepted","Rejected"),values=c("green","red"))



#Histograms of accepted loans and rejected loans based on debt to income ratio
qplot(D2IR, data = LoanData)
qplot(D2IR, data = RejectData)


