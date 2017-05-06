##############################################################################################
#Project 1_4 Pre Processing, Graphics
#CSV files can be found at 
#https://www.lendingclub.com/info/download-data.action
#In the current iteration lending club has the following CSV's We need them all.
##########################################################################################


library(data.table)
library(tidyr)
library(dplyr)
library(stringi)
library(maps)
library(ggplot2)
library(sqldf)
library(zoo)


setwd("C:/Users/SHARON/Desktop/R/Data Science Project 1/Proj1_4")
sqlite    <- dbDriver("SQLite")

#Connecting to DB and accessing the data
con <- dbConnect(sqlite, dbname='R1_4.SQLite')
dbBegin(con)

LD <- data.table(dbReadTable(con, 'LD'))
MD <- data.table(dbReadTable(con, 'MD'))

dbDisconnect(con, shutdown=TRUE)

#Pre-setup data frames, and constants, that will be used frequently.
LA <- LD%>%filter(Status == 'Accepted')
LA <- data.table(LA)

LR <- LD%>%filter(Status == 'Rejected')
LR <- data.table(LR)

N <- dim(LD)[1]
N1 <- dim(LA)[1]
N2 <- dim(LR)[1]

#########################################################################################


#############
#Plots 1 - 3#
#############

#Frequency Plots, Line graphs MoTY

#1) Normalized counts of loans accepted vs Month of the year
#2) Normalized counts of loans rejected vs Month of the year
#3) Difference in normalized number of accepted loans & rejected Loans


#Pre-Thoughts. Since some months are shorter than others,
#Feb 28 normally 29 on leap years. Since our dates are from 2008 - 2016 we have 3
#Others without 31 are April, June, September, November.
#Using Elementary Bayesian Stats we can estimate number of loans
#these months would have achieved if given 31 day time frames.

#P(A|B) = P(B|A)*P(B)/P(A)
#Using this we get ((3/8)*(31/29)+(5/8)*(31/28)) for Feb and (31/30) for the rest.

#1) Plot One, Normalized Counts of loans accepted vs MoTY
LAF_MoTY <- data.table(table(LA[,'AppMo']))
LAF_MoTY$N <- as.numeric(LAF_MoTY$N)
LAF_MoTY[2,2] <- as.numeric(LAF_MoTY[2,2]*((3/8)*(31/29)+(5/8)*(31/28)))
LAF_MoTY[c(4,6,10,11),2] <- LAF_MoTY[c(4,6,10,11),2]*(31/30)
LAF_MoTY$Norm <- LAF_MoTY$N / N1


#2) Plot Two, Normalized Counts of loans rejected vs MoTY
LRF_MoTY <- data.table(table(LR[,'AppMo']))
LRF_MoTY$N <- as.numeric(LRF_MoTY$N)
LRF_MoTY[2,2] <- as.numeric(LRF_MoTY[2,2]*((3/8)*(31/29)+(5/8)*(31/28)))
LRF_MoTY[c(4,6,10,11),2] <- LRF_MoTY[c(4,6,10,11),2]*(31/30)
LRF_MoTY$Norm <- LRF_MoTY$N / N2

#3) Normalized Counts of loans accepted, divided by loans rejected vs MoTY
LDF_MoTY <- LAF_MoTY
LDF_MoTY$Norm <- LAF_MoTY$Norm / LRF_MoTY$Norm

#########################################################################################


#############
#Plots 4 - 6#
#############

#Frequency Plots, Line graphs, % increase in frequency over year

#4) % increase in loans rejected by year, 2008 - 2016
LAF_Year <- data.table(table(LA[,'AppYr']))
LAF_Year$N <- LAF_Year$N / N1
for(i in 0:7)
{
  LAF_Year[(9-i),2] <- (LAF_Year[(9-i),2] / LAF_Year[(9-i-1),2])
}
LAF_Year[1,2] <- 1

#5) % increase in loans rejected by year, 2008 - 2016
LRF_Year <- data.table(table(LR[,'AppYr']))
LRF_Year$N <- LRF_Year$N / N2
for(i in 0:7)
{
  LRF_Year[(9-i),2] <- (LRF_Year[(9-i),2] / LRF_Year[(9-i-1),2])
}
LRF_Year[1,2] <- 1

#6) ratio of rate of increase in loans accepted vs loans rejected in years 2008 - 2016
LDF_Year <- LAF_Year
LDF_Year$N <- LAF_Year$N / LRF_Year$N



#########################################################################################


#############
#Plots 7 - 9#
#############

#Frequency Plots, Mapped 

#7) Loans Accepted by state
LAFmap <- data.table(table(LA[,'State'])) 
LAFmap <- setnames(LAFmap, 'V1', 'Abrev.')
LAFmap <- merge(MD,LAFmap, by = 'Abrev.')
LAFmap[,'N'] <- (LAFmap[,'N'] / LAFmap[,'StatePop']) / N1


#8) Loans Rejected by state
LRFmap <- data.table(table(LR[,'State'])) 
LRFmap <- setnames(LRFmap, 'V1', 'Abrev.')
LRFmap <- merge(MD,LRFmap, by = 'Abrev.')
LRFmap[,'N'] <- (LRFmap[,'N'] / LRFmap[,'StatePop']) / N2


#9) ratio in Loans accepted and Rejected by state
LDFmap <- LAFmap
LDFmap$N <- (LAFmap$N / LRFmap$N)



#########################################################################################


###############
#Plots 10 - 12#
###############

#Loan Size, Line Graphs.

#10) Average Loan Accepted vs MoTY
LALS_MoTY <- data.table(LA)[, mean(as.numeric(LoanSize)), by = AppMo]

#11) Average Loan Rejected vs MoTY
LRLS_MoTY <- data.table(LR)[, mean(as.numeric(LoanSize)), by = AppMo]

#12) Average ratio between Accepted and Rejected loan size vs MoTY
LDLS_MoTY <- LALS_MoTY
LDLS_MoTY$V1 <- LALS_MoTY$V1 / LRLS_MoTY$V1


#########################################################################################


###############
#Plots 13 - 15#
###############

#Loan Size, Maps

#13) Average Loan Accepted plotted over US 48
LALSmap <- data.table(LA)[, mean(as.numeric(LoanSize)), by = State]
LALSmap <- setnames(LALSmap, 'State', 'Abrev.')
LALSmap <- merge(MD,LALSmap, by = 'Abrev.')

#14) Average Loan Rejected plotted over US 48
LRLSmap <- data.table(LR)[, mean(as.numeric(LoanSize)), by = State]
LRLSmap <- setnames(LRLSmap, 'State', 'Abrev.')
LRLSmap <- merge(MD,LRLSmap, by = 'Abrev.')

#15) Average ratio between Accepted and Rejected loan size plotted over US 48
LDLSmap <- LALSmap
LDLSmap$V1 <- LALSmap$V1 / LRLSmap$V1


#########################################################################################

###############
#Plots 16 - 18#
###############

#DTI, Line Graphs.

#16) Average DTI of an accepted loan vs MoTY
LADTI_MoTY <- data.table(LA)[, mean(as.numeric(DTI)), by = AppMo]

#17) Average DTI of a rejected loan vs MoTY
LRDTI_MoTY <- data.table(LR)[, mean(as.numeric(DTI)), by = AppMo]

#18) Average ratio between Accepted and Rejected DTI vs MoTY
LDDTI_MoTY <- LADTI_MoTY
LDDTI_MoTY$V1 <- LADTI_MoTY$V1 / LRDTI_MoTY$V1


#########################################################################################


###############
#Plots 19 - 21#
###############

#DTI, Maps

#19) Average DTI Accepted plotted over US 48
LADTImap <- data.table(LA)[, mean(as.numeric(DTI)), by = State]
LADTImap <- setnames(LADTImap, 'State', 'Abrev.')
LADTImap <- merge(MD,LADTImap, by = 'Abrev.')

#20) Average DTI Rejected plotted over US 48
LRDTImap <- data.table(LR)[, mean(as.numeric(DTI)), by = State]
LRDTImap <- setnames(LRDTImap, 'State', 'Abrev.')
LRDTImap <- merge(MD,LRDTImap, by = 'Abrev.')

#21) Average ratio between Accepted and Rejected DTI plotted over US 48
LDDTImap <- LADTImap
LDDTImap$V1 <- LADTImap$V1 / LRDTImap$V1

#########################################################################################

############
#Plot 22,23#
############

#22) State with highest loan acceptance rate by Title
LATmap <- data.table(table(LA[,c('Title','State')]))
LAT <- data.table(table(LA[,'Title']))
LAT <- setnames(LAT, 'V1', 'Title')
LAT <- setnames(LAT, 'N', 'N2')
LATmap <- merge(LATmap,LAT, by = 'Title')
LATmap <- LATmap %>% mutate(N3 = N/N2) %>% group_by(Title,State,N3) %>% summarise() %>%
          group_by(State) %>% mutate(MaxN = max(N3)) %>% filter(N3 == MaxN) 
LATmap <- data.table(LATmap)
LATmap <- setnames(LATmap, 'State', 'Abrev.')
LATmap <- merge(MD,LATmap, by = 'Abrev.')

#23) State with highest loan rejection rate from Title
LRTmap <- data.table(table(LR[,c('Title','State')]))
LRT <- data.table(table(LR[,'Title']))
LRT <- setnames(LRT, 'V1', 'Title')
LRT <- setnames(LRT, 'N', 'N2')
LRTmap <- merge(LRTmap,LRT, by = 'Title')
LRTmap <- LRTmap %>% mutate(N3 = N/N2) %>% group_by(Title,State,N3) %>% summarise() %>%
  group_by(State) %>% mutate(MaxN = max(N3)) %>% filter(N3 == MaxN)
LRTmap <- data.table(LRTmap)
LRTmap <- setnames(LRTmap, 'State', 'Abrev.')
LRTmap <- merge(MD,LRTmap, by = 'Abrev.')


#########################################################################################

######################
#Writing Tables to DB#
######################

con <- dbConnect(sqlite, dbname='R1_4.SQLite')
dbBegin(con)

dbWriteTable(con = con, name = 'LAF_MoTY', LAF_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRF_MoTY', LRF_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDF_MoTY', LDF_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbWriteTable(con = con, name = 'LAF_Year', LAF_Year, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRF_Year', LRF_Year, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDF_Year', LDF_Year, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbWriteTable(con = con, name = 'LAFmap', LAFmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRFmap', LRFmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDFmap', LDFmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbWriteTable(con = con, name = 'LALS_MoTY', LALS_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRLS_MoTY', LRLS_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDLS_MoTY', LDLS_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbWriteTable(con = con, name = 'LALSmap', LALSmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRLSmap', LRLSmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDLSmap', LDLSmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

dbWriteTable(con = con, name = 'LADTI_MoTY', LADTI_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRDTI_MoTY', LRDTI_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDDTI_MoTY', LDDTI_MoTY, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbWriteTable(con = con, name = 'LADTImap', LADTImap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRDTImap', LRDTImap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LDDTImap', LDDTImap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


dbWriteTable(con = con, name = 'LATmap', LATmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(con = con, name = 'LRTmap', LRTmap, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

dbCommit(con)
dbDisconnect(con, shutdown=TRUE)
rm(list=ls(all=TRUE))




