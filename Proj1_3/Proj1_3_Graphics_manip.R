
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
library(zipcode)
library(ggplot2)
data(zipcode)
setwd("~/data science")
set.seed(123)
no_cores <- detectCores() - 1
sqlite    <- dbDriver("SQLite")
db <- dbConnect(sqlite, dbname='R1_3.SQLite')
setwd("~/data science")

#plots 1 - 4 are frequency vs times, and state
#plots 5 - 8 are loan amount vs times, and state
#plots DTI, employment length, and loan purpose vs state
#plots 
#Plotting
#Initial setup
L <- data.table(dbReadTable(conn = db, "LoanStats", header = TRUE))
R <- data.table(dbReadTable(conn = db, "RejectStats", header = TRUE))
StatesPopData <- data.table(read_csv('StatesPopData.csv', col_names = TRUE))
MoY =c("Jan",'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


#########################################################################################


#getting a more accurate depiction of the time series data for
#number of loans rejected vs day of month

# there are 60 observed months in our data set, meaning 1/5th of febs are 
#leap years so 1/5 of febs have 29 days meaning 56/60 have 29 days, 55/60 have 
#30 days and 35/60 have 31 days

#also since banks are close on weekends its safe to assume that those days
#can be ruled out as well from our sample, so lets make a new table to reflect this
RWeekDays <- filter(R, AppWDay != 'Sat' & 
                      AppWDay != 'Sun')
RDays <- data.table(table(RWeekDays[,'AppDay']))

#adjusting RDays accordingly
RDays[29,2] <- as.integer(RDays[29,2]*60/56)
RDays[30,2] <- as.integer(RDays[30,2]*60/55)
RDays[31,2] <- as.integer(RDays[31,2]*60/35)

#We can see from our plot that even accounting for weekends and
#months that did not have a set number of days we still get a 
#series that shows the best time to apply for a loan is towards the
#end of the month

#Reference Plot one in Proj1_3_Graphics

dbWriteTable(conn = db, name = 'RDays', RDays, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

############################################################################################

#Plot two, Days of Week vs Loan frequency, when is best time during the week to
#request a loan?

R2 <- data.frame(table(R[,'AppWDay']))
R2$Var1 <- R2$Var1 <- as.factor(R2$Var1)
R2$Var1 <- factor(R2$Var1, levels = DoW) #Set DoW variable or wont run properly
R2 <- R2[order(R2$Var1),]

dbWriteTable(conn = db, name = 'R2', R2, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

###########################################################################################

#Plot Three, Months of year vs loan rejection frequency

R3 <- data.table(table(R[,'AppMnth']))
min <- min(R3[,'N'])
max <- max(R3[,'N'])

dbWriteTable(conn = db, name = 'R3', R3, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

###########################################################################################

#plot four
#Heatmap Creation
#Importing StatePopData

StatesPopData <- data.table(read_csv('StatesPopData.csv', col_names = TRUE))
#setting up data sets to be joined
StatesData <- StatesPopData[,c(1,2,4,6)]
R4 <- data.table(table(filter(R[,'State'],!is.na(R[,'State']))))
setnames(R4, 'V1', 'States')
setnames(StatesData,'State', 'region')
setnames(StatesData,'Abrev.','States')
setnames(StatesData,'pop%_usa10','USA10Pop')
MapData <- inner_join(R4,StatesData)
MapData <- mutate(MapData, LoansPercUSA = ((N/5)/pop_2010)/USA10Pop)
MapData[,'region'] <- sapply(MapData[,'region'], function(x) tolower(x))
MapData <- data.table(dbReadTable(conn = db, "MapData", header = TRUE))

#Heat map of loans normalized based on state population relative to united states population
#and normalized on number of loans rejected from that state vs total # of rejections

States <- map_data('state')
Map <- merge(States, MapData, by = 'region', all.MapData = T)

dbWriteTable(conn = db, name = 'Map', Map, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

#now to create the plot of states, see plot four in Graphics


#From LoansPercUSA variable, its not a true quality analysis but it
#shows roughly the idea we are trying to do in practice.

#Goal is to take number of rejected loan observations divide them by the average
#population per state to get a normalized loan rejection average
#then to divide this by the percent of the population this represents in the
#united states. in effect this normalizes the number of rejections to the average
#over the whole united states. 

#For future to get an accurate map would want united states population data for every
#year of the data we are referencing in this case it would be 2007 - 2012.
#Further more we would split data by year average these values and then take mean
#of the five yearly calculated populations. 

############################################################################################

############################################################################################

#Plot Five
#Average Loan value vs day of the week 

#getting a more accurate depiction of the time series data for
#number of loans rejected vs day of month

# there are 60 observed months in our data set, meaning 1/5th of febs are 
#leap years so 1/5 of febs have 29 days meaning 56/60 have 29 days, 55/60 have 
#30 days and 35/60 have 31 days

#also since banks are close on weekends its safe to assume that those days
#can be ruled out as well from our sample, so lets make a new table to reflect this
RWeekDays <- filter(R, AppWDay != 'Sat' & 
                      AppWDay != 'Sun')
RDayLoan <- data.table(RWeekDays)[, sum(AmountRequested.), by = AppDay]

#adjusting RDays accordingly
RDayLoan[29,2] <- as.integer(RDayLoan[29,2]*60/56)
RDayLoan[30,2] <- as.integer(RDayLoan[30,2]*60/55)
RDayLoan[31,2] <- as.integer(RDayLoan[31,2]*60/35)

for (i in 1:31)
{
  RDayLoan[i,2] <- RDayLoan[i,2]/RDays[i,2]
}

dbWriteTable(conn = db, name = 'RDayLoan', RDayLoan, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

############################################################################################

#Plot six, Days of Week vs Loan frequency, when is best time during the week to
#request a loan?

RWDL <- data.table(R)[, sum(AmountRequested.), by = AppWDay]
R2 <- data.frame(table(R[,'AppWDay']))

for (i in 1:7)
{
  RWDL[i,2] <- RWDL[i,2]/R2[i,2]
}
RWDL
dbWriteTable(conn = db, name = 'RWDL', RWDL, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

###########################################################################################

#Plot seven, Months of year vs average loan rejection request

RM <- data.table(R)[, sum(AmountRequested.), by = AppMnth]
R3 <- data.table(table(R[,'AppMnth']))


for (i in 1:12)
{
  RM[i,2] <- RM[i,2]/R3[i,2]
}

dbWriteTable(conn = db, name = 'RM', RM, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)

###########################################################################################

#plot Eight
#Heatmap Creation
#creating required data
RS <- data.table(mutate(R, NAN = !is.na(State)))
RS <- RS[RS$NAN,]
RS <- data.table(RS)[, sum(AmountRequested.), by = State]
R4 <- data.table(table(filter(R[,'State'],!is.na(R[,'State']))))
StatesData <- StatesPopData[,c(1,2,4,6)]

setnames(RS, 'State', 'States')
setnames(StatesData,'State', 'region')
setnames(StatesData,'Abrev.','States')
setnames(StatesData,'pop%_usa10','USA10Pop')
#setting up data sets to be joined
RS <- arrange(RS, States)
RS <- cbind(RS,R4[,2])
MapRData <- inner_join(RS,StatesData)

#normalize data relative to % of loans occuring inside of that state
MapRData <- mutate(MapRData, NormStateLoanR = V1/N)

MapRData[,'region'] <- sapply(MapRData[,'region'], function(x) tolower(x))


States <- map_data('state')
MapR <- merge(States, MapRData, by = 'region', all.MapRData = T)

dbWriteTable(conn = db, name = 'MapR', MapR, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)




#########################################################################################


#Plot 9
#DTI of rejected Loans by state
#setting up initial data
#Checking for, and clearing missing values.
StatesData <- StatesPopData[,c(1,2,4,6)]
States <- map_data('state')
RDTI <- data.table(mutate(R, NAN = !is.na(DTI)))
RDTI <- RDTI[RDTI$NAN,]
RDTI <- data.table(RDTI)[, mean(DTI), by = State]
RDTI <- RDTI[-c(52),]

#append to state data
setnames(RDTI, 'State', 'States')
setnames(StatesData,'State', 'region')
setnames(StatesData,'Abrev.','States')
RDTI <- arrange(RDTI, States)
MapRDTI_Data <- inner_join(RDTI,StatesData)
MapRDTI_Data[,'region'] <- sapply(MapRDTI_Data[,'region'], function(x) tolower(x))
MapRDTI <- merge(States, MapRDTI_Data, by = 'region', all.MapRDTI_Data = T)

dbWriteTable(conn = db, name = 'MapRDTI', MapRDTI, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)


#########################################################################################

#average employment length by state
StatesPopData[,2] <- arrange(StatesPopData[,2], Abrev.)
SNames <- sapply(StatesPopData[,2], function(x) as.character(x))


RE2 <-  data.table(R[,c('State','EmploymentLength')] %>%
                   filter(EmploymentLength != 'n/a' & State != 'n/a'  & State != 'DC')%>%
                   group_by(EmploymentLength) %>% mutate(n1 = n())%>%
                   group_by(EmploymentLength,State) %>% mutate(n2 = n()) %>%
                   mutate(n3 = n2 / n1) %>%  group_by(State,EmploymentLength,n1,n2,n3)%>%
                   summarise() %>% group_by(State) %>% mutate(MaxRej = max(n3))
                   %>% filter(n3 == MaxRej)) 
                   
RE2 <- RE2[,c(1,2)]
setnames(RE2, 'State', 'States')
setnames(StatesData,'State', 'region')
setnames(StatesData,'Abrev.','States')
MapRE2_Data <- inner_join(RE2,StatesData)
MapRE2_Data[,'region'] <- sapply(MapRE2_Data[,'region'], function(x) tolower(x))
States <- map_data('state')
MapRE2 <- merge(States, MapRE2_Data, by = 'region', all.MapRE2_Data = T)

dbWriteTable(conn = db, name = 'MapRE2', MapRE2, 
             row.names = FALSE, header = TRUE ,sep = ',', eol = '\n', overwrite = TRUE)







