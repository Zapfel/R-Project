install.packages('readr')
install.packages('foreach')
install.packages('doParallel')
install.packages('data.table')
install.packages('plyr')
install.packages('ggplot2')
install.packages("maps")
install.packages('mapproj')

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
set.seed(123)
no_cores <- detectCores() - 1  #detects number of cores in the system

#The Goal of this program is to further improve cleaning the data we 
#practiced with in proj1_1 while speeding up our code in segments as well

#basic code setup
#make copies of these function whenever we are trying to do parallel processing
cl <- makeCluster(no_cores)
registerDoParallel(cl) #only needed in foreach() statements
#End all our parallel processing segments with this function
stopCluster(cl)


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

#(2) quickly removing the xx's from zipcodes
cl <- makeCluster(no_cores)
RejectStats[,'ZipCode'] <- parSapply(cl = cl, RejectStats[,'ZipCode'], function(x) 
                      as.numeric(gsub('xx', '',x)))
stopCluster(cl)



#Plotting
L <- LoanStats
R <- RejectStats

#plot 1, Loan granted vs. Annual income
P1 <- ggplot(L, aes(x=loan_amnt , y=annual_inc))
P1 + geom_line() +ylim(0,100000)

RDays <- data.table(table(R[,'AppDay']))
#raw data time series
ggplot(RDays, aes(V1, N, group = 1)) + geom_line()

RDays$N

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
ggplot(RDays, aes(V1, N, group = 1)) + geom_line() + ylim(min(RDays[,2]), max(RDays[,2]))


DoW =c("Mon",   "Tue",  "Wed"  , "Thu", "Fri"  , "Sat",   "Sun")
DoW2 = c(2,6,7,5,1,3,4)

R2 <- data.frame(table(R[,'AppWDay']))
R2$Var1 <- R2$Var1 <- as.factor(R2$Var1)
R2$Var1 <- factor(R2$Var1, levels = DoW)
R2 <- R2[order(R2$Var1),]


ggplot(R2, aes(y=Freq, x=Var1, group = 1)) + geom_line()

R3 <- data.table(table(R[,'AppMnth']))
min <- min(R3[,'N'])
max <- max(R3[,'N'])

p1 <- ggplot(R3, aes(y = N, x = V1, group = 1)) + geom_line() + ylim(0,100000)
p1 <- p1+geom_hline(yintercept = min, color = 'green')
p1 <- p1+geom_hline(yintercept = max, color = 'red')

MoY =c("Jan",'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


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


#now to create the plot of states
States <- map_data('state')
Map <- merge(States, MapData, by = 'region', all.MapData = T)
Map <- Map[order(Map$order),]
p <- ggplot(data = Map, aes(x=long, y=lat, group = group))
p <- p + geom_polygon(aes(fill = LoansPercUSA))
p  + geom_path() +  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") +
     coord_map()
  
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












