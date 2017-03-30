
library(ggplot2)

setwd("~/data science")
DoW =c("Mon",   "Tue",  "Wed"  , "Thu", "Fri"  , "Sat",   "Sun")
###############################################################################################

##########
#Plot One#
##########

#Days of Month vs loan frequency

ggplot(RDays, aes(V1, N, group = 1)) + geom_line() + ylim(min(RDays[,2]), max(RDays[,2]))



###########################################################################################

##########
#Plot Two#
##########

R2 <- data.table(dbReadTable(conn = db, "R2", header = TRUE))
R2 <- data.frame(table(R[,'AppWDay']))
R2$Var1 <- R2$Var1 <- as.factor(R2$Var1)
R2$Var1 <- factor(R2$Var1, levels = DoW) #Set DoW variable or wont run properly
R2 <- R2[order(R2$Var1),]

#Days of week vs loan frequency
ggplot(R2, aes(y=Freq, x=Var1, group = 1)) + geom_line()

###########################################################################################

############
#Plot Three#
############

R3 <- data.table(dbReadTable(conn = db, "R3", header = TRUE))

#Months vs Loan Frequency

p1 <- ggplot(R3, aes(y = N, x = V1, group = 1)) + geom_line() + ylim(0,100000)
p1 <- p1+geom_hline(yintercept = min, color = 'green')
p1 <- p1+geom_hline(yintercept = max, color = 'red')


##########################################################################################

###########
#Plot Four#
###########


Map <- Map[order(Map$order),]
p <- ggplot(data = Map, aes(x=long, y=lat, group = group))
p <- p + geom_polygon(aes(fill = LoansPercUSA))
p  + geom_path() +  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") +
  coord_map()

##########################################################################################

##########################################################################################


###########
#Plot Five#
###########

ggplot(RDayLoan, aes(AppDay,V1 , group = 1)) + geom_line() 


###########################################################################################

##########
#Plot Six#
##########
RWDL <- data.table(dbReadTable(conn = db, "RWDL", header = TRUE))
RWDL$AppWDay <- RWDL$AppWDay <- as.factor(RWDL$AppWDay)
RWDL$AppWDay <- factor(RWDL$AppWDay, levels = DoW) #Set DoW variable or wont run properly
RWDL <- RWDL[order(RWDL$AppWDay),]

#Days of week vs loan frequency
ggplot(RWDL, aes(y=V1, x=AppWDay, group = 1)) + geom_line()

##########################################################################################

############
#Plot Seven#
############


RM <- data.table(dbReadTable(conn = db, "RM", header = TRUE))
min <- min(R3[,'N'])
max <- max(R3[,'N'])
#Months vs Loan Frequency

p4 <- ggplot(RM, aes(y = V1, x = AppMnth, group = 1)) + geom_line() + ylim(0,25000)

##########################################################################################

############
#Plot Eight#
############


MapR <- MapR[order(MapR$order),]
p <- ggplot(data = MapR, aes(x=long, y=lat, group = group))
p <- p + geom_polygon(aes(fill = NormStateLoanR))
p  + geom_path() +  scale_fill_gradientn(colours= heat.colors(10), na.value="grey90") +
  coord_map()

#########################################################################################

###########
#Plot Nine#
###########

MapRDTI <- MapRDTI[order(MapRDTI$order),]
p <- ggplot(data = MapRDTI, aes(x=long, y=lat, group = group))
p <- p + geom_polygon(aes(fill = V1))
p  + geom_path() +  scale_fill_gradientn(colours= rev(heat.colors(10)), na.value="grey90") +
  coord_map()


#########################################################################################

##########
#Plot Ten#
##########

MapRE2 <- MapRE2[order(MapRE2$order),]
REL <- MapRE2[,c(1,2,3,8)]


            
MapRE2 <- MapRE2[order(MapRE2$order),]
p <- ggplot(data = MapRE2, aes(x=long, y=lat, group = group))
p <- p + geom_polygon(aes(fill = EmploymentLength))
p <- p + geom_path() + coord_map() 











