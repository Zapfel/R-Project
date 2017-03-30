LoanStats = read.csv("LoanStats3a.csv", header=FALSE, stringsAsFactors=FALSE)
RejectStats = read.csv("RejectStatsA.csv",header=FALSE, stringsAsFactors=FALSE)


#Inital sub() Function did not work due to the '*' character.
#In regular sub() this only works on first instance of the
#Character prior to it while in the gsub() function
#the '*' Character affects all instances of the character prior
LoanStats2 <- LoanStats[,c(11,20,21,22)]


i = 1
for (i in 1:42540)
{
  LoanStats2[i,1] <- gsub(',*','',LoanStats2[i,1])
             
  LoanStats2[i,2] <- gsub(',*','',LoanStats2[i,2])
             
  LoanStats2[i,3] <- gsub(',*','',LoanStats2[i,3])
             
  LoanStats2[i,4] <- gsub(',*','',LoanStats2[i,4])

}


#Updating the original LoanStats dataset to reflect the changes made
#from the above loop
LoanStats[,c(11,21,22)] <- LoanStats2[,c(1,3,4)]
LoanStats <- LoanStats[,c(1:18,21:50)]


#Code to write updated csv files to contain our new data.
#This data can be properly read into our sql files.
#The problem with our database code is when it reads in the ','
#character it views it without an escape key so it splits data into
#two seperate columns causing it to not be able to read into our 
#database. The above loop fixes this by removing all commas.

LoanWrite <- write.csv(LoanStats[2:42540,],file = 'LoanStats.csv', quote = TRUE, 
                       sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                       col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))


N = 50000
N2 = 55493

#splitting RejectStats into 16 subgroups since
#R cannot process the full RejectStats dataset in a loop at one time

RejectStats1 <- RejectStats[1:50000,]
RejectStats2  <- RejectStats[50001:100000,]
RejectStats3 <- RejectStats[100001:150000,]
RejectStats4 <- RejectStats[150001:200000,]
RejectStats5 <- RejectStats[200001:250000,]
RejectStats6 <- RejectStats[250001:300000,]
RejectStats7 <- RejectStats[300001:350000,]
RejectStats8 <- RejectStats[350001:400000,]
RejectStats9 <- RejectStats[400001:450000,]
RejectStats10 <- RejectStats[450001:500000,]
RejectStats12 <- RejectStats[500001:550000,]
RejectStats13 <- RejectStats[550001:600000,]
RejectStats14 <- RejectStats[600001:650000,]
RejectStats15 <- RejectStats[650001:700000,]
RejectStats16 <- RejectStats[700001:755493,]

#Removing all punctiation from strings in RejectStats

for(i in 1:N)
{
  RejectStats1[i,3] <- gsub(',*', '', RejectStats1[i,3])                                              
}

for(i in 1:N)
{
  RejectStats2[i,3] <- gsub(',*', '', RejectStats2[i,3])                                              
}

for(i in 1:N)
{
  RejectStats3[i,3] <- gsub(',*', '', RejectStats3[i,3])                                              
}

for(i in 1:N)
{
  RejectStats4[i,3] <- gsub(',*', '', RejectStats4[i,3])                                              
}

for(i in 1:N)
{
  RejectStats5[i,3] <- gsub(',*', '', RejectStats5[i,3])                                              
}

for(i in 1:N)
{
  RejectStats6[i,3] <- gsub(',*', '', RejectStats6[i,3])                                              
}

for(i in 1:N)
{
  RejectStats7[i,3] <- gsub(',*', '', RejectStats7[i,3])                                              
}

for(i in 1:N)
{
  RejectStats8[i,3] <- gsub(',*', '', RejectStats8[i,3])                                              
}

for(i in 1:N)
{
  RejectStats9[i,3] <- gsub(',*', '', RejectStats9[i,3])                                              
}

for(i in 1:N)
{
  RejectStats10[i,3] <- gsub(',*', '', RejectStats10[i,3])                                              
}

##### accidently skipped rejectstats11

for(i in 1:N)
{
  RejectStats12[i,3] <- gsub(',*', '', RejectStats12[i,3])                                              
}

for(i in 1:N)
{
  RejectStats13[i,3] <- gsub(',*', '', RejectStats13[i,3])                                              
}

for(i in 1:N)
{
  RejectStats14[i,3] <- gsub(',*', '', RejectStats14[i,3])                                              
}

for(i in 1:N)
{
  RejectStats15[i,3] <- gsub(',*', '', RejectStats15[i,3])                                              
}

for(i in 1:N2)
{
  RejectStats16[i,3] <- gsub(',*', '', RejectStats16[i,3])                                              
}



#saving the RejectStats dataset into multiple csv files

RejectStats1 <- write.csv(RejectStats1,file = 'RejectStats1.csv', quote = TRUE, 
                         sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                         col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats2 <- write.csv(RejectStats2,file = 'RejectStats2.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats3 <- write.csv(RejectStats3,file = 'RejectStats3.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats4 <- write.csv(RejectStats4,file = 'RejectStats4.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats5 <- write.csv(RejectStats5,file = 'RejectStats5.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats6 <- write.csv(RejectStats6,file = 'RejectStats6.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats7 <- write.csv(RejectStats7,file = 'RejectStats7.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats8 <- write.csv(RejectStats8,file = 'RejectStats8.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats9 <- write.csv(RejectStats9,file = 'RejectStats9.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats10 <- write.csv(RejectStats10,file = 'RejectStats10.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats11 <- write.csv(RejectStats11,file = 'RejectStats11.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats12 <- write.csv(RejectStats12,file = 'RejectStats12.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats13 <- write.csv(RejectStats13,file = 'RejectStats13.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats14 <- write.csv(RejectStats14,file = 'RejectStats14.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats15 <- write.csv(RejectStats15,file = 'RejectStats15.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
RejectStats16 <- write.csv(RejectStats16,file = 'RejectStats16.csv', quote = TRUE, 
                          sep = ',', eol = '\n', na = 'NA', dec = '.', row.names = FALSE,
                          col.names = LoanStats[2,1:50], qmethod = c("escape", "double"))
