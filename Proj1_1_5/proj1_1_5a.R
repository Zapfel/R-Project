#code designed to create an SQLite server connected to R

install.packages('sqldf')
install.packages('XLConnect')
install.packages('RSQLite')
library(sqldf)
library(tcltk)
#Creating the database
sqlite    <- dbDriver("SQLite")
db <- dbConnect(sqlite, dbname='R.SQLite')

#Creating a table from a .csv file
dbWriteTable(conn = db, name = 'LoanStats', value = 'LoanStats.csv', 
             row.names = FALSE, header = FALSE,sep = ',', eol = '\n', overwrite = TRUE)

#The file RejectStats was to large to load off my comptuer at once 
#So I had to break it into multiple peices to load it into my database



dbWriteTable(conn = db, name = 'RejectStats1', value = 'RejectStats1.csv', 
             row.names = FALSE, header = FALSE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats2', value = 'RejectStats2.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats3', value = 'RejectStats3.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats4', value = 'RejectStats4.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats5', value = 'RejectStats5.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats6', value = 'RejectStats6.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats7', value = 'RejectStats7.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats8', value = 'RejectStats8.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats9', value = 'RejectStats9.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats10', value = 'RejectStats10.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats11', value = 'RejectStats11.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats12', value = 'RejectStats12.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats13', value = 'RejectStats13.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats14', value = 'RejectStats14.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats15', value = 'RejectStats15.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)
dbWriteTable(conn = db, name = 'RejectStats16', value = 'RejectStats16.csv', 
             row.names = FALSE, header = TRUE, sep = ',', eol = '\n', overwrite = TRUE)


#Combinding all of the smaller tables into a fully formed table 


dbGetQuery(db,'CREATE TABLE RejectStats as
      SELECT * FROM RejectStats1
      UNION ALL
      SELECT * FROM RejectStats2
      UNION ALL
      SELECT * FROM RejectStats3
      UNION ALL
      SELECT * FROM RejectStats4
      UNION ALL
      SELECT * FROM RejectStats5
      UNION ALL
      SELECT * FROM RejectStats6
      UNION ALL
      SELECT * FROM RejectStats7
      UNION ALL
      SELECT * FROM RejectStats8
      UNION ALL
      SELECT * FROM RejectStats9
      UNION ALL
      SELECT * FROM RejectStats10
      UNION ALL
      SELECT * FROM RejectStats12
      UNION ALL
      SELECT * FROM RejectStats13
      UNION ALL
      SELECT * FROM RejectStats14
      UNION ALL
      SELECT * FROM RejectStats15
      UNION ALL
      SELECT * FROM RejectStats16;', db = 'R.SQLite')

#Functions to check table names, and column names
dbListTables(db)
dbListFields(db, 'LoanStats')
dbGetQuery(db,"Select *FROM LoanStats LIMIT 10")
dbGetQuery(db, "DROP TABLE RejectStats")













