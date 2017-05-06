Project 1_4, Initial data analysis on the loans completed.

Please Run the scripts in the following order
1) DB initialize
2) Data Clean
3) PP_Graphics
4) Loan Summary

If done correctly you will get a document like the Loan Report one I have included
so the full script is not required to be run to see the final results.

Other Software required:
This program uses Sqlite and it must be installed to create a functioning database.
It is not required but makes using the program in the future require much less
work.


Libraries Required:
library(data.table)
library(tidyr)
library(dplyr)
library(stringi)
library(maps)
library(ggplot2)
library(sqldf)
library(zoo)
library(foreach)
library(doParallel)

Additional Notes:
This Script uses all the Lending Club accepted and rejected loans which leads
to a very large file. My computer uses almost all of its 8gb of ram processing
speicifc segments due to R running all data in RAM. 


