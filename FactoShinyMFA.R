setwd("C:/Users/andrew.reyes/Desktop/R Folder/Working Directory")

#install.packages('FactoMineR')

#install.packages('Factoshiny')

#install.packages('writexl')

#install.packages('readxl')

library(FactoMineR)

library(writexl)

library(Factoshiny)

library(readxl)

beer<- read_excel("1.25.24 Hard Lemonade Competitive set.xlsx") #reads in the dataframe for GPA

beer2<- read_excel("1.25.24 Hard Lemonade Competitive set.xlsx")

beer<-as.data.frame(beer)

rownames(beer)<-beer$SampleID #adds the product names for the index but also creates a first column with brandnames

beer<-beer[,-(1:1)] #removes the brand names from the index but leaves numbers as the index

#sets the brands as the index from the import copy/beer2

beer<-as.data.frame(beer)

Factoshiny(beer)
