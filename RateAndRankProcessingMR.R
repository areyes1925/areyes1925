setwd("//harvest/Redirected$/Desktop/R Folder/Working Directory")
#install.packages("readxl") #installs the package to import the excel file
#install.packages("yaml")
#install.packages("multcompView")
#install.packages("reshape2")
#install.packages("RColorBrewer")
#install.packages("writexl")

library(readxl) #loads up the readxl package to execute the command
library(dplyr)
library(ggplot2)
library(multcompView)
library(reshape2)
library(tidyr)
library(RColorBrewer)
library(writexl)
library(readr)


RRData<- read_excel("01.09.23.RateData.xlsx") #read in excel file as a dataframe 

RRData2<-lapply(RRData, function(x) (gsub("[-,]","",x))) #deletes all "-" in the loaded dataframe and creates RRData2

RRData2$Packaging<-substring(RRData2$Packaging,3)

RRData$Packaging<-substring(RRData$Packaging,4) #removes the first 3 characters from the package code column in RRData



dates<-as.character(parse_number(RRData$Packaging)) #pulls out only the numbers from original dataframe column packaging and creates a new 


dates<-formatC(as.numeric(dates),width = 6,format = 'd',digits = -5,flag = '0')  #reformats dates as a number and defintes the width of the number to be 6 to match the mmddyy width. If the width is not 6 then it adds a 0 at position 6

dates<-as.data.frame(strptime(dates,"%m%d%y")) #converts all of the numbers into date format for dates dataframe


RRData<-cbind(RRData,dates) #adds the newly formatted as dates "dates" dataframe to the originally imported RRData dataframe


colnames(RRData)<-c("FirstName","LastName","UserCode","Num","Packaging","age","TastingOrder","Comment","Date")#changes the names of the columns in RRData dataframe

RRData<-RRData[(order(as.Date(RRData$Date))),] #adds the newly formatted as dates "dates" dataframe to the originally imported RRData dataframe


resaov<- aov(age~as.factor(Packaging), data = RRData2) #does anova for age by packaging for the RRData2 set


AgeSummary<- group_by(RRData,Packaging) %>%  #groups together the data in RRData data frame by Packaging and calculates the mean and standard deviation. The table is then sorted ascending by default by the package dates
  summarise(mean=mean(age),sd=sd(age)) %>%
  arrange(Packaging)


RRResults<-TukeyHSD(resaov)  #The Tukey HSD performs a multiple comparisons analysis on the data and uses data from the anova data frame from earlier. The multiple comparisons Tukey data is then stored in a data frame named RRResults



cld<-multcompLetters4(resaov,RRResults) #multcompLetters adds a connecting letters layer to the Tukey test. It utilizes the anova and tukey output data frames for this and stores in another dataframe called cld


cld2 <- as.data.frame.list(cld$`as.factor(Packaging)`) #generates a list from cld that creates this as a data frame



AgeSummary$cld2 <- cld2$Letters #Combines the mean data to the connecting letters data


dates2<-parse_number(AgeSummary$Packaging) #pulls out just the numbers from the packaging codes in the above data frame and creates a value list called dates2


dates2<-formatC(as.numeric(dates2),width = 6,format = 'd',digits = -5,flag = '0') #this looks at all of the the dates2 value list and ensures that they all have a length of 6 digits. If it is not 6 digits it will insert a 0 at the beginning 

dates2<-as.data.frame(strptime(dates2,"%m%d%y")) #leading 0 created in above line necessary for the conversion of the dates 2 string into a date format. This line changes dates2 to a data frame as well

AgeSummary<-cbind(AgeSummary,dates2$`strptime(dates2, "%m%d%y")`) #appends the newly formatted as dates, dates to the AgeSummary table (which now has the connecting letters)



colnames(AgeSummary)<-c("Packaging","mean","sd","cld2","Date") #simply changes the column names in the AgeSummary dataframe to be more interpretable


AgeSummary<-AgeSummary[(order(as.Date(AgeSummary$Date))),] #sorts the age summary data in descending order by date

Combination<-as.data.frame(paste(signif(AgeSummary$mean,2),AgeSummary$cld2,sep = "//")) #just makes a data frame with one column that has the means combined with the connecting letter

AgeSummary<-cbind(AgeSummary,Combination) #adds the above data frame onto the Age Summary data frame

colnames(AgeSummary)<-c("Packaging","mean","sd","cld2","Date","Combination") #makes the column names of the new AgeSummary more interpretable



ggplot(RRData, mapping = aes(x = reorder(Packaging,Date),y = age))+ #creates the boxplot for the data
  geom_boxplot(aes(fill = Packaging))+
  geom_point()+
  geom_count()+
  labs(x="Package", y = "Age Rating") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_text(data = AgeSummary,aes(x = Packaging, y = max(mean), label = Combination),size = 4, vjust = -5, hjust = -0.3)


ggsave("01.19.23RatePlot.png",width = 8,height = 3,dpi = 1000) #saves the plot to a filename of your choice into the working directory

write_xlsx(AgeSummary,"//harvest/Redirected$/Desktop/R Folder/Working Directory\\01.19.23RateReport.xlsx") #saves the summary data into an excel with a filename of your choice into the working directory
