setwd("C:/Users/andrew.reyes/Desktop/R Folder/Working Directory")

#install.packages("readxl") 
#install.packages("multcompView")
#install.packages("writexl")
#install.packages("readr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyr")



library(readxl) #loads up the readxl package to load in .xlsx files
library(dplyr) #used in conjunction with tidyr
library(ggplot2) #used for graphing and plots as well as saving images
library(multcompView) #For connecting letters operation
library(tidyr) #used for the parse_number function & in conjunction with dplry
library(writexl) #for writing excel files to save summary stats 
library(readr) #used for the parse_number function & in conjunction with dplry


RRData<- read_excel("11.20.23.RateData.xlsx") #readxl#read in excel file as a dataframe 

#str(RRData) #for Demo

RRData2<-lapply(RRData, function(x) (gsub("[-,]","",x))) #BASER#replaces all "-" with a space""in the loaded dataframe and creates RRData2

#?lapply() #for Demo
#?gsub() #for Demo
#str(RRData2) #for Demo

RRData2$Packaging<-substring(RRData2$Packaging,3) #BASER# removes the first 2 characters from the package code, which is the line

?substring() #for Demo


RRData$Packaging<-substring(RRData$Packaging,4) #BASER#removes the first 3 characters from the package code column in RRData for parsing later for "dates"



dates<-as.character(parse_number(RRData$Packaging)) #tidyr#pulls out only the numbers from original dataframe column packaging and creates a new 

dates<-formatC(as.numeric(dates),width = 6,format = 'd',digits = -5,flag = '0')  #BASER#reformats dates as a number and defines the width of the number to be 6 to match the mmddyy width. If the width is not 6 then it adds a 0 at position 6


#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/formatC #fordemo

dates<-as.data.frame(strptime(dates,"%m%d%y")) #tidyr#converts all of the numbers into date format for dates dataframe

#?strptime() #for demo

RRData<-cbind(RRData,dates) #BASER#adds the newly formatted as dates "dates" dataframe to the originally imported RRData dataframe


colnames(RRData)<-c("FirstName","LastName","UserCode","Num","Packaging","age","TastingOrder","Comment","Date")#BASER#changes the names of the columns in RRData dataframe

RRData<-RRData[(order(as.Date(RRData$Date))),] #BASER#adds the newly formatted as dates "dates" dataframe to the originally imported RRData dataframe


resaov<- aov(age~as.factor(Packaging), data = RRData2) #stats#does anova for age by packaging for the RRData2 set

#str(RRData)

#str(RRData2)

#?aov()

AgeSummary<- group_by(RRData,Packaging) %>%  #dplyr#groups together the data in RRData data frame by Packaging and calculates the mean and standard deviation. The table is then sorted ascending by default by the package dates
  summarise(mean=mean(age),sd=sd(age)) %>%
  arrange(Packaging)

#str(AgeSummary)

RRResults<-TukeyHSD(resaov)  #stats#The Tukey HSD performs a multiple comparisons analysis on the data and uses data from the anova data frame from earlier. The multiple comparisons Tukey data is then stored in a data frame named RRResults

#RRResults #for demo

cld<-multcompLetters4(resaov,RRResults) #multcompLetters adds a connecting letters layer to the Tukey test. It utilizes the anova and tukey output data frames for this and stores in another dataframe called cld

#cld

cld2 <- as.data.frame.list(cld$`as.factor(Packaging)`) #BASER#generates a list from cld that creates this as a data frame



AgeSummary$cld2 <- cld2$Letters #BASER#Combines the mean data to the connecting letters data


dates2<-parse_number(AgeSummary$Packaging) #readr#pulls out just the numbers from the packaging codes in the above data frame and creates a value list called dates2


dates2<-formatC(as.numeric(dates2),width = 6,format = 'd',digits = -5,flag = '0') #BASER#this looks at all of the the dates2 value list and ensures that they all have a length of 6 digits. If it is not 6 digits it will insert a 0 at the beginning 

dates2<-as.data.frame(strptime(dates2,"%m%d%y")) #BASER#leading 0 created in above line necessary for the conversion of the dates 2 string into a date format. This line changes dates2 to a data frame as well

AgeSummary<-cbind(AgeSummary,dates2$`strptime(dates2, "%m%d%y")`) #BASER#appends the newly formatted as dates, dates to the AgeSummary table (which now has the connecting letters)



colnames(AgeSummary)<-c("Packaging","mean","sd","cld2","Date") #BASER#simply changes the column names in the AgeSummary dataframe to be more interpretable


AgeSummary<-AgeSummary[(order(as.Date(AgeSummary$Date))),] #sorts the age summary data in descending order by date

Combination<-as.data.frame(paste(signif(AgeSummary$mean,2),AgeSummary$cld2,sep = "//")) #BASER#creates a data frame with one column that has the means combined with the connecting letter

AgeSummary<-cbind(AgeSummary,Combination) #BASER#adds the above data frame onto the Age Summary data frame

colnames(AgeSummary)<-c("Packaging","mean","sd","cld2","Date","Combination") #BASER#makes the column names of the new AgeSummary more interpretable



ggplot(RRData, mapping = aes(x = reorder(Packaging,Date),y = age))+ #ggplot2#creates the boxplot for the data
  geom_boxplot(aes(fill = Packaging))+
  geom_point()+
  geom_count()+
  labs(x="Package", y = "Age Rating") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_text(data = AgeSummary,aes(x = Packaging, y = max(mean), label = Combination),size = 4, vjust = -5, hjust = -0.3)


ggsave("01.19.23RatePlot.png",width = 8,height = 3,dpi = 1000) #ggplot2#saves the plot to a filename of your choice into the working directory

write_xlsx(AgeSummary,"C:/Users/andrew.reyes/Desktop/R Folder/Working Directory\\01.19.23RateReport.xlsx") #writexl#saves the summary data into an excel with a filename of your choice into the working directory
