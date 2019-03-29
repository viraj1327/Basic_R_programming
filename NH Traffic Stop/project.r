# Importing CSV and creating dataframe as "NH_tbl"
library(dplyr)
NH_cleaned <- read.csv("NH_cleaned.csv")
NH_tbl <- tbl_df(NH_cleaned)

# Removing records which do not contain an age and getting a count of records
NH_TA <- filter(NH_tbl, NH_tbl$driver_age >0)
NH_TA$nrowTA <- nrow(NH_TA)

# Filtering records for only ones that occurr in 2015

NH_TA$date <- as.Date(NH_TA$stop_date, format= "%Y-%m-%d")
NH_TA <- subset(NH_TA, date> "2014-12-31")

# Creating Age Classes Column as well as the Order Column

for(i in 1:length(NH_TA$id))
# for(i in 1:10) 
{
 if(NH_TA$driver_age[i]>=0 & NH_TA$driver_age[i] <=21)
 {
  NH_TA$AgeClasses[i] <- "Teenagers"
  NH_TA$AgeClassesOrder[i] <- 1
 }else
  if(NH_TA$driver_age[i]>21 & NH_TA$driver_age[i] <=37)
   {
   NH_TA$AgeClasses[i] <- "Millenials"
   NH_TA$AgeClassesOrder[i] <- 2
  }else
   if(NH_TA$driver_age[i]>37 & NH_TA$driver_age[i] <=53)
   {
    NH_TA$AgeClasses[i] <- "Gen X"
    NH_TA$AgeClassesOrder[i] <- 3
   }else
    if(NH_TA$driver_age[i]>53 & NH_TA$driver_age[i]<=72)
    {
     NH_TA$AgeClasses[i] <- "Boomers"
     NH_TA$AgeClassesOrder[i] <- 4
    }else
     {NH_TA$AgeClasses[i] <-"Silent"
    NH_TA$AgeClassesOrder[i] <- 5
     }
}

# Creating Age Class Summary Table after removing NAs and 2014 data

NH_TA %>%
 group_by(AgeClasses) %>%
 summarise(NHcount2=n(), NHcount=n()/nrow)
 
# Creating month column

library(lubridate)
# NH_TA$TAmonthname <- month(ymd(NH_TA$stop_date), label=TRUE) # Using the lubridate library to convert the stop date column into month
NH_TA$TAstop_date <- as.POSIXct(NH_TA$stop_date) # converting factor to calendar date & time format
NH_TA$TAmonthname <- format(NH_TA$TAstop_date,"%B") # converting to month name
NH_TA$TAmonthnumber <- format(NH_TA$TAstop_date,"%m") # converting to month number

# Creating Line Plot

#Create column to count stops for line chart

NH_TA$stop_count <- 1
NH_TA$stop_sum <- sum(NH_TA$stop_count)

library(ggplot2)
library(plotly)

Lp <- ggplot(NH_TA, aes(x=TAmonthnumber,color=AgeClasses,group=AgeClasses))+ geom_line(stat='count')
Lp <- Lp + xlab("Month")+ylab("# of Stops") # labels
Lp <- Lp + ggtitle("Monthly Trend of Stops by Age Class") # title
Lp <- Lp + theme_classic() # White background
ggplotly(Lp)

# Create Plot for County Graph

Top5_County_DriverAge_Class <- filter(NH_TA,NH_TA$county_name=='Merrimack County'|NH_TA$county_name=='Rockingham County'
                  |NH_TA$county_name=='Grafton County'|NH_TA$county_name=='Hillsborough County'|NH_TA$county_name=='Strafford County')
Chart_County <- ggplot(Top5_County_DriverAge_Class,aes(x=county_name,fill=AgeClasses))+geom_bar()
Chart_County <- Chart_County + xlab("County")+ylab("# of Stops") # labels
Chart_County <- Chart_County + ggtitle("County Traffic Stops by Age Class") # title
Chart_County <- Chart_County + theme_classic() # White background
ggplotly(Chart_County)

# Bar Chart for Violations

library(ggplot2)
outcomebar<-ggplot(NH_TA,aes(x=stop_outcome,fill=AgeClasses))+geom_bar(position="dodge")
outcomebar<-outcomebar+ggtitle("Bar Chart of Stop Outcome by Age")
outcomebar<-outcomebar+theme_bw()
outcomebar <- outcomebar + xlab("Violation Type")+ylab("# of Stops") # labels
ggplotly(outcomebar)

# Bar Chart Male/Female

NH_TAMF <- filter(NH_TA,(NH_TA$driver_gender =="M" | NH_TA$driver_gender =="F"))
MFbar<-ggplot(NH_TAMF,aes(x=AgeClasses,fill=driver_gender))+geom_bar()
MFbar<-MFbar+xlab("Age Classes")+ylab("# of Stops")
MFbar<-MFbar+ggtitle("Age Class by Male/Female")
ggplotly(MFbar)

# Final Violation vs Race Plot

May <- c("May")
NH_May <- filter(NH_TA, NH_TA$TAmonthname %in% May)
NH_May$may_stop_day <- format(NH_May$TAstop_date,"%d") # converting to day number

Lp2 <- ggplot(NH_May, aes(x=may_stop_day,color=AgeClasses,group=AgeClasses))+ geom_line(stat='count')
Lp2 <- Lp2 + xlab("Day")+ylab("# of Stops") # labels
Lp2 <- Lp2 + ggtitle("Daily Trend of Stops by Age Class") # title
Lp2 <- Lp2 + theme_classic() # White background
ggplotly(Lp2)
