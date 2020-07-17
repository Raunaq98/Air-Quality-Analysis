directory<- getwd()

data_1999 <- read.table(paste0(directory,"/data/1999.txt"),comment.char="#",sep = "|",header=FALSE,na.strings = "")
data_2012 <- read.table(paste0(directory,"/data/2012.txt"), comment.char="#",sep = "|",header=FALSE,na.strings = "")

cnames<- readLines(paste0(directory,"/data/1999.txt"),1)
cnames<- strsplit(cnames,"|",fixed = TRUE)
cnames2<- readLines(paste0(directory,"/data/2012.txt"),1)
cnames2<- strsplit(cnames2,"|",fixed = TRUE)

names(data_1999)<- make.names(cnames[[1]])
names(data_2012)<- make.names(cnames2[[1]])

PM_1999 <- data_1999$Sample.Value
PM_2012<- data_2012$Sample.Value

summary(PM_1999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    7.20   11.50   13.74   17.90  157.10   13217 

summary(PM_2012)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -10.00    4.00    7.63    9.14   12.00  908.97   73133 

# we can see that the mean and median has gone down
# the peak in 2012 is too high for measurement and might have been a faulty measurement

boxplot(PM_1999,PM_2012,names=c("1999","2012"))
# since the boxplot looks skewed, we can take the log and then plot it

boxplot(log10(PM_1999),log10(PM_2012),names = c("1999","2012"))


# summary of PM_1999 shows negative values which are not possible

negative<- PM_2012<0
sum(negative, na.rm = TRUE)
# [1] 26474

# we want to see if these negatve values occur ar certain points of time

date<- data_2012$Date
date<- as.Date(as.character(date), "%Y%m%d")

hist(date,"month")
# this prints the date vector grouped by month
# most of the measurements occur between december and june

hist(date[negative],"month")
# not much negative values in summer months
# we may want to investigate this later on.

# one direct way of comparison would be to pick a monitor that has been around since 1999
# and comparing PM2.5 levels on it

# we will look in state code 36

site1999 <- subset(data_1999,State.Code == 36, c(County.Code,Site.ID))

# there will repeated values since there are multiple entries

site1999<- unique(site1999)

site2012<- unique(subset(data_2012, State.Code == 36, c(County.Code,Site.ID)))

# in order to make comparison, we need comparable objects

site1999<- paste(site1999[,1],site1999[,2],sep=".")
# chr [1:33] "1.5" "1.12" "5.73" "5.80" "5.83" "5.110" "13.11" "27.1004" "29.2" "29.5" "29.1007" "31.3" ...
site2012<- paste(site2012[,1],site2012[,2],sep=".")
# chr [1:18] "1.5" "1.12" "5.80" "5.133" "13.11" "29.5" "31.3" "47.122" "55.1007" "61.79" "61.134" "63.2008" ...

common <- intersect(site1999,site2012)
common
# [1] "1.5"     "1.12"    "5.80"    "13.11"   "29.5"    "31.3"    "63.2008" "67.1015" "85.55"   "101.3" 

# since there are multiple monitors that have been present in both time periods,
# we will want the ones with large number of observations

data_1999$county.site <- with(data_1999,paste(County.Code, Site.ID,sep="."))
data_2012$county.site <- with(data_2012,paste(County.Code, Site.ID,sep="."))

temp_1999<- subset(data_1999, State.Code == 36 & county.site %in% common)
temp_2012<- subset(data_2012, State.Code == 36 & county.site %in% common)

# thus, we now have 1999 and 2012 data belonging to the concerned monitors
# we now wan them split county.site wise

sapply(split(temp_1999,temp_1999$county.site), nrow)
#   1.12     1.5   101.3   13.11    29.5    31.3    5.80 63.2008 67.1015   85.55 
#    61     122     152      61      61     183      61     122     122       7 
sapply(split(temp_2012,temp_2012$county.site), nrow)
#   1.12     1.5   101.3   13.11    29.5    31.3    5.80 63.2008 67.1015   85.55 
#    31      64      31      31      33      15      31      30      31      31 

# we are now focusing on 1.5 data as it has considerable number of observations in both

data_1999_1.5 <- subset(data_1999, State.Code ==36 & County.Code ==1 & Site.ID == 5)
data_2012_1.5 <- subset(data_2012, State.Code ==36 & County.Code ==1 & Site.ID == 5)

library(dplyr)
library(ggplot2)
library(gridExtra)
pm2.5_1999_1.5 <- select(data_1999_1.5,Sample.Value,Date)
pm2.5_1999_1.5$Date<- as.Date(as.character(pm2.5_1999_1.5$Date),"%Y%m%d")

pm2.5_2012_1.5 <- select(data_2012_1.5,Sample.Value,Date)
pm2.5_2012_1.5$Date<- as.Date(as.character(pm2.5_2012_1.5$Date),"%Y%m%d")


g1<- ggplot(pm2.5_1999_1.5,aes(Date,Sample.Value))
p1<- g1 + geom_point() +  labs( x="time",y="PM 2.5 Levels", title = "PM 2.5 levels in 1999")+
  coord_cartesian(ylim = c(0,25)) + stat_summary(mapping= aes(Date,Sample.Value),fun.y = median)
print(p1)

g2<- ggplot(pm2.5_2012_1.5,aes(Date,Sample.Value))
p2<- g2 + geom_point() + labs( x="time",y="PM 2.5 Levels", title = "PM 2.5 levels in 2012")
+coord_cartesian(ylim = c(0,25))
print(p2)

require(gridExtra)
plot1<- p1
plot2<- p2
grid.arrange(plot1,plot2,ncol=2)


#### comparing statewise averages

mean_1999 <- with(data_1999,tapply(Sample.Value,State.Code,mean,na.rm=TRUE))
mean_2012 <- with(data_2012,tapply(Sample.Value,State.Code,mean,na.rm=TRUE))

df_1999<- data.frame( state= names(mean_1999),mean = mean_1999)
df_2012<- data.frame( state= names(mean_2012),mean = mean_2012)

df_merged <- merge(df_1999,df_2012, by= "state")

par(mfrow=c(1,1))

with(df_merged,plot(rep(1999,52),df_merged[,2],xlim=c(1998,2013)))
with(df_merged,points(rep(2012,52),df_merged[,3],xlim=c(1998,2013)))



