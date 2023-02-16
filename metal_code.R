##set working directory
setwd("C:/Users/ljasn/dropbox/stuff/classes/R-Fresher/")

##check directory is correct
dir()

##load data
metal_bands_2017<-read.csv("metal_bands_2017.csv")

##check data
head(metal_bands_2017)

##look at popularity
hist(metal_bands_2017$fans)

##basic stats
mean(metal_bands_2017$fans)
median(metal_bands_2017$fans)

##age of band
##this will throw an error - why?
metal_bands_2017$split-metal_bands_2017$formed

##look for error
head(metal_bands_2017)

##recoding
table(metal_bands_2017$split)
metal_bands_2017$split[metal_bands_2017$split=="-"]<-NA

##age of band
##try again with recoding
metal_bands_2017$split-metal_bands_2017$formed

##why still an error?
table(metal_bands_2017$formed)

##more recoding
metal_bands_2017$formed[metal_bands_2017$formed=="-"]<-NA

##age of band
##try again with recoding
metal_bands_2017$split-metal_bands_2017$formed

##check something else
class(metal_bands_2017$split)
class(metal_bands_2017$formed)

##try making everything numeric
as.numeric(metal_bands_2017$split)-as.numeric(metal_bands_2017$formed)

##save that as something
age<-as.numeric(metal_bands_2017$split)-as.numeric(metal_bands_2017$formed)

##add it to the dataframe
metal_bands_2017<-cbind(metal_bands_2017,age)

##save it!
write.csv(metal_bands_2017,file="metal_bands_2017_withAge.csv")

##test whether older bands (who eventually split before 2017) are more popular
cor.test(metal_bands_2017$age,metal_bands_2017$fans)

##are UK metal bands more popular than non-UK metal bands?

##let's look at how 'origin' is coded
table(metal_bands_2017$origin)

##recode using an if/else statement
fromUK<-ifelse(grepl("United Kingdom",metal_bands_2017$origin),1,0)

##now we can run the test
t.test(metal_bands_2017$fans[fromUK==1],metal_bands_2017$fans[fromUK==0])

##recode 'formed' by decade
formed_decade<-metal_bands_2017$formed

##what do we do about NAs?
min(formed_decade)
min(formed_decade,na.rm=T)
max(formed_decade,na.rm=T)

##recode the data
formed_decade[1960<=formed_decade&formed_decade<1970]<-1960
formed_decade[1970<=formed_decade&formed_decade<1980]<-1970
formed_decade[1980<=formed_decade&formed_decade<1990]<-1980
formed_decade[1990<=formed_decade&formed_decade<2000]<-1990
formed_decade[2000<=formed_decade&formed_decade<2010]<-2000
formed_decade[2010<=formed_decade&formed_decade<2020]<-2010

##add it to the data frame
metal_bands_2017<-cbind(metal_bands_2017,formed_decade)


##recode fans
fans_median<-metal_bands_2017$fans
fans_median[fans_median<median(metal_bands_2017$fans,na.rm=T)]<-0
fans_median[fans_median>=median(metal_bands_2017$fan,na.rm=T)]<-1

##add it to the data frame
metal_bands_2017<-cbind(metal_bands_2017,fans_median)
