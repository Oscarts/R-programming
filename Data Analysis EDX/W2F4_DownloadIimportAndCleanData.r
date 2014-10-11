#W2F4_DownloadImportAndCleanData

a <- read.table("clipboard", header=T, sep="\t")   
#This works if there is something in your windows or linux clipboard memory
#Copy data from the file List_P_3D_data.csv  You find it below the video

a <- read.table(pipe("pbpaste"),  header=T, sep="\t") #This works for mac users

head(a)
names(a)

measurements <- c("EDV","ESV","SV","EF")
paste(measurements, "3D", sep=".")
c(paste(measurements, "3D", sep="."), paste(measurements, "2D", sep="."))
names(a) <-  c("subject",paste(measurements, "3D", sep="."), paste(measurements, "2D", sep="."))

setwd("    here you must write your own relevant path    ")
dir()

b <- read.table(file="List_P_3D_data.csv", header=T, sep=";")
dim(b)
head(b)
tail(b)
b <- b[-31:-33,-10:-12]
tail(b)

c <- read.table(file="List_P_3D_data_missing-columns.txt", header=T, sep=";")
c <- read.table(file="List_P_3D_data_missing-columns.txt", header=T, sep=";",fill=T)

# read.csv()     read.fwf()  scan()   readLines()

?read.table()
installPackages("HistData")


#http://www.data.gov/
#http://www.healthdata.gov/dataset/search
#http://data.gov.uk/data/search
#http://data.gov.in/
#http://data.gov.in/community/developer-community


# data.gov.uk
# http://www.hscic.gov.uk/catalogue/PUB13648

	
test <- read.csv("http://www.hscic.gov.uk/catalogue/PUB14142/nhs-dent-stat-eng-2013-14-thir-quar-anx4u-UDA-CCG.csv")
str(test)
boxplot(test$Total ~ test$Patient_Type)
boxplot(test$Urgent_Occasional)
boxplot(test$Urgent_Occasional ~ test$Region_Code)
boxplot(test$Urgent_Occasional ~ test$Patient_Type)

#install.packages("RCurl")

#http://www.ncbi.nlm.nih.gov/gds   search with the word 'diabetes'

webq <- readLines("http://www.ncbi.nlm.nih.gov/gds/?term=diabetes")
head(webq)
grep("diabetes",webq)
webq[grep("diabetes",webq)]
date()


