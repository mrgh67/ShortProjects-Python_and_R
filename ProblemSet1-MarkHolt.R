#Problem Set 1

#1 a
TSS = read.csv("TimesSquareSignage.csv", header= TRUE)
dim(TSS)
#[1] 184  18

#1 b
str(TSS)
#'data.frame':	184 obs. of  18 variables:
# $ Screen.Name..LED...Vinyl.Signs.: Factor w/ 175 levels "1 Times Sq south of Walgreens (Too Big to Fail)",..: 126 149 23 84 156 138 162 161 75 157 ...
# $ Building.Address               : Factor w/ 55 levels "1 Times Sq","1460 Broadway",..: 35 35 36 37 39 42 1 1 1 1 ...
#$ Location.Description           : Factor w/ 117 levels "","2 panels",..: 1 65 70 16 12 15 27 102 56 29 ...
# $ Location                       : Factor w/ 4 levels "42nd/Below","Bowtie",..: 1 1 1 1 1 1 2 2 2 2 ...
# $ Height                         : Factor w/ 4 levels "","Above 65 Ft",..: 3 3 4 4 4 2 2 2 2 4 ...
# $ Type                           : Factor w/ 2 levels "LED","Vinyl": 1 1 1 1 1 1 1 1 1 1 ...
# $ X.                             : int  1 3 1 1 1 1 1 1 1 1 ...
# $ Width                          : int  30 NA 11 24 4 18 51 40 35 35 ...
# $ X__Height                      : int  10 NA 11 18 10 13 53 53 50 40 ...
# $ SF                             : int  300 44 121 432 40 234 2703 2120 1750 1400 ...
# $ Note.Photo                     : Factor w/ 103 levels "","IMG_8862",..: 91 1 96 92 86 84 1 1 65 1 ...
# $ X_.                            : int  NA NA NA NA NA NA NA NA NA NA ...
# $ X_Width                        : int  NA NA NA NA NA NA NA NA NA NA ...
# $ X_Height                       : int  NA NA NA NA NA NA NA NA NA NA ...
# $ X_SF                           : int  0 0 0 NA 0 0 0 0 0 0 ...
# $ TOTAL                          : int  1 3 1 1 1 1 1 1 1 1 ...
# $ TOTAL.SF                       : int  300 44 121 432 40 234 2703 2120 1750 1400 ...
# $ TOTAL.BY.TYPE                  : Factor w/ 3 levels "","112,376","274,896": 1 1 1 1 1 1 1 1 1 1 ...

$1. c
#NA is visible in the output from str(TSS)
#Also
is.na(TSS$X_Height)
#[1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
# [24]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [47]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [70]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
# [93]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#[116]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#[139]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#[162]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE

# Also the following, possibly, very useful functions
#TSS_NA = TSS[!complete.cases(TSS),]
#str(TSS_NA)
#or
#TSS_Complete = TSS[complete.cases(TSS),]
#str(TSS_Complete)

#1. d. 
# I might want to convert the following colums:
#Screen.Name..LED...Vinyl.Signs.
#Location.Description
#Location
#Height
#Note.Photo
#TOTAL.BY.TYPE

#2 I chose to convert the "Note.Photo" column from type factor to type character
TSS$Note.Photo = as.character(TSS$Note.Photo)
#TSS$Note.Photo
#I note when I run TSS$Note.Photo that there are many values of "". These are missing values too??? I find the whole NA automatic substitution to indicate missing values confusing.
str(TSS)
#data.frame':	184 obs. of  18 variables:
# $ Screen.Name..LED...Vinyl.Signs.: Factor w/ 175 levels "1 Times Sq south of Walgreens (Too Big to Fail)",..: 126 149 23 84 156 138 162 161 75 157 ...
# $ Building.Address               : Factor w/ 55 levels "1 Times Sq","1460 Broadway",..: 35 35 36 37 39 42 1 1 1 1 ...
# $ Location.Description           : Factor w/ 117 levels "","2 panels",..: 1 65 70 16 12 15 27 102 56 29 ...
# $ Location                       : Factor w/ 4 levels "42nd/Below","Bowtie",..: 1 1 1 1 1 1 2 2 2 2 ...
# $ Height                         : Factor w/ 4 levels "","Above 65 Ft",..: 3 3 4 4 4 2 2 2 2 4 ...
# $ Type                           : Factor w/ 2 levels "LED","Vinyl": 1 1 1 1 1 1 1 1 1 1 ...
# $ X.                             : int  1 3 1 1 1 1 1 1 1 1 ...
# $ Width                          : int  30 NA 11 24 4 18 51 40 35 35 ...
# $ X__Height                      : int  10 NA 11 18 10 13 53 53 50 40 ...
# $ SF                             : int  300 44 121 432 40 234 2703 2120 1750 1400 ...
# $ Note.Photo                     : chr  "IMG_8993" "" "IMG_8998" "IMG_8994" ...
# $ X_.                            : int  NA NA NA NA NA NA NA NA NA NA ...
# $ X_Width                        : int  NA NA NA NA NA NA NA NA NA NA ...
# $ X_Height                       : int  NA NA NA NA NA NA NA NA NA NA ...
# $ X_SF                           : int  0 0 0 NA 0 0 0 0 0 0 ...
# $ TOTAL                          : int  1 3 1 1 1 1 1 1 1 1 ...
# $ TOTAL.SF                       : int  300 44 121 432 40 234 2703 2120 1750 1400 ...
# $ TOTAL.BY.TYPE                  : Factor w/ 3 levels "","112,376","274,896": 1 1 1 1 1 1 1 1 1 1 ...


#3 a
TSS_Addresses = TSS[TSS$Location=="Upper Bway",]
#TSS_Addresses
write.csv(TSS_Addresses, "UpperBroadwaySignData.csv")

#3 b
TSS[TSS$SF > mean(TSS$SF),]
write.csv(TSS[TSS$SF > mean(TSS$SF),], "SignsGTAvSqFootage.csv")
#Rassure myself!
#mSF = mean(TSS$SF)
#mSF

#3 c
#I also included the SF column
#TSS[order(TSS$TOTAL.SF, decreasing=TRUE), c(17,1,2,4)][1:10,]
#Without TOTAL.SF column
TSS_TopTen = TSS[order(TSS$TOTAL.SF, decreasing=TRUE), c(1,2,4)][1:10,]
write.csv(TSS_TopTen, "SignsTopTenTotSqFootage.csv")


#4
HSD = read.csv("HarborSamplingData.csv", header=TRUE)
#str(HSD)
#names(HSD)
#head(HSD)
#dim(HSD)
HSD_Matrix=cbind(HSD$"Fecal.Coliform....100.mL..Top", HSD$"Fecal.Coliform....100.mL..Bot", HSD$"Enterococcus....100.mL..Top", HSD$"Enterococcus....100.mL..Bot")
colnames(HSD_Matrix) = c("Fecal.Coliform....100.mL..Top", "Fecal.Coliform....100.mL..Bot", "Enterococcus....100.mL..Top", "Enterococcus....100.mL..Bot")

#5 
#NB: for readability I deliberately separated out the calculation of the vector_Median
calc_MAD = function(x, i) {
	vector_Median = median(x, na.rm=TRUE)
	the_MAD = median(abs(x[i] - vector_Median), na.rm=TRUE)
	return(the_MAD)
}

#6 
#Firstly convert relevant data frame column to type numeric
HSD$"Fecal.Coliform....100.mL..Top"=as.numeric(HSD$"Fecal.Coliform....100.mL..Top")
#str(HSD)
HSD_FecalTopColiformMeans = with(HSD, aggregate(Fecal.Coliform....100.mL..Top, by = list(Site), mean, na.action = na.omit))
colnames(HSD_FecalTopColiformMeans) = c("Site", "Mean Fecal Coliform Conc. Top")

#7
theCol = TSS$Screen.Name..LED...Vinyl.Signs.
class(theCol)
ifelse( is.factor(theCol), ifelse( length(levels(theCol))/length(theCol) > 1/3, theCol<-as.character(theCol), print("did not convert ration < 1/3")), print("not a factor"))
class(theCol)

theCol = TSS$TOTAL.BY.TYPE
class(theCol)
ifelse( is.factor(theCol), ifelse( length(levels(theCol))/length(theCol) > 1/3, theCol<-as.character(theCol), print("did not convert ratio < 1/3")), print("not a factor"))
class(theCol)

theCol = TSS$Location.Description
class(theCol)
ifelse( is.factor(theCol), ifelse( length(levels(theCol))/length(theCol) > 1/3, theCol<-as.character(theCol), print("did not convert ratio < 1/3")), print("not a factor"))
class(theCol)


