remove_outliers = function(x){
    		threshold = quantile(x, 0.75, na.rm=TRUE) + 1.5*IQR(x, na.rm=TRUE)
    		filtered_x = ifelse(x > threshold, NA, x)
    		return(filtered_x)
}

remove_notnumbersandinfs = function(x) {
	#if(!is.finite(x)) {
	#	cat("there is stuff to un inf and un Nan here\n")
	#}
	filtered_x = ifelse(is.finite(x),x,NA)
	return(filtered_x)
}

doExit = function(exitString) {
	cat("\n\n", exitString, "\n")
	cat("Usage: PP(dF = data frame, catVList = list of categorical variables of interest, numerical variable of interest, fnList = list of numerical functions, ... = any arguments required for numerical functions)\n")
	cat("Example: Results = PP(dataFrame, catVariableList, numericalVariable, functionList, commonargumentsforallfunctions)\n")
	cat("PP returns a list. The first element is the cleaned dataframe and the second element is the summary statistics required\n\n")
	newDF = data.frame()
	return(newDF)
}

doFuns <- function(x, fnList, ...) {
	fvector=numeric()
	for(i in 1:length(fnList)) {
		fvector=c(fvector, fnList[[i]](x, ...))
	}
	return(fvector)
}

PP = function(dF, catVList, numVar, fnList, ...) {
	#This takes a dataframe and a list of categorical variables, a numerical variable, a list of functions, and an argument list
	#Do some sanity checking on the user provided input before we do anything else
	#debug=1
	debug=0
	
	#Make sure dF really is a data frame
	if(!is.data.frame(dF)) {
		return(doExit("dF is not a data frame\n"))
	}
	if(debug==1)
		cat("passed check for dataframe\n")
	
	#Make sure the categorical variable list is a list
	if(!is.list(catVList)) {
		return(doExit("catVList is not a list\n"))
	}
	if(debug==1)
		cat("The argument for the categorical variable is a list\n")
	
	#Make sure the list of user specified functions is a list
	if(!is.list(fnList)) {
		return(doExit("fnList is not a list\n"))
	}
	if(debug==1)
		cat("The argument for the functions is a list\n")
	
	#Now turn to the list of user specified functions. Check the list actually contains functions
	for(fname in fnList) {
		if(!is.function(fname)) {
			return(doExit("The list of functions you have provided contains a non-function\n"))
		}
	}
	if(debug==1)
		cat("The function list does contain functions\n")
		
	#Now parse the categorical variable list and ensure that they exist as column names in the data frame
	#Create a new list containing the respective categorical variables
	#colNList is a list of the column numbers used for degugging
	colNList=list()
	theCatList=list()
	for(name in catVList) {
		if(name %in% colnames(dF)) {
			colNList=append(colNList,match(name, colnames(dF)))
			theCatList = append(theCatList, dF[match(name, colnames(dF))])
		} else
		return(doExit("One of the categorical variables supplied does not exist within the data frame"))
	}
	if(debug==1)
		cat("The categorical variables named in the categorical variable list are legitimate\n")
	
	#For the legitimate column names provided now actually check that the variable is categorical
	for(ccol in colNList) {
			#cat("ccol is ", ccol, "\n")
			if(!is.factor(dF[,ccol])) {
			return(doExit("The list of categorical variables provided contains a non-categorical variable"))
		}
	}
	if(debug==1)
		cat("The categorical variables are legitimate\n")
		
	#Now check that the Numerical Variable for which summary data is being prepared exists, and get it's index
	if(numVar %in% colnames(dF)) {
		nVIndex = match(numVar, colnames(dF))
	} else
		return(doExit("The numerical variable was not found in the data frame"))
	if(debug==1)
		cat("The numerical variable is legitimate\n")
	
	#--------------------------
	#This was an attempt to match function names and arguments from ... list
	
	
#	Now parse the ellipses argument to obtain a list of functions and a list of lists of their respective arguments
#	args=list(...)
#	found = 0
#	if(length(args) > 0) {
#		argsToFnList=list()
#		fnList = list();
#		count = -1;
#		argumentsList=list()
#		for (a in args) {
#			if(is.function(a)) {
#				found = 1
#				fnList = append(fnList,a)
#				cat("found a function\n")
#				if(count == 1) {
#					if(length(argumentsList) == 0) {
#						argumentsList=list("N")
#					}
#					argsToFnList = append(argsToFnList, list(argumentsList))
#					argumentsList=list();
#				}
#				count = 1
#			} else {
#				cat("found some arguments  ", a, ' count is ', count, "\n")
#				argumentsList = append(argumentsList, a)
#			}
#		}
#		if(found == 1) {
#			if(length(argumentsList) == 0) {
#				argumentsList=list("N")
#			}
#			cat("here\n")
#			argsToFnList = append(argsToFnList, list(argumentsList))
#			cat("here1\n")
#		} 
#	} else {
#		return(doExit("No functions were passed as argumentsB"))
#	}
	
	
	#Convert the Numerical Variable to numeric. No need to check, just do a conversion that may be redundant if column is already numeric
	#NB: For the aggregate function I am using a copy of the numerical variable which has therefore not had outliers or NaNs, Infs removed. Easy to change this.
	dFTarget = as.numeric(dF[,nVIndex])
	
	#Let's do some basic clean up of the dataframe as a whole
	
	for(i in 1:length(colnames(dF))) {
		
		#For character columns replace the NAs - beware the control characters
		if(is.character(dF[,i]))	{
			dF[,i]=gsub("^$",NA,dF[,i])
		}

		#For numeric columns identify and replace any outliers - use our "remove_outliers() function
		if(is.numeric(dF[,i])) {
			dF[,i]=remove_outliers(dF[,i])	
		}
		
		#Also for numeric columns remove and replace NaNs and Infs - use a function along similar lines to "remove_outliers" 
		if(is.numeric(dF[,i])) {	
			dF[,i]=remove_notnumbersandinfs(dF[,i])
		}
		
	}
	
	
	#Finally do the aggregate. Utilize a separate functions "doFuns". This takes the list of user supplied functions (stored in fnList) and 
	#returns a vector of function calls suitable for use with aggregate.
		
	summdF=aggregate(dFTarget,theCatList, function(x) doFuns(x,fnList,...))
	
	#Now name the summary columns. Ideally wanted to extract the names of the functions supplied, but couldn't find a way of doing it. Call them "Function 1, Function 2, ..."
	for(j in 1:length(fnList)) {
		nm=paste("Function", j, sep=" ")
		colnames(summdF)[length(catVList)+1]=as.character(nm)
	}
		
	#Return the complete data frame that has been cleaned, as well as the summary
	return(list(dF, summdF))
}


#TESTING
F1=read.csv("BrooklynInspectionResults.csv", header=TRUE)
F2=read.csv("HarborSamplingData.csv", header=TRUE)
F3=read.csv("TimesSquareSignage.csv", header=TRUE)
F4=read.csv("campaign_contributions.csv", header=TRUE)
F5=read.csv("nyc_land_use.csv", header=TRUE)
F6=read.csv("nycNO2.csv", header=TRUE)

str(F1)
catVList=list("INSPECTION.DATE")
fnList=list(mean)

Res=PP(F1, catVList, "ZIPCODE", fnList, na.rm=TRUE)
Res[[2]][1:10,]

#   INSPECTION.DATE Function 1
#1       2014/01/02   11217.88
#2       2014/01/03   11235.00
#3       2014/01/04   11208.10
#4       2014/01/06   11221.99
#5       2014/01/07   11221.31
#6       2014/01/08   11218.22
#7       2014/01/09   11217.13
#8       2014/01/10   11223.24
#9       2014/01/11   11229.60
#10      2014/01/13   11215.73 
str(F2)
catVList=list("Site")
fnList=list(mean, median)
Res=PP(F2, catVList, "Enterococcus....100.mL..Top", fnList, na.rm=TRUE)
Res[[2]][1:10,]

#   Site Function 2.1 Function 2.2
#1   AC1    124.34615    127.50000
#2   BB2    134.48276    130.00000
#3   BB4    115.86207     99.00000
#4   BR1    115.61538    110.00000
#5   BR3    118.20000    110.00000
#6   BR5    140.64516    152.00000
#7  CIC2    125.14286    131.50000
#8  CIC3    110.82759    117.00000
#9   E10     82.85185     76.00000
#10  E11     81.82759     76.00000

str(F3)
catVList=list("Type", "Height")
fnList=list(min, max, mean, median, sd, mad)
Res=PP(F3, catVList, "TOTAL", fnList, na.rm=TRUE)
Res[[2]][1:8,]

#   Type      Height Function 6.1 Function 6.2 Function 6.3 Function 6.4 Function 6.5 Function 6.6
#1   LED                1.0000000    1.0000000    1.0000000    1.0000000           NA    0.0000000
#2 Vinyl                1.0000000   15.0000000    1.8636364    1.0000000    2.9808189    0.0000000
#3   LED Above 65 Ft    1.0000000    3.0000000    1.2307692    1.0000000    0.5991447    0.0000000
#4 Vinyl Above 65 Ft    1.0000000    3.0000000    1.1739130    1.0000000    0.4373835    0.0000000
#5   LED     Marquee    1.0000000    3.0000000    1.4000000    1.0000000    0.6992059    0.0000000
#6 Vinyl     Marquee    1.0000000    4.0000000    1.6000000    1.0000000    1.3416408    0.0000000
#7   LED Under 65 Ft    1.0000000    5.0000000    1.1904762    1.0000000    0.8728716    0.0000000
#8 Vinyl Under 65 Ft    1.0000000    3.0000000    1.2272727    1.0000000    0.5202205    0.0000000


str(F4)
catVList=list("CANDLAST", "COMMITTEE", "C_CODE")
fnList=list(min, max, mean, median, sd, mad)
Res=PP(F4, catVList, "AMNT", fnList, na.rm=TRUE)
Res[[2]][1:10,]

#         CANDLAST COMMITTEE C_CODE Function 6.1 Function 6.2 Function 6.3 Function 6.4 Function 6.5 Function 6.6
#1        CANDLAST COMMITTEE C_CODE          Inf         -Inf          NaN           NA           NA           NA
#2          Dobrin         H    CAN    100.00000    250.00000    175.00000    175.00000    106.06602    111.19500
#3        Krongold         H    CAN  -7700.00000   7700.00000      0.00000      0.00000   6287.55384   5782.14000
#5       Rosenthal         H    CAN   8250.00000   8250.00000   8250.00000   8250.00000           NA      0.00000
#6  Simmons-Oliver         H    CAN    100.00000    100.00000    100.00000    100.00000           NA      0.00000
#7          Taylor         H    CAN   2750.00000   2750.00000   2750.00000   2750.00000           NA      0.00000
#8        Waterman         H    CAN   2500.00000   2500.00000   2500.00000   2500.00000           NA      0.00000
#9         Gotlieb         I    CAN     50.00000   1200.00000    486.22222    300.00000    434.46340    296.52000
#10       Hoffnung         I    CAN   2750.00000   2750.00000   2750.00000   2750.00000           NA      0.00000

str(F5)
catVList=list("Application.Type")
fnList=list(mad)
Res=PP(F5, catVList, "Amount", fnList, constant=1, na.rm=TRUE, high=FALSE)
Res[[2]][1:10,]

#                              Application.Type Function 1
#1  All other sections of the Zoning Resolution      755.0
#2               Amendment to Zoning Resolution        0.0
#3                         CEQR Application Fee    10642.5
#4               Changes to City Map & Landfill     1660.0
#5            Franchises and revocable consents     1672.5
#6              Natural Feature Restoration Fee        0.0
#7                               Special Permit     2080.0
#8          Supplemental Fee for Large Projects    40000.0
#9                         Zoning Authorization       75.0
#10        Zoning Authorization for Open Spaces      755.0

str(F6)
catVList=list("geo_type_id")
fnList=list(mad)
Res=PP(F6, catVList, "data_value", fnList, constant=1, na.rm=TRUE, high=FALSE)

#The list of categorical variables provided contains a non-categorical variable 
#Usage: PP(dF = data frame, catVList = list of categorical variables of interest, numerical variable of interest, fnList = list of numerical functions, ... = any arguments required for# numerical functions)
#Example: Results = PP(dataFrame, catVariableList, numericalVariable, functionList, commonargumentsforallfunctions)
#PP returns a list. The first element is the cleaned dataframe and the second element is the summary statistics required

