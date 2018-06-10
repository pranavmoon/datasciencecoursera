#########################################################
### R-programming-week-2-project assignment-1         ### 
### The file has three functions:                     ###
### 1. Pollutantmean()  : Find mean of data values    ###
### 2. complete() : finds data without missing values ###
### 3. corr() : finds correlation between observations### 
###           of two quantities from each data-tables.###
### After function definition I have defined variable ### 
### 'dirpath' which store path of 'specdata/' folder. ###
### Then i have written all the questions that was    ###
### asked in the assignment-project of week-2 of R-   ###
### programming course.                               ###
###                                                   ###
### To get the output in current working directory:   ###
### 1.Set variable 'dirpath' the path to the specdata/###
###   folder from your current working directory.     ###
### 2.On console execute  the command                 ###
###   < source("pollutantmean.R") >                   ###
#########################################################
###--------------pollutantmean()----------------------###
pollutantmean<-function(directory,pollutant,id=1:332){

	p<-rep(0,times=length(id)) ## initialize p of size id to store file location.
#	print("id: ");print(id)
	dat<-NULL   ##-initialize a variable
	data<-NULL  ##-initialize a variable

	
	for(i in 1:length(id)){
 		if(id[i]<10){
 		p[i]<-paste(directory,"00",id[i],".csv",sep="") 
		}
		else if(id[i]>=10&id[i]<100){
		p[i]<-paste(directory,"0",id[i],".csv",sep="")
		}
 		else{
		p[i]<-paste(directory,id[i],".csv",sep="")
		}##end of if-else
#	 print(p[i])
	 data<-read.csv(p[i])  ## read a data file 
	 z<-names(data)
	 j<-which(pollutant==z) ## gives position index of pollutant
	 dat<-append(dat,data[,j])  ## stores all the data of pollutant in single variable.
	 } ##end of for-loop
	
   print(z[j])
   print('False=values and TRUE=NA')
   print(table(is.na(dat)))
   print(round(mean(dat,na.rm=TRUE),3))
} ##end of function

###-------------------complete----------------------------###
complete<-function(directory,id=1:332){

	p<-rep(0,times=length(id))
#	print("id: ");print(id)
	nobs<-NULL
	data<-NULL

	for(i in 1:length(id)){
 		if(id[i]<10){
 		p[i]<-paste(directory,"00",id[i],".csv",sep="") 
		}
		else if(id[i]>=10&id[i]<100){
		p[i]<-paste(directory,"0",id[i],".csv",sep="")
		}
 		else{
		p[i]<-paste(directory,id[i],".csv",sep="")
		}
#	 print(p[i]);data<-NULL
	 data<-read.csv(p[i])
	 data<-length(which(complete.cases(data))) ## extract number of complete observed values from each data file.
	 nobs<-append(nobs,data)  ## stores number of complete observed values of all data file.  
	 }
# 	print('dataframe')
	 data.frame(id=id,nobs=nobs)  ## create a data table of file no. and observed values in the file.
#	 print(data.frame(id=id,nobs=nobs))
#	 obs<-data.frame(id=id,nobs=nobs)
#	 obs$nobs

} ## end-of-function
###----------------------corr---------------------------###
corr<-function(directory,threshold=0){
	
### threshold: to pick files (used to find correlation) with ###
### minimum number of complete obervations equal to threshold.###  
  
	id=1:332
	p<-rep(0,times=length(id))
#	print("id: ");print(id)
	co<-NULL
	data<-NULL

	for(i in 1:length(id)){
 		if(id[i]<10){
 		p[i]<-paste(directory,"00",id[i],".csv",sep="") 
		}
		else if(id[i]>=10&id[i]<100){
		p[i]<-paste(directory,"0",id[i],".csv",sep="")
		}
 		else{
		p[i]<-paste(directory,id[i],".csv",sep="")
		}
#	 print(p[i])
	 data<-read.csv(p[i])
	 value<-which(complete.cases(data))  ## gives index of complete observation in a file. 
	  	if(length(value)>=threshold){
		  data<-data[value,]
	 	  data<-cor(data$s,data$n) ##finds correlation b/w sulfate and nitrate.
	 	  co<-append(co,data) ## stores value of correlation of each data-file.
		  }
	 }
	 co
	
} ###end-of-function

####-----------specdata path-------------------###

dirpath<-"rprog_data_specdata/specdata/"

####---------------Questions-------------------###

print('---Q1: mean of sulfate for 1:10 csv files---')
pollutantmean(dirpath,"sulfate",1:10)
print('-------------------')
print('---Q2: mean of nitrate for 70:72 csv files---')
pollutantmean(dirpath,"nitrate",70:72)
print('-------------------')
print('---Q3: mean of sulfate for 34.csv file---')
pollutantmean(dirpath,"sulfate",34)
print('-------------------')
print('---Q4: mean of nitrate for all csv files---')
	pollutantmean(dirpath,"nitrate")

print('-------------------')
print('---Q5: what value printed at the end of the following code?---')
	cc <- complete(dirpath, c(6, 10, 20, 34, 100, 200, 310))
	print(cc$nobs)
print('-------------------')
print('---Q6: what value printed at the end of the following code?---')
	cc <- complete(dirpath, 54)
	print(cc$nobs)
print('-------------------')
print('---Q7: what value printed at the end of the following code?---')
	set.seed(42)
	cc <- complete(dirpath, 332:1)
	use <- sample(332, 10)
	print(cc[use, "nobs"])

print('-------------------')
print('---Q8: what value printed at the end of the following code?---')
	cr <- corr(dirpath)
	cr <- sort(cr)
	set.seed(868)
	out <- round(cr[sample(length(cr), 5)], 4)
	print(out)

print('--------------------------------------')
print('***Information related to Q 9 and 10***')
#print('Threshold is a numeric vector of length 1 indicating the number 
#	of completely observed observations (in a data-file on all variables
#	i.e. without any missing data in complete data table) required to 
#	compute the correlation between nitrate and sulfate; default is 0.')
	print('---Number of complete observations>=129---')
	print(nrow(cc[cc$n>=129,]))
	print('---Number of complete observations>=1000---')
	print(nrow(cc[cc$n>=1000,]))
	print('---Number of complete observations>=2000---')
	print(nrow(cc[cc$n>=2000,]))
print('--------------------------------------')
print('---Q9: what value printed at the end of the following code?---')
	cr <- corr(dirpath, 129)
	cr <- sort(cr)
	n <- length(cr)
	set.seed(197)
	out <- c(n, round(cr[sample(n, 5)], 4))
	print(out)

print('-------------------')
print('---Q10: what value printed at the end of the following code?---')
	cr <- corr(dirpath, 2000)
	n <- length(cr)
	cr <- corr(dirpath, 1000)
	cr <- sort(cr)
	print(c(n, round(cr, 4)))
