#Answer to quiz-1 of week-1 of R-programming course
Qone<-function(){
	w1data<-read.csv("hw1_data.csv")
	print("0. data for quiz1")
	w1data
	print('1. first two lines of data file\n')
	print(head(w1data,2))
	print('2. last two lines of data file')
	print(tail(w1data,2))
	print('3. name of column of data')
	print(names(w1data))
	print('4. value ozone from 47th row')
	print(w1data$O[47])
	print('5. Misisng values of ozone')
	print(table(is.na(w1data$O)))
	print(length(w1data[is.na(w1data$O), "Ozone"]))
	print('6. Mean of Solar.R for ozone>31 and Temp>90')
	print(mean(w1data[(w1data$O>31&w1data$T>90),"Solar.R"],na.rm=TRUE))
	print('7. mean of Temp for month 6')
	print(mean(w1data[w1data$M==6,"Temp"]))
	print('8. Max of ozone for month 5')
	print(max(w1data[w1data$M==5&!is.na(w1data$O),"Ozone"]))
	print(max(w1data[w1data$M==5,"Ozone"],na.rm=T))
	print('9. mean of ozone')
	print(mean(w1data[!is.na(w1data$O),"Ozone"]))
	print(mean(w1data$O,na.rm=TRUE))
	print('10. number of rows in data')
	print(nrow(w1data))
	print(dim(w1data))

}

