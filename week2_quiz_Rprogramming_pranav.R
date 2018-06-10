print('## Q1: for answer run <cube(3)> in console')
cube <- function(x, n) {
	x^3
## answer: 27 returned
}
print('## Q2: for answer run <q2()> in console')
q2<-function(){
	x <- 1:10
	if(x > 5) {
	   x <- 0
	}	


##  warning:  In if (y > 5) { :
##  the condition has length > 1 and only the first element will be used

}

print('## Q3: for answer do following in colsole: 1. z<-10 and 2.q3(3)')
q3 <- function(x) {
		g <- function(y) {
		y + z
		}
	z <- 4
	x + g(x)

## answer: 10	
}

print('## Q4: for answer run <q4()> in console')
q4<-function(){
		x <- 5
		y <- if(x < 3){	NA}
		     else { 10 }
  	y
## answer4: 10
}

print('#Q5: which symbol in the below function is  a free variable?

h <- function(x, y = NULL, d = 3L) {
	z <- cbind(x, d)
	if(!is.null(y))
	z <- z + y
	else
	z <- z + f
	g <- x + y / z
	if(d == 3L)
	return(g)
	g <- g + 10
	g
## Answer5: f
}')

print('Q6: What is an environment in R? #Answer6: acollection of symbols/value pairs')

print('## Q7: The  R language uses what type of scoping rule for  resolving free variables? 
## Answer7: Lexical scoping')

print('## Q8: How are free variables in R functions resolved?
## Answer8:  The values of free variables are searched in the environment in which the function was defined.')

print('## Q9: what is one of the consequences of  the scoping rules used in R? ?
## Answer9:  All objects must be stored in memory.')

print('## Q10: In R, what is the parent frame?
## Answer10: It is a environment in which a function was called.')



