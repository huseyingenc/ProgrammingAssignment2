#These pair of functions cache inverse of a matrix and catche it.
#If the inverse of the matrix is cached before then 
#it just returns it without new calculation.

#First function
#set the value of the matrix, get the value of the matrix
#set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<- solve
	getmatrix<-function() m
	list(set=set, get=get,
		setmatrix=setmatrix,
		getmatrix=getmatrix)
	}

#Checks if inverse of the matrix is calculated already and 
#if it was calculated returns the cached value if not then
#it calculates the value and caches it in setmatrix.

cacheSolve <- function(x=matrix(), ...) {
	m<-x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix<-x$get()
	m<-solve(matrix, ...)
	x$setmatrix(m)
	m
}
