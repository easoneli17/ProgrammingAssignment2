## These functions cache the inverse of a matrix
## once it has been computed to save computation
## time.

## makeCacheMatrix takes a matrix and creates
## a special "matrix", which is really a 
## list containing a function to set and
## get the value of the matrix and set and
## get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set<-function(y){
          x<<-y
          s<<-NULL
    }
    get<-function() x
    setsolve<-function(solve) s<<-solve
    getsolve<-function() s
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## cacheSolve takes as an argument the 
## output of the above function, and 
## first checks to see if the inverse
## has already been calculated and cached
## so it can skip computation. If not, it
## calculates the inverse of the above
## "matrix" and stores it so that if it
## is called again, it can refer to the 
## cached inverse.

cacheSolve <- function(x, ...) {
    s<-x$getsolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
