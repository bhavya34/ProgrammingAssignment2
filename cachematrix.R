## There are 2 pairs of function. And they together cache the
##inverse of a matrix (Square Invertible Matrix)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

i <- NULL

set <- function(matrix){
	m <<- matrix
	i <<- NULL
}

get<-function(){
	m
}

setInverse <- function(inverse){
	i <<- inverse
}

getInverse <- function(){
	i
}

list(set=set,get=get,set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ##returning the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ##Matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    x$setInverse(m)

    m
}
