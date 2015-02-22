## Functions may be used for inverse matrix calculation in fast way, 
## as if an inverse matrix was once calculated it takes previosly calucates value form a cache

##  makes a special object - creaters a definitions for getting a matrix, setting a matrix, 
## store inverse matrix and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL  # when new object is created inv variable is set to NULL
    set <- function(y) {  
        x <<- y   #changes a matrix with new values
        inv <<- NULL  # when an object is changed inv variable is set to NULL
    }   
    get <- function() x  #gives initial matrix
    setsolve <- function(solve) inv <<- solve  #assigning inverse matrix to variable inv
    getsolve <- function() inv   # gives what is stored in inv variable, may be NULL or inverse matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)  # creates a list of functions, that will be used in cacheSolve function
}


## gives an inverse matrix, either by calculating it or by getting from cache

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()    #takes a value inv, from makeCacheMatrix function, may be NULL or inverse matrix
    if(!is.null(inv)) {    # if inv is not NULL gives inv and prins proper information
        message("getting cached data")
        return(inv)
    }
    data <- x$get()    # if inv is NULL writes matrix to variable data
    inv <- solve(data, ...) # calculates inverse matrix and assigns it to inv
    x$setsolve(inv)    # assignes inverse data to inv variable in makeCacheMatrix function
    inv  # returns inv variable
}
