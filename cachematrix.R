## mjd 2020/04/02 
## Coursera Data Science Spec - Course 02 - R Programming
## Week 3 - Programming Assignment 2
## 
## makeCacheMatrix creates matrix object and solves and caches inverse
## cacheSolve retrieves cached inverse or calculates it
## 

## makeCacheMatrix creates matrix object and solves and caches inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## defines function that sets value of special Matrix
    ## uses <<- since x,m are outside scope of this function
    set <- function(y) {    
        x <<- y
        m <<- NULL
    }
    
    ## retrieves the value of the special matrix by returning itself
    get <- function() x
    
    setinver <- function(inver) inv <<- inver
    
    getinver <- function() inv
    
    list( set = set, 
          get = get, 
          setinver = setinver,
          getinver = getinver)
}


## cacheSolve returns the inverse of the matrix object
## first it checks the matrix object if there is a cached inverse
## otherwise it calculates and stores the inverse using setinver

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## get what's stored in inverse cache of "matrix" x
    inv <- x$getinver()
    
    ## check if a cached value exists, if so return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    ## only executed if cached inverse not returned
    ## get the matrix itself, find inverse, set it and reutrn it
    matr <- x$get()
    inv <-solve(matr)
    x$setinver(inv)
    inv
}
