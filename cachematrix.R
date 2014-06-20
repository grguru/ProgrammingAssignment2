## Below functions calculates inverse of a matrix. 
## It stores inverse in Cache
##It returns Inverse from cache if it presents 
## otherwise calculates

## This function returns a vector of functions  

makeCacheMatrix <- function(x = matrix()) {
    ## set the inverse matrix to blank 
    invm <- matrix()
    
    ## create a setter that caches x and inverse x
    set <- function(y){
        x <<- y
        invm <<- matrix()
    }
    
    ## create getter that returns the caches x
    get <- function() x     
    
    ## create a setInverse function 
    setInverse <- function(m) {
        invm <<- m
    }
    
    ## gets the inverse from cache or calculates
    getInverse <- function() invm
    
    ## return the 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Checks Cache for result. If found return from cache 
## Otherwise calculated Inverse. And then sets the result to cache 

cacheSolve <- function(x, ...) {
    
    invm <- x$getInverse()
    
    #Check if Cache matrix and argument matrix are same and Inverse in not null
    if(identical(c,x) && !is.na(invm)[1,1]) {
        message("getting cached data")
        return (invm)  
    }
    
    data <- x$get() 
    invm <- solve(data, ...) ##calculate inverse
    x$setInverse(invm) ## Set the inverse into Cache
    invm 
}
