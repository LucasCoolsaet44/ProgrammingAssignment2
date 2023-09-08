#makeCacheMatrix is our function which requires an input x, a matrix.
#then, by using set and get functions, we can then retrieve our value of the
#inverse, xinv.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y){
                x <<- y
                xinv <<- NULL
        }
        get <- function() x 
        #getting x from cache
        setinv <- function(inverse) xinv <<- inverse 
        #set xinv to value stored in cache
        getinv <- function() xinv 
        #obtain inverse
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        #we'll want to make a list of our functions if we want to call them
}

#cachesolve is a function that will take our input matrix x, and the ellipses
#are there to take the other remaining arguments
cacheSolve <- function(x, ...) {
        xinv <- x$getinv()
        #xinv is assigned previously stored inverse in the cache
        if(!is.null(xinv)){
                message('getting cached data')
                return(xinv)
        #this if statement is here to retrieve xinv if it is not NULL
        }
        xdata <- x$get()
        #retrieves previous matrix x
        xinv <- solve(xdata, ...)
        #we use solve here to calculate our inverse
        x$setinv(xinv)
        #we'll want to set our newly calculated inverse to xinv
        return(xinv)
}
