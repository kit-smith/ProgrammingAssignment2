## makeCacheMatrix initiates the objects x and i, and then defines the functions 
## set, get, set.inverse and get.inverse and puts them into a list.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y) {
                x <<- y
                i <<- NULL
         }
                
        get <- function() x
        set.inverse <- function(solve) i <<- solve
        get.inverse <- function() i
                
        list(set = set,
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}

## If these is a valid inverse cached in i, cacheSolve returns it to the patent
## environment. If not, then cacheSolve gets x and solves for its inverse.

cacheSolve <- function(x, ...) {
        i <- x$get.inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<- x$get()
        i<- solve(data,...)
        x$set.inverse(i)
        i
}
