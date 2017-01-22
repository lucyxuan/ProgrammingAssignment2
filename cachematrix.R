## Firstly makeCacheMatrix combines the original matrix and four functions together 
## So it is easier to retrieve the original matrix and the inversed matrix if there is one
## And cacheSolve can retrieve the inversed matrix or calculate the inversed matrix 

## Make a unique matrix that allows the user to get or set the original matrix, 
## and get or set the inveresd matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x<<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inv) i <<- inv
        geti <- function() i
        list(set = set, get = get, seti = seti, geti= geti)
}


## To retrieve the inverse of a matrix if there is one, and to calculate the
## inverse of a matrix if there is not.

cacheSolve <- function(x, ...) {
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$seti(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
