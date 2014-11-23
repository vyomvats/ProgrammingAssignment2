## A code for a pair of functions that cache the inverse of a matrix


## Creates a special matrix object that is able to cache its inverse

makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set up the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get our matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of our matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of our matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by the function ## "makeCacheMatrix"
## If the inverse has already been calculated (and also, the matrix has not
## changed), then the finction "cachesolve" should retrieve the inverse from the ## cache.


cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}