# Matrix inversion is usually a costly computation so you may benefit from caching the calculation
# rather than computing it more than once.

# The first function (makeCacheMatrix) takes a matrix as an argument and returns a cacheable matrix 
# "object" (for want of a better term ... that makes sense to me). It has getters and setters for
# both the matrix and the inverse value. Note that it does NOT do the actual matrix inversion, only
# stores the result.

# The second function (cacheSolve) takes a cacheable matrix, created using the makeCacheMatrix function,
# and returns the inverted matrix. Note that it will only calculate the inversion once, using the 
# solve() function. It will cache that value and subsequent calls to cacheSolve will return the cached 
# value. (You can verify that the cached value is used as the message "getting cached data" will be 
# displayed.)

# For this assignment, assume that the matrix supplied is always invertible.

##########
#
# This function takes a matrix a returns an object that holds the matrix and its inverted value.
#
# The object returned has the following functions:
#
# $get(): returns the matrix
# $set(matrix): takes a matrix as an argument and stores it in the object
# $getinverse: returns the inverse of the matrix
# $setinverse(inverse): sets the inverse of the matrix and stores it in the object
#
##########
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    # cache the inverse matrix in i
    
    get <- function () x    # get the matrix data
    
    set <- function (y) {   # set the matrix data
        x <<- y
        i <<- NULL    # Don't forget to clear i when x is reset
    }
    
    getinverse <- function () i    # get the inverse data
    
    setinverse <- function (inv) i <<- inv    # set the inverse data
    
    return (list (get = get, set = set, getinverse = getinverse, setinverse = setinverse))
}


##########
#
# This function returns a matrix that is the inverse of the "cacheable matrix" passed as
# an argument. Note that the "cacheable matrix" object is created with the function
# makeCacheMatrix.
#
##########
cacheSolve <- function(x, ...) {
    i <- x$getinverse ()    # Get the inverse
    if (!is.null (i)) {     # Is it not null (meaning the value is cached)?
        message ("getting cached data")    # Let them know it is a cached value
        return (i)    # Returned the cached value
    }
    # If we got here then the matrix inversion has not been computed yet
    data <- x$get ()          # Get the matrix
    i <- solve (data, ...)    # Invert the matrix
    x$setinverse (i)          # Save the inverse
    return (i)                # And finally return the inverse
}
