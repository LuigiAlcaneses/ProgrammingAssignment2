#get value of the matrix and set the value of the inverse, and get the value of the inverse
#the above is done by the list function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
                }
                get <- function() {x}
                setInverse <- function(inverse) {inv <<- inverse}
                getInverse <- function() {inv}
                list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#compute the inverse of the matrix
#first check if the inverse has already been calculated with the isnull
#once retrieved, display message and then return the inverse
#to compute inverse of matrix, set value of inverse on the cache using setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
                mat <- x$get()
                inv <- solve(mat, ...)
                x$setInverse(inv)
                inv
}
