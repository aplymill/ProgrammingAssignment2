# This pair of functions calculates and caches the inverse of a matrix

#makeChacheMatrix creates a matrix to which the inverse can be stored

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){ 
        x <<- y
        m <<- NULL #stores matrix in cache 
    }
    get <- function() x #get the matrix
    setinv <- function(solve) m <<- solve #stores inverse matrix in cache
    getinv <- function() m #get inverse matrix
    list (set=set, get=get, setinv=setinv, getinv=getinv) #create list of functions
}


#cacheSolve calculates inverse of matrix stored above or if inverse already calculated, retrives inverse from cache

cacheSolve <- function(x, ...) {
        m <-x$getinv() #obtaining x's cache
        if (!is.null(m)){   #if cache previosly calculated show message indicating so and returning cached calculation
        message("getting cached data")
        return m 
        }
        data <- x$get() #get matrix used by the makeChacheMatrix function
        m <- solve(data, ...) #calculate inverse of matrix
        x$setinv(m) #store inverse in cache using makeChacheMatrix setinv function
        }
}
