## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: creates an object that can cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
		## inv: the cached inverse matrix
		inv <- NULL
		
		## 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
		## 2. get the value of the matrix
        get <- function() x
		
		## 3. set the value of the inverse matrix
		setinv <- function(solve) inv <<- solve
		
		## 4. get the value of the inverse matrix
		getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		
		## If the inverse has already been calculated, return the inverse from the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
		## Calculate inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
