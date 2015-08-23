# Inversion of matrixes is a potentially time-consuming computations task.
# Therefore, rather than repeating the computation, saving/caching the inverse of a matrix can be done . 
# Functions makeCacheMatrix and cacheSolve below will perform this task.

# Function makeCacheMatrix will create a special "matrix", of type 
# list with a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

            m <- NULL
            set <- function(y) {
			
			#the `<<-` operator can be used to
            #assign a value to an object in an environment that is different from the
            #current environment.
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setMatrix <- function(solve) m <<- solve
            getMatrix <- function() m
            list(set = set, get = get,
                 setMatrix = setMatrix,
                 getMatrix = getMatrix)

}


# The cacheSolve function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
            m <- x$getMatrix()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setMatrix(m)
            m		
		
}
