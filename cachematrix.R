#Function makeCachematrix creates a special "matrix" to "cache" or store"
makeCachematrix <- function(x = matrix()) {
        #sets value of the matrix (default=null) and creates and function that
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        
        #set the value of the matrix
        setmat <- function(solve) m <<- solve
        
        #get the value of the matrix
        getmat <- function() m
        
        #list of values
        list(set = set, get = get, setmat = setmat, getmat = getmat)
}

#Function cacheSolve calculates the inverse of the a matrix, but checks to see
#if is stored in the makeCachematrix function.
#If the matrix is not stored it will calculate the inverse of that matrix
cacheSolve <- function(x, ...) {
        #calls the value of m from the previous function
        m <- x$getmat()
        #logical test to see if there is a cached value
        if(!is.null(m)) {
                #If TRUE the function calls the stored matrix
                message("getting cached data")
                return(m)
        }
        #If FALSE it calculates the inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        #Sets a new value in the cache
        x$setmat(m)
        ##returns inverse of matrix
        m
}