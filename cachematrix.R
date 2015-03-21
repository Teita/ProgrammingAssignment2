## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



## The makeCacheMatrix() function creates a special matrix that can
## cache its inverse. The cacheSolve() function will complement it
## by computing the inverse or retrieving it from the cache, if it was already 
## computed before

makeCacheMatrix <- function(x = matrix()) {
        
        ## I create the placeholder,im, for inverse matrix, and set
        ## it to NULL
        
        im <- NULL
        
        ## I define the set() function to assign the value y to the matrix
        ## I store the matrix y in x and x will be in a different environment
        ## I reset the inverse matrix to NULL
        
        ## By using the double assignment operator, we can expect a search
        ## for x and im in the environment immediately outside of the one in  
        ## in which the set function was defined,i,e, a parent environment.
        
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        ## I define the get() function that will return the matrix x
        
        get <- function() x
        
        ## I define the setinverse() function that will set the inverse matrix 
        
        setinverse <- function(inversematrix) im <<- inversematrix
        
        ## I define the getinverse() function that will return the inverse matrix
        getinverse <- function() im
        
        ## I use the list() function to store all the functions defined in
        ## this environment that is "set", "get", setinverse", and "getinverse"
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        ## The objective of this function is to retrieve the inverse 
        ## from the cache computed by the previous call to the function
        ## with the same argument
        
        ## I begin by calling the inverse and I store the result in im
        im <- x$getinverse()
        
        ##  If the matrix was previously solved, its inverse would be in the
        ## cache, so I retrieve it from there
    
        if(!is.null(im)) {
                message("getting cached inverse")
                return(im)
        }
        
       ## otherwise, I retrieve the stored matrix to compute the inverse
        
        matrix <- x$get()
        im <- solve(matrix, ...)
       
       ## I put the inverse in the cache so it doesn't have to be computed 
       ## again for the next call of the function with the same argument
        x$setinverse(im)
        
        ## Return the inverse
        im
        
}
