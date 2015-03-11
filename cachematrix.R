## Caching the Inverse of a Matrix..

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL;
        
        #checks if matrix x is empty matrix or not. If empty matrix it returns "True" else "False"
        nullmatrixtest <- function(){
                log <- T;
                for (i in seq_len(nrow(x))){
                        for (j in seq_len(ncol(x))){
                                if (is.na(x[i,j]) == F){
                                        log <- F;
                                }
                        }
                }
                return(log)
        }
        
        #On set function, for multiple sets it compares current matrix with previous value and if it both are not equal it sets the value.
        set <- function(y) {
                if (nullmatrixtest()){
                        #print("Setting the value");
                        x <<- y;
                        inverse <<- NULL
                } else if ((length(x)!=length(y)) || (!all(x == y))) 
                {
                        # print("Updating the value");
                        x <<- y;
                        inverse <<- NULL
                }         
        }
        
        #Retreives the x matrix
        get <- function() {
                return(x);
        }
        
        #set the inverse of the matrix returned by cacheSolve to inverse.
        setinverse <- function(inv){
                inverse <<- inv;
        }
        
        #get the inverse value
        getinverse  <- function(){
                return(inverse);
        }
        
        #list to pick the four functions.
        list (set = set,get=get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
