# Author Roberto
# Data science toolkit - coursera
# date: january 15th 2017
# This is programming assignment 2 of the R-course
# the functions are able to calculate the inverse of the matrix ánd return it from cache (to avoid heavy calculations)
# 

# The below function can do 4 things on the matrix
# 1: set the matrix
# 2: get the (original) matrix
# 3: function to get the inverse of the matrix (calculated the 1st time and set to cache with item 4)
# 4: set the inverse matrix (calculated in item 3) to the cache

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL #initialize the inverse as NULL
    
    #set the matrix and reset the cache to NULL
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    
    #get the original matrix
    get <- function()
    {
        x
    }
    
    #set the inverse matrix (calculation is not done here: that is done in the getmatrix (where the result is )
    # set via this method)
    setinverse <- function(inv) 
    {
        inverse <<- inv
    }
    
    # return the inverse matrix (if it already is calculated)
    getinverse <- function() 
    {
        inverse
    }
    
 #this list below makes sure that these 4 items get PUBLIC (instead of private) so they become accessible
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


# the below function is there to retrieve data from the cache 

cacheSolve <- function(x, ...) {
    ## getInverse() is called here, which was (a few lines above) made public so we can use it here to retrieve the inversed matrix
    inv <- x$getinverse()
    if(!is.null(inv)) #check if there is result from cache 
    {
    #if the result is there, message this, so we know it for debugging purposes
        message("data is returned from cache!")
        return(inv)#and return it
    }
    
#if data  is not there from cache, we need to calculate it via the solve method
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)#and here store it into cache
    inv     #return the inversed
}

#below a few lines to test the code


    myMatrix = makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
    
    myMatrix$get()         # get the original matrix
    myMatrix$getinverse()  # return inverse >> will here return null, because it is not yet calculated (using cachesolve)
    cacheSolve(myMatrix)   # calculate and cache the inverse of the matrix
    myMatrix$getinverse()  # return inverse >> will now return data from cache 
    
    #2nd matrix with different values
    myMatrix$set(matrix(10:13, nrow=2, ncol=2)) #create a 2nd matrix (overwrite)
    myMatrix$get()         # get the original matrix
    cacheSolve(myMatrix);#solve myMatrix from cache
    myMatrix$getinverse()  # return inverse   
    cacheSolve(myMatrix)   # calculate and cache the inverse of the matrix
    

    

