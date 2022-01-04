## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv<- NULL                                  ## initializing inverse as null
        set<- function(y)                    
                {
                        x<-y
                        inv <- NULL
                
}
        get <- function() x                 ## function to get matrix
        setinverse <- function(inverse) inv <<- inverse       
        getinverse <- function()             
                {
                inver<-ginv(x)                ## function to obtain the inverse of the matrix
                inver%*%x
                }
        list(set=set,get=get,setinverse=setinverse,getinverse =getinverse)


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                           ## gets cache data
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv))                             ## checking if inverse is null or not
                {
                message("getting cached data")
                return(inv)
                }
data<- x$get()
        inv<- solve(data,...)                         ## calculating inverse value
        x$setinverse(inv)
        inv                                             ## finally returning the inverse of matrix
        
}
