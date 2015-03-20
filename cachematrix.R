## Week 3
## GITHub Assignment
## Author:  Bryan Higgins


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #       Sets m to null
        m<<-NULL
        #print(x)
        #       Checks to see if the entered matrix is square
        if (nrow(x)!=ncol(x))
        {
                #       Alerts the user to a non-square matrix
                message("Entered Matrix is not square")
                #       Stops the program
                stop()
        }
        #message("Passed if")
        #       Sets pushes the matrix x to y in the function
        set<-function(y)
        {
                x<<-y
                m<<-NULL
        }
        #       Maintains the original matrix in memory
        get<-function() x
        #       Sets up the inverse matrix function
        setInvMat<-function(solve) m<<-solve
        #       Caches the inverse matrix
        getInvMat<-function() m
        list(set=set, get=get, setInvMat=setInvMat,
             getInvMat=getInvMat)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInvMat()
        # Checks to see if the inverse is already cached.
        if(!is.null(m))
        {
                message("getting cached data")
                return(m) #Returns the cached copy
        }
        data <- x$get()
        ##      Solves for the inverse matrix
        m<-solve(data,...)
        ##      Sets the cache
        x$setInvMat(m)
        ##      Returns the inverse matrix
        m
        
}
