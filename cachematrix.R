## Week 3
## GITHub Assignment
## Author:  Bryan Higgins


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<<-NULL
        #print(x)
        if (nrow(x)!=ncol(x))
        {
                message("Entered Matrix is not square")
                stop()
        }
        #message("Passed if")
        set<-function(y)
        {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setInvMat<-function(solve) m<<-solve
        getInvMat<-function() m
        list(set=set, get=get, setInvMat=setInvMat,
             getInvMat=getInvMat)
}

c
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
        ##      Returns the inverse
        m
        
}
