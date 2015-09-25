## The following pair of functions caches the inverse of a matrix


## The following function creates an matrix object thqt can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y) {
                x<<- y
                inv<<- NULL
        }
        get<-function() x
        setinv<-function(inverse) inv<<-inverse
        getinv<-function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function computes the inverse of the matrix returned 
## by makeCacheMatrix. If the inverse has been calculated, then 
## cashSolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv) #cache computed inverse 
        return(inv)
}
