## Assignment: write a pair of functions that cache the inverse of a matrix. 

## Write a short comment describing this function
## makeCacheMatric: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get<- function() x
    setinv = function(inverse) inv<<-inverse
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## cacheSolve: computes inverse of special "matrix" returned by makeCacheMatrix. 
## if inverse already calculated and matrix not changed, 
## cache should simply retrieve.
cacheSolve <- function(x, ...) {
    inv<- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    data<-x$get()
    inv<-solve(data, ...) ## Return a matrix that is the inverse of 'x'
    x$setinv(inv)
    inv
} 