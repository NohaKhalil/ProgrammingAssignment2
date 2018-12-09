#This code is used to facilitate faster calculation of the inverse of a matrix by
#storing (caching) it to be easily retrieved when the same matrix is used instead of re-calculating.


#makeCacheMatrix Function creates a set of functions that are used to cache the matrix and its
#inverse of a matrix and retrieve it when needed again.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y)
    {
        x <<- y 
        inv <<- NULL
    }
    get <- function() x
    
    setinverse <- function (inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list (set = set,get = get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)){
        message("Retrieving Cached Inverse of Matrix")
        return(i)
    }
    
    data_matrix = x$get()
    i=solve(data_matrix)
    x$setinverse(i)
    i
    
}


#Testing the functions
##Define a simple testing matrix
test_matrix= matrix(nrow = 2, ncol =2, c(1,2,3,4))

##Cahcing the matrix and its inverse
test = makeCacheMatrix(test_matrix)

##First Call to cache inverse
cacheSolve(test)

##Second Call to test
cacheSolve(test)