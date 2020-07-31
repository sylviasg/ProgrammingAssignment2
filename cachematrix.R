## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        if (nrow (x)!= ncol(x))
        {
                print ("not a square matrix")
                x = matrix() 
        }
        setmatrix <- function (mtrx)
        {
                if (nrow (mtrx)== ncol(mtrx))
                {
                        x <<- mtrx
                        inv <<- NULL
                }
                else
                {
                        print ("not a square matrix")
                        x = matrix()
                }
                
        }
        getmatrix <- function()
        {
                x
        }
        setinverse <- function(inverse)
        {
                inv <<- inverse
        }
        getinverse <- function()
        {
                inv
        }
        list (set=setmatrix, get = getmatrix, setinv = setinverse, getinv = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheinv <- x$getinv()
        if (!is.null(cacheinv))
        {
                message ("using cached value of inverse")
                
        }
        else
        {
                mymatrix <-x$get()
                cacheinv <- solve(mymatrix)
                x$setinv(cacheinv)
                
        }
        cacheinv
}
S