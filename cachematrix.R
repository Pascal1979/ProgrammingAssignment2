## These functions cache potentially time-consuming computations. In particular
## both functions allow to cache the inverse of a matrix rather then compute it
## every time you need it.

## the first function creates a vector which is a list of four functions:
## set, get, setinv, getinv. 
makeCacheMatrix <- function(x = matrix())  {
        inv <- NULL
        set <- function(y)  {  ## eet change the matrix stored in the main 
                x <<- y  ## function when the matrix value is changed. x <<- y
                inv <<- NULL  ## substitutes x with the imput of main function
        }  ## inv<<-NULL restore null value for inv so new computation is need
        get <- function()  {  ## get return the value of x in the main function
                x
        }
        setinv <- function(solve)  {  ##  setinv stores the value of the imput
                inv <<- solve  ## in the variable inv in the main function.
        }    
        getinv <- function()  {  ##getinv simply return the variable inv
                inv  ## setinv and getinv functions DO NOT calculate the
        }  ## inverse: computation is performed in the function 'cacheSolve'.
        list(set = set, get = get,  ## a list storing the four functions is
             setinv = setinv,  ##returned.
             getinv = getinv)
}
##  this second function compute the inverse of the matrix in the object where
##  makeCacheMatrix is stored.
cacheSolve <- function(x = matrix(), ...)  {
        inv <- x$getinv()
        if (!is.null(inv)) {  ## the function verify if the value of inv is not
                message("getting cached data")  ## null. If it exists simply 
                return(inv)  ##return a message that the value was beforehand
        }  ##stored and return it.
        data <- x$get()  ## else data gets the matrix stored with makeCacheMatrix
        inv <- solve(data, ...)  ## the calculation of the inverse is assigned 
        x$setinv(inv) ## to inv and stored in the object previously generated.
        inv  ## Return a matrix that is the inverse of 'x'.
}