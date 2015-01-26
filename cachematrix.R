##
## Caching - This is the concept of storing and retrieving data that can be either 
## 1. Entered or manual data - static content, or
## 2. Computed, gathered or calculated data - dynamic content
## In this program, we are creating two functions makeCacheMatrix and cacheSolve.
## The goal of makeCacheMatrix is to create a special "matrix" object that can cache its inverse
## The goal of cacheSolve is to compute the inverse of the special "matrix" returned by makeCacheMatrix.
## The trick is to retrieve the inverse from the cache, if and only IF, 
## 1. the inverse has already been calculated, and
## 2. the matrix remains the same (or is unchanged)
##
## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
##
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  						
        set <- function(y) { 								## set the value of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x									## get the value of the matrix
        setinverse <- function(inverse) i <<- inverse		## set the value of the inverse
        getinverse <- function() i 			        		## get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##
## The cacheSolve function checks to see if there was prior computation of the inverse function(s) results.
## If there was none, it calculates the inverse of the special "matrix" that was created by makeCacheMatrix,
## and sets the value in the cache. cacheSolve assumes that the matrix supplied is always invertible.
## If it was calculated and stored in prior, it gets the inverse from the cache and skips the computation.
## And if not, it calculates the inverse of the data and sets the value of the mean in the cache.
##


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()                                     ## get the value of the inverse
        if(!is.null(i)) {                                       ## if the obtained inverse is NOT null
                message("ALERT: Obtaining Cached Data.")        ## display the Alert message
                return(i)                                       ## return the value of the inverse
        }
        data <- x$get()                                         ## get the value of the matrix
        i <- solve(data)                                        ## solve computes the inverse of the matrix
        x$setinverse(i)                                         ## set the value of the inverse
        return(i)                                               ## return the value of the inverse
}

## Alerting a user is important when it comes to caching in programming or software,
## to ensure that users are aware of the fact that it is not real-time response/information.

## TESTING THE CODE
## I simulated a simple matrix example to demonstrate the two functions:
## 1. makeCacheMatrix
## 2. cacheSolve
## To do this, I would have to create a matrix "x".
##      > x <- rbind(c(2,4), c(3,-6))
##      > m = makeCacheMatrix(x)
##      > x
##      [,1] [,2]
##      [1,]    2    4
##      [2,]    3   -6
##      > cacheSolve(m)
##      [,1]        [,2]
##      [1,] 0.250  0.16666667
##      [2,] 0.125 -0.08333333
##      > cacheSolve(m)
##      ALERT: Obtaining Cached Data.
##      [,1]        [,2]
##      [1,] 0.250  0.16666667
##      [2,] 0.125 -0.08333333
##      > 

## In here "x" is a 2x2 matrix, where the values were binded and makeCacheMatrix(x) was then stored in m.
## When cacheSolve(m) was ran the first time (since the inverse was not computed or stored before)
## the inverse was computed and displayed, but when we called it the second time it alerted stating that
## the values were being obtained from the cache.