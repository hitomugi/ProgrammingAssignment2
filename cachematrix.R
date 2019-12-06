# Below are two functions that are used to create a object that stores a matrix and caches its inverse.
 

# The first function, makeCacheMatrix creates a matrix, which is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 

}


## The following function calculates the inverse of the matrix created with the above function. 
## First, checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. If not, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# I tested my code using the sample test which the mentor Alan E. Berger provided.
# Confirmed these functions work.
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1 

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

m1 %*% n1

n1 %*% m1

solve(m1)

solve(n1)

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)

cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)








