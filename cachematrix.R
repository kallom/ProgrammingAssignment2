## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

    # To shamelessly copy the description: 
    # The first function, `makeCacheMatrix` creates a special "matrix", which 
    # is really a list containing a function to
    # 1.  set the value of the matrix
    # 2.  get the value of the matrix
    # 3.  set the value of the inverse
    # 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invMx <- NULL
    set <- function(y) {
        x <<- y
        invMx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) invMx <<- inverseMatrix
    getinverse <- function() invMx
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

    # The following function calculates the inverse of the special "matrix"
    # created with the above function. However, it first checks to see if the
    # inverse has already been calculated. If so, it gets the inverse from the
    # cache (using the getinverse function )and skips the computation. Otherwise,  
    # it calculates the inverse of the matrix and sets the value of the inverse 
    # in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Get the inverse of the matrix, if it isn't NULL, it's already calculated
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    } else { 
        # The inverse wasn't cached (it was NULL), so it needs to be calculated
        data <- x$get()
        m <- solve(data)
        # Store the calculated inverse matrix in the cache
        x$setinverse(m)
        # Return the result
        m
    }    
}

# To test the functions: 
#     1. Create the xx object initialized with a 4x4 (or so) matrix of random 
#        numeric values:
#     
#        xx <- makeCacheMatrix(matrix(rnorm(16),4,4))
# 
#     2. Check the matrix (assigning it to a variable named m1):
#         
#        m1 <- xx$get()
#     
#     3. Calculate the inverse matrix (assigning it to a variable named m2):
#         
#        m2 <- cacheSolve(xx)
#     
#        Notice the cacheSolve function printed no messages, the value 
#        wasn't cached yet.
# 
#     4. Check if m1 and m2 are inverse matrices using the %*% operator
#        the result should be a matrix with 1's in the diagonal and 0's 
#        everywhere else but solve() only calculates it with less precision 
#        (I suppose) so instead of 0's we get values around 1e-17 - 1e-19:
#             
#        m1 %*% m2
#         
#     5. Now let's check it again invoking the cacheSolve function which should 
#        have the inverse stored now:
#         
#        xx$get() %*% cacheSolve(xx)    
# 
#        Notice the cacheSolve function printed the message 'getting 
#        cached data', so the value was cached.
    
