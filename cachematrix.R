## The functions below serve to provide a mechanism by which to cache the inverse of a matrix
##      in memory

## This function will return a list of functions. Due to lexical scoping, these functions will all 
##      share the same parent environment, the environment they were defined in. This will allow them 
##      to operate on the same values. 
##      This allows function calculateAndCacheMatrixInverse() to access the same base_matrix value 
##      which is referenced by the initial assignment from the constructor's parameter as well as 
##      functions setBaseMatrix() and getBaseMatrix()
##      Each call to matrixInverterCached() will produce a new list of functions; this new list of
##      functions will have their own distinct parent environment. This means that lists created by
##      separate calls to matrixInverterCached() will NOT share the same parent environment, and as a
##      result will not operate on the same values. In this sense, the parameter to matrixInverterCached()
##      can be thought of as analogous to a parameter being sent to a class constructor method. Methods
##      getBaseMatrix() and getInvertedMatrix() are analogous to accessors, and setBaseMatrix() and
##      setInvertedMatrix() are analogous to mutators in the Object-Oriented concept of encapsulation
matrixInverterCached <- function(base_matrix_to_construct_with = matrix())
{
    base_matrix <- base_matrix_to_construct_with;
    inverse_matrix_to_cache <- NULL;
    
    setBaseMatrix <- function(matrix_to_set_as_base)
    {
        base_matrix <<- matrix_to_set_as_base;
        inverse_matrix_to_cache <-- NULL
    }
    
    getBaseMatrix <- function()
    {
        base_matrix;
    }
    
    setInvertedMatrix <- function(matrix_to_set_as_inverted)
    {
        inverse_matrix_to_cache <<- matrix_to_set_as_inverted;
    }
    
    getInvertedMatrix <- function()
    {
        inverse_matrix_to_cache
    }
    
    # This function will return the inverse of the parameter matrix. It will not
    #   cache the result
    calculateMatrixInverse <- function(matrix_to_calculate_inverse_of)
    {
        # According to ?solve, "If missing, b is taken to be an identity matrix and 
        #                       solve will return the inverse of a"
        inverse_matrix <- solve(matrix_to_calculate_inverse_of);
        inverse_matrix
    }
    
    # This function will cache the inverse of the cached base_matrix and return it
    calculateAndCacheMatrixInverse <- function()
    {
        # According to ?solve, "If missing, b is taken to be an identity matrix and 
        #                       solve will return the inverse of a"
        inverse_matrix_to_cache <<- solve(base_matrix);
        inverse_matrix_to_cache
    }
    
    list(calculateMatrixInverse = calculateMatrixInverse,
         calculateAndCacheMatrixInverse = calculateAndCacheMatrixInverse,
         getInvertedMatrix = getInvertedMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getBaseMatrix = getBaseMatrix,
         setBaseMatrix = setBaseMatrix)
}

# Implement makeCacheMatrix as an alias for matrixInverterCached
# Parameter x is expected to be an invertible matrix
makeCacheMatrix <- function(x = matrix())
{
    return(matrixInverterCached(x))
}


## This function will check to see if the parameter matrixInverterCachedInstance
##   (assumed to be a list) has an inverted matrix cached by executing accessor function
##   getInvertedMatrix. If so, it will return the inverted matrix. If not, it will
##   execute a call to calculateAndCacheMatrixInverse which will calculate the inverse
##   of the base matrix defined by matrixInverterCachedInstance$calculateAndCacheMatrixInverse(),
##   cache that inverted matrix, and return the inverted matrix. This function will then return 
##   said inverse returned by matrixInverterCachedInstance$calculateAndCacheMatrixInverse()
cache_and_return_inverted_matrix <- function(matrixInverterCachedInstance)
{
    inverted_matrix <- matrixInverterCachedInstance$getInvertedMatrix();
    if (!is.null(inverted_matrix))
    {   # The calling block wants the cached inverse and the cache has been set
        return(inverted_matrix);
    }

    # Even if use_cached is FALSE, we will cache the inverse of the matrix
    inverted_matrix_to_cache_and_return <- matrixInverterCachedInstance$calculateAndCacheMatrixInverse()
    inverted_matrix_to_cache_and_return
}

# Implementing cacheSolve as an alias for cache_and_return_inverted_matrix
# Parameter x is expected to be a list returned by a call to function makeCacheMatrix()
cacheSolve <- function(x, ...)
{
    return(cache_and_return_inverted_matrix(x));
}
