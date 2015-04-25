##
##----------------------------------------------------------------------------
##
##              Matrix Inverter and Cache
##
##   The functions below are used in conjunction to find the
##   inverse of a matrix and cache it for future retrieval.
##
##   It is operated as follows:
##
##      1. At the start, makeCacheMatrix is called with the
##         fresh matrix as the argument.  makeCacheMatrix does
##         not need to be called again for the same matrix.
##
##      2. The object returned by makeCacheMatrix is a list of functions.
##         It is used as the argument to cacheSolve, which calls these
##         functions to retrieve the matrix or retrieve/cache the Inverse.
##
##      3. For a given matrix, the first time cacheSolve is called,
##         it retrieves the matrix from the cache, calculates its
##         Inverse, stores the Inverse in the cache, and returns
##         the Inverse to the caller.
##
##      4. In subsequent calls to cacheSolve, the Inverse is
##         retrieved from the cache and returned to the caller.
##
## ----------------------------------------------------------------------------
##
##  The makeCacheMatrix function does the following:
##
##     1. Initializes the Inverse cache, matInv.
##
##     2. Caches the Matrix on the first call to this funtion.
##
##     3. Returns a list of functions that are used by cacheSolve to:
##
##        + Retrieve the matrix (getMat)
##        + Store the Inverse in the cache (setInv)
##        + Retrieve the Inverse from the cache (getInv)
##
## --------------------------------------------------------------------------------

makeCacheMatrix <- function(mat = matrix()) {

  matInv <- NULL          ##  Initialize the Inverse cache

                                ##  Cachee the matrix when makeCacheMatrix called
  getMat <- function() mat      ##  (i.e. 1st Time around); AND -
                                ##  Retrieve the matrix when called from cachesolve

  setInv <- function(Inv) matInv <<- Inv    ##  Cache the Inverse: The super-assignment
                                            ##  "<<-" operator means that Inv refers to
                                            ##  the object in the Calling environment.
                                            ##  Inv is not defined in the Local environment.
                                            ##  And, it should not be mistaken for any
                                            ##  other object named Inv in the parent
                                            ##  environment(s) of this function.

  getInv <- function() matInv               ##  Retrieve the Inverse from cache

  list(getMat = getMat,         ##  Return a list of
       setInv = setInv,         ##  functions that can be
       getInv = getInv)         ##  called by cacheSolve
}


## --------------------------------------------------------------------------------
##
##   The cacheSolve function does the following:
##
##       1. Checks the cache to see if the Inverse has already been
##          calculated and stored.
##       2. If the Inverse is present in the cache it is retrieved
##          and returned to the caller.
##       3. If not, the matrix is retrieved from the cache and its Inverse
##          is calculated.
##       4. The Inverse is stored in the cache and returned to the caller.
##
## ---------------------------------------------------------------------------

cacheSolve <- function(xlist) {

  matInv <- xlist$getInv()

  if(!is.null(matInv)){
    message("Inverse Retrieved from Cache")

    return(matInv)              ## Return Inverse to caller
  }

  message("Calculating Inverse ... and Caching it")

  data <- xlist$getMat()        ## Retrieve matrix from cache

  matInv <- solve(data)         ## Calculate Inverse

  xlist$setInv(matInv)          ## Store Inverse in Cache

  matInv                        ## Return Inverse to caller
}

##---------------------------------------------------------------------------




