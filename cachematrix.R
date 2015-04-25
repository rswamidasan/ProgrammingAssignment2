##
##------------------------------------------------------------------------------
##
##                      Matrix Inverter and Cache
##                      =========================
##
##   The functions below are used in conjunction to find the inverse of a
##   matrix and cache it for future retrieval.
##
##   They are operated as follows:
##
##      1. At the start, makeCacheMatrix is called with the fresh matrix as
##         the argument.  makeCacheMatrix does not need to be called again
##         for the same matrix.
##
##      2. The object returned by makeCacheMatrix is a list of functions.  It
##         is used as the argument to cacheSolve, which calls the functions in
##         the list to retrieve the matrix or retrieve/cache the Inverse.
##
##      3. For a given matrix, the first time cacheSolve is called, it
##         retrieves the matrix from the cache, calculates its Inverse,
##         stores the Inverse in the cache, and returns the Inverse to the
##         caller.
##
##      4. In subsequent calls to cacheSolve, the Inverse is retrieved from
##         the cache and returned to the caller.
##
##------------------------------------------------------------------------------
##
##      Tech Details: In returning a list of functions, makeCacheMatrix is
##         returning an environment in which these functions are defined.
##         This environment (which is NOT the same as the Global environment)
##         includes the variables named "mat" and "matInv" (in the code below)
##         and the 3 functions themselves. Thus, everytime makeCacheMatrix is ,
##         called, it creates and returns a unique environment with unique
##         locations in memory for mat and matInv, i.e. the Cache.
##
##         When cacheSolve calls getMat, getInv or setInv, it passes the unique
##         environment created and returned by makeCacheMatrix.  Thus, the
##         unique locations in memory allocated to mat and matInv are accessed
##         to store and retrieve these variables to and from "Cache".
##
## -----------------------------------------------------------------------------
##
##  The makeCacheMatrix function does the following:
##
##      1. Creates (implicitly) an environment in which 3 functions and
##         2 variables are defined.
##
##      2. Initializes the Inverse cache - matInv, to NULL.
##
##      3. Initializes the Matrix cache - mat, to the value in the formal
##         argument.
##
##      4. Returns a list of functions that are used by cacheSolve to:
##
##         + Retrieve the matrix (getMat)
##         + Store the Inverse in the cache (setInv)
##         + Retrieve the Inverse from the cache (getInv)
##
## -----------------------------------------------------------------------------

makeCacheMatrix <- function(mat = matrix()) {

  matInv <- NULL          ##  Initialize the Inverse cache

  ##  ****  There is no need for a set() or setMat() function:
  ##        1. It is not called from cacheSolve (or anywhere else).
  ##        2. Its initialization functions are performed elsewhere.
  ##            (a) above, explicitly, for matInv
  ##            (b) implicitly, through the formal argument, mat
  ##

  getMat <- function() mat      ##  Retrieve the matrix from cache

  setInv <- function(Inv) matInv <<- Inv    ##  Cache the Inverse.

            ##  The super-assignment, "<<-" operator indicates that matInv
            ##  refers to the object in the Calling environment (or Parent
            ##  Frame), not to the object in the defining environment.


  getInv <- function() matInv   ##  Retrieve the Inverse from cache

  list(getMat = getMat,         ##  Return a list of
       setInv = setInv,         ##  functions that can be
       getInv = getInv)         ##  called by cacheSolve
}


## -----------------------------------------------------------------------------
##
##   The cacheSolve function does the following, using the list of functions
##   and the environment passed in its argument:
##
##       1. Checks the cache to see if the Inverse has already been calculated
##          and stored.
##
##       2. If the Inverse is present in the cache it is retrieved and returned
##          to the caller.
##
##       3. If not, the matrix is retrieved from the cache and its Inverse is
##          calculated.
##
##       4. The Inverse is stored in the cache and returned to the caller.
##
## -----------------------------------------------------------------------------

cacheSolve <- function(flist) {

  matInv <- flist$getInv()

  if(!is.null(matInv)){
    message("Inverse Retrieved from Cache")

    return(matInv)              ## Return Inverse to caller
  }

  message("Calculating Inverse ... and Caching it")

  data <- flist$getMat()        ## Retrieve matrix from cache

  matInv <- solve(data)         ## Calculate Inverse

  flist$setInv(matInv)          ## Store Inverse in Cache

  matInv                        ## Return Inverse to caller
}

##---------------------------------------------------------------------------

