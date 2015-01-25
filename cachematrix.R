
##----------------------------------------------------------------------------
##
##              Matrix Inverter and Cache
##
##   The functions below are used in conjunction to find the  
##   inverse of a matrix and cache it for future retrieval. 
##
##   It is operated as follows:
##
##      1. The first time around makeCacheMatrix is called
##         with the fresh matrix as the argument. 
##         (makeCacheMatrix does not need to be called again
##          for the same matrix.)
##      2. The value returned by makeCacheMatrix is used as
##         the argument to cacheSolve
##      3. For a given matrix, the first time cacheSolve is called,
##         it retrieves the matrix from the cache, calculates its
##         Inverse, stores the Inverse in the cache, and returns
##         the Inverse to the caller.
##      4. In subsequent calls to cacheSolve, the Inverse is 
##         retrieved from the cache and returned to the caller.
##
## ----------------------------------------------------------------------------
##
##  The makeCacheMatrix function does the following:
##
##     1. Initializes the Inverse cache, mI
##
##     2. Stores the Matrix on the first call to this funtion.
##    
##     3. Returns a list of functions that are used by cacheSolve to:
##
##        + Retrieve the matrix (getMat)
##        + Store the Inverse in the cache (setInv)
##        + Retrieve the Inverse from the cache (getInv)
##
## --------------------------------------------------------------------------------

makeCacheMatrix <- function(mat = matrix()) {
  
  mI <- NULL          ##  Initialize the Inverse cache
  
  getMat <- function() mat             ##  Store (1st Time) and 
                                       ##  Retrieve the matrix
  
  setInv <- function(Inv) mI <<- Inv   ##  Store the Inverse
  
  getInv <- function() mI              ##  Retrieve the Inverse
    
  list(getMat = getMat,       ##  return a list of functions
       setInv = setInv,       ##  that will be called by
       getInv = getInv)       ##  cacheSOlve
}


## --------------------------------------------------------------------------------
## 
##   The cacheSolve function does the following:
##
##       1. Checks the cache to see if the Inverse has already been 
##          and stored.
##       2. If the Inverse is present in the cache it is retrieved
##          and returned to the caller.
##       3. If not, the matrix is retrieved from the cache and its Inverse
##          is calculated.
##       4. The Inverse is stored in the cache and returned to the caller.
##
## ---------------------------------------------------------------------------

cacheSolve <- function(xlist) {
  
  mI <- xlist$getInv()
  
  if(!is.null(mI)){
    message("Retrieving Inverse from Cache")
    
    return(mI)               ## Return Inverse to caller
  }
  else {
    message("Calculating Inverse ... and Storing it")
    
    data <- xlist$getMat()   ## Retrieve matrix from cache
    
    mI <- solve(data)        ## Calculate Inverse
    
    xlist$setInv(mI)         ## Store Inverse in Cache
    
    return(mI)               ## Return Inverse to caller
  }
  
}

##---------------------------------------------------------------------------


  
  
  