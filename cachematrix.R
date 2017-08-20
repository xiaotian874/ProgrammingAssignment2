## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix())
{
        m <- NULL	
        set <- function(y)
		{
                x <<- y			
                m <<- NULL
        }
			
        get <- function() x	
        setInverse <- function(inverse)  m <<- inverse
        getInverse <- function() m

list(set = set, 
get = get, 
setInverse = setInverse, 
getInverse = getInverse)
}


## cacheSolve function

cacheSolve <- function(x, ...)
{

        o <- x$getInverse()
        if(!is.null(o))
		{
                message("getting cached data")
                return(o)
        }
		
        d <- x$get()
        o <- solve(d)
        x$setInverse(o)
        o
}
