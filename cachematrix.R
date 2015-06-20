#Returns the list of available functions(for eg. get,getinverse etc) within the function.
#Takes Matrix as an input.


makeCacheMatrix <- function(x =matrix(nrow=0, ncol=0))
{
	m <- NULL
	set <- function(y)
		{
			x <<- y
			m <<-NULL
		}
	get <- function() x

	setinverse <- function(inv) 
	{
		m <<- inv
		
	}
	getinverse <- function() m

	list(get = get , set = set , getinverse = getinverse , setinverse = setinverse)
}

#Gets like actual matrix and inverse of the matrix from the cache.
#if input matrix and the matrix from the cache are equal and the inverse of the matrix is present in cache.
#Then return the the cached value
#Else compute the inverse and replace matrix and inverse in the cahce.

cacheSolve <- function(x,mat) 
{
	m <- x$getinverse()	
	cachedmat <- x$get()
	 
 	if(length(m) != 0 && all(dim(cachedmat) == dim(mat)) && all(cachedmat == mat))
		{
			print("Returning Cached Value")
			return(m)
		}
	else
		{
			x$set(mat)
			m <- solve(mat)
			x$setinverse(m)
			m
		}
}
