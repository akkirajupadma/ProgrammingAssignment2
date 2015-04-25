## makeCacheMatrix provides mechanism to create store a matrix.
## and its inverse. It needs to be used first before
## using cacheinverse
##
## makeCacheMatrix provides functions to store invertible matrix
## and its inverse.

makeCacheMatrix <- function (x= matrix()) {
	myinverse <- NULL;
	set <- function(y) {
		x <<- y;
		myinverse <<- NULL;
	}
	get <- function() x;
	setinverse <- function(inverse){ myinverse <<- inverse};
	getinverse <- function() myinverse;
	list(set = set, get=get, setinverse=setinverse, getinverse=getinverse);
	}
## cacheinverse function first checks if inverse exists in cache.
## if it does, uses cached result. Else it computes inverse and stores
##
cacheinverse <- function(x,...) {
	m <- x$getinverse();
	if(!is.null(m)) {
		message("getting inverse matrix");
		return(m);
	}
	data <- x$get();
	m <- solve(data)
	x$setinverse(m)
	m
}

