## 
##      The functions here will demonstrate the lexical scoping capabilities that
##      the R programming language has.  Primarily this means that R has a 
##      hierarchical set of environments where we are able to create functions
##      within functions and complete assignment states to variables in other
##      environments (ancestors).
##
##      Specifically, this assignment is to cache a matrix.
##      Each time that we try to inverse a matrix it can be very expensive
##      in computing costs.  If we can cache the matrix and we determine that
##      nothing needs to be done then we can save some CPU cycles and this
##      could save on performance.
##

##
##      Function:       makeCacheMatrix()
##
##      Description:    Create a matrix and provide function closures to:
##                              --      Set the values in the matrix
##                              --      Get the values from the matrix
##                              --      Set the matrix inverse
##                              --      Get the matrix inverse
##
##      Input:          - x     :       Existing matrix -- if none given new matrix defined
##
##      Output:         List of matrix closures
##

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
                
                ##
                ##       Just print out the environment for fun
                ##
                
                print(environnment())
        }
        
        get<-function() {
                x
        }
        
        setmatrix<-function(solve) {
                m<<- solve
        }
        
        getmatrix<-function() {
                m
        }
        
        list(set=set, get=get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)
}


##
##      Function:       cacheSolve
##      
##      Description:    Inverse a matrix after cache matrix was created
##
##      Input:          List of makeCacheMatrix closure functions
##
##      Output:         Value of matrix
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        data<-x$get()
        m<-solve(data, ...)
        x$setmatrix(m)
        m
}
