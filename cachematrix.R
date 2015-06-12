#########################################################################################################################
# File: cacheMatrix.R
# Author: Neil Kingston
# Create date: 12 June 2015 AEST
# Facetious comments: It's late and I am tired!!!
# Useful comments:
# This file contains two functions for Assignment 2 of the Introduction to R Programming course on Coursera
# along with some test scripts.
# The controlling function is cacheSolve which orchestrates the calling of functions held within makeCacheMatrix
# that relate to efficient setting and calculating the inverse matrices.  By 'efficient' we mean that we avoid
# calculing the inverse matrix when it is already known to the system
########################################################################################################################


## For testing only.  Comment out next 'rm' instruction line when not testing.  
## Clear the memory, as we don't want to skew results 
rm(list=ls())




############################################### function definitions ###################################################

makeCacheMatrix <- function(x = matrix()) {

        #################################################################################################################
        # this is a functions with other function definitions inside.  These functions relate to solving an
        # inverse matrix and storing that inverse in the R global environment.  Additional functions are provided to
        # ensure that we save CPU by only calling the inverse matrix calculation if the input matrix has changed
        # there are five function defined
        # 1. setInput() takes an input matrix and stores it in the environment
        # 2. getInput() returns the matrix already stored in the environment
        # 3. inputChanged() takes an input matrix and returns TRUE if it the same as the matrix stored in environment
        # 4. setInverse() takes input matrix, inverts it, and stores it is memory
        # 5. getInverse() retrieves teh inverse matirx stored in memory
        #################################################################################################################

                setInput <- function (y)
                {
                        inputMatrix_m    <<- y
                } 
                
                inputChanged <- function (y)
                {
                          ## If this is the first time this function is invoked then inputMatrix_m will not exist         
                          if (exists("inputMatrix_m"))
                          {
                                  if(identical(y, inputMatrix_m))
                                  {
                                          return(FALSE)
                                  }
                                  else
                                  {
                                           return(TRUE)
                                  }          
                          }
                          else
                          {
                                  inputMatrix_m <<- y  
                                  return (TRUE)
                          }
                } #end function inputChanged
                              

                getInput <- function (){
                        inputMatrix_m
                } 
                                
                setInverse <- function (y)
                {
                        inverseMatrix_m <<- solve(y)
                }
                
                
                getInverse <- function ()
                {
                        inverseMatrix_m
                }
                
                list(setInput     = setInput,
                     setInverse   = setInverse,
                     getInput     = getInput,
                     inputChanged = inputChanged,
                     getInverse   = getInverse)
} # end function makeCacheMatrix


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        ## It assumes that the input matrix is solvable.  All solvable matrices are square.
        ## Only a subset of square matrices can be inverted.  Instructions for assignment said we should
        ## assume all inputs are solvable

        temp <- makeCacheMatrix(x)

        if(temp$inputChanged(x) == TRUE){
                temp$setInput(x)
 
                temp$setInverse(x)
        }
        return (temp$getInverse())                
} # end function cacheSolve

############################# end of functions ################################################################

### the following are test scripts.  N.B can only use solvable matrix inputs as per instructions

## test can run when never run before (due in this case to the rm(list-ls()) instruction at the top)
mat <- matrix (1:4, nrow=2, ncol=2)
a <- cacheSolve(mat)
print(a)

## test can run with identifical input and not run setInverse again
a <- cacheSolve(mat)
print(a)

## test can run with changed input of same input matrix dimensions
mat <- matrix (5:8, nrow=2, ncol=2)
a <- cacheSolve(mat)
print(a)

## test can run with changed input of changed input matrix dimensions
mat <- matrix (c(7,2,1,0,3,-1,-3,4,-2), nrow=3, ncol=3)
a <- cacheSolve(mat)
print(a)