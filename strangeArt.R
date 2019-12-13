#functions to generate strange art, inspired by the following code
#https://github.com/aschinchon/general-2D-map/blob/master/general_2d_map.R

library(e1071) #for kurtosis

#using a starting point (x0, y0) and 
#an array of strange attractor parameters (a)
#this function generates the strange attractor
#trajectory with (n) points
createTrajectory <- function(n, x0, y0, a){
    x <- rep(0, n)
    y <- rep(0, n)
    
    x[[1]] <- x0
    y[[1]] <- y0
    
    for(i in 2:n){
        x[[i]] <- a[[1]] + 
            a[[2]]*x[[i-1]] + 
            a[[3]]*y[[i-1]] + 
            a[[4]]*(abs(x[[i-1]])^a[[5]]) + 
            a[[6]]*(abs(y[[i-1]])^a[[7]])
        
        y[[i]] <- a[[8]] + 
            a[[9]]*x[[i-1]] + 
            a[[10]]*y[[i-1]] + 
            a[[11]]*(abs(x[[i-1]])^a[[12]]) + 
            a[[13]]*(abs(y[[i-1]])^a[[14]])
    }
    
    createTrajectory <- data.frame(
        x = x,
        y = y
    )
}

#if there are any NaN's in the input, return false
nanCheck <- function(df){
    if(sum(is.nan(df$x)) > 0){
        nanCheck <- FALSE
    } else if(sum(is.nan(df$y)) > 0){
        nanCheck <- FALSE
    } else {
        nanCheck <- TRUE
    }
}

#if there are any infinites in the input, return false
infCheck <- function(df){
    if(sum(is.infinite(df$x)) > 0){
        infCheck <- FALSE
    } else if(sum(is.infinite(df$y)) > 0){
        infCheck <- FALSE
    } else {
        infCheck <- TRUE
    }
}

#if the input reaches unreasonably large numbers, return false
sizeCheck <- function(df, threshold = 10^10){
    if(sum(which(abs(df$x) > threshold) > 0)){
        sizeCheck <- FALSE
    } else if(sum(which(abs(df$y) > threshold) > 0)){
        sizeCheck <- FALSE
    } else {
        sizeCheck <- TRUE
    } 
}

#if kurtosis of any vector in the input is < .5, return false
#this keeps a more desireable "spread out" shape
kurtosisCheck <- function(df, threshold = .5){
    if(kurtosis(df$x) < threshold){
        kurtosisCheck <- FALSE
    } else if(kurtosis(df$y) < threshold){
        kurtosisCheck <- FALSE
    } else {
        kurtosisCheck <- TRUE
    } 
}

#if x and y have a correlation greater than .05, return false
#this, too, helps to create a pleasant, random, natural shape
correlationCheck <- function(df, threshold = .05){
    if(cor(df$x, df$y) > threshold){
        correlationCheck <- FALSE
    }else {
        correlationCheck <- TRUE
    }
}

#if x and y start to repeat, return false
#this is the greatest threat against a good random attractor
uniquesCheck <- function(df){
    if(nrow(unique(df))<nrow(df)){
        uniquesCheck <- FALSE
    } else {
        uniquesCheck <- TRUE
    }
}

#using random numbers within the range of min and max,
#make a number of attempts at strange attractors.
#build (iterations) number of random parameter vectors, or "attempts",
#generate trajectories of size (nTest) with them, 
#then run those trajectories against all tests for reasonableness 
#and aesthetic value. in case every attempt fails, return
#a vector of parameters 1 larger than (max) to be
#error handled outside the function
generateParameters <- function(iterations, min, max, nTest = 2000, paintingNumber){
    for(i in 1:iterations){
        a <- runif(14, min, max)
        b <- rep(max+1, 14)
        
        df <- createTrajectory(nTest, 1, 1, a)
        
        if(!nanCheck(df)){
            print(paste('painting number',paintingNumber,'-', i,'failed nanCheck'))
        }else if(!infCheck(df)){
            print(paste('painting number',paintingNumber,'-', i,'failed infCheck'))
        }else if(!sizeCheck(df)){
            print(paste('painting number',paintingNumber,'-', i,'failed sizeCheck. max:',max(df)))
        }else if(!kurtosisCheck(df)){
            print(paste('painting number',paintingNumber,'-', i,'failed kurtosisCheck. kurt:',kurtosis(df$x), kurtosis(df$y)))
        }else if(!correlationCheck(df, threshold = .2)){
            print(paste('painting number',paintingNumber,'-', i,'failed correlationCheck:',cor(df$x, df$y)))
        }else if(!uniquesCheck(df)){
            print(paste('painting number',paintingNumber,'-', i,'failed uniquesCheck'))
        }else{
            print(paste('painting number',paintingNumber,'-', i,'passed!'))
            b <- a
            break
        }
    }
    
    generateParameters <- b

}
