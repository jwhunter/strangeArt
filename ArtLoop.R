#this loop is designed as a large trial-and-error exercise
#in making art with strange attractors using the strangeArt.R functions
#it builds a folder for each day the loop is run,
#then dumps all art in that folder. i found this is convenient
#because it can take a long time to get good art,
#and with this loop i can, slow-cooker style, "set it and forget it"
#-----------------------------------------------------------------------

library(tidyverse) #for usefulness
library(ggplot2) #for plotting

#this builds a string to become the title of the daily folder
datestring <- gsub("-", "",Sys.Date())

#for a regular user, this should be changed to whatever you prefer
setwd("/Users/Jacob/R/art/")

#create the daily folder
dir.create(datestring)

#work in the daily folder
setwd(datestring)

#use the recommended (-4,4) boundaries
min <- -4
max <- 4

#a nice, clean, empty theme to frame our art
opt <-  theme(legend.position  = "none",
              panel.background = element_rect(fill="white", color="black"),
              plot.background  = element_rect(fill="white"),
              axis.ticks       = element_blank(),
              panel.grid       = element_blank(),
              axis.title       = element_blank(),
              axis.text        = element_blank(),
              aspect.ratio = 1)

#main loop below--------------------

#October 22 note, in this run I'm making the 
#kurtosis threshold 0, and vectors must have a LOWER value
#this should be a fatter tailed distribution, so points
#should be more spread out. these changes were made
# in the strangeArt file, function kurtosisCheck. fingers crossed!
### it failed, utterly. sparse points, or maybe an oblong shape. back to kurtosis
### being forced ABOVE .5. perhaps the function works counterintuitively.

#best case scenario, the loop will build 100 pieces of art.
#it will take about two hours to get through the whole thing.
#i like to do it before i go to bed, so my laptop's fan
#becomes a nice white noise track for sleeping
iterations <- 100
for (i in 1:iterations){
    #a should become a vector of parameters that, at least,
    #makes a good picture of 50,000 points
    a <- generateParameters(2000, min, max, nTest = 50000, paintingNumber = i)
    
    #the if statement first checks to make sure generateParameters could make
    #a good vector of parameters. recall that in the case that every attempt 
    #to make parameters fails, it returns a vector of parameters 1 larger than (max) to be
    #error handled outside the function.
    if(max(a)<=max){ 
        filename <- paste("strange",datestring,i, sep="-")
        
        #now we use our vector 
        df <- createTrajectory(10000000, 1, 1, a)
        
        if(!nanCheck(df)){
            print(paste(i,'failed nanCheck at large trajectory'))
        }else if(!infCheck(df)){
            print(paste(i,'failed infCheck at large trajectory'))
        }else if(!sizeCheck(df)){
            print(paste(i,'failed sizeCheck at large trajectory. max:',max(df)))
        }else if(!kurtosisCheck(df)){
            print(paste(i,'failed kurtosisCheck. kurt:',kurtosis(df$x), kurtosis(df$y)))
        }else{
            print(paste(i,'passed at large trajectory! Writing file now.'))
            mx <- quantile(df$x, probs = 0.05, na.rm = TRUE)
            Mx <- quantile(df$x, probs = 0.95, na.rm = TRUE)
            my <- quantile(df$y, probs = 0.05, na.rm = TRUE)
            My <- quantile(df$y, probs = 0.95, na.rm = TRUE)
            
            df %>% filter(x > mx, x < Mx, y > my, y < My) -> df
            
            plot <- ggplot(df) +
                geom_point(aes(x, y), shape=46, alpha=0.01, size=0, color="black") +
                scale_x_continuous(expand = c(0,0))+
                scale_y_continuous(expand = c(0,0))+
                coord_fixed() + 
                opt
            
            #write the parameters in case I want to use them again later
            write(a, filename, sep=",")
            
            #save the art
            ggsave(paste(filename,".png", sep=''), 
                   plot, 
                   height = 10, 
                   width = 10, 
                   units = 'in', 
                   dpi = 1200)
        }
    }
}
