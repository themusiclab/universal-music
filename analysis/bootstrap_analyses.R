

# load libraries ----------------------------------------------------------


library(tidyverse)
library(here)


# load data ---------------------------------------------------------------


fulldata <- read_csv(here("results", "FFfull.csv"), 
                     col_types = "fiiffiifcccccddddddddddffffddfcccffff") %>% 
  left_join(.,
            read_csv(here("data", "countries.csv"),
                     col_types = "ccc"), 
            by = "natcountry") %>% 
  mutate(songfunction = factor(songfunction, levels = c("danc", "baby", "heal", "love")))


# run first bootstrap analysis --------------------------------------------


set.seed(1)
n = 10000

# This function takes 8 participants' ratings for each song,
# creates a new df with the mean ratings on the dance dimension
# and returns the correlation between the web and field cohorts' 
# dance ratings on all 118 songs 

boot <-  function(fulldata, type) {
  
  # create web df using sample of 8 listeners
  bootweb <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("song", "mean_score")
  colnames(bootweb) <- x
  
  # for each song, take all associated trials, grab 8 of them at random, and average them to create a new df
  for (i in 1:118) { 
    websample <- (fulldata[fulldata$study == "web" & fulldata$song == i,]) # all ratings for song i
    indx <- sample(nrow(websample), 30, replace = T) # sample of 8 indices
    bootweb[i,1] <- i # song id
    bootweb[i,2] <- mean(websample[indx,][[type]]) # ratings on dance dim
  }
  
  # create field df using sample of 8 listeners
  bootfield <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(bootfield) <- x
  for (i in 1:118) {
    fieldsample <- (fulldata[fulldata$study == "field" & fulldata$song == i,]) 
    indx <- sample(nrow(fieldsample), 30, replace = T) # sample of 8 indices
    bootfield[i,1] <- i
    bootfield[i,2] <- mean(fieldsample[indx,][[type]], na.rm=T)
    
  }
  
  cor(bootweb[ ,2], bootfield[ ,2], method ='s') # correlate web and field dfs
}

# run bootstrap on all 4 song types, and repeating n times
bootstrap_data <- map_dfr(c("danc", "baby", "heal", "love") |> set_names(), \(song_type) {
  map_dbl(1:n, ~ boot(fulldata, song_type))
})

write_csv(bootstrap_data, file = here("results", "bootstrap_res.csv"))

