

# load libraries ----------------------------------------------------------


library(tidyverse)
library(here)
library(boot)


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
repeats <- 10000
n_songs <- 118


correlation_extractor <- function(df, i) {
  df[i, ] |> 
    group_by(song, study) |> 
    summarise(mean_score = mean(score, na.rm = TRUE)) |> 
    pivot_wider(names_from = study, values_from = mean_score) |> 
    with(cor(web, field, method = "spearman"))
}

sampler <- function(data, p) {
  slice_sample(data |> group_by(song, study), n = 30, replace = TRUE)
}

bootstrap_data <- map_dfr(c("danc", "baby", "heal", "love") |> set_names(), \(.song_type) {
  out <- rename(fulldata, score = {{.song_type}}) |> 
    boot::boot(correlation_extractor, R = repeats, sim = "parametric",
               ran.gen = sampler,
               parallel = "multicore")
  as.numeric(out$t)
}) |> suppressMessages()


write_csv(bootstrap_data, file = here("results", "bootstrap_res.csv"))


# run second bootstrap analysis -------------------------------------------


# This chunk defines the function used to bootstrap random pairs of countries, correlated their responses on a given scale, and store the coefficient (in `cors`). 
# To save time while knitting this document, this chunk will not be executed. Instead, results of the bootstrapping procedure are read in from results/bootstrap_res2.csv. 

set.seed(1)

webraw <- filter(fulldata, study == "web")

# randomly select two industrialised cohorts, compute their song-wise mean ratings, and store their correlation. 

boot2 <- function(type, n = 500000) {
  cors <- c()
  
  for (i in 1:n) {
    cohorts <- sample(unique(webraw$cohort),2)
    
    c1 <- fulldata[fulldata$cohort==cohorts[1],]
    c2 <- fulldata[fulldata$cohort==cohorts[2],]
    
    c1 <- c1 %>% 
      group_by(song, songfunction) %>% 
      summarize(mean_danc = mean(danc, na.rm = T),
                mean_heal = mean(heal, na.rm = T),
                mean_baby = mean(baby, na.rm = T),
                mean_love = mean(love, na.rm = T)) 
    c2 <- c2 %>% 
      group_by(song, songfunction) %>% 
      summarize(mean_danc = mean(danc, na.rm = T),
                mean_heal = mean(heal, na.rm = T),
                mean_baby = mean(baby, na.rm = T),
                mean_love = mean(love, na.rm = T)) 
    
    if (type == "danc") {
      cors[i] <- cor.test(c1$mean_danc, c2$mean_danc)$estimate
    } else if (type =="baby") {
      cors[i] <- cor.test(c1$mean_baby, c2$mean_baby)$estimate
    } else if (type =="heal") {
      cors[i] <- cor.test(c1$mean_heal, c2$mean_heal)$estimate
    }else if (type =="love") {
      cors[i] <- cor.test(c1$mean_love, c2$mean_love)$estimate
    } else {
      warning("Incorrect input.")
    }
  }
  
  return(cors)
}

danccors <- suppressMessages(boot2("danc"))
babycors <- suppressMessages(boot2("baby"))
healcors <- suppressMessages(boot2("heal"))
lovecors <- suppressMessages(boot2("love"))

within_cors <- cbind(danccors, babycors, healcors, lovecors)
write.csv(within_cors,here("results", "bootstrap_res2.csv"), row.names = FALSE)

