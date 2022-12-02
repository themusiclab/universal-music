# FF dataset builder
# by Lidya Yurdum
# 1 March 2021 
# Written under R version 4.0.4 (2021-02-15)

# Work out of github.com/themusiclab/ff

library(tidyverse) #install.packages("tidyverse")
library(readxl) #install.packages("readxl")
library(stringr) #install.packages("stringr")
library(haven) #install.packages("haven")
library(here) #install.packages("here")


### Builder code: Field data ------------------------------------------------------------

### Bind raw field data files

input_files <- list.files(here("data"), pattern = "FFfield_", full.names = TRUE)


dfsites <- list()

for (i in 1:length(input_files)) {
  df <- read_excel(input_files[i], guess_max = 10000) %>% 
    .[-1, ] %>% # drop first row
    select(indx_cohort = Subject, # add cohort index
                  age = Age, 
                  gender = Sex, 
                  cohort = Group, 
                  experimenter = ResearcherID, 
                  dot = SessionDate, 
                  condition = cond, 
                  songfunction = type, 
                  song = songfile, 
                  q = englishQ, 
                  resp = naivQ.RESP) %>% 
    filter(!songfunction == "practice") %>% 
    arrange(indx_cohort)
  dfsites[[i]] <- df # Write to dfsites list
}

fulldata_field <- do.call("rbind", dfsites)



### Clean up data, recode variables

fulldata_field <- fulldata_field %>% 
  mutate(cohort = replace(cohort, which(cohort == "Vanuatu"), "Bislama")) %>% # few errant mislabels in Vanuatu cohort, all should be called Bislama
  distinct() %>% # This drops a weird duplicate participant - check "problem solving" code below for details
  mutate(
    q = case_when(
      str_detect(q, "express love to another person") ~ "love",
      str_detect(q, "to heal illness") ~ "heal",
      str_detect(q, "soothe a baby") ~ "baby",
      str_detect(q, "praise a person's achievements") ~ "achi",
      str_detect(q, "for dancing") ~ "danc",
      str_detect(q, "to tell a story") ~ "stor",
      str_detect(q, "to greet visitors") ~ "visi",
      str_detect(q, "to mourn the dead") ~ "dead"
    ),
    songfunction = case_when(
      songfunction == "LOVE" ~ "love",
      songfunction == "LULLABY" ~ "baby",
      songfunction == "HEALING" ~ "heal",
      songfunction == "DANCE" ~ "danc"
    ),
    song = str_extract(song, "\\d{3}(?=.mp3)") %>% as.integer
  ) %>%
  filter(!(indx_cohort > 900)) %>% # drop test runs
  group_by(cohort, indx_cohort) %>% 
  mutate(
    indx = cur_group_id(),
    # replace NA stand-ins with NAs
    resp = ifelse(resp == "{SPACE}" | resp == "{ENTER}", NA, resp),
    study = "field",
    natcountry = case_when(
      cohort == "Mentawi" ~ "Indonesia",
      cohort == "Bislama" ~ "Vanuatu",
      cohort == "Ethiopia" ~ "Ethiopia"
    ),
    natlang = case_when(
      cohort == "Mentawi" ~ "ment1249",
      cohort == "Bislama" ~ "bisl1239",
      cohort == "Ethiopia" ~ "nyan1315"
    ),
    natlang_name = case_when(
      natlang == "ment1249" ~ "Mentawai",
      natlang == "bisl1239" ~ "Bislama",
      natlang == "nyan1315" ~ "Nyangatom"
    )
  ) %>% 
  filter(!is.na(resp))

### Recode responses

# Due to the way the keyboard layout was set up in each field site, responses need to be recoded to represent the actual rating scale.
# For Mentawi: keys 2/3/4 become ratings 1/2/3
# For others: keys 5/6/7 become ratings 1/2/3
# NOTE: The field dataset is on a 3-point scale. To protect my dumb future self from thinking 
#   web and field are on the same scale when combining the datasets, I will recode field responses as 11, 12 and 13.

table(fulldata_field$resp) # full range of responses

recode_if <- function(x, condition, ...) { # function for conditionally recoding some vars
  if_else(condition, recode(x, ...), x)
}

fulldata_field <- fulldata_field %>%
  mutate(
    resp = recode_if(resp, cohort == "Mentawi", "2" = "11", "3" = "12", "4" = "13"),
    resp = recode_if(resp, cohort != "Mentawi", "5" = "11", "6" = "12", "7" = "13")
    ) %>% 
  select(indx, everything()) %>% 
  arrange(indx) %>% 
  pivot_wider(names_from = q, values_from = resp) 

# Make rating variables numeric
cols.num <- c("heal", "dead", "achi", "visi", "love", "danc", "baby", "stor")
fulldata_field[cols.num] <- sapply(fulldata_field[cols.num], as.numeric) 

fulldata_field <- fulldata_field %>% 
  # filtering out participants who lack responses for one of the categories
  filter(if_any(c(baby,danc,heal,love,stor,achi,visi,dead), ~ !is.na(.x)))

### Add song metadata

# Add actual song functions from NHSDiscography_metadata.csv. 
metaDisc <- read_csv(here("data", "NHSDiscography_metadata.csv"))
metaDisc <- metaDisc %>%
  dplyr::select(song, type, 
                id_nhs,
                song_region=hraf_region,
                song_subregion=hraf_subregion,
                song_latitude=latitude,
                song_longitude=longitude) %>% 
  mutate(
    song = row_number(), #as.character(
    songfunction = recode(type, "Lullaby" = "baby",
                          "Healing" = "heal",
                          "Dance" = "danc",
                          "Love" = "love")
  )

# Add song language code from id_glottolog column in NHSCultures_metadata.csv
# This file can be found on OSF at https://osf.io/2kqzt/
metaCult <- read_csv(here("data", "NHSCultures_metadata.csv"))
metaCult <- metaCult %>% 
  select(id_nhs, 
         songlang = id_glottolog) %>% 
  mutate(songlang = recode(songlang, "tupi1282|west2640" = "tupi1277",
                           "west2809|east2720" = "azte1234",
                           "loya1239|newc1243" = "newc1243"))

# merge by song, adding id_nhs
fulldata_field <- left_join(fulldata_field,
                            metaDisc[ ,c(1,3:7)], 
                            by = "song")

# merge by id_nhs, adding song language info
fulldata_field <- left_join(fulldata_field,
                            metaCult, 
                            by = "id_nhs") 


### Drop participants with concerning responses and keep only relevant vars

fulldata_field <- fulldata_field %>%
  mutate(
    # drop trial with accidental button press
    dead = ifelse(dead == 1, NA, dead),
    # correct an accidental data entry mistake
    gender = ifelse(cohort == "Bislama" & indx_cohort == 102, "male", gender)
    ) %>% 
  filter (
    !(cohort == "Mentawi" & indx_cohort == 14) & # drop subjects that field experts expressed concern about
      !(cohort == "Mentawi" & indx_cohort == 28) &
      !(cohort == "Mentawi" & indx_cohort == 34) & 
      !(cohort == "Mentawi" & indx_cohort == 43) &
      !(cohort == "Mentawi" & indx_cohort == 57) &
      !(cohort == "Mentawi" & indx_cohort == 33) &
      !(cohort == "Ethiopia" & indx_cohort == 19) &
      !(cohort == "Ethiopia" & indx_cohort == 22) &
      !(cohort == "Ethiopia" & indx_cohort == 52) &
      !(cohort == "Ethiopia" & indx_cohort == 56) &
      !(cohort == "Bislama" & indx_cohort == 5) &
      !(cohort == "Bislama" & indx_cohort == 12) &
      !(cohort == "Bislama" & indx_cohort == 60),
    condition == "Raw"
  ) %>% 
  select(
    study, 
    indx, 
    indx_cohort, 
    cohort, 
    gender, 
    age, 
    song, 
    songfunction, 
    songlang, 
    danc, 
    heal, 
    baby, 
    love, 
    achi, 
    visi, 
    stor, 
    dead,
    natcountry,
    natlang,
    natlang_name,
    song_latitude,
    song_longitude,
    song_region,
    song_subregion
  )

### Builder code: Web data ------------------------------------------------------------

### Bind raw data files

# Create df of raw data file names
input_files <- list.files(here("data"), pattern = "FFweb_")
# remove meta-info files 
input_files <- input_files[input_files != "FFweb_locations.csv" & input_files != "FFweb_qids.xlsx"] 
# grab lang code from file name
langsvar <- str_extract(input_files, "(?<=_)\\w{4}") #extract the 4 letter sequence that follows an underscore

# For every raw data file: 
# add column with language code, filter out incomplete data, write to dfcountries list
dfcountries <- list()
for (i in 1:length(input_files)) {
  df <- read.csv(file.path(here("data"), input_files[i]), stringsAsFactors = F) %>% 
    .[-1, ] %>% 
    mutate(lang = langsvar[i]) %>% 
    filter(gc == 1) %>% # "gc" is qualtrics for "good complete"; 1 means all their conditions for inclusion were satisfied; 2, 3, 4 are partial completes of one form or another (this info from Sam's email correspondences)
    select(ip = V6, 
           lang, # data collection lang
           V1:Q_Language, # reported lang 
           gc:LocationAccuracy) # Specified columns like this to get around a random extra column in two datasets (port_1of2 and port_2of2) that were causing problems when merging with other datasets
  dfcountries[[i]] <- df # Write to dfcountries list
}

# Bind everything in dfcountries into one long dataset
fulldata_web <- bind_rows(dfcountries) %>%
                mutate(indx = row_number()) # add cross-cohort participant indx

### Rename vars

# Read in csv with old and new names for columns (based on the 'rename' commands in Sam's stata code)
# All names are uppercase ('oldupper') to match the qualtrics output format.
newcolnames <- read.csv(here("data", "recoded_vars.csv")) 
newcolnames <- newcolnames %>% 
               mutate(new = as.character(new)) %>% 
               mutate(oldupper = as.character(oldupper))

# Replace old column names with ones in 'newcolnames'
names(fulldata_web)[match(newcolnames[ ,"oldupper"], colnames(fulldata_web))] = newcolnames[ ,"new"] 

### Add geolocation info (NOTE: for the public data repository, identifying IP addresses have been removed, so this code will be skipped.)

## Create vector of ip info
## ip info is from previous analysis (New ip information can be retrieved from ip-api.com)
#ip_files <- list.files(here("results", "temp", "geo"), pattern = ".dta")
#ip_files <- ip_files[!str_detect(ip_files, pattern = "synth")] # exclude synth.dta files

# # Bind all ip files
# ip_data <- do.call("rbind", lapply(ip_files, FUN = function(x) {read_dta(file.path(here("results", "temp", "geo"), x))})) %>% 
#            rename(geo_country_name = v1) %>% 
#            rename(geo_country_code = v2) %>% 
#            rename(geo_latitude = v3) %>% 
#            rename(geo_longitude = v4) %>% 
#            rename(ip = v5) 

# # Merge qualtrics output with ip data
# fulldata_web <- left_join(fulldata_web, 
#                           ip_data, 
#                           by = "ip") %>%
#                 distinct() # remove the duplicate entries created by join

# to maintain column numbers for analyses, I have populated the four hidden IP columns with NA values
fulldata_web <- fulldata_web %>% 
  mutate(geo_country_name = NA,
         geo_country_code = NA,
         geo_latitude = NA,
         geo_longitude = NA)



### Cleaning up output

# Get all columns with song ratings and transform them into numeric
colnames <- fulldata_web %>% 
  select(starts_with("love"), 
         starts_with("heal"),
         starts_with("baby"),
         starts_with("danc"),
         starts_with("achi"),
         starts_with("visi")) %>% 
  names()

fulldata_web[colnames] <- sapply(fulldata_web[colnames], as.numeric) 

# Fix random Qualtrics coding errors
fulldata_web$danc3[fulldata_web$danc3 == 7] <- 3 
fulldata_web$danc3[fulldata_web$danc3 == 8] <- 4
fulldata_web <- fulldata_web %>%
  # rescaling love scale, as it was incorrectly shown on a scale of 1 to 5 instead of 1 to 4
  mutate(love49 = love49*4/5) %>% 
  mutate_all(na_if, "")


# Pivot into long data and drop NAs.
fulldata_web <- fulldata_web %>%
                group_by(natcountry) %>% 
                mutate(
                  # add country index
                  indx_cohort = row_number(),
                  study = "web",
                  # fix random error
                  age = ifelse(age == "28,", "28", age),
                  age = as.numeric(age)
                  ) %>% 
                pivot_longer(cols = play1:like118,
                             names_to = c(".value", "song"),
                             # values_drop_na = TRUE, # for some reason this does nothing??
                             names_pattern = "(^.{4})(\\-*\\d+\\.*\\d*)") %>%
  mutate(song = as.integer(song))

# add song type info
fulldata_web <- left_join(fulldata_web,
                          metaDisc, 
                          by = "song")

# add song language info
fulldata_web <- left_join(fulldata_web,
                          metaCult, 
                          by = "id_nhs") 


glot <- read.csv(here("data", "languoid glottolog.csv"), stringsAsFactors = F)

# add natlang glottocode
glot_web <- glot %>% 
            select (natlang_name = name, natlang = id)

fulldata_web <- fulldata_web %>% 
  select(study,
         indx,
         indx_cohort,
         cohort = lang, # changed to match field dataset. "Cohort" means data collection group
         gender,
         age,
         song,
         songfunction,
         songlang,
         song_region,
         song_subregion,
         uhoh,
         danc,
         heal,
         baby,
         love,
         achi,
         visi,
         like,
         natcountry,
         geo_country_name, #These columns are NAs for privacy
         geo_country_code, 
         geo_latitude, 
         geo_longitude, 
         natlang_name = natlang,
         internet,
         musiclisten,
         musicskill,
         musictrad,
         song_latitude,
         song_longitude) %>% 
  filter(!uhoh == "Yes") %>% 
  left_join(glot_web, 
            by = "natlang_name")

### Bind datasets and add meta language info -----------------------------------------------------------

full <- bind_rows(fulldata_web, fulldata_field) # merge web and field data

glot_song <- glot %>% # meta language info for songs
             select (songlang = id, 
                            songlang_fam = family_id, 
                            songlang_parent = parent_id)
  
glot_speak <- glot %>% # meta language info for speaker language
              select (natlang = id, 
                             natlang_fam = family_id, 
                             natlang_parent = parent_id)

fulldata <- full %>% 
  left_join(glot_song, 
            by = "songlang") %>% 
  # For song language: add songlang_fam (family_id), songlang_parent (parent_id)
  # iroq1247 and seri1257 are missing family and parent id info
  mutate(
    songlang_fam = case_when(
      songlang == "iroq1247" ~ "iroq1247",
      songlang == "seri1257" ~ "seri1257",
      TRUE ~ songlang_fam
      ),
    songlang_parent = case_when(
      songlang == "iroq1247" ~ "iroq1247",
      songlang == "seri1257" ~ "seri1257",
      TRUE ~ songlang_parent
      )
  ) %>%
  left_join(glot_speak, 
            by = "natlang") %>% # For speaker language: add natlang_fam (family_id), natlang_parent (parent_id)
  mutate(natlang_fam = ifelse(is.na(natlang_fam), "sino1245", natlang_fam)) %>%     
  select(study,
         indx,
         indx_cohort,
         cohort, 
         gender,
         age,
         song,
         songfunction,
         songlang,
         songlang_fam,
         songlang_parent,
         song_region,
         song_subregion,
         song_latitude,
         song_longitude,
         danc,
         heal,
         baby,
         love,
         achi,
         visi,
         stor,
         dead,
         like,
         natcountry,
         geo_country_name, # geo columns are NAs for privacy
         geo_country_code, 
         geo_latitude, 
         geo_longitude, 
         natlang_name,
         natlang,
         natlang_fam,
         natlang_parent,
         internet,
         musiclisten,
         musicskill,
         musictrad)

write.csv(fulldata, here("results", "FFfull.csv"), row.names = F)

