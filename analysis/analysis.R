

# libraries ---------------------------------------------------------------

library(pacman)
p_load(
  here,
  nlme,
  lme4,
  multcomp,
  RColorBrewer,
  lmerTest,
  gt,
  scales,
  ggpubr,
  geodist,
  cowplot,
  patchwork,
  ggsignif,
  broom,
  broom.mixed,
  ggeffects,
  ggtext,
  ggnewscale,
  glue,
  rgeos,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  kableExtra,
  factoextra,
  tidyverse
)

set.seed(42)

# load data ---------------------------------------------------------------

fulldata <- read_csv(here("results", "FFfull.csv"), 
                     col_types = "fiiffiifcccccddddddddddffffddfcccffff") %>% 
  left_join(.,
            read_csv(here("data", "countries.csv"),
                     col_types = "ccc"), 
            by = "natcountry") %>% 
  mutate(songfunction = factor(songfunction, levels = c("danc", "baby", "heal", "love")))

# data storage lists
mods <- list()

# remove IPs that don't match reported location ---------------------------

# some participants slipped through the Qualtrics exclusion criteria. The code below identifies 8 participants whose IP address did not match the country they reported being in. 
# For the public repository for this data, we have removed identifying info such as IP addresses. As such, these participants have been manually removed.

# mismatch_ips <- fulldata %>% 
#   select(indx,geo_country_name, natcountry, listener_subregion) %>% distinct() %>% 
#   mutate(oops = ifelse(geo_country_name != natcountry, 1,0)) %>% 
#   filter(oops == 1 & natcountry ! = "Czech Republic" &  natcountry != "Russian Federation") %>% distinct()

mismatch_ips <- c(1994, 2001, 2005, 2010, 2159, 4822, 4999, 5036) # manually remove mismatched IPs

fulldata <- fulldata %>% 
  filter(!indx %in% mismatch_ips)


# wrangling ---------------------------------------------------------------

# generate song level data for Ind. cohort confirmatory analyses
webdat <- fulldata %>% 
  filter(study == "web") %>% 
  group_by(song, songfunction) %>% 
  # averaging scores
  mutate(across(
    c(danc, heal, baby, love, achi, visi),
    ~ mean(.x, na.rm = T),
    .names = "mean_{.col}")) %>% 
  mutate(across(
    c(danc, heal, baby, love, achi, visi),
    ~ sd(.x, na.rm = T)/sqrt(n()),
    .names = "se_{.col}")) %>% 
  dplyr::select(song, songfunction,starts_with(c("mean","se"))) %>% distinct() %>% 
  ungroup() %>% 
  # z-scoring
  mutate(across(
    c(mean_danc, mean_baby, mean_heal, mean_love),
    ~ scale(.x),
    .names = "zm_{.col}"
  )) %>% 
  rename_with(~ str_remove(.x, "mean_"), contains("zm")) %>% 
  # dummy variables
  mutate(var = 1, temp = songfunction,
         songfunction = factor(songfunction, levels = c("danc", "heal", "love", "baby"))) %>% 
  pivot_wider(names_from = temp, values_from = var, values_fill = 0)

# generate song level data for SS cohort confirmatory analyses
fieldraw <- fulldata %>% 
  filter(study == "field")

fielddat <- fieldraw %>% 
  mutate(across(
    c(danc, heal, baby, love),
    # the recoding is because the SS society responses were originally coded as
    # 11,12,13 instead of 1,2,3, to avoid confusing the scale with the 4-point industrialised cohort scale. 
    ~ recode(.x, `11` = 1, `12` = 2, `13` = 3)
  )) %>% 
  group_by(song, songfunction) %>% 
  # averaging scores
  mutate(across(
    c(danc, heal, baby, love, achi, visi, stor, dead),
    ~ mean(.x, na.rm = T),
    .names = "mean_{.col}")) %>% 
  mutate(across(
    c(danc, heal, baby, love, achi, visi, stor, dead),
    ~ sd(.x, na.rm = T)/sqrt(n()),
    .names = "se_{.col}")) %>% 
  dplyr::select(song, songfunction,starts_with(c("mean","se"))) %>% distinct() %>% 
  ungroup() %>% 
  # z-scoring
  mutate(across(
    c(mean_danc, mean_baby, mean_heal, mean_love),
    ~ scale(.x),
    .names = "zm_{.col}"
  )) %>% 
  rename_with(~ str_remove(.x, "mean_"), contains("zm")) %>% 
  # dummy variables
  mutate(var = 1, temp = songfunction) %>% 
  pivot_wider(names_from = temp, values_from = var, values_fill = 0)

webraw <- fulldata %>% 
  filter(study == "web") %>% 
  dplyr::select(-c("stor", "dead")) %>%
  mutate(
    langshare = ifelse(songlang_fam == natlang_fam, "Shared", "Not shared"),
    geoshare_sub = ifelse(song_subregion == listener_subregion, "Shared", "Not shared"),
    geoshare_reg = ifelse(song_region == listener_region, "Shared", "Not shared")
  ) %>% 
  # z-scoring
  mutate(across(
    c(danc, baby, heal, love),
    ~ scale(.x),
    .names = "zm_{.col}"
  ),
  # adding distance variable
  dist = geodist_vec(song_latitude,
                     song_longitude,
                     geo_latitude,
                     geo_longitude, 
                     paired=T,
                     measure="haversine")) %>% 
  mutate(z_dist = scale(dist),
         log_dist = log1p(dist))

# combined data
combined_data <- left_join(
  webdat %>% dplyr::select(song, songfunction, 
                    web_danc = mean_danc,
                    web_heal = mean_heal,
                    web_baby = mean_baby,
                    web_love = mean_love), 
  fielddat %>% dplyr::select(song, songfunction, 
                      field_danc = mean_danc,
                      field_heal = mean_heal,
                      field_baby = mean_baby,
                      field_love = mean_love), 
  by = c("song", "songfunction"))

# demographic -------------------------------------------------------------

# store demographic info about Ind. and SS cohorts

info <- list()
info$web <- map(c("indx", "natlang", "natlang_fam", "natcountry") %>%
                  set_names(c("n","lang","langfam","country")),
                ~ n_distinct(webraw[[.x]]))
info$field$n <- fieldraw$indx %>% n_distinct


# counts per song ---------------------------------------------------------

counts_per_song_web <- webraw %>% 
  group_by(song) %>% 
  mutate(n_persong = n()) %>% 
  dplyr::select(song, n_persong) %>% distinct()

counts_per_song_field <- fieldraw %>% 
  group_by(song) %>% 
  mutate(n_persong = n()) %>% 
  dplyr::select(song, n_persong) %>% distinct()


# Industrialised society linear models  -----------------------------------

# (regressing behavioural context ratings onto actual song type)

# Function to run linear models and spit out formatted list
mod_extracter <- function(outcome_var, contrasts, target, data_x) {
  out <- list()
  x <- reformulate(c("danc + baby + love + heal - 1"), response = outcome_var)
  mod <- lm(x, data = data_x)
  out$mod <- mod %>% glance
  out$coef <- mod %>% tidy %>% split(.$term)
  lht <- glht(mod, linfct = contrasts)
  out$lht <- lht %>%
    summary(., adjusted("bonferroni")) %>% tidy %>%
    bind_cols(., lht %>% confint %>% tidy %>% dplyr::select(conf.low, conf.high)) %>% 
    mutate(contrast = snakecase::to_snake_case(contrast)) %>% split(.$contrast)
  x <- reformulate(c(target), response = outcome_var)
  out$lht$diff <- lm(x, data = data_x) %>% tidy %>%
    mutate(term = snakecase::to_snake_case(term)) %>% split(.$term)
  
  return(out)
}

mods$danc <- mod_extracter("zm_danc", c("baby - danc = 0", 
                                        "heal - danc = 0",
                                        "love - danc = 0"), 
                           "danc", webdat)
mods$baby <- mod_extracter("zm_baby", c("danc - baby = 0",
                                        "heal - baby = 0",
                                        "love - baby = 0"),
                           "baby", webdat)
mods$heal <- mod_extracter("zm_heal", c("baby - heal = 0",
                                        "danc - heal = 0",
                                        "love - heal = 0"),
                           "heal", webdat)
mods$love <- mod_extracter("zm_love", c("baby - love = 0",
                                        "heal - love = 0",
                                        "danc - love = 0"),
                           "love", webdat)



# SS society linear models ---------------------------------------------------------------

# (regressing behavioural context ratings onto actual song type)

# run linear models for SS cohort and store results

field <- list()

field$danc <- mod_extracter("zm_danc", c("baby - danc = 0", 
                                         "heal - danc = 0",
                                         "love - danc = 0"), 
                            "danc", fielddat)
field$baby <- mod_extracter("zm_baby", c("danc - baby = 0",
                                         "heal - baby = 0",
                                         "love - baby = 0"),
                            "baby", fielddat)
field$heal <- mod_extracter("zm_heal", c("baby - heal = 0",
                                         "danc - heal = 0",
                                         "love - heal = 0"),
                            "heal", fielddat)
field$love <- mod_extracter("zm_love", c("baby - love = 0",
                                         "heal - love = 0",
                                         "danc - love = 0"),
                            "love", fielddat)


# Cross-cohort correlations ------------------------------------------------------------

# Compute correlations between ind and SS cohorts for ratings on each dimension 
cors <- tibble(
  x = c("web_danc", "web_baby", "web_love", "web_heal"),
  y = c("field_danc", "field_baby", "field_love", "field_heal")
) %>% 
  mutate(test = map2(x, y, ~ cor.test(combined_data[[.x]], combined_data[[.y]]) %>% tidy)) %>% 
  unnest(test) %>% 
  mutate(name = str_extract(x, "(?<=_).*")) %>% 
  split(.$name)

# noise ceiling -----------------------------------------------------------

# In line with an anonymous reviewer suggestion we calculated the noise ceilings for the SS and Ind. cohorts, 
# so as to adjust the cross-cohort correlations in Figure 3. 
# However, the high sampling variance in the SS society makes it impossible to calculate noise ceilings for the SS cohort.
# We keep the code here for transparency.
# formula: sqrt(1-mean(standard error per song using raw scores) / var (mean raw score on a dimension per song))

## INDUSTRIALISED COHORT:
# lullabies:
sqrt(1 - (mean(webdat$se_baby^2))/(var(webdat$mean_baby)))
# dance songs:
sqrt(1 - (mean(webdat$se_danc^2))/(var(webdat$mean_danc)))
# healing songs:
sqrt(1 - (mean(webdat$se_heal^2))/(var(webdat$mean_heal)))
# love songs:
sqrt(1 - (mean(webdat$se_love^2))/(var(webdat$mean_love)))


## SMALLER-SCALE COHORT:
# lullabies:
sqrt(1 - (mean(fielddat$se_baby^2))/(var(fielddat$mean_baby)))
# dance songs:
sqrt(1 - (mean(fielddat$se_danc^2))/(var(fielddat$mean_danc)))
# healing songs:
sqrt(1 - (mean(fielddat$se_heal^2))/(var(fielddat$mean_heal)))
# love songs:
sqrt(1 - (mean(fielddat$se_love^2))/(var(fielddat$mean_love)))


# Cross-cohort pairwise tests ------------------------------------------------------------

# Print the results of 16 pairwise t-tests, comparing z-scored ratings in ind. and SS cohorts for each song type x rating dimension. 
# None of the tests approach significance. The code is left here as evidence of this.

tmp1 <- webdat %>% 
  dplyr::select(song, songfunction, w_danc=zm_danc, w_baby=zm_baby, w_heal=zm_heal, w_love=zm_love)
tmp2 <- fielddat %>% 
  dplyr::select(song, f_danc=zm_danc, f_baby=zm_baby, f_heal=zm_heal, f_love=zm_love)
z_comparison <- tmp1 %>% left_join(tmp2, by="song")

for (i in 1:length(unique(z_comparison$songfunction))) {
  
  tmp <- z_comparison[z_comparison$songfunction==unique(z_comparison$songfunction)[i],]
  
  # dance comparison 
  print(paste0(as.character(unique(tmp$songfunction)), " songs ratings on the dancing dimension:"))
  print(t.test(tmp$w_danc, tmp$f_danc))
  
  print(paste0(as.character(unique(tmp$songfunction)), " songs ratings on the soothing a baby dimension:"))
  # baby comparison 
  print(t.test(tmp$w_baby, tmp$f_baby))
  
  print(paste0(as.character(unique(tmp$songfunction)), " songs ratings on the healing dimension:"))
  # healing comparison 
  print(t.test(tmp$w_heal, tmp$f_heal))
  
  print(paste0(as.character(unique(tmp$songfunction)), " songs ratings on the expressing love dimension:"))
  # love comparison 
  print(t.test(tmp$w_love, tmp$f_love))
}

# Internal consistency analyses for Ind. cohort --------------------------------------------------------------------

# regressing accuracy on song type for each of the 28 linguistic subcohorts

webdat_bylang <- fulldata %>% 
  filter(study == "web") %>% 
  group_by(natlang_name, song, songfunction) %>% 
  # averaging scores
  summarize(across(
    c(danc, heal, baby, love, achi, visi),
    ~ mean(.x, na.rm = T),
    .names = "mean_{.col}")) %>% 
  ungroup() %>% 
  # z-scoring
  mutate(across(
    c(mean_danc, mean_baby, mean_heal, mean_love),
    ~ scale(.x),
    .names = "zm_{.col}"
  )) %>% 
  rename_with(~ str_remove(.x, "mean_"), contains("zm")) %>% 
  # dummy variables
  mutate(var = 1, temp = songfunction,
         songfunction = factor(songfunction, levels = c("danc", "heal", "love", "baby"))) %>% 
  pivot_wider(names_from = temp, values_from = var, values_fill = 0)


webdat_bylang <- webdat_bylang %>% drop_na()

get_p.values <- function(song_type) {
  map_dfr(unique(webdat_bylang$natlang_name) %>% set_names, ~ {
    x <- reformulate("danc + baby + love + heal - 1", response = str_c("zm_", song_type))
    m <- lm(x, webdat_bylang %>% filter(natlang_name == .x))
    tidy(m) %>% filter(term == song_type) %>% dplyr::select(p.value) 
  }, .id = "country")
}

danc_df <- get_p.values("danc")
baby_df <- get_p.values("baby")
heal_df <- get_p.values("heal")
love_df <- get_p.values("love")



# PCA ---------------------------------------------------------------------

# Mostly based on https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
# and http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# FF PCA web ----------------------------------------------------------

tmp <- webdat %>% 
  mutate(song = paste(songfunction, song, sep = "_")) %>% 
  dplyr::select(song, mean_danc:mean_love)%>%
  column_to_rownames("song")

pca_res <- prcomp(tmp, scale=T) 
summary(pca_res)
# pca_res$x
#pca_res$center

# scree plot
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 80))

eig.val_web <- get_eigenvalue(pca_res)
eig.val_web

pca_res$rotation 

# FF PCA field ----------------------------------------------------------

tmp_field <- fielddat %>% 
  mutate(song = paste(songfunction, song, sep = "_")) %>% 
  dplyr::select(song, mean_danc:mean_love)%>%
  column_to_rownames("song")

pca_res_field <- prcomp(tmp_field, scale=T)
summary(pca_res_field)
pca_res_field$x[,1:3]
pca_res_field$center

var_explained <- pca_res_field$sdev^2/sum(pca_res_field$sdev^2)
var_explained

# scree plot
var_explained <- pca_res_field$sdev^2/sum(pca_res_field$sdev^2)
var_explained
fviz_eig(pca_res_field, addlabels = TRUE, ylim = c(0, 80))
eig.val_field <- get_eigenvalue(pca_res_field)
eig.val_field

pca_res_field$rotation<- pca_res_field$rotation*(-1) # manually change sign to aid interpretability with field results



# regress pca dimensions on song type
pc_mod_dat <- as.data.frame(pca_res$x) %>% 
  rownames_to_column("song") %>% 
  separate(song,"type") %>% 
  mutate(danc = ifelse(type == "danc", 1,0),
         baby = ifelse(type == "baby", 1,0),
         love = ifelse(type == "love", 1,0),
         heal = ifelse(type == "heal", 1,0))

pc_mod_dat_field <- as.data.frame(pca_res_field$x*(-1)) %>% 
  rownames_to_column("song") %>% 
  separate(song,"type")%>% 
  mutate(danc = ifelse(type == "danc", 1,0),
         baby = ifelse(type == "baby", 1,0),
         love = ifelse(type == "love", 1,0),
         heal = ifelse(type == "heal", 1,0))

# you can ignore the second and third arguments in the mod_extracter below: 
# these arguments are for comparing each song type to another. We're not interested in these pairwise 
# analyses here. Rather, I'm testing whether each song type differs from the "midpoint" of a PC dimension.

mods$pc1 <- mod_extracter("PC1", c("baby - danc = 0", 
                                   "heal - danc = 0",
                                   "love - danc = 0"), 
                          "danc", pc_mod_dat)


mods$pc2 <- mod_extracter("PC2", c("baby - danc = 0", 
                                   "heal - danc = 0",
                                   "love - danc = 0"), 
                          "danc", pc_mod_dat)

mods$pc3 <- mod_extracter("PC3", c("baby - danc = 0", 
                                   "heal - danc = 0",
                                   "love - danc = 0"), 
                          "danc", pc_mod_dat)

mods$pc4 <- mod_extracter("PC4", c("baby - danc = 0", 
                                   "heal - danc = 0",
                                   "love - danc = 0"), 
                          "danc", pc_mod_dat)

mods$pc1_field <- mod_extracter("PC1", c("baby - danc = 0", 
                                         "heal - danc = 0",
                                         "love - danc = 0"), 
                                "danc", pc_mod_dat_field)

mods$pc2_field <- mod_extracter("PC2", c("baby - danc = 0", 
                                         "heal - danc = 0",
                                         "love - danc = 0"), 
                                "danc", pc_mod_dat_field)

mods$pc3_field <- mod_extracter("PC3", c("baby - danc = 0", 
                                         "heal - danc = 0",
                                         "love - danc = 0"), 
                                "danc", pc_mod_dat_field)

mods$pc4_field <- mod_extracter("PC4", c("baby - danc = 0", 
                                         "heal - danc = 0",
                                         "love - danc = 0"), 
                                "danc", pc_mod_dat_field)


# Exploratory analysis models (cultural proximity analyses) ---------------------------------------------

# fit mixed-effects models for each of the song-types
ran_mods <- webraw %>% 
  pivot_longer(names_to = "type", values_to = "z", cols = c(zm_danc, zm_baby, zm_heal, zm_love), names_prefix = "zm_") %>% 
  split(.$type) %>% 
  map(~ lmer(z ~ songfunction - 1 + (1|indx) + (1|song) + (1|cohort), data = .) %>% tidy %>% 
        mutate(term = str_remove(term, "songfunction")) %>% 
        split(.$term))

# model on raw scale for comparison
ran_mods$raw <- lmer(danc ~ songfunction - 1 + (1|indx) + (1|song) + (1|cohort), data = webraw) %>% tidy %>%
  mutate(term = str_remove(term, "songfunction")) %>%  split(.$term)


# function to compute mixed-effects model + pull marginal effects estimates
prox_function <- function(prox_metric) {
  x <- reformulate(c(prox_metric, "(1|song)", "(1|indx)"), response = "score")
  
  webraw %>% 
    pivot_longer(names_to = "type", values_to = "score", cols = c(danc, baby, heal, love)) %>% 
    filter(type == songfunction) %>% 
    split(.$type) %>% 
    map(~ {
      m <- lmer(x, data = .x)
      list(
        # Compute marginal effects
        effects = ggpredict(m, prox_metric) %>% tibble,
        # return full model
        mod = m %>% tidy %>% 
          # cleaning up names
          mutate(term = str_remove(term, prox_metric),
                 term = str_replace_all(term, "[^[:alnum:]]", "")) %>% 
          split(.$term)
      )
    })
}

prox_mods <- prox_function("langshare")
geo_mods <- prox_function("geoshare_sub")


# bootstrap cors ----------------------------------------------------------

# This chunk of code uses a previously generated bootstrapped dataset found in results/bootstrap_res.csv.
# The results of this analysis are in S2
boot <- read.csv(here("results", "bootstrap_res.csv"))

boot_cors <- boot %>% 
  rename_with(~ str_extract(.x, ".*(?=_)"), everything()) %>% 
  map(~ t.test(.x, mu = 0, alternative = "greater"))


# more boots --------------------------------------------------------------

# test hypothesis that tru correlation > 0 for each song type
boots <- read_csv(here("results","bootstrap_res2.csv")) %>% 
  map(~ t.test(.x, mu = 0, alternative = "greater") %>% tidy) 


# save everything ---------------------------------------------------------


save(list = ls(), file = here("results", "analysis.RData"))




