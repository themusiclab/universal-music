# Code written by Courtney Hilton, 2023
#
# libraries ---------------------------------------------------------------

library(pacman)
p_load(
  here,
  tidymodels,
  tidyverse
)

load(here("results", "analysis.RData"))
set.seed(42)

# building data for LASSO ----------------------------------------------------------------

### Expert annotations 

# Features to keep
features.expert <- c( 
  'tempo_adj',
  'macrometer_ord',
  'syncopate',
  'accent',
  'dynamics',
  'ritard_accel',
  'micrometer_duple',
  'micrometer_triple',
  'macrometer_duple',
  'macrometer_triple',
  'variation_rhythmic',
  'variation_melodic',
  'ornament',
  'vibrato',
  'tension',
  'scale_quality_minor'
)

# load expert ratings + pool certain variables
disco.expert <- read_csv(here::here('data', 'NHSDiscography_Annotate.csv')) %>% 
  mutate(
    # Pooling MICROMETER
    micrometer_duple = micrometer %in% c('Duple', 'Both duple and triple'),
    micrometer_triple = micrometer %in% c('Triple', 'Both duple and triple'),
    # Pooling MACROMETER
    macrometer_duple = (rowSums(dplyr::select(., matches("macrometer_([2468]{1}|\\d[02468]$)"))) > 0) |
      ifelse(macrometer_other != ".", as.numeric(macrometer_other) %% 2 == 0, FALSE),
    macrometer_triple = (rowSums(dplyr::select(., matches("macrometer_([369]{1}|1[258]$)"))) > 0) |
      ifelse(macrometer_other != ".", as.numeric(macrometer_other) %% 3 == 0, FALSE),
    # Pooling MELODIC VARIATION
    variation_melodic = repeat_vary %in% c('Melodic variation', 'Rhythmic and melodic variation'),
    # Pooling RHYTHMIC VARIATION
    variation_rhythmic = repeat_vary %in% c('Rhythmic variation', 'Rhythmic and melodic variation'),
    # DYNAMICS
    dynamics = dynamics %in% c('Gets louder', 'Multiple dynamics', 'Quiets down'),
    # TENSION/RELEASE
    tension = rowSums(dplyr::select(.,tension_melody, tension_harmony, tension_rhythm, tension_motif, tension_accent, tension_dynamic)),
    # TEMPO VARIATION
    ritard_accel = ritard %in% c('Slows down', 'Speeds up', 'Speeds up and slows down'),
    # SCALE QUALITY (MINOR)
    scale_quality_minor = ifelse(scale_quality == 'Unknown', NA, scale_quality == 'Minor')
  ) %>% 
  dplyr::select(song, features.expert) %>% 
  group_by(song) %>% 
  dplyr::summarise(across(everything(), mean, na.rm=T))

#### Expert transcriptions 

# selected transcription features
features.transcription <- c(
  'mean_interval',
  'distance_btwn_modal_intervals',
  'common_intervals_count',
  'stepwise_motion',
  'melodic_thirds',
  'duration_of_melodic_arcs',
  'size_of_melodic_arcs',
  'rel_strength_top_pitchcls',
  'interval_btwn_strongest_pitchcls',
  'pitch_class_variety',
  'range',
  'note_density',
  'average_note_duration',
  'modal_interval_prevalence',
  'rel_strength_modal_intervals',
  'amount_of_arpeggiation',
  'direction_of_motion',
  'modal_pitchcls_prev',
  'initial_tempo',
  'quality'
)

# automatically extracted features from raw audio
disco.transcription <- read_csv(here::here('data', 'NHSDiscography_TranscriptionFeatures.csv')) %>% 
  # select chosen features
  dplyr::select(song, all_of(features.transcription))

#### Metadata 

disco.meta <- read_csv(here::here('data', 'NHSDiscography_Metadata.csv')) %>% 
  mutate(song = abs(parse_number(song))) %>% 
  dplyr::select(song, type, 5:10, location_modern) %>% 
  group_by(song) %>% 
  # extracting country information
  mutate(country = tail(unlist(str_split(location_modern, ", ")), n=1))

####  Z-transformed mean listener ratings on each behavioural dimension
song_ratings <- webdat %>% 
  dplyr::select(song,starts_with("zm")) %>% 
  pivot_longer(cols=starts_with("zm"),names_to = "response", values_to = "web")

song_ratings_field <- fielddat %>% 
  dplyr::select(song,starts_with("zm")) %>% 
  pivot_longer(cols=starts_with("zm"),names_to = "response", values_to = "field")

song_ratings <- song_ratings %>% 
  left_join(song_ratings_field, by = c("song", "response")) %>% 
  mutate(response = gsub("zm_","",response)) 


##### Combine all musical features and listener ratings 
song_data <- plyr::join_all(list(disco.expert,
                                 disco.transcription,
                                 disco.meta,
                                 song_ratings),
                            by = "song", type = "inner")

### prep data for LASSO

# ratings on a given scale for each song, per cohort
responses <- song_data %>% 
  dplyr::select(song, type, response,web, field)

# musical feature matrix
disco.nocontext.mat <- song_data %>% 
  dplyr::select(tempo_adj:quality) %>% distinct() %>% 
  as.matrix.data.frame() %>% 
  scale()


# defining lasso feature selection function -------------------------------


run_lasso <- function(data_x, cv_repeats = 2, secondary_regularization) {
  
  output <- list()
  
  # Preprocessing recipe
  feature_recipe <- recipe(pct ~ ., data = data_x) %>%
    step_zv(all_numeric(), -all_outcomes()) %>%
    step_normalize(all_numeric(), -all_outcomes())
  
  # Add to workflow
  wf <- workflow() %>% 
    add_recipe(feature_recipe)
  
  # Specify LASSO model
  lasso_spec <- linear_reg(mode = "regression", penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", grouped = FALSE)
  
  doParallel::registerDoParallel()
  
  lasso_grid <- tune_grid(
    wf %>% add_model(lasso_spec),
    resamples = vfold_cv(data_x, v = 10, repeats = cv_repeats),
    grid = grid_regular(penalty(range = c(-10, -0.5)), levels = 60),
    metrics = metric_set(rmse, yardstick::rsq)
  )
  
  output$lambda <- select_by_one_std_err(lasso_grid, desc(penalty), metric = "rmse")
  
  # final lasso with selected lambda
  final_lasso <- finalize_workflow(
    wf %>% add_model(lasso_spec),
    output$lambda
  )
  
  lasso_fit <- final_lasso %>% 
    fit(data_x)
  
  output$coefs <- lasso_fit %>% 
    pull_workflow_fit() %>% 
    tidy() %>% 
    filter(
      # secondary regularization to err on side of fewer features
      abs(estimate) > secondary_regularization,
      term != "(Intercept)") %>% 
    arrange(desc(abs(estimate))
    )
  
  return(output)
}


# Running feature selection -----------------------------------------------


# number of cross-validation repeats to do in each model
cv_repeats <- 10

lasso_data <- responses |> 
  mutate(across(c(web, field), as.vector)) |> 
  pivot_wider(names_from = response, values_from = c(web, field))

selected_features <- map(c("danc", "heal", "baby", "love") %>% set_names, ~ {
  output <- list()
  # run web model
  output$web <- lasso_data %>% 
    dplyr::select(matches(str_c("web_", .x))) %>% 
    rename(pct = 1) %>% 
    bind_cols(., as_tibble(disco.nocontext.mat)) %>% 
    run_lasso(., cv_repeats, 0.02)
  
  # run field model
  output$field <- lasso_data %>% 
    dplyr::select(matches(str_c("field_", .x))) %>% 
    rename(pct = 1) %>% 
    bind_cols(., as_tibble(disco.nocontext.mat)) %>% 
    run_lasso(., cv_repeats, 0.02)
  
  output$feats <- union(output$web$coefs$term, output$field$coefs$term)
  
  return(output)
})

# Regress listener ratings onto selected features -----------------------------------------------

reg_results <- list()
for (type_x in c("danc", "heal", "baby")) {
  for (data_x in c("web", "field")) {
    predictors <- paste0(as.vector(selected_features[[type_x]]$feats), collapse = " + ")
    lasso_feature_reg <- reformulate(termlabels = predictors, response = data_x)

    reg_results[[type_x]][[data_x]] <- summary(lm(lasso_feature_reg, song_data[song_data$response==type_x,])) %>% tidy()

  }
}


# saving ------------------------------------------------------------------


save(selected_features, cv_repeats, reg_results, file = here("results", "lasso_analyses.RData"))

