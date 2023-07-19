# Map app created by Courtney B. Hilton, 2023
# libraries ---------------------------------------------------------------


library(shiny)
library(tidyverse)
library(here)


# preprocess data ---------------------------------------------------------

load(here("results", "plot_data.RData"))

# cleaning up to have more presentable names
plot_data <- plot_data |> 
  mutate(
    songfunction = case_when(
      songfunction == "danc" ~ "Dance Song",
      songfunction == "heal" ~ "Healing Song",
      songfunction == "love" ~ "Love Song",
      songfunction == "baby" ~ "Lullaby",
      
    )
  ) |> 
  rename_with(.cols = contains("mean"), .fn = \(.x) {
    case_when(
      str_detect(.x, "danc") ~ "... for dancing",
      str_detect(.x, "heal") ~ "...to heal illness",
      str_detect(.x, "love") ~ "...to express love for another person",
      str_detect(.x, "baby") ~ "...to soothe a baby"
    )
  }) |> 
  # rescaling smaller scale society scores to be on 1-4 scale (to make plotting easier)
  mutate(across(c("... for dancing", "...to heal illness", "...to soothe a baby", "...to express love for another person"), \(.score) {
    if_else(study == "field", .score * (4/3), .score)
  }))

# get list of all NHS discography song files
NHS_song_files <- tibble(filename = list.files(here("shiny_app", "songs"))) |> 
  mutate(song = parse_number(filename))

# combine with plot data
plot_data <- plot_data |> 
  left_join(NHS_song_files, join_by(song))

# load NHS discography metadata
nhs_metadata <- read_csv(here("shiny_app", "metadata", "NHSDiscography_Metadata.csv"))

# combine few useful things with plot_data
plot_data <- plot_data |> 
  left_join(nhs_metadata |> 
              select(song, culture, location_modern, year), join_by(song))

# defining colour scheme
colour_scheme <- c("#6f9acc", "#f27553", "#fccc54", "#44bb74")
colour_scheme_named <- c("Dance Song" = "#6f9acc", "Healing Song" = "#f27553", "Love Song" = "#fccc54", "Lullaby" = "#44bb74")

bpca_data <- read_csv(here("shiny_app", "metadata", "disco_bpca_scores_highlighted.csv")) |> 
  select(-song) |> 
  rename(song = indx) |> 
  # adding the FF guessing data
  left_join(plot_data, join_by(song), multiple = "all") |> 
  mutate(
    study = if_else(study == "web", "Industrialised", "Smaller-scale")
  )

# extract: accent, strength of macrometer, changes in tempo, tempo, rhythmic variation, average note duration, note density

# expert listener annotations
# nhs_annotated <- read_csv(here("shiny_app", "metadata", "NHSDiscography_Annotate.csv")) |> 
#   select(song, "Tempo" = tempo_adj, "Accent" = accent, "Strength of Macrometer" = macrometer_ord, "Changes in Tempo" = ritard_accel, "Rhythmic Variation" = variation_rhythmic)
# 
# feature_options <- c("Accent", "Strength of Macrometer", "Changes in Tempo", "Tempo", "Rhythmic Variation", "Average Note Duration", "Note Density")


# Shiny app ---------------------------------------------------------------


# these are the different options the user can choose between
song_types <- c("... for dancing", "...to heal illness", "...to soothe a baby", "...to express love for another person")
# setting the path for the NHS song files
addResourcePath(prefix = "songs", directoryPath = here("shiny_app", "songs"))

# creating the UI element
ui <- fluidPage(
  #Navbar structure for UI
  navbarPage("Interactive figures",
             tabPanel("Listener ratings", fluid = TRUE, icon = icon("star"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('songfunc', 'Song function', song_types),
                          HTML(str_c("<p>This is an interactive figure that accompanies the 'Mutual intelligibility in musical communication' paper.<br><br>", 
                                     "It shows the average song intuitions for listeners from smaller-scale societies (y-axis) and industrialised societies (x-axis), ",
                                     "allowing you to compare between them to see how similar their intutions are. ",
                                     "Each point represents one of the songs people listened to. If you click on a point, you can listen to the song and see further details. ",
                                     "<br><br>The selector above lets you toggle between the four different song functions we survey people on. ",
                                     "For example, when you have the '...for dancing' song function selected, the figure shows the average rating people gave for whether they thought the song was 'used for dancing'.</p>")),
                          tags$head(tags$style("#scatter{height:90vh !important}"))
                        ),
                        mainPanel(
                          plotOutput("scatter", click = "plot_click")
                        )
                      )
             ),
             tabPanel("Acoustic features", fluid = TRUE, icon = icon("music"),
                      # acoustic features plot
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('songfunc2', "Song function", song_types),
                          selectInput("sample", "Sample", c("Industrialised", "Smaller-scale")),
                          checkboxGroupInput("songtype", "Song Type", 
                                             choices = c("Dance", "Lullaby", "Love", "Healing"), selected = c("Dance", "Lullaby", "Love", "Healing")),
                          HTML(str_c("<p>",
                                     "This interactive figure lets you explore the relation between the acoustic features of the songs and people's ratings about song function.",
                                     "<br><br>",
                                     "Like before, each point represents a song you can click on to listen and learn more about it. ",
                                     "The 'Song function' selector above also lets you toggle between the different song functions people gave ratings on. ",
                                     "The 'Sample' selector lets yout toggle between the Industralised society and Smaller-scale society cohorts.", 
                                     "<br><br>",
                                     "The x-y axis represents the first two components of a Bayesian Principal Components analysis of all the features, ",
                                     "originally conducted in <a href='https://science.sciencemag.org/content/366/6468/eaax0868' target='_blank'>Mehr et al., 2019, Science</a>. ",
                                     "The further each point is to the right along the horizontal x-axis, the more melodically complex a song is. ",
                                     "Similarly, the further upward a point is along the vertical y-axis, the more rhythmically complex a song is. ",
                                     "<br><br>",
                                     "Finally, while we encourage you to explore all these options, we also caution that the 'Smaller-scale' society ratings have ",
                                     "much smaller sample sizes of ratings per song. So interpretation needs to be taken with caution.",
                                     "</p>")),
                          tags$head(tags$style("#bpca{height:90vh !important}"))
                        ),
                        mainPanel(
                          plotOutput("bpca", click = "plot_click2")
                        )
                      )
             )
  )
)

# creating the server element
server <- function(input, output) {
  # dynamically creates plot dataset based on which song-type user chooses
  data <- reactive({
    plot_data |> 
      select(song, study, input$songfunc, filename,
             songfunction, culture, location_modern, year) |> 
      pivot_wider(names_from = study, values_from = input$songfunc)
  })
  
  data2 <- reactive({
    bpca_data |> 
      rename(songfunc = input$songfunc2) |> 
      filter(study == input$sample, type %in% input$songtype)
  })
  
  # creates ggplot
  output$scatter <- renderPlot({
    ggplot(data(), aes(web, field, colour = songfunction)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        y = "Smaller-scale society ratings",
        x = "Industrialised society ratings"
      ) +
      scale_y_continuous(breaks = seq(1,4,3/2), limits = c(1,4), labels = seq(1,3,1),
                         expand = c(0,0)) + 
      scale_x_continuous(expand = c(0,0), limits = c(1,4)) +
      scale_colour_manual(values = colour_scheme) + 
      guides(colour = guide_legend(override.aes = list(size = 5))) + 
      coord_fixed(ratio = 1) +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_blank(), 
        legend.position = "bottom"
      )
  })
  
  # create bpca plot
  output$bpca <- renderPlot({
    ggplot(data2(), aes(bpca_1, bpca_2, size = songfunc, colour = songfunction)) +
      geom_point(alpha = 0.6) +
      labs(
        y = "PC2: Rhythmic Complexity",
        x = "PC1: Melodic Complexity"
      ) +
      lims(
        x = c(-4,2),
        y = c(-3,2)
      ) + 
      scale_size_continuous(range = c(1, 9)) +
      scale_colour_manual(values = colour_scheme_named) + 
      guides(colour = guide_legend(override.aes = list(size = 5), order = 1)) + 
      theme_minimal() + 
      coord_fixed(ratio = 1) +
      theme(
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_blank(), 
        legend.position = "bottom"
      )
  })
  
  # monitors for when user clicks on specific song and then launches modal
  observeEvent(input$plot_click, {
    point <- nearPoints(data(), input$plot_click)
    if (nrow(point) > 0) {
      showModal(modalDialog(
        title = "Song Information",
        HTML(str_c("<p>This song is a ", point$songfunction, " song from the ", point$culture, " culture, recorded in ", point$location_modern, "<br>",
              "<br><audio controls src='",
              "songs/", point$filename,
              "' type='audio/mp3'></audio>")),
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$plot_click2, {
    point2 <- nearPoints(data2(), input$plot_click2)
    if (nrow(point2) > 0) {
      showModal(modalDialog(
        title = "Song Information",
        HTML(str_c("<p>This song is a ", point2$type, " song from the ", point2$culture, " culture, recorded in ", point2$location_modern, "<br>",
                   "<br><audio controls src='",
                   "songs/", point2$filename,
                   "' type='audio/mp3'></audio>")),
        easyClose = TRUE
      ))
    }
  })
}

# launching the app
shinyApp(ui = ui, server = server)
