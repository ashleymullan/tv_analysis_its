
library(googlesheets4) #for data acquirement
library(ggplot2) #for data viz
library(dplyr) #for data wrangling

local_dir <- "/Users/ashleymullan/Documents/" #change me to your local machine
couples_path <- "tv_analysis_its/data/couples.csv"  #fixed repo path to couples data
ratings_path <- "tv_analysis_its/data/ratings.csv"  #fixed repo path to ratings data


original_couples <- read.csv(paste0(local_dir, couples_path)) #read in couples data
original_ratings <- read.csv(paste0(local_dir, ratings_path)) #read in ratings data


couples <- original_couples |> #subset to valid candidate couple rows
  filter(Exclude_Why %in% c("",
                            "First Kiss in Season 1 Episode 1",
                            "Haven't Kissed"))


ratings_couples <- couples |> #join ratings to couples (required to refine episode counts)
    left_join(original_ratings,
              by = join_by(Show == Show),
              relationship = "many-to-many")


ratings_couples <- ratings_couples |> #create kiss markers
    group_by(Show, Couple) |>
    mutate(show_couple_id = 1:n(),
           is_first_kiss_ep = Season == FirstKiss_Season & Episode == FirstKiss_Episode,
           first_kiss_id = min(ifelse(test = is_first_kiss_ep, yes = show_couple_id, no = 9999)),
           after_first_kiss = show_couple_id > first_kiss_id) |>
    dplyr::select(-is_first_kiss_ep) |>
    mutate(eps_since_kiss = show_couple_id - first_kiss_id) |>
    ungroup()


#TO DO: SURVIVAL ANALYSIS



