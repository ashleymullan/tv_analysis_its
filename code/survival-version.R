
library(googlesheets4) #for data acquirement
library(ggplot2) #for data viz
library(dplyr) #for data wrangling
library(survival) #for analysis
library(ggsurvfit) #for survival curves

local_dir <- "/Users/ashleymullan/Documents/" #change me to your local machine
couples_path <- "tv_analysis_its/data/couples.csv"  #fixed repo path to couples data
ratings_path <- "tv_analysis_its/data/ratings.csv"  #fixed repo path to ratings data


original_couples <- read.csv(paste0(local_dir, couples_path)) #read in couples data
original_ratings <- read.csv(paste0(local_dir, ratings_path)) #read in ratings data

seen <- c("Jim & Pam", "Jess & Nick", "Rachel & Ross", "Derek & Meredith",
          "Barney & Robin", "Ben & Leslie", "Jake & Amy", "Robin & Ted",
          "Cece & Schmidt", "Damon & Elena", "Cory & Topanga", "Fry & Leela",
          "Leonard & Penny", "Chidi & Eleanor", "Kermit & Miss Piggy",
          "Dwight & Angela", "Damon & Elena", "Mike & Phoebe", "Monica & Chandler",
          "April & Jackson", "Lexie & Mark", "Lucifer & Chloe", "Ned & Suzie",
          "Andy & April", "Ann & Chris", "Caleb & Hanna", "Isabelle & Simon", "Donna & Harvey",
          "Kara & Mon-El", "Amy & Sheldon", "Barry & Iris", "Kelly & Ryan",
          "Jan & Michael", "Caroline & Stefan", "Elena & Stefan")


couples <- original_couples |> #subset to valid candidate couple rows
  filter(Exclude_Why %in% c("",
                            "First Kiss in Season 1 Episode 1",
                            "Haven't Kissed")) |>
  filter(Couple %in% seen)


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

seen_with_ratings <- ratings_couples |>
  group_by(Couple) |> tally() |>
  filter(n > 2) |> pull(Couple)

rc_seen <- ratings_couples |> filter(Couple %in% seen_with_ratings)

#TO DO: SURVIVAL ANALYSIS

survfit(Surv(show_couple_id, after_first_kiss) ~ Couple, data = rc_seen) |>
  ggsurvfit() +
  labs(
    x = "Episodes",
    y = "Overall survival probability"
  )

cox <- coxph(Surv(show_couple_id, after_first_kiss) ~ Couple, data = rc_seen)
cox_coefs <- summary(cox)$coefficients[,c(1,3)] |> as.data.frame() |> rename(se = "se(coef)")
cox_coefs |> mutate(lb = coef - 1.96 * se,
                    est = coef,
                    ub = coef + 1.96 * se)
