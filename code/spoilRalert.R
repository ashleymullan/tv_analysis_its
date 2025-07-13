# Google search term: "will they won't they couples" 			
couples = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1R8WJhW0E-5_SnKRsT66lw4UuY45dezcGFVhQgJG2LIA/edit?usp=sharing") |> 
  data.frame()

# Summarize with unique couples 
couples_unique = couples |> 
  dplyr::group_by(Show, Couple) |> 
  dplyr::summarize(Sources = dplyr::n()) |> 
  data.frame()

# Visualize unique couples 
library(ggplot2)
couples_unique |> 
  dplyr::arrange(desc(Sources)) |> 
  dplyr::slice(1:10) |> 
  dplyr::mutate(Couple_Show = paste0(Couple, "\n(", Show, ")")) |> 
  ggplot(aes(x = reorder(Couple_Show, Sources), y = Sources)) + 
  geom_bar(stat = "identity", 
           fill = ThemePark::barbie_theme_colors["medium"]) + 
  ThemePark::theme_barbie() + 
  coord_flip() + 
  xlab("Couple (Show)") + 
  ylab("Number of Lists In Which They Appear") + 
  ggtitle("Top 10 Most Talked About Will They/Won't They Couples on TV")
