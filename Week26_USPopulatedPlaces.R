# Challenge: # TidyTuesday 2023 Week-26
# Data:      US Populated Places
# Author:    Devashree Madhugiri
# Date:      2023-06-28
#------------------------------------------
# Importing libraries
#-------------------------------------------------
library(usmap)
library(tidyverse)
library(tibble)
library(showtext)
library(extrafont)
library(thematic)
library(cowplot)
#-------------------------------------------------
#Importing Data
#-------------------------------------------------
place_names <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
glimpse(place_names)
#-------------------------------------------------
# Custom Theme
#-------------------------------------------------
font_add_google(name = "Lobster",  family = "lobster")
theme_dsm <- function() {
  theme_minimal() %+replace%
    theme(text = element_text(family = "lobster", , face = "bold"),
          plot.background = element_rect(fill = "#f8ffe5", colour = "#f8ffe5"),
          panel.background = element_rect(fill = "#f8ffe5", colour = "#f8ffe5"),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 2, family = "pacifico"),
          legend.position = "bottom",axis.ticks = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_blank(),axis.text.y = element_blank(),
          legend.title = element_text(size=15),
          legend.spacing.x = unit(0.05, 'cm'),
          panel.border = element_rect(colour = "gray40", fill=NA, size=0.5),
          
  )
}
#-------------------------------------------------
# Feature counts from Original data
#-------------------------------------------------
statepop<- place_names %>%
  group_by(state_name)%>%
  summarise(feature_count = n()) %>%
  arrange(desc(feature_count))
print(statepop)

#-------------------------------------------------
# Get centroids
#-------------------------------------------------
centroid_labels <- usmapdata::centroid_labels("states")

names(centroid_labels)[names(centroid_labels) == "full"] <- "state_name"
centroid_labels

#-------------------------------------------------
# Join data to centroids
#-------------------------------------------------
data_labels <- merge(centroid_labels, statepop, by = "state_name")
data_labels

#-------------------------------------------------
#Arranging data and dropping columns
#-------------------------------------------------
new_df<-data_labels %>%
  arrange(desc(feature_count))
new_df <- subset(new_df, select = -c(x, y))

#-------------------------------------------------
#Creating a tibble
#-------------------------------------------------

df<-as_tibble(new_df)
df

#-------------------------------------------------
#Reordering columns
#-------------------------------------------------
coast<-df %>% relocate(state_name, .after=abbr)

#-------------------------------------------------
# Plotting East Coast
#-------------------------------------------------
east_coast <- plot_usmap(data = coast, labels = TRUE,label_color = "white",
                         values = "feature_count", 
                         color = "white",
                         include = c("CT", "DE", "FL", "GA", "ME", "MD", "MA",
                                     "NH", "NJ", "NY", "NC", "RI", "SC", "VA")) +
  scale_fill_continuous(low = "#00a8e8", high = "#390099", 
                        label = scales::comma) +
  guides(fill = guide_legend(title="East Coast")) +
  theme_dsm()

#-------------------------------------------------
# Plotting West Coast
#-------------------------------------------------
west_coast <- plot_usmap(data = coast, labels = TRUE,label_color = "white",
                         values = "feature_count", 
                         color = "white",
                         include = c("CA", "OR", "WA","AK", "HI")) +
  guides(fill=guide_legend(title="West Coast")) + theme_dsm() +
  scale_fill_continuous(low = "#ff9770", high = "#89023e", 
                        label = scales::comma)
#-------------------------------------------------
# Combining East and West coast plots
#-------------------------------------------------
combined_plot<-plot_grid(west_coast,east_coast)
# Adding Title
title <- ggdraw() + draw_label(
  "Feature counts across coastal areas of US",
  fontfamily = "lobster",
  fontface = 'bold',
  x = 0.5,y=0.5,
  hjust = 0.5,vjust=0.5,size = 24
) 
title1 <- ggdraw() + draw_label(
  "#TidyTuesday | Data Viz by: @Dev_Subhash21 | Source:National Map Staged Products Directory",
  fontfamily = "mono",
  fontface = 'bold',
  x = 0.5,y=0.5,
  hjust = 0.5,vjust=0.5,size = 9
) 
#-------------------------------------------------
# Combining title and plot
#-------------------------------------------------
final_plot<-plot_grid(title, combined_plot,title1, ncol=1, rel_heights = c(0.05, 0.9,0.05),
                      align_margin="first")
final_plot + theme_dsm()
