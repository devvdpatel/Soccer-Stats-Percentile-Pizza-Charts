# Installing/loading necessary packages
# install.packages("devtools")
library(devtools)

# For scraping data from FBref
#devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

# For ggplot, dplyr, and other visualization tools
#install.packages("tidyverse")
library(tidyverse)

# For sorting within ggplot
library(forcats)

# Miscellaneous
library(glue)

# Load ggplot2 and dplyr
library(ggplot2)
library(dplyr)

# ---------------------------
# Getting and preparing data
# ---------------------------
player_df <- fb_player_scouting_report("https://fbref.com/en/players/d9565625/Diogo-Dalot", pos_versus = "primary")

# Print the statistic column to choose metrics for radar plot
print(player_df$Statistic)

# Select top 15 metrics for potential RWB
df_metrics <- player_df[c(13, 14, 51, 48, 68, 47, 49, 83, 94, 87, 91, 114, 102, 132, 133), ]

# Create a separate column for attribute type
df_metrics <- df_metrics %>%
  mutate(stat = case_when(
    Statistic %in% c(
      "Progressive Carries",
      "Progressive Passes",
      "Crosses into Penalty Area",
      "Key Passes",
      "Shot-Creating Actions",
      "xA: Expected Assists",
      "Passes into Final Third"
    ) ~ "Attacking",
    
    Statistic %in% c(
      "Tackles Won", 
      "Interceptions",
      "Dribbles Tackled",
      "Blocks"
    ) ~ "Defending",
    
    Statistic %in% c(
      "Carries into Final Third",
      "Touches (Att 3rd)",
      "Ball Recoveries",
      "Aerials Won"
    ) ~ "Work Rate",
    
    TRUE ~ NA_character_  # Default case for unlisted statistics
  ))

# Remove rows with missing values in Statistic, Percentile, and stat
df_metrics <- df_metrics %>% drop_na(Statistic, Percentile, stat)



# --------------------------------
# Creating the Radar Chart
# --------------------------------
ggplot(df_metrics, aes(fct_reorder(Statistic, stat), Percentile)) +                       
  geom_bar(aes(y=100, fill=stat), stat="identity", width=1, colour="white", alpha=0.5) +          
  geom_bar(stat="identity", width=1, aes(fill=stat), colour="white") +                    
  coord_polar() +                                                                       
  geom_label(aes(label=Percentile, fill=stat), size=2, color="white", show.legend = FALSE) +     
  scale_fill_manual(values=c(
    "Attacking" = "#1A78CF",
    "Defending" = "#FF9300",
    "Work Rate" = "#D70232"
  )) +                                                             
  scale_y_continuous(limits = c(-10,100)) +                                                
  labs(fill="", caption = "Data via FBref.com", title=unique(df_metrics$Player)[1]) +                                                  
  theme_minimal() +                                                                     
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(family="Arial"),  # Using a common font
    plot.title = element_text(hjust=0.5),
    plot.caption = element_text(hjust=0.5, size=6),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

temp <- (360/(nrow(df_metrics))/2)
myAng <- seq(-temp, -360+temp, length.out = nrow(df_metrics))
ang<-ifelse(myAng < -90, myAng+180, myAng)
ang<-ifelse(ang < -90, ang+180, ang)

df_metrics$Statistic <- gsub(" ", "\n", df_metrics$Statistic)

ggplot(df_metrics, aes(fct_reorder(Statistic, stat), Percentile)) +                       
  geom_bar(aes(y=100, fill=stat), stat="identity", width=1, colour="white", alpha=0.5) +          
  geom_bar(stat="identity", width=1, aes(fill=stat), colour="white") +                    
  coord_polar() +                                                                       
  geom_label(aes(label=Per90, fill=stat), size=2, color="white", show.legend = FALSE) +     
  scale_fill_manual(values=c(
    "Attacking" = "#1A78CF",
    "Defending" = "#FF9300",
    "Work Rate" = "#D70232"
  )) +                                                             
  scale_y_continuous(limits = c(-10,100)) +                                                
  labs(fill="", 
       caption = "Data via FBref.com", 
       title = glue("{df_metrics$Player[1]} | Manchester United"), 
       subtitle = glue::glue("{df_metrics$season} | Compared to midfielder in 
                             top 5 comp. | Stats per 90")) +
  theme_minimal() +                                                                     
  theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=10, angle = ang),
        text = element_text(family="Arial"), 
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5, size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)
  ) 

ggsave("dalot_chart.png", width = 10, height = 10, dpi = 300)
