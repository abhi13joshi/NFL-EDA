#Installed libraries
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("hablar")
#install.packages("tidyverse")
#install.packages('sqldf')

#Included libraries
#library('xml2')
library(rvest)
library(dplyr)
library(ggplot2)
library(hablar)
library(tidyverse)
library(sqldf)

#NFL Season Passing Stats
Passing_Stats <- "https://www.pro-football-reference.com/years/2021/passing.htm"

url <- Passing_Stats
pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

pageobj %>%  
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) -> Passing_Stats

Passing_Stats <- row_to_names(Passing_Stats, 0)

Passing_Stats <- clean_names(Passing_Stats)
names(Passing_Stats)

colnames(Passing_Stats) <- c('ID', 'Player', 'Team', 'Age', 'Pos', 'GP', 'GS', 
                            'Record', 'Comp', 'Att', 'CompPct', 'Yards', 'TD', 
                            'TDPct', 'Int', 'IntPct','FirstDown', 'Long', 'YPA',
                            'AvgYPA', 'YPC', 'YPG', 'Rating', 'QBR', 'Sacks', 
                            'SackYards', 'SackPct', 'NetYPA', 'AdjNetYPA', 
                            'Comebacks', 'GWD')

Passing_Stats<-Passing_Stats[!(Passing_Stats$ID=="Rk")  , ]

i <- c(1, 4, 6:7, 9:31) 
Passing_Stats[ , i] <- apply(Passing_Stats[ , i], 2, function(x) as.numeric(as.character(x)))

Passing_Stats['Pos'][Passing_Stats['Pos'] == "qb"] <- "QB"

Passing_Stats$Player <- stringr::str_replace(Passing_Stats$Player, '\\*', '')
Passing_Stats$Player <- stringr::str_replace(Passing_Stats$Player, '\\+', '')
Passing_Stats

view(Passing_Stats)

Top_Passers <- sqldf("SELECT * FROM Passing_Stats WHERE (GP >= 8) & (GS >= 7)
                     & (Pos = 'QB') ORDER BY Yards DESC")
view(Top_Passers)

#write.csv(Top_Passers, 'Top_Passers_2021_Season.csv')

options(ggrepel.max.overlaps = Inf)

Efficiency <- ggplot(Top_Passers,aes(x = YPG, y = CompPct, color = Team)) + 
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = Player, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "NFL Passing Leaders 2021-2022 Season", 
       x = "Passing Yards Per Game", 
       y = "Completion Percentage", 
       caption = "Data Viz by Abhi Joshi | Data from Pro Football Reference | 2021 NFL Season") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("ARI" = "#97233F", "ATL" = "#A71930", "BAL" = "#241773", "BUF" = "#00338D",
                             "CAR" = "#0085CA", "CHI" = "#0B162A", "CIN" = "#FB4F14", "CLE" = "#311D00",
                             "DAL" = "#003594", "DEN" = "#FB4F14", "DET" = "#0076B6", "GNB" = "#203731", 
                             "HOU" = "#03202F", "IND" = "#002C5F", "JAX" = "#101820", "KAN" = "#E31837",
                             "LAC" = "#0080C6", "LAR" = "#003594", "LVR" = "#000000", "MIA" = "#008E97",
                             "MIN" = "#4F2683", "NOR" = "#D3BC8D", "NWE" = "#002244", "NYG" = "#0B2265", 
                             "NYJ" = "#125740", "PHI" = "#004C54", "PIT" = "#FFB612", "SEA" = "#002244",
                             "SFO" = "#AA0000", "TAM" = "#D50A0A", "TEN" = "#0C2340", "WAS" = "#773141")) +
  scale_color_manual(values=c("ARI" = "#97233F", "ATL" = "#A71930", "BAL" = "#241773", "BUF" = "#00338D",
                              "CAR" = "#0085CA", "CHI" = "#0B162A", "CIN" = "#FB4F14", "CLE" = "#311D00",
                              "DAL" = "#003594", "DEN" = "#FB4F14", "DET" = "#0076B6", "GNB" = "#203731", 
                              "HOU" = "#03202F", "IND" = "#002C5F", "JAX" = "#101820", "KAN" = "#E31837",
                              "LAC" = "#0080C6", "LAR" = "#003594", "LVR" = "#000000", "MIA" = "#008E97",
                              "MIN" = "#4F2683", "NOR" = "#D3BC8D", "NWE" = "#002244", "NYG" = "#0B2265", 
                              "NYJ" = "#125740", "PHI" = "#004C54", "PIT" = "#FFB612", "SEA" = "#002244",
                              "SFO" = "#AA0000", "TAM" = "#D50A0A", "TEN" = "#0C2340", "WAS" = "#773141"))

Efficiency

#ggsave(filename = 'NFL_Top_Passers_2021_Season.png', plot = Efficiency, width=7.5, height=5, units="in", dpi=300)
