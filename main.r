
# Libraries

library("gganimate")
library("data.table")
library("knitr")
library("gridExtra")
library("tidyverse")
library("plotly")

# Load Athletes Events Data

dataOlympics <- read_csv("datasets/athleteEvents.csv", col_types = cols(
                   ID = col_character(),
                   Name = col_character(),
                   Sex = col_factor(levels = c("M","F")),
                   Age =  col_integer(),
                   Height = col_double(),
                   Weight = col_double(),
                   Team = col_character(),
                   NOC = col_character(),
                   Games = col_character(),
                   Year = col_integer(),
                   Season = col_factor(levels = c("Summer","Winter")),
                   City = col_character(),
                   Sport = col_character(),
                   Event = col_character(),
                   Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                 )
)

glimpse(dataOlympics)
head(dataOlympics)

# Load data matching NOCs with respective countries

NOCs <- read_csv("datasets/nocRegions.csv", col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))
glimpse(NOCs)
head(NOCs)

# No. of nations,athletes & events, without art competitions

numbers <- dataOlympics %>%
  group_by(Year, Season) %>%
  summarize(Nations = length(unique(NOC)), Athletes = length(unique(ID)), Events = length(unique(Event))
  )

numbers <- numbers %>%
  mutate(gap= if(Year<1920) 1 else if(Year>=1920 & Year<=1936) 2 else 3)

plotNations <- ggplot(numbers, aes(x=Year, y=Nations, group=interaction(Season,gap), color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("chocolate","deepskyblue4")) +
  labs(x = " ", y = "Nations", 
       title="Nations, Athletes and Events", 
       subtitle = "Olympic Games from 1896 to 2016")
  
plotAthletes <- ggplot(numbers, aes(x=Year, y=Athletes, group=interaction(Season,gap), color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("chocolate","deepskyblue4")) +
  xlab("") 

plotEvents <- ggplot(numbers, aes(x=Year, y=Events, group=interaction(Season,gap), color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("chocolate","deepskyblue4")) 
  
grid.arrange( plotNations, plotAthletes, plotEvents, ncol=1)

# The total no. of medals given to each team

medalCounts <- dataOlympics %>% filter(!is.na(Medal))%>% 
  group_by(NOC, Medal, Event, Games) %>%
  summarize(isMedal=1)

medalCounts <-  medalCounts %>% 
  group_by(NOC, Medal) %>%
  summarize(Count= sum(isMedal))

medalCounts <- left_join(medalCounts, NOCs, by= "NOC" )

medalCounts <- medalCounts %>% 
  mutate (Team = region)

medalCounts <- medalCounts %>% select( Medal, Team, Count)


# Ordering team by total medal count

levelsTeam <- medalCounts %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(desc(Total)) %>%
  select(Team) %>%
  slice(10:1)

medalCounts$Team <- factor(medalCounts$Team, levels=levelsTeam$Team)

medalCounts <- medalCounts %>% filter(Team != "NA")

# Plot medal counts

ggplot(medalCounts, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold3","gray64","sienna")) +
  labs(x = "Nations", y = "Count", 
       title="Top 10 - Nations with the most medals won in the history", 
       subtitle = "Olympic Games from 1896 to 2016") 

# No. of medals given to each team

medalCounts <- dataOlympics %>% filter(!is.na(Medal))%>% 
  group_by(NOC, Medal, Event, Games, Year) %>%
  summarize(isMedal=1)

medalCounts <-  medalCounts %>% 
  group_by(NOC, Medal, Year) %>%
  summarize(Count= sum(isMedal))

medalCounts <- left_join(medalCounts, NOCs, by= "NOC" )

medalCounts <- medalCounts %>% 
  mutate (Team = region)

medalCounts <- medalCounts %>% select( Medal, Team, Count, Year)


# Ordering team by total medal count

levelsTeam <- medalCounts %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(desc(Total)) %>%
  select(Team) %>%
  slice(10:1)

medalCounts$Team <- factor(medalCounts$Team, levels=levelsTeam$Team)

medalCounts <- medalCounts %>% filter(Team != "NA")

# Animated plot medal count
 
plotMedalsAnim<- ggplot(medalCounts, aes(x=Team, y=Count, fill=Medal)) +
  labs(x = "Nations", y = "Count", 
       title='Top 10 - Comparison over time, nations with the most medals', 
       subtitle = 'Olympic Games from 1896 to 2016 - Year: {frame_time}')  +
  transition_time(Year)+
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold3","gray64","sienna"))
  
animate(plotMedalsAnim,fps=2)

