#DS Session 3 (Spring 2025 D3S), Week 4



setwd("~/Desktop/D3S Spring 2025/Spring 2025 Datasets")

library(tidyverse)
library(knitr) # Load the 'knitr' package, which is useful for dynamic report generation in R
library(ggthemes)# Load the 'ggthemes' package, which provides additional themes for ggplot2 visualizations
library (ggplot2)

tcpd = read.csv('All_States_GE.csv')

View (tcpd)

#Exercise 1: Last 4 election years

tcpd.ge.data.last4<-tcpd%>%
  filter(Year==2019|Year==2014|Year==2009|Year==2004)%>%
  filter(Poll_No==0) 

#as per Lok Dhaba Code book: 
#The variable is 0 in case it was a
#regularly scheduled election, 1 for the
#first bye-poll for that assembly and
#constituency, 2 for the second bye-poll,
#and so on.

tcpd.ge.data.last4 %>% 
  head()

##Exercise 2: Looking at ST reserved constituencies per state in 2009

tcpd.ge.data.last4%>%
  filter(Year==2009&Position==1&Constituency_Type=='ST')%>% #Rank of the contestant based on Vote Share
  group_by(State_Name)%>%
  summarise(N=n(),
            Percent=(N/47*100))%>%  #47 is the total number of reserved seats in the Lok Sabha Elections 
  arrange(-Percent) %>% 
  kable()

###Exercise 3: Turnout in Delhi across years from 2004

Delhi = tcpd %>% 
  filter(State_Name == 'Delhi' & Year >= 2004 & Year <=2019 & Poll_No == 0)

Delhi_turnout = Delhi %>% 
  filter(Position==1)%>%
  group_by(Year, Constituency_No)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (State = "Delhi")

Delhi_turnout %>% 
  head() %>% 
  kable()

ggplot(Delhi_turnout, aes(x = factor(Year), y = turnout))+
  geom_boxplot()

###Exercise 4: Turnout in Haryana across years from 2004

Haryana = tcpd %>% 
  filter(State_Name == 'Haryana' & Year >= 2004 & Year <=2019 & Poll_No == 0)

Haryana_turnout = Haryana %>% 
  filter(Position==1)%>%
  group_by(Year, Constituency_No)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (State = "Haryana")


Haryana_turnout %>% 
  head() %>% 
  kable()

ggplot(Haryana_turnout, aes(x = factor(Year), y = turnout))+
  geom_boxplot()

# Combine both datasets- Caution: Not the same as left join etc!! 

combined_turnout = bind_rows(Delhi_turnout, Haryana_turnout) 

# Create faceted box plots
ggplot(combined_turnout, aes(x = factor(Year), y = turnout)) +
  geom_boxplot() +
  facet_wrap(~State) +  # Creates separate panels for Delhi & Haryana
  labs(title = "Voter Turnout in Delhi and Haryana (2004-2019)", 
       x = "Year", 
       y = "Turnout Percentage") +
  theme_minimal()

Bihar = tcpd %>% 
  filter(State_Name == 'Bihar' & Year >= 2004 & Year <=2019 & Poll_No == 0)

Bihar_turnout = Bihar %>% 
  filter(Position==1)%>%
  group_by(Year, Constituency_No)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (State = "Bihar")

Bihar_turnout %>% 
  head() %>% 
  kable()

ggplot(Bihar_turnout, aes(x = factor(Year), y = turnout))+
  geom_boxplot()

combined_turnout_2 = bind_rows(Bihar_turnout, Haryana_turnout)

# Create faceted box plots
ggplot(combined_turnout_2, aes(x = factor(Year), y = turnout)) +
  geom_boxplot() +
  facet_wrap(~State) +  # Creates separate panels for Delhi & Haryana
  labs(title = "Voter Turnout in Bihar and Haryana (2004-2019)", 
       x = "Year", 
       y = "Turnout Percentage") +
  theme_minimal()

  #Vote Share across major parties in Delhi (1980+)

# Define Congress and BJP aliases
congress_aliases <- c("Indian National Congress", "INC", "Cong(I)", "Congress", "INC(I)", )
bjp_aliases <- c("Bharatiya Janata Party", "BJP", "Bharatiya Janata Dal", "JNP")

# Filter for Delhi from 1980 onwards (General Elections)
tcpd.ge.data.1980onwards.Delhi <- tcpd %>%
  filter(Year >= 1980 & State_Name == "Delhi" & Poll_No == 0) %>%
  mutate(Party_Category = case_when(
    Party %in% congress_aliases ~ "Congress",
    Party %in% bjp_aliases ~ "BJP",
    TRUE ~ "Others"
  ))

View (tcpd.ge.data.1980onwards.Delhi)

# Compute total votes and vote share per party category
vote_share_summary_delhi <- tcpd.ge.data.1980onwards.Delhi %>% 
  group_by(Year, Party_Category) %>% 
  summarise(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(Vote_Share = (Total_Votes / sum(Total_Votes)) * 100) %>%  # Normalize within each year
  arrange(Year, desc(Vote_Share))  # Sort by year and vote share

View (vote_share_summary_delhi)

##The above has something wrong. Can you spot it? 











###Corrected 

# Compute total votes and vote share per party category
vote_share_summary_delhi_corrected <- tcpd.ge.data.1980onwards.Delhi %>% 
  group_by(Year, Party_Category) %>% 
  summarise(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%  # Group by year before calculating vote share
  mutate(Vote_Share = (Total_Votes / sum(Total_Votes)) * 100) %>%  # Normalize within each year
  ungroup() %>%  # Remove grouping after calculation
  arrange(Year, desc(Vote_Share))  # Sort by year and vote share

View (vote_share_summary_delhi_corrected)

# Plot the vote share trends over time
ggplot(vote_share_summary_delhi_corrected, aes(x = Year, y = Vote_Share, color = Party_Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Vote Share of Major Parties in Delhi (1980 Onwards)",
       x = "Year",
       y = "Vote Share (%)",
       color = "Party") +
  scale_color_manual(values = c("BJP" = "Orange", "Congress" = "Blue", "Others" = "gray")) +
  theme_minimal()

##Remember that coloring is an important part of data narratives & visualisation!

    

