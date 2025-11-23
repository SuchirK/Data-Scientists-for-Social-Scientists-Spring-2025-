tcpd_assembly <- read.csv ("All_States_AE.csv") 

tcpd_ge <- read.csv('All_States_GE.csv')

View (tcpd_assembly)

tcpd_assembly = tcpd_assembly %>% 
  mutate(decade = case_when(
    Year >= 1960 & Year < 1970 ~ 1,
    Year >= 1970 & Year < 1980 ~ 2,
    Year >= 1980 & Year < 1990 ~ 3,
    Year >= 1990 & Year < 2000 ~ 4,
    Year >= 2000 & Year < 2010 ~ 5,
    Year >= 2010 & Year < 2020 ~ 6,
    Year >= 2020 & Year < 2030 ~ 7,
    TRUE ~ NA_real_
  ))

tcpd_ge = tcpd_ge %>% 
  mutate(decade = case_when(
    Year >= 1960 & Year < 1970 ~ 1,
    Year >= 1970 & Year < 1980 ~ 2,
    Year >= 1980 & Year < 1990 ~ 3,
    Year >= 1990 & Year < 2000 ~ 4,
    Year >= 2000 & Year < 2010 ~ 5,
    Year >= 2010 & Year < 2020 ~ 6,
    Year >= 2020 & Year < 2030 ~ 7,
    TRUE ~ NA_real_
  ))

tcpd_assembly_turnout_summary = tcpd_assembly %>% 
  filter(Position==1)%>%
  group_by(decade)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (Election_Type = "Assembly Elections")

tcpd_ge_turnout_summary = tcpd_ge %>% 
  filter(Position==1)%>%
  group_by(decade)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (Election_Type = "General Elections")

combined_turnout = bind_rows(tcpd_assembly_turnout_summary, tcpd_ge_turnout_summary) 

# Plot the turnout trends
ggplot(combined_turnout, aes(x = decade, y = turnout, color = Election_Type, group = Election_Type)) +
  geom_line(size = 1) +       # Line plot
  geom_point(size = 3) +      # Add points for clarity
  labs(title = "Voter Turnout Trends by Decade",
       x = "Decade",
       y = "Average Turnout (%)",
       color = "Election Type") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7, labels = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")) +
  scale_color_manual(values = c("Assembly Elections" = "red", "General Elections" = "blue"))

tcpd_assembly_turnout_summary_delhi = tcpd_assembly %>% 
  filter(Position==1 & State_Name =="Delhi")%>%
  group_by(decade)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (Election_Type = "Assembly Elections")

tcpd_general_turnout_summary_delhi = tcpd_ge %>% 
  filter(Position==1 & State_Name =="Delhi")%>%
  group_by(decade)%>%
  summarise(turnout=mean(Turnout_Percentage,na.rm=T)) %>% 
  mutate (Election_Type = "General Elections")

combined_turnout_in_delhi= bind_rows(tcpd_assembly_turnout_summary_delhi, tcpd_general_turnout_summary_delhi) 

# Plot the turnout trends in Delhi

ggplot(combined_turnout_in_delhi, aes(x = decade, y = turnout, color = Election_Type, group = Election_Type)) +
  geom_line(size = 1) +       # Line plot
  geom_point(size = 3) +      # Add points for clarity
  labs(title = "Voter Turnout Trends by Decade",
       x = "Decade",
       y = "Average Turnout (%)",
       color = "Election Type") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7, labels = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")) +
  scale_color_manual(values = c("Assembly Elections" = "red", "General Elections" = "blue"))


