#DS Session 2 (Spring 2025 D3S)

setwd("~/Desktop/D3S Spring 2025/Spring 2025 Datasets")

##Lubridate 

library(lubridate) #part of tidyverse
library (tidyverse)

ias <- read.csv("ias-profile.csv")

ias = ias %>% 
  mutate(DOB = ymd(Date_of_Birth))

ias = ias %>% 
  mutate(YOB = year(DOB))

ias = ias %>% 
  mutate(YOB = year(ymd(Date_of_Birth)))

current_date <- today()

View (current_date)


ias_with_currentage <- ias %>%
  mutate(Current_Age = interval(DOB,current_date)/years(1))

ias_with_currentage <- ias %>%
  mutate(Current_Age = 2025-YOB)

View (ias_with_currentage)
## Case When + Mutate

ias_decade = ias %>% 
  mutate(decade = case_when(Allotment_Year > 1979 & Allotment_Year < 1990 ~ 1,
                            Allotment_Year > 1989 & Allotment_Year < 2000 ~ 2,
                            Allotment_Year > 1999 & Allotment_Year < 2010 ~ 3,
                            Allotment_Year > 2009 & Allotment_Year < 2020 ~ 4,
                            TRUE ~ NA_real_))

ias_edu = read.csv('ias-education.csv')
ias_exp = read.csv('ias-experience.csv')

View (ias_edu)
View (ias_exp)


## Q1
# Average Age of IAS Officers at the time of joining by Cadre

##Age at the time of joining 

ias2 = ias %>% 
  mutate(DOB = ymd(Date_of_Birth)) %>% 
  mutate(YOB = year(DOB)) %>% 
  mutate(DOJ = ymd(Date_of_Joining)) %>% 
  mutate(YOJ = year(DOJ)) %>% 
  mutate(Age_joining = YOJ - YOB)

##Age at the time of joining by Cadre

cadre_age = ias2 %>% 
  group_by(Cadre) %>% 
  filter(!is.na(Age_joining)) %>% 
  summarise(mean_age = mean(Age_joining, na.rm = T))

##Age at the time of joining by Gender

Gender_age = ias2 %>% 
  group_by(Gender) %>% 
  filter(!is.na(Age_joining)) %>% 
  summarise(mean_age = mean(Age_joining, na.rm = T))

## Q2
# Average Tenure in each designation level 

ias_exp = ias_exp %>% 
  mutate(tenure_duration = as.numeric(difftime(ymd(End_Date), 
                                               ymd(Start_Date),
                                               units = 'days')))  #This will make a new field called Tenure Duration in the IAS Experience dataset 
View (ias_exp)
ias_exp_average_tenure = ias_exp %>% 
  group_by(Level) %>% 
  summarise(mean_tenure = mean(tenure_duration, na.rm = T))

ias_exp_average_tenure 

##Q3
#Average tenure by Gender (DIY)

###There is no Gender variable in the Experience dataset. Time to merge! 

#Checks!
missing_in_ias_profile <- setdiff(ias$ID_NO, ias_exp$ID_NO) # Find IDs that are in 'ias' but NOT in 'ias_exp'
missing_in_ias_exp <- setdiff(ias_exp$ID_NO, ias$ID_NO) # Find IDs that are in 'ias_exp' but NOT in 'ias'

print(missing_in_ias_profile) #None!!
print(missing_in_ias_exp)   #None!!

merged_ias_profile_exp = left_join(ias, ias_exp, by = 'ID')

View(merged_ias_profile_exp)

merged_ias_profile_exp_tenure_gender = merged_ias_profile_exp%>% 
  group_by(ID) %>% 
  group_by(Gender) %>% 
  summarise(mean_tenure = mean(tenure_duration, na.rm = T))


View(merged_ias_profile_exp_tenure_gender)

##Q4: How frequently are women officers transferred as compared to men?

###Step 1: Find the number of Transfers for the officers gender wise 

##At this point of time, we are ignoring the IAS Officers on leave etc (Only week 3 of the class)!!

View (merged_ias_profile_exp)

Officer_Transfer_Summary <-  merged_ias_profile_exp%>%
  group_by(ID) %>% 
  group_by(Gender) %>%
  summarise(
    Total_Postings = n(),  # Total rows per officer (including initial posting)
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    Total_Tenure_Years = sum(tenure_duration, na.rm = TRUE) / 365  # Days to years
  ) %>%
  ungroup()  # Ungroup to avoid future issues

View (Officer_Transfer_Summary)

table(merged_ias_profile_exp~Gender)

#####FOR UP ONLY#####

Officer_Transfer_Summary_UP <-  merged_ias_profile_exp%>%
  filter (Cadre.x == "Uttar Pradesh")%>%
  group_by(Gender) %>%
  summarise(
    Total_Postings = n(),  # Total rows per officer (including initial posting)
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    Total_Tenure_Years = sum(tenure_duration, na.rm = TRUE) / 365  # Days to years
  ) %>%
  ungroup()  # Ungroup to avoid future issues

View (Officer_Transfer_Summary_UP)

#######Cadre & Gender wise#########

Officer_Transfer_Summary_Cadre_Wise <-  merged_ias_profile_exp%>%
  group_by(Cadre.x,Gender) %>%
  summarise(
    Total_Postings = n(),  # Total rows per officer (including initial posting)
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    Total_Tenure_Years = sum(tenure_duration, na.rm = TRUE) / 365  # Days to years
  ) %>%
  ungroup()  # Ungroup to avoid future issues

Officer_Transfer_Summary_Cadre_Wise

# Create a box plot using base R
boxplot(Number_of_Transfers ~ Gender, data = Officer_Transfer_Summary_Cadre_Wise ,
        main = "Number of Transfers",
        xlab = "Gender", ylab = "Number of Transfers",
        col = c("lightblue", "lightpink"))  # Different colors for gender


# Filter data for UP  (before summarization)
UP_Transfers <- merged_ias_profile_exp %>%
  filter(Cadre.x == "Uttar Pradesh") %>%
  group_by(ID, Cadre.x, Gender) %>%  # Keep officer-level granularity
  summarise(
    Total_Postings = n(), 
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    .groups = "drop"  # Avoids unnecessary grouping issues
  )

View (UP_Transfers) 

# Create a box plot using base R
boxplot(Number_of_Transfers ~ Gender, data = UP_Transfers,
        main = "Number of Transfers (UP)",
        xlab = "Gender", ylab = "Number of Transfers",
        col = c("lightblue", "lightpink"))


## Q5: How frequently are male and female officers transferred in the first five years of their careers as IAS Officers?

####Method 1

tfrs_female <- merged_ias_profile_exp %>% 
  mutate(Cutoff = Allotment_Year + 5) %>% 
  mutate(SY = year(ymd(Start_Date))) %>% 
  filter(SY <= Cutoff & Gender == "Female") %>%
  group_by(ID) %>%  # Keep officer-level granularity
  summarise(
    Total_Postings = n(), 
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    .groups = "drop"  # Avoids unnecessary grouping issues
  )

View (tfrs_female)

tfrs_female %>% 
  summarise(mean = mean(Number_of_Transfers))


tfrs_male <- merged_ias_profile_exp %>% 
  mutate(Cutoff = Allotment_Year + 5) %>% 
  mutate(SY = year(ymd(Start_Date))) %>% 
  filter(SY <= Cutoff & Gender == "Male") %>%
  group_by(ID) %>%  # Keep officer-level granularity
  summarise(
    Total_Postings = n(), 
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    .groups = "drop"  # Avoids unnecessary grouping issues
  )

View (tfrs_male)

tfrs_male %>% 
  summarise(mean = mean(Number_of_Transfers))

##############Method 2- Better ##########

tfrs_female_method2 <- merged_ias_profile_exp %>% 
  mutate(SY = year(ymd(Start_Date))) %>%  # Extract Start Year
  mutate(transfer_year = SY - Allotment_Year) %>%  # Compute years since allotment
  filter(transfer_year > 0 & transfer_year <= 5, Gender == "Female") %>%
  group_by(ID) %>%  # Keep officer-level granularity
  summarise(
    Total_Postings = n(), 
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    .groups = "drop"  # Avoids unnecessary grouping issues
  )
  
View (tfrs_female_method2)

tfrs_female_method2  %>% 
  summarise(mean = mean(Number_of_Transfers)) 

tfrs_male_method2 <- merged_ias_profile_exp %>% 
  mutate(SY = year(ymd(Start_Date))) %>%  # Extract Start Year
  mutate(transfer_year = SY - Allotment_Year) %>%  # Compute years since allotment
  filter(transfer_year > 0 & transfer_year <= 5, Gender == "Male") %>%
  group_by(ID) %>%  # Keep officer-level granularity
  summarise(
    Total_Postings = n(), 
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    .groups = "drop"  # Avoids unnecessary grouping issues
  )

View (tfrs_male_method2)

tfrs_male_method2  %>% 
  summarise(mean = mean(Number_of_Transfers)) 

# Possible Difference: If an officer is allotted in 2010, then:
# - Method 1 includes all postings up to 2015 (including 2010 itself).
# - Method 2 includes postings only from 2011 to 2015, ensuring that 
#   transfers in the allotment year itself (2010) are excluded.


## Q7: How are educated levels related to transfers?

Officer_Transfer_Summary <-  merged_ias_profile_exp%>%
  group_by(ID, Name.x, Gender) %>%
  summarise(
    Total_Postings = n(),  # Total rows per officer (including initial posting)
    Number_of_Transfers = n() - 1,  # Transfers = Total postings - 1
    Total_Tenure_Years = sum(tenure_duration, na.rm = TRUE) / 365  # Days to years
  ) %>%
  ungroup()  # Ungroup to avoid future issues

view (Officer_Transfer_Summary)

merged_ias_transfers_edu = left_join(ias_edu, Officer_Transfer_Summary,  by = 'ID')

View (merged_ias_transfers_edu)

merged_ias_transfers_edu = merged_ias_transfers_edu %>% group_by(ID) %>%mutate(Highest_education_level = max(Reference_Value)) #Finding the highest level of education for each ID 

View (merged_ias_transfers_edu)

merged_ias_transfers_edu_stats <- merged_ias_transfers_edu %>%
  group_by(Highest_education_level) %>%
  summarise(
    Mean_Transfers = mean(Number_of_Transfers, na.rm = TRUE),
    Median_Transfers = median(Number_of_Transfers, na.rm = TRUE),
    SD_Transfers = sd(Number_of_Transfers, na.rm = TRUE),
    Min_Transfers = min(Number_of_Transfers, na.rm = TRUE),
    Max_Transfers = max(Number_of_Transfers, na.rm = TRUE),
    Count = n()
  )

View (merged_ias_transfers_edu_stats)
              

# Scatter plot for Mean Transfers vs Highest Education Level

# Scatter plot for Mean Transfers vs Highest Education Level
plot(merged_ias_transfers_edu_stats$Highest_education_level, 
     merged_ias_transfers_edu_stats$Mean_Transfers,
     xlab = "Highest Education Level", 
     ylab = "Mean Number of Transfers",
     main = "Scatter Plot of Mean Transfers by Education Level",
     pch = 19,        # Plotting character (solid points)
     col = "blue")    # Color of the points

# Linear regression model (without any controls etc)
model <- lm(Mean_Transfers ~ Highest_education_level, data = merged_ias_transfers_edu_stats)
summary(model)











