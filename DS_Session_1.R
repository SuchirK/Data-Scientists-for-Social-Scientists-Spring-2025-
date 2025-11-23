#DS Session 1 (Spring 2025 D3S)

#Install packages. You can install packages in two way: through Rstudio or console
#type install.packages("tidyverse") in the console
#You can also go to Tools -> Install packages... for more visual interface

install.packages("tidyverse")
library("tidyverse")

########################################################################################
###Introducing Packages 
###############################################################################################

yard_conversion <- 1.0936

#You'll note that yard_conversion just appeared in the top-right "environment"
#window This is where R shows you the variables. For reasons that will become
#clear, this is very, very useful. Now let's math! Say we are taking a penalty
#in football and we're in the US, we'd have to measure in yards. We can make
#this calculation by using the conversion variable

11*yard_conversion

#You should get 12.0296 yards. So far so good.

#We can also use the variable for more complicated formulas. Let's say you want
#to know the area of a circle in yards, but you only know the radius is 23 meters.
#We can do that like so:

pi*(23*yard_conversion)^2

AreaYards <- function (meters){
  
  return(pi*(meters*yard_conversion)^2)
  
}

#AreaYards is the name of the function. The blue word 'function' is a
#"reserved word" that is part of the syntax of R. Essentially, it tells R you
#want to create a function, much like the + operator tells it you want to add
#things. In the parenthesis is the variable (argument) that you are going to put
#in. It can have any name, but obviously it is useful to name it something that
#gives an indication as to what it is. Between the curly brackets is the "guts"
#of the function.This is where you take the variable that was entered and
#perform an operation on it. It then returns the result of that operation.

#Now that our function is made let's run it:

AreaYards(23)

#Sweet! Same answer

circle_areas <- AreaYards(seq(5,25, by=5))
circle_areas_max <- max(circle_areas)
circle_areas_min <- min(circle_areas)
circle_areas_min_max <- c(circle_areas_min, circle_areas_max) #c(a,b,c...) concatenates values
circle_areas_mean <- mean(circle_areas_min_max)

circle_areas_max
#This code is a bit verbose. There's a lot of steps, and most of the are for
#creating intermediate variables that serve no purpose other than the final
#calculation. This happens a lot in R. So a fellow named Hadley Wickham came
#along and with a team of coders made the "tidyverse" package. This is a package
#used to do common data manipulations in R more efficiently. It is also a
#framework (dialect) of the R language meant to make it cleaner and more
#readable.

install.packages("tideverse") #in case, not already installed

library(tidyverse)

circle_areas <- AreaYards(seq(5,25,5))
circle_areas_mean2 <-  c(min(circle_areas),
                         max(circle_areas)) %>%
  mean()
circle_areas_mean2


# You notice the strange %>% (pipe) symbol. This basically means "and then."
# Thus, this line of code says concatenate the min and max values and then give
# the mean. with dplyr (part of tidyverse) you can pipe through a bunch of
# functions without reading them into variables or nesting them. It makes for
# clearer reading especially if you are piping through quite a number of
# commands.

#### Working Directory

getwd()
setwd("/Users/suchirkalra/Desktop/D3S Spring 2025/Spring 2025 Datasets")

### Loading the CSV file

ias = read.csv("ias-profile.csv")      #Downloaded the data from https://tcpd.ashoka.edu.in/bureaucrats-of-india/


########################################################################################
###Viewing Data 
###############################################################################################
View(ias)

head(ias)

names(ias)

dim(ias)

length(ias) #Number of Columns

glimpse(ias)

?glimpse #In case you don't what something means, just use the ? (question mark symbol)


### Accessing a particular field

ias$Name

########################################################################################
###Dplyr functions
###############################################################################################

### Select function

name_cadre = select(ias, Name, Cadre)

name_cadre

ias_names = select(ias, Name, Cadre)

# Arrange (Sort in order)

ias_year = arrange(ias, Allotment_Year)

view (ias_year)

ias_year = arrange(ias, -Allotment_Year)

# IN CLASS ASSIGNMENT

name_cadre = select(ias, Name, Allotment_Year)
ias_years = arrange(name_cadre, -Allotment_Year)

ias_name__UP<- ias %>% filter(Cadre == "Uttar Pradesh") %>% select(Name, Allotment_Year, Cadre) %>% arrange(-Allotment_Year)

ias_name_dob <- ias %>% filter(Cadre == "Bihar") %>%  select(Name, Date_of_Birth) %>% arrange(desc(Date_of_Birth))

view (ias_name_dob)

ias_name_dob_bihar_wo_na <- drop_na(ias_name_dob)
  
view (ias_name_year_UP)

# Slice

sliced_ias = slice(ias,25:35)

# Filter

ias_k = filter(ias, Cadre == 'Karnataka')

# Distinct

ias_state = distinct(ias, Allotment_Year, .keep_all = TRUE) #return unique/distinct rows and  .keep_all = TRUE, the function retains all the columns of the data frame for the selected rows.

# Mutate
# Update Serving to explicitly handle 0 and 1

ias_retd <- mutate(ias, Serving = ifelse(Retired == "1", "No", ifelse(Retired == "0", "Yes", NA)))

table(ias_retd$Serving)


########################################################################################
###Group By and Summarise
###############################################################################################


##I want to check whether there are more male or female IAS officers serving or not in Bihar

##The longer way (without pipes)

ias_bihar = filter(ias_retd, Cadre =='Bihar' & Serving == "Yes")
gender_grouped <- group_by(ias_bihar, Gender)
gender_count <- summarise(gender_grouped, Count = n())

gender_count

###Better to use Pipes!! - Clearer and easier 

# Step 1: Filter for IAS officers serving in Bihar

gender_count <- ias_retd %>% filter(Cadre == "Bihar" & Serving == "Yes") %>%  ##Using ias_retd
  group_by(Gender) %>%
  summarise(Count = n())

# Step 3: View the result

gender_count

#######Alternative######

ias_gender_bihar = ias %>% filter (Cadre == "Bihar" & Retired == "0") %>% group_by(Gender) %>%  summarise(Count = n())
ias_gender_bihar



##Cadre-wise IAS Officers by gender in service 

cadre_gender_count <- ias %>%
  mutate(Serving = ifelse(Retired == 1, "No", "Yes")) %>%  # Step 1: Add Serving column
  filter(Serving == "Yes") %>%  # Step 2: Filter for serving IAS officers
  group_by(Cadre, Gender) %>%   # Step 3: Group by Cadre and Gender
  summarise(Count = n(), .groups = "drop") # Step 4: Count officers in each group ##Drops the grouping for easier handling of the output.

view(cadre_gender_count)

## CADRE WISE IAS OFFICERS...

cadre_genderwise <- filter(ias_retd, Serving == "Yes") %>%
  group_by(Cadre, Gender) %>%
  summarise(Count = n())

cadre_genderwise

###Plot cadre-wise gender 

# Create a bar plot

ggplot(cadre_gender_count, aes(x = Cadre, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use "dodge" to separate bars by Gender
  labs(
    title = "Count of Serving IAS Officers by Cadre and Gender",
    x = "Cadre",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal() +  # A clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


###Which state has most females in service?

state_with_female_count <- ias %>%
  mutate(Serving = ifelse(Retired == 1, "No", "Yes")) %>%  # Step 1: Add Serving column
  filter(Serving == "Yes", Gender == "Female") %>%         # Step 2: Filter for serving females
  group_by(Cadre) %>%                                      # Step 3: Group by Cadre
  summarise(Female_Count = n(), .groups = "drop") %>%      # Step 4: Count females in each state
  arrange(desc(Female_Count))

# View the result
print(state_with__female_count)

##Print only the state which has most females

state_with_most_females_in_service = slice(state_with_female_count,1)

print(state_with_most_females_in_service)

ias_education <- read.csv("ias-education.csv")




