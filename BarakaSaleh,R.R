#reading the file
medals <- read.csv("C:/Users/DELL/Downloads/medals.csv")
View(medals)
head(medals)

#	Check for missing values and handle them appropriately 
colSums(is.na(medals))
#handling missing value
medals[470, 'medal_code'] <- 3
colSums(is.na(medals))
#Ensure data types are correct for each column.
str(medals)
# convert medal_date from character to  "Date"
medals$medal_date <- as.Date(medals$medal_date) 
str(medals)
#Generate a summary of the dataset's statistics.
summary(medals)
#Find the total number of record in the dataset.
nrow(medals)
#Identify which countries has obtained highest number of Gold, Silver and Bronze medals
install.packages('dplyr')
library(dplyr)

country_medal_counts <- medals %>%
  group_by(country, medal_type) %>%
  summarise(medal_count = n(), .groups = 'drop')

highest_medals <- country_medal_counts %>%
  group_by(medal_type) %>%
  slice(which.max(medal_count))

print(highest_medals)
#Identify the top 3 medalists.
medallist_summary <- medals %>%
  group_by(name) %>%
  summarise(total_medals = n(), .groups = 'drop') %>%
  arrange(desc(total_medals))

top_medallists <- head(medallist_summary, 3)

print(top_medallists)
#Create visualizations to show the distribution of 
#using ggplot2 Packages
install.packages('ggplot2')
library(ggplot2)

event_distribution <- medals %>%
  group_by(event_type, medal_type) %>%
  summarize(count = n(), .groups = 'drop')

    #create a bar chart
 
ggplot(event_distribution, aes(x = reorder(event_type, count), y = count, fill = medal_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of Medals by Event Type",
       x = "Event Type",
       y = "Number of Medals") +
  scale_fill_manual(values = c("Gold Medal" = 'grey', "Silver Medal" = "pink", "Bronze Medal" = "red")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Aggregate the data to calculate total Gold medals obtained by “Team” category for France
library(dplyr)
total_gold_team_france <- medals %>%
  filter(country == "France", medal_type == "Gold Medal", grepl("TEAM", event_type)) %>%          # Filter for France, Gold, and Team
  summarise(total_gold = n())  # Count the number of rows


filter(total_gold_team_france)

#Update the gender abbreviation for female from ‘W’ to ‘F’.
medals$gender<- sub('W', 'F', medals$gender)

#Which country has the highest female medalists?
highest_female_medallists <- medals %>%
  filter(gender == "F") %>%  
  group_by(country) %>%          
  summarise(Total_Female_Medallists = n_distinct(name), .groups = 'drop') %>%  
  slice_max(order_by = Total_Female_Medallists, n = 1)

print(highest_female_medallists)

#Which country come out as the top achiever in Athletics discipline. 
top_athletics_country <- medals %>%                      
  filter(event_type == "ATH") %>%  
  group_by(country) %>%                 
  summarise(Total_Medals = n(), .groups = 'drop') %>%  
  slice_max(order_by = Total_Medals, n = 1)  


print(top_athletics_country)

#Calculate the total medals obtained by the country for man and woman category. 

total_medals_by_gender <- medals %>%
  group_by(country, gender) %>%           
  summarise(Total_Medals = n(), .groups = 'drop')  
print(total_medals_by_gender)

#visualize the result
ggplot(medals,aes(x=gender))+
  geom_bar(fill='pink')+
  labs(title='Medal Distribution by gender',x='gender',y='Number of Medals')+
  theme_minimal()

#Calculate and present the Gold Metals performance for top 10 countries. 
top_gold_medals <- medals %>%
  filter(trimws(medal_type) == "Gold Medal") %>%  
  group_by(country) %>%
  summarise(Total_Gold_Medals = n(), .groups = 'drop') %>%
  arrange(desc(Total_Gold_Medals)) %>%
  slice_head(n = 10)
print(top_gold_medals)

#Visualize the result

ggplot(top_gold_medals, aes(x= reorder(country, -Total_Gold_Medals), y = Total_Gold_Medals)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "Top 10 Countries by Gold Medals",
       x = "Total Gold Medals",
       y = "Country") +
  theme_minimal() 

#additional data analysis.
#Calculate and present the Silver Metals performance for top 10 countries.

top_Silver_medals <- medals %>%
  filter(trimws(medal_type) == "Silver Medal") %>%  
  group_by(country) %>%
  summarise(Total_Silver_Medals = n(), .groups = 'drop') %>%
  arrange(desc(Total_Silver_Medals)) %>%
  slice_head(n = 10)
print(top_Silver_medals)

