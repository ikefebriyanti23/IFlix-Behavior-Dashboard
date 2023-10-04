#Import Package
library(readr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(stringr)
library(clipr)

#Import Dataset
asset <- read.csv("assets.csv")
demographics <- read.csv("demographics.csv")
plays <- read.csv("plays.csv")
psychographics <- read.csv("psychographics.csv")
users <- read.csv("users.csv")

# Data Cleaning
## Asset Data
### Data Checking
describe(asset)

### Check for Null Values
asset%>%
  filter(is.na(show_type)) #0 rows for null values

### Check Duplicates
asset%>%
  duplicated() #No duplicate data detected

### Data Manipulation
head(asset)
top_asset <- asset%>%
             select(genre, show_type)
  
top_asset$asset_type <- str_c(top_asset$genre, top_asset$show_type,
                              sep=",")
top_asset <- top_asset%>%
             select(asset_type)%>%
             group_by(asset_type)%>%
             count()%>%
             arrange(desc(n))%>%
             rename(num_asset=n)
  
top_asset <- head(top_asset,5)
top_asset

### Data Visualization
top_asset_viz <- ggplot(data = top_asset,
                        aes(x=genre,y=count,fill=show_type)) +
                        geom_bar(position="dodge",
                        stat="identity")+
                        labs(title = "Top Iflix Assets")
top_asset_viz


## Demographics Data
describe(demographics)

### Check for Null Values
demographics%>%
  filter(is.na(user_id)) #0 rows for null values

### Dealing with Duplicates
demographics%>%
  duplicated() #No duplicate data detected

### Data Manipulation
demographics_income <- demographics%>% #Include Income information
                  filter(level_2 == 'Income')
demographics_income

demographics_age <- demographics%>% #Include Age information
                  filter(level_2 == 'Age')
demographics_age

demographics_gender <- demographics%>% #Include Gender information
                  filter(level_2 == 'Gender')
demographics_gender

### Delete Unused Columns
demographics_income <- demographics_income%>%
                  select(-platform, -level_1, - level_2, -confidence_score)%>%
                  rename(income = level_3)
head(demographics_income)

demographics_age <- demographics_age%>%
                  select(-platform, -level_1, -level_2, -confidence_score)%>%
                  rename(age = level_3)
head(demographics_age)

demographics_gender <- demographics_gender%>%
                  select(-platform, -level_1, -level_2, -confidence_score)%>%
                  rename(gender = level_3)
head(demographics_gender)

### Count Demographic Observed
count_dem_inc <- demographics_income%>%
                  group_by(income)%>%
                  count()%>%
                  rename(num_users = n)
count_dem_inc

count_dem_age <- demographics_age%>%
                  group_by(age)%>%
                  count()%>%
                  rename(num_users = n)
count_dem_age

count_dem_gender <- demographics_gender%>%
                    group_by(gender)%>%
                    count()%>%
                    rename(num_users = n)
count_dem_gender

### Create new data frame: dem_device
dem_device <- demographics%>%
              arrange(user_id)%>%
              select(user_id, platform)
head(dem_device)

count_dem_device <- dem_device%>%
                    group_by(platform)%>%
                    count()%>%
                    rename(num_users = n)%>%
                    arrange(desc(num_users))
count_dem_device

count_dem_device <- head(count_dem_device, 5)

### Data Visualization
income <- ggplot(data = count_dem_inc,
                        aes(x=income,y=num_users)) +
                        geom_bar(position="dodge",
                        stat="identity")+
                        labs(title = "Income Demography")
income

age <- ggplot(data = count_dem_age,
              aes(x=age,y=num_users)) +
              geom_bar(position="dodge",
              stat="identity")+
              labs(title = "Age Demography")
age

gender <- ggplot(data = count_dem_gender,
              aes(x=gender,y=num_users)) +
              geom_bar(position="dodge",
              stat="identity")+
              labs(title = "Gender Demography")
gender

device <- ggplot(data = count_dem_gender,
                 aes(x=gender,y=num_users)) +
                 geom_bar(position="dodge",
                 stat="identity")+
                 labs(title = "Gender Demography")
device


## Plays Data
plays <- plays%>%
         arrange(user_id)
head(plays)

describe(plays)

### Check for Null Values
plays%>%
  filter(is.na(user_id)) #0 rows for null values

### Dealing with Duplicates
plays%>%
  duplicated() #No duplicate data detected

### Top Assets Played
top_played <- plays%>%
              select(asset_id, minutes_viewed)%>%
              arrange(desc(minutes_viewed))

top_played <- head(top_played,5)
top_played

### Merge with asset data
asset_played <- asset%>%
                select(asset_id, show_type, genre)

top_played_details <- top_played%>%
                      left_join(asset_played, by = c("asset_id"="asset_id"))
top_played_details$show = paste(top_played_details$show_type, 
                                top_played_details$genre, sep = ",")
top_played_details <- top_played_details%>%
                      select(show, minutes_viewed)
top_played_details <- head(top_played_details,5)

### Data Visualization
top_played_viz <- ggplot(data = top_played_details,
                  aes(x=show,y=minutes_viewed)) +
                  geom_bar(position="dodge",
                  stat="identity")+
                  labs(title = "Top Played")
top_played_viz


## Psychographics Data
head(psychographics)
describe(psychographics)
psychographics <- psychographics%>%
                  arrange(user_id)

### Check for Null Values
psychographics%>%
  filter(is.na(user_id)) #0 rows for null values

### Dealing with Duplicates
psychographics%>%
  duplicated() #No duplicate data detected

### Data Manipulation
psychographics <- psychographics%>%
                  select(-platform, -level_1, -level_2,
                         -confidence_score)

psychographics <- psychographics%>%
                  mutate(interest = case_when(
                  level_3 == "Sci-Fi TV Fans"|level_3 ==
                  "Sci-Fi Movies Fans" ~ "Sci-Fi",
                  level_3 == "Documentary and Biography Movies Fans"
                  |level_3 == "Documentary and Biography TV Fans" ~ 
                  "Documentary and Biography",
                  level_3 == "Reality TV Fans"|level_3 =="Reality Movies Fans"~
                  "Reality", level_3 == "Comedy Movies Fans"
                  |level_3 =="Comedy TV Fans"~
                  "Comedy", level_3 == "Kids Movies Fans"
                  |level_3=="Kids TV Fans" ~
                  "Kids", level_3 == "Action and Adventure Movies Fans"
                  |level_3 == "Action and Adventure TV Fans"
                  ~"Action and Adventure",
                  level_3 == "Drama TV Fans"|level_3 =="Drama Movies Fans"~
                  "Drama", level_3 == "Fantasy Movies Fans"
                  |level_3 =="Fantasy TV Fans"~"Fantasy",
                  level_3 == "Romance TV Fans"|level_3 ==
                  "Romance Movies Fans"~"Romance", level_3 == 
                  "Horror Movies Fans"|level_3 =="Horror TV Fans"~
                  "Horror", level_3 == "Crime and Mystery Movies Fans"
                  |level_3 =="Crime and Mystery TV Fans"~"Mystery",
                  level_3 =="Lifestyle and Fashion TV Fans"
                  |level_3 =="Lifestyle and Fashion Movies Fans"
                  ~"Lifestyle and Fashion",
                  level_3 == "Anime TV Fans"|level_3 ==
                  "Anime Movies Fans"~"Anime",
                  level_3 == "Thriller Movies Fans"~"Thriller",
                  level_3 == "Music Movies Fans"~"Music",
                  level_3 == "Sports TV Fans"
                  |level_3 =="Sports Movies Fans"~"Sports",
                  level_3 == "Adult Romance Movies Fans"~"Adult Romance",
                  level_3 == "Animation Movies Fans"
                  |level_3 =="Animation TV Fans"~
                  "Animation", level_3 == "Family Movies Fans"|level_3 == 
                  "Family TV Fans"~"Family", level_3 == "eSports Movies Fans"
                  |level_3 == "eSports TV Fans"~"eSports",level_3 ==
                  "Health and Fitness Movies Fans"~"Health and Fitness",
                  level_3 == "Live Events and Specials Movies Fans"~
                  "Live Events and Specials", level_3 == 
                  "News Movies Fans"|level_3 =="News TV Fans"~"News",
                  level_3 == "Religion and Faith Movies Fans"|level_3 ==
                  "Religion and Faith TV Fans"~"Religion and Faith",
                  level_3 == "Best of Web or Viral Movies Fans"|level_3 ==
                  "Best of Web or Viral TV Fans"~"Best of Web or Viral",
                  level_3 == "Education TV Fans"
                  |level_3 =="Education Movies Fans"~
                  "Education",level_3 == "Extreme Sports Movies Fans"~
                  "Extreme Sports", level_3 == "Game Show TV Fans"~
                  "Game Show", level_3 == level_3 ~ "Others"))
head(psychographics)

### Top Interest
top_interest <- psychographics%>%
                filter(grepl("Fans", level_3, fixed = TRUE))%>%
                group_by(level_3)%>%
                count()%>%
                rename(num_users = n)%>%
                rename (top_interest = level_3)%>%
                arrange(desc(num_users))
top_interest <- head(top_interest,5)
top_interest

### Data Visualization
top_interest_viz <- ggplot(data = top_interest,
                    aes(x=top_interest,y=num_users)) +
                    geom_bar(position="dodge",
                    stat="identity")+
                    labs(title = "Top User Interest")
top_interest_viz

## Users Data
### Data Checking
users <- users%>%
         arrange(user_id)
describe(users)

### Check for Null Values
users%>%
  filter(is.na(user_id)) #0 rows for null values

### Check Duplicates
users%>%
  duplicated() #No duplicate data detected

### Count users per Country
head(users)

users%>%select(country_code)%>%distinct()

users_country <- users%>%
                 group_by(country_code)%>%
                 count()%>%
                 rename(num_users = n)%>%
                 arrange(desc(num_users))
users_country

## Data Visualization
top_countries <- ggplot(data = users_country,
                 aes(x=country_code,y=num_users)) +
                 geom_bar(position="dodge",
                 stat="identity")+
                 labs(title = "Top Countries")
top_countries

#Other Data for Visualization
##Total Users
total_users <- users%>%
               count()%>%
               rename(total_users = n)
total_users

##Total Countries
total_countries <- users%>%
                   distinct(country_code)%>%
                   count()%>%
                   rename(total_countries = n)

## Total Plays
total_plays <- plays%>%
               select(user_id)%>%
               count()%>%
               rename(total_plays = n)

## Minutes Viewed
minutes_viewed <- plays%>%
                  select(minutes_viewed)%>%
                  sum()
minutes_viewed

minutes_viewed <- data.frame(minutesviewed)
minutesviewed <- c(18464504)
str(minutes_viewed)
minutes_viewed

## Total TV Shows
tv_shows <- asset%>%
            select(show_type)%>%
            filter(show_type =="TV")%>%
            count()%>%
            rename(tv_shows = n)
tv_shows

## Total Movies
movies <- asset%>%
  select(show_type)%>%
  filter(show_type =="Movies")%>%
  count()%>%
  rename(movies = n)
movies

## Customer Segmentation
cust_seg <- plays%>%
select(-platform)%>%
arrange(desc(user_id))

#Low Quality Customer
cust_seg_low <- cust_seg%>%
                filter(minutes_viewed <= 1)
cust_seg_low

count_low <- cust_seg_low%>%
  distinct(user_id)%>%
  count()

asset_low <- cust_seg_low%>%select(asset_id)%>%count()
asset_low <- asset_low/count_low

viewed_low <- mean(cust_seg_low$minutes_viewed)

viewed_low

#Medium Quality Customer
cust_seg_medium <- cust_seg%>%
                   filter(minutes_viewed >1 & minutes_viewed <=16)
cust_seg_medium

count_med <- cust_seg_medium%>%
  distinct(user_id)%>%
  count()

asset_med <- cust_seg_medium%>%select(asset_id)%>%count()
asset_med <- asset_med/count_med
asset_med

viewed_med <- mean(cust_seg_medium$minutes_viewed)

viewed_med

#High Quality Customer
cust_seg_high <- cust_seg%>%
  filter(minutes_viewed >16 & minutes_viewed <=59)
cust_seg_high

count_high <- cust_seg_high%>%
  distinct(user_id)%>%
  count()
count_high

asset_high <- cust_seg_high%>%select(asset_id)%>%count()
asset_high <- asset_high/count_high
asset_high

viewed_high <- mean(cust_seg_high$minutes_viewed)

viewed_high

#Loyal Quality Customer
cust_seg_loyal <- cust_seg%>%
  filter(minutes_viewed >59)
cust_seg_loyal

count_loyal <- cust_seg_loyal%>%
  distinct(user_id)%>%
  count()
count_loyal

asset_loyal <- cust_seg_loyal%>%select(asset_id)%>%count()
asset_loyal <- asset_loyal/count_loyal
asset_loyal

viewed_loyal <- mean(cust_seg_loyal$minutes_viewed)
                 
viewed_loyal

customer_segmentation <- data.frame(cluster=c("Low Quality Users",
                                              "Medium Quality Users",
                                              "High Quality Users",
                                              "Loyal Quality Users"),
                                    total_users=c(57304,48850,33783,34249),
                                    avg_asset_watched=c(2,3,4,4),
                                    avg_watch_minutes=c(0.42,6.59,36.46,94.94))
customer_segmentation

