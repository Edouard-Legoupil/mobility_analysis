## packages

packages <- c("dplyr","leaflet","htmltools","ggplot2","lubridate","qdap","tm","SnowballC","wordcloud","RColorBrewer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
rm(packages)
library(dplyr)
library(leaflet)
library(htmltools)
library(ggplot2)
library(lubridate)
library(qdap)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(unhcRstyle)



## Get data ######
#install.packages("acled.api")

# devtools::install_github("chris-dworschak/acled.api")
#
# library(acled.api)
# # Importing the ACLED d
#
# ## tu use the function - install the package and then enter in your Renvironment file the following
#
# # EMAIL_ADDRESS="myemail@email.org"
# # ACCESS_KEY="mykey"
#
# acled.data <- acled.api(start.date = "2017-01-01",
#                         end.date = "2020-11-20")


# write.csv( acled.data, "/data-raw/acled.csv", row.names = FALSE)

getwd()
acled.data <- read.csv(paste0(getwd(),"/data-raw/acled.csv"), sep=";")


### Tiday data ###############

names(acled.data)
levels(as.factor(acled.data$region))

levels(as.factor(acled.data$country))
table(acled.data$country)
table(acled.data$year)

# Looking at columns actor2, ADMIN2, ADMIN3, LOCATION, NOTES, SOURCE we see that they have some missing data.
apply(acled.data, 2, function(x) any(is.na(x)))

# For Column actor2, NA tells us that there was no second actor. So we can replace it with string "NONE"

table(acled.data$actor1, useNA = "ifany")
acled.data$actor1[is.na(acled.data$actor1)] <- "NONE"

acled.data$inter1 <- as.character(acled.data$inter1)
lut1 <- c("1" = "Government or mutinous force", "2" = "Rebel force",
          "3" = "Political militia", "4" = "Ethnic militia",
          "5" = "Rioters", "6" = "Protesters", "7" = "Civilians",
          "8" = "Outside/external force")
acled.data$inter1_lab <- lut1[acled.data$inter1]
head(acled.data$inter1_lab)



acled.data$interaction <-as.character(acled.data$interaction)
lut3<-c("10" = "SOLE MILITARY ACTION", "11" = "MILITARY VERSUS MILITARY",
        "12" = "MILITARY VERSUS REBELS", "13" = "MILITARY VERSUS POLITICAL MILITIA",
        "14" = "MILITARY VERSUS COMMUNAL MILITIA", "15" = "MILITARY VERSUS RIOTERS",
        "16" = "MILITARY VERSUS PROTESTERS", "17" = "MILITARY VERSUS CIVILIANS",
        "18" = "MILITARY VERSUS OTHER", "20" = "SOLE REBEL ACTION ",
        "22" = "REBELS VERSUS REBELS", "23" = "REBELS VERSUS POLITICAL MILIITA",
        "24" = "REBELS VERSUS COMMUNAL MILITIA", "25" = "REBELS VERSUS RIOTERS",
        "26" = "REBELS VERSUS PROTESTERS", "27" = "REBELS VERSUS CIVILIANS",
        "28" = "REBELS VERSUS OTHERS", "30" = "SOLE POLITICAL MILITIA ACTION",
        "33" = "POLITICAL MILITIA VERSUS POLITICAL MILITIA",
        "34" = "POLITICAL MILITIA VERSUS COMMUNAL MILITIA",
        "35" = "POLITICAL MILITIA VERSUS RIOTERS",
        "36" = "POLITICAL MILITIA VERSUS PROTESTERS",
        "37" = "POLITICAL MILITIA VERSUS CIVILIANS",
        "38" = "POLITICAL MILITIA VERSUS OTHERS",
        "40" = "SOLE COMMUNAL MILITIA ACTION",
        "44" = "COMMUNAL MILITIA VERSUS COMMUNAL MILITIA",
        "45" = "COMMUNAL MILITIA VERSUS RIOTERS",
        "46" = "COMMUNAL MILITIA VERSUS PROTESTERS",
        "47" = "COMMUNAL MILITIA VERSUS CIVILIANS",
        "48" = "COMMUNAL MILITIA VERSUS OTHER","50" = "SOLE RIOTER ACTION",
        "55" = "RIOTERS VERSUS RIOTERS", "56" = "RIOTERS VERSUS PROTESTERS",
        "57" = "RIOTERS VERSUS CIVILIANS", "58" = "RIOTERS VERSUS OTHERS",
        "60" = "SOLE PROTESTER ACTION", "66" = "PROTESTERS VERSUS PROTESTERS",
        "67" = "PROTESTERS VERSUS CIVILIANS", "68" = "PROTESTERS VERSUS OTHER",
        "70" = "SOLE CIVILIANS", "77" = "CIVILIANS VERSUS CIVILIANS",
        "78" = "OTHER ACTOR VERSUS CIVILIANS", "80" = "SOLE OTHER ACTION",
        "88" = "OTHERS VERSUS OTHERS")
acled.data$interaction_lab <- lut3[acled.data$interaction]
head(acled.data$interaction_lab)

# Convert LONGITUDE and LATITUDE columns to Numeric
acled.data$longtitude <-as.numeric(acled.data$longtitude)
acled.data$LATITUDE<-as.numeric(acled.data$LATITUDE)




acled.data$fatcal[acled.data$fatalities <= 10] <- '0 to 10 fatalities'
acled.data$fatcal[10 < acled.data$fatalities & acled.data$fatalities <= 50] <- '10 to 50 fatalities'
acled.data$fatcal[50 < acled.data$fatalities & acled.data$fatalities  <= 100] <- '50 to 100 fatalities'
acled.data$fatcal[acled.data$fatalities > 100] <- 'Over 100 fatalities'


acled.data$event_date1 <-dmy(acled.data$event_date)
acled.data$month <- month(acled.data$event_date1)
acled.data$yearmonth <- paste0(acled.data$year,"-", acled.data$month)
#head(acled.data$yearmonth)

acled.data$quarter <- quarter(acled.data$event_date1)
acled.data$yearquarter <- paste0(acled.data$year,"-", acled.data$quarter)
#head(acled.data$yearquarter)


##  Violence per Actor and Year ###
# "Caribbean"
#"Central America"

acled.data  %>%
  filter(., region == "South America"
         # "Caribbean"
         #"Central America"
  )  %>%
  ggplot(mapping = aes(x=year, group=event_type, color=event_type)) +
  geom_smooth(stat = "count") + labs(x = "Year", y = "Number of Events", title = "Violence per Actor and Year", color="Actor Instigating Conflict") +
  facet_wrap(~ country, ncol = 3) +
  #facet_grid(vars(type), vars(country_region)) +
  unhcr_theme()

# Violent events in America by amounts of fatalities #####
g <- list(
  scope = 'america',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

plot_geo(acled.data, locationmode = 'country') %>%
  add_markers(x = ~longtitude, y = ~latitude,
              color = ~fatcal,
              #frame=~year,
              text = ~paste(fatalities, "fatalities")) %>%
  layout(title = 'Violent events in America by amounts of fatalities <br>(Click legend to toggle)',
         geo = list(
           scope = 'america',
           projection = list(type = 'Mercator'),
           showland = TRUE,
           landcolor = toRGB("gray85"),
           subunitwidth = 1,
           countrywidth = 1,
           subunitcolor = toRGB("white"),
           countrycolor = toRGB("white")
         ))



## Frequency Plot for Number of Conflicts in past 20 years ####
acled.data %>%
  filter(., region == "South America"
         # "Caribbean"
         #"Central America"
  )  %>%
  group_by(yearmonth, country)%>%
  summarise(count=n())%>%
  arrange(desc(count)) %>%

  ggplot(aes(x = yearmonth ,y=count #,fill=-count
  )) +
  geom_bar(stat = "identity",
           color= unhcr_blue,
           fill = unhcr_blue) +
  # scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total Number of Conflicts",labels = scales::comma) +
  ggtitle("Frequency Plot for Number of Conflicts in past 20 years",
          subtitle = "Data about number of conflicts which happened from 1997 to 2017") +
  facet_wrap(~ country, ncol = 3) +
  unhcr_theme()


## Data about fatalities happening every year due to conflicts" ######
acled.data %>%
  filter(., region == "South America"
         # "Caribbean"
         #"Central America"
  )  %>%
  group_by(yearquarter, country) %>%
  summarise(total = sum(fatalities)) %>%
  ggplot(aes(x=yearquarter,
             y=total#,
             #fill=-total
  )) +
  geom_bar(stat = "identity", fill = unhcr_blue)+
  #scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total Number of fatalities",
                     labels = scales::comma) +
  ggtitle("fatalities over the years due to conflicts",
          subtitle = "Data about fatalities happening every year due to conflicts") +
  facet_wrap(~ country, ncol = 3) +
  unhcr_theme()


## Data about fatalities happening every year due to conflicts" ######
acled.data %>%
  group_by(yearquarter) %>%
  summarise(total = sum(fatalities)) %>%
  ggplot(aes(x=yearquarter,
             y=total#,
             #fill=-total
  )) +
  geom_bar(stat = "identity", fill = unhcr_blue)+
  #scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total Number of fatalities",
                     labels = scales::comma) +
  ggtitle("fatalities over the years due to conflicts",
          subtitle = "Data about fatalities happening every year due to conflicts") +
  unhcr_theme()


# Data about fatalities which happened in 2019 per month #####
acled.data %>%
  filter(year==2019)%>%
  group_by(yearmonth)%>%
  summarise(total=sum(fatalities)) %>%
  ggplot(aes(x=as.factor(yearmonth),
             y=total,
             fill=-total)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Month of 1999") +
  scale_y_continuous(name = "Total Fatalities",
                     labels = scales::comma) +
  ggtitle("Number of fatalities in 1999",
          subtitle = "Data about fatalities which happened in 2019 per month")

## Frequency Plot for Number of Conflicts
acled.data %>%
  group_by(yearmonth) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = yearmonth ,
             y=count
             #,fill=-count
  )) +
  geom_bar(stat = "identity",
           color= unhcr_blue,
           fill = unhcr_blue) +
  # scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total Number of Conflicts",labels = scales::comma) +
  ggtitle("Frequency Plot for Number of Conflicts",
          subtitle = "Data about number of conflicts") +
  unhcr_theme()


# Frequency Plot for Number of Conflicts ####
acled.data %>%
  filter(., region == "South America"
         # "Caribbean"
         #"Central America"
  )  %>%
  filter(., ! ( country %in% c("French Guiana",
                               "Falkland Islands")))  %>%
  group_by(yearmonth, country)%>%
  summarise(count=n())%>%
  arrange(desc(count)) %>%

  ggplot(aes(x = yearmonth ,y=count #,fill=-count
  )) +
  geom_bar(stat = "identity",
           color= unhcr_blue,
           fill = unhcr_blue) +
  # scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total Number of Conflicts",labels = scales::comma) +
  ggtitle("Frequency Plot for Number of Conflicts",
          subtitle = "Data about number of conflicts which happened") +
  facet_wrap(~ country, ncol = 3) +
  unhcr_theme()


### Violence per Actor and Year ###
acled.data %>%
  filter(., region == "South America"
         # "Caribbean"
         #"Central America"
  )  %>%
  filter(., ! ( country %in% c("French Guiana",
                               "Falkland Islands")))  %>%
  ggplot(mapping = aes(x=year, group=event_type, color=event_type)) +
  geom_smooth(stat = "count") +
  labs(x = "Year",
       y = "Number of Events",
       title = "Violence per Actor and Year",
       color="Actor Instigating Conflict") +
  facet_wrap(~ country, ncol = 3) +
  #facet_grid(vars(type), vars(country_region)) +
  unhcr_theme()


## Top 10 countries with highest number of conflicts in 2019 ####
acled.data %>%
  filter(year==2019) %>%
  group_by(country) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(n=10) %>%
  arrange(desc(count))%>%
  ggplot(aes(x = reorder(country,count), y=count #,fill=-count
  )) +
  geom_bar(stat = "identity", fill = unhcr_blue) +
  scale_x_discrete(name = "country") +
  scale_y_continuous(name = "Total Number of Conflicts",labels = scales::comma) +
  ggtitle("Frequency Plot for Number of Conflicts  in 2019",
          subtitle = "Top 10 countries with highest number of conflicts in 2019")+
  coord_flip() +
  unhcr_theme()


### Map of events #####
#creating dataset with desired Latitude and longtitude
data = df1 %>%
  filter(year==2019 &
           #   country=="Venezuela") %>%
           country %in% c("Venezuela", "Colombia")) %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longtitude))

#defining median location
center_lon = median(data$longtitude)
center_lat = median(data$latitude)

#creating color palette
palette_color <- colorFactor(c("red","blue","green","yellow","orange"), data$country)

#creating map
leaflet(data) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude,#radius = ~(),
             color = ~palette_color(country))%>%

  #setting the location of the map
  setView(lng=center_lon, lat=center_lat,zoom = 5) %>%

  #adding a minimap
  addMiniMap("bottomright", width = 150, height = 150,
             collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
             zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = FALSE,
             toggleDisplay = FALSE, autoToggleDisplay = FALSE, minimized = FALSE,
             aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
             shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE,
                                      opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap",
                                                                                    showText = "Show MiniMap"), tiles = NULL, mapOptions = list())



##Text Analysis ######
# A text analysis on the notes column can help us understand which words were used the most in the text.
notes_source<-VectorSource(acled.data$NOTE)
notes_corpus<-VCorpus(notes_source)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"Top200Words"))
  return(corpus)
}

notes_clean<-clean_corpus(notes_corpus)
notes_tdm<-TermDocumentMatrix(notes_clean)
notes_m<-as.matrix(notes_tdm)
notes_words<-rowSums(notes_m)
notes_words<-sort(notes_words,decreasing=TRUE)

notes_freqs <-data.frame(term = names(notes_words),
                         num = notes_words)

wordcloud(notes_freqs$term,
          notes_freqs$num,
          max.words = 30,
          colors = c("chartreuse", "cornflowerblue", "darkorange"))

