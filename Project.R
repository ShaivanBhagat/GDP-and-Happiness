# Load all essential libraries which might be used for analyses here
library(tidyverse)
library(lubridate)
library(readr)
library(rvest)
library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(ggmap)
library(leaflet)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(reshape2)
library(corrgram)
library(corrplot)
library(gridExtra)
library(rworldmap)
library(ggalt)
library(RColorBrewer)
library(plot3D)
library(maps)
library(mapdata)

# Read in files which were obtained from Kaggle based on 
# the sources "World Bank Group" and "The World Happiness Report survey" and 
# storing them as a variable
# The data sets from 2015-2019 will go through similar steps when doing this

#The data set of 2015 from the WHR survey 
#This data set will be cleaned and variables that are unnecessary are removed
data2015 <- read.csv("Data/2015.csv") |>  
  select(-Standard.Error, 
         -Dystopia.Residual, 
         -Region)

#A rear column is added to make merging easier later on 
data2015$year <- 2015

#Remove any missing values or N/A values from the data set
missing_values <- any(is.na(data2015))
  if (missing_values) {
    data2015 <- na.omit(data2015)
  }
  
#The 4 following data loading will follow the same steps and therefore
#will not be elaborated on in detail.
#The following data sets will all be formatted to match data2015 for merging purposes
#the number of variables will be the same in all data sets from the WHR

#The data set of 2016 from the WHR survey
data2016 <- read.csv("Data/2016.csv") |>  
  select(-Lower.Confidence.Interval, 
         - Upper.Confidence.Interval, 
         - Dystopia.Residual, 
         - Region)

data2016$year <- 2016

missing_values <- any(is.na(data2016))
  if (missing_values) {
    data2016 <- na.omit(data2016)
  }

#The data set of 2017 from the WHR survey
#Filtering the data to match the format of the other csv files since the columns 
#contain to many variables, spaces, etc in one.
data2017 <- read.csv("Data/2017.csv", header = TRUE, quote = "")
  colnames(data2017) <- gsub("^X\\.|\\.\\.?$", "", colnames(data2017))
  colnames(data2017) <- gsub("^\\.|\\.$", "", colnames(data2017))

#Converting the "Happiness Rank" to a double from a character
data2017$Happiness.Rank <- as.double(data2017$Happiness.Rank)
  
data2017 <- data2017 |>  
    select(-Whisker.high, 
           - Whisker.low, 
           -Dystopia.Residual)
  
# # data2017$Happiness.Rank <- as.integer(data2017$Happiness.Rank)

data2017$year <- 2017
  
missing_values <- any(is.na(data2017))
  if (missing_values) {
    data2017 <- na.omit(data2017)
  }
  
#The data set of 2018 from the WHR survey
#Changing from character to double
data2018 <- read.csv("Data/2018.csv")
  data2018$Perceptions.of.corruption <- as.double(data2018$Perceptions.of.corruption)
 
#Renaming to match the variable headers in the other data frames
data2018 <- data2018 |>  
    rename(
      Happiness.Rank = "Overall.rank",
      Country = "Country.or.region",
      Happiness.Score = "Score",
      Economy..GDP.per.Capita = "GDP.per.capita", 
      Health..Life.Expectancy = "Healthy.life.expectancy",
      Freedom = "Freedom.to.make.life.choices",
      Trust..Government.Corruption = "Perceptions.of.corruption"
    )
  
data2018$year <- 2018
  missing_values <- any(is.na(data2018))
  if (missing_values) {
    data2018 <- na.omit(data2018)
  }

#The data set of 2019 from the WHR survey  
data2019 <- read.csv("Data/2019.csv")

#Renaming to match the variable headers in the other data frames
data2019 <- data2019 |> 
  rename(
    Happiness.Rank = "Overall.rank",
    Country = "Country.or.region",
    Happiness.Score = "Score",
    Economy..GDP.per.Capita = "GDP.per.capita", 
    Health..Life.Expectancy = "Healthy.life.expectancy",
    Freedom = "Freedom.to.make.life.choices",
    Trust..Government.Corruption = "Perceptions.of.corruption"
  )

data2019$year <- 2019
  missing_values <- any(is.na(data2019))
  if (missing_values) {
    data2019 <- na.omit(data2019)
  }

#The data set on countries and their 3 letter code
country_code <- read.csv("Data/country_codes.csv")

#The worldwide GDP data set starting from 1960 
gdp <- read.csv("Data/gdp_data.csv") |>  
  rename(
    gdp = "value")

#Combining all the WHR data sets by vertically stacking them
combined_data_year <- bind_rows(data2015, 
                                data2016, 
                                data2017, 
                                data2018, 
                                data2019) 

#Grouping based on country and removing NA values in the combined data
#Summarizing since there are several years for the same country, and only 
#the first observation for each year in the data set will be included.
combined_data_country <- combined_data_year |> 
  group_by(Country) |> 
  summarise(
    Happiness.Rank = first(na.omit(Happiness.Rank)),
    Happiness.Score = first(na.omit(Happiness.Score)),
    Economy..GDP.per.Capita. = first(na.omit(Economy..GDP.per.Capita.)),
    Family = first(na.omit(Family)),
    Health..Life.Expectancy. = first(na.omit(Health..Life.Expectancy.)),
    Freedom = first(na.omit(Freedom)),
    Trust..Government.Corruption. = first(na.omit(Trust..Government.Corruption.)),
    Generosity = first(na.omit(Generosity)),
    Economy..GDP.per.Capita = first(na.omit(Economy..GDP.per.Capita)),
    Health..Life.Expectancy = first(na.omit(Health..Life.Expectancy)),
    Trust..Government.Corruption = first(na.omit(Trust..Government.Corruption)),
    Social.support = first(na.omit(Social.support))
  )

#Combing gdp data and countrycodes based on the common variable country code
combined_data_gdp <- merge(gdp, country_code, by = "country_code")

#Create a scatterplot showing the relationship between happiness score and GDP per capita, 
#with points colored based on life expectancy. It also includes a linear regression line to visualize the trend in the data. 
ggplot(combined_data_year, aes(x = Happiness.Score, 
                               y = Economy..GDP.per.Capita, 
                               color = Health..Life.Expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Happiness Score", y = "GDP per Capita")

#Create a scatterplot showing the relationship between happiness score and healthy life expectancy, 
#with points colored based on GDP per capita. It also includes a linear regression line to visualize the trend in the data.
ggplot(combined_data_year, aes(x = Happiness.Score, 
                               y = Health..Life.Expectancy, 
                               color = Economy..GDP.per.Capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Happiness Score", y = "Healthy Life Expectancy")

#Create scatterplot showing the relationship between healthy life expectancy and GDP per capita, 
#with points colored based on happiness rank. It also includes a linear regression line to visualize the trend in the data.
ggplot(combined_data_year, aes( x = Health..Life.Expectancy,
                                y = Economy..GDP.per.Capita, 
                                color = Happiness.Rank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Happiness Score", y = "Healthy Life Expectancy")



#Filters numeric columns from 2015, and create the correlation matrix for this year, 
#and then create a color-coded correlation plot to visualize the relationships between the variables.
data2015_filtered <- data2015 |> 
    select(-year)
Num.cols <- sapply(data2015_filtered, is.numeric)
Cor.data <- cor(data2015_filtered[, Num.cols])
corrplot(Cor.data, method = 'color') 

#Same as the previous step but then for 2019 data 
data2019_filtered <- data2019 |> 
    select(-year)
Num.cols <- sapply(data2019_filtered, is.numeric)
Cor.data <- cor(data2019_filtered[, Num.cols])
corrplot(Cor.data, method = 'color') 

#Look at the top 10 countries for each year
top10_2015<-data2015 |>  
    select(Country,Happiness.Rank,Happiness.Score) |>  
    head(n=10)

top10_2016<-data2016 |>  
    select(Country,Happiness.Rank,Happiness.Score) |>  
    head(n=10)

top10_2017<-data2017 |>  
    select(Country,Happiness.Rank,Happiness.Score) |>  
    head(n=10)

top10_2018<-data2018 |>  
    select(Country,Happiness.Rank,Happiness.Score) |>  
    head(n=10)

top10_2019<-data2019 |>  
    select(Country,Happiness.Rank,Happiness.Score) |>  
    head(n=10)

#Create a bar chart showing the happiness ranks of the top 10 countries in each year.
g1 <- ggplot(top10_2015,
           aes(x=factor(Country,levels=Country),
               y=Happiness.Rank))+
    geom_bar(stat="identity",
             width=0.5,
             fill="navyblue") +
    theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
    labs(title="2015",x="Country",y="Rank")

g2 <- ggplot(top10_2016,
           aes(x=factor(Country,levels=Country),
               y=Happiness.Rank)) +
    geom_bar(stat="identity",
             width=0.5,fill="darkred")+
    theme(axis.text.x = element_text(angle=90, vjust=0.6))+
    labs(title="2016",x="Country",y="Rank")

g3 <- ggplot(top10_2017,
           aes(x=factor(Country,levels=Country),
               y=Happiness.Rank))+
    geom_bar(stat="identity",
             width=0.5,fill="orange")+
    theme(axis.text.x = element_text(angle=90, vjust=0.6))+
    labs(title="2017",x="Country",y="Rank")

g4 <- ggplot(top10_2018,
           aes(x=factor(Country,levels=Country),
               y=Happiness.Rank))+
    geom_bar(stat="identity",
             width=0.5,fill="pink")+
    theme(axis.text.x = element_text(angle=90, vjust=0.6))+
    labs(title="2018",x="Country",y="Rank")

g5 <- ggplot(top10_2019,
           aes(x=factor(Country,levels=Country),
               y=Happiness.Rank))+
    geom_bar(stat="identity",width=0.5,fill="purple")+
    theme(axis.text.x = element_text(angle=90, vjust=0.6))+
    labs(title="2019",x="Country",y="Rank")

#Create one visualization combining all the bar charts of the previous step for better comparison
grid.arrange(g1, g2, g3,g4,g5, ncol = 5,nrow=1)


#Use country names and GDP per capita from the 2015 data and create a new variable
#which can be used for a geospatial visualization
data_geospatial_2015 <- data.frame(
  country=data2015$Country,
  value=data2015$Economy..GDP.per.Capita.)

#Repeat the previous step for 2019
data_geospatial_2019 <- data.frame(
    country=data2019$Country,
    value=data2019$Economy..GDP.per.Capita)

#Using a join to link the geospatial data from the previous step with a map based on country names.
node_2015 <- joinCountryData2Map(data_geospatial_2015, joinCode="NAME", 
                         nameJoinColumn="country")

#Same as the previous step but for 2019
node_2019 <- joinCountryData2Map(data_geospatial_2019, joinCode="NAME", 
                                 nameJoinColumn="country")

#Create a world map visualization based on GDP per capita data for the year 2015, using node_2015. 
#The countries without a color have no available data
mapCountryData(node_2015, nameColumnToPlot="value", 
               mapTitle="World Map for GDP per Capita-2015",
               colourPalette="terrain")

#Same as the previous step but then for 2019
mapCountryData(node_2019, nameColumnToPlot="value", 
               mapTitle="World Map for GDP per Capita-2019",
               colourPalette="terrain")

#Create a new variable for data2015 and data2019 containing only country and happiness score
d2015 <- data2015 |>  
    select(Country,HS15=Happiness.Score)
d2019 <- data2019 |>  
    select(Country,HS16=Happiness.Score)

#Join the two new variables and calculate the difference in happiness scores between 2015 and 2019
#After this filter the data to retain only the rows where there's a positive increase
score <- inner_join(d2015,d2019) |>  
    mutate(score_diff= HS16-HS15) |>  
    filter(score_diff>0)

# Create a dumbell plot comparing hapiness scores for different countries between 2015 and 2019
score$Country <- factor(score$Country, levels=as.character(score$Country))
gg <- ggplot(score, aes(x=HS15, xend=HS16, y=Country, group=Country)) + 
  geom_dumbbell(size=2, color="#e3e2e1", 
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x=NULL, 
       y=NULL, 
       
       title=" Country Happiness Scores Increased: 2015 vs 2019"
  ) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)

#Create a scatterplot with dots colored and sized based on GDP per capita values, 
#as well as a smooth line representing the relationship between life expectancy and 
#GDP per capita. Country names are also labeled on the plot.
myPalette <- colorRampPalette(rev(brewer.pal(7, "Accent")))
ggplot(combined_data_country,aes(x=(Health..Life.Expectancy.*100),y=Economy..GDP.per.Capita.))+
  geom_point(aes(size=Economy..GDP.per.Capita.,color=Economy..GDP.per.Capita.),alpha=.7)+
  theme(legend.position="none")+ geom_smooth()+
  scale_colour_gradientn(colours = myPalette(100), values=seq(0, 100, length.out=100)/100) + 
  geom_text(aes(label=Country), size=3,color="black")+
  labs(title="Economy vs Life Expectancy",x="Life Expectany")

#creates a scatter plot showing the relationship between life expectancy and GDP per capita, 
#with points colored and sized based on GDP per capita values, a smoothed line indicating the trend, 
#and country names labeled on the plot. 
#The color gradient of the points is customized using a custom palette.
ggplotRegression <- function (fit) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(shape=1,size=3,color="#003399")+
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#Combining several variables that affect happiness data and create a scatterplot
#This 3d plot visualized the relationship between life expectancy and GDP per capita, 
# and freedom to analazye aspects that can influence happiness score.
scatter3D(combined_data_country$Freedom, 
          combined_data_country$Health..Life.Expectancy, 
          combined_data_country$Happiness.Score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness data", xlab = "Freedom",
          ylab ="Life.Expectancy", zlab = "Happiness.Score")

#Converting year in the data set to numeric and storing this in the same variable
combined_data_gdp$year <- as.numeric(combined_data_gdp$year)

#Joining two data sets by country and year
combined_data_final <- inner_join(combined_data_year, 
                                  combined_data_gdp, 
                                  by = c("Country" = "country_name", "year"))

#Create a scatterplot that visualizes the relationship between GDP and hapiness score.
#Log is used to visualize the GDP better since it is less scattered
#Each point represents a country data point with the position indicating the countries
#GDP and happiness score values
scatter_plot <- ggplot(combined_data_final, 
                       aes(x = log(gdp), 
                           y = Happiness.Score)) +
  geom_point() +
  labs(x = "GDP", y = "Happiness Score") +
  ggtitle("Scatter Plot of GDP vs Happiness Score")

# Convert ggplot to plotly for interactivity
scatter_plot_interactive <- ggplotly(scatter_plot)

# Display the plots
scatter_plot_interactive

# Plotting function for geospatial map
plot_gdp_happiness_map <- function(year) {

# Filter data for the specific year
data <- combined_data_final[combined_data_final$year == year, ]
  
# Merge data with map data
world <- map_data("world")
merged_data <- merge(world, data, by.x = "region", by.y = "Country", all.x = TRUE)
  
#Create the geospatial map
plot <- plot_ly(merged_data, 
                  z = ~Happiness.Score,
                  locations = ~region,
                  locationmode = "country names",
                  type = "choropleth",
                  colors = "Blues",
                  text = ~paste("Country: ", region, "<br>",
                                "GDP: ", gdp, "<br>",
                                "Happiness Score: ", Happiness.Score),
                  hoverinfo = "text")
  
# Customize layout for better visualisation
plot <- plot |>  layout(title = paste("World Happiness Score and GDP -", year),
                          geo = list(showframe = FALSE, showcoastlines = TRUE))
  
  return(plot)
}

# Plot the geospatial map for 2015 and 2019
plot_gdp_happiness_map(2015)
plot_gdp_happiness_map(2019)

# Convert GDP column to numeric
combined_data_final$gdp <- as.numeric(combined_data_final$gdp)

#Plot GDP vs Happiness Score over the years
#log gdp is used to get a better distribution
ggplot(combined_data_final, 
       aes(x = log(gdp), 
           y = Happiness.Score, 
           color = factor(year))) +
  geom_point() +
  scale_color_discrete(name = "Year") +
  labs(title = "GDP vs Happiness Score Over the Years",
       x = "GDP",
       y = "Happiness Score") +
  theme_minimal()

#Conclusion: ?