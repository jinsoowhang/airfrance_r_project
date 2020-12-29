# Calling libraries needed for script
library(readxl)
library(ggplot2)
library(plotly) 
library(data.table) 
library(dplyr)

# Getting the Air France dataset and defining into a variable name 
my_af <- read_excel("C:/Users/jwtre/OneDrive/Desktop/Hult/R/R Team Project/Air France Case Spreadsheet Supplement.xls", 
                    sheet = "DoubleClick")

# Viewing the dataset
View(my_af)
glimpse(my_af)

#Looking at the first 5 rows of the dataset
head(my_af)

#Finding the class of the dataset
class(my_af)

#Checking all the variable names
colnames(my_af)

#Exploring the summary statistics of each variable
summary(my_af)

##########################################----------------------------------------------------
########### Data Massaging ###############----------------------------------------------------
##########################################----------------------------------------------------


### Col names transformation for consistency

# Setting " " as "_" in col names for the future analysis
names(my_af) <- gsub(" ", "_", names(my_af))
colnames(my_af)

# Making all col names consistent /  Removing "%" sign
colnames(my_af)[which(names(my_af) == "Total_Cost/_Trans.")] <- "Total_Cost_Each_Trans"
colnames(my_af)[which(names(my_af) == "Engine_Click_Thru_%")] <- "Engine_Click_Through_Prct"
colnames(my_af)[which(names(my_af) == "Trans._Conv._%")] <- "Trans_Conv_Prct"
colnames(my_af)[which(names(my_af) == "Avg._Pos.")] <- "Avg_Pos"
colnames(my_af)[which(names(my_af) == "Avg._Cost_per_Click")] <- "Avg_Cost_Each_Click"
colnames(my_af)


### Class \ Type check 

# Setting my_af as data frame just in case
my_af <- as.data.frame(my_af)

# Setting characters as factors
my_af[sapply(my_af, is.character)] <- lapply(my_af[sapply(my_af, is.character)], 
                                             as.factor) ## All variables or only publisher???????

### Missing Value Detection / Replacement

# Detecting the number of missing values
sum(is.na(my_af))

# Removing missing values
my_af_clean <- na.omit(my_af) 

####################################
#### End of massaging for now.  ####
####################################


# Before start plotting the findings,set a theme for consistency

theme_my_af <- function(){ 
  theme_minimal() +
    theme(
      text = element_text(color = "gray25"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}


# Creating an additional data frame without $0s of Booking Value
my_nonzero_clean <- my_af_clean[my_af_clean$Total_Volume_of_Bookings > 0, ]
my_nonzero <- my_af[my_af$Total_Volume_of_Bookings > 0, ]

# Creating a new variable called ROA for my original dataset
my_af$Profit <- my_af$Amount - my_af$Total_Cost
my_af$ROA <- my_af$Profit / my_af$Total_Cost

# Creating the same variables for my original clean dataset
my_af_clean$Profit <- my_af_clean$Amount - my_af_clean$Total_Cost
my_af_clean$ROA <- my_af_clean$Profit / my_af_clean$Total_Cost

# Creating the same variables for my nonzero dataset 
my_nonzero$Profit <- my_nonzero$Amount - my_nonzero$Total_Cost
my_nonzero$ROA <- my_nonzero$Profit / my_nonzero$Total_Cost


# Creating the same variables for my nonzero and clean dataset 
my_nonzero_clean$Profit <- my_nonzero_clean$Amount - my_nonzero_clean$Total_Cost
my_nonzero_clean$ROA <- my_nonzero_clean$Profit / my_nonzero_clean$Total_Cost



### Categorizing Keyword_Group and create new variable called "new_cat"
rowsinmy <- nrow(my_nonzero_clean) # getting row indexes to use in my loop
my_nonzero_clean$new_cat <- NA # Creating an empty variable

for (i in 1:rowsinmy){    # Looping the catch keywords to categorize them in 4
  if(my_nonzero_clean$Keyword_Group[i] %like% "Air France" ) {
    my_nonzero_clean$new_cat[i] <- "Branded Only"
  } else if (my_nonzero_clean$Keyword_Group[i] %like% "Branded") {
    my_nonzero_clean$new_cat[i] <- "Geo Branded"
  } else if (my_nonzero_clean$Keyword_Group[i] %like% "Sale") {
    my_nonzero_clean$new_cat[i] <- "Branded Only"
  } else if (my_nonzero_clean$Keyword_Group[i] %like% "Discount"){
    my_nonzero$new_cat[i] <- "Branded Only"  
  } else if (my_nonzero_clean$Keyword_Group[i] %like% "Unassigned"){
    my_nonzero_clean$new_cat[i] <- "Others"
  }else {
    my_nonzero_clean$new_cat[i] <- "Geo Only"
  }
} # Closing the i-loop


table(my_nonzero_clean$new_cat) # Checking my new variable


########################### Tables and Plots ###################################################
my_af # Original dataset
my_af_clean # Original dataset without NAs
my_nonzero # Nonzero dataset with NAs
my_nonzero_clean # Nonzero dataset without NAs


###### Keyword Group Plot
my_plot1 <- ggplot(data = my_nonzero_clean, aes(x = my_nonzero_clean$Engine_Click_Through_Prct ,
                                          y= my_nonzero_clean$Trans_Conv_Prct , color = new_cat)) +
  geom_point(aes(alpha = 1), shape = 5) + labs(title = "Keywords - Click Through vs Conversion",
                                               x = "Conversion Rate", y = "Click Through Rate") + theme_my_af()

ggplotly(my_plot1)


#########################################
### Pivot Table for Match Type with averages ##
my_table1 <- my_af_clean %>%
  group_by(Match_Type) %>%
  summarize(avgClick = round(mean(Clicks)), avgCost = mean(Avg_Cost_Each_Click), avgConv = mean(Trans_Conv_Prct),
            avgBook = mean(Total_Volume_of_Bookings)) %>%
  arrange(desc(avgBook)) %>%
  head(10)

my_table1

# Plot for Match Type by Average Clicks vs Average Conversion Rate 
plot_ly(my_table1, x = ~avgConv, y = ~avgClick,
        textposition = "auto",
        type = 'scatter', 
        mode = 'markers', 
        size = ~avgClick/avgConv, 
        color = ~Match_Type, 
        colors = 'Paired',
        marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Match Type by Average Clicks vs Conversion',
         xaxis = list(title = "Average Conversion Rate", showgrid = TRUE),
         yaxis = list(title = "Average Clicks", showgrid = TRUE),
         showlegend = TRUE) 


# Table for Match Type

my_table2 <- my_af %>%
  group_by(Match_Type) %>%
  summarize(avgClick = round(mean(Clicks)), avgCost = mean(Avg_Cost_Each_Click), avgConv = mean(Trans_Conv_Prct),
            avgBook = mean(Total_Volume_of_Bookings), avgcostpertrans =mean(Total_Cost_Each_Trans), profit = mean(Profit), roa = mean(ROA)) %>%
  arrange(desc(avgBook)) %>%
  head(10)

View(my_table2)

# Just a modify version of the table above for presenting
my_table3 <- my_af %>%
  group_by(Match_Type) %>%
  summarize(avgClick = round(mean(Clicks)), avgConv = mean(Trans_Conv_Prct),
            avgBook = mean(Total_Volume_of_Bookings), avgcostpertrans =mean(Total_Cost_Each_Trans), profit = mean(Profit), roa = mean(ROA)) %>%
  arrange(desc(avgBook)) %>%
  head(10)

View(my_table3)

my_table_camp <- my_af %>%
  group_by(Campaign) %>%
  summarize(avgROA = mean(ROA)) %>%
  arrange(desc(avgROA)) %>%
  tail(23)

View(my_table_camp)

# Plot for Campaign vs ROA
ggplot(my_table_camp, aes(x = reorder(Campaign, -avgROA), y = avgROA)) + 
  geom_col(fill = "pink") + 
  labs(x = "Campaign", y = "ROA (Average)", title = "ROA by Campaign") + 
  theme_my_af() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Table for Publisher Name vs ROA
my_table4 <- my_nonzero %>%
  group_by(Publisher_Name) %>%
  summarize(avgROA = mean(ROA), avgClickThru = mean(Engine_Click_Through_Prct),avgConv = mean(Trans_Conv_Prct)) %>%
  arrange(desc(avgROA)) %>%
  head(10)

my_table4

# Plot for Publisher Name vs ROA
ggplot(my_table4, aes(x = reorder(Publisher_Name, -avgROA), y = avgROA), fill = Publisher_Name) + 
  geom_col(fill = "lightblue") + 
  labs(x = "Publisher_Name", y = "ROA (Average)", title = "Publishers by Average ROA (Return on Advertisement)") + 
  theme_my_af() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#########################################
### Pivot Table for Average Click Through Rate by Publisher Name 

my_table5 <- my_af %>%
  group_by(Publisher_Name) %>%
  summarize(avgClickThrough = mean(Engine_Click_Through_Prct), avgConv = mean(Trans_Conv_Prct)) %>%
  arrange(desc(avgClickThrough)) %>%
  head(10)

my_table5

########################################
### Plot on Publisher Name vs Amount

my_plot2 <- ggplot(data = my_af, aes(x = Publisher_Name,
                                     y= Amount)) +
  geom_point(aes(alpha = 1), shape = 5) + labs(title = "Keywords - Amount vs Publisher Name",
                                               x = "Publisher Name", y = "Amount") + theme_my_af()

ggplotly(my_plot2) #After plotting, zoom in the outlier in Yahoo US to point out that it does better than Google US


###### Status: Live vs Paused

# Table for Status = "Live"
Live_status <- my_af %>%
  group_by(Publisher_Name) %>%
  filter(Status == "Live") %>%
  summarize(avgROA = mean(ROA), avgClickThru = mean(Engine_Click_Through_Prct),avgConv = mean(Trans_Conv_Prct)) %>%
  arrange(desc(avgROA)) %>%
  head(10)
trans_Live <- t(Live_status)

# Table for Status = "Paused"
Paused_status <- my_af %>%
  group_by(Publisher_Name) %>%
  filter(Status == "Paused") %>%
  summarize(avgROA = mean(ROA), avgClickThru = mean(Engine_Click_Through_Prct),avgConv = mean(Trans_Conv_Prct)) %>%
  arrange(desc(avgROA)) %>%
  head(10)


#### Plots
library(tidyr)      # library for gather function
library(gridExtra)  # library for binding the 2 plots in 1 grid
library(grid)       # library for binding the 2 plots in 1 grid

## Live_Status: Filters with Publisher name
clustered_live <- gather(Live_status, type, value, -Publisher_Name)

LivePlot <- ggplot(clustered_live, aes(type, value)) + 
  geom_bar(aes(fill = Publisher_Name), stat = "identity", position = "dodge")+
  theme_my_af()+
  labs(x = "Parameters",title = "Status: LIVE")

## Paused_Status: Filters with Publisher name
clustered_paused <- gather(Paused_status, type , value, -Publisher_Name)

PausedPlot <- ggplot(clustered_paused, aes(type, value)) + 
  geom_bar(aes(fill = Publisher_Name), stat = "identity", position = "dodge")+
  theme_my_af()+
  labs(x = "Parameters",title = "Status: PAUSED")

# Plotting 2 graphs in the same grid
grid.arrange(LivePlot, PausedPlot, nrow = 1)




