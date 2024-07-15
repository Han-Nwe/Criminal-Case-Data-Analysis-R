library(tidyverse)
library(data.table)
library(visdat)
library(dplyr)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(factoextra)
library(tidyr)
library(scales)
library(pastecs)
library(caret)

library(class)

library(caret)
library(randomForest)
#-----------------------------------------------------------------------------
#2014_Mereching
#-----------------------------------------------------------------------------
#import files
folder_path <- "/Users/hannwenyein/Desktop/my text/202/Dataset/2014" 
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
csv_files

for (file in csv_files) {
  month <- tolower(gsub(".csv", "", as.list(strsplit(file, "_")[[1]])[4]))
  data <- read.csv(file)
  data$month <- month
  data$year  <- 2014
  write.csv(data, file, row.names = FALSE)
}

setwd("/Users/hannwenyein/Desktop/my text/202/Dataset/2014")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist(temp)
write.csv(data,file = "combined_2014.csv", row.names = FALSE)

data_2014 = read_csv('/Users/hannwenyein/Desktop/my text/202/Dataset/2014/combined_2014.csv',col_names = TRUE)
names(data_2014)
#-----------------------------------------------------------------------------
#2015_Mereching
#-----------------------------------------------------------------------------
#import files
folder_path <- "/Users/hannwenyein/Desktop/my text/202/Dataset/2015" 
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
csv_files

for (file in csv_files) {
  month <- tolower(gsub(".csv", "", as.list(strsplit(file, "_")[[1]])[4]))
  data <- read.csv(file)
  data$month <- month
  data$year  <- 2015
  write.csv(data, file, row.names = FALSE)
}

setwd("/Users/hannwenyein/Desktop/my text/202/Dataset/2015")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist(temp)
write.csv(data,file = "combined_2015.csv", row.names = FALSE)

data_2015 = read_csv('/Users/hannwenyein/Desktop/my text/202/Dataset/2015/combined_2015.csv',col_names = TRUE)
names(data_2015)

#-----------------------------------------------------------------------------
#2016_Mereching
#-----------------------------------------------------------------------------
#import files
folder_path <- "/Users/hannwenyein/Desktop/my text/202/Dataset/2016" 
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
csv_files

for (file in csv_files) {
  month <- tolower(gsub(".csv", "", as.list(strsplit(file, "_")[[1]])[4]))
  data <- read.csv(file)
  data$month <- month
  data$year  <- 2016
  write.csv(data, file, row.names = FALSE)
}

setwd("/Users/hannwenyein/Desktop/my text/202/Dataset/2016")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist(temp)
write.csv(data,file = "combined_2016.csv", row.names = FALSE)

data_2016 = read_csv('/Users/hannwenyein/Desktop/my text/202/Dataset/2016/combined_2016.csv',col_names = TRUE)
names(data_2016)
data_2016$month
data_2016$year

#-----------------------------------------------------------------------------
#2017_Mereching
#-----------------------------------------------------------------------------
#import files
folder_path <- "/Users/hannwenyein/Desktop/my text/202/Dataset/2017" 
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
csv_files

for (file in csv_files) {
  month <- tolower(gsub(".csv", "", as.list(strsplit(file, "_")[[1]])[4]))
  data <- read.csv(file)
  data$month <- month
  data$year  <- 2017
  write.csv(data, file, row.names = FALSE)
}

setwd("/Users/hannwenyein/Desktop/my text/202/Dataset/2017")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist(temp)
write.csv(data,file = "combined_2017.csv", row.names = FALSE)

data_2017 = read_csv('/Users/hannwenyein/Desktop/my text/202/Dataset/2017/combined_2017.csv',col_names = TRUE)
names(data_2017)
data_2017$month
data_2017$year

#-----------------------------------------------------------------------------
#2018_Mereching
#-----------------------------------------------------------------------------
#import files
folder_path <- "/Users/hannwenyein/Desktop/my text/202/Dataset/2018" 
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
csv_files

for (file in csv_files) {
  month <- tolower(gsub(".csv", "", as.list(strsplit(file, "_")[[1]])[4]))
  data <- read.csv(file)
  data$month <- month
  data$year  <- 2018
  write.csv(data, file, row.names = FALSE)
}

setwd("/Users/hannwenyein/Desktop/my text/202/Dataset/2018")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist(temp)
write.csv(data,file = "combined_2018.csv", row.names = FALSE)

data_2018 = read_csv('/Users/hannwenyein/Desktop/my text/202/Dataset/2018/combined_2018.csv',col_names = TRUE)
names(data_2018)
data_2018$month
data_2018$year
#-----------------------------------------------------------------------------
# Merge the five data frames 
#-----------------------------------------------------------------------------
combined_data <- rbind(data_2014,data_2015, data_2016, data_2017, data_2018)
combined_data
names(combined_data)
View(combined_data)

#-----------------------------------------------------------------------------
#Data Cleaning    
#-----------------------------------------------------------------------------
#Rename columns
#-----------------------------------------------------------------------------
removing_number_col <- function(dataset){
  colnames(dataset) <- gsub("Number.of.", "", colnames(dataset))
  colnames(dataset) <- gsub("\\.", "_", colnames(dataset))
  colnames(dataset) <- tolower(colnames(dataset))
  return(dataset)
}
combined_data<- removing_number_col(combined_data)
names(combined_data)

removing_conv_col <- function(dataset){
  colnames(dataset) <- gsub("_convictions", "", colnames(dataset))
  return(dataset)
}
combined_data<- removing_conv_col(combined_data)
combined_data <- combined_data %>% rename(country = 'x')
names(combined_data)

#-----------------------------------------------------------------------------
#Removing Percentage and other Columns form datasets
#-----------------------------------------------------------------------------
percentage_cols <- grep("Percentage", names(combined_data), ignore.case = TRUE)
combined_data <- combined_data[, -percentage_cols]

combined_data <- combined_data %>% select(-c(offences_against_the_person, offences_against_the_person_unsuccessful,
                                             public_order_offences, public_order_offences_unsuccessful,
                                             motoring_offences,motoring_offences_unsuccessful))
            
                                            
names(combined_data)

write.csv(combined_data, row.names = FALSE)
names(combined_data)
View(combined_data)

#-----------------------------------------------------------------------------
#Add Date columns and sort by date
#-----------------------------------------------------------------------------
combined_data$date <- as.Date(paste(combined_data$year, 
                                    combined_data$month, "01", sep = "-"), "%Y-%b-%d")
combined_data <- combined_data[order(combined_data$date),]

names(combined_data)
View(combined_data)
unique(combined_data$country)


glimpse(combined_data)

#-----------------------------------------------------------------------------
#Checking missing values
#-----------------------------------------------------------------------------
vis_miss(combined_data)
vis_dat(combined_data)

#-----------------------------------------------------------------------------
#EDA
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#Data Transformation
#-----------------------------------------------------------------------------
crimes <- combined_data[, c("country","homicide", "sexual_offences","burglary","robbery",
                            "theft_and_handling","fraud_and_forgery", "criminal_damage", 
                            "drugs_offences","month", "year","date")]
unsuccessful_crimes <- combined_data[, c("country","month", "year","date","country","homicide_unsuccessful","sexual_offences_unsuccessful",
                                         "burglary_unsuccessful", "robbery_unsuccessful", "theft_and_handling_unsuccessful",
                                         "criminal_damage_unsuccessful", "fraud_and_forgery_unsuccessful", "criminal_damage",
                                         "drugs_offences_unsuccessful")]

num_data <- crimes %>% select(-c(country, month,date,year))

#----------------------------------------------------------------------------
#Descriptive Analysis
#-----------------------------------------------------------------------------
#names(combined_data)

summary(combined_data)

#Coviance and Correlation
num_data <- combined_data %>% select(-c(country,year,month,date))
rescov = cov(num_data, method = "pearson")
rescov

res <- cor(num_data)
res
round(res,2)

# Visualize correlation matrix
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 80)
corrplot(res2$r, type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")

# Correlation matrix with significance levels (p-value)
rcorr(res, type = "pearson")
res2 <- rcorr(as.matrix(cov_crimes)) 
res # Correlation matrix 
res2 # Significance Level

# Extract the correlation coefficients
res2$r

# Extract p-values
res2$P

flattenCorrMatrix <- function(cormat, pmat) { 
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]], 
    column = rownames(cormat)[col(cormat)[ut]],
    cor =(cormat)[ut], 
    p = pmat[ut]
  )
} 
res2<-rcorr(as.matrix(num_data)) 
flattenCorrMatrix(res2$r, res2$P)

#Heap Map
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)
#--------------------------------
#Visualization
#Total Conviction Data
# Combine the crime variables into a single dataframe
crime_data_stacked <- combined_data %>%
  select(country, homicide, sexual_offences, burglary, robbery, theft_and_handling, 
         fraud_and_forgery, criminal_damage, drugs_offences, month, year, 
         date)%>% filter(country != "National")

# Reshape the data into long format
crime_data_long <- crime_data_stacked %>%
  pivot_longer(cols = -c(country, month, year, date), names_to = "crime_type", values_to = "crime_count")

# Create the grouped bar chart
ggplot(crime_data_long, aes(x = year, y = crime_count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Years", y = "Crime Count", fill = "Crime Type") +
  ggtitle("Crime Count by Country and Crime Type")
#--------------------------------
#For Unsuccessful Data
# Combine the unsuccessful crime variables into a single dataframe
unsuccessful_crime_data_stacked <- combined_data %>%
  select(country,homicide_unsuccessful,sexual_offences_unsuccessful,burglary_unsuccessful,
         robbery_unsuccessful,theft_and_handling_unsuccessful,criminal_damage_unsuccessful,
         fraud_and_forgery_unsuccessful, drugs_offences_unsuccessful,month, year, 
         date)%>% filter(country != "National")

# Reshape the unsuccessful data into long format
crime_unsuccessful_data_long <- unsuccessful_crime_data_stacked %>%
  pivot_longer(cols = -c(country, month, year, date), names_to = "crime_type", values_to = "crime_count")

# Create the grouped bar chart
ggplot(crime_unsuccessful_data_long, aes(x = year, y = crime_count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Years", y = "Crime Count", fill = "Crime Type") +
  ggtitle("Crime Count by Country and Crime Type")

# Create the plot
ggplot(crime_unsuccessful_data_long, aes(x = year, y = crime_count, fill = country)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Crime Count", fill = "Country") +
  ggtitle("Unsuccessful Crime Count by Country and Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Create the plot
ggplot(crime_data_long, aes(x = year, y = crime_count, fill = country)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Crime Count", fill = "Country") +
  ggtitle("Crime Count by Country and Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#--------------------------------
#Linear Regression
#--------------------------------
total_crime <- crime_data_stacked %>%
  group_by(date) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences)) %>%
  mutate(data = "date")




unique(total_crime$data)
names(total_crime)
crime_data <- total_crime[, c("total_crime_rate", "date")]
crime_data <- total_crime[, c("total_crime_rate", "date")]
set.seed(123)
train_produce <- createDataPartition(crime_data$total_crime_rate, p = 0.8, list = FALSE)
train_data <- crime_data[train_produce, ]
summary(train_data)

test_data <- crime_data[-train_produce, ]
summary(test_data)

model <- lm(total_crime_rate ~ date, data = train_data)
summary(model)


plot(model)

predicted <- predict(model, newdata = test_data)
summary(predicted)

plot_data <- data.frame(date = test_data$date, actual = test_data$total_crime_rate, predicted)

ggplot(plot_data, aes(x = date))+
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(x = "date", y = "Total Crime Rate") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  ggtitle("Actual vs. Predicted Total Crime Rate")


#--------------------------------
#Multiple Linear Regression
#--------------------------------
total_crime_mul <- crime_data_stacked %>%
  group_by(date) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            burglary = sum(burglary),
            theft_and_handling = sum(theft_and_handling),
            drugs_offences = sum(drugs_offences))


head(total_crime_mul)
# Select the required columns
crime_data_subset <- total_crime_mul[, c("total_crime_rate","burglary", "theft_and_handling", "drugs_offences")]

# Split the data into training and testing sets
set.seed(123)
train_produce_mul <- sample(1:nrow(crime_data_subset), 0.8 * nrow(crime_data_subset))
train_data <- crime_data_subset[train_produce_mul, ]
summary(train_data)

test_data <- crime_data_subset[-train_produce_mul, ]
summary(test_data)

model <- lm(total_crime_rate ~ burglary + theft_and_handling + drugs_offences, data = train_data)
summary(model)

predicted_values <- predict(model, newdata = test_data)

plot(test_data$total_crime_rate, predicted_values,
     xlab = "Actual Total Crime Rate", ylab = "Predicted Total Crime Rate")
abline(0, 1, col = "red")

#--------------------------------
total_crime_mul <- crime_data_stacked %>%
  group_by(date) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            country = toString(unique(country)),
            month = first(month),
            year = first(year))

unique_levels_country <- unique(train_data$country)
unique_levels_month <- unique(train_data$month)
unique_levels_year <- unique(train_data$year)

print(unique_levels_country)
print(unique_levels_month)
print(unique_levels_year)

# Specify the predictor and response variables
predictors <- c("country", "month", "year")
response <- "total_crime_rate"

# Create the training and testing datasets
train_indices <- createDataPartition(total_crime_mul$total_crime_rate, p = 0.8, list = FALSE)
train_data <- total_crime_mul[train_indices, ]
test_data <- total_crime_mul[-train_indices, ]

model <- lm(total_crime_rate ~ country + month + year, data = train_data)

predictions <- predict(model, newdata = test_data)

plot(test_data$total_crime_rate, predictions, xlab = "Actual Value", ylab = "Predicted Value", main = "Actual vs Predicted", col = "blue")
abline(0, 1, col = "red")

#--------------------------------
total_crime <- crime_data_stacked %>%
  select(country, homicide, sexual_offences, burglary, robbery, theft_and_handling, 
         fraud_and_forgery, criminal_damage, drugs_offences, month, year, 
         date)

total_crime <- crime_data_stacked %>%
  group_by(year) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences))

total_crime <- crime_data_stacked %>%
  group_by(year_group = year) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences)) %>%
  mutate(dayete = as.Date(paste(date, "-01-01", sep = ""), format = "%Y-%m-%d"))

model <- lm(total_crime_rate ~ year, data = total_crime)
summary(model)

predicted <- predict(model, newdata = total_crime)
plot_data <- cbind(total_crime, predicted)

ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = total_crime_rate, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(x = "Years", y = "Total Crime Rate") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  ggtitle("Actual vs. Predicted Total Crime Rate")


# Plotting the data points
ggplot(total_crime, aes(x = year, y = total_crime_rate)) +
  geom_point() +
  labs(x = "Years", y = "Total Crime Rate") +
  ggtitle("Total Crime Rate Over the Years")

# Adding the regression line
ggplot(total_crime, aes(x = year, y = total_crime_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Years", y = "Total Crime Rate") +
  ggtitle("Total Crime Rate Over the Years")
#-------------------------------------------
#Clustering
#-------------------------------------------

crimes_cluster <- crime_data_stacked %>% select(-c(country,year,month,date))
summary(crimes_cluster)

scaled_crimes_data <- scale(crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 3,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 4,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 5,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 6,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)
#-------------------------------------------
# Assuming your crime data is stored in the 'crime_data' dataframe
names(crime_data_stacked)
total_crimes_by_country <- crime_data_stacked %>%
  group_by(country) %>%
  summarize(total = sum(homicide + sexual_offences + burglary + robbery + 
                          theft_and_handling + fraud_and_forgery +criminal_damage+
                          drugs_offences))

total_crimes_by_country <- crime_data_stacked %>%
  group_by(country) %>%
  summarize(total = sum(homicide, sexual_offences, burglary, 
                        robbery, theft_and_handling, fraud_and_forgery,
                        criminal_damage, drugs_offences))

total_crime_hc <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences),
            )

names(total_crime_hc)
um(homicide, sexual_offences, burglary, robbery,
   theft_and_handling, fraud_and_forgery,
   criminal_damage, drugs_offences))

# Subset the necessary columns (e.g., country and crime variables)
crime_subset <- total_crime_hc[, c("homicide", "sexual_offences", "burglary",
                                   "robbery","theft_and_handling","fraud_and_forgery",
                                   "criminal_damage","drugs_offences" )] 


# Remove missing values if necessary
crime_subset <- na.omit(crime_subset)

# Prepare the distance matrix
distance_matrix <- dist(crime_subset[, -1])  # Exclude the 'country' column from the distance calculation

# Perform hierarchical clustering
hclust_result <- hclust(distance_matrix)

# Cut the tree into desired number of clusters (e.g., 3 clusters)
clusters <- cutree(hclust_result, k = 3)

# Add the cluster labels to the original crime_data dataframe
clustered_data <- crime_data_stacked[1:length(clusters), ]

clustered_data$cluster <- clusters
# View the result
print(clustered_data)
# Create a new dataframe with cluster labels and years
clustered_data <- data.frame(Cluster = clusters, Country = total_crime_hc$country)  # Adjust 'Year' column name based on your dataset

# Plot the dendrogram
#plot(hclust_result, hang = -1)

# Perform hierarchical clustering
hclust_result <- hclust(distance_matrix)

# Cut the tree into desired number of clusters (e.g., 3 clusters)
clusters <- cutree(hclust_result, k = 3)

# Add the cluster labels to the original crime_data dataframe
clustered_data <- crime_data_stacked[1:length(clusters), ]

# Rename the cluster labels with country names
clustered_data$cluster <- total_crime_hc$country

# Create a new dataframe with cluster labels and years
clustered_data <- data.frame(Cluster = clusters, Country = clustered_data$cluster)

# Plot the dendrogram with modified labels
plot(hclust_result,labels = clustered_data$Country)

# Add color to the dendrogram based on the assigned clusters
rect.hclust(hclust_result, k = 3, border = 2:4)
#-------------------------------------------
#Hierarchical Clustering
#-------------------------------------------
crime_hc <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences),
  )

crime_subset_hc <- crime_hc[, c("homicide", "sexual_offences", "burglary",
                                   "robbery","theft_and_handling","fraud_and_forgery",
                                   "criminal_damage","drugs_offences" )] 

crime_subset_hc <- na.omit(crime_subset_hc)

# Prepare the distance matrix
#distance_matrix <- dist(crime_subset_hc[, -1]) 
# Perform hierarchical clustering
hclust_crimes <- hclust(distance_matrix)

# Cut the tree into desired number of clusters (e.g., 3 clusters)
clusters <- cutree(hclust_crimes, k = 3)

# Add the cluster labels to the original crime_data dataframe
clustered_data <- crime_data_stacked[1:length(clusters), ]

clustered_data$cluster <- clusters
# View the result
print(clustered_data)
# Create a new dataframe with cluster labels and years
clustered_data <- data.frame(Cluster = clusters, Country = total_crime_hc$country)  # Adjust 'Year' column name based on your dataset


# Perform hierarchical clustering
hclust_crimes <- hclust(distance_matrix)

# Cut the tree into desired number of clusters (e.g., 3 clusters)
clusters <- cutree(hclust_crimes, k = 3)

# Add the cluster labels to the original crime_data dataframe
#clustered_data <- crime_data_stacked[1:length(clusters), ]

# Rename the cluster labels with country names
clustered_data$cluster <- total_crime_hc$country
# Create a new dataframe with cluster labels and years
clustered_data <- data.frame(Cluster = clusters, Country = clustered_data$cluster)
# Plot the dendrogram with modified labels
plot(hclust_result,labels = clustered_data$Country)
# Add color to the dendrogram based on the assigned clusters
rect.hclust(hclust_crimes, k = 3, border = 2:4)
#-------------------------------------------
#Last Heraricheical Clustering####
#-------------------------------------------
crime_hc <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences))

crime_subset_hc <- crime_hc[, c("homicide", "sexual_offences", "burglary",
                                "robbery","theft_and_handling","fraud_and_forgery",
                                "criminal_damage","drugs_offences" )] 
summary(crime_subset_hc)

crime_subset_hc <- na.omit(crime_subset_hc)

#the distance matrix
distance_matrix <- dist(crime_subset_hc[, -1]) 

#hierarchical clustering
hclust_result <- hclust(distance_matrix)

#Cut 3 clusters
clusters <- cutree(hclust_result, k = 3)

#Add the cluster labels
clustered_data <- crime_data_stacked[1:length(clusters), ]

#country names
clustered_data$cluster <- total_crime_hc$country

#dataframe with cluster labels
clustered_data <- data.frame(Country = clustered_data$cluster)

#Plot
plot(hclust_result,labels = clustered_data$Country)

#Add color
rect.hclust(hclust_result, k = 3, border = 2:4)
#-------------------------------------------
#Cut 5 clusters
clusters <- cutree(hclust_result, k = 5)
#Plot
plot(hclust_result,labels = clustered_data$Country)

#Add color
rect.hclust(hclust_result, k = 5, border = 2:4)
#-------------------------------------------
#Cut 10 clusters
clusters <- cutree(hclust_result, k = 10)
#Plot
plot(hclust_result,labels = clustered_data$Country)
#Add color
rect.hclust(hclust_result, k = 10, border = 2:4)
#-------------------------------------------
library(randomForest)

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(crime_data_stacked), 0.8 * nrow(crime_data_stacked))
train_data <- crime_data_stacked[train_indices, ]
test_data <- crime_data_stacked[-train_indices, ]

colnames(train_data)

model <- randomForest(~ ., 
                      data = train_data[, c("homicide", "sexual_offences", "burglary", "robbery",
                                            "theft_and_handling", "fraud_and_forgery", "criminal_damage", 
                                            "drugs_offences")],ntree = 100)

predictions <- predict(model, newdata = test_data)


selected_columns <- c("homicide", "sexual_offences", "burglary", "drugs_offences")
data_subset <- train_data[selected_columns]

# Build the random forest model
model <- randomForest(formula = homicide + sexual_offences + burglary + drugs_offences ~ .,
                      data = data_subset,
                      ntree = 100)

# Print the model summary
print(model)
#-------------------------------------------
library(randomForest)
library(caret)
crime_rf <- crime_data_stacked %>%
  group_by(country,year) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences),
            .groups = "drop")

set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(crime_rf), 0.7*nrow(crime_rf))  # 70% for training
train_data <- crime_rf[train_indices, ]
test_data <- crime_rf[-train_indices, ]

names(crime_rf)
View(crime_rf)

train_x <- train_data[, c("total_crime_rate","country")]
train_y <- train_data$year

test_x <- test_data[, c("total_crime_rate", "country")]
test_y <- test_data$year

rf_model <- randomForest(train_x, train_y)

#predictions <- predict(rf_model, test_x)

confusion_matrix <- confusionMatrix(predictions, test_y)

# Convert predictions and test_y to factors with the same levels
#predictions <- factor(predictions, levels = levels(test_y))
#test_y <- factor(test_y, levels = levels(test_y))

# Compute the confusion matrix
#confusion_matrix <- confusionMatrix(predictions, test_y)


# Identify unique levels from predictions and test_y
unique_levels <- unique(c(levels(predictions), levels(test_y)))

# Convert predictions and test_y to factors with the aligned levels
#predictions <- factor(predictions, levels = unique_levels)
#test_y <- factor(test_y, levels = unique_levels)

# Compute the confusion matrix
#confusion_matrix <- confusionMatrix(predictions, test_y)



# Check levels of predictions and test_y
levels(predictions)
levels(test_y)

# Align the levels
common_levels <- union(levels(predictions), levels(test_y))
predictions <- factor(predictions, levels = common_levels)
test_y <- factor(test_y, levels = common_levels)

# Compute the confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_y)


#-------------------------------------------


crime_rf <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences))
names(crime_rf)

selected_columns <- c("total_crime_rate","homicide","sexual_offences", "burglary","robbery","theft_and_handling",
                      "fraud_and_forgery","criminal_damage","drugs_offences")

data_subset <- crime_rf[selected_columns]

# Set a seed for reproducibility
set.seed(123)

# Split the data into training and testing datasets
train_indices <- sample(nrow(data_subset), nrow(data_subset) * 0.7)  # 70% for training
train_data <- data_subset[train_indices, ]
test_data <- data_subset[-train_indices, ]

# Build the random forest model using the training data
model <- randomForest(total_crime_rate ~ ,
                      data = train_data,
                      ntree = 100)


# Predict using the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
accuracy <- mean(predictions == test_data$drugs_offences)  # Adjust the target variable as needed
print(paste("Accuracy:", accuracy))

accuracy <- mean(predictions == test_data$sexual_offences)  # Adjust the target variable as needed
print(paste("Accuracy:", accuracy))


# Create a data frame with actual and predicted values
plot_data <- data.frame(Actual = test_data$homicide, Predicted = predictions)
summary(plot_data)

# Plot the data
plot(plot_data$Actual, type = "l", col = "blue", ylim = range(plot_data$Actual, plot_data$Predicted),
     xlab = "Observations", ylab = "Values", main = "Actual vs Predicted")

lines(plot_data$Predicted, type = "l", col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)


library(randomForest)
library(randomForestExplainer)

# Assuming your random forest model is stored in the 'model' object

# Plotting feature importance
varImpPlot(model)

# Plotting random forest tree diagram
rf_explainer <- explain(model, data = train_data[, -c("homicide", "sexual_offences", "burglary", "drugs_offences")])
plot(rf_explainer)
#-------------------------------------------
se <- combined_data %>%
  select(country, homicide, sexual_offences, burglary, robbery, theft_and_handling, 
         fraud_and_forgery, criminal_damage, drugs_offences, month, year, 
         date)%>% filter(country != "National")

se <- subset(se, year == 2018)

library(cluster)

# Select the relevant columns for clustering
crime_features <- se[, c("sexual_offences", "burglary", "robbery","criminal_damage", "drugs_offences")]

# Calculate the distance matrix
dist_matrix <- dist(crime_features)

# Perform Hierarchical Clustering
cluster_model <- hclust(dist_matrix)

# Print the clustering results
print(cluster_model)

# Plot the dendrogram
plot(cluster_model)
num_subsets <- 20  # Specify the number of subsets you want
subset_size <- floor(nrow(se) / num_subsets)  # Calculate the subset size

subset_list <- vector("list", num_subsets)  # Initialize a list to store the subsets

# Iterate over the subsets
for (i in 1:num_subsets) {
  start_index <- (i - 1) * subset_size + 1  # Calculate the start index
  end_index <- min(start_index + subset_size - 1, nrow(crime_data))  # Calculate the end index
  
  subset_list[[i]] <- se[start_index:end_index, ]  # Subset the data
}

# Perform Hierarchical Clustering and plot for each subset
for (i in 1:num_subsets) {
  subset_data <- subset_list[[i]]  # Get the subset data
  
  # Perform Hierarchical Clustering
  # Use the subset_data and follow the previous steps for hierarchical clustering
  
  # Plot the dendrogram
  plot(cluster_model)}

#-------------------------------------------
names(crime_data_stacked)

glimpse(crimes_cluster)
plot(crimes_cluster)
pairs(crimes_cluster)
set.seed(12345)
crimes_cluster <- combined_data[, c("country","homicide", "sexual_offences",
                                    "burglary","robbery", "criminal_damage", 
                                    "drugs_offences","year")]

#Using DPLYR function, select the required columns and filter the required year and eliminate the unnecessary elements
#c.cluster<- crimes_cluster %>% select(country, homicide, sexual_offences,
#                                      burglary,robbery, criminal_damage, 
#                                      drugs_offences,year) %>% filter(country != "National")
#

crime

#Using DPLYR function, select the required columns and filter the required year and eliminate the unnecessary elements
c.cluster<- crimes_cluster %>% select(homicide, sexual_offences,
                                      burglary,robbery, criminal_damage, 
                                      drugs_offences)


c.model <- kmeans(c.cluster,centers = 5,nstart = 25)
c.model$cluster
summary(c.model)

fviz_cluster(c.model, data = c.cluster[,-5],
             ellipse.type = "convex",
             ggtheme = theme_bw()
)


c.cluster<- crimes_cluster %>% select(homicide, sexual_offences,
                                      burglary,robbery, criminal_damage, 
                                      drugs_offences)
#--------------------------------
crimes_cluster <- crime_data_stacked %>% select(-c(country,year,month,date))
summary(crimes_cluster)

scaled_crimes_data <- scale(crimes_cluster)
cluster_model <- kmeans(scaled_crimes_data,centers = 4,nstart = 25)
cluster_model$cluster
summary(cluster_model)
fviz_cluster(cluster_model, data = crimes_cluster[,-5], ellipse.type = "convex",ggtheme = theme_bw()
)


#--------------------------------
# Simple regression: robbery and drugs
#--------------------------------
criminal <- crimes_data$criminal_damage
drugs <-crimes_data$drugs_offences
burglary <-crimes_data$burglary
robbery<- crimes_data$robbery
sexual_offences<-crimes_data$sexual_offences
homocide <- crimes_data$homicide
colyr <- crimes_data$year

plot(criminal, drugs)

crimes_data
cor.test(drugs,robbery)


drug_robbery_model<-lm(drugs~robbery, data = crimes_data)
summary(drug_robbery_model)

preds <- predict(drug_robbery_model,crimes_data)

# Create a scatter plot with the regression line
ggplot(drug_robbery_model, aes(x = robbery, y = drugs,color = colyr )) +
  geom_point(show.legend = FALSE) +
  geom_smooth(formula = "y~x+0",method = "lm",se = FALSE)


# Mulitple regression
multiple_model<-lm(drugs ~ criminal + robbery, data = crimes_data)
summary(multiple_model)

plotting_multiple<-expand.grid( cri= seq(min(criminal), max(criminal), length.out=30), 
                                bur = c(min(burglary), mean(burglary), max(burglary)),
                                rob = c(min(robbery), mean(robbery), max(robbery)))

plotting_multiple$predicted.y <- predict.lm(multiple_model, newdata = plotting_multiple)

plotting_multiple

------------------

#function for crimes by country


  
  
  plot_crime_country <- function(data, y_variable) {
  ggplot(data, aes(x = country, y = !!sym(y_variable))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    xlab("Country") +
    ylab(paste("Number of", y_variable)) +
    ggtitle(paste(y_variable, "by Country")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


plot_crime <- function(data, y_variable){
  ggplot(data, aes(x = country, y = !!sym(y_variable))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    xlab("Country") +
    ylab(paste("Number of", y_variable)) +
    ggtitle(paste(y_variable, "by Country")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
}
  
plot_crime(crime_data_stacked, "homicide")
plot_crime(crime_data_stacked, "sexual_offences")
plot_crime(crime_data_stacked, "burglary")
plot_crime(crime_data_stacked, "robbery")
plot_crime(crime_data_stacked, "theft_and_handling")
plot_crime(crime_data_stacked, "criminal_damage")
plot_crime(crime_data_stacked, "drugs_offences")




#-------------------------------------------
#Clustering
#-------------------------------------------
crimes_cluster <- combined_data[, c("homicide", "sexual_offences",
                                    "burglary","robbery", "criminal_damage", 
                                    "drugs_offences","year")]

#Using DPLYR function, select the required columns and filter the required year and eliminate the unnecessary elements
c_crimes1 <- combined_data %>% select(country, homicide, sexual_offences,
                                   burglary,robbery, criminal_damage, 
                                   drugs_offences,year) %>%
  filter(year == 2018, country != "National")
c_crime2 <- scale(c_crimes1[2:7])
c_crime2

#Use fviz function to view Elbow plot for visualizing optimal number of clusters
fviz_nbclust(c_crime2, kmeans, method = "wss")

fviz_nbclust(c_crime2, kmeans, method = "silhouette")

#Set seed for obtaining same results every time
set.seed(1234)

#--------------------------


crimes_cluster <- crime_data_stacked %>% select(-c(country,year,month,date))
summary(crimes_cluster)

scaled_crimes_data <- scale(crimes_cluster)

fviz_nbclust(scaled_crimes_data, kmeans, method = "wss")
fviz_nbclust(c_crime2, kmeans, method = "silhouette")

cluster_model <- kmeans(scaled_crimes_data,centers = 3,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 4,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 5,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

cluster_model <- kmeans(scaled_crimes_data,centers = 6,nstart = 25)
fviz_cluster(cluster_model, data = crimes_cluster)

#--------------------------

#Analyse the cluster values. Check between to total sum of squares
c_crime_cluster1 <- kmeans(c_crime2, 2)
c_crime_cluster1


c_crime_cluste1 <- kmeans(c_crime2, 2)
c_crime_cluster1
fviz_cluster(c_crime_cluster1, data = c_crime2)

c_crime_cluster2 <- kmeans(c_crime2, 3)
c_crime_cluster2
fviz_cluster(c_crime_cluster2, data = c_crime2)

c_crime_cluster3 <- kmeans(c_crime2, centers = 4, nstart = 100)
c_crime_cluster3
fviz_cluster(c_crime_cluster3, data = c_crime2)

c_crime3 <- c_crimes1

c_crime3$Clusters <- c_crime_cluster3$cluster
c_crime3$Cluster_names <- factor(c_crime3$Clusters, levels = c(1,2,3,4), 
                               labels  = c("Peaceful", "Critical", "Serious", "Under Control"))
attach(c_crime3)

c_crime4 <- c_crime3 %>% group_by(Cluster_names) %>% summarize(Homicide= mean(homicide), 
                                                           Sexual_offences = mean(sexual_offences),
                                                           Burglary= mean(burglary),
                                                           Robbery= mean(robbery), 
                                                           Criminal_damage= mean(criminal_damage),
                                                           Drugs= mean(drugs_offences), Count= n()) 
                                                           
c_crime4



#--------------------------------
glimpse(crimes_cluster)
plot(crimes_cluster)
pairs(crimes_cluster)
set.seed(12345)
crimes_cluster <- combined_data[, c("country","homicide", "sexual_offences",
                                    "burglary","robbery", "criminal_damage", 
                                    "drugs_offences","year")]

#Using DPLYR function, select the required columns and filter the required year and eliminate the unnecessary elements
#c.cluster<- crimes_cluster %>% select(country, homicide, sexual_offences,
#                                      burglary,robbery, criminal_damage, 
#                                      drugs_offences,year) %>% filter(country != "National")
#

#Using DPLYR function, select the required columns and filter the required year and eliminate the unnecessary elements
c.cluster<- crimes_cluster %>% select(homicide, sexual_offences,
                                      burglary,robbery, criminal_damage, 
                                      drugs_offences)


c.model <- kmeans(c.cluster,centers = 5,nstart = 25)
c.model$cluster
summary(c.model)

fviz_cluster(c.model, data = c.cluster[,-5],
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

#--------------------------------

group_county <- function(dataframe){
  dataframe <- dplyr::select(dataframe, -c("month"))
  dataframe <- dataframe[dataframe$country != "National",]
  dataframe <- group_by(dataframe, country)
  dataframe <- summarise_all(dataframe, funs(sum))
  dataframe2 <- dataframe[,-1]
  rownames(dataframe2) <- dataframe$country
  dataframe2 <- scale(dataframe2)
  return(dataframe2)
}

hc_clustering <- function(dataframe) {
  distance_mat <- dist(dataframe, method = 'euclidean')
  set.seed(240)
  model <- hclust(distance_mat, method = "average")
  return(list(distance_mat, model))
}

hc_data = group_county(crimes_data)

summary(hc_data)


#--------------------------------
#PCA
res.pca <- prcomp(crimes_cluster[,-5], scale = TRUE)
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)

ind.coord$cluster <- factor(res.km$cluster)
ind.coord$year <- crimes_cluster$year

head(ind.coord)


eigenvalue <- round(get_eigenvalue(res.pca), 1) 
variance.percent <- eigenvalue$variance.percent 
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2",
  color = "cluster", palette = "npg", ellipse = TRUE,
  ellipse.type = "convex", size = 1.5, 
  legend = "right", ggtheme = theme_bw(), 
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
)+ stat_mean(aes(color = cluster), size = 4)


names(crimes_cluster)

# Remove irrelevant columns
crimes_cluster1 <- subset(crimes_cluster, select = -c(homocide, sexual_offences, year))
names(crime_2018)
# Normalize the variables
normalized_data <- scale(crimes_cluster)

# Compute the distance matrix
distance_matrix <- dist(normalized_data)

# Perform hierarchical clustering
hc_result <- hclust(distance_matrix, method = "complete")

# Plot the dendrogram
plot(hc_result)

#-----------------------------------------------------------------------------
#Time Series Analysis for robbery
#-----------------------------------------------------------------------------
#bat_ts <- ts(bat_train$Battery, frequency = 24)
library(forecast)
drugs_offences_ts <- ts(crime_data_stacked$drugs_offences, frequency = 24)

drugs_ts_dec <- drugs_offences_ts %>%
  tail(365) %>%
  decompose()


drugs_ts_dec %>%
  autoplot()

library(ggplot2)
#--------------------------------

# Create MSTS Object
drugs_multi <- msts(crime_data_stacked$drugs_offences, seasonal.periods = c(24*7, # Weekly
                                                                  24*30)) # Monthly


# Decompose MSTS Object
drugs_multi_dec <- drugs_multi %>%
  mstl()

drugs_multi_dec %>%
  tail(365) %>%
  autoplot()

drugs_multi_dec %>%
  tail(12) %>%
  autoplot()


#------------------------------------------------------------------------------------
#Seasonality Analysis

data_drugs_multi <- as.data.frame(drugs_multi_dec)
Plot2 <- data_drugs_multi %>%
  mutate(month = crime_data_stacked$date) %>%
  group_by(month) %>%
  summarise(seasonal = sum(Seasonal168 + Seasonal720)) %>%
  ggplot(aes(x = month, y = seasonal)) +
  geom_point(col = "maroon") +
  geom_line(col = "red") +
  theme_minimal()


Plot2 <- data_drugs_multi %>%
  mutate(month = crime_data_stacked$date) %>%
  group_by(month) %>%
  summarise(seasonal = sum(Seasonal168 + Seasonal720)) %>%
  head(24*2) %>%
  ggplot(aes(x = month, y = seasonal)) +
  geom_point(col = "maroon") + geom_line(col = "red") +
  theme_minimal()

Plot2
names(crime_data_stacked)
#months
Plot4 <- data_drugs_multi %>%
  mutate( day = crime_data_stacked$date,  month = month(crime_data_stacked$month, label= TRUE )) %>%
  group_by(month) %>%
  summarise(seasonal = sum(Seasonal168 + Seasonal720)) %>%
  head(24*30) %>%
  ggplot(aes(x = month, y = seasonal)) +
  geom_point() + geom_col() +
  theme_minimal()

Plot4
Plot4 <- data_drugs_multi %>%
  mutate(day = crime_data_stacked$date,
         month = factor(month(crime_data_stacked$date), labels = month.name)) %>%
  group_by(month) %>%
  summarise(seasonal = sum(Seasonal168 + Seasonal720)) %>%
  head(24*30) %>%
  ggplot(aes(x = month, y = seasonal)) +
  geom_point() +
  geom_col() +
  theme_minimal()


Plot4 <- data_drugs_multi %>%
  mutate(day = crime_data_stacked$date,
         month = factor(month(crime_data_stacked$date), labels = month.name)) %>%
  group_by(month) %>%
  summarise(seasonal = sum(Seasonal168 + Seasonal720)) %>%
  ggplot(aes(x = month, y = seasonal)) +
  geom_point() +
  geom_col() +
  theme_minimal()

#--------------------------------
#Visualisation
#--------------------------------

crimes_data <- combined_data[, c("country","homicide", "sexual_offences",
                                 "burglary","robbery", "criminal_damage", 
                                 "drugs_offences","month", "year","date")]
unsuccessful_crimes <- combined_data[, c("country","homicide_unsuccessful", "sexual_offences_unsuccessful",
                                         "burglary_unsuccessful","robbery_unsuccessful", "criminal_damage_unsuccessful", 
                                         "drugs_offences_unsuccessful","date")]
crimes_data$date <- as.Date(crimes_data$date)
unsuccessful_crimes$date <- as.Date(unsuccessful_crimes$date)

# Create a line plot for convictions
convictions_plot <- ggplot(data = crimes_data, aes(x = date, y = burglary)) +
  geom_line(color = "blue", linetype = "solid") +
  ylab("Number of Convictions")

# Create a line plot for unsuccessful crimes
unsuccessful_plot <- ggplot(data = unsuccessful_crimes, aes(x = date, y = burglary_unsuccessful)) +
  geom_line(color = "red", linetype = "dashed") +
  ylab("Number of Unsuccessful Crimes")

#Combine the two plots using 'grid.arrange()' from the 'gridExtra' package
library(gridExtra)
combined_plot <- grid.arrange(convictions_plot, unsuccessful_plot, nrow = 2)

# Display the combined plot
print(combined_plot)

#--------------------------------
Decison Tree
#--------------------------------
names(crime_data_stacked)
# Load required libraries
library(rpart)
library(rpart.plot)
# Filter the crime data for Gloucestershire
gloucestershire_data <- filter(crime_data_stacked, country == "Gloucestershire" && )

# View the filtered data
print(gloucestershire_data)

qplot(burglary, drugs_offences, data=crime_data_stacked, colour=country, size=I(4))

rpart <- rpart(country ~ ., data=crime_data_stacked, method="class",) 
rpart
n= 150


tree1 <- tree(country ~ burglary+ drugs_offences, data = crime_data_dc) 

plot(crime_data_dc$burglary,crime_data_dc$drugs_offences,pch=19,col=as.numeric(crime_data_dc$country)) 
partition.tree(tree1,label="country",add=TRUE) 
legend(1.75,4.5,legend=unique(crime_data_dc$country),col=unique(as.numeric(crime_data_dc$country)),pch=19)

graph <- qplot(burglary, drugs_offences, data=crime_data_dc, colour=country, size=I(4))
graph + geom_hline(aes(yintercept=2.65)) + 
  geom_vline(aes(xintercept=0.8)) + geom_vline(aes(xintercept=1.75)) + geom_vline(aes(xintercept=1.35))


crime_dc <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences))




# Convert factor variables to character
crime_data_dc$burglary <- as.character(crime_data_dc$burglary)
crime_data_dc$drugs_offences <- as.character(crime_data_dc$drugs_offences)

# Apply one-hot encoding using the 'model.matrix' function
encoded_data <- model.matrix(~ burglary + drugs_offences - 1, data = crime_data_dc)

# Add the encoded variables to the dataset
crime_data_dc <- cbind(crime_data_dc, encoded_data)

colnames(crime_data_dc)
colnames(crime_data_dc) <- make.unique(colnames(crime_data_dc))
crime_data_dc$variable <- as.character(crime_data_dc$variable)

tree1 <- tree(country ~ ., data = crime_dc)
plot(tree1)
text(tree1)

# Fit the decision tree using the encoded variables
tree1 <- tree(country ~ ., data = crime_data_dc)


# Visualize the decision tree
rpart.plot(tree_model)

#--------------------------------
#KNN 
#--------------------------------
# Load the required library
library(class)

set.seed(123)
#70% for training
train_indices <- sample(nrow(crime_data_stacked), 0.7 * nrow(crime_data_stacked))  
train_data <- crime_data_stacked[train_indices, ]
test_data <- crime_data_stacked[-train_indices, ]

# predictor variables
predictors <- c("homicide", "sexual_offences", "burglary", "robbery", "theft_and_handling",
                "fraud_and_forgery", "criminal_damage", "drugs_offences")

# KNN model
k <- 5
knn_model <- knn(train_data[, predictors], test_data[, predictors], train_data$country, k)

# Predict 
predictions <- as.factor(knn_model)

#confusion matrix
confusion_matrix <- table(predictions, test_data$country)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

#matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
#--------------------------------
#Random Forest and Cofusion Matrix
#--------------------------------

# Load necessary libraries
library(caret)
library(randomForest)



crime_rf <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences))


split_data <- crime_data_stacked
split_data$date <- as.Date(crime_data_stacked$date)
split_data$country <- as.factor(crime_data_stacked$country)

num_df <- dplyr::select(split_data, -c("country","year", "month", "date" ))

split_data $total_crimes <- rowSums(num_df)

test_rf <- split_data[split_data$year == 2014,]
train_rf <- split_data[split_data$year != 2014,]

test_rf <- dplyr::select(test_rf, c("country","year", "total_crimes"))
train_rf <- dplyr::select(train_rf, c("country","year", "total_crimes"))

list(test_rf,train_rf)

set.seed(123)
rf_model <- randomForest(country ~ year + total_crimes, data = train_rf, proximity=TRUE)

predictions <- predict(rf_model, newdata = test_rf)

confusion_matrix <- confusionMatrix(as.factor(predictions),as.factor(test_rf$country))


# Compute the confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_rf$country)

list(rf_model,confusion_matrix)


plot(rf_model)




# Calculate accuracy
accuracy <- confusion_matrix$overall['Accuracy']

# Print the confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", accuracy))







names(crime_rf)
# Data preprocessing
# Handle missing values
crime_data_rf <- na.omit(crime_rf)

# Encoding categorical variables
crime_data_rf$country <- as.factor(crime_data_rf$country)
crime_data_rf$month <- as.factor(crime_data_rf$month)

# Feature engineering
# Extract date features
crime_data_rf$weekday <- as.factor(weekdays(as.Date(crime_data_rf$date)))
crime_data_rf$year <- as.factor(format(as.Date(crime_data_rf$date), "%Y"))

# Feature selection (select relevant predictors)
predictors <- c("homicide", "sexual_offences", "burglary", "robbery", "theft_and_handling",
                "fraud_and_forgery", "criminal_damage", "drugs_offences")

# Model training and evaluation
# Split the data into training and test sets
set.seed(123)
train_indices <- sample(nrow(crime_data_rf), 0.7 * nrow(crime_data_rf))
train_data <- crime_data_rf[train_indices, ]
test_data <- crime_data_rf[-train_indices, ]

# Convert country to a factor
train_data$country <- as.factor(train_data$country)

# Train a random forest model
rf_model <- randomForest(country ~ ., data = train_data, ntree = 100)


# Train a random forest model
#rf_model <- randomForest(country ~ ., data = train_data, ntree = 100)

# Make predictions on the test data
predictions <- predict(rf_model, newdata = test_data)
# Convert predictions to a factor with the same levels as test_data$country
predictions <- factor(predictions, levels = levels(test_data$country))

# Compute the confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$country)


# Calculate accuracy
accuracy <- confusion_matrix$overall['Accuracy']

# Print the confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", accuracy))



#--------------------------------
#Plot
# Add predicted labels to the test data
test_data$predicted_country <- knn_model

# Create a scatter plot
ggplot(test_data, aes(x = burglary, y = drugs_offences, color = predicted_country)) +
  geom_point() +
  labs(x = "Burglary", y = "Drugs Offences", color = "Predicted Country") +
  ggtitle("KNN Classification Results")+theme(plot.width = 10, plot.height = 6) 
  
#--------------------------------
#All Plot

#--------------------------------
#KNN Final ONe
#--------------------------------
# Split the data into training and testing sets

knn_data <- crime_data_stacked
summary(knn_data)

set.seed(123)
#70%
train_produce <- sample(nrow(knn_data), 0.7 * nrow(crime_data_stacked))
train_data <- crime_data_stacked[train_produce, ]
test_data <- crime_data_stacked[-train_produce, ]

#predictor variables
predictors <- c("homicide", "sexual_offences", "burglary", "robbery", "theft_and_handling",
                "fraud_and_forgery", "criminal_damage", "drugs_offences")

#KNN model
k <- 5
knn_model <- knn(train_data[, predictors], test_data[, predictors], train_data$country, k)
summary(knn_model)

# Add predicted labels
test_data$predicted_country <- knn_model

# parallel coordinate plot
pc_data <- cbind(test_data[, predictors], predicted_country = test_data$predicted_country)

# Normalize variables
pc_data_normalized <- as.data.frame(lapply(pc_data[, -ncol(pc_data)], scale))
pc_data_normalized$predicted_country <- pc_data$predicted_country

# Create a scatter plot
ggplot(test_data, aes(x = burglary, y = drugs_offences, color = predicted_country)) +
  geom_point() +
  labs(x = "Burglary", y = "Drugs Offences", color = "Predicted Country") +
  ggtitle("KNN Classification Results")


#--------------------------------
predicted_labels <- as.factor(knn_model)
data <- data.frame(Predicted = predicted_labels, Actual = test_data$country)
label_mapping <- distinct(data, Predicted, Actual)

ggplot(data, aes(x = Predicted, y = Actual)) +
  geom_point() +
  labs(x = "Predicted Label", y = "Actual Label") +
  scale_x_discrete(labels = label_mapping$Predicted) +
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_discrete(labels = label_mapping$Actual) +
  ggtitle("KNN Model Results")
#--------------------------------




#--------------------------------
crime_binary <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences))


region_data <- crime_binary[crime_binary$country == "Gloucestershire", ]
region_data

# Select the relevant columns for classification
selected_columns <- c("country", "homicide", "sexual_offences", "burglary", "robbery",
                      "theft_and_handling", "fraud_and_forgery", "criminal_damage",
                      "drugs_offences")
crime_subset <- crime_binary[, selected_columns]

# Create a binary response variable based on crime rate
crime_subset$crime_rate_category <- ifelse(crime_data$crime_rate > median(crime_data$crime_rate), "high", "low")

# Convert categorical variables to factors
crime_subset$crime_rate_category <- as.factor(crime_subset$crime_rate_category)


#--------------------------------
#Random Forest and Cofusion Matrix
#--------------------------------
crime_rf <- crime_data_stacked %>%
  group_by(country) %>%
  summarise(total_crime_rate = sum(homicide, sexual_offences, burglary, robbery,
                                   theft_and_handling, fraud_and_forgery,
                                   criminal_damage, drugs_offences),
            homicide = sum(homicide),
            sexual_offences = sum(sexual_offences),
            burglary = sum(burglary),
            robbery = sum(robbery),
            theft_and_handling = sum(theft_and_handling),
            fraud_and_forgery = sum(fraud_and_forgery),
            criminal_damage = sum(criminal_damage),
            drugs_offences = sum(drugs_offences))


split_data <- crime_data_stacked
split_data$date <- as.Date(crime_data_stacked$date)
split_data$country <- as.factor(crime_data_stacked$country)

num_df <- dplyr::select(split_data, -c("country","year", "month", "date" ))

split_data $total_crimes <- rowSums(num_df)

test_rf <- split_data[split_data$year == 2014,]
train_rf <- split_data[split_data$year != 2014,]

test_rf <- dplyr::select(test_rf, c("country","year", "total_crimes"))
summary(test_rf)

train_rf <- dplyr::select(train_rf, c("country","year", "total_crimes"))
summary(train_rf)

set.seed(123)
rf_model <- randomForest(country ~ year + total_crimes, data = train_rf, proximity=TRUE)
summary(rf_model)

predictions <- predict(rf_model, newdata = test_rf)
#confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_rf$country)
summary(confusion_matrix)

# Calculate accuracy
accuracy <- confusion_matrix$overall['Accuracy']

# Print the confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

#confusion_matrix$table

country <- "West Yorkshire"
# Find the index of the country in the confusion matrix
country_index <- which(colnames(confusion_matrix$table) == country)
# Calculate the accuracy for the specific country
accuracy_specific <- confusion_matrix$table[country_index, country_index] / sum(confusion_matrix$table[country_index, ])

# Print the accuracy for the specific country
print(paste("Accuracy for", country, ":", accuracy_specific))
