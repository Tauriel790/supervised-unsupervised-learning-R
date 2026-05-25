# load the libraries needed
library(tidyverse)
library(ggplot2)
library(reshape2)
library(e1071)
library(moments)
library(ggcorrplot)
library(factoextra)
library(dplyr)
library(cluster)
library(plotly)

# load the data set into the environment
data <- read.csv("smartphones.csv")
head(data)
summary(data)
str(data)

# counting rows and columns in the data set
nrow(data)
ncol(data)


# ------------------------------------------------- EXPLORATORY DATA ANALYSIS------------------------------------------------------
# First we perform the exploratory data analysis (EDA). This procedure is used to analyze and investigate the data contained in 
# the dataset to find hidden patterns within the data, anomalies, and analyze their characteristics usually by using also different 
# data visualization techniques.

# check if there are missing values. In this case there are missing values so we have to drop them from the dataset
is.na(data)
sum(is.na(data))        # this part of the code will show the total number of null values in the whole data set
colSums(is.na(data))    # and this will show the number of null values that are present in each column

# Since there are null values, we have to drop them
cleaned_data <- na.omit(data)
print (cleaned_data)

# Then, we check if there are any duplicates. By running this line of code we see that there are no duplicates in our dataset
anyDuplicated(data)

# we can drop the variable fast charging available because it is constant

cleaned_data$fast_charging_available

table (cleaned_data$fast_charging_available)
cleaned_data <- select(cleaned_data, - fast_charging_available)
cleaned_data

# check which are the numerical variables in the dataset

numeric_variables <- sapply(cleaned_data, is.numeric)

print (numeric_variables)

numeric <- cleaned_data[, numeric_variables]
print(numeric)
sum(numeric_variables)

# now, we visualize the distribution of the variables of the dataset

numeric_for_plotting <- numeric %>%
  select(where(~ length(unique(.)) > 2))

data_reshaped_long <- melt(numeric_for_plotting)

ggplot(data_reshaped_long, aes(x = value))+
  geom_histogram(col = "black", fill = "lightblue", bins = 20)+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Variables distribution plots")

# we can also visualize the density of each numerical feature

ggplot (data_reshaped_long, aes (x = value))+
  geom_density(col = "red", linewidth = 0.6, fill = "lightyellow")+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Variables density plots")

# now combine the two plots together using again a ggplot function

ggplot (data_reshaped_long, aes(x = value))+
  geom_histogram(aes (y = after_stat(density)), col = "black", fill = "lightblue", bins = 20)+
  geom_density(col = "red", linewidth = 0.6)+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Distribution of the numeric variables")

# After, we can analyze the relationship between the variables and make insights about the dataset. For example:
# 1) What is the relationship between average rating and model of the smartphone? What is the highest average rated smartphone brand?
#    We see that the most highly rated is the brand Lenovo, followed by Asus, Doogee, Zte, ect.......

average_rating_by_brand <- cleaned_data %>%
  group_by(brand_name) %>%
  summarise(avg_rating = mean(avg_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))

ggplot (average_rating_by_brand, aes(x = reorder(brand_name, avg_rating), y = avg_rating, fill = avg_rating))+
  geom_col(width = 0.5)+
  scale_fill_gradient(low = "red", high = "yellow") +
  coord_flip()+
  labs(title = "Average rating by brand",
       x = "brand name", y = "average raring for smartphones")+
  theme_minimal()

# We can also search for features of the smartphones which most influence the price of the smartphones in the market:

# 2) For example, what is the relationship between the price of the smartphones and the battery capacity of them? We can see from 
#    the plot that there is no direct relationship between battery capacity and price, so battery capacity is not a strong 
#    predictor for the price of the smartphone even if we see from the trend line a slight negative relationship between them.
#    So, there is no evidence that a higher battery capacity will lead always to higher prices of the smartphones.

ggplot(cleaned_data, aes (x = battery_capacity, y = price))+
  geom_point(color = "blue", alpha = 0.5)+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  labs (title = "Relationship between battery capacity and price ",
        x = "Battery capacity", y = "price of the smartphone")

# 3) Then what is the relationship between number of smartphones and brand name? We can use this relationship to find out which
#    of them is the most recurrent phone brand in the dataset. From the bar plot we can see that the most common one is the 
#    one from the brand Xiaomi, followed by Samsung and Realme

table (cleaned_data$brand_name) %>%      # By this summary we can see that the highest number of phones is detained by Xiaomi
  sort(increasing = TRUE)                

number_of_smartphones_by_brand <- cleaned_data %>%
  group_by(brand_name) %>%
  summarise(number_of_phones = n()) %>%
  arrange(desc(number_of_phones))

ggplot(number_of_smartphones_by_brand, aes(x = reorder(brand_name, number_of_phones), y = number_of_phones, fill = number_of_phones))+
  geom_col () +
  labs (title = "Number of phones by brand name",
        x = "Brand name", y = "Number of phones") +
  scale_fill_gradient(low = "darkgreen", high = "lightgreen") +
  theme_bw()+
  theme(legend.position = "none")

# 4) what is the relationship between number of rear cameras and price of the smartphone? Does a higher number of rear cameras means
# higher price for the smartphone? 

# first we visualize the phones by the number of real cameras they have on them. We can see that most of the smartphones have 
# 3 rear cameras implemented on them

ggplot (cleaned_data, aes(x = as.factor(num_rear_cameras), fill = num_rear_cameras))+
  geom_bar()+
  scale_fill_gradient(low = "purple", high = "pink")+
  labs (title = "Number of phones by number of rear cameras",
        x = "Number of rear cameras", y = "number of phones")


# 5) then we can do a plot to show the relationship between number of rear cameras and price of the smartphones. We can see that as 
# the number of rear cameras rise also the average price rise. The highest average price is reached when the rear cameras on the
# smartphone is 3. But for example if the number of cameras on the phone is 4 we can see that the price is higher than 1 rear camera
# but less then the phones with 3 rear cameras. It means that the higher number of cameras doesn't always mean higher prices for
# the smartphones.

average_price_by_rear_cameras <- cleaned_data %>%
  group_by(num_rear_cameras) %>%
  summarise(average_price = mean(price, na.rm = TRUE)) %>%
  arrange(num_rear_cameras)

ggplot(average_price_by_rear_cameras, aes(x = as.factor(num_rear_cameras), y = average_price, fill = average_price))+
  geom_col()+
  scale_fill_gradient(low = "yellow", high = "green")+
  labs (title = "Relationship between number of rear cameras and price",
        x = "Number of rear cameras", y = "Average price")

# 6) After all of these, then we can also visualize which of the smartphone's brand offers the highest number of rear cameras. 
#    We can see from the plot that the most rear cameras are provided by brands like Zte, Xiaomi, Vivo, Techno and Sony because their 
#    maximum number of rear cameras for this brand are 4 which is the maximum number of rear cameras provided in the data set.

number_cameras_for_brand <- cleaned_data %>%
  group_by(brand_name) %>%
  summarise(number_cameras = max (num_rear_cameras, na.rm = TRUE)) %>%
  arrange(desc(number_cameras))

ggplot(number_cameras_for_brand, aes(x = reorder(brand_name, number_cameras), y = number_cameras, fill = number_cameras))+
  geom_col()+
  scale_fill_gradient(low = "lightyellow", high = "lightgreen")+
  coord_flip()+
  labs (title = "Number of rear cameras by brand", x = "Brand names", y = "Numbers of rear cameras")

# 7)Another relationship we can analyze is the one between the presence of 5G phones and how 5G affects the price of the 
#   smartphones

#first we count the smartphones with 5G for each brand
smartphones_with_5G <- cleaned_data %>%
  filter (X5G_or_not == "1") %>%
  count(brand_name, sort = TRUE) 
  
# then we plot the results as follows. We can see that Xiaomi and Realme offer the highest number of 5G phones respectively
# 68 and 51 5G phones.
ggplot(smartphones_with_5G, aes(x = reorder(brand_name, n), y = n, fill = n)) +
  geom_col()+
  geom_text(aes(label = n, fontface = "bold"))+
  scale_fill_gradient(low = "yellow", high = "orange")+
  labs (title = "5G phones by brand",
        x = "Brand name", y = "5G phones number")
  
# Now, we can also see the effect of the 5G on the prices of the smartphones. First we calculate the average price, and then split
# the smartphones based on the fact that they are implemented with 5G or not. As we can see from the bar plot, the average price of
# smartphones that have 5G is definitely higher than the smartphones that don't have the 5G. So the presence of 5G has a great 
# influence on the prices of the smartphones, and smartphones with 5G tend to have higher prices in respect to the ones without 5G

average_prices_by_5G <- cleaned_data %>%
  group_by(X5G_or_not) %>%
  summarise(average_price = mean (price))

ggplot (average_prices_by_5G, aes (x = as.factor(X5G_or_not), y = average_price, fill = X5G_or_not))+
  geom_col()+
  scale_fill_gradient (low = "purple", high = "magenta")+
  geom_text(aes(label = average_price, fontface = "bold")) +
  labs (title = "Relationship between price and presence of 5G",
               x = "5G No = 0, Yes = 1", y = "Average price")

# 8) Another relationship we can focus on is the one between the screen size and the prices of the smartphones. In this case we
#    use a scree plot to do the visualization

# first we filter prices that are above 150000 (INR)

filtered_data <- cleaned_data %>%
  filter(price < 120000)

# then we check the range of the screen size of the smartphones

range(cleaned_data$screen_size)

# after, we visualize the relationship between the screen size and the the price of the smartphones by using a scatter plot. From the
# results we can see that for some smartphones the larger screen size influence the prices of the smartphones by rising the prices
# but this doesn't function for all the smartphones. In this case some smartphones even though they have a bigger screen size then 
# others they may still have a lower price then the ones with lower screen size.

ggplot (filtered_data, aes(x = screen_size, y = price, color = screen_size))+
  scale_x_continuous(breaks = seq(4.7, 7.6, by = 0.5))+
  geom_point(size = 1)+
  labs (title = "Realtionship between screen size and the price of the smartphones",
        x = "Screen size (inches)", y = "Price (INR)")
  
# 9) Another factor that may affect the price of the smartphones can be the RAM capacity. We can see that there are many outleirs
#    outside the various box plots. In general the median of the boxplots rise as the RAM capacity increases. This means that
#    that the prices of the smartphones tend to increase as the RAM capacity of the smartphone increases

range (cleaned_data$ram_capacity)

filtered_data2 <- cleaned_data %>%
  filter(price < 120000)

ggplot(filtered_data2, aes(x = as.factor(ram_capacity), y = price, fill = ram_capacity))+
  geom_boxplot()+
  labs (title = "Relationship between ram capacity and price",
        x = "RAM capacity (GB)", y = "price (INR)")

# --------------------------------------- Preparing the data for modelling --------------------------------------------------------
# first of all, before beginning with the modelling we have to prepare the data

# We have already selected the numerical values. Before modelling is essential to scale/normalize them

scaled_data <- scale (numeric)
summary(scaled_data)

# now we check if there are outliers that are present in our dataset, and if so we have to drop them otherwise they will affect the 
# results of PCA and clustering techniques

# to check if there are outliers we can use the boxplot visualization

scaled_data_reshaped <- melt (numeric)
head(scaled_data_reshaped)

excluded_variables <- c("X5G_or_not","extended_memory_available")
filtered_data_for_plot <- scaled_data_reshaped[!scaled_data_reshaped$variable %in% excluded_variables, ]

ggplot (filtered_data_for_plot, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot()+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Boxplots of the scaled numeric variables")+
  theme (legend.position = "none")

# after understanding that there are several outliers in our datasets, we should remove them before modelling and for this, we can 
# use the IQR method

outliers_removal <- function (x) {
  Q1 <- quantile (x, 0.25, na.rm = TRUE)
  Q3 <- quantile (x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  return(x >= lower_bound & x <= upper_bound)
}

# after defining the function for the IQR method we apply it to the numeric data of our dataset and remove the outliers before the
# modelling process:

no_outliers_numeric <- sapply(numeric, outliers_removal)
no_outliers_numeric

rows_without_outliers <- apply(no_outliers_numeric, 1, all)

cleaned_numeric <- numeric [rows_without_outliers,]   # after this we keep only rows that contain numeric data without outliers

# now we check for the skewness of our numeric data
skewness(cleaned_numeric)

# some of the variables return skewness = Nan because they have no variance, so before PCA and
# clustering methods is better to drop them

skewness_variables <- cleaned_numeric %>%
  select_if(function(col) length(unique(col)) > 2)

# now, after removing binary variables, we check again the skewness of the variables
skewness(skewness_variables)

# the only highly skewed variables after applying the skewness function are "price", "battery_capacity", and "primary_camera_front"
# so we should modify their skewness, because highly skewed variables can have a bad effect on PCA.

high_skewness_variables <- c("price", "battery_capacity", "primary_camera_front")

# after identifying the highly skewed variables we should apply the log transformation to them to modify the skewness

numeric_variables_transformed <- skewness_variables %>%
  mutate(across(all_of(high_skewness_variables), ~ log1p(.)))

# now we scale the numeric variables that we transformed, to normalize/standardize them

final_scaled_numeric <- scale (numeric_variables_transformed)

# after this we check another time the skewness to verify that everything worked correctly

skewness(final_scaled_numeric)

# We can also use a correlation matrix and visualize it through a heatmap to see the correlation between the variables

correlation_matrix <- cor(final_scaled_numeric)
correlation_matrix

correlation_matrix_long <- melt (correlation_matrix)

ggplot (data = correlation_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()+
  scale_fill_gradient2(low = "green", high = "blue", mid = "white")+
  labs (title = "Correlation matrix heatmap")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# --------------------------------------------------------- PCA -------------------------------------------------------------------

# After preparing the data, we can now begin to do the modelling. First we want to apply Principal Component Analysis to our data.
# When faced with a large set of correlated variables, principal components allow us to summarize this set with a smaller number 
# of representative variables that collectively explain most of the variability of the original set. So, Principal Component 
# Analysis (PCA) refers to the process by which principal components are computed, and the subsequent use of these components in 
# understanding the data.

pca_results <- prcomp (final_scaled_numeric, scale = TRUE)

# Now we can print the results of the PCA we have performed

summary(pca_results)

# Now we can visualize the result by using a scree plot. And the components we should take are the ones that explain most of the
# variance. From the scree plot we see that the optimal principal components which explain most of the variance are between 2 and 3
# since PC1, PC2 and PC3 seem to explain most of the variance of the original set.

fviz_eig(pca_results, ncp = 11)    # I added ncp = 11 otherwise the scree plot would have automatically cut off the last component
                                   # PC11, probably because it explains almost no variance or very little of the variance, as we 
                                   # can see when we run the code for the scree plot

# We can use also a variable contribution plot to visualize the result of the Principal Component Analysis, to visualize how
# much each variable contributes to the principal components

fviz_pca_var(pca_results, col.var = "contrib",
             gradient.cols = c("red", "blue", "green"))

# Now we extract PC1 and PC2

pca_data <- as.data.frame(pca_results$x[, 1:2])
pca_data                                        # this prints the data of the first 2 principal components 

# Now we can also use the WSS method to determine the optimal number of clusters. The WSS method (within sum of squares), is one
# of the methods used to determine the optimal number of clusters, also known as the "Elbow method", since the optimal number
# of clusters in its graphical representation is where the curve begin to flatten, and in our case, as we can see from the plot,
# the optimal number of clusters should be between 2 and 3.

fviz_nbclust(pca_data,
             FUNcluster = kmeans,        
             method = "wss")

# We can also use the silhouette method to determine the optimal number of clusters. In this case the optimal number of clusters 
# appears to be k = 3

fviz_nbclust(pca_data,
              FUNcluster = kmeans,
              method = "silhouette")

# Another method to determine the optimal number of clusters is the gap method. In this case the optimal number of clusters 
# appears to be k = 2 and not K = 3 as the Silhouette method suggested before

fviz_nbclust(pca_data,
             FUNcluster = kmeans,
             method = "gap")

# so from the 3 method used above, we can see that in general, the optimal numbers of clusters for our analysis falls between
# k = 2 and k = 3, so it is better to compute for both values k - means and hierarchical clustering, in order to compare their 
# results and find out which one is better between them.

# ---------------------------------------------- k - means clustering ------------------------------------------------------------

# Clustering refers to a very broad set of techniques for finding subgroup, or clusters, in a data set. When we cluster the 
# observations of a data set, we seek to partition them into distinct groups so that the observations within each group are quite
# similar to each other, while observations in different groups are quite different from each other. Both PCA and clustering seek 
# to simplify the data via a small number of summaries, but their mechanism are different:

# - PCA looks to find a low - dimensional representation of the observations that explain a good fraction of the variance;
# - Clustering looks to find homogeneous subgroups among the observations

# Since clustering is popular in many fields, there exist a great number of clustering methods. The best known approaches are 
# k - means clustering and hierarchical clustering. In this part of the project we will focus on the k - means clustering. In 
# k -means clustering we seek to partition the observations into a pre - specified number of clusters. (This definitions where
# taken from the book "An introduction to statistical learning with applications in R")

# 1) First we apply k - means in the case where the optimal number of clusters is k = 2

set.seed(130)

kmeans_k_2 <- kmeans(pca_data, centers = 2, nstart = 50 )

head(kmeans_k_2$cluster)

pca_data$cluster <- as.factor(kmeans_k_2$cluster)     # with this command we add the labels to the PCA data we have found before
                                                      # in our analysis

# now we can visualize the results of the k - means clustering with k = 2, by using the function fviz_cluster

# Since before we applied labels to the pca data, we have to create another variable which contains numeric data, without the
# non numeric labels otherwise the plot doesn't function correctly

numeric_pca <- pca_data[, 1:2]

fviz_cluster(kmeans_k_2,
             data = numeric_pca,
             geom = "point",
             ellipse.type = "convex",
             repel = TRUE
)

# 2) Now, we can apply also k - means in the case where the optimal number of principal components its k = 3 and not k = 2

pca_data_2 <- as.data.frame(pca_results$x[, 1:3])

set.seed(130)

kmeans_k_3 <- kmeans(pca_data_2, centers = 3, nstart = 50)

pca_data_2$cluster <- as.factor(kmeans_k_3$cluster)

# now we can do the same for the k - means clustering with k = 3, by using the same function

numeric_pca_2 <- pca_data_2 [, 1:3]

# 2 dimension visualization of the three cluster created with k - means clustering algorithm
fviz_cluster(kmeans_k_3,
             data = numeric_pca_2,
             geom = "point",
             ellipse.type = "convex",
             repel = TRUE)

# Visualization with a 3d plot

# first we create a dataframe with the PC1, PC2 and PC3 with the cluster labels

numeric_pca_3d <- cbind(pca_data_2[, 1:3], cluster = pca_data_2$cluster)

plot_ly(data = numeric_pca_3d,
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        color = ~ cluster,
        colors = c("red", "lightblue", "green"),
        type = "scatter3d",
        mode = "markers") %>%
  layout(title = "3D visualization of the clusters resulting from K - means",
         scene = list(
           xaxis = list(title = "PC1"),
           yaxis = list(title = "PC2"),
           zaxis = list(title = "PC3")
         ))

# Now we can compare the results of the to k - means clustering with k = 2 and k = 3 by using the silhouette method

# 1) For k = 2 we have:

silhouette_k_2 <- silhouette(kmeans_k_2$cluster, dist(pca_data[, 1:2]))

# After we can give also a graphical representation of the silhouette results for k = 2

fviz_silhouette(silhouette_k_2)

# we can also print the average silhouette width for k = 2 which is 0.4415739 as we can see

mean (silhouette_k_2[, 3])

# 2) For k = 3 we have:

silhouette_k_3 <- silhouette(kmeans_k_3$cluster, dist(pca_data_2[, 1:3]))

# We can give a graphical representation of the silhouette results also for k = 3 like the following

fviz_silhouette(silhouette_k_3)

# then we can print also the average width of the silhouette for k = 3, which is 0.3771361

mean (silhouette_k_3[, 3])

# Since for k = 2 the average silhouette score is 0.4415739 and for k = 3 the average silhouette score is 0.3371361, the stronger
# clustering is with k = 2, because 0.4415739 > 0.3371361, so the average silhouette score is higher for k = 2 in respect to k = 3

# --------------------------------------------- Hierarchical clustering -----------------------------------------------------------
# Hierarchical clustering differs from the k - means clustering because, in hierarchical clustering we do not know in advance
# how many clusters we want; in fact, we end up with a tree - like visual representation of the observations, called a "dendrogram",
# that allows us to view at once the clusterings obtained for each possible number of clusters, from 1 to n (definition from book
# "An introduction to statistical learning with applications in R)

# We can use use 5 different types of hierarchical clustering techniques like the following ones. First we do it for k = 2:

# 1) With k = 2 

# fist we compute the distance matrix of the pca data 

distance_matrix <- dist(pca_data)
distance_matrix

# SINGLE LINKAGE

h1 <- hclust(distance_matrix, method = "single")
plot (h1, main = "Single linkage dendrogram")

# AVERAGE LINKAGE

h2 <- hclust(distance_matrix, method = "average")
plot (h2, main = "Average linkage dendrogram")

# COMPLETE LINKAGE

h3 <- hclust(distance_matrix, method = "complete")
plot (h3, main = "Complete linkage dendrogram")

# CENTROID

h4 <- hclust(distance_matrix, method = "centroid")
plot (h4, main = "Centroid method dendrogram")

# WARD (usually it is the best one to use, in fact this create the most clear dendrogram among the other methodes used above for
# hierarchical clustering)

h5 <- hclust(distance_matrix, method = "ward.D2")
plot (h5, main = "Ward method dendrogram for k = 2")

# 2) Since the best method is the Ward method, we apply it also also to the k = 3

distance_matrix_2 <- dist(pca_data_2)

h6 <- hclust(distance_matrix_2, method = "ward.D2")
plot (h6, main = "Ward method dendrogram for k = 3")






