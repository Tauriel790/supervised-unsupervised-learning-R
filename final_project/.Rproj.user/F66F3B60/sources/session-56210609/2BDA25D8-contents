# load the libraries needed for the project
library (tidyverse)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(factoextra)
library(e1071)
library(dplyr)
library(caret)
library(pROC)
library(glmnet)
library(randomForest)
library(kernlab)
library(rpart)
library(rpart.plot)

# For the supervised learning part of the project i decided to use another data set, which is about breast cancer, and the target
# of this analysis will be to identify, based on some features of the cancer, if it is malignant (M) or not (B).

# First we load the data set into our environment

data <- read.csv("breast_cancer.csv")
head (data)

# ---------------------------------- analysis (EDA) and preparation of the data before modelling---------------------------------------

# First of all, we check if there are any null values present in the data set. We can see that there are no null values present in 
# our data set
is.na(data)
sum(is.na(data))

# We check also if there are any duplicates. And, as we can see, there aren't also any duplicates in the data

anyDuplicated(data)

# Now to visualize how many individuals have malignant cancer (M) or not (B), we can print a table which summarize the number of
# M and B in the data set. We can see that there are 212 individuals with a malignant form of breast cancer (M), and 357 individuals
# with a non - malignant form of breast cancer (B)

print (data$diagnosis)
table (data$diagnosis)

# Now, it is better to change malignant = M and benign = B, to malignant = 1 and benign = 0

data$diagnosis <- ifelse(data$diagnosis == "M", 1, 0)
print(data$diagnosis)

# To ensure that we converted them correctly we print again table (data$diagnosis) and we verify if the individuals who have a 
# malignant form and those who have a non malignant form are in the same numbers has before. Using the table function we can see that
# as before M = 212 and B = 357 but know they changed to M = 1 and B = 0.

table (data$diagnosis)

# Now we visualize the data about diagnosis throughout a bar plot. We can see that individuals with a malignant form of breast cancer
# are way less than those who have a non malignant form. In fact the pink column (non - malignant cancer = 0) is higher than the 
# magenta (malignant cancer = 1). 

ggplot (data, aes(x = as.factor(diagnosis), fill = diagnosis))+
  geom_bar ()+
  scale_fill_gradient (low = "pink", high = "magenta") +
  labs (title = "Diagnosis bar plot",
        x = "Benign = 0, Malignant = 1" )+
  theme(legend.position = "none")

# Now we can visualize the distribution of all of the variables of the dataset by using histograms and density plots

# first, we can do it using histograms.

filtered_data <- data [, !(names(data) %in% c("id", "diagnosis"))]
data_reshaped_long <- melt (filtered_data)

ggplot (data_reshaped_long, aes(x = value))+
  geom_histogram(col = "purple", fill = "pink", bins = 20)+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Distribution of the variables")

# By seeing the visualization of the distributions, it is better to drop the id column because it is not useful for our analysis since
# its only and identifier. Before removing it we check if there are any patient Id duplicates

anyDuplicated(data$id)

# since there are no duplicates we can remove the id column from our data, then we print the summary of our data in order to check
# if the variable its still there or it has been correctly removed from our original data.

data <- data[, -1]
summary(data)

# Now we can visualize the distribution of the dataset by using also the density plots like the following:

ggplot(data_reshaped_long, aes (x = value))+
  geom_density(col = "red", fill = "lightyellow")+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "density plots")

# We can also unite together the 2 graphical representations we have done before like this:

ggplot(data_reshaped_long, aes (x = value))+
  geom_histogram(aes (y = after_stat(density)), col = "purple", fill = "pink", bins = 20)+
  geom_density(col = "red")+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Distribution of the variables")

# Now, we can make some insights about the variables that are present in our dataset. Since it is about the diagnosis of a malignant
# or benign form of breast cancer, we can analyze the influence of some cancer features on the diagnosis and see which of them 
# increase the chance of getting a malignant form in respect to other features

# 1) What is the relationship between "radius mean" of the cancer and the "diagnosis"? 

#    We can see it by using a density plot which will show the difference in distribution between the two variables. We can see from 
#    the two box plots that if the radius of the cancer is longer, then there is a higher probability that the cancer is malignant 
#    rather then benign. So individuals who have a cancer with longer radius, are more likely to develop a malignant form of breast 
#    cancer. 

ggplot(data, aes(x = as.factor(diagnosis), y = radius_mean, fill = diagnosis)) +
  geom_boxplot() +
  scale_fill_gradient(low = "pink", high = "purple") +
  labs (title = "Radius mean vs Diagnosis",
        x = "Benign = 0, Malignant = 1")+
  theme_light()+
  theme(legend.position = "none")
  
# we can also visualize the relationship by using a scatter plot to show the relationship between them. The red dots represent the 
# malignant form of breast cancer, and the blue dots represent the benign form. As we can see also from the scatter plot, has the 
# radius mean increases, also the chance of having a malignant breast cancer increases. On the other hand, if and individual has
# lower radius means there is an higher chance that the breast cancer turns out to be benign. So, there is a clear separation between 
# the two classes, so we can say that the radius mean is a strong predictor for detecting if a breast cancer is malignant or not. 

ggplot (data, aes(x = as.factor(diagnosis), y = radius_mean, color = as.factor (diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c("pink", "purple" ), labels = c("Benign = 0", "Malignant = 1"))+
  labs (title = "Radius mean vs Diagnosis",
        x = "Diagnosis", y = "Radius mean",
        color = "Breast cancer Diagnosis")+
  theme_minimal()

# We can use also a histogram to show the relationship

ggplot(data, aes(x = radius_mean))+
  geom_histogram(color = "white", fill = "pink", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs (title = "Radius mean vs Diagnosis",
        x = "Radius mean", y = "Number of patients")+
  theme_get()

# 2) What is the relationship between "texture mean" and "diagnosis"?

#    As we have done before, first we visualize the relationship between the two variables by using a box plot. Since the median
#    (represented by the black line in the middle of the two box plots), is higher for malignant in respect to the benign cancer, 
#    means that individuals who have higher values of texture mean for the cancer, tend to have a cancer that will be diagnosed as
#    malignant. 

ggplot(data, aes(x = as.factor(diagnosis), y = texture_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "lightgreen", high = "darkgreen")+
  labs (title = "Texture mean vs Diagnosis",
        x = "Benign = 0, Malignant = 1", y = "Texture mean")+
  theme_light()+
  theme(legend.position = "none")

#   then we use a scatter plot. We can see that also the texture mean is a great predictor for the diagnosis of malignant vs benign
#   breast cancer, but is less effective than the radius mean in the scatter plot we used before because there is a weaker shift in 
#   texture mean values between the two categories (malignant and benign) in respect to the stronger shift we had for the values of
#   radius mean.

ggplot (data, aes(x = as.factor(diagnosis), y = texture_mean, color = as.factor (diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c("lightgreen", "darkgreen"), labels = c("Benign = 0", "Malignant = 1"))+
  labs (title = "Texture mean vs Diagnosis",
        x = "Diagnosis", y = "Texture mean", color = "Breast cancer diagnosis" )+
  theme_minimal()

# We can use also a histogram to show the distribution of the variables through the two classes

ggplot(data, aes (x = texture_mean))+
  geom_histogram(color = "white", fill = "lightgreen")+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs (title = "Texture mean vs diagnosis",
        x = "Texture mean", y = "Number of patients") +
  theme_get()

# 3) What is the relationship between "area mean" and "diagnosis"?

# As before, we visualize the two variables through a box plot: We can see that also the area mean is a strong predictor for the
# cancer diagnosis, and as we see from the median of the box plots, the median for malignant cancer is higher than the median of
# the boxplot that identifies the benign diagnosis. So there is a higher chance of getting a diagnosis of malignant breast cancer as
# the area mean value increase.

ggplot(data, aes(x = as.factor(diagnosis), y = area_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "lightblue", high = "blue")+
  labs (title = "Area mean vs Diagnosis",
        x = "Benign = 0, Malignant = 1", y = "Area mean")+
  theme_light()+
  theme(legend.position = "none")

# We can see the same by also representing the relationship between the two variables through the scatter plot

ggplot(data, aes (x = as.factor(diagnosis), y = area_mean, color = as.factor(diagnosis)))+
  geom_jitter()+
  scale_color_manual (values = c("lightblue", "blue"))+
  labs (title = "Area mean vs Diagnosis",
        x = "Diagnosis", y = "Area mean", color = "Breast cancer diagnosis")+
    theme_minimal()

# Then, we can represent the relationship also by using an histogram

ggplot(data, aes(x = area_mean)) +
  geom_histogram(color = "white", fill = "lightblue", bins = 20) +
  facet_wrap(~ diagnosis, ncol = 2)+
  labs (title = "Area mean vs Diagnosis",
        x = "Area mean", y = "Number of patients")+
  theme_get()

# 4) What is the relationship between "perimeter mean" and "diagnosis"?

# Box plot:

ggplot(data, aes(x = as.factor(diagnosis), y = perimeter_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "yellow", high = "orange")+
  labs (title = "Perimeter mean vs Diagnosis",
        x = "Benign = 0, Malignant = 1", y = "Perimeter mean")+
  theme_light()+
  theme(legend.position = "none")

# scatter plot:

ggplot(data, aes(x = as.factor(diagnosis), y = perimeter_mean, color = as.factor(diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c("yellow", "orange"))+
  labs(title = "Perimeter mean vs Diagnosis",
       x = "Diagnosis", y = "Perimeter mean", color = "Breast cancer diagnosis") +
  theme_minimal()

# We can also use an histogram to show the distribution of the perimeter mean between the two classes 

ggplot(data, aes(x = perimeter_mean, fill = perimeter_mean))+
  geom_histogram(color = "white", fill = "orange", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs(title = "Perimeter mean vs diagnosis",
       x = "Perimeter mean", "Number of patients")+
  theme_get()

# 5) What is the relationship between "concavity mean" and "diagnosis"? From all the plots used to visualize the relationship 
#    between concavity mean and diagnosis, it can be seen that concavity mean is a strong predictor of the cancer diagnosis, and also
#    that has the concavity mean value rises, the chances of getting a malignant form of tumor increase as well. 
#    So, there is a strong relationship between the concavity mean of the cancer and the fact that the cancer is malignant. On the
#    other hand, cancer patients with a lower concavity mean value tend to have a benign form of breast cancer.

# Box plot:

ggplot (data, aes(x = as.factor(diagnosis), y = concavity_mean, fill = diagnosis)) +
  geom_boxplot()+
  scale_fill_gradient(low = "blue", high = "green")+
  labs(title = "Concavity mean vs diagnosis",
        x = "Benign = 0, Malignant = 1", y = "Concavity mean")+
  theme_light()+
  theme(legend.position = "none")

# Scatter plot: 

ggplot (data, aes(x = as.factor(diagnosis), y = concavity_mean, color = as.factor(diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c("blue", "green"))+
  labs(title = "Concavity mean vs diagnosis",
       x = "Diagnosis", y = "Concavity mean", color = "Breast cancer diagnosis")+
  theme_minimal()

# We can also show the distribution of the concavity mean by differentiating between the two class of diagnosis by using a 
# histogram for the visualization of the relationship between the two variables

ggplot(data, aes(x = concavity_mean))+
  geom_histogram(color = "white", fill = "blue", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs(title = "Concavity mean vs diagnosis",
       x = "Concavity mean", y = "Number of patients")+
  theme_get()

# 6) What is the relationship between "symmetry" and "diagnosis"?

# Box plot:

ggplot (data, aes(x = as.factor(diagnosis), y = symmetry_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "brown", high = "sandybrown")+
  labs (title = "Symmetry vs Diagnosis",
        x = "Benign = 0, Malignant = 1", y = "Symmetry mean")+
  theme_light()+
  theme(legend.position = "none")

# Scatter plot:

ggplot(data, aes(x = as.factor(diagnosis), y = symmetry_mean, color = as.factor (diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c("brown", "sandybrown"))+
  labs(title = "Symmetry mean vs Diagnosis",
       x = "Diagnosis", y = "Symmetry mean", color = "Breast cancer diagnosis")+
  theme_minimal()

# Histogram:

ggplot (data, aes(x = symmetry_mean))+
  geom_histogram(color = "white", fill = "sandybrown", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs(title = "Symmetry mean vs Diagnosis",
       x = "Symmetry mean", y = "Number of patients")+
  theme_get()

# 7) What is the relationship between "fractal dimension mean" and "diagnosis"?

# Box plot:

ggplot(data, aes(x = as.factor(diagnosis), y = fractal_dimension_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "#FF69B4", "#8B008B")+
  labs(title = "Fractal dimesion mean vs Diagnosis",
       x = "Benign = 0, Malignant = 1", y = "Fractal dimension mean")+
  theme_light()+
  theme(legend.position = "none")

# Scatter plot:

ggplot(data, aes(x = as.factor(diagnosis), y = fractal_dimension_mean, color = as.factor(diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c("#FF69B4", "#8B008B"))+
  labs (title = "Fractal dimension vs diagnosis",
        x = "Diagnosis", y = "Fractal dimension mean", color = "Breast cancer diagnosis")+
  theme_minimal()

# Histogram:

ggplot(data, aes(x = fractal_dimension_mean))+
  geom_histogram(color = "white", fill = "#FF69B4", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs (title = "Fractal dimension mean vs Diagnosis",
        x = "Fractal dimension mean", y = "Number of patients")+
  theme_get()

# 8) What is the relationship between "smoothness mean" and "diagnosis"?

# Box plot:

ggplot(data, aes(x = as.factor(diagnosis), y = smoothness_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "#F08080", high = "darkred")+
  labs (title = "Smoothness mean vs Diagnosis",
          x = "Benign = 0, Malignant = 1", y = "Smoothness mean")+
  theme_light()+
  theme(legend.position = "none")

# Scatter plot:

ggplot (data, aes (x = as.factor(diagnosis), y = smoothness_mean, color = as.factor(diagnosis)))+
  geom_jitter()+
  scale_color_manual (values = c ("#F08080","darkred"))+
  labs(title = "Smoothness mean vs Diagnosis",
       x = "Diagnosis", y = "Smoothness mean", color = "Breast cancer diagnosis")+
  theme_minimal()

# Histogram

ggplot(data, aes(x = smoothness_mean, fill = as.factor(diagnosis)))+
  geom_histogram(color = "white", fill = "#F08080", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs(title = "Smoothness mean vs Diagnosis",
       x = "Smoothness mean", y = "Number of patients")+
  theme_get()

# 9) What is the relationship between "compactness mean" and "diagnosis"?

# box plot:

ggplot(data, aes(x = as.factor(diagnosis), y = compactness_mean, fill = diagnosis))+
  geom_boxplot()+
  scale_fill_gradient(low = "#2E8B57", high = "#98FB98") +
  labs (title = "Compactness mean vs diagnosis",
        x = "Benign = 0, Malignant = 1", y = "Comptactness mean")+
  theme_light()+
  theme(legend.position = "none")

# Scatter plot:

ggplot (data, aes (x = as.factor(diagnosis), y = compactness_mean, color = as.factor(diagnosis)))+
  geom_jitter()+
  scale_color_manual(values = c ("#2E8B57", "#98FB98"))+
  labs(title = "Compactness mean vs Diagnosis",
       x = "Diagnosis", y = "Compactness mean", color = "Breast cancer diagnosis")+
  theme_minimal()

# Histogram:

ggplot(data, aes(x = compactness_mean))+
  geom_histogram(color = "white", fill = "#98FB98", bins = 20)+
  facet_wrap(~ diagnosis, ncol = 2)+
  labs (title = "Compactness mean vs Diagnosis",
        x = "Compactness mean", y = "Number of patients")+
  theme_get()

# ---------------------------------------- preparing the data for modelling -------------------------------------------------------

# Now before apply different supervised models to our data set, it is really important to prepare the data by eliminating missing
# values, indentify outliers and remove them, checking the skewness of the data and modify it ect ....

numeric_data <- data[, sapply(data, is.numeric)]

# We first check the skewness of these variables, to find if they are left skewed, right skewed or they are symmetric. But first
# we have to exclude the variable "diagnosis" because, even though it is numeric, it is a binary feature and not a continuous feature
# so we can calculate the skewness for it, but if we do, that value has no meaning.

skewness_values <- sapply(numeric_data[, names(numeric_data) != "diagnosis"], skewness)
print(skewness_values)

# from the skewness values we have printed we can see that some variables like area_mean, concavity_mean, concave.points_mean and
# so on, are right skewed. So it is better to transform them by using the log transformation

high_skewed_variables <- c("area_mean", "concavity_mean", "concave.points_mean", "radius_se", "perimeter_se", "area_se", 
                           "compactness_se", "fractal_dimension_se", "area_worst")

for (var in high_skewed_variables) {
  data[[var]] <- log1p(data [[var]])
}

# Then we check if the log transformation was effective on all the highly skewed variables
sapply(data[, high_skewed_variables], skewness)

# From the skewness values we can see that there is still room for improvement in skewness for some variables like concavity_mean,
# concave.points_mean, and fractal_dimension_se which skewness values are still above 1 so they are still rightly skewed. Since its
# like this it is better to apply to them some other kinds of transformations; maybe first we can try with the sqrt () transformation

# Fist we apply the transformation to "concavity_mean":

data$concavity_mean <- sqrt(data$concavity_mean)

# Then we apply it also the the variable "concave.points_mean":

data$concave.points_mean <- sqrt(data$concave.points_mean)

# Finally, we apply the transformation also to the variable "fractal_dimension_se":

data$fractal_dimension_se <- sqrt(data$fractal_dimension_se)

# Now to check if they have better skewness values we print again the skewness values of all the highly skewed variables. 

sapply(data[, high_skewed_variables], skewness)

# Then we scale our numeric data. In the case of this dataset, each variables is of type "numeric" so we scale each of the variables
# present in the dataset, the only ones we don't scale is the patient ID because we removed it before for the fact that it is only
# an identifier of the patient and does not give any contribution to our analysis, and "diagnosis" which is a categorical variable.

scaled_data <- scale(data[, names(data) != "diagnosis"])
scaled_data <- as.data.frame(scaled_data)
scaled_data <- cbind(diagnosis = data$diagnosis, scaled_data)
summary(scaled_data)

# Second we have to check if there is the presence of outliers. As we have seen before, we can check it by using boxplots. And if 
# there are dots outside the boxplots (usually above or below the boxes), the there are outliers and we have to remove them.

# to do so, we do the boxplots of all the numerical features present in the data set

data_reshaped_long_boxplot <- melt (scaled_data[, -1])
head(data_reshaped_long_boxplot)

ggplot (data_reshaped_long_boxplot, aes(x = variable, y = value, fill = variable))+
  geom_boxplot()+
  facet_wrap(~ variable, scales = "free")+
  labs (title = "Boxplots of all the variables in the dataset")+
  theme (legend.position = "none")

# we can see that there are several outliers, so we can now remove them by using the IQR formula

outliers_removal <- function (x) {
  Q1 <- quantile (x, 0.25, na.rm = TRUE)
  Q3 <- quantile (x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  return(x >= lower_bound & x<= upper_bound)
}

# then, we apply this function of the IQR formula to our numeric data, to remove the outliers before we begin with the modelling
# part of the project

data_without_outliers <- sapply(scaled_data [, -1], outliers_removal)
data_without_outliers

rows_without_outliers <- apply (data_without_outliers, 1, all)

cleaned_data <- scaled_data[rows_without_outliers,]  # By using this command we kept only rows which contains data which not present outliers
cleaned_data

# After all of this we can do a correlation matrix, and print the respective heatmap in order to find highly correlated variables,
# and them remove some of them.

correlation_matrix <- cor(cleaned_data)
print(correlation_matrix)

# Now we can visualize the correlations by using a correlation matrix heatmap

correlation_matrix_long <- melt(correlation_matrix)

ggplot(data = correlation_matrix_long, aes (x = Var1, y = Var2, fill = value))+
  geom_tile()+
  scale_fill_gradient2(low = "pink", high = "purple", mid = "white" )+
  labs(title = "Correlation matrix heatmap")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Now we identify which are the highly correlated variables in our correlation matrix

highly_correlated_variables <- findCorrelation(correlation_matrix, cutoff = 0.9)
names(cleaned_data[highly_correlated_variables])

# After, this we see that there are several highly correlated features so we should remove some of them to avoid multicollinearity
# during some of the modelling we have to do afterwards

cleaned_data_final <- cleaned_data[, - highly_correlated_variables]
cleaned_data_final

# To check if the removal was effective, we can print the number of columns of the original cleaned_data and compare it to the
# number of the columns that are non in the final data (cleaned_data_final).
ncol(cleaned_data)
ncol(cleaned_data_final)

# ----------------------------------- Dividing the data into train and test data (60% - 40%) ---------------------------------------

# First of all we divide our data into train and test data in the proportions respectively of 60 % and 40 %

set.seed(200)

training_set_index <- createDataPartition(cleaned_data_final$diagnosis, p = 0.6, list = FALSE)
train_data <- cleaned_data_final[training_set_index, ]
test_data <- cleaned_data_final[- training_set_index, ]

# -------------------------------------------- Cross  Validation control ----------------------------------------------------------

# Firs of all, before going on with the logistic regression is important to define the control to do the cross validation

cross_validation <- trainControl(method = "cv", number = 20)

# -------------------------------------------- Logistic regression model ----------------------------------------------------------

# First we control that the diagnosis is seen as a factor

train_data$diagnosis <- factor(train_data$diagnosis, levels = c(0,1), labels = c("Benign", "Malignant"))
test_data$diagnosis <- factor(test_data$diagnosis, levels = c(0,1), labels = c("Benign", "Malignant"))

# After setting the control for cross validation, we train the logistic regression model (using the regularized version with glmnet)

logistic_regression_model <- train (diagnosis~., data = train_data, method = "glmnet", family = "binomial",
                                    trControl = cross_validation)
print (logistic_regression_model)

# Now we get the predictions and the probabilities for the logistic regression model by using this time the test data

lr_probabilities <- predict (logistic_regression_model, newdata = test_data, type = "prob")
lr_predictions <- predict (logistic_regression_model, newdata = test_data)

print (lr_probabilities)
print (lr_predictions)

# After all we have done we can create the confusion matrix and plot it througth a heatmap

confusion_matrix_lr <- table (Predicted = lr_predictions, Actual = test_data$diagnosis)
print (confusion_matrix_lr)

# To plot the confusion matrix, first we have to change it into a dataframe

confusion_matrix_lr_dataframe <- as.data.frame (confusion_matrix_lr)
print (confusion_matrix_lr_dataframe)

# Now we can plot it using the ggplot function

ggplot (confusion_matrix_lr_dataframe, aes (x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient (low = "blue", high = "red")+
  geom_text (aes(label = Freq), size = 7, color = "white")+
  labs (title = "Confusion matrix heatmap logistic regression",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# After the plotting we can also get the value of the accuracy of the model. We can see that the accuracy of the logistic regression
# model is 0.9535 so 95.35% accuracy.

logistic_regression_accuracy <- mean (lr_predictions == test_data$diagnosis)
print (paste("Logistic regression accuracy:", round (logistic_regression_accuracy, 4)))

# At the end, we can also represent the ROC curve of the model

roc_curve_logistic <- roc(test_data$diagnosis, predictor = lr_probabilities [, "Malignant"])
plot (roc_curve_logistic, col = "red", lwd = 2, main = "ROC curve for logistic regression model")
auc(roc_curve_logistic)

# accuracy, ROC and AUC for the trainig set ------------------------------------------------------------
# all of this can be done also for the training set in order to check for overfitting and underfitting

# probabilities and predictions on the train set

lr_probabilities <- predict (logistic_regression_model, newdata = train_data, type = "prob")
lr_predictions <- predict (logistic_regression_model, newdata = train_data)

print (lr_probabilities)
print (lr_predictions)

# confusion matrix on the train set 

confusion_matrix_lr <- table (Predicted = lr_predictions, Actual = train_data$diagnosis)
print (confusion_matrix_lr)

# To plot the confusion matrix, first we have to change it into a dataframe

confusion_matrix_lr_dataframe <- as.data.frame (confusion_matrix_lr)
print (confusion_matrix_lr_dataframe)

# Now we can plot it using the ggplot function

ggplot (confusion_matrix_lr_dataframe, aes (x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient (low = "blue", high = "red")+
  geom_text (aes(label = Freq), size = 7, color = "white")+
  labs (title = "Confusion matrix heatmap logistic regression",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# accuracy

logistic_regression_accuracy_train <- mean (lr_predictions == train_data$diagnosis)
print (paste("Logistic regression accuracy for the train set:", round (logistic_regression_accuracy_train, 4)))

# ROC and AUC for the train set 

roc_curve_logistic_train <- roc(train_data$diagnosis, predictor = lr_probabilities [, "Malignant"])
plot (roc_curve_logistic_train, col = "red", lwd = 2, main = "ROC curve for logistic regression model")
auc(roc_curve_logistic_train)

# comparison between the two performances:

# in this case, since the performances of the two sets are mostly equal to each other, it indicates that overfitting is minimal
# or even absent. The accuracy of the model for the train data is slightly higher than the model on the test data, and even on
# the train data, the model have a really high performance in distinguishing between mailgnant and benign classes. 

# -------------------------------------------------------- Decision tree ---------------------------------------------------------

# First we train the decision tree model

set.seed(200)
decision_tree_model <- rpart(diagnosis~., data = train_data, method = "class") 

# Now we plot the decision tree by using rpart.plot

rpart.plot (decision_tree_model, type = 2, extra = 104, fallen.leaves = TRUE, branch = 0,
            main = "Classification tree")

# Now we evaluate the performance of the model

decision_tree_predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
decision_tree_probabilities <- predict(decision_tree_model, newdata = test_data, type = "prob")

print(decision_tree_predictions)
print(decision_tree_probabilities)

# First we get the confusion matrix of the model

confusion_matrix_dt <- table (Predicted = decision_tree_predictions, Actual = test_data$diagnosis)
print (confusion_matrix_dt)

# Now first to plot the confusion matrix using ggplot we have to convert it into a data frame

confusion_matrix_dt_dataframe <- as.data.frame(confusion_matrix_dt)
print(confusion_matrix_dt_dataframe)

# Then, we plot the confusion matrix through a heatmap

ggplot(confusion_matrix_dt_dataframe, aes(x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient(low = "brown", high = "beige")+
  geom_text(aes(label = Freq), size = 7, color = "black")+
  labs (title = "Confusion matrix heatmap Decision tree",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# After the plotting we can also get the value of the accuracy of the model. We can see that the accuracy of the decision tree
# model is 0.9186 so 91.86%

decision_tree_accuracy <- mean (decision_tree_predictions == test_data$diagnosis)
print (paste("Decision tree accuracy:", round (decision_tree_accuracy, 4)))

# At the end, we can also represent the ROC curve of the model

roc_curve_decision_tree <- roc(test_data$diagnosis, predictor = decision_tree_probabilities [, "Malignant"])
plot (roc_curve_decision_tree, col = "brown", lwd = 2, main = "ROC curve for decision tree model")
auc(roc_curve_decision_tree)

# Confusion matrix, ROC, AUC and accuracy on the train set ---------------------------------------------

decision_tree_predictions_train <- predict(decision_tree_model, newdata = train_data, type = "class")
decision_tree_probabilities_train <- predict(decision_tree_model, newdata = train_data, type = "prob")

print(decision_tree_predictions)
print(decision_tree_probabilities)

# confusion matrix and heatmap

confusion_matrix_dt <- table (Predicted = decision_tree_predictions_train, Actual = train_data$diagnosis)
print (confusion_matrix_dt)

confusion_matrix_dt_dataframe <- as.data.frame(confusion_matrix_dt)
print(confusion_matrix_dt_dataframe)

ggplot(confusion_matrix_dt_dataframe, aes(x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient(low = "brown", high = "beige")+
  geom_text(aes(label = Freq), size = 7, color = "black")+
  labs (title = "Confusion matrix heatmap Decision tree",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# accuracy

decision_tree_accuracy_train <- mean (decision_tree_predictions_train == train_data$diagnosis)
print (paste("Decision tree accuracy:", round (decision_tree_accuracy_train, 4)))

# ROC and AUC

roc_curve_decision_tree_train <- roc(train_data$diagnosis, predictor = decision_tree_probabilities_train [, "Malignant"])
plot (roc_curve_decision_tree_train, col = "brown", lwd = 2, main = "ROC curve for decision tree model for train set")
auc(roc_curve_decision_tree_train)

# ---------------------------------------------------- Random forest model -------------------------------------------------------

# Another machine learning algorithm we can use is the "random forest model", which combines the results of multiple decision trees
# in order to get a unique result for the analysis

# First we train the model on the test data

set.seed(200)
Random_Forest_model <- randomForest(diagnosis~., data = train_data, ntree = 200, mtry = 20, importance = TRUE)

# Now we print the results of the model we have implemented

print (Random_Forest_model)

# Then, we can get predictions on the test set

rf_predictions <- predict(Random_Forest_model, newdata = test_data)

# Afterwards, we can also create the confusion matrix for the model and plot it through a heatmap to show visually the results

confusion_matrix_rf <- table (Predicted = rf_predictions, Actual = test_data$diagnosis)
print (confusion_matrix_rf)

# Now first to plot the confusion matrix using ggplot we have to convert it into a data frame

confusion_matrix_rf_dataframe <- as.data.frame(confusion_matrix_rf)
print(confusion_matrix_rf_dataframe)

ggplot(confusion_matrix_rf_dataframe, aes(x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_text(aes(label = Freq), size = 7, color = "black")+
  labs (title = "Confusion matrix heatmap Random Forest",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# We can also calculate the accuracy of the random forest we have used

Random_forest_accuracy <- mean (rf_predictions == test_data$diagnosis)
print (paste("Random forest accuracy:", round(Random_forest_accuracy, 4)))

# We can also get the values of the variable importance in our model

print(importance(Random_Forest_model))

# Then we can plot the variable importance by using a variable importance plot; this will show which of the variables in the data
# are identified by the model as the most important ones

varImpPlot(Random_Forest_model, main = "Variable importance plot for random forest", color = "black")

# At the end we can also represent the roc curve and print the respective AUC. First we have to get the probabilities

rf_probabilities <- predict(Random_Forest_model, newdata = test_data, type = "prob")

roc_curve_rf <- roc(test_data$diagnosis, predictor = rf_probabilities [, "Malignant"])
plot (roc_curve_rf, col = "green", lwd = 2, main = "ROC curve of Random forest")
auc(roc_curve_rf)

# Confusion matrix, ROC, AUC and accuracy for the train set

# predictions and probabilities

rf_predictions_train <- predict(Random_Forest_model, newdata = train_data)
rf_probabilities_train <- predict(Random_Forest_model, newdata = train_data, type = "prob")

# confusion matrix and heatmap

confusion_matrix_rf_train <- table (Predicted = rf_predictions_train, Actual = train_data$diagnosis)
print (confusion_matrix_rf_train)

confusion_matrix_rf_dataframe <- as.data.frame(confusion_matrix_rf_train)
print(confusion_matrix_rf_dataframe)

ggplot(confusion_matrix_rf_dataframe, aes(x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_text(aes(label = Freq), size = 7, color = "black")+
  labs (title = "Confusion matrix heatmap Random Forest",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# ROC, AUC and accuracy

Random_forest_accuracy_train <- mean (rf_predictions_train == train_data$diagnosis)
print (paste("Random forest accuracy:", round(Random_forest_accuracy_train, 4)))

roc_curve_rf_train <- roc(train_data$diagnosis, predictor = rf_probabilities_train [, "Malignant"])
plot (roc_curve_rf_train, col = "green", lwd = 2, main = "ROC curve of Random forest for the train set")
auc(roc_curve_rf_train)

# ------------------------------------------------ Support vector machines (SVM) --------------------------------------------------

# Now we can use another important machine learning algorithm which is Support Vector Machines (SVM). We have different kinds of 
# methods/types for support vector machines, but for our analysis we will use only the radial basis function, the linear basis
# function, the polynomial basis function and the sigmoid basis function. Implementing this models, we will use the cross validation
# control we have set before for the logistic regression

# ----------------------------------------------- Svm with radial basis function --------------------------------------------------

svm_radial_model <- svm (diagnosis~., data = train_data, kernel = "radial", cost = 1, gamma = 0.1, probability = TRUE)
print (svm_radial_model)

# Now we get probabilities and predictions

svm_radial_predictions <- predict (svm_radial_model, newdata = test_data, probability = TRUE)
svm_radial_probabilities <- attr(svm_radial_predictions, "probabilities")

# Now we get the confusion matrix of the model

confusion_matrix_svm_r <- table (Predicted = svm_radial_predictions, Actual = test_data$diagnosis)
print (confusion_matrix_svm_r)

# We have to transform the confusion matrix into a dataframe

confusion_matrix_svm_r_dataframe <- as.data.frame(confusion_matrix_svm_r)
print (confusion_matrix_svm_r_dataframe)

# Then we can plot the confusion matrix

ggplot (confusion_matrix_svm_r_dataframe, aes (x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient (low = "orange", high = "magenta")+
  geom_text (aes(label = Freq), size = 7, color = "white")+
  labs (title = "Confusion matrix heatmap svm radial",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# After the plotting we can also get the value of the accuracy of the model. We can see that the accuracy of the svm radial model
# As we can see the value is accuracy = 0.9748

svm_radial_accuracy <- mean (svm_radial_predictions == test_data$diagnosis)
print (paste("Svm radial accuracy:", round (svm_radial_accuracy, 4)))

# At the end, we can also represent the ROC curve of the model

roc_curve_svm_radial <- roc(test_data$diagnosis, predictor = svm_radial_probabilities [, "Malignant"])
plot (roc_curve_svm_radial, col = "magenta", lwd = 2, main = "ROC curve for SVM radial model")
auc(roc_curve_svm_radial)

# accuracy, ROC, AUC on the train set ----------------------------------------

# probabilities and predictions

svm_radial_predictions_train <- predict (svm_radial_model, newdata = train_data, probability = TRUE)
svm_radial_probabilities_train <- attr(svm_radial_predictions_train, "probabilities")

# confusion matrix and its heatmap

confusion_matrix_svm_r <- table (Predicted = svm_radial_predictions_train, Actual = train_data$diagnosis)
print (confusion_matrix_svm_r)

confusion_matrix_svm_r_dataframe <- as.data.frame(confusion_matrix_svm_r)
print (confusion_matrix_svm_r_dataframe)

ggplot (confusion_matrix_svm_r_dataframe, aes (x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient (low = "orange", high = "magenta")+
  geom_text (aes(label = Freq), size = 7, color = "white")+
  labs (title = "Confusion matrix heatmap svm radial",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# accuracy

svm_radial_accuracy_train <- mean (svm_radial_predictions_train == train_data$diagnosis)
print (paste("Svm radial accuracy on train set:", round (svm_radial_accuracy_train, 4)))

# At the end, we can also represent the ROC curve of the model

roc_curve_svm_radial_train <- roc(train_data$diagnosis, predictor = svm_radial_probabilities_train [, "Malignant"])
plot (roc_curve_svm_radial_train, col = "magenta", lwd = 2, main = "ROC curve for SVM radial model on train set")
auc(roc_curve_svm_radial_train)

# ----------------------------------------------- Svm with linear basis function --------------------------------------------------

svm_linear <- svm (diagnosis~., data = train_data, kernel = "linear", cost = 1, probability = TRUE)
print (svm_linear)

# Now we get the predictions and the probabilities

svm_linear_predictions <- predict (svm_linear, newdata = test_data, probability = TRUE)
svm_linear_probabilities <- attr (svm_linear_predictions, "probabilities")

# Then, we do the confusion matrix

confusion_matrix_svm_l <- table (Predicted = svm_linear_predictions, Actual = test_data$diagnosis)
print (confusion_matrix_svm_l)

# As we have done before, we have to transform the confusion matrix into a dataframe

confusion_matrix_svm_l_dataframe <- as.data.frame(confusion_matrix_svm_l)
print (confusion_matrix_svm_l_dataframe)

# Then we can plot the confusion matrix

ggplot (confusion_matrix_svm_l_dataframe, aes (x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient (low = "orange", high = "magenta")+
  geom_text (aes(label = Freq), size = 7, color = "white")+
  labs (title = "Confusion matrix heatmap svm linear",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# After the plotting we can also get the value of the accuracy of the model. We can see that the accuracy of the svm linear model
# As we can see the value is accuracy = 0.9686

svm_linear_accuracy <- mean (svm_linear_predictions == test_data$diagnosis)
print (paste("Svm linear accuracy:", round (svm_linear_accuracy, 4)))

# ROC curve of the svm linear model

roc_curve_svm_linear <- roc(test_data$diagnosis, predictor = svm_linear_probabilities [, "Malignant"])
plot (roc_curve_svm_linear, col = "magenta", lwd = 2, main = "ROC curve for SVM linear model")
auc(roc_curve_svm_linear)

# ------------------------------------- SVM model with Polynomial basis function --------------------------------------------------

svm_polynomial <- svm (diagnosis~., data = train_data, kernel = "polynomial", cost = 1, probability = TRUE)
print (svm_polynomial)

# Now we get the predictions and the probabilities

svm_polynomial_predictions <- predict (svm_polynomial, newdata = test_data, probability = TRUE)
svm_polynomial_probabilities <- attr (svm_polynomial_predictions, "probabilities")

# Then, we do the confusion matrix

confusion_matrix_svm_p <- table (Predicted = svm_polynomial_predictions, Actual = test_data$diagnosis)
print (confusion_matrix_svm_p)

# As we have done before, we have to transform the confusion matrix into a dataframe

confusion_matrix_svm_p_dataframe <- as.data.frame(confusion_matrix_svm_p)
print (confusion_matrix_svm_p_dataframe)

# Then we can plot the confusion matrix

ggplot (confusion_matrix_svm_p_dataframe, aes (x = Actual, y = Predicted, fill = Freq))+
  geom_tile(color = "black")+
  scale_fill_gradient (low = "orange", high = "magenta")+
  geom_text (aes(label = Freq), size = 7, color = "white")+
  labs (title = "Confusion matrix heatmap svm polynomial",
        x = "Actual cancer diagnosis", y = "Predicted cancer diagnosis")+
  theme_minimal()

# After the plotting we can also get the value of the accuracy of the model. We can see that the accuracy of the svm linear model
# As we can see the value is accuracy = 0.9686

svm_polynomial_accuracy <- mean (svm_polynomial_predictions == test_data$diagnosis)
print (paste("Svm polynomial accuracy:", round (svm_polynomial_accuracy, 4)))

# ROC curve of the svm linear model

roc_curve_svm_polynomial <- roc(test_data$diagnosis, predictor = svm_polynomial_probabilities [, "Malignant"])
plot (roc_curve_svm_polynomial, col = "magenta", lwd = 2, main = "ROC curve for SVM polynomial model")
auc(roc_curve_svm_polynomial)

# Now we can print together all the accuracy results for the models we have used on the dataset

accuracy_result_final <- data.frame(Model = c("Logistic regression model",
                                              "Decision tree model",
                                              "Random forest model",
                                              "SVM radial model",
                                              "SVM linear model",
                                              "SVM polynomial model"),
                                    Accuracy = c(round(logistic_regression_accuracy, 4),
                                                 round(decision_tree_accuracy, 4),
                                                 round(Random_forest_accuracy, 4),
                                                 round(svm_radial_accuracy, 4),
                                                 round(svm_linear_accuracy, 4),
                                                 round(svm_polynomial_accuracy, 4)))
print (accuracy_result_final)

# By printing the accuracy values of the various models we have used, we can see that the greatest model we can use the Support
# Vector machines model with radial basis function, which as accuracy = 0.9748, so 97% accuracy, which is higher than the accuracy
# of the other models we have used.

# We can also print all the ROC curves together to compare them, within all the models

plot(roc_curve_logistic, col = "blue", lwd = 2, main = "ROC curves for all the models")
lines(roc_curve_decision_tree, col = "green", lwd = 2)
lines(roc_curve_rf, col = "purple", lwd = 2)
lines(roc_curve_svm_linear, col = "red", lwd = 2)
lines(roc_curve_svm_radial, col = "orange", lwd = 2)
lines(roc_curve_svm_polynomial, col = "pink", lwd = 2)

legend("bottomright", legend = c("Logistic regression", "Decision Tree", "Random forest", "SVM radial", "SVM linear", 
                                 "SVM polynomial"), col = c("blue", "green", "purple", "red", "orange", "pink"), lwd = 2)

# and compare also all the AUC values. By observing the AUC values, we can confirm again that the most accurate model is the 
# Support Vector Machine model with radial basis function, because it has the highest value of AUC (area under the ROC curve),
# which is AUC = 0.9983.

AUC_values <- data.frame (Model = c("Logistic regression model",
                                    "Decision tree model",
                                    "Random forest model",
                                    "SVM radial model",
                                    "SVM linear model",
                                    "SVM polynomial model"),
                          AUC = c(auc(roc_curve_logistic),
                                  auc(roc_curve_decision_tree),
                                  auc(roc_curve_rf),
                                  auc(roc_curve_svm_radial),
                                  auc(roc_curve_svm_linear),
                                  auc(roc_curve_svm_polynomial)))
print (AUC_values)

