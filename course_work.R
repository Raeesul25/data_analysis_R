# MSc in BDA
# Data Analysis CMM703
# Course work - Task 1.

# setup the working directory 
setwd('G:/MSc in BDA/Semester 1/Data Analysis/Course Work')
getwd()

# load package
library(ggplot2)
library(dplyr)
library(caret)
library(shiny)

# Load the data set
weather_data = read.csv('02. SriLanka_Weather_Dataset.csv')
nrow(weather_data)
ncol(weather_data)

# Getting the data set dimensions
dim(weather_data)

# checking the null values
colSums(is.na(weather_data))

# Getting unique values of City
unique(weather_data$city)

# Defining cities of Colombo district
colomo_cities = c('Colombo', 'Mount Lavinia', 'Kesbewa', 'Moratuwa',
                  'Maharagama', 'Athurugiriya', 'Sri Jayewardenepura Kotte',
                  'Kolonnawa', 'Oruwala')

# Extract Colombo district data
colombo_dist = weather_data[weather_data$city %in% colomo_cities, ]

# Evaporation vs Temperature 
# fit the linear model 
lm_model = lm(et0_fao_evapotranspiration ~ temperature_2m_mean, 
              data = colombo_dist)

ggplot(data = colombo_dist, aes(x = temperature_2m_mean, 
                                y = et0_fao_evapotranspiration, 
                                color = city)) +
  geom_point() +
  geom_smooth(method = lm, aes(linetype = "regression"), 
              data = lm_model, color = "black") + # adding regression line
  labs(title = "Evapotranspiration vs Mean Temperature of Colombo District",
       x = "Mean Temperature (°C)",
       y = "Evapotranspiration") +
  theme(legend.position = "top")

# Wind Speed Distribution 
ggplot(weather_data, aes(x = city, y = windspeed_10m_max)) +
  geom_boxplot() +
  labs(title = "Distribution of Maximum Wind Speeds Across Cities",
       x = "City",
       y = "Maximum Wind Speed (m/s)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# MSc in BDA
# Data Analysis CMM703
# Course work - Task 2.

# ********** Task 2.1. **********
# Load the data set
lepto_data = read.csv('02. lepto_data.csv')

# Number of observations and features:
cat("Number of observations:", nrow(lepto_data), "\n")
cat("Number of features:", ncol(lepto_data), "\n")

# extract column names
cols = names(lepto_data)

# Extract column names and their types
col_types = sapply(lepto_data, class)
unique(col_types)


# Extract 'character' features only
char_vars = col_types[col_types == 'character']
char_vars

# Checking what are the unique values and 
# how many rows for each unique value has in character features
for (col in names(char_vars)) {
  # extract unique values of character features
  char_unique = unique(lepto_data[, col])
  
  # Print information about the column
  cat("*****Column Name:", col, "********\n")
  cat("Unique Values:\n")
  print(char_unique)
  cat("\n")
}

# Convert char values into numerical
for (col in names(char_vars)){
  lepto_data[[col]] = as.numeric(lepto_data[[col]]) 
}

# recheck column type
unique(sapply(lepto_data, class))

# Extract 'logical' features only
log_vars = col_types[col_types == 'logical']
log_vars

# only PomonaF feature is logical
# get unique value of PomonaF
unique(lepto_data$PomonaF)

# PomonaF has all null values so, let's remove PomonaF feature
lepto_data = lepto_data[, -which(names(lepto_data) == "PomonaF")]

# Serial feature is a unique id feature 
# remove serial feature
lepto_data = lepto_data[, -which(names(lepto_data) == "Serial")]

# print dimension of lepto data
print(dim(lepto_data))

# Analyze Target Variable
# table of Final
table(lepto_data$Final)

# Summary of the "Final" variable
summary(lepto_data$Final)

# Plot the distribution using bar chart
ggplot(lepto_data, aes(x = Final)) +
  geom_bar(stat = "count") +
  labs(title = "Distribution of Final", 
       x = "leptospirosis status of the patient", 
       y = "Count")

# Analyze the Missing data
# get the missing counts of each feature
missing_data = colSums(is.na(lepto_data))

# create a dataframe for missing data
missing_df = data.frame(Variables = names(missing_data), 
                        Missing_Count = missing_data)

# get only null records
missing_df = missing_df[missing_df$Missing_Count != 0, ]

# visualize bar plot for null counts
ggplot(data = missing_df, aes(x = Variables, y = Missing_Count)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Missing Data Distribution", 
       x = "Variables", 
       y = "Missing Count")

# get 50% of record counts
c = nrow(lepto_data) * 0.5

# get records with more than 50% NA values
many_na_cols = missing_df$Variables[missing_df$Missing_Count > c]

# Remove features with more than 1000 null values
lepto_new = lepto_data[, !names(lepto_data) %in% many_na_cols]

# print dimension of new lepto data
print(dim(lepto_new))

# still there are some null values
# so, let's replace those null values with 99
lepto_new = replace(lepto_new, is.na(lepto_new), 99)

# check are there any null records in lepto new data
colnames(lepto_new)[colSums(is.na(lepto_new)) > 0]

# Analyze features with 99 values
# get the missing counts of each feature
data_99 = colSums(lepto_new == 99)

# create a dataframe for missing data
df_99 = data.frame(Variables = names(data_99), 
                   Count_99 = data_99)

# get only null records
df_99 = df_99[df_99$Count_99 != 0, ]

# visualize bar plot for null counts
ggplot(data = df_99, aes(x = Variables, y = Count_99)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "99 Data Distribution", 
       x = "Variables", 
       y = "99 Count")

# get records with more than 50% 99 values
many_99_cols = df_99$Variables[df_99$Count_99 > c]
length(many_99_cols)

# Remove features with more than 1000 null values
lepto_new1 = lepto_new[, !names(lepto_new) %in% many_99_cols]

# print dimension of new lepto data
print(dim(lepto_new1))

# still there are some 99 values 
# so, let's replace those 99 values with 0
lepto_new1[lepto_new1 == 99] = 0

# check are there any 99 values in lepto new data
colnames(lepto_new1)[colSums(lepto_new1 == 99) > 0]

# function for analyze given variable
analyze_vars = function(df){
  
  for (col in names(df)){
    unique_count = length(unique(df[[col]]))
    
    cat("***** Column Name:", col, "********\n")
    cat('Unique value count:', length(unique(df[, col])), '\n')
    
    # if unique value count more than 12 then
    # consider as numerical
    if (unique_count > 12){
      # summary statistic of the variable
      cat('Summary of', col, '\n')
      cat(summary(df[[col]]), '\n')
      cat('\n')
    }
    # otherwise consider as categorical
    else{
      # Analyze variable
      cat("Unique Values:\n")
      cat(unique(df[, col]), '\n')
      cat('Table of', col, '\n')
      cat(table(df[[col]]), '\n')
      cat('\n')
    }
  }
}

# calling the analyze function
analyze_vars(lepto_new1)

# MAT_set_1 feature has 0 and 1 value. its mean only one value
# so, remove the MAT_set_1 feature
lepto_new1 = lepto_new1[, !names(lepto_new1) %in% "MAT_set_1"]

# print dimension of new lepto data
print(dim(lepto_new1))

# identify the qualitative and quantitative features
qual_vars = character()
quan_vars = character()

for (col in names(lepto_new1)){
  
  # if unique value count less than 10 then
  # consider as qualitative
  if (length(unique(lepto_new1[[col]])) < 10){
    qual_vars = c(qual_vars, col)
  }
  # otherwise consider as quantitative
  else{
    quan_vars = c(quan_vars, col)
  }
}

# display the qualitative and quantitative features
quan_vars
qual_vars

# Outlier Detection
outlier_detec = function(x){
  
  # Calculate quadrilles and IQR
  q1 = quantile(x, 0.25)
  q3 = quantile(x, 0.75)
  iqr = q3 - q1
  
  # Calculate lower and upper bounds for outliers
  lower_bound = q1 - 1.5 * iqr
  upper_bound = q3 + 1.5 * iqr
  
  # Identify outlier indices
  outlier_indices = which(x < lower_bound | x > upper_bound)
  
  return(list(lower_bound, upper_bound, outlier_indices))
}

# Outliers Analysis
outliers_analysis = function(df, numeric_vars){
  
  for (col in numeric_vars){
    cat("***** Outliers of :", col, "********\n")
    
    # Outlier Detection
    outliers = outlier_detec(df[[col]])
    outlier_indices = outliers[[3]]
    
    # print the outlier details
    if (length(outlier_indices) > 0) {
      cat(col, 'has', length(outlier_indices), 'Outliers \n')
      cat(outlier_indices, '\n')
    } else {
      cat('No Outliers in', col, '\n')
    }
  }
}

# calling outlier detection function
outliers_analysis(lepto_new1, quan_vars)

# outlier features
outlier_cols = c('Income', 'WBCcount', 'Ncount', 'Lcount', 'L', 
                 'Plateletcount')

# plot the histogram to check the distribution
par(mfrow = c(2, 3))
for (col in outlier_cols){
  hist(lepto_new1[[col]], main = paste("Ditribution of", col), 
       xlab = col, 
       col = "lightblue", 
       border = "black")
}

# Outlier columns are distributed in right skewed, a robust imputation 
# technique such as median imputation could be a good choice
# Outlier Imputation
outlier_imputation = function(df){
  for (col in outlier_cols){
    cat("***** Outliers Imputation for :", col, "********\n")
    
    # Calculate median
    median_value = median(df[[col]], na.rm = TRUE)
    
    # outlier detection
    outliers = outlier_detec(df[[col]])
    lower_limit = outliers[[1]]
    upper_limit = outliers[[2]]
    cat('Lower limit :', lower_limit, '\n')
    cat('Upper limit :', upper_limit, '\n')
    
    # impute for lower outliers
    df[[col]][df[[col]] < lower_limit] = median_value
    
    # impute for upper outliers
    df[[col]][df[[col]] > upper_limit] = median_value
    
    cat("***** Done Outliers Imputation for :", col, "********\n\n")
  }
  
  return(df)
}

# calling outlier imputation function
lepto_df = outlier_imputation(lepto_new1)

# calling outlier detection function
outliers_analysis(lepto_df, outlier_cols)

# Correlation analysis
# Function to identify highly correlated features
highly_correlated_features <- function(df, threshold = 0.95) {
  
  # Calculate correlation matrix
  corr_matrix = cor(df)
  
  # Exclude self-correlations on the diagonal
  diag(corr_matrix) = 0
  
  # Get indices of highly correlated features
  highly_correlated_indices = which(abs(corr_matrix) > threshold, 
                                    arr.ind = TRUE)
  
  # Convert indices to feature names
  features = rownames(corr_matrix)[highly_correlated_indices[, 1]]
  
  # Remove duplicates
  features = unique(features)
  
  return(features)
}

# Find highly correlated features
highly_correlated = highly_correlated_features(lepto_df)
length(highly_correlated)

# remove highly correlated features
lepto_df = lepto_df[, !names(lepto_df) %in% highly_correlated]

# print dimension of new lepto data
print(dim(lepto_df))

# In Final feature 1 represented confirmed leptospirosis and 2 represented
# not confirmed leptospirosis
# so, replace 'Final' feature value 2 into 0
lepto_df$Final[lepto_df$Final == 2] = 0

# Data Transformation
# backup the data
scaled_data = lepto_df

# Apply logarithmic transformation 
scaled_data[, -ncol(scaled_data)] = log(scaled_data[, -ncol(scaled_data)] + 1)

# check are there any null records in lepto new data
colnames(scaled_data)[colSums(is.na(scaled_data)) > 0]

# if there are is null records, let's replace those null values with 0
scaled_data = replace(scaled_data, is.na(scaled_data), 0)

# check are there any null records in lepto new data
colnames(scaled_data)[colSums(is.na(scaled_data)) > 0]

# Split the Data
# Determine number of rows for training and testing
n_train = round(nrow(scaled_data) * 0.8)
n_test = nrow(scaled_data) - n_train

cat("Number of Training Records : ", n_train, "\n")
cat("Number of Testing Records : ", n_test, "\n")

# Randomly shuffle the data
shuffled_data = scaled_data[sample(nrow(scaled_data)), ]

# Task 2.2.
# Split data into training and testing sets
train_data = shuffled_data[1:n_train, ]
test_data = shuffled_data[(n_train + 1):(n_train + n_test), ]

# Print dimensions of training and testing sets
print(dim(train_data))
print(dim(test_data))

# target feature has two values so, we need do binary classification
# logistic regression is good for binary classification

# build full logistic model
model = glm(Final ~ ., data = train_data, family=binomial(link=logit))
summary(model)

# Forward selection
forward_model = step(model, direction = "forward", trace = 0)
summary(forward_model)

# Backward selection
backward_model = step(model, direction = "backward", trace = 0)
summary(backward_model)

# backward model has low AIC value 1500.
# so, best model backward model
final_model = backward_model
summary(final_model)

# Task 2.3.
# make a prediction for testing data
y_pred = predict(final_model, newdata = test_data, type = 'response')

# Change values based on condition
y_pred = ifelse(y_pred >= 0.5, 1, 0)
y_pred = factor(y_pred)

# get target values of test data
y_test = test_data$Final
y_test = factor(y_test)

# check the levels
levels(y_test)
levels(y_pred)

# Compute confusion matrix
conf_matrix = confusionMatrix(y_pred, y_test)

# Extract performance metrics
accuracy = conf_matrix$overall['Accuracy']
precision = conf_matrix$byClass['Precision']
recall = conf_matrix$byClass['Recall']
f1_score = conf_matrix$byClass['F1']

# Print the performance metrics
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# Task 2.4.
# identify non clinical features
print(names(lepto_new1))
non_clinical = c("Year", "Month", "Hospital", "Sample", "ICU", "OPD", "Sex",
                 "Age", "Ethnicity", "Income", "Education", 
                 "TertiaryEducation", "Prophylactics", "Pasttreatments", 
                 "Pastantibiotics", "Chronicillness", "Possibleexposure",
                 "Final")

# extract non clinical features
train_non_clinical = train_data[, names(train_data) %in% non_clinical]

# Print dimensions of non clinical data
print(dim(train_non_clinical))

# build full logistic model
model_nc = glm(Final ~ ., data = train_non_clinical, 
               family=binomial(link=logit))
summary(model_nc)

# Forward selection
forward_model_nc = step(model_nc, direction = "forward", trace = 0)
summary(forward_model_nc)

# Backward selection
backward_model_nc = step(model_nc, direction = "backward", trace = 0)
summary(backward_model_nc)

# backward model has low AIC value 1687.8
# so, best model backward model
final_model_nc = backward_model_nc
summary(final_model_nc)

# AIC of non clinical data is less than AIC of whole lepto dataset.

# make a prediction for non clinical testing data
y_pred_nc = predict(final_model_nc, newdata = test_data, 
                    type = 'response')

# Change values based on condition
y_pred_nc = ifelse(y_pred_nc >= 0.5, 1, 0)
y_pred_nc = factor(y_pred_nc)

# check the levels
levels(y_pred)

# Compute confusion matrix
conf_matrix_nc = confusionMatrix(y_pred_nc, y_test)

# Extract performance metrics
accuracy_nc = conf_matrix_nc$overall['Accuracy']
precision_nc = conf_matrix_nc$byClass['Precision']
recall_nc = conf_matrix_nc$byClass['Recall']
f1_score_nc = conf_matrix_nc$byClass['F1']

# Print the performance metrics
print(paste("Accuracy:", accuracy_nc))
print(paste("Precision:", precision_nc))
print(paste("Recall:", recall_nc))
print(paste("F1 Score:", f1_score_nc))

# Discussion
compare_df = data.frame(
  Dataset = c("Whole Data", "Non-Clinical"),
  Accuracy = c(accuracy, accuracy_nc),
  Precision = c(precision, precision_nc),
  Recall = c(recall, recall_nc),
  F1_Score = c(f1_score, f1_score_nc)
)
compare_df

# whole data model better performance than non clinical data 
# based on the accuracy, precision, recall, and f1 score of.


# MSc in BDA
# Data Analysis CMM703
# Course work - Task 3.

# function Outlier Detection
outlier_detec = function(x){
  
  # Calculate quadrilles and IQR
  q1 = quantile(x, 0.25)
  q3 = quantile(x, 0.75)
  iqr = q3 - q1
  
  # Calculate lower and upper bounds for outliers
  lower_bound = q1 - 1.5 * iqr
  upper_bound = q3 + 1.5 * iqr
  
  # Identify outlier indices
  outlier_indices = which(x < lower_bound | x > upper_bound)
  
  return(list(lower_bound, upper_bound, outlier_indices))
}


# function for summarize variable
summarize_vars = function(df){
  
  # identify the continuous and non continuous features
  cont_vars = character()
  non_cont_vars = character()
  
  for (col in names(df)){
    unique_count = length(unique(df[[col]]))
    
    # if unique value count more than 4 then
    # consider as numerical
    if (unique_count > 4){
      cont_vars = c(cont_vars, col)
    }
    # otherwise consider as categorical
    else{
      non_cont_vars = c(non_cont_vars, col)
    }
  }
  
  return(list(cont_vars, non_cont_vars))
}


# function for convert categorical into numerical
label_encoding = function(df, qualitative){
  for (col in qualitative){
    # extract unique values
    unique_values = unique(df[[col]])
    
    category_mapping = setNames(1:length(unique_values), unique_values)
    df[[col]] = category_mapping[df[[col]]]
  }
  return(df)
}


# function for best predictive model with performance evaluation
predictive_model = function(df, response) {
  
  response_len = length(unique(df[[response]]))
  col_type = sapply(df[response] ,class)
  
  # Check response variable type
  if (response_len == 2 & col_type == "integer") {
    type = "Binary"
  } 
  else if(col_type == "numeric"){
    type = "Continuous"
  } 
  else{ 
    print("Response variable must be continuous or binary")
    return("This function execution has ended")
  }
  
  # Determine number of rows for training and testing
  n_train = round(nrow(df) * 0.8)
  n_test = nrow(df) - n_train
  
  # Split data into training and testing sets
  train_data = df[1:n_train, ]
  test_data = df[(n_train + 1):(n_train + n_test), ]
  
  # Build the model
  if (type == "Binary") {
    # logistic model
    model_name = "Logistic Regression"
    model = glm(train_data[[response]] ~ ., data = train_data,
                family=binomial(link=logit))
  }else{
    # linear model
    model_name = "Linear Regression"
    model = lm(train_data[[response]] ~ ., data = train_data)
  }
  
  # Perform forward selection
  forward_model = step(model, direction = "forward", trace = 0)
  
  best_model = forward_model
  model_selection = "Forward Selection"
  
  # AIC value
  aic_value = AIC(best_model)
  
  # Get best features from the chosen model
  best_features = names(coef(best_model))[2:length(names(coef(best_model)))]
  
  # get target values of test data
  y_test = test_data[[response]]
  
  # model evaluation
  if (type == "Binary") {
    # make a prediction for testing data
    y_pred = predict(best_model, newdata = test_data, 
                     type = 'response')
    
    # Change values based on condition
    y_pred = ifelse(y_pred >= 0.5, 1, 0)
    
    # Calculate the number of correct predictions
    correct_predictions = sum(y_test == y_pred)
    
    # Calculate accuracy
    accuracy = correct_predictions / length(y_test)
    
  } else {
    # make a prediction for testing data
    y_pred = predict(best_model, newdata = test_data)
    
    # Mean Squared Error (MSE) for continuous response
    accuracy = mean((y_pred - y_test)^2)
  }
  
  return(list(type = type, model_selection = model_selection,
              final_model = best_model, model_name = model_name, 
              AIC_value = aic_value, accuracy = accuracy))
}


# Define the UI of R shiny dashborad
ui = fluidPage(
  titlePanel("Data Exploration and Modeling"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data CSV"),
      textInput("text", "Enter Text"),
      actionButton("process", "Process Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Types", tableOutput("combined")),
        tabPanel("Missing Values",tableOutput("missing_values")),
        tabPanel("Outliers", tableOutput("outliers")),
        tabPanel("Plots", uiOutput("plots")),
        tabPanel("Summary", tableOutput("summary"))
      )
    )
  )
)

# Define the server logic
server = function(input, output) {
  
  # R function for Data Analysis
  data_analysis = function(df, response) {
    
    column_names = setdiff(names(df), response) 
    qualitative_vars = list()
    quantitative_vars = list()
    missing_count = list()
    outlier_list = list()
    
    # Iterate through each column of the df
    for (col in names(df)) {
      
      if (is.numeric(df[, col])) {
        quantitative_vars[[col]] = "Quantitative"
        # Count missing values
        missing_count[[col]] = sum(is.na(df[[col]]))
        # Impute with mean
        df[[col]] = replace(df[[col]], is.na(df[[col]]), 
                            mean(df[[col]], na.rm = TRUE))
        # Identify outliers
        outliers = outlier_detec(df[[col]])
        outlier_indices = outliers[[3]]
        
        # Print information about outliers (optional)
        if (length(outlier_indices) > 0) {
          outlier_list[[col]] = length(outlier_indices)
        }
      } 
      else {
        qualitative_vars[[col]] = "Qualitative"
        # Count missing values
        missing_count[[col]] = sum(is.na(df[[col]]))
        # Get mode of column
        mode_val = names(sort(table(df[[col]]), decreasing = TRUE)[1])
        df[[col]][is.na(df[[col]])] = mode_val
      }
    }
    
    # calling the summarize variable function
    summary_analysis = summarize_vars(df)
    
    # identify the continuous and non continuous features
    cont_vars = summary_analysis[[1]]
    non_cont_vars = summary_analysis[[2]]
    
    # store plots
    plots = list()
    
    # histogram plot for numerical variables
    for (col in cont_vars){
      plots[[col]] = ggplot(df, aes(x = df[[col]])) +
        geom_histogram(fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of", col),
             x = col,
             y = "Frequency")
    }
    
    # Bar plot plot for categorical variables
    for (col in non_cont_vars){
      plots[[col]] = ggplot(df, aes(x = factor(df[[col]]))) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = paste("Bar Plot of", col),
             x = col,
             y = "Frequency")
    }
    
    encoded_df = label_encoding(df, names(qualitative_vars))
    
    # calling the function of predictive model
    model_data = predictive_model(encoded_df, response)
    
    combined = append(qualitative_vars, quantitative_vars)
    type = model_data$type
    model_name = model_data$model_name
    model_selection = model_data$model_selection
    AIC_value = model_data$AIC_value
    accuracy = model_data$accuracy
    
    summary_list = list() 
    summary_list[["Number of Observations"]] = nrow(df) 
    summary_list[["Number of Features"]] = ncol(df) 
    summary_list[["Qualitative Features"]] = length(names(qualitative_vars)) 
    summary_list[["Quantitative Features"]] = length(names(quantitative_vars)) 
    summary_list[["Features with Outlier"]] = length(names(outlier_list)) 
    summary_list[["Type of Target Feature"]] = type 
    summary_list[["Suitable Model Name"]] = model_name
    summary_list[["Selected Best Model"]] = model_selection 
    summary_list[["AIC of Best Model"]] = AIC_value
    summary_list[["Model Evaluation"]] = accuracy
    
    return(list(comlist = combined, 
                qualitative = qualitative_vars, 
                quantitative = quantitative_vars, 
                missing_values = missing_count, 
                outlier_values = outlier_list,
                plots = plots, 
                type = model_data$type,
                model_name = model_data$model_name, 
                model_selection = model_data$model_selection,
                AIC = model_data$AIC_value, 
                accuracy = model_data$accuracy,
                summary_data = summary_list))
  }
  
  # Reactive expression to process data when button is clicked
  data_processed = eventReactive(input$process, {
    req(input$file)
    data = read.csv(input$file$datapath)
    text = input$text
    data_analysis(data, text)
  })
  
  # for data types
  output$combined = renderTable({
    combined = data_processed()$comlist
    df = data.frame(
      "Column Name" = names(combined),
      "Data Type" = unlist(combined),
      stringsAsFactors = FALSE
    )
    df
  }, rownames = FALSE)
  
  # for missing values
  output$missing_values = renderTable({
    missing = data_processed()$missing_values
    df = data.frame(
      "Column Name" = names(missing),
      "Missing Count" = unlist(missing),
      stringsAsFactors = FALSE
    )
    if (length(missing) == 0) {
      paste("No Missing Values")
    }else{df}
    
  }, rownames = FALSE)
  
  # for outliers
  output$outliers = renderTable({
    outliers = data_processed()$outlier_values
    df = data.frame(
      "Column Name" = names(outliers),
      "Outliers Count" = unlist(outliers),
      stringsAsFactors = FALSE
    )
    if (length(missing) == 0) {
      paste("No Outliers detected")
    }else{df}
    
  }, rownames = FALSE)
  
  # for plots
  output$plots = renderUI({
    plots = data_processed()$plots
    plot_list = lapply(names(data_processed()$plots), function(col) {
      plotOutput(paste0("plot_", col))
    })
    
    # Arrange the plots in columns and rows
    do.call(tagList, lapply(seq_along(data_processed()$plots), function(i) {
      fluidRow(column(width = 6, plot_list[i]))
    }))
  })
  
  # Render each plot
  observe({
    plots = data_processed()$plots
    for (col in names(data_processed()$plots)) {
      output[[paste0("plot_", col)]] = renderPlot({
        data_processed()$plots[[col]]
      })
    }
  })
  
  # for model data
  output$summary = renderTable({ 
    summary = data_processed()$summary_data 
    df = data.frame( 
      "Type" = names(summary), 
      "Data" = unlist(summary), 
      stringsAsFactors = FALSE ) 
    if (length(missing)== 0) { 
      paste("No Missing Values") 
    }
    else{df} 
  }, rownames = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
