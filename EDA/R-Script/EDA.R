#load the dataset
# Load the clinical dataset
clinical_data <- read.csv("clinical.csv")

# Load the genomics dataset
genomics_data <- read.csv("genomics.csv")

# Example code to merge
merged_data <- merge(clinical_data, genomics_data, by = "ID", all.x = TRUE)


# save the file
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

# Data Preprocessing
# Convert "NULL" to NA for the entire dataset
merged_data[merged_data == "NULL"] <- NA

# Alternatively, convert "NULL" to NA for a specific column if need be
# my_data$ColumnName[my_data$ColumnName == "NULL"] <- NA


# Get the names of numerical columns
numerical_columns <- names(merged_data)[sapply(merged_data, is.numeric)]

# Print the names of numerical columns
print(numerical_columns)

# Loop through each numerical column to apply mean imputation
for(col in numerical_columns) {
  merged_data[[col]][is.na(merged_data[[col]])] <- mean(merged_data[[col]], na.rm = TRUE)
}

# checking:

# Check the structure of your dataframe to see the data types of each column
str(merged_data)

# Check the number of non-NA values in each numerical column
for(col in numerical_columns) {
  print(paste(col, ":", sum(!is.na(merged_data[[col]]))))
}

##Based on this output, we can see that some columns that should be numeric are actually character columns (for example, Tumor.Size, N, and M). This could be one reason why imputation didn't work as expected.

# Before imputing missing values, character columns  should be converted to numeric
# Convert character columns to numeric
merged_data$Tumor.Size <- as.numeric(as.character(merged_data$Tumor.Size))
merged_data$N <- as.numeric(as.character(merged_data$N))
merged_data$M <- as.numeric(as.character(merged_data$M))

# Check the structure again to make sure the conversion worked
str(merged_data)

# after examining the dataset, I decided to do KNN_Imputation
# Load the necessary library
install.packages("tidymodels")
install.packages("recipes")

library(recipes)

library(tidymodels)

recipe_obj <- recipe(~ ., data = merged_data) %>%
  step_impute_knn(all_predictors())

prep_obj <- prep(recipe_obj, training = merged_data)
baked_data <- bake(prep_obj, new_data = NULL)


# Check if NAs are imputed
sum(is.na(baked_data))

#export the data
write.csv(baked_data, "cleaned_data.csv", row.names = FALSE)

#EDA 
#Histogram

# library(ggplot2)
ggplot(baked_data, aes(x=Survival.Months)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7) +
  geom_vline(aes(xintercept=mean(Survival.Months)),
             color="red", linetype="dashed", size=1) +
  ggtitle("Histogram for Survival Months")

# Boxplot

ggplot(baked_data, aes(y=Survival.Months)) +
  geom_boxplot() +
  ggtitle("Boxplot for Survival Months")

# Trends and Patterns
# Correlation Analysis: To identify the relationship between variables.
cor_matrix <- cor(baked_data[, numerical_columns], use = "complete.obs")
print(cor_matrix)

install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix)

#Survival Analysis
# Kaplan-Meier Estimator: To estimate the survival function.
install.packages("survival")
library(survival)

# Then, fit the Kaplan-Meier survival curve.

surv_fit <- survfit(Surv(Survival.Months) ~ 1, data = baked_data)
plot(surv_fit, main = "Kaplan-Meier Estimation")

# clustering
# Clustering Genes

# Load Libraries and Data
library(cluster)
# genomics_data has genes as columns and IDs as rows




