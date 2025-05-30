
# Install and load required packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")
install.packages("psych")
install.packages("e1071")

#Load libraries
library(readxl)
library(ggplot2)
library(car)
library(corrplot)
library(psych)
library(e1071)

# Load the dataset
Concrete_data <- read_excel("concrete compressive strength.xlsx")

# Display the first few rows of the dataset
head(Concrete_data)

# Column names
colnames(Concrete_data)

# Clean the column names
colnames(Concrete_data) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", 
                             "Water", "Superplasticizer", "Coarse_Aggregate", 
                             "Fine_Aggregate", "Age", 
                             "Concrete_Category", "Contains_Fly_Ash", "Concrete_Strength")

# Verify cleaned Column names
colnames(Concrete_data)

#Checking the structure of the data
str(Concrete_data)

#Number of roles and columns
dim(Concrete_data)

# Descriptive statistics
describe(Concrete_data)


# Checking for Missing Values
sum(is.na(Concrete_data))

# Variable Transformation
Concrete_data$Concrete_Category <- factor(Concrete_data$Concrete_Category)
Concrete_data$Contains_Fly_Ash <- factor(Concrete_data$Contains_Fly_Ash)

# Histogram for Concrete_Strength
hist(Concrete_data$Concrete_Strength)

# Square root transformation
Concrete_data$Sqrt_Concrete_Strength <- sqrt(Concrete_data$Concrete_Strength)

# Histogram for Concrete_Strength
hist(Concrete_data$Sqrt_Concrete_Strength)

# Bar plot for Concrete_Category
ggplot(Concrete_data, aes(x = Concrete_Category)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Concrete Categories", x = "Concrete Category", y = "Count")

# Bar plot for Fly_Ash
ggplot(Concrete_data, aes(x = Contains_Fly_Ash)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Fly Ash", x = "Fly Ash", y = "Count")



# Compute correlation matrix
cor_matrix <- cor(Concrete_data[, c("Cement", "Blast_Furnace_Slag",
                                    "Water", "Superplasticizer", "Coarse_Aggregate",
                                    "Fine_Aggregate", "Age", "Sqrt_Concrete_Strength")])

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle")


# Linear regression for Strength
Strength_model <- lm(Sqrt_Concrete_Strength ~ Cement + Blast_Furnace_Slag + Water + 
                       Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Contains_Fly_Ash + Age 
                     + Concrete_Category + Fly_Ash, data = Concrete_data)


# Summary of the heating model
summary(Strength_model)


#MODEL ASSUMPTIONS

# Test for linearity 
pairs(Concrete_data[,c(11,1,2,4,5,6,7,8)], lower.panel = NULL,  upper.panel = panel.smooth, pch = 19,cex = 0.2)

# Applying transformation to some of the variables
# Square root transformation
Concrete_data$Sqrt_Blast_Furnace_Slag <- sqrt(Concrete_data$Blast_Furnace_Slag)

# Log transformation
Concrete_data$Log_Fine_Aggregate <- log(Concrete_data$Fine_Aggregate)

# Log transformation
Concrete_data$Log_Coarse_Aggregate <- log(Concrete_data$Coarse_Aggregate)

# Log transformation
Concrete_data$Log_Water <- log(Concrete_data$Water)

# Square root transformation
Concrete_data$Sqrt_Superplasticizer <- sqrt(Concrete_data$Superplasticizer)

# Log transformation
Concrete_data$Log_Age <- log(Concrete_data$Age)


#New Data to check for Linearity
data <- Concrete_data [, c("Cement", "Blast_Furnace_Slag", 
                           "Log_Water", "Sqrt_Superplasticizer", "Log_Coarse_Aggregate", 
                           "Log_Fine_Aggregate", "Log_Age", "Sqrt_Concrete_Strength")]

# Test for linearity 
pairs(data[,c(8,1,2,3,4,5,6,7)], lower.panel = NULL,  upper.panel = panel.smooth, pch = 19,cex = 0.2)

#New Model with transformed variables
New_Strength_model <- lm(Sqrt_Concrete_Strength ~ Cement + Blast_Furnace_Slag + Contains_Fly_Ash + 
                        + Log_Coarse_Aggregate + Log_Fine_Aggregate 
                        + Log_Water   + Sqrt_Superplasticizer + Log_Age 
                         + Concrete_Category, data = Concrete_data)

summary(New_Strength_model)


#No multicollinearity
vif(New_Strength_model)

# Residual independence
plot(New_Strength_model, 1)

# Normality of residuals
plot(New_Strength_model, 2)

#Equal variances of the residuals (Homoscedasticity)
plot(New_Strength_model, 3)

# Hypothesis Testing

# One-Sample t-test: Testing if the Mean Compressive Strength Differs from 35
t.test(Concrete_data$Concrete_Strength, mu = 35)

# Two-sample t-test: Testing the impact of Fly Ash on Concrete_Strength
t.test(Concrete_Strength ~ Contains_Fly_Ash, data = Concrete_data)

# Chi-Square test- Testing the association Between Fly Ash Usage and Concrete Category
# Create a contingency table for Contains_Fly_Ash and Concrete_Category
Contigency_table <- table(Concrete_data$Contains_Fly_Ash, Concrete_data$Concrete_Category)

# Perform the chi-square test
chi_square_test <- chisq.test(Contigency_table)

# Display results
chi_square_test
