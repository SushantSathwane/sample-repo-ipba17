library(dplyr)
library(ggplot2)
library(DataExplorer)
library(summarytools)
library(reshape2)  
library(corrplot)


##### Import & Clean
## setwd("C:\\Data\\Kaushal\\Study\\IPBA\\R_Scripts")

#Data Import
data <- read.csv("https://raw.githubusercontent.com/KunaalNaik/R_Basics/main/IPBA%2017/Employee_Attrition.csv")

##### Categorical

# Attrition Plot
attrition_plot <- ggplot(data, aes(x = factor(Attrition))) + geom_bar() +
  labs(title = "Attrition Bar Plot",
       x = "Attrition",
       y = "Count")
print(attrition_plot)


# Attrition By Department
# Stage 1 - Calculate the percentage of attrition by department
attrition_percentage <- data %>%
  group_by(Department) %>%
  summarize(AttritionRate = mean(Attrition) * 100)

# Stage 2 Plotting
attrition_dept_plot <- ggplot(attrition_percentage, aes(x = Department, y = AttritionRate, fill = Department)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Attrition by Department",
       x = "Department",
       y = "Attrition Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(attrition_dept_plot)


# Avg Salary by JobRole
ggplot(data, aes(x =MonthlyIncome , y = JobRole,fill=JobRole)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Monthly Income by Job Role",
       x = "Job Role",
       y = "Average Monthly Income")



##### Numerical Plots

### Histogram - Age
age_histogram <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black")

# Display the histogram
print(age_histogram)

### Histogram - MonthlyIncome
### Histogram - Age
MonthlyIncome_histogram <- ggplot(data, aes(x = MonthlyIncome)) +
  geom_histogram(binwidth =1000, fill = "grey", color = "black")

# Display the histogram
print(MonthlyIncome_histogram)

# Display the histogram



### Box Plot - Age
age_boxplot <- ggplot(data, aes(y = Age)) +
  geom_boxplot(fill = "skyblue", color = "black")+
  labs(title = "check this", y="Age")

# Display the box plot
print(age_boxplot)

### Box Plot - MonthlyIncome
MonthlyIncome_boxplot <- ggplot(data, aes(y = MonthlyIncome)) +
  geom_boxplot(fill = "skyblue", color = "black") 

# Display the box plot
print(MonthlyIncome_boxplot)


### Correlation Heat Map
# 1 Select numerical features for correlation analysis
numerical_features <- data[, c("Age","DailyRate","MonthlyRate","MonthlyIncome","YearsAtCompany")]

# 2 Calculate the correlation matrix
correlation_matrix <- cor(numerical_features)
# 3 Create a correlation heatmap using corrplot
corrplot(correlation_matrix, method = "number", tl.cex = 0.7)
