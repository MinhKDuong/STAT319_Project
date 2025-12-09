# Question 1
# 1) Dataset Overview:
# This data set collects vehicle-related information to replicate a genuine automobile market
# It gathers information such as car brands, models, manufacture years, price, and technical characteristics including engine size, fuel type and transmission.
# This dataset contains approximately 2500 entries with 10 distinct columns consist of 6 categorical columns and 4 numerical columns.
# As stated in the dataset content, this is an experimental study.

# 2)
car_info <- read.csv("car_price_prediction_.csv")
get_mode <- function(x) {
  x <- x[!is.na(x)]      # remove NA values
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# For quantitative columns:
# Year:
sprintf("Mean of Year = %d", round(mean(car_info$Year, na.rm = TRUE)))
sprintf("Median of Year = %d", median(car_info$Year, na.rm = TRUE))
sprintf("Mode of Year = %d", get_mode(car_info$Year))
range_year <- range(car_info$Year, na.rm = TRUE)
sprintf("Range of Year = %d to %d", range_year[1], range_year[2])
sprintf("Variance of Year = %.2f",  var(car_info$Year, na.rm = TRUE))
sprintf("Standard Deviation of Year = %.2f", sd(car_info$Year, na.rm = TRUE))
sprintf("Interquantile of Year = %d", IQR(car_info$Year))

# Engine Size:
sprintf("Mean of Engine Size = %.2f", mean(car_info$Engine.Size, na.rm = TRUE))
sprintf("Median of Engine Size = %.2f", median(car_info$Engine.Size, na.rm = TRUE))
sprintf("Mode of Engine Size = %.2f", get_mode(car_info$Engine.Size))
range_ez <- range(car_info$Engine.Size, na.rm = TRUE)
sprintf("Range of Engine Size = %.2f to %.2f", range_ez[1], range_ez[2])
sprintf("Variance of Engine Size = %.2f",  var(car_info$Engine.Size, na.rm = TRUE))
sprintf("Standard Deviation of Engine Size = %.2f", sd(car_info$Engine.Size, na.rm = TRUE))
sprintf("Interquantile of Engine Size = %.2f", IQR(car_info$Engine.Size))

# Mileage
sprintf("Mean of Mileage = %.2f", mean(car_info$Mileage, na.rm = TRUE))
sprintf("Median of Mileage = %.2f", median(car_info$Mileage, na.rm = TRUE))
sprintf("Mode of Mileage = %.2f", get_mode(car_info$Mileage))
range_mileage <- range(car_info$Mileage, na.rm = TRUE)
sprintf("Range of Mileage = %.2f to %.2f", range_mileage[1], range_mileage[2])
sprintf("Variance of Mileage = %.2f",  var(car_info$Mileage, na.rm = TRUE))
sprintf("Standard Deviation of Mileage = %.2f", sd(car_info$Mileage, na.rm = TRUE))
sprintf("Interquantile of Mileage = %.2f", IQR(car_info$Mileage))

# Price
sprintf("Mean of Price = %.2f", mean(car_info$Price, na.rm = TRUE))
sprintf("Median of Price = %.2f", median(car_info$Price, na.rm = TRUE))
sprintf("Mode of Price = %.2f", get_mode(car_info$Price))
range_price <- range(car_info$Price, na.rm = TRUE)
sprintf("Range of Price = %.2f to %.2f", range_price[1], range_price[2])
sprintf("Variance of Price = %.2f",  var(car_info$Price, na.rm = TRUE))
sprintf("Standard Deviation of Price = %.2f", sd(car_info$Price, na.rm = TRUE))
sprintf("Interquantile of Price = %.2f", IQR(car_info$Price))

# For categorical variables:
# Brand:
frq_brand <- table(car_info$Brand)
proportions_brand <- prop.table(frq_brand)
print("Frequencies of Brand")
frq_brand
print("Proportions of Brand")
proportions_brand

# Fuel Type
frq_ft <- table(car_info$Fuel.Type)
proportions_ft <- prop.table(frq_ft)
print("Frequencies of Fuel Type")
frq_ft
print("Proportions of Fuel Type")
proportions_ft

# Transmission
frq_trans <- table(car_info$Transmission)
proportions_trans <- prop.table(frq_trans)
print("Frequencies of Transmission")
frq_trans
print("Proportions of Transmission")
proportions_trans

# Condition
frq_condition <- table(car_info$Condition)
proportions_condition <- prop.table(frq_condition)
print("Frequencies of Condition")
frq_condition
print("Proportions of Condition")
proportions_condition

# Model
frq_model <- table(car_info$Model)
proportions_model <- prop.table(frq_model)
print("Frequencies of Model")
frq_model
print("Proportions of Model")
proportions_model

# 3)
sample_means <- replicate(length(car_info$Price), mean(sample(car_info$Price, 200, replace = TRUE)))
hist(sample_means, main = "Sample Distribution of Sample Mean on Price", xlab = "Sample Mean", col = "lightblue", border = "white")
# The histogram shows that the sample distribition of "Price" is symmetric


#-----------------------------------------------------------------------------------------------------------#



# Question 2
# 1)
# H_0: sample mean = 50000 ; H_1: sample mean > 50000
# Research question: is the true average car price is greater than 50000?

# 2)
# Check sample size:
# Sample is large or satisfied large sample requirement if the length of the variable is >= 30
if (length(car_info$Price) >= 30) {
  print("Sample size is satisfied")
} else {
  print("Sample size is not large enough")
}

# Check normality:
# Looking at the histogram displayed in question 1 part 3, it shows the normality requirement is satisfied

# Check equal variances
# Since this is one sample test, it is unable to verify equal variances when only given 1 sample

# Check independence
# Data is independence if each entry is unique thus condition below displays whether dataset is independence
if (any(duplicated(car_info)) == FALSE) {
  print("The chosen dataset is independence with unique entries")
} else {
  print("The chosen dataset is not independences with repeated, duplicated entries")
}

# 3)
# Conduct z-test since sample size is large with known standard deviation
xbar <- mean(car_info$Price, na.rm = TRUE)
n <- length(car_info$Price)
sigma <- sd(car_info$Price, na.rm = TRUE)
miu0 <- 50000

z <- (xbar - miu0) / (sigma / sqrt(n))
z

# 4) 
# Compute p-value
p <- 1 - pnorm(z)

# Compute 95% CI
alpha <- 0.05
lower_bound <- xbar - (qnorm(1 - alpha/2) * sigma / sqrt(n))
upper_bound <- xbar + (qnorm(1 - alpha/2) * sigma / sqrt(n))
sprintf("95%% CI = (%.2f, %.2f)", lower_bound, upper_bound)

# 5)
# Check p-value with 95% CI
if (p <= alpha) {
  print("With p <= 0.05, we rejected H_0")
} else {
  print("with p > 0.05, we failed to reject H_0")
}


#---------------------------------------------------------------------------------------------#




#---------------------------------------------------------------------------------------------#


# Question 4
# 1)
# Perform Anova of Price across Brand, Fuel Type, Condition, Model
# Anova on Brand
anova_brand <- aov(Price ~ Brand, data = car_info)
summary_brand <- summary(anova_brand)
f_brand <- summary_brand[[1]]$`F value`[1]
p_brand <- summary_brand[[1]]$`Pr(>F)`[1]
# Rejection region with 95% CI 
alpha <- 0.05
brand_df1 <- summary_brand[[1]]$Df[1]
brand_df2 <- summary_brand[[1]]$Df[2]
rr_brand <- qf(1 - alpha, brand_df1, brand_df2)
sprintf("Test statistic of ANOVA of Price by Brand is %.4f", f_brand)
sprintf("P-value of ANOVA of Price by Brand is %.4f", p_brand)
sprintf("Rejection region of ANOVA of Price by Brand is >= %.4f", rr_brand)

# Anova on Fuel Type
anova_ft <- aov(Price ~ Fuel.Type, data = car_info)
summary_ft <- summary(anova_ft)
f_ft <- summary_ft[[1]]$`F value`[1]
p_ft <- summary_ft[[1]]$`Pr(>F)`[1]
# Rejection region with 95% CI
ft_df1 <- summary_ft[[1]]$Df[1]
ft_df2 <- summary_ft[[1]]$Df[2]
rr_ft <- qf(1 - alpha, ft_df1, ft_df2)
sprintf("Test statistic of ANOVA of Price by Fuel Type is %.4f", f_ft)
sprintf("P-value of ANOVA of Price by Fuel Type is %.4f", p_ft)
sprintf("Rejection region of ANOVA of Price by Fuel Type is >= %.4f", rr_ft)

# Anova on Condition
anova_condition <- aov(Price ~ Condition, data = car_info)
summary_condition <- summary(anova_condition)
f_condition <- summary_condition[[1]]$`F value`[1]
p_condition <- summary_condition[[1]]$`Pr(>F)`[1]
# Rejection region with 95% CI
condition_df1 <- summary_condition[[1]]$Df[1]
condition_df2 <- summary_condition[[1]]$Df[2]
rr_condition <- qf(1 - alpha, condition_df1, condition_df2)
sprintf("Test statistic of ANOVA of Price by Condition is %.4f", f_condition)
sprintf("P-value of ANOVA of Price by Condition is %.4f", p_condition)
sprintf("Rejection region of ANOVA of Price by Condition is >= %.4f", rr_condition)

# Anova on Model
anova_model <- aov(Price ~ Model, data = car_info)
summary_model <- summary(anova_model)
f_model <- summary_model[[1]]$`F value`[1]
p_model <- summary_model[[1]]$`Pr(>F)`[1]
# Rejection region with 95% CI
model_df1 <- summary_model[[1]]$Df[1]
model_df2 <- summary_model[[1]]$Df[2]
rr_model <- qf(1 - alpha, model_df1, model_df2)
sprintf("Test statistic of ANOVA of Price by Model is %.4f", f_model)
sprintf("P-value of ANOVA of Price by Model is %.4f", p_model)
sprintf("Rejection region of ANOVA of Price by Model is >= %.4f", rr_model)

# 2)
# ANOVA table of Price by Brand
print("ANOVA table of Price by Brand:")
summary_brand
# ANOVA table of Price by Fuel Type
print("ANOVA table of Price by Fuel Type:")
summary_ft
# ANOVA table of Price by Condition
print("ANOVA table of Price by Condition:")
summary_condition
# ANOVA table of Price by Model
print("ANOVA table of Price by Model:")
summary_model

# 3)
# Check normality
# For ANOVA on Brand
plot(anova_brand, 2)
# The plot shows an S shaped pattern which indicate systemic deviation from the normal distribution. 
# This results in normality assumption is not satisfied.
# However, due to large sample size with n = 2500, the ANOVA remains reasonably reliable

# For ANOVA on Fuel Type
plot(anova_ft, 2)
# Similar to the plot of ANOVA of Price by Brand, this plot also shows an S shaped pattern indicate a deviation from the normal distribution.
# Due to large sample size with n = 2500, this result is still reasonably reliable despite the deviation.

# For ANOVA on Condition
plot(anova_condition, 2)
# Likewise, this plot also displays an S shaped pattern similar to the previous 2 plots. 
# As the sample size is large, the result of this procedure despite the deviation is still considered to be reliable due to the ANOVA procedure generally robust to such deviations.

# For ANOVA on Model
plot(anova_model, 2)
# Similar to previous plots, this plot also shows an S shape pattern. 
# With a large sample size of n = 2500, the deviation is negligible and the result remains reliable.


# Check equal variance
# For ANOVA on Brand
plot(anova_brand, 1)
# The plot shows residual scattered with constant vertical spread across all fitted values. 
# This suggest the ANOVA model is reasonably satisfied.

# For ANOVA on Fuel Type
plot(anova_ft, 1)
# Similar to the ANOVA model on Brand, this plot also shows constant vertical spread across all fitted values.
# Since no funnel shape or systematic pattern are formed, the result is considered to be reliable.

# For ANOVA on Condition
plot(anova_condition, 1)
# Since Condition variable only has a few categories, the plot seems to be less scattered.
# However, the plot shows a constant vertical across multiple Condition categories with no curvature and no funnel shape.
# Thus, this still satisfied the requirement for equal variance.

# For ANOVA on Model
plot(anova_model, 1)
# This plot shows a more scattered result due to the Model variable consists of multiple categories. 
# With that accounted, the plot shows a constant vertical across the fitted values with no curvature.
# As a result, this plot is highly satisfied the equal variance requirement.

# Check independence
# Logically, this is satisfied as the dataset has all unique entries. 
# Thus, each observation represents a different vehicle. 

# 4)
# Based on the result of the F-test performed for the factors Brand, Fuel Type, Condition, and Model, none of the ANOVA provided sufficient statistical evidence to conclude that the car mean differs across the treatment levels of these variables.
# In each case, the observed F-value fell below the rejection region and the corresponding p-value exceed the alpha value. 
# Therefore, we failed to reject the null hypothesis of equal mean prices across groups.
# However, the visual checks indicate the ANOVA model satisfied equal variance, independence, and reasonably satisfied normality. 
# As a result, the ANOVA results are reliable despite failed to reject null hypothesis.

# Next step: 
# Consider that the single factor ANOVA indicate there is no different in mean prices across Brand, Fuel Type, Condition, and Model, we can try to conduct comparison on two or more factor variables each with at least 4 treatment levels.


#--------------------------------------------------------------------------------------------------------#





