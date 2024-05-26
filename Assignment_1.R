#Introduction to Bayesian Data Analysis - Assignment 1
#Ecem Nur Karagöz 
#Matrikel Nr. : 03780131

# load packages here
library(dplyr)

#Task Set 1
#For tasks 1.1-1.3, assume you throw 3 dice – normal dice with 6 sides each ;)

#Task 1.1
#Create a data frame with all possible combinations (outcomes) that can result
#from throwing all the dice. (Each row should represent one possible outcome.) 
#Print the first and last 10 rows of the data frame and 
#state how many possible outcomes there are.

# write code here
dice_values <- 1:6
all_combinations <- expand.grid(dice_values, dice_values, dice_values)
print(head(all_combinations, 10))  # First 10 rows of the data frame
print(tail(all_combinations, 10))  # Last 10 rows of the data frame

# Count total possible outcomes
total_outcomes <- nrow(all_combinations)
cat("Total possible outcomes:", total_outcomes, "\n")

#Task 1.2
#Create a data frame showing all possible sums that can result from throwing the three dice
#along with their probabilities. Report the results in a summary table (data frame) and a plot
#(visual graph).

#write code here
# Add a column for the sum of dice values
all_combinations$sum <- rowSums(all_combinations)

# Total number with their probabilities
sum_table <- all_combinations %>%
  group_by(sum) %>%
  summarise(count = n()) %>%
  mutate(probability = count / total_outcomes)

# Summary table
print(sum_table)

# Visual graph
library(ggplot2)
ggplot(sum_table, aes(x = sum, y = probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Sum", y = "Probability", title = "Probability Distribution of Dice Sum")

#Task 1.3
#Compute the probability that the sum is ≥ 10, given that at least one of the dice shows a 3.

# write code here
# Probability of at least one dice showing a 3
at_least_one_3 <- 1 - (5/6)^3

# Probability that the total is at least 10 and at least one dice rolls 3
prob_sum_at_least_10_given_3 <- sum_table %>%
  filter(sum >= 10) %>%
  summarise(probability = sum(probability))

# Print the result
cat("The probability of the sum being at least 10, if at least one dice shows a 3:", prob_sum_at_least_10_given_3$probability, "\n")

#Task Set 2
#For Task 2.1-2.3, assume you toss a globe 10 times, leading to either land or water.

#Task 2.1
#Compute the probability of all possible numbers of occurrence of land, given the candidate
#proportion of .5. Report the results in a summary table and a plot and indicate whether the
#plot shows a probability distribution or a likelihood function.

#write code here
L <- "land"
W <- "water"

compute_land_probabilities <- function(n, p) {
  land_counts <- 0:n
  probabilities <- dbinom(land_counts, n, p)
  return(data.frame(Land_Count = land_counts, Probability = probabilities))
}

# Given rate and number of tosses
p <- 0.5
n <- 10

# Calculate probabilities
land_probabilities <- compute_land_probabilities(n, p)

# Print the summary table
print(land_probabilities)

# Visualize probability distribution
ggplot(land_probabilities, aes(x = Land_Count, y = Probability)) +
  geom_point() +
  geom_line() +
  labs(x = "Land Count", y = "Probability", title = "Probability Distribution of Land Count")

#Task 2.2

#Assume you observe 7 water. Take the candidate proportions of land cp = 0, 0.1, 0.2, 0.3, 0.4,
#0.5, 0.6, 0.7, 0.8, 0.9, 1. For each of these candidates, compute the probability of observing 7
#water. Report the results in a summary table and a plot and indicate whether the plot shows
#a probability distribution or a likelihood function.

#write code here
# Calculate probabilities for each candidate proportion
compute_water_probabilities <- function(n, cp) {
  water_counts <- 0:n
  probabilities <- dbinom(water_counts, n, 1 - cp)
  return(data.frame(Water_Count = water_counts, Probability = probabilities))
}

# Number of observed water
observed_water <- 7

# Candidate proportions
candidate_proportions <- seq(0, 1, by = 0.1)

# Calculate probabilities
water_probabilities_list <- lapply(candidate_proportions, function(cp) {
  compute_water_probabilities(n, cp)
})

# Create a data frame
water_probabilities_df <- do.call(rbind, water_probabilities_list)

# Print the summary table
print(head(water_probabilities_df))

# Visualize probability distribution
ggplot(water_probabilities_df, aes(x = Water_Count, y = Probability)) +
  geom_point() +
  geom_line() + 
  labs(x = "Water Count", y = "Probability", title = "Probability Distribution of Water Count")

#Task 2.3
#For each candidate proportion of land, compute the probability of all possible number of occurrences
#of land. Report the results in a summary table, showing the probability distributions
#as columns and the likelihood functions as rows.

#write code here

# Function to compute probabilities for all candidate proportions
compute_land_probabilities_all_candidates <- function(n, candidate_proportions) {
  result <- list()
  
  for (cp in candidate_proportions) {
    land_counts <- 0:n
    probabilities <- dbinom(land_counts, n, cp)
    result[[as.character(cp)]] <- probabilities
  }
  
  return(result)
}

# Candidate proportions
candidate_proportions <- seq(0, 1, by = 0.1)

# Calculate probabilities
land_probabilities_all_candidates <- compute_land_probabilities_all_candidates(n, candidate_proportions)

# Summary table
summary_table <- as.data.frame(do.call(cbind, land_probabilities_all_candidates))
rownames(summary_table) <- 0:n
colnames(summary_table) <- paste("Proportion of Land =", candidate_proportions)

print(summary_table)

