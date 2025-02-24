# Install if not already installed
install.packages(c("tidyverse", "lubridate", "mc2d", "triangle"))
install.packages("anytime")


# Load libraries
library(tidyverse)
library(lubridate)
library(mc2d)
library(triangle)
library(anytime)


# Load dataset 
df <- read.csv("/Users/nithin/Downloads/Monte Carlo Sim/projects-operations-csv-.csv", stringsAsFactors = FALSE)


head(df)
str(df$boardapprovaldate)
head(df$boardapprovaldate, 10)

df$boardapprovaldate <- gsub(" PDT", "", df$boardapprovaldate)  
df$boardapprovaldate <- as.Date(df$boardapprovaldate, format="%Y-%m-%d")
summary(df$boardapprovaldate)
head(df$boardapprovaldate)

df <- df %>% filter(boardapprovaldate >= as.Date("1950-01-01"))
df$closingdate <- gsub(" PDT", "", df$closingdate)  
df$closingdate <- as.Date(df$closingdate, format="%Y-%m-%d")

df <- df %>% filter(closingdate >= as.Date("1950-01-01"))
df <- df %>% filter(duration_days >= 0)
summary(df$duration_days)
hist(df$duration_days, breaks=50, col="blue", main="Project Durations", xlab="Days")

# Define function for Monte Carlo Simulation using PERT Distribution
simulate_project_duration <- function(actual_duration, n=10000) {
  optimistic <- actual_duration * 0.8  # 80% of actual
  most_likely <- actual_duration       # Actual duration
  pessimistic <- actual_duration * 1.2 # 120% of actual
  
  # Generate 10,000 random samples from PERT distribution
  sim_durations <- rpert(n, min=optimistic, mode=most_likely, max=pessimistic, shape=4)
  
  return(sim_durations)
}

# Pick a sample project for visualization
sample_project <- df %>% sample_n(1)  # Random project
sim_results <- simulate_project_duration(sample_project$duration_days)

# Plot the simulated project completion times
hist(sim_results, breaks=50, col="blue", main="Monte Carlo Simulation of Project Completion Time",
     xlab="Project Duration (Days)", probability=TRUE)

# Add density curve
lines(density(sim_results), col="red", lwd=2)

summary(sim_results)
quantile(sim_results, c(0.05, 0.25, 0.5, 0.75, 0.95))  # Probabilities of completion time

# Simulate durations for all projects
df$simulated_mean_duration <- sapply(df$duration_days, function(x) mean(simulate_project_duration(x)))

# Identify high-risk projects (those with very high simulated durations)
high_risk_projects <- df %>% filter(simulated_mean_duration > quantile(df$simulated_mean_duration, 0.90))

# View high-risk projects
head(high_risk_projects)


write.csv(df, "MonteCarlo_ProjectDurations.csv", row.names=FALSE)

