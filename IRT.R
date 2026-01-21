

# ========================
# IRT 2PL Analysis with Error Diagnostics and Graphs
# ========================

# Load library
library(mirt)


# Step 1: Read data (change path as needed)
data <- read.csv("C:/Users/USER/Desktop/AIMS/simulated_sampledata_irt2pl.csv")

# Step 2: Extract test taker names and item responses
test_taker_names <- data[, 1]
response_data <- data[, -1]
item_names <- colnames(response_data)

# Step 3: Fit the 2PL model
model_2PL <- mirt(response_data, 1, itemtype = "2PL")

# Step 4: View item parameters
item_params <- coef(model_2PL, IRTpars = TRUE, simplify = TRUE)$items
cat("\n=== Item Parameters (a: Discrimination, b: Difficulty) ===\n")
print(item_params)

# Step 5: Estimate person ability (θ) and Standard Errors
theta_scores <- fscores(model_2PL, full.scores.SE = TRUE)
theta_values <- theta_scores[, 1]
theta_SE <- theta_scores[, 2]

# Step 5b: First 10 and Last 3 Students - Ability & SE
cat("\n=== Ability (θ) and Standard Errors for First 10 Students ===\n")
first10 <- data.frame(
  Student = test_taker_names[1:10],
  Ability = round(theta_values[1:10], 4),
  SE = round(theta_SE[1:10], 4)
)
print(first10)

cat("\n=== Ability (θ) and Standard Errors for Last 3 Students ===\n")
last3 <- data.frame(
  Student = tail(test_taker_names, 3),
  Ability = round(tail(theta_values, 3), 4),
  SE = round(tail(theta_SE, 3), 4)
)
print(last3)

# Step 6: Print basic ranges
cat("\n--- Parameter Ranges ---\n")
cat("Ability (θ): ", range(theta_values), "\n")
cat("Discrimination (a): ", range(item_params[, "a"]), "\n")
cat("Difficulty (b): ", range(item_params[, "b"]), "\n")

# Step 7: ICCs for all items
plot(model_2PL, type = "trace", main = "Item Characteristic Curves")

# Step 8: Test Information Curve
plot(model_2PL, type = "info", main = "")

# Step 9: ICC for one selected item
item_index <- 5
plot(model_2PL, type = "trace", which.items = item_index,
     main = paste("ICC for", item_names[item_index]))

# Step 17: Test Characteristic Curve (TCC)
plot(model_2PL, type = "score", main = "Test Characteristic Curve (TCC)")

# Step 10: Barplot of Discrimination (Descending)
sorted_a <- sort(item_params[, "a"], decreasing = TRUE)
barplot(sorted_a,
        names.arg = names(sorted_a),
        las = 2,
        col = "skyblue",
        main = "Discrimination Parameters (Descending)",
        ylab = "Discrimination (a)")

# Step 11: Barplot of Difficulty (Ascending)
sorted_b <- sort(item_params[, "b"], decreasing = FALSE)
barplot(sorted_b,
        names.arg = names(sorted_b),
        las = 2,
        col = "salmon",
        main = "Difficulty Parameters (Ascending)",
        ylab = "Difficulty (b)")

# Step 12: Barplot of Estimated Person Ability (θ)
barplot(theta_values,
        col = "lightgreen",
        main = "Estimated Person Abilities (θ)",
        ylab = "Ability (θ)",
        xlab = "Test Takers",
        names.arg = rep("", length(theta_values)))  # Suppress long names

# Step 13: Barplot for first 10 test takers’ abilities
barplot(theta_values[1:10],
        names.arg = test_taker_names[1:10],
        las = 2,
        col = "violet",
        main = "Ability Estimates for First 10 Test Takers",
        ylab = "Ability (θ)")

# Step 14: Barplot of % Passed vs Failed per item
pass_fail_percent <- sapply(response_data, function(item) {
  c(Fail = mean(item == 0) * 100,
    Pass = mean(item == 1) * 100)
})

barplot(pass_fail_percent,
        beside = TRUE,
        col = c("tomato", "steelblue"),
        legend = TRUE,
        main = "Percentage of Students Passing and Failing by Item",
        ylab = "Percentage",
        las = 2)

# Step 15: Model Fit Statistics
cat("\n=== Model Fit Statistics ===\n")
cat("AIC: ", AIC(model_2PL), "\n")
cat("BIC: ", BIC(model_2PL), "\n")
cat("Log-Likelihood: ", logLik(model_2PL), "\n")

# Step 16: Item Fit (S-X² diagnostics)
cat("\n=== Item Fit Statistics (S-X²) ===\n")
item_fit <- itemfit(model_2PL)
print(item_fit)

library(ggplot2)


# Generate theta values across range
theta_grid <- seq(-4, 4, 0.1)

# Expected scores from the model (TCC)
tcc_vals <- expected.test(model_2PL, Theta = matrix(theta_grid))

df_tcc <- data.frame(theta = theta_grid, expected = tcc_vals)

# Observed total scores
observed_scores <- rowSums(response_data)
df_obs <- data.frame(theta = theta_values, observed = observed_scores)

# Bin observed scores
df_obs$theta_bin <- cut(df_obs$theta, breaks = 10)
obs_summary <- aggregate(observed ~ theta_bin, data = df_obs, mean)

# Bin midpoints
theta_mid <- sapply(strsplit(as.character(obs_summary$theta_bin), ","), function(x){
  mean(as.numeric(gsub("[^0-9.-]", "", x)))
})
df_obs_summary <- data.frame(theta = theta_mid, observed = obs_summary$observed)

# Plot with ggplot2
ggplot() +
  geom_line(data = df_tcc, aes(x = theta, y = expected), color = "black", size = 1) +
  geom_point(data = df_obs_summary, aes(x = theta, y = observed), color = "red", size = 2) +
  geom_line(data = df_obs_summary, aes(x = theta, y = observed), color = "red", linetype = "dashed") +
  labs(title = "Test Characteristic Curve (TCC) with Observed Scores",
       x = expression(theta), y = "Total Score") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80"))


# ========================
# Step X: Ability Estimates – First 10 and Last 3
# ========================

# Create table for first 10 abilities
first10 <- data.frame(
  Test_Taker = test_taker_names[1:10],
  Ability = round(theta_values[1:10], 3),
  SE = round(theta_SE[1:10], 3)
)

# Create table for last 3 abilities
last3 <- data.frame(
  Test_Taker = tail(test_taker_names, 3),
  Ability = round(tail(theta_values, 3), 3),
  SE = round(tail(theta_SE, 3), 3)
)

# Print with separation
cat("\n=== Ability Estimates (First 10) ===\n")
print(first10)

cat("\n...\n")  # three vertical dots to indicate continuation

cat("\n=== Ability Estimates (Last 3) ===\n")
print(last3)


# ========================
# Step Y: Ability Distribution Visualizations (Separate Plots)
# ========================

ability_df <- data.frame(Theta = theta_values)

# Histogram
ggplot(ability_df, aes(x = Theta)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "", x = expression(theta), y = "Frequency")

# Boxplot
ggplot(ability_df, aes(y = Theta)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "", y = expression(theta))

# Density plot
ggplot(ability_df, aes(x = Theta)) +
  geom_density(fill = "salmon", alpha = 0.6) +
  theme_minimal() +
  labs(title = "", x = expression(theta), y = "Density")

ggplot(ability_df, aes(x = "Ability Estimates", y = Theta)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Ability Estimates", x = "", y = expression(theta))

