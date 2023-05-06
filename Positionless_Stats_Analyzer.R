library(dplyr)
library(ggplot2)

# Set the path to the directory containing the data files
data_dir <- "/Users/atijmahesh/Desktop/BSASpring2023"

# Define a function to read the data and return it as a result
read_data <- function(year, data_type) {
  # Construct the path to the data file
  file_path <- file.path(data_dir, as.character(year), paste0(year, "_", data_type, "_data.csv"))
  
  # Read the data from the file into a data frame
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Return the data frame
  return(data)
}

# Initialize an empty list to store the means for each year
yearly_means <- list()

# Loop over each year
for (year in 2005:2022) {
  # Read in the receiving and rushing data for the current year
  receiving_data <- read_data(year, "receiving")
  rushing_data <- read_data(year, "rushing")
  
  # Select columns for receiving data
  receiving_cols <- c("Player", "Pos", "Rec", "Yds", "TD")
  receiving_data <- receiving_data[, receiving_cols]
  
  # Filter out rows where "Rec" is less than 5 or "Player" is "Player"
  receiving_data <- receiving_data %>% 
    filter(Rec >= 1, Player != "Player", Pos != "WR", Pos != "")
  
  # Select columns for rushing data
  rushing_data <- rushing_data[-1, ]
  rushing_cols <- c("Player", "Pos", "Att", "Yds", "TD")
  rushing_data <- rushing_data[, rushing_cols]
  
  # Filter out rows where "Att" is less than 3 or "Player" is "Player"
  rushing_data <- rushing_data %>% 
    filter(Att >= 3, Player != "Player", Pos != "RB", Pos != "")
  
  # Calculate the means for each column of receiving data
  receiving_data[, c("Rec", "Yds", "TD")] <- lapply(receiving_data[, c("Rec", "Yds", "TD")], as.numeric)
  receiving_means <- colMeans(receiving_data[, c("Rec", "Yds", "TD")], na.rm = TRUE)
  names(receiving_means) <- c("Rec_mean", "Yds_mean", "TD_mean")
  
  # Calculate the means for each column of rushing data
  rushing_data[, c("Att", "Yds", "TD")] <- lapply(rushing_data[, c("Att", "Yds", "TD")], as.numeric)
  rushing_means <- colMeans(rushing_data[, c("Att", "Yds", "TD")], na.rm = TRUE)
  names(rushing_means) <- c("Att_mean", "Yds_mean", "TD_mean")
  
  # Store the means for the current year in a list
  yearly_means[[year]] <- list(receiving_means, rushing_means)
}
# Plot the yearly means for rushing data
# Create an empty list to store the yearly rushing data
rushing_data <- list()

# Loop over all years in yearly_means
for (i in seq_along(yearly_means)) {
  # Extract the rushing data for the current year and store it in the rushing_data list
  rushing_data[[i]] <- yearly_means[[i]][["rushing"]]
}

# Initialize empty vectors to store the total stats for each category
total_rushing_attempts <- numeric()
total_rushing_yds <- numeric()
total_rushing_tds <- numeric()

# Loop over all years in yearly_means
for (i in seq_along(yearly_means)) {
  # Extract the rushing data for the current year and store it in the rushing_data list
  rushing_data[[i]] <- yearly_means[[i]][["rushing"]]
  
  # Add the current year's rushing stats to the total vectors
  total_rushing_attempts <- c(total_rushing_attempts, rushing_data[[i]]$Att_mean)
  total_rushing_yds <- c(total_rushing_yds, rushing_data[[i]]$Yds_mean)
  total_rushing_tds <- c(total_rushing_tds, rushing_data[[i]]$TD_mean)
}

# Set up the plot with the correct axis labels
plot(2005:2022, total_rushing_attempts, type = "l", ylim = c(0, max(total_rushing_attempts)),
     xlab = "Year", ylab = "Total Rushing Attempts")

# Add lines to the plot for the total yards and touchdowns
lines(2005:2022, total_rushing_yds, col = "blue")
lines(2005:2022, total_rushing_tds, col = "red")

# Add a legend to the plot
legend("topleft", legend = c("Attempts", "Yards", "Touchdowns"), col = c("black", "blue", "red"), lty = 1)

# Set up an empty plot with the correct axis labels
plot(0, ylim = c(0, 100), xlim = c(2005, 2022), xlab = "Year", ylab = "Attempts")

# Loop over each year's data
for (year in 2005:2022) {
  # Extract the number of attempts from the "rushing" sublist
  print(yearly_means[[year]][[2]][2])
  attempts <- yearly_means[[year]][[2]][3]
  
  # Add a point to the plot at the current year and number of attempts
  points(year, attempts)
}

