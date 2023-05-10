library(dplyr)
library(ggplot2)
library(tidyr)

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

# Initialize an empty list to store the sums for each year
yearly_sums <- list()

# Loop over each year
for (year in 1994:2022) {
  # Read in the receiving and rushing data for the current year
  receiving_data_original <- read_data(year, "receiving")
  rushing_data_original <- read_data(year, "rushing")
  
  # Select columns for receiving data
  receiving_cols <- c("Player", "Pos", "Rec", "Yds", "TD")
  receiving_data <- receiving_data_original[, receiving_cols]
  
  # Filter out rows where "Rec" is less than 5 or "Player" is "Player"
  receiving_data <- receiving_data_original %>% 
    filter(Rec >= 1, Player != "Player", Pos != "WR", Pos != "")
  
  # Select columns for rushing data
  rushing_cols <- c("Player", "Pos", "Att", "Yds", "TD")
  rushing_data <- rushing_data_original[, rushing_cols]
  
  # Filter out rows where "Att" is less than 3 or "Player" is "Player"
  rushing_data <- rushing_data_original %>% 
    filter(Att >= 1, Player != "Player", Pos != "RB", Pos != "")
  
  wr_receiving_data <- receiving_data_original %>%
    filter(Rec >= 1, Player != "Player", Pos == "WR")
  
  rb_rushing_data <- rushing_data_original %>%
    filter(Att >= 1, Player != "Player", Pos == "RB")
  
  fb_rushing_data <- rushing_data_original %>%
    filter(Att >= 1, Player != "Player", Pos == "FB")
  
  wr_rushing_data <- rushing_data_original %>%
    filter(Att >= 1, Player != "Player", Pos == "WR")
  
  
  # Calculate the sums for each column of receiving data
  receiving_data[, c("Rec", "Yds", "TD")] <- lapply(receiving_data[, c("Rec", "Yds", "TD")], as.numeric)
  receiving_sums <- colSums(receiving_data[, c("Rec", "Yds", "TD")], na.rm = TRUE)
  names(receiving_sums) <- c("Non-Receiver Receptions", "Non-Receiver Yards", "Non-Receiver Touchdowns")
  
  # Calculate the sums for each column of rushing data
  rushing_data[, c("Att", "Yds", "TD")] <- lapply(rushing_data[, c("Att", "Yds", "TD")], as.numeric)
  rushing_sums <- colSums(rushing_data[, c("Att", "Yds", "TD")], na.rm = TRUE)
  names(rushing_sums) <- c("Non-Running Back Attempts", "Non-Running Back Yards", "Non-Running Back Touchdowns")
  
  # Calculate the sums for each column of wr receiving data
  wr_receiving_data[, c("Rec", "Yds", "TD")] <- lapply(wr_receiving_data[, c("Rec", "Yds", "TD")], as.numeric)
  wr_receiving_sums <- colSums(wr_receiving_data[, c("Rec", "Yds", "TD")], na.rm = TRUE)
  names(wr_receiving_sums) <- c("WR Receptions", "WR Yards", "WR Touchdowns")
  
  # Calculate the sums for each column of rushing data
  rb_rushing_data[, c("Att", "Yds", "TD")] <- lapply(rb_rushing_data[, c("Att", "Yds", "TD")], as.numeric)
  rb_rushing_sums <- colSums(rb_rushing_data[, c("Att", "Yds", "TD")], na.rm = TRUE)
  names(rb_rushing_sums) <- c("RB Attempts", "RB Yards", "RB Touchdowns")
  
  # Calculate the sums for each column of rushing data
  fb_rushing_data[, c("Att", "Yds", "TD")] <- lapply(fb_rushing_data[, c("Att", "Yds", "TD")], as.numeric)
  fb_rushing_sums <- colSums(fb_rushing_data[, c("Att", "Yds", "TD")], na.rm = TRUE)
  names(fb_rushing_sums) <- c("FB Attempts", "FB Yards", "FB Touchdowns")
  
  # Calculate the sums for each column of rushing data
  wr_rushing_data[, c("Att", "Yds", "TD")] <- lapply(wr_rushing_data[, c("Att", "Yds", "TD")], as.numeric)
  wr_rushing_sums <- colSums(wr_rushing_data[, c("Att", "Yds", "TD")], na.rm = TRUE)
  names(wr_rushing_sums) <- c("WR Attempts", "WR Yards", "WR Touchdowns")
  print(year)
  print(rushing_sums)
  print(rb_rushing_sums)
  print(fb_rushing_sums)
  print(wr_rushing_sums)
  # Store the sums for the current year in a list
  yearly_sums[[year]] <- list(receiving_sums, rushing_sums, wr_receiving_sums, rb_rushing_sums, fb_rushing_sums, wr_rushing_sums)
}

# Create an empty data frame to store the data
yearly_rec_yds <- data.frame()
yearly_rec <- data.frame()
yearly_rec_tds <- data.frame()
yearly_att <- data.frame()
yearly_rush_yds <- data.frame()
yearly_rush_tds <- data.frame()
yearly_wr_yds <- data.frame()
yearly_wr_rec <- data.frame()
yearly_wr_tds <- data.frame()
yearly_rb_att <- data.frame()
yearly_rb_yds <- data.frame()
yearly_rb_tds <- data.frame()
yearly_fb_att <- data.frame()
yearly_fb_yds <- data.frame()
yearly_fb_tds <- data.frame()
yearly_wr_att <- data.frame()
yearly_wr_rush_yds <- data.frame()
yearly_wr_rush_tds <- data.frame()

# Loop over each year
for (year in 1994:2022) {
  # Get the sublist for the current year
  nonwr_year_sums <- yearly_sums[[year]][[1]]
  yearly_rec <- rbind(yearly_rec, data.frame(year = year, rec = nonwr_year_sums[1]))
  yearly_rec_yds <- rbind(yearly_rec_yds, data.frame(year = year, rec_yards = nonwr_year_sums[2]))
  yearly_rec_tds <- rbind(yearly_rec_tds, data.frame(year = year, rec_tds = nonwr_year_sums[3]))
  
  nonrb_year_sums <- yearly_sums[[year]][[2]]
  yearly_att <- rbind(yearly_att, data.frame(year = year, att = nonrb_year_sums[1]))
  yearly_rush_yds <- rbind(yearly_rush_yds, data.frame(year = year, rush_yards = nonrb_year_sums[2]))
  yearly_rush_tds <- rbind(yearly_rush_tds, data.frame(year = year, rush_tds = nonrb_year_sums[3]))
  
  wr_year_sums <- yearly_sums[[year]][[3]]
  yearly_wr_rec <- rbind(yearly_wr_rec, data.frame(year = year, rec = wr_year_sums[1]))
  yearly_wr_yds <- rbind(yearly_wr_yds, data.frame(year = year, rec_yards = wr_year_sums[2]))
  yearly_wr_tds <- rbind(yearly_wr_tds, data.frame(year = year, rec_tds = wr_year_sums[3]))
  
  rb_year_sums <- yearly_sums[[year]][[4]]
  yearly_rb_att <- rbind(yearly_rb_att, data.frame(year = year, att = rb_year_sums[1]))
  yearly_rb_yds <- rbind(yearly_rb_yds, data.frame(year = year, rush_yards = rb_year_sums[2]))
  yearly_rb_tds <- rbind(yearly_rb_tds, data.frame(year = year, rush_tds = rb_year_sums[3]))
  
  fb_year_sums <- yearly_sums[[year]][[5]]
  yearly_fb_att <- rbind(yearly_fb_att, data.frame(year = year, att = fb_year_sums[1]))
  yearly_fb_yds <- rbind(yearly_fb_yds, data.frame(year = year, rush_yards = fb_year_sums[2]))
  yearly_fb_tds <- rbind(yearly_fb_tds, data.frame(year = year, rush_tds = fb_year_sums[3]))
  
  wr_rush_year_sums <- yearly_sums[[year]][[6]]
  yearly_wr_att <- rbind(yearly_wr_att, data.frame(year = year, att = wr_rush_year_sums[1]))
  yearly_wr_rush_yds <- rbind(yearly_wr_rush_yds, data.frame(year = year, rush_yards = wr_rush_year_sums[2]))
  yearly_wr_rush_tds <- rbind(yearly_wr_rush_tds, data.frame(year = year, rush_tds = wr_rush_year_sums[3]))
}


# set working directory to where you want to save the PDF files
setwd("/Users/atijmahesh/Desktop/BSASpring2023/Graphs_PDF")

# create a PDF file for each plot
pdf("yearly_rec.pdf")
plot(yearly_rec$year, yearly_rec$rec,
     main = "Non-WR Receptions per Year", xlab = "Year", ylab = "Receptions", type = "l")
dev.off()

pdf("yearly_rec_yds.pdf")
plot(yearly_rec_yds$year, yearly_rec_yds$rec_yards,
     main = "Non-WR Receiving Yards per Year", xlab = "Year", ylab = "Receiving Yards", type = "l")
dev.off()

pdf("yearly_rec_tds.pdf")
plot(yearly_rec_tds$year, yearly_rec_tds$rec_tds,
     main = "Non-WR Receiving TDs per Year", xlab = "Year", ylab = "Receiving TDs", type = "l")
dev.off()

# Create PDF with rushing attempts data
pdf("yearly_rush_attempts.pdf")
plot(yearly_att$year, yearly_att$att, type = "l", col = "blue",
     main = "Non-RB Rushing Attempts per Year", xlab = "Year", ylab = "Rushing Attempts",
     ylim = c(0, max(yearly_att$att, yearly_fb_att$att, yearly_wr_att$att))) # Set the y-axis limits accordingly
lines(yearly_fb_att$year, yearly_fb_att$att, col = "red")
lines(yearly_wr_att$year, yearly_wr_att$att, col = "green")
legend("topright", legend = c("Non-RB", "Fullback", "Wide Receiver"), col = c("blue", "red", "green"), lty = 1)
dev.off()

# Create PDF with rushing yards data
pdf("yearly_rush_yards.pdf")
plot(yearly_rush_yds$year, yearly_rush_yds$rush_yards, type = "l", col = "blue",
     main = "Non-RB Rushing Yards per Year", xlab = "Year", ylab = "Rushing Yards",
     ylim = c(0, max(yearly_rush_yds$rush_yards, yearly_fb_yds$rush_yards, yearly_wr_rush_yds$rush_yards))) # Set the y-axis limits accordingly
lines(yearly_fb_yds$year, yearly_fb_yds$rush_yards, col = "red")
lines(yearly_wr_rush_yds$year, yearly_wr_rush_yds$rush_yards, col = "green")
legend("topright", legend = c("Non-RB", "Fullback", "Wide Receiver"), col = c("blue", "red", "green"), lty = 1)
dev.off()

# Create PDF with rushing touchdowns data
pdf("yearly_rush_tds.pdf")
plot(yearly_rush_tds$year, yearly_rush_tds$rush_tds, type = "l", col = "blue",
     main = "Non-RB Rushing Touchdowns per Year", xlab = "Year", ylab = "Rushing Touchdowns",
     ylim = c(0, max(yearly_rush_tds$rush_tds, yearly_fb_tds$rush_tds, yearly_wr_rush_tds$rush_tds))) # Set the y-axis limits accordingly
lines(yearly_fb_tds$year, yearly_fb_tds$rush_tds, col = "red")
lines(yearly_wr_rush_tds$year, yearly_wr_rush_tds$rush_tds, col = "green")
legend("topright", legend = c("Non-RB", "Fullback", "Wide Receiver"), col = c("blue", "red", "green"), lty = 1)
dev.off()


# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")

pdf("wr_rec_vs_nonwr_rec.pdf")
ggplot() +
  geom_col(data=yearly_wr_rec, aes(x=year, y=rec, fill="WRs")) +
  geom_col(data=yearly_rec, aes(x=year, y=rec, fill="Non-WRs")) +
  labs(title="Receptions by Position", fill="Position", y="Receptions") +
  scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
  scale_fill_manual(values = c("red", "blue"), name = "Position", labels = c("Non-WRs", "WRs")) +
  expand_limits(x = c(2005, 2022))
dev.off()

pdf("wr_yds_vs_nonwr_yds.pdf")
ggplot() +
  geom_col(data=yearly_wr_yds, aes(x=year, y=rec_yards, fill="WRs")) +
  geom_col(data=yearly_rec_yds, aes(x=year, y=rec_yards, fill="Non-WRs")) +
  labs(title="Receiving Yards by Position", fill="Position", y="Receiving Yards") +
  scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
  scale_fill_manual(values = c("red", "blue"), name = "Position", labels = c("Non-WRs", "WRs")) +
  expand_limits(x = c(2005, 2022))
dev.off()

pdf("wr_tds_vs_nonwr_tds.pdf")
ggplot() +
  geom_col(data=yearly_wr_tds, aes(x=year, y=rec_tds, fill="WRs")) +
  geom_col(data=yearly_rec_tds, aes(x=year, y=rec_tds, fill="Non-WRs")) +
  labs(title="Receiving Touchdowns by Position", fill="Position", y="Receiving Touchdowns") +
  scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
  scale_fill_manual(values = c("red", "blue"), name = "Position", labels = c("Non-WRs", "WRs")) +
  expand_limits(x = c(2005, 2022))
dev.off()

pdf("rb_att_vs_nonrb_att.pdf")
ggplot() +
  geom_col(data=yearly_rb_att, aes(x=year, y=att, fill="RBs")) +
  geom_col(data=yearly_att, aes(x=year, y=att, fill="Non-RBs")) +
  geom_col(data=yearly_fb_att, aes(x=year, y=att, fill="FBs")) +
  geom_col(data=yearly_wr_att, aes(x=year, y=att, fill="WRs")) +
  labs(title="Rushing Attempts by Position", fill="Position", y="Rushing Attempts") +
  scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
  scale_fill_manual(values = c("orange", "blue", "chartreuse", "magenta"), name = "Position", labels = c("FBs", "Non-RBs", "RBs", "WRs")) +
  expand_limits(x = c(2005, 2022))
dev.off()

pdf("rb_yds_vs_nonrb_yds.pdf")
ggplot() +
  geom_col(data=yearly_rb_yds, aes(x=year, y=rush_yards, fill="RBs")) +
  geom_col(data=yearly_rush_yds, aes(x=year, y=rush_yards, fill="Non-RBs")) +
  geom_col(data=yearly_fb_yds, aes(x=year, y=rush_yards, fill="FBs")) +
  geom_col(data=yearly_wr_rush_yds, aes(x=year, y=rush_yards, fill="WRs")) +
  labs(title="Rushing Yards by Position", fill="Position", y="Rushing Yards") +
  scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
  scale_fill_manual(values = c("orange", "blue", "chartreuse", "magenta"), name = "Position", labels = c("FBs", "Non-RBs", "RBs", "WRs")) +
  expand_limits(x = c(2005, 2022))
dev.off()


pdf("rb_tds_vs_nonrb_tds.pdf")
ggplot() +
  geom_col(data=yearly_rb_tds, aes(x=year, y=rush_tds, fill="RBs")) +
  geom_col(data=yearly_rush_tds, aes(x=year, y=rush_tds, fill="Non-RBs")) +
  geom_col(data=yearly_fb_tds, aes(x=year, y=rush_tds, fill="FBs")) +
  geom_col(data=yearly_wr_rush_tds, aes(x=year, y=rush_tds, fill="WRs")) +
  labs(title="Rushing Touchdowns by Position", fill="Position", y="Rushing Touchdowns") +
  scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
  scale_fill_manual(values = c("orange", "blue", "chartreuse", "magenta"), name = "Position", labels = c("FBs", "Non-RBs", "RBs", "WRs")) +
  expand_limits(x = c(2005, 2022))
dev.off()

