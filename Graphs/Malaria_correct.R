library(ggplot2)
library(dplyr)
library(tidyr)
library (lubridate)
folder_path <- "C:/Users/palla/Indiana University/Pabon-Rodriguez, Felix - Data-REDCap-Malaria"
# OR
folder_path <- "C:\\Users\\palla\\Indiana University\\Pabon-Rodriguez, Felix - Data-REDCap-Malaria"
# List all CSV files in the specified directory
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# Print file names with an index
cat(paste0(seq_along(csv_files), ": ", basename(csv_files), "\n"))
file_path1 <- csv_files[1]
file_path2 <- csv_files[2]
file_path3 <- csv_files[3]
file_path4 <- csv_files[4]
file_path5 <- csv_files[5]
file_path6 <- csv_files[6]
file_path7 <- csv_files[7]
file_path8 <- csv_files[8]


malaria.visits <- read.csv(file_path4)
passive <- read.csv(file_path6)
risk<- read.csv(file_path8)

# Getting the years from the visit dates
malaria.visits$s_dt <- as.Date(malaria.visits$s_dt)
malaria.visits$Visit.Year <- year(malaria.visits$s_dt)
malaria.visits$Visit.YearMonth <- format(malaria.visits$s_dt, "%Y-%m")

yrs.pf <- 2003:2020 
yrs.diag <- 2007:2020 
malaria.cases.diagnosed <- data.frame(matrix(NA,ncol = 5,nrow = length(yrs.diag)))
malaria.cases.finalpf <- data.frame(matrix(NA,ncol = 5,nrow = length(yrs.pf)))
colnames(malaria.cases.diagnosed) <- c("Year","No.Malaria", "Yes.Malaria", "NA", "Total")
colnames(malaria.cases.finalpf) <- c("Year","No.Malaria", "Yes.Malaria", "NA", "Total")


# Correct initialization of malaria.cases.diagnosed
malaria.cases.diagnosed <- data.frame(
  Year = 2007:2020,  # Replace with actual years if different
  No.Malaria = integer(14),  # Initialize with zeros
  Yes.Malaria = integer(14), # Initialize with zeros
  UnknownMalaria = integer(14), # Rename from 'NA' to 'UnknownMalaria' to avoid confusion
  Total = integer(14)  # Initialize with zeros
)


for(i in yrs.diag) {
  x <- malaria.visits[malaria.visits$Visit.Year == i, ]
  t1 <- table(x$c_mal_dx, useNA = "ifany")
  
  # Initialize with zeros for expected outcomes
  diagnosed_counts <- c(`No` = 0, `Yes` = 0, `Unknown` = 0)
  
  # Update counts based on table t1
  if ("0" %in% names(t1)) diagnosed_counts["No"] <- t1["0"]
  if ("1" %in% names(t1)) diagnosed_counts["Yes"] <- t1["1"]
  if (!is.null(t1["<NA>"])) diagnosed_counts["Unknown"] <- t1["<NA>"]
  
  # Construct new row
  new_row <- c(Year = i, 
               No.Malaria = diagnosed_counts["No"], 
               Yes.Malaria = diagnosed_counts["Yes"], 
               UnknownMalaria = diagnosed_counts["Unknown"],
               Total = sum(diagnosed_counts))
  
  # Insert new row into the dataframe
  malaria.cases.diagnosed[which(malaria.cases.diagnosed$Year == i), ] <- new_row
}


malaria.cases.diagnosed

for(i in yrs.pf) {
  x <- malaria.visits[malaria.visits$Visit.Year == i,]
  t2 <- table(x$m1_final_pf, useNA = "always")  # Use the correct column name
  
  # Initialize default counts for '0' and '1', and 'NA' for missing data
  pf_counts <- c('0' = 0, '1' = 0, 'NA' = 0)
  
  # Update counts based on the table t2
  if (!is.null(names(t2))) {
    pf_counts['0'] <- ifelse('0' %in% names(t2), t2['0'], 0)
    pf_counts['1'] <- ifelse('1' %in% names(t2), t2['1'], 0)
    pf_counts['NA'] <- ifelse('NA' %in% names(t2), t2['NA'], 0)  # For NA counts
  }
  
  # Construct new row: use backticks or quotes for 'NA' to differentiate it from the NA value
  new_row <- c(Year = i, 
               `No.Malaria` = pf_counts['0'], 
               `Yes.Malaria` = pf_counts['1'], 
               `NA` = pf_counts['NA'], 
               Total = sum(pf_counts['0'], pf_counts['1'], pf_counts['NA']))
  
  # Insert new row into the dataframe
  malaria.cases.finalpf[which(yrs.pf == i), ] <- new_row
}
malaria.cases.finalpf

saveRDS(malaria.cases.finalpf, "malaria.cases.finalpf.rds")
saveRDS(malaria.cases.diagnosed, "malaria.cases.diagnosed.rds")



p <- ggplot() + 
  geom_line(data = malaria.cases.diagnosed, aes(x = Year, y = Yes.Malaria, color = "Diagnosed"), size = 1) +
  geom_point(data = malaria.cases.diagnosed, aes(x = Year, y = Yes.Malaria, color = "Diagnosed")) +
  geom_text(data = malaria.cases.diagnosed, aes(x = Year, y = Yes.Malaria, label = Yes.Malaria), vjust = -1, size = 3, color = "black") +
  geom_line(data = malaria.cases.finalpf, aes(x = Year, y = Yes.Malaria, color = "Final.Pf"), size = 1) +
  geom_point(data = malaria.cases.finalpf, aes(x = Year, y = Yes.Malaria, color = "Final.Pf")) +
  geom_text(data = malaria.cases.finalpf, aes(x = Year, y = Yes.Malaria, label = Yes.Malaria), vjust = 2, size = 3, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_continuous("Year", breaks = seq(min(malaria.cases.finalpf$Year, na.rm = TRUE), max(malaria.cases.finalpf$Year, na.rm = TRUE), by = 1)) +
  ylab('Confirmed Malaria Cases') +
  scale_color_manual(name = "Variable Used", values = c("Diagnosed" = "blue", "Final.Pf" = "red"), labels = c("Diagnosed", "Final.Pf")) +
  ggtitle("Annual Number of Confirmed Malaria Cases by Variable Used")

print(p)
