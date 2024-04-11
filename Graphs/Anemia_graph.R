#Preprocessing of Anemia dataset:
  
#Indivudual dataset- Household.ID, Sex
#Mapping- Household.ID (Record.ID), Household.elevation
#Risk- Record.ID, Household.ID, Age, Visit.Date
#Passive- Record.ID,  Hemoglobin, Repeat.Instance, Pregnancy status, Smear.Date


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

file_path1 <- csv_files[6]
file_path2 <- csv_files[8]
file_path3 <- csv_files[5]
file_path4 <- csv_files[4]
file_path5 <- csv_files[3]
risk_factors <- read.csv(file_path2)
Mapping_data<- read.csv(file_path3)
Individual_data<- read.csv(file_path5)
passive_data<- read.csv(file_path1)
Individual_selected <- Individual_data[, c("Record.ID","Current.household.ID", "Sex")]
Mapping_selected <- Mapping_data[, c("Record.ID", "Elevation..meteres.")]
Passive_selected <- passive_data[, c("Record.ID", "Hemoglobin..g.dL.", "Repeat.Instance", "If.female..what.is.this.person.s.pregnancy.status", "Smear.Date")]
Risk_selected <- risk_factors[, c("Record.ID", "Household.ID", "Age", "Visit.date")]

# Rename the column in the Mapping dataset for consistency
colnames(Mapping_selected)[colnames(Mapping_selected) == "Record.ID"] <- "Household.ID"
colnames(Mapping_selected)

# Merge the datasets based on 'Current.household.ID' from Individual_selected 
# and 'Household.ID' from Mapping_data
indiv_map<- merge(Individual_selected, Mapping_selected, 
                     by.x = "Current.household.ID", by.y = "Household.ID", all = TRUE)

#Since there were 2 blank values in enterede date column, so I removed it from the dataframe - indiv_map
# Remove rows where any value is NA, but specifically checking Entered.date here
indiv_map <- na.omit(indiv_map, cols = "Entered.date")

# Selecting specified columns from the Risk dataset
Risk_selected <- risk_factors[, c("Record.ID", "Household.ID", "Age", "Visit.date")]

# Merging indiv_map with Risk_selected
indiv_map_risk<- merge(indiv_map, Risk_selected, 
                           by.x = c("Record.ID", "Current.household.ID"), 
                           by.y = c("Record.ID", "Household.ID"), 
                           all = TRUE)


###

#To accomplish merge of indiv_map_risk and passive: Following data preparation required:
#Prepare the Data: Ensure that the Visit.date in indiv_map_risk and Smear.Date in Passive_selected are in the correct date format. Also, extract the year from these date columns to facilitate matching by year.

#Merge on Record.ID and Full Date: Perform an initial merge on Record.ID and the exact dates (Visit.date and Smear.Date).

#Merge on Record.ID and Year: For rows that do not have an exact date match, merge on Record.ID and the year extracted from the dates.

#Handling Multiple Matches: If there are multiple matches for the same Record.ID and year, you can decide to keep just one of them (for example, the first one).

###


# Convert to Date format
indiv_map_risk$Visit.date <- as.Date(indiv_map_risk$Visit.date)
Passive_selected$Smear.Date <- as.Date(Passive_selected$Smear.Date)

# Extract the year from the dates
indiv_map_risk$Year <- format(indiv_map_risk$Visit.date, "%Y")
Passive_selected$Year <- format(Passive_selected$Smear.Date, "%Y")

# Merge based on Record.ID and exact dates
merged_exact <- merge(indiv_map_risk, Passive_selected, by.x = c("Record.ID", "Visit.date"), by.y = c("Record.ID", "Smear.Date"), all = FALSE)

# Exclude already matched records
indiv_map_risk_unmatched <- indiv_map_risk[!indiv_map_risk$Record.ID %in% merged_exact$Record.ID, ]
Passive_selected_unmatched <- Passive_selected[!Passive_selected$Record.ID %in% merged_exact$Record.ID, ]

# Merge remaining records based on Record.ID and Year
merged_year <- merge(indiv_map_risk_unmatched, Passive_selected_unmatched, by.x = c("Record.ID", "Year"), by.y = c("Record.ID", "Year"), all = FALSE)

# Standardize 'Year' columns
merged_exact$Year <- merged_exact$Year.x  # Assuming Year.x is the correct year to align with merged_year

# Drop the unnecessary 'Year.x' and 'Year.y' from merged_exact
merged_exact <- subset(merged_exact, select = -c(Year.x, Year.y))

# Ensure 'Smear.Date' is added to merged_exact for consistency, even if it's filled with NA
if(!"Smear.Date" %in% names(merged_exact)) {
  merged_exact$Smear.Date <- NA
}

# Align the column order of merged_year to match merged_exact before combining
merged_year <- merged_year[names(merged_exact)]

# Combine the datasets
indiv_map_risk_passive <- rbind(merged_exact, merged_year)


# Sort final_merged_data by Record.ID
indiv_map_risk_passive <- indiv_map_risk_passive[order(indiv_map_risk_passive$Record.ID), ]
# Convert 'Smear.Date' from numeric back to Date format
indiv_map_risk_passive$Smear.Date <- as.Date(indiv_map_risk_passive$Smear.Date, origin = "1970-01-01")
indiv_map_risk_passive







# Adjust Hb values for altitude
indiv_map_risk_passive$Adjusted.Hb <- ifelse(indiv_map_risk_passive$Elevation..meteres. > 1900,
                                             indiv_map_risk_passive$`Hemoglobin..g.dL.` - 0.8, 
                                             indiv_map_risk_passive$`Hemoglobin..g.dL.`)


# Add a simplified anemia condition for demonstration
indiv_map_risk_passive$Anemia_Simple <- ifelse(indiv_map_risk_passive$Adjusted.Hb <= 11, 1, 0)


# Determine Anemia based on WHO guidelines and adjusted Hb
indiv_map_risk_passive$Anemia <- with(indiv_map_risk_passive, ifelse(
  (Age < 5 & Adjusted.Hb <= 11) |
    (Age >= 5 & Age < 12 & Adjusted.Hb <= 11.5) |
    (Age >= 12 & Age < 15 & Adjusted.Hb <= 12) |
    (Age >= 15 & Sex == "Female" & `If.female..what.is.this.person.s.pregnancy.status` != "Pregnant" & Adjusted.Hb <= 12) |
    (Age >= 15 & Sex == "Female" & `If.female..what.is.this.person.s.pregnancy.status` == "Pregnant" & Adjusted.Hb <= 11) |
    (Age >= 15 & Sex == "Male" & Adjusted.Hb <= 13),
  1, 0))

# Aggregate anemia cases by year

anemia_summary <- indiv_map_risk_passive %>% 
  group_by(Year) %>%
  summarise(No.Anemia = sum(Anemia == 0, na.rm = TRUE),
            Yes.Anemia = sum(Anemia == 1, na.rm = TRUE))

# Aggregate this new anemia case condition by year
anemia_summary_simple <- indiv_map_risk_passive %>% 
  group_by(Year) %>%
  summarise(No.Anemia_Simple = sum(Anemia_Simple == 0, na.rm = TRUE),
            Yes.Anemia_Simple = sum(Anemia_Simple == 1, na.rm = TRUE))

# Convert 'Year' back to numeric for plotting
anemia_summary$Year <- as.numeric(as.character(anemia_summary$Year))

# Merge the two summaries for comparison
anemia_summary_combined <- merge(anemia_summary, anemia_summary_simple, by = "Year")

# Save the anemia_summary_combined dataframe to an RDS file
saveRDS(anemia_summary_combined, "anemia_summary_combined.rds")



g <- ggplot() +
  geom_line(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia, color = "WHO Guidelines"), size = 1) +
  geom_point(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia, color = "WHO Guidelines")) +
  geom_text(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia, label = Yes.Anemia), vjust = -0.5, size = 3, color = "black") +
  geom_line(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia_Simple, color = "Hb <= 11"), size = 1) +
  geom_point(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia_Simple, color = "Hb <= 11")) +
  geom_text(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia_Simple, label = Yes.Anemia_Simple), vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_continuous("Year", breaks = seq(min(anemia_summary_combined$Year, na.rm = TRUE), max(anemia_summary_combined$Year, na.rm = TRUE), by = 1)) +
  ylab('Number of Anemia Cases') +
  scale_color_manual(name = "Definition Used", values = c("WHO Guidelines" = "darkgreen", "Hb <= 11" = "darkorange")) +
  ggtitle("Annual Number of Anemia Cases by Definition Used")

# Print the plot
print(g)
