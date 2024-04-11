
setwd("C:/Users/palla/Indiana University")

install.packages("leaflet")
library(tidyr)
library(dplyr)

library(ggplot2)
library(sf)
library(raster) 

folder_path <- "C:/Users/palla/Indiana University/Pabon-Rodriguez, Felix - Data-REDCap-Malaria"
# OR
folder_path <- "C:\\Users\\palla\\Indiana University\\Pabon-Rodriguez, Felix - Data-REDCap-Malaria"
# List all CSV files in the specified directory
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Print file names with an index
cat(paste0(seq_along(csv_files), ": ", basename(csv_files), "\n"))

file_path5 <- csv_files[5]
mapping_data<- read.csv(file_path5)
saveRDS(mapping_data, "mapping_data.rds")
risk_passive_map_df <- readRDS("risk_passive_map_df.rds")
risk_passive_data <- readRDS("risk_passive_data.rds")
risk_passive_mappings <- merge(risk_passive_data, mapping_data, by.x = "Household.ID", by.y = "Record.ID")
risk_passive_mappings
risk_passive_mappings <- risk_passive_mappings[, c("Household.ID", "Year", "Latitude..N.", "Longitude..E.", "X.PF", "Hemoglobin..g.dL.")]
risk_passive_mappings
risk_passive_mappings <- risk_passive_mappings %>%
  mutate(X.PF = case_when(
    X.PF == "Positive (+)" ~ 1,
    X.PF == "Negative (-)" ~ 0,
    TRUE ~ as.integer(NA)  # This will ensure that NA values stay as NA (and are not converted to strings)
  )) %>%
  drop_na(X.PF)

risk_passive_mappings

# Assuming the common key is Household.ID
malaria_hemoglobin<- merge(risk_passive_map_df, risk_passive_mappings, by = "Household.ID", all = TRUE)
malaria_hemoglobin
colnames(malaria_hemoglobin)

# Selecting columns using dplyr's select, explicitly specifying the package
malaria_selected <- dplyr::select(malaria_hemoglobin, Household.ID, Year.x, Anemia, Year.y, Latitude..N., Longitude..E., X.PF)
malaria_selected
saveRDS(malaria_selected, "malaria_selected.rds")


#Creating both column:
# Assuming malaria_selected has been properly loaded and dplyr is available
malaria_selected <- malaria_selected %>%
  mutate(BOTH = ifelse(X.PF == 1 & Anemia == 1, 1, 0))
malaria_selected


# Define the years of interest based on your dataset
years_of_interest <- 2002:2022  # Adjust based on your actual data years

# Load the Kenya boundary as an 'sf' object
kenya_map <- st_as_sf(getData('GADM', country = 'KE', level = 1, download = TRUE), crs = 4326)

saveRDS(kenya_map, "kenya_map.rds")


# Loop through each year and create a map
for(year in years_of_interest) {
  # Filter the dataset for the given year and positive malaria cases
  data_for_year <- dplyr::filter(malaria_selected, Year.x == year, X.PF == 1)
  
  # Summarize the data to count cases at each location
  location_counts <- data_for_year %>%
    dplyr::group_by(Latitude..N., Longitude..E.) %>%
    dplyr::summarise(Count = n(), .groups = 'drop')
  
  # Skip years with no data
  if(nrow(location_counts) == 0) next
  file_name <- paste0("location_counts_", year, ".rds")  # Create a file name based on the year
  saveRDS(location_counts, file_name)
}
  
  
  
  # Plotting
  ggplot_object <- ggplot() +
    geom_sf(data = kenya_map, fill = "lightgrey", color = "black") +
    geom_point(data = location_counts, aes(x = Longitude..E., y = Latitude..N., size = Count, color = Count),
               alpha = 0.6) +
    scale_size(range = c(2, 10)) +  # Adjust size range based on your data
    scale_color_viridis_c(option = "magma", begin = 0.3, end = 0.9, direction = 1) +  # Change color scale as needed
    labs(title = paste("Malaria Case Density in Kenya - Year", year),
         size = "Case Count",
         color = "Case Count") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Display the plot
  print(ggplot_object)
  
  


year <- 2005  # Example year
data_for_year <- filter(malaria_selected, Year.x == year, X.PF == 1)

location_counts <- data_for_year %>%
  group_by(Latitude..N., Longitude..E.) %>%
  summarise(Count = n(), .groups = 'drop')

if(nrow(location_counts) > 0) {
  ggplot_object <- ggplot() +
    geom_sf(data = kenya_map, fill = "lightgrey", color = "black") +
    geom_point(data = location_counts, aes(x = Longitude..E., y = Latitude..N., size = Count, color = Count),
               alpha = 0.6) +
    scale_size(range = c(2, 10)) +
    scale_color_viridis_c(option = "magma", begin = 0.3, end = 0.9, direction = 1) +
    labs(title = paste("Malaria Case Density in Kenya", year),
         subtitle = "Visualization by Latitude and Longitude",
         x = "Longitude",
         y = "Latitude",
         size = "Case Count",
         color = "Case Count") +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(ggplot_object)
}


# Assuming necessary libraries are loaded
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)

# Your existing code to create ggplot_object
year <- 2005  # Example year
# Assuming malaria_selected and kenya_map are already loaded and prepped
data_for_year <- filter(malaria_selected, Year.x == year, X.PF == 1)

location_counts <- data_for_year %>%
  group_by(Latitude..N., Longitude..E.) %>%
  summarise(Count = n(), .groups = 'drop')

if(nrow(location_counts) > 0) {
  # Dynamically determine the bounds based on your data
  min_longitude <- min(location_counts$Longitude..E.) - 0.1  # Adjust buffer as needed
  max_longitude <- max(location_counts$Longitude..E.) + 0.1  # Adjust buffer as needed
  min_latitude <- min(location_counts$Latitude..N.) - 0.1  # Adjust buffer as needed
  max_latitude <- max(location_counts$Latitude..N.) + 0.1  # Adjust buffer as needed
  
  ggplot_object <- ggplot() +
    geom_sf(data = kenya_map, fill = "lightgrey", color = "black") +
    geom_point(data = location_counts, aes(x = Longitude..E., y = Latitude..N., size = Count, color = Count),
               alpha = 0.6) +
    scale_size(range = c(2, 10)) +
    scale_color_viridis_c(option = "magma", begin = 0.3, end = 0.9, direction = 1) +
    labs(title = paste("Malaria Case Density in Kenya", year),
         subtitle = "Visualization by Latitude and Longitude",
         x = "Longitude",
         y = "Latitude",
         size = "Case Count",
         color = "Case Count") +
    theme_minimal() +
    theme(legend.position = "right") +
    coord_fixed(xlim = c(min_longitude, max_longitude), ylim = c(min_latitude, max_latitude))  # Set initial zoom area
}

# Convert ggplot object to Plotly
plotly_object <- ggplotly(ggplot_object)

# Print the Plotly object to view it
plotly_object





