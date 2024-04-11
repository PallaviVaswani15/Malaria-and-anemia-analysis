

#setwd("C:/Users/palla/Indiana University")
library(plotly)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rmarkdown)
library(leaflet)
library(scales)
library(kableExtra)
library(DT)


combined_counts <- readRDS("combined_counts1.rds")
anemia_cases2 <- readRDS("anemia_cases2.rds")
anemia_cases <- readRDS("anemia_cases.rds")
risk_factors_long_new <- readRDS("risk_factors_long_new.rds")
bednet_long <- readRDS("bednet_long.rds")
travel_long <- readRDS("travel_long.rds")
symptom_counts <- readRDS("symptom_counts.rds")
malaria_treatment_long <- readRDS("malaria_treatment_long.rds")
malaria_selected <- readRDS("malaria_selected.rds")
kenya_map <- readRDS("kenya_map.rds")
anemia_summary_combined <-readRDS("anemia_summary_combined.rds")
malaria_selected<- readRDS("malaria_selected.rds")
kenya_map<-readRDS("kenya_map.rds")
malaria.cases.finalpf<-readRDS("malaria.cases.finalpf.rds")
malaria.cases.diagnosed<-readRDS("malaria.cases.diagnosed.rds")
Both_data <- readRDS("Both_data.rds")
summary_stats<-readRDS("summary_stats.rds")
boxplot<-readRDS("boxplot.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Malaria and Anemia Trends in Kenya: Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-line")),
      menuItem("Spatio-temporal Maps", tabName = "maps", icon = icon("globe")),
      menuItem("Statistical Analysis", tabName = "analysis", icon = icon("area-chart")),
      menuItem("About Us", tabName = "aboutus", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Style adjustments for the header */
        .main-header .logo {
          background-color: #fff; /* White background */
          color: #333; /* Dark text */
          font-weight: bold;
          border-bottom: 3px solid #00c0ef; /* Blue accent border */
        }
        
        /* Style adjustments for the sidebar */
        .main-sidebar {
          background-color: #f9f9f9; /* Light grey background */
        }

        .sidebar-menu > li > a {
          color: #333; /* Dark text for menu items */
        }

        .sidebar-menu > li.active > a {
          background-color: #00c0ef; /* Blue background for active menu item */
          color: #fff; /* White text for active menu item */
        }
        
        /* Sidebar menu item hover effect */
        .sidebar-menu li:not(.active) > a:hover {
          background-color: #00c0ef; /* Blue background for hover */
          color: #fff; /* White text for hover */
        }
        
        /* Body and content styling */
        .content-wrapper {
          background-color: #fff; /* White background for content */
          min-height: 100vh; /* Full height */
        }

        .content-header > h1 {
          color: #333; /* Dark text for content headers */
        }

        /* Custom styles for the body of tabs */
        .tab-content {
          padding: 20px;
          background-color: #f4f4f4; /* Slightly grey background for tab content */
          border: 1px solid #ddd; /* Light border for tab content */
          border-radius: 5px; /* Rounded corners for tab content */
          background-image: url('your_image_path_here'); /* Background image for the tab content */
          background-size: cover; /* Ensure the image covers the entire content area */
          background-position: center center; /* Center the background image */
        }
        
        /* Overriding Bootstrap defaults to mimic the ShinyStan interface */
        .content {
          background: rgba(255, 255, 255, 0.8); /* Semi-transparent white background */
          border-radius: 8px; /* Rounded corners for the content boxes */
          padding: 20px; /* Padding inside the content boxes */
          margin-top: 20px; /* Space above the content boxes */
        }

        h2 {
          color: #333; /* Dark color for main headings */
          margin-bottom: 15px; /* Space below headings */
        }

        p {
          font-size: 16px; /* Slightly larger font size for paragraphs */
        }

        /* Reduce margin and padding around the image */
        img {
          display: block;
          margin-top: 0;
          margin-bottom: 0;
        }
        
        /* Remove padding above h2 */
        .content-header > h2 {
          margin-top: 0;
        }
      ")),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
      
      
    ),
    tabItems(
      tabItem(tabName = "home",
              div(style = "text-align: center;",
                  imageOutput("homeImage"),
                  h2("Welcome to the Malaria and Anemia Trends Dashboard"),
                  p(HTML("Malaria, a life-threatening disease caused by <i>Plasmodium parasites</i>, is predominantly transmitted through the bites of females infected <i>Anopheles</i> mosquitoes. It remains a significant public health challenge, particularly in African regions, including Kenya. According to the World Health Organization (WHO), there were an estimated 229 million cases of malaria worldwide in 2019, with the African region accounting for about 94% of cases and deaths. In Kenya, malaria is a leading cause of morbidity and mortality, with varying transmission patterns across the country. The disease's impact extends beyond health, affecting socio-economic development due to its burden on healthcare systems and communities. Anemia, often associated with malaria, further exacerbates the health crisis. The WHO defines anemia as a condition in which the number of red blood cells is insufficient to meet physiological needs, varying by age, sex, altitude, smoking, and pregnancy status. This study aims to analyze the spatio-temporal patterns of malaria and anemia in Kenya, highlighting their interrelationship and the crucial role of health informatics and statistical analysis in understanding and addressing these public health challenges."))
              ),
              fluidRow(
                box(
                  title = "EPIDEMIOLOGICAL PROFILE",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::dataTableOutput("epidemiologicalProfileTable"),
                  width = 12,
                  footer = tags$p(
                    HTML("Source: "),
                    a(href = "https://apps.who.int/malaria/maps/threats/#/dashboards?theme=prevention&country=KE",
                      "WHO Malaria Threats Map",
                      target = "_blank")
                  )
                )
              )
      ),
      tabItem(tabName = "overview",
              h2("Overview of Malaria and Anemia"),
              p("This section provides a general overview of the situation, including key findings and insights from the data analysis."),
              fluidRow(
                box(
                  title = "Understanding Malaria and Anemia in Kenya",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  icon = icon("search"),
                  p("Dive into the intertwined stories of malaria and anemia over the last twenty years within Kenya. Our dashboard provides you with a spatio-temporal analysis that sheds light on the patterns, trends, and interventions shaping the health landscape."),
                  p("Understand how various socio-economic and environmental factors contribute to the prevalence of these conditions and explore the effectiveness of prevention strategies through interactive and comprehensive data visualizations.")
                )
              ),
              fluidRow(
                box(
                  title = "Trends from 2002 to 2022",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  icon = icon("search"),
                  p("Examine the historical data outlining the prevalence and symptoms of malaria. Observe how bednet usage, travel history, and previous treatments have evolved through time and influenced the trends in malaria and anemia cases.")
                ),
                box(
                  title = "Geographical Insights",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  icon = icon("search"),
                  p("Explore interactive maps displaying the density and number of cases across Kenya. Utilize the latitude and longitude data to pinpoint areas of high prevalence and observe how these figures have changed annually.")
                )
              ),
              fluidRow(
                box(
                  title = "Statistical Analysis and Bayesian Models",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  icon = icon("search"),
                  p("Delve into a detailed analysis of how age, previous malaria instances, proximity to water sources, bednet usage, and other factors correlate with malaria occurrences. Our use of Bayesian statistical models provides a sophisticated approach to understanding these relationships."),
                  p("This analytical framework allows us to incorporate prior knowledge and update our understanding as new data emerges, offering a robust and dynamic view of malaria's impact.")
                )
              ),
              fluidRow(
                box(
                  title = "About This Dashboard",
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  icon = icon("search"),
                  p("Get to know the collaborative effort behind this dashboard. Learn about the Principal Investigator, the project committee, and the ethical guidelines that shaped our approach to creating a platform that informs and empowers."),
                  p("This section serves as an acknowledgment of the hard work and dedication of the team committed to bringing health informatics to the forefront of public health strategy in Kenya.")
                )
              )
              
      ),
      tabItem(tabName = "eda",
              h2("Exploratory Data Analysis"),
              selectInput("correlationCategory", "Select Category:", 
                          choices = c("Symptoms","Bednet Usage", "Previously Treated for Malaria", "Travel History")),
              plotOutput("edaPlot"),
              
      ),
      tabItem(tabName = "maps",
              h2("Longitudinal Plots"),
              p("Before we look at the geographical distribution, let us explore the trajectories over time."),
              selectInput("Disease", "Select Disease:",
                          choices = c("Malaria", "Anemia")),
              plotOutput("longplots"), # anemia and malaria cases
              h2("Geographical Distribution"),
              p("Maps will be displayed here to show the geographical distribution of Malaria and Anemia cases."),
              h3("Select Year for Malaria Case Density Maps"),
              selectInput("selectedYear", "Select Year:", 
                          choices = 2002:2022),  # Ensure 'years_of_interest' is defined globally
              
              leafletOutput("malariaMap"),  # Map for Malaria case density
              
              h3("Number of both Malaria and Anemia Cases"),
              p("Table showing the annual number of Malaria and Anemia cases across Kenya from 2002:2022"),
              DT::dataTableOutput("casesSummaryTable")
      ),
      tabItem(tabName = "analysis",
              h2("Statistical Analysis"),
              div(class = "analysis-section",
                  # Introduction and Methodology sections
                  fluidRow(
                    box(title = "Introduction", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                        p("Welcome to the Statistical Analysis segment of our dashboard!"),
                        p("This section delves deep into the data surrounding malaria and anemia in Kenya, covering the period from 2002 to 2022. We have used a comprehensive dataset that includes variables such as disease incidence, demographic information, environmental factors, and preventive measures taken."),
                        
                        
                        p("Our analysis is powered by the Bayesian statistical framework, facilitated by the brms package in R (Bürkner, 2017). Bayesian analysis allows us to incorporate prior knowledge into our statistical models, offering a more nuanced view of our data compared to traditional methods. This approach is particularly beneficial in public health studies where past research and expert knowledge are invaluable."),
                        
                    ),
                    box(title = "Methodology", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 12,
                        p("Our study leverages sophisticated statistical techniques to unravel the intricate associations present within our dataset. At its core, we employ Generalized Linear Mixed Models (GLMMs), a type of model adept at handling both fixed effects—variables like age, gender, and health behaviors (e.g., use of bed nets)—and random effects, which account for inherent differences across diverse groups or settings (e.g., variations among households). This dual approach enables a nuanced analysis of how individual and collective factors influence health outcomes."),
                        p("Taking a step further, we adopt a Bayesian framework for our analysis. Unlike traditional statistical methods that rely on direct calculation of probabilities, Bayesian statistics treat all unknown parameters as probabilities. This perspective allows us to incorporate prior knowledge and update our beliefs in light of new evidence, yielding a more dynamic and flexible analysis. The Bayesian method shines in public health research by offering a robust mechanism for uncertainty quantification, enhancing the reliability of decision-making processes based on statistical findings (McElreath, 2016)."),
                        p("To implement our Bayesian GLMMs, we utilize the `brms` package in R, designed for advanced Bayesian modeling. This package enables us to specify complex models in a relatively straightforward manner, facilitating the estimation of our model's parameters via Markov Chain Monte Carlo (MCMC) sampling. The beauty of `brms` lies in its ability to handle a wide array of statistical distributions and link functions, making it a versatile tool for various research scenarios (Bürkner, 2017)."),
                        p("Our analysis workflow includes generating summary statistics and visualizations to interpret the model's findings effectively. Tools like `kableExtra` aid in crafting visually appealing tables for our results, while `ggplot2` is instrumental in producing clear and informative data visualizations. These packages collectively enhance the presentation and comprehension of our statistical analysis, bridging the gap between complex models and actionable insights (Zhu, 2021; Wickham, 2016)."),
                        
                    )
                  ),
                  
                  # Understanding Our Statistical Model and Key Components sections
                  fluidRow(
                    box(title = "Understanding Our Statistical Model", status = "info", solidHeader = TRUE, width = 12,
                        p("In our study, we have constructed a statistical model to analyze factors affecting the prevalence of malaria, represented by the presence of the Plasmodium falciparum parasite. This model helps us understand how various factors like age, bednet usage, travel history, and environmental conditions contribute to malaria risk among individuals in Kenya.")
                    ),
                    
                    box(title = "Key Components of Our Model", status = "success", solidHeader = TRUE, width = 12,
                        h4("1. Outcome Variable (Malaria Prevalence):"),
                        p("Our main interest is whether individuals are affected by malaria. This is represented as a binary outcome: '1' if malaria is present and '0' if it is not."),
                        
                        h4("2. Predictors (Factors Being Analyzed):"),
                        p("2.1. Demographic Factors: Age and sex of individuals, as these can influence susceptibility to malaria."),
                        p("2.2. Preventive Measures: Usage of bednets and bednet insecticides, critical in preventing mosquito bites."),
                        p("2.3. Environmental Conditions: Water sources near the household, as standing water can serve as mosquito breeding sites."),
                        p("2.4. Health Status: Presence of anemia and whether the individual was previously treated for malaria, as these can impact and be impacted by malaria."),
                        p("2.5. Behavioral Factors: Travel history, as traveling to areas with high malaria transmission can increase risk."),
                        
                        h4("3. Random Effects (Variability Among Households):"),
                        p("We account for differences between households that might affect malaria risk, acknowledging that some households may have characteristics that make them more susceptible to malaria.")
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           h3("Results"),
                           uiOutput("summaryStatsTable")
                    ),
                    column(width = 6,
                           h3("Interpretation"),
                           uiOutput("interpretationText")
                    )
                  ),
                  
                  
                  # Space between table and text
                  tags$br(),  # This creates a line break for spacing
                  
                  # Heading for the visualization
                  h3("Visualization"),
                  
                  # Descriptive text for the box plot
                  p("The box plot below was generated using ShinyStan, which provides interactive visual and numerical diagnostics for Bayesian models."),
                  
                  imageOutput("conditionalEffectsPlot")
              )
              
              
      ),
      tabItem(tabName = "aboutus",
              h2("About the dashboard"),
              h3("Dashboard Developer"),
              h4("Pallavi Vaswani"),
              p("Role: Student & Dashboard Developer - Master's in Health Informatics, Indiana University"),
              p("Contribution: Developed the interactive dashboard for data visualization and analysis, playing a pivotal role in the project's success."),
              p("Email: pvaswan@iu.edu"),
              
              h3("About the Data/IRB"),
              p("Due to the nature of the collected data from Kenya, a multilevel statistical model will be used to account for both household and individual variability. Modeling malaria infection within households with multiple individuals involves understanding both individual-level factors and household-level factors. This study aims to analyze the spatio-temporal patterns of malaria and anemia cases in Kenya, highlighting the interrelationship of these health conditions, the impact of different predictors, and the crucial role of health informatics and statistical analysis in understanding and addressing public health challenges."),
              p("All electronic subject data is securely stored in IU REDCap, a research data management platform equipped with encryption measures to uphold confidentiality. Access to this data is strictly limited to authorized research personnel, ensuring that only individuals directly involved in the study have permission to interact with the information. Additionally, during the dashboard implementation and statistical analysis phases of the project, no subject identifiers will be shared during the dissemination of the results, reinforcing the commitment to preserving subject confidentiality at every stage of the research process."),
              p("We thank Michael Goings (mgoings@iu.edu) for maintaining the Kenya REDCap database and providing us with access and data support when necessary."),
              p("All assent and informed consent forms, data collection instruments, and protocol were approved by the Indiana University Human Research Protection Program (HRPP) under the Expedited IRB Protocol #1711086661, with title “Malaria Transmission and Immunology (MTI): identifying risk factors for continued low malaria transmission”. PI: John, Chandy."),
              
              h3("Our Team"),
              h4("Chandy C. John, MD, MS"),
              p("Role:  Owner/Principal Investigator & Ryan White Professor of Pediatrics"),
              p("Contribution: Leading the study and overseeing research activities."),
              
              h4("Félix Pabón-Rodríguez, PhD"),
              p("Role: Advisor & Assistant Professor of Biostatistics & Health Data Science, Indiana University "),
              p("Email: fpabonr@iu.edu"),
              
              h4("Yan Zhuang, PhD"),
              p("Role: Co-Advisor & Assistant Professor, Health Informatics, Indiana University"),
              p("Email: ynzhuang@iu.edu")
              
      )
    )     
  )
  
  
)



server <- function(input, output) {
  # Assuming all your data prep has been done above this line
  
  output$edaPlot <- renderPlot({
    # Depending on the selection, render the appropriate plot
    if (input$correlationCategory == "Bednet Usage") {
      
      # Define colors for each category
      # Define colors for each category
      colors <- c("Count_Yes" = "blue", 
                  "Count_No" = "red", 
                  "Count_Sometimes" = "green", 
                  "Count_Unknown" = "orange",
                  "Count_Blank" = "gray")
      
      # Create the plot with the specified colors
      p_bednet <- ggplot(bednet_long, aes(x = Year, y = Count, color = Bednet_Use)) +
        geom_line(aes(group = Bednet_Use), size = 1) +  # Add lines
        geom_point(size = 3) +  # Add points
        scale_x_continuous("Year", breaks = seq(from = min(bednet_long$Year, na.rm = TRUE), 
                                                to = max(bednet_long$Year, na.rm = TRUE), by = 1)) +
        scale_color_manual(values = colors) +  # Assign colors
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),  # Increase font size for X axis text
              axis.text.y = element_text(size = 14),  # Increase font size for Y axis text
              axis.title = element_text(size = 16),  # Increase font size for axis titles
              plot.title = element_text(size = 18, face = "bold"),  # Increase font size and bold the plot title
              legend.position = "bottom",
              legend.title = element_text(size = 14),  # Increase font size for legend title
              legend.text = element_text(size = 12)) +  # Increase font size for legend items
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              legend.position = "bottom") +
        labs(title = "Count of Individuals by Bednet Usage per Year",
             x = "Year",
             y = "Count of Individuals",
             color = "Bednet Use")
      
      # Print the plot
      print(p_bednet)
      
      
    }else if (input$correlationCategory == "Travel History") {
      # Define colors for each category
      colors <- c("Count_Yes" = "blue", 
                  "Count_No" = "red", 
                  
                  "Count_Unknown" = "orange",
                  "Count_Blank" = "gray")
      # Assuming travel_long is already defined and contains the necessary data
      # Find the range of years without NA values
      year_range <- range(travel_long$Year, na.rm = TRUE)
      
      # Create the ggplot object for travel data
      p_travel <- ggplot(travel_long, aes(x = Year, y = Count, color = Travel_Response)) +
        geom_line(aes(group = Travel_Response), size = 1) +  # Add lines
        geom_point(size = 3) +  # Add points
        scale_x_continuous("Year", breaks = seq(from = min(travel_long$Year, na.rm = TRUE), 
                                                to = max(travel_long$Year, na.rm = TRUE), by = 1)) +
        scale_color_manual(values = colors) +  # Use the correct color mapping
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),  # Increase font size for X axis text
              axis.text.y = element_text(size = 14),  # Increase font size for Y axis text
              axis.title = element_text(size = 16),  # Increase font size for axis titles
              plot.title = element_text(size = 18, face = "bold"),  # Increase font size and bold the plot title
              legend.position = "bottom",
              legend.title = element_text(size = 14),  # Increase font size for legend title
              legend.text = element_text(size = 12)) +  # Increase font size for legend items
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom") +
        labs(title = "Count of Individuals Who Traveled Outside Nandi County per Year",
             x = "Year",
             y = "Count of Individuals",
             color = "Travel Response")
      
      # Print the ggplot for travel data
      print(p_travel)
      
    } else if (input$correlationCategory == "Symptoms") {
      Symptoms <- c("Backache", "Chills", "Diarrhoea", "Fever", "Headache", 
                    "Jaundice", "Joint_pains", "Loss_of_appetite", "Nausea", 
                    "Severe_Malaise", "Vomiting")
      
      # Create a color palette for the symptoms
      symptom_colors <- setNames(rainbow(length(Symptoms)), Symptoms)
      
      p_symptom <- ggplot(data = symptom_counts, aes(x = Year, y = Count, color = Symptom)) +
        geom_line(aes(group = Symptom), size = 1) +  # Add lines
        geom_point(size = 3) +  # Add points
        scale_x_continuous("Year", breaks = seq(min(symptom_counts$Year, na.rm = TRUE), max(symptom_counts$Year, na.rm = TRUE), by = 1)) +
        scale_color_manual(values = symptom_colors) +  # Use the correct color mapping
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),  # Increase font size for X axis text
              axis.text.y = element_text(size = 14),  # Increase font size for Y axis text
              axis.title = element_text(size = 16),  # Increase font size for axis titles
              plot.title = element_text(size = 18, face = "bold"),  # Increase font size and bold the plot title
              legend.position = "bottom",
              legend.title = element_text(size = 14),  # Increase font size for legend title
              legend.text = element_text(size = 12)) +  # Increase font size for legend items
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom") +
        labs(title = 'Count of Individuals with Symptoms by Year', x = 'Year', y = 'Count of Individuals') +
        ylab('Confirmed Symptoms Cases')
      
      # Print the ggplot
      print(p_symptom)
      
      
    }else if (input$correlationCategory == "Previously Treated for Malaria") {
      # Define the range of years without NA values for malaria_treatment_long
      year_range <- range(malaria_treatment_long$Year, na.rm = TRUE)
      # Define colors for each category
      colors <- c("Count_Yes" = "blue", 
                  "Count_No" = "red", 
                  
                  "Count_Unknown" = "orange",
                  "Count_Blank" = "gray")
      
      # Create the ggplot object for malaria treatment data
      p_malaria_treatment <- ggplot(malaria_treatment_long, aes(x = Year, y = Count, color = Treatment_Response)) +
        geom_line(aes(group = Treatment_Response), size = 1) +  # Add lines
        geom_point(size = 3) +  # Add points
        scale_x_continuous("Year", breaks = seq(from = min(malaria_treatment_long$Year, na.rm = TRUE), 
                                                to = max(malaria_treatment_long$Year, na.rm = TRUE), by = 1)) +
        scale_color_manual(values = colors) +  # Use the correct color mapping
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),  # Increase font size for X axis text
              axis.text.y = element_text(size = 14),  # Increase font size for Y axis text
              axis.title = element_text(size = 16),  # Increase font size for axis titles
              plot.title = element_text(size = 18, face = "bold"),  # Increase font size and bold the plot title
              legend.position = "bottom",
              legend.title = element_text(size = 14),  # Increase font size for legend title
              legend.text = element_text(size = 12)) +  # Increase font size for legend items
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              legend.position = "bottom") +
        labs(title = "Count of Individuals Treated for Malaria per Year",
             x = "Year",
             y = "Count of Individuals",
             color = "Treatment Response")
      
      # Print the ggplot for malaria treatment data
      print(p_malaria_treatment)
      
    }
    
    
    
    
  }) # This closes the output$edaPlot renderPlot
  
  output$longplots <- renderPlot({
    if (input$Disease == "Malaria") {
      p <- ggplot() + 
        geom_line(data = malaria.cases.diagnosed, aes(x = Year, y = Yes.Malaria, color = "Diagnosed"), size = 1) +
        geom_point(data = malaria.cases.diagnosed, aes(x = Year, y = Yes.Malaria, color = "Diagnosed")) +
        geom_text(data = malaria.cases.diagnosed, aes(x = Year, y = Yes.Malaria, label = Yes.Malaria), vjust = -1, size = 6, color = "black") +
        geom_line(data = malaria.cases.finalpf, aes(x = Year, y = Yes.Malaria, color = "Final.Pf"), size = 1) +
        geom_point(data = malaria.cases.finalpf, aes(x = Year, y = Yes.Malaria, color = "Final.Pf")) +
        geom_text(data = malaria.cases.finalpf, aes(x = Year, y = Yes.Malaria, label = Yes.Malaria), vjust = 2, size = 6, color = "black") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),  # Increase font size for X axis text
              axis.text.y = element_text(size = 14),  # Increase font size for Y axis text
              axis.title = element_text(size = 16),  # Increase font size for axis titles
              plot.title = element_text(size = 18, face = "bold"),  # Increase font size and bold the plot title
              legend.position = "bottom",
              legend.title = element_text(size = 14),  # Increase font size for legend title
              legend.text = element_text(size = 12)) +  # Increase font size for legend items
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        scale_x_continuous("Year", breaks = seq(min(malaria.cases.finalpf$Year, na.rm = TRUE), max(malaria.cases.finalpf$Year, na.rm = TRUE), by = 1)) +
        ylab('Confirmed Malaria Cases') +
        scale_color_manual(name = "Variable Used", values = c("Diagnosed" = "blue", "Final.Pf" = "red"), labels = c("Diagnosed", "Final.Pf")) +
        ggtitle("Annual Number of Confirmed Malaria Cases by Variable Used")
      
      print(p)
      
    } else if (input$Disease == "Anemia") {
      # Calculate the maximum count value and add a buffer for visibility
      max_count <- max(c(anemia_summary_combined$Yes.Anemia, anemia_summary_combined$Yes.Anemia_Simple), na.rm = TRUE)
      buffer <- max_count * 0.1  # Adding a 10% buffer to the maximum count value
      
      g <- ggplot() +
        geom_line(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia, color = "WHO Guidelines"), size = 1) +
        geom_point(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia, color = "WHO Guidelines")) +
        geom_text(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia, label = Yes.Anemia), vjust = -0.5, size = 5, color = "black") +
        geom_line(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia_Simple, color = "Hb <= 11"), size = 1) +
        geom_point(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia_Simple, color = "Hb <= 11")) +
        geom_text(data = anemia_summary_combined, aes(x = Year, y = Yes.Anemia_Simple, label = Yes.Anemia_Simple), vjust = 1.5, size = 5, color = "black") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),  # Increase font size for X axis text
              axis.text.y = element_text(size = 14),  # Increase font size for Y axis text
              axis.title = element_text(size = 16),  # Increase font size for axis titles
              plot.title = element_text(size = 18, face = "bold"),  # Increase font size and bold the plot title
              legend.position = "bottom",
              legend.title = element_text(size = 14),  # Increase font size for legend title
              legend.text = element_text(size = 12)) +  # Increase font size for legend items
        scale_x_continuous("Year", breaks = seq(min(anemia_summary_combined$Year, na.rm = TRUE), max(anemia_summary_combined$Year, na.rm = TRUE), by = 1)) +
        scale_y_continuous(limits = c(0, max_count + buffer)) +  # Dynamically adjust y-axis limits
        ylab('Number of Anemia Cases') +
        scale_color_manual(name = "Definition Used", values = c("WHO Guidelines" = "darkgreen", "Hb <= 11" = "darkorange")) +
        ggtitle("Annual Number of Anemia Cases by Definition Used")
      
      # Print the plot
      print(g)
    }
  }) 
  
  
  output$malariaMap <- renderLeaflet({
    # Get the year selected by the user
    selected_year <- input$selectedYear
    
    # Construct the filename from the selected year
    file_name <- paste0("location_counts_", selected_year, ".rds")
    
    # Check if the file exists
    if(file.exists(file_name)) {
      # Read the location data for the selected year
      location_counts <- readRDS(file_name)
      
      # Ensure the 'Count' column exists
      if("Count" %in% colnames(location_counts)) {
        # Compute the center of your data points for better map focus
        center_lat <- mean(c(min(location_counts$`Latitude..N.`), max(location_counts$`Latitude..N.`)))
        center_lon <- mean(c(min(location_counts$`Longitude..E.`), max(location_counts$`Longitude..E.`)))
        
        # Define color bins and labels
        breaks <- quantile(location_counts$Count, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
        color_pal <- colorBin("YlOrRd", domain = location_counts$Count, bins = breaks, na.color = "transparent")
        labels <- paste(head(breaks, -1), tail(breaks, -1), sep = " - ")
        colors <- color_pal(breaks[-length(breaks)])  # Exclude the last break for color length matching
        
        # Generate the Leaflet map
        leaflet_data <- leaflet(location_counts) %>%
          addTiles() %>%
          setView(lng = center_lon, lat = center_lat, zoom = 12) %>%
          addCircleMarkers(
            lng = ~`Longitude..E.`, lat = ~`Latitude..N.`,
            radius = ~sqrt(Count) * 2,
            color = ~color_pal(Count),
            fillOpacity = 0.8,
            popup = ~paste('<b>Latitude:</b>', `Latitude..N.`, '<br>', '<b>Longitude:</b>', `Longitude..E.`, '<br>', '<b>Count:</b>', Count),
            options = leafletOptions(
              onEachFeature = htmlwidgets::JS(
                "function(feature, layer) {",
                "layer.on('mouseover', function(e) {",
                "layer.openPopup();",
                "});",
                "layer.on('mouseout', function(e) {",
                "layer.closePopup();",
                "});",
                "}"
              )
            )
          ) %>%
          addLegend(position = "bottomright", pal = color_pal, values = ~Count, labels = labels, title = "Case Count")
        
        return(leaflet_data)
      } else {
        return(leaflet() %>% addTiles() %>% addMarkers(lng = 36.8219, lat = -1.2921, popup = "No 'Count' column found in data"))
      }
    } else {
      return(leaflet() %>% addTiles() %>% addMarkers(lng = 36.8219, lat = -1.2921, popup = "No data available for this year"))
    }
  })
  output$modelComponents <- renderUI({
    tags$ul(
      tags$li(HTML("<strong>Outcome Variable (Malaria Prevalence):</strong> Our main interest is whether individuals are affected by malaria. This is represented as a binary outcome: '1' if malaria is present and '0' if it is not.")),
      tags$li(HTML("<strong>Predictors (Factors Being Analyzed):</strong> Detailed breakdown as follows:")),
      tags$li(HTML("<strong>Demographic Factors:</strong> Age and sex of individuals, as these can influence susceptibility to malaria.")),
      tags$li(HTML("<strong>Preventive Measures:</strong> Usage of bednets and bednet insecticides, critical in preventing mosquito bites.")),
      tags$li(HTML("<strong>Environmental Conditions:</strong> Water sources near the household, as standing water can serve as mosquito breeding sites.")),
      tags$li(HTML("<strong>Health Status:</strong> Presence of anemia and whether the individual was previously treated for malaria, as these can impact and be impacted by malaria.")),
      tags$li(HTML("<strong>Behavioral Factors:</strong> Travel history, as traveling to areas with high malaria transmission can increase risk.")),
      tags$li(HTML("<strong>Random Effects (Variability Among Households):</strong> We account for differences between households that might affect malaria risk, acknowledging that some households may have characteristics that make them more susceptible to malaria."))
    )
  })
  # Load your data
  dt <- readRDS("summary_stats.rds")
  
  # Function to color negative numbers red
  color_negatives <- function(x) {
    ifelse(x < 0, 
           paste0('<span style="color:red;">', sprintf("%.3f", x), '</span>'), 
           sprintf("%.3f", x))
  }
  
  # Apply conditional formatting
  dt_formatted <- dt
  numeric_columns <- sapply(dt_formatted, is.numeric)  # Identify numeric columns
  dt_formatted[, numeric_columns] <- lapply(dt_formatted[, numeric_columns], color_negatives)
  
  # Render the table
  output$summaryStatsTable <- renderUI({
    # Create the table using kable and style it with kableExtra
    kbl(dt_formatted[1:13,], format = "html", escape = FALSE) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE, 
        position = "left", 
        font_size = 12
      ) %>%
      row_spec(0, bold = TRUE, color = "white", background = "#4e79a7") %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      scroll_box(width = "100%", height = "500px") %>%
      as.character() %>%
      HTML()
  })
  output$interpretationText <- renderUI({
    tags$ul(
      tags$li(HTML("<strong>Intercept:</strong> This is a starting point for the chance of getting malaria before thinking about any specific personal factors. The model suggests this starting chance is quite low.")),
      tags$br(),
      tags$li(HTML("<strong>Age:</strong> As people get older, they might be less likely to get malaria, according to the model.")),
      tags$br(),
      tags$li(HTML("<strong>Sex:</strong> Whether someone is male doesn't seem to have a strong effect on their chance of getting malaria compared to females.")),
      tags$br(),
      tags$li(HTML("<strong>Bednet Usage (Sometimes):</strong> People who sometimes use bednets might have a slightly different risk of getting malaria, but it's not clear if it's higher or lower.")),
      tags$br(),
      tags$li(HTML("<strong>Bednet Usage (Yes):</strong> People who regularly use bednets are likely to have a reduced risk of malaria.")),
      tags$br(),
      tags$li(HTML("<strong>Travelled (Yes):</strong> Individuals who have recently traveled might be at a higher risk of malaria.")),
      tags$br(),
      tags$li(HTML("<strong>Bednet Insecticide (Yes):</strong> Using bednets treated with insecticide might lower the risk of getting malaria.")),
      tags$br(),
      tags$li(HTML("<strong>Water Source Proximity:</strong> Living close to water doesn't seem to increase the risk of malaria as much as might be expected.")),
      tags$br(),
      tags$li(HTML("<strong>Malaria Treatment (Yes):</strong> If someone has been treated for malaria before, they're more likely to have had malaria.")),
      tags$br(),
      tags$li(HTML("<strong>Anemia (Yes):</strong> The presence of anemia doesn't seem to significantly affect the likelihood of having malaria.")),
      tags$br(),
      tags$li(HTML("<strong>Pregnancy Status (Pregnant):</strong> Being pregnant might change someone's risk of getting malaria a bit.")),
      tags$br(),
      tags$li(HTML("<strong>Household Variation:</strong> There's a big difference in malaria risk from one household to another."))
    )
  })
  
  output$conditionalEffectsPlot <- renderImage({
    # Read the raw vector from the RDS file
    image_raw <- readRDS("boxplot.rds")
    
    # Create a temporary file
    image_path <- tempfile(fileext = ".png")
    
    # Write the raw vector to the temporary file
    writeBin(image_raw, image_path)
    
    # Return a list containing the path and other attributes for the image
    list(src = image_path,
         contentType = 'image/png',
         width = "100%",
         height = "400px",
         alt = "This is an image")
  }, deleteFile = TRUE) # Set deleteFile = TRUE to remove the temp file after use
  
  
  
  output$homeImage <- renderImage({
    
    home_raw <- readRDS("kenya_map.rds")
    
    # Create a temporary file with a .webp extension
    home_pic <- tempfile(fileext = ".webp")
    
    # Write the raw vector to the temporary file
    writeBin(home_raw, home_pic)
    
    # Return a list with the correct parameters for a .webp image
    list(
      src = home_pic,
      contentType = 'image/webp',
      width = "100%",
      height = "400px",
      alt = "Kenya Map"
    )
  }, deleteFile = TRUE)
  
  epidemiological_profile <- data.frame(
    `Country/Area` = "Kenya",
    `Year` = 2020,
    `High Transmission` = "37.7M (70%)",
    `Low Transmission` = "16M (30%)",
    `Malaria-Free` = "0 (0%)",
    `Estimated Cases` = "2.7M [1.6M, 4.5M]",
    `Estimated Deaths` = "12.6K [11.4K, 14.8K]"
  )
  output$epidemiologicalProfileTable <- renderDT({
    datatable(
      epidemiological_profile,
      rownames = FALSE,
      options = list(
        dom = 't',
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE
      )
    )
  })
  
  
  # Server part
  output$casesSummaryTable <- DT::renderDataTable({
    # Prepare the data frame based on the summarized information
    cases_summary_df <- data.frame(
      Category = c('cases=0', 'cases <=5', 'cases<=10', 'cases>10'),
      Years = c('2002, 2003, 2004, 2005, 2006, 2017, 2018, 2022',
                '2012, 2016, 2020, 2021',
                '2010, 2013, 2015, 2019',
                '2007, 2008, 2009, 2011, 2014')
    )
    
    # Return the data frame to be rendered as a DataTable
    DT::datatable(cases_summary_df, options = list(pageLength = 5, searching = FALSE, paging = FALSE))
  })


  
} # This closes the server function

# Shiny app call should be outside the function definitions
shinyApp(ui = ui, server = server)

