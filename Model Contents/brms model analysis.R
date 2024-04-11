
library(glmmLasso)
library(pROC)
library(brms) 
library(lme4)
library(bild)
library(optimx)
library(MuMIn)
library(posterior)
library(bayesplot)
library(tidyverse)


model.data <-readRDS("Model_data.rds")
colnames(model.data)

# consider all blank as NA (for now!)
model.data[(model.data == "")|(model.data== " ")] <- NA

# set proper data type
model.data$Year <- as.numeric(model.data$Year)
# Ensure 'Sex' is a factor
model.data$Sex <- as.factor(model.data$Sex)

# Set the levels of the factor, if needed (optional since it's already 0 and 1)
levels(model.data$Sex) <- c("0", "1")

model.data$Risk.factors[is.na(model.data$Risk.factors)] <- "No"
model.data$Risk.factors <- as.factor(model.data$Risk.factors)
levels(model.data$Risk.factors) <- c("0","1")

model.data$Bednet <- as.factor(model.data$Bednet)
levels(model.data$Bednet) <- c("No","Sometimes",NA,"Yes")

model.data$Travelled <- as.factor(model.data$Travelled)
levels(model.data$Travelled) <- c("No",NA,"Yes")

model.data$Bednet_Insecticide <- as.factor(model.data$Bednet_Insecticide)
levels(model.data$Bednet_Insecticide) <- c("0","1")

model.data$malaria_treated <- as.factor(model.data$malaria_treated)
levels(model.data$malaria_treated) <- c("0","1")

model.data$pregnancy_status <- as.factor(model.data$pregnancy_status)
levels(model.data$pregnancy_status) <- c("Not Pregnant","Not Pregnant","Pregnant")

model.data$X.PF <- as.factor(model.data$X.PF)
levels(model.data$X.PF) <- c("0","1")

model.data$Pond <- as.factor(model.data$Pond)
levels(model.data$Pond) <- c("1","0")
model.data$Pond <- factor(model.data$Pond, levels=c("0", "1"))

model.data$River <- as.factor(model.data$River)
levels(model.data$River) <- c("1","0")
model.data$River <- factor(model.data$River, levels=c("0", "1"))

model.data$Well <- as.factor(model.data$Well)
levels(model.data$Well) <- c("1","0")
model.data$Well <- factor(model.data$Well, levels=c("0", "1"))

model.data$Pan <- as.factor(model.data$Pan)
levels(model.data$Pan) <- c("0")

model.data$Borehole <- as.factor(model.data$Borehole)
levels(model.data$Borehole) <- c("1","0")
model.data$Borehole <- factor(model.data$Borehole, levels=c("0", "1"))

model.data$Piped <- as.factor(model.data$Piped)
levels(model.data$Piped) <- c("1","0")
model.data$Piped <- factor(model.data$Piped, levels=c("0", "1"))

model.data$Tank <- as.factor(model.data$Tank)
levels(model.data$Tank) <- c("1","0")
model.data$Tank <- factor(model.data$Tank, levels=c("0", "1"))

model.data$Water_Source <- 
  as.numeric(as.character(model.data$Pond)) + 
  as.numeric(as.character(model.data$River)) + 
  as.numeric(as.character(model.data$Well)) + 
  as.numeric(as.character(model.data$Pan)) + 
  as.numeric(as.character(model.data$Borehole)) + 
  as.numeric(as.character(model.data$Piped)) +
  as.numeric(as.character(model.data$Tank))
model.data$Water_Source2 <- as.factor(ifelse(model.data$Water_Source > 0, 1,0))
model.data$Anemia <- as.factor(model.data$Anemia)

model.data$Household.ID <- as.factor(model.data$Household.ID)
model.data$Record.ID <- as.factor(model.data$Record.ID)


m2 <- glmer(X.PF ~ Repeat.Instance + Age + Sex + Bednet + 
              Travelled + Bednet_Insecticide + Water_Source + 
              malaria_treated + Anemia + pregnancy_status +
              (1 | Household.ID), 
            data = model.data, 
            family = binomial(link = "logit"),
            control = glmerControl(optimizer ='optimx', 
                                   optCtrl=list(method='nlminb')))

summary(m2)





# Defining the model
# We will keep the structure similar to your glmer model
# Note: brms automatically uses non-informative priors if we don't specify them explicitly.
bform <- bf(
  X.PF ~ Repeat.Instance + Age + Sex + Bednet + 
    Travelled + Bednet_Insecticide + Water_Source + 
    malaria_treated + Anemia + pregnancy_status + 
    (1 | Household.ID),
  family = bernoulli(link = "logit")
)

# Compile and fit the model
# This might take a while because it's sampling from the posterior distribution
bmodel <- brm(
  formula = bform, 
  data = model.data,
  chains = 4,             # Default, but specifying for clarity
  warmup = 2000,          # Number of warmup iterations per chain (adaptation)
  iter = 4000,            # Total number of iterations per chain
  control = list(adapt_delta = 0.95) # Higher adapt_delta helps with convergence, especially for complex models
)

# Summary of the model
summary(bmodel)
plot(bmodel)
coef(bmodel)
mcmc.brm <- as.mcmc(bmodel, combine_chains = TRUE)

ex.bBednetYes <- mcmc.brm[,grepl("BednetYes",colnames(mcmc.brm))]
hist(ex.bBednetYes)
mean(ex.bBednetYes < 0)


draws <- as_draws_array(bmodel)
# Using posterior package functions to calculate summary statistics
summary_stats <- posterior::summarise_draws(draws)
summary_stats

# Calculating probabilities of parameters being greater than 0
prob_greater_than_0 <- colMeans(draws > 0, dims = 2)
prob_less_than_0 <- 1 - prob_greater_than_0

# Assuming the order of rows in summary_stats matches the order of parameters in draws
# Adding the calculated probabilities to the summary_stats tibble
summary_stats$ProbGreater0 <- prob_greater_than_0
summary_stats$ProbLess0 <- prob_less_than_0
setwd("C:/Users/palla/Indiana University")
saveRDS(summary_stats, "summary_stats.rds")

# Displaying the final enhanced table
print(summary_stats)

print(summary_stats, n = 13)



install.packages("kableExtra")
library(kableExtra)
dt <- summary_stats[1:13,]
kableExtra::kbl(dt) %>% kable_styling()

## For exploration
# Note: brms automatically uses non-informative priors if we don't specify them explicitly.
bform2 <- bf(
  Anemia ~ Repeat.Instance + Age + Sex + Bednet + 
    Travelled + Bednet_Insecticide + Water_Source + 
    malaria_treated + X.PF + pregnancy_status + 
    (1 | Household.ID),
  family = bernoulli(link = "logit")
)





#Visualizations:\

# Assuming 'bmodel' is your fitted brms model
# Generate MCMC trace plots for specified parameters
mcmc_trace(bmodel, pars = c("b_Intercept", "b_Repeat.Instance", "b_Age"))



library(brms)  
library(ggplot2)  

# Plot conditional effects for single predictors
plot(conditional_effects(bmodel, effects = c("Age", "Sex", "Bednet")))

if (!requireNamespace("shinystan", quietly = TRUE)) {
  install.packages("shinystan")
}
library(shinystan)


