install.packages('TMB', type = 'source')
library(lme4)
library(ggplot2)
library(circular)
library(dplyr)
library(glmmTMB)
# read in female data
df <- read.csv("C:/Users/chamberlinlab/Desktop/Roost_Chapter/Final_All_Covariates_Females_8-7.csv")
use <- subset(df, Use==1)
avail <- subset(df, Use==0)
summary(use$feature.roughness)
# Count how many locations are removed per ID and then multiply by 5
location_count_removed <- use %>%
  group_by(ID) %>%
  summarise(locations_removed = 5 * sum(grepl("Incubation", Phase, ignore.case = TRUE)))

# Calculate the total of locations_removed column
total_locations_removed <- sum(location_count_removed$locations_removed)

# Assuming 'use' is your data frame
total_incubation_count <- sum(grepl("incubation", use$Phase, ignore.case = TRUE))

# Merge location_count_removed with avail to get the number of locations removed for each ID
merged_data <- merge(avail, location_count_removed, by = "ID", all.x = TRUE)

# Calculate the new number of points to keep for each ID
merged_data <- merged_data %>% group_by(ID) %>%
  mutate(points_to_keep = n() - locations_removed)

# Filter avail to keep only the required number of points for each ID
filtered_avail <- merged_data %>%
  group_by(ID) %>%
  slice(seq_len(min(first(points_to_keep), n()))) %>%
  ungroup()

# Remove incubation from use
use <- subset(use, !grepl("Incubation", Phase, ignore.case = TRUE))

# Assuming 'use' is your data frame
# Duplicate each row by 5 times
duplicated_use <- use %>%
  slice(rep(row_number(), each = 5))

# Order 'duplicated_use' by ID
duplicated_use <- duplicated_use %>%
  arrange(ID)

# Order 'avail' by ID
filtered_avail <- filtered_avail %>%
  arrange(ID)

# Add the 'Phase' column to 'duplicated_use'
filtered_avail$Phase <- cbind(duplicated_use$Phase)

# Combine data frames by rows
df <- bind_rows(filtered_avail, use)

# Remove observations of one roost
# Remove specific values from the ID column
df <- df[!df$ID %in% c(46213, 47396), ]

write.csv(df, "C:/Users/chamberlinlab/Desktop/Roost_Chapter/Final_All_Covariates_Females_8-7.csv")

# Read in network file
network <- read.csv("C:/Users/chamberlinlab/Desktop/Roost_Chapter/Final_Network_Analysis_8-21.csv")

# Standardize covariates
df$proads.s <- scale(df$proads)
df$sroads.s <- scale(df$sroads)
df$aspect.s <- scale(df$feature.aspect)
df$elevation.s <- scale(df$elevation)
df$roughness.s <- scale(df$feature.roughness)
df$slope.s <- scale(df$feature.slope)
df$dist_open.s <- scale(df$dist_open)
df$dist_water.s <- scale(df$dist_water)

# Make phases compressed
df[df == "Prelaying_Before_03_01"] <- "Prelaying"
df[df == "Before_A2"] <- "Transition"
df[df == "Before_A3"] <- "Transition"
df[df == "Before_A4"] <- "Transition"

# Remove brooding from use
df <- subset(df, !grepl("Brooding", Phase, ignore.case = TRUE))

# Remove transition from use
df <- subset(df, !grepl("Transition", Phase, ignore.case = TRUE))

# Compute Pearson correlation coefficients
cor(df[, 40:46], use = "complete.obs")

# RSF Modeling
# Null model
mod1 <- glmmTMB(Use ~ 1 + (1|ID), family = binomial(), data = df)
summary(mod1)

# Fit the model with the specified optimizer
mod2 <- glmmTMB(Use ~ sroads.s:Phase +  
                  elevation.s:Phase + roughness.s:Phase + 
                  dist_open.s:Phase + dist_water.s:Phase +
                  (1|ID), 
                  family = binomial(), data = df)
summary(mod2)

# Landcover model
mod3 <- glmmTMB(Use ~ sroads.s:Phase + dist_open.s:Phase + dist_water.s:Phase +
                  (1|ID), family = binomial(), data = df)
summary(mod3)

# Terrain model
mod4 <- glmmTMB(Use ~ elevation.s:Phase + roughness.s:Phase +
                  (1|ID), family = binomial(), data = df)
summary(mod4)

# Network modeling
# Standardize covariates
network$proads.s <- scale(network$proads) 
network$sroads.s <- scale(network$sroads)
network$aspect.s <- scale(network$feature.aspect)
network$elevation.s <- scale(network$elevation)
network$roughness.s <- scale(network$feature.roughness)
network$slope.s <- scale(network$feature.slope)
network$dist_open.s <- scale(network$dist_open)
network$dist_water.s <- scale(network$dist_water)
unique(network$Phase)
# Remove brooding from use
network <- subset(network, !grepl("Brooding", Phase, ignore.case = TRUE))

# Remove transition from use
network <- subset(network, !grepl("Transition", Phase, ignore.case = TRUE))

# Compute Pearson correlation coefficients
cor(network[, 54:60], use = "complete.obs")

############# run with hub (1) and satellite (0) as the response ##############

# Make hubs and satellites 1 and 0s
network[network == "Hub"] <- 1
network[network == "Non-Hub"] <- 0

# Make type column numeric
network$type <- as.numeric(network$type)

# Null model
mod1nh <- glmmTMB(type ~ 1 + (1|ID), family = binomial(), data = network)
summary(mod1nh)

# Global model
mod2nh <- glmmTMB(type ~ sroads.s:Phase  + 
                   elevation.s:Phase + roughness.s:Phase + 
                   dist_open.s:Phase + dist_water.s:Phase +
                   (1|ID), family = binomial(), data = network)
summary(mod2nh)

# Landcover model
mod3nh <- glmmTMB(type ~ sroads.s:Phase + dist_open.s:Phase + dist_water.s:Phase +
                    (1|ID), family = binomial(), data = network)
summary(mod3nh)

# Terrain model
mod4nh <- glmmTMB(type ~ elevation.s:Phase + roughness.s:Phase +
                    (1|ID), family = binomial(), data = network)
summary(mod4nh)

#################### AIC output ###############################################
library(AICcmodavg)
#### Roost RSF ####
Cand.models.rsf <- list(mod1,mod2,mod3,mod4)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(Cand.models.rsf), sep = " ")

##generate AICc table
aictab(cand.set = Cand.models.rsf, modnames = Modnames, sort = TRUE)

##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = Cand.models.rsf, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

#### Roost type (hub, non-hub) ####
Cand.models.net <- list(mod1nh,mod2nh,mod3nh,mod4nh)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(Cand.models.net), sep = " ")

##generate AICc table
aictab(cand.set = Cand.models.net, modnames = Modnames, sort = TRUE)

##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = Cand.models.net, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

############# Model Performace ##############################################
# Model performance
library(gmodels)
library(pROC)

# RSF Performance
rsf <- predict(mod2, df, type="response",allow.new.levels=T)

AUC_rsf <- ci(roc(df$Use, rsf)) %>% as.numeric %>%
  round(digits = 2)

AUC_rsf

# type Performance
type <- predict(mod2nh, network, type="response",allow.new.levels=T)

AUC_type <- ci(roc(network$type, type)) %>% as.numeric %>%
  round(digits = 2)

AUC_type

############ coefficient plots ###############################################
library(ggplot2)
library(broom.mixed)
library(splitstackshape)

# Resource selction model
# Extract the fixed effects coefficients using 'tidy'
coef_data <- tidy(mod2, effects = "fixed", conf.int=TRUE)

#Splits the columns
coef_data <- cSplit(coef_data,c("term"), c(":"))

write.csv(coef_data, "C:/Users/chamberlinlab/Desktop/Roost_Chapter/Coefficient_RSF_8-21.csv")

coef_data <- read.csv("C:/Users/chamberlinlab/Desktop/Roost_Chapter/Coefficient_RSF_8-21.csv")

# Create a new variable by combining 'term_1' and 'term_2' using interaction()
coef_data$term_interaction <- with(coef_data, interaction(term_1, term_2))

# Plot the coefficients with different colors for each interaction term
coef_plot <- ggplot(coef_data, aes(x = term_2, y = estimate, shape = term_1)) +
  geom_pointrange(position = position_dodge(width = 0.6), aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Covariates") +
  ylab("Coefficient estimate") +
  scale_y_continuous(limits = c(-0.3,0.3)) +
  guides(color = guide_legend(title = "Reproductive phase"),
         shape = guide_legend(title = "Reproductive phase"),
         fill = FALSE) +  # Remove fill legend
  theme_bw()

print(coef_plot)

# Network model
# Extract the fixed effects coefficients using 'tidy'
coef_data2 <- tidy(mod2nh, effects = "fixed", conf.int=TRUE)

#Splits the columns
coef_data2 <- cSplit(coef_data2,c("term"), c(":"))

write.csv(coef_data2, "C:/Users/chamberlinlab/Desktop/Roost_Chapter/Coefficient_HVS_8-21.csv")

coef_data2 <- read.csv("C:/Users/chamberlinlab/Desktop/Roost_Chapter/Coefficient_HVS_8-21.csv")

# Create a new variable by combining 'term_1' and 'term_2' using interaction()
coef_data2$term_interaction <- with(coef_data2, interaction(term_1, term_2))

# Plot the coefficients with different colors for each interaction term
coef_plot2 <- ggplot(coef_data2, aes(x = term_2, y = estimate, shape = term_1)) +
  geom_pointrange(position = position_dodge(width = 0.6), aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Covariates") +
  ylab("Coefficient estimate") +
  scale_y_continuous(limits = c(-0.5,0.5)) +
  guides(color = guide_legend(title = "Reproductive phase"),
         shape = guide_legend(title = "Reproductive phase"),
         fill = FALSE) +  # Remove fill legend
  theme_bw()

print(coef_plot2)
unique(network$featur)

ggplot(data = network, 
       aes(x = Phase, y = feature.aspect)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = 1) +
  geom_boxplot(width = .3, outlier.shape = NA, alpha = 0.5)+ 
  guides(fill = FALSE) +
  guides(color = FALSE)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

##################### Predictive Plots #########################################
library(ggeffects)
summary(network_final_use)
pred <- ggpredict(mod2nh, terms = c("aspect.s[all]", "Phase"))
plot(pred)
network_final_use$feature.aspect <- as.numeric(network_final_use$feature.aspect)

pred$aspect_unscaled <- pred$x * sd(network_final_use$feature.aspect) + 
                        mean(network_final_use$feature.aspect)

aspect_plot <- ggplot(data=pred, aes(x=aspect_unscaled, y=predicted, color=group)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Aspect") + 
  ylab("Probability of Hub") + theme_classic()
aspect_plot

