library(sp)
library(geosphere)
library(dplyr)
library(rgdal)
library(purrr)
library(dbscan) # Density based cluster analysis with noise
library(asnipe) # Social Network Package frome Farine
library(igraph) # For ploting and calculating the statistics
library(ggplot2)
library(sf)

# Read in roost file
df1 <- read.csv("E:/Roost_Chapter/Use_Available_Covariates_50_random.csv")
unique(df$ID)
df <- subset(df1, Use==1)
df1 <- subset(df1, Use==0)

# Convert date column to proper date format (skip this step if it's already in the correct format)
df$LMT_Date <- as.Date(df$LMT_Date, format = "%Y-%m-%d")
head(df)

# Make phases compressed
df[df$Phase == "Prelaying_Before_03_01", "Phase"] <- "Prelaying"
df[df$Phase == "Before_A2", "Phase"] <- "Transition"
df[df$Phase == "Before_A3", "Phase"] <- "Transition"
df[df$Phase == "Before_A4", "Phase"] <- "Transition"

# Remove brooding from use
df <- subset(df, !grepl("Brooding", Phase, ignore.case = TRUE))

# Remove transition from use
df <- subset(df, !grepl("Transition", Phase, ignore.case = TRUE))

###############################################################################
######################## For loop to do all the individuals and store in
######################## a dataframe ##########################################

# Create list of unique IDs
uniql <- unique(unlist(df$ID))

# Create empty list by unique ID to store data
datalist <- list()
datalist = random_memory = vector("list", length(uniql))

datalist2 <- list()
datalist2 = random_memory = vector("list", length(uniql))

# Create an empty vector to store quantile sensitivity values
opdist_values <- numeric(length(uniql))

# Loop through each group (ID) in the dataframe
for (j in 1:length(uniql)) {
  
  # Subset the dataframe for the current ID
  df_edges_test <- subset(df, ID == uniql[j])
  head(df_edges_test)
  
  # Calculate distance matrix using Haversine formula
  distMatrix <- distm(df_edges_test[, c("Lon", "Lat")], fun = distHaversine)
  
  # Determine optimal clustering parameter using sensitivity analysis
  distRange <- seq(27.5, 3500, length.out = 20)
  sensitivity <- numeric(length(distRange))
  for (i in 1:length(distRange)) {
    db <- dbscan(distMatrix, eps = distRange[i], minPts = 2)
    sensitivity[i] <- sum(db$cluster != 0)
  }
  quantile_sensitivity <- quantile(sensitivity, 0.5)
  optIndex <- min(which(sensitivity >= quantile_sensitivity))
  optDist <- 500
  
  # Store quantile_sensitivity for the current individual
  opdist_values[j] <-  optDist
  
  # Run DBSCAN clustering with optimal parameter
  clusters <- dbscan(distMatrix, eps = optDist, minPts = 2)
  
  # Add a new column to the spatial data with cluster affiliation information
  df_edges_test$cluster_id <- NA
  for (i in unique(clusters$cluster)) {
    if (i == 0) next  # skip noise points
    indices <- clusters$cluster == i
    df_edges_test$cluster_id[indices] <- i
  }
  
  # Assign unique cluster IDs to NAs
  n_na <- sum(is.na(df_edges_test$cluster_id))
  if (n_na > 0) {
    max_cluster_id <- max(df_edges_test$cluster_id, na.rm = TRUE)
    new_cluster_ids <- seq(max_cluster_id + 1, max_cluster_id + n_na)
    df_edges_test$cluster_id[is.na(df_edges_test$cluster_id)] <- new_cluster_ids
  }
  
  # Append the current individual's data with cluster affiliation to the results dataframe
  datalist[[j]] <- as.data.frame(df_edges_test)
  # Append the current individual's data with cluster affiliation to the results dataframe
  datalist2[[j]] <- sensitivity
}

# Re-combine to a dataframe nodes and edges
roost_edges <- do.call(rbind, datalist)

###############################################################################
######################### Final Network Analysis ##############################
###############################################################################

# Create an empty list to store subset_data
subset_data_list <- list()

# Create an empty dataframe to store the results
attributes_df <- data.frame(ID = character(),
                            mean_cluster_lat = numeric(),
                            mean_cluster_lon = numeric(),
                            connectedness = numeric(),
                            strength = numeric(),
                            betweenness = numeric(),
                            degree = numeric(),
                            clustering_coefficient = numeric(),
                            network_structure = character(),
                            stringsAsFactors = FALSE)

edges_df <- data.frame(bird_id = character(),
                       edges = I(list()),
                       stringsAsFactors = FALSE)

hub_df <- data.frame(bird_id = character(),
                     type = character(), 
                     mean_cluster_lat = numeric(),
                     mean_cluster_lon = numeric(),
                     stringsAsFactors = FALSE)

# Iterate over each unique bird_id
for (ID in unique(roost_edges$ID)) {
  # Subset the data for the current bird_id
  subset_data <- roost_edges[roost_edges$ID == ID, ]
  
  # Create edges based on cluster_id column for the current bird_id
  edges <- as.data.frame(cbind(head(subset_data$cluster_id, -1), tail(subset_data$cluster_id, -1)))
  
  # Group the data by source and target nodes and calculate interaction frequency
  frequency_data <- edges %>%
    group_by(V1, V2) %>%
    summarise(weight = n()) %>%
    ungroup()
  
  # Merge the interaction frequencies back into the original data frame
  edges_with_weights <- merge(edges, frequency_data, by = c("V1", "V2"), all.x = TRUE)
  
  # Create the edges for the graph with the interaction frequencies as weights
  edges <- edges_with_weights[, c("V1", "V2")]
  
  # Optional: If you want to include the weights in the graph as an attribute
  weights <- edges_with_weights$weight
  
  # Calculate mean cluster lat and lon
  subset_data$mean_cluster_lat <- tapply(subset_data$Lat, subset_data$cluster_id, mean)[subset_data$cluster_id]
  subset_data$mean_cluster_lon <- tapply(subset_data$Lon, subset_data$cluster_id, mean)[subset_data$cluster_id]
  
  # Save the subset_data before removing duplicates
  subset_data_list[[ID]] <- subset_data
  
  # Remove duplicates from the "Name" column
  subset_data <- distinct(subset_data, cluster_id, .keep_all = TRUE)
  
  # Create a graph object for the current bird_id
  graph <- graph_from_data_frame(d = edges, directed = TRUE)
  
  # Assign the weights to the edges as an attribute (optional)
  E(graph)$weight <- weights
  
  # Add vertex attributes to the graph object
  V(graph)$mean_cluster_lat <- subset_data$mean_cluster_lat
  V(graph)$mean_cluster_lon <- subset_data$mean_cluster_lon
  rows <- length(V(graph)$mean_cluster_lon)
  # Append NA at the end of edges and weights
  edges <- rbind(edges, c(NA, NA))
  
  # Calculate the connectedness for each node
  connectedness_values <- degree(graph)
  
  # Calculate the strength for each node
  strength_values <- strength(graph)
  
  # Calculate the betweenness centrality for each node
  betweenness_values <- betweenness(graph)
  
  # Calculate the closeness for each node
  closeness_values <- closeness(graph, normalized=TRUE)
  
  # Calculate the centrality for each node
  centrality_values <- eigen_centrality(graph, scale=TRUE)
  centrality_values
  # Calculate clustering coefficient for each node
  clustering_coefficient <- transitivity(graph)
  
  # Calculate degree for each node
  degree_values <- degree(graph)
  
  # Calculate the 90th percentile of Degree
  degree_90th_percentile <- quantile(degree_values, 0.9)
  
  # Create a new column for the node type
  type <- ifelse(degree_values >= degree_90th_percentile, "Hub", "Non-Hub")
  
  # Store the results in the respective data frames
  # Extract attributes, connectedness, and centrality from the graph
  attributes <- data.frame(ID = subset_data$ID,
                           mean_cluster_lat = V(graph)$mean_cluster_lat,
                           mean_cluster_lon = V(graph)$mean_cluster_lon,
                           connectedness = connectedness_values,
                           strength = strength_values,
                           betweenness = betweenness_values,
                           centrality = centrality_values,
                           closeness = closeness_values,
                           degree = degree_values,
                           clustering_coefficient = clustering_coefficient)
  head(subset_data)
  # Store the attributes in the attributes_df dataframe
  attributes_df <- rbind(attributes_df, attributes)
  edges_df <- rbind(edges_df, data.frame(ID = ID, edges = list(edges), stringsAsFactors = FALSE))
  hub_df <- rbind(hub_df, data.frame(ID = subset_data$ID, 
                                     type = type, 
                                     mean_cluster_lat = subset_data$mean_cluster_lat,
                                     mean_cluster_lon = subset_data$mean_cluster_lon,
                                     stringsAsFactors = FALSE))
}

# Re-combine to a dataframe nodes and edges
network_final <- do.call(rbind, subset_data_list)
network_final <- as.data.frame(network_final)
summary(network_final)
# Create a column (ID and Date) to match laying within each date sheet
attributes_df$identifier <- paste(attributes_df$ID, attributes_df$mean_cluster_lat, 
                                  attributes_df$mean_cluster_lon)

hub_df$identifier <- paste(hub_df$ID, hub_df$mean_cluster_lat, 
                           hub_df$mean_cluster_lon)

network_final$identifier <- paste(network_final$ID, network_final$mean_cluster_lat, 
                                  network_final$mean_cluster_lon)

# Add data to network final sheet
network_final$type <- hub_df$type[sapply(network_final$identifier, 
                        function(x) match(x, hub_df$identifier))]

network_final$connectedness <- attributes_df$connectedness[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]

network_final$strength <- attributes_df$strength[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]

network_final$betweeness <- attributes_df$betweenness[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]

network_final$centrality.centralization <- attributes_df$centrality.vector[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]

network_final$closeness <- attributes_df$closeness[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]

network_final$clustering_coefficient <- attributes_df$clustering_coefficient[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]

network_final$degree <- attributes_df$degree[sapply(network_final$identifier, 
                        function(x) match(x, attributes_df$identifier))]
summary(network_final)

########################### Create a sheet with no duplicate roost ############

# Write csv of network data
write.csv(network_final, "C:/Users/chamberlinlab/Desktop/Roost_Chapter/Final_Network_Analysis_8-21.csv")
network_final <- read.csv("C:/Users/chamberlinlab/Desktop/Roost_Chapter/Final_Network_Analysis_8-21.csv")

# Keep only one row per unique combination of ID and ClusterID
unique_rows <- network_final %>%
  distinct(ID, cluster_id, .keep_all = TRUE)

unique(network_final$Phase)

# Convert date column to proper date format (skip this step if it's already in the correct format)
network_final$LMT_Date <- as.Date(network_final$LMT_Date, format = "%Y-%m-%d")

# Arrange the data frame within each group (ID and Phase) based on the date or timestamp
network_final <- network_final %>%
  group_by(ID, Phase) %>%
  arrange(LMT_Date)

# Total roost by phase
table(network_final$Phase)

# Calculate the average distance and standard deviation for each phase
summary_distance_by_Phase <- network_final %>%
  group_by(Phase) %>%
  summarise(average_distance_meters = mean(distance_meters, na.rm = TRUE),
            sd_distance_meters = sd(distance_meters, na.rm = TRUE))

# Calculate number of unique roost by phase
# Step 1: Sum the unique cluster_id by ID and Phase
sum_by_ID_Phase <- network_final %>%
  group_by(ID, Phase) %>%
  summarise(unique_clusters = n_distinct(cluster_id))

# Step 2: Sum the total of unique cluster_id for each Phase
total_by_Phase <- sum_by_ID_Phase %>%
  group_by(Phase) %>%
  summarise(total_unique_clusters = sum(unique_clusters))

# Step 3: Calculate site fidelity by phase
# Calculate unique_clusters and total_count for each ID-Phase combination
sum_by_ID_Phase <- network_final %>%
  group_by(ID, Phase) %>%
  summarise(unique_clusters = n_distinct(cluster_id),
            total_count = n())

# Calculate the total unique_clusters, total count, and ratio for each individual (ID)
total_by_ID <- sum_by_ID_Phase %>%
  group_by(ID,Phase) %>%
  summarise(total_unique_clusters = sum(unique_clusters),
            total_count = sum(total_count)) %>%
  mutate(ratio_unique_to_total = 1 - (total_unique_clusters / total_count))

# Calculate the mean and standard deviation of ratio_unique_to_total by phase
summary_by_Phase <- total_by_ID %>%
  group_by(Phase) %>%
  summarise(mean_ratio_unique_to_total = mean(ratio_unique_to_total),
            sd_ratio_unique_to_total = sd(ratio_unique_to_total))

# Calculate Total site fidelity
# Calculate unique_clusters and total_count for each ID-Phase combination
sum_by_ID <- network_final %>%
  group_by(ID) %>%
  summarise(unique_clusters = n_distinct(cluster_id),
            total_count = n())

# Calculate the total unique_clusters, total count, and ratio for each individual (ID)
total_by_ID_Total <- sum_by_ID %>%
  group_by(ID) %>%
  summarise(total_unique_clusters = sum(unique_clusters),
            total_count = sum(total_count)) %>%
  mutate(ratio_unique_to_total = 1 - (total_unique_clusters / total_count))

# Calculate the mean and standard deviation of ratio_unique_to_total by phase
summary_by_ID <- total_by_ID_Total %>%
  summarise(mean_ratio_unique_to_total = mean(ratio_unique_to_total),
            sd_ratio_unique_to_total = sd(ratio_unique_to_total),
            range = range(ratio_unique_to_total))

# Calculate the average distance and standard deviation for each phase
summary_distance_ID <- network_final %>%
  group_by(ID) %>%
  summarise(average_distance_meters = mean(distance_meters, na.rm = TRUE))

continue_summary_dis_ID <- summary_distance_ID %>%
  summarise(average_distance_total = mean(average_distance_meters, na.rm = TRUE),
            sd_dis = sd(average_distance_meters, na.rm = TRUE),
            range_dis = range(average_distance_meters, na.rm = TRUE))

# Calculate summaries by hub and satellite
hub_summary <- unique_rows %>%
  summarise(average_closeness = mean(closeness, na.rm = TRUE),
            sd_closeness = sd(closeness, na.rm = TRUE),
            med_closeness = median(closeness, na.rm=TRUE),
            range_closeness = range(closeness, na.rm = TRUE),
            average_between = mean(betweeness, na.rm = TRUE),
            sd_betweeness = sd(betweeness, na.rm = TRUE),
            med_betweeness = median(betweeness, na.rm=TRUE),
            range_betweeness = range(betweeness, na.rm = TRUE),
            average_degree = mean(degree, na.rm = TRUE),
            sd_degree = sd(degree, na.rm = TRUE),
            med_degree = median(degree, na.rm=TRUE),
            range_degree = range(degree, na.rm = TRUE),
            average_clusterco = mean(clustering_coefficient, na.rm = TRUE),
            sd_clusterco = sd(clustering_coefficient, na.rm = TRUE),
            med_clusterco = median(clustering_coefficient, na.rm=TRUE),
            range_clusterco = range(clustering_coefficient, na.rm = TRUE),
            average_central = mean(centrality.centralization, na.rm = TRUE),
            sd_central = sd(centrality.centralization, na.rm = TRUE),
            med_central = median(centrality.centralization, na.rm=TRUE),
            range_central = range(centrality.centralization, na.rm = TRUE))
summary(unique_rows)
##################### Run model for metrics ###################################
library(glmmTMB)
install.packages("TMB")
cc <- subset(unique_rows, Site=="CC")
unique(cc$ID)
indiv <- subset(cc, ID==47469)

write.csv(indiv, "C:/Users/chamberlinlab/Desktop/Roost_Chapter/47469_hubs.csv")
# Make hubs and satellites 1 and 0s
unique_rows[unique_rows$type == "Hub", "type"] <- 1
unique_rows[unique_rows$type == "Non-Hub", "type"] <- 0

# Make type column numeric
unique_rows$type <- as.numeric(unique_rows$type)

# Standardize covariates
unique_rows$closeness.s <- scale(unique_rows$closeness) 
unique_rows$betweeness.s <- scale(unique_rows$betweeness)
unique_rows$degree.s <- scale(unique_rows$degree)
unique_rows$clustering_coefficient.s <- scale(unique_rows$clustering_coefficient)
unique_rows$centrality.centralization.s <- scale(unique_rows$centrality.centralization)

# Compute Pearson correlation coefficients
cor(unique_rows[, 47:51], use = "complete.obs")

# Null model
modspnet <- glmmTMB(type ~ closeness.s + betweeness.s + clustering_coefficient.s +
                    centrality.centralization.s,
                    family = binomial(), data = unique_rows)
summary(modspnet)
summary(unique_rows)
unique_rows %>% group_by(type) %>%
  summarise(mean = mean(clustering_coefficient, na.rm=TRUE),
         std = sd(clustering_coefficient, na.rm=TRUE))
################ plot predictions
library(ggeffects)
library(ggpubr)
library(gridExtra)
table(unique_rows$type)
bet_pred <- ggpredict(modspnet, terms = c("betweeness.s[all]"))
plot(bet_pred)

bet_pred$betweenness_unscaled <- bet_pred$x*sd(unique_rows$betweeness) + mean(unique_rows$betweeness)

bet_plot <- ggplot(data=bet_pred, aes(x=betweenness_unscaled, y=predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Betweenness") + theme_classic()+
  theme(axis.title.y = element_blank()) # Remove y-axis title
bet_plot

clus_pred <- ggpredict(modspnet, terms = c("clustering_coefficient.s[all]"))
plot(clus_pred)

clus_pred$clustering_coefficient_unscaled <- clus_pred$x*sd(unique_rows$clustering_coefficient, na.rm=TRUE) + 
  mean(unique_rows$clustering_coefficient, na.rm=TRUE)

clus_plot <- ggplot(data=clus_pred, aes(x=clustering_coefficient_unscaled, y=predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Clustering coefficient") + theme_classic()+
  theme(axis.title.y = element_blank()) # Remove y-axis title
clus_plot

cen_pred <- ggpredict(modspnet, terms = c("centrality.centralization.s[all]"))
plot(cen_pred)

cen_pred$centrality.centralization_unscaled <- cen_pred$x*sd(unique_rows$centrality.centralization, na.rm=TRUE) + 
  mean(unique_rows$centrality.centralization, na.rm=TRUE)

cen_plot <- ggplot(data=cen_pred, aes(x=centrality.centralization_unscaled, y=predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Eigenvalue centrality") +theme_classic()+
  theme(axis.title.y = element_blank()) # Remove y-axis title
cen_plot

closeness_pred <- ggpredict(modspnet, terms = c("closeness.s[all]"))
plot(closeness_pred)

closeness_pred$closeness_unscaled <- closeness_pred$x*sd(unique_rows$closeness, na.rm=TRUE) + 
  mean(unique_rows$closeness, na.rm=TRUE)

closeness_plot <- ggplot(data=closeness_pred, aes(x=closeness_unscaled, y=predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Closeness") +
  theme_classic() +
  theme(axis.title.y = element_blank()) # Remove y-axis title
closeness_plot
# Use ggarrange to arrange the plots in a 1x2 grid layout
arranged_plots <- grid.arrange(bet_plot, closeness_plot, clus_plot, cen_plot)

# Add a centered y-axis label using a raw grob
y_label <- text_grob("Probability of hub", rot = 90)
y_label_no_grid <- y_label +
  theme(text = element_text(vjust = 0.5, hjust = 0.5, angle = 0))  # Center the text

# Combine the arranged plots with the centered y-axis label
final_plot <- arranged_plots +
  annotation_custom(grob = y_label_no_grid, ymin = 0, ymax = 1, xmin = 0, xmax = 1)

# Create a label for the y-axis title
y_axis_title <- "Probability of hub"

# Create an empty plot with the y-axis title rotated and positioned to the left
empty_plot <- ggplot(data.frame(x = c(0), y = c(1))) +
  geom_text(aes(x, y, label = y_axis_title), vjust = 1, hjust = 0.5, angle = 90) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0.5, 0, 0), "lines"))  # Adjust left margin

# Arrange the empty plot (y-axis title) and the original plots in a 1x2 grid layout using ggarrange
final_plot <- ggarrange(empty_plot, arranged_plots, ncol = 2, widths = c(0.15, 2, 0.5))

final_plot
########################### Scale free network #################################

# Calculate the degree distribution
degree_dist <- data.frame(Degree = unique_rows$degree) %>%
  group_by(Degree) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency))

# Create a log-log plot of the degree distribution
degree_plot <- ggplot(data = degree_dist, aes(x = Degree, y = Proportion)) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  geom_point() +
  labs(x = "Degree", y = "Proportion") +
  theme_minimal()

# Display the degree plot
degree_plot

# Plot metrics based on hub and satellite sites
no_dups_roost$b
# Create the boxplot using ggplot2
ggplot(no_dups_roost, aes(x = type, y = betweenness)) +
  geom_boxplot() +
  xlab("Type") +
  ylab("Centrality") +
  ggtitle("Centrality Boxplot") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

