# INF4000: Data Visualisation Assignment
# Information School, University of Sheffield
#===============================================================================
# Music visualisation 
# Dataset: MusicOset (2018)
# This script performs data visualisation of music in 2018 of using the MusicOset dataset.
#===============================================================================
# Close any existing graphical devices
dev.off() 

# Install packages 
install.packages("GGally")
install.packages("ggcorrplot")
install.packages("tidyverse")
install.packages("treemapify")
install.packages("webr")

# Import libraries
library(corrplot)
library(dplyr)
library(GGally)
library(ggcorrplot)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(treemapify)
library(viridis)
library(webr)

# Import song dataset
library(readxl)
song_description <- read_excel("song_selected.xlsx")
View(song_description)

################################################################################
# Data preparation

# Select song during 2008 to 2018 (for INF6027 assignment)
start_date <- as.Date("2008-01-01")
end_date <- as.Date("2018-12-31")

song_filtered <- song_description[song_description$release_date >= start_date & 
                                    song_description$release_date <= end_date,]

# Check Duplication song
song_edited <- song_filtered[!duplicated(song_filtered$tracks.song_id),]

# Check data structure
str(song_edited)

# Check missing values
colSums(is.na(song_edited))

# Remove rows with missing values
song_cleaned <- na.omit(song_edited)

# Select song in 2018 (for INF4000 assignment)
year18_startdate <- as.Date("2018-01-01")
year18_enddate <- as.Date("2018-12-31")

song_2018 <- song_cleaned[song_cleaned$release_date >= year18_startdate & 
                            song_cleaned$release_date <= year18_enddate,]

################################################################################
# Pie-Donut Chart: using webr package

# Create categories for proportion calculation
song_pop_grouped <- cut(song_2018$songs.popularity, 
                        breaks = c(0, 20, 40, 60, 80, 100), 
                        labels = c("0-20", "21-40", "41-60", "61-80", "81-100"), 
                        right = FALSE)

## Create Pie-donut chart
song_2018$song_pop_grouped <- cut(song_2018$songs.popularity, 
                                  breaks = c(0, 20, 40, 60, 80, 100), 
                                  labels = c("0-20", "21-40", "41-60", "61-80", "81-100"), 
                                  right = FALSE)

song_result <- song_2018 %>%
  group_by(song_pop_grouped, song_pop.is_pop) %>%
  summarise(count = n()) %>%
  spread(song_pop.is_pop, count, fill = 0)

song_result_long <- song_result %>%
  pivot_longer(cols = c(`TRUE`, `FALSE`),   
               names_to = "type",           
               values_to = "frequency")     

colnames(song_result_long) <- c("Group", "Popularity", "Frequency")

# Plot a pie-donut chart
PieDonut(song_result_long, 
         aes(Popularity, Group, count = Frequency),
         title = "Popularity Distribution Among Groups",
         explode= 2, 
         labelposition = 1, 
         ratioByGroup = FALSE,
         explodeDonut = FALSE,
         pieLabelSize = 4,
         donutLabelSize = 3, 
         titlesize = 6
         )

################################################################################
# Tree map

# Create a frequency table and convert it to a data frame
name_freq_2018 <- as.data.frame(table(song_2018$artists.name))

# Plot a basic treemap
ggplot(name_freq_2018, aes(area = Freq, fill = Freq, label = Var1)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "center", grow = TRUE) +
  labs(title = "Distribution of Artist Frequency in 2018",
       fill = "Frequency",
       caption = "Data Source: MusicOset Dataset") +
  theme_void() +
  scale_fill_viridis_c(option = "plasma")

# Customised Treemap
ggplot(name_freq_2018, aes(area = Freq, fill = Freq, label = Var1)) +
  geom_treemap(color = "white", lwd = 0.2) +  # Add borders with thin white lines
  geom_treemap_text(
    colour = "white", 
    place = "center", 
    grow = TRUE,
    reflow = TRUE  # Adjust text to fit better
  ) +
  labs(
    title = "Artist Frequency Distribution in 2018",
    subtitle = "Representation of artists based on their frequency of occurrence",
    caption = "Data Source: MusicOset Dataset",
    fill = "Frequency"
  ) +
  scale_fill_viridis_c(
    option = "plasma", 
    begin = 0.2, 
    end = 0.9  # Use a subset of the color palette for better contrast
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Bold title
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),  # Subtitle
    plot.caption = element_text(size = 10, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.4, "cm")
  )

################################################################################
# Parallel coordinate 

# Plot a basic chart
para_2018 <- ggparcoord(song_2018, columns=c(19:31), 
                        groupColumn = 16, 
                        alphaLines = 0.2, 
                        showPoints = TRUE
                        ) +
  theme_bw() +
  facet_grid(. ~ song_pop.is_pop) +
  labs(colour='Popularity', 
       x='Features', 
       y='Scaled values',
       title='Song features by popularity in 2018',
       caption = "Data Source: MusicOset Dataset"
       ) +
  scale_color_viridis_c() +
  coord_flip()

# Customised chart
ggparcoord(song_2018, 
           columns = c(19:31), 
           groupColumn = 16, 
           alphaLines = 0.1, 
           showPoints = TRUE
           ) +
  theme_bw() +
  facet_grid(. ~ song_pop.is_pop, scales = "free_x", space = "free") +
  labs(
    colour = 'Popularity', 
    x = 'Features', 
    y = 'Scaled Values',
    title = 'Song Features by Popularity in 2018',
    caption = "Data Source: MusicOset Dataset"
  ) +
  scale_color_viridis_c(option = "plasma") +
  coord_flip() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12)
  )

################################################################################
# Scatter plot: Valence vs Energy vs Popularity

ggplot(song_2018, aes(acoustic_features.valence, acoustic_features.energy, colour = songs.popularity)) +
  geom_jitter(aes(size = song_chart.weeks_on_chart)) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(title = "",
       x = "Valance",
       y ="Energy",
       colour = "Popularity score",
       caption = "Data Source: MusicOset Dataset") +
  annotate('text', 0.25 / 2, 0.95, label = "Turbulent/Angry", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Happy/Joyful", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Chill/Peaceful", fontface = "bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Sad/Depressing", fontface = "bold")

# Customised chart
ggplot(song_2018, aes(acoustic_features.valence, acoustic_features.energy, colour = songs.popularity)) +
  geom_jitter(aes(size = song_chart.weeks_on_chart), alpha = 0.7, shape = 16) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Relationship Between Valence and Energy in Songs (2018)",
    subtitle = "Colored by Popularity Score and Sized by Weeks on Chart",
    x = "Valence (Happiness/Positivity)",
    y = "Energy (Intensity/Dynamism)",
    colour = "Popularity Score",
    size = "Weeks on Chart",
    caption = "Data Source: MusicOset Dataset"
  ) +
  annotate('text', x = 0.1, y = 0.9, label = "Turbulent/Angry", fontface = "bold", size = 4, color = "blue") +
  annotate('text', x = 0.9, y = 0.9, label = "Happy/Joyful", fontface = "bold", size = 4, color = "blue") +
  annotate('text', x = 0.9, y = 0.1, label = "Chill/Peaceful", fontface = "bold", size = 4, color = "blue") +
  annotate('text', x = 0.1, y = 0.1, label = "Sad/Depressing", fontface = "bold", size = 4, color = "blue") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  guides(
    colour = guide_colorbar(barwidth = 1, barheight = 5),
    size = guide_legend(title = "Weeks on Chart", override.aes = list(alpha = 1))
  )

################################################################################
# Perform XGBoost model to analyse feature importance related to song popularity group.

install.packages("caret")
install.packages("randomForest")
install.packages("xgboost")
install.packages("pROC")
install.packages("Metrics")

# Load libraries
library(xgboost)
library(caret)  
library(Matrix)  
library(pROC)

selected_data <- song_2018 %>%
  select(
    song_pop.is_pop,  
    acoustic_features.duration_ms, 
    acoustic_features.key, 
    acoustic_features.mode,
    acoustic_features.time_signature, 
    acoustic_features.acousticness, 
    acoustic_features.danceability,
    acoustic_features.energy, 
    acoustic_features.instrumentalness, 
    acoustic_features.liveness,
    acoustic_features.loudness,
    acoustic_features.speechiness,
    acoustic_features.valence,
    acoustic_features.tempo
  )

# Convert target variable to binary factor
selected_data$song_pop.is_pop <- as.factor(ifelse(selected_data$song_pop.is_pop == TRUE, 1, 0))

# Split data into training and testing sets
train_index <- createDataPartition(selected_data$song_pop.is_pop, p = 0.7, list = FALSE)
train_data <- selected_data[train_index, ]
test_data <- selected_data[-train_index, ]

# Normalize numerical features
preprocess <- preProcess(train_data[, -1], method = c("center", "scale"))

train_normalized <- train_data
train_normalized[, -1] <- predict(preprocess, train_data[, -1])

test_normalized <- test_data
test_normalized[, -1] <- predict(preprocess, test_data[, -1])

# Convert data to matrix for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_normalized[, -1]), 
                            label = as.numeric(train_normalized$song_pop.is_pop) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(test_normalized[, -1]), 
                           label = as.numeric(test_normalized$song_pop.is_pop) - 1)

xgb_model <- xgboost(data = train_matrix, 
                     max_depth = 6, 
                     eta = 0.3, 
                     nrounds = 100, 
                     objective = "binary:logistic",
                     importance_type = "gain",
                     verbose = 0)

# Predictions and Evaluation for XGBoost
xgb_preds <- predict(xgb_model, test_matrix)
xgb_class <- ifelse(xgb_preds > 0.5, 1, 0)
xgb_accuracy <- mean(xgb_class == as.numeric(test_normalized$song_pop.is_pop) - 1)
cat("XGBoost Accuracy:", xgb_accuracy, "\n")

xgb_conf_matrix <- confusionMatrix(as.factor(xgb_class), 
                                   as.factor(test_normalized$song_pop.is_pop))
cat("XGBoost Metrics:\n")
print(xgb_conf_matrix)

# ROC 
roc_xgb <- roc(as.numeric(test_normalized$song_pop.is_pop) - 1, xgb_preds)
plot(roc_xgb, col = "steelblue", main = "ROC Curves")

# Calculate Feature Importance
importance_matrix <- xgb.importance(model = xgb_model)

# View Feature Importance
print(importance_matrix)

# Visualize Feature Importance
# xgb.plot.importance(importance_matrix, main = "XGBoost Feature Importance")

importance_data <- as.data.frame(importance_matrix)

library(viridis)
imp_bar_plot <- ggplot(importance_data, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(option = "C", direction = -1) +  # Use the Viridis palette
  coord_flip() +
  theme_bw() +
  labs(
    title = "XGBoost Feature Importance",
    x = "Features",
    y = "Gain",
    caption = "Data Source: MusicOset Dataset") +
  geom_text(aes(label = round(Gain, 2)), hjust = -0.2)

print(imp_bar_plot)

# Customised chart
ggplot(importance_data, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_viridis(option = "plasma", direction = -1) +
  coord_flip(clip = "off") +
  theme_minimal() +
  labs(
    title = "XGBoost Feature Importance",
    subtitle = "Top features influencing song popularity",
    x = "Features",
    y = "Importance (Gain)",
    caption = "Data source: MusicOset dataset"
  ) +
  geom_text(aes(label = round(Gain, 2)), hjust = -0.3, size = 3.5, color = "black") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Make interactive plot using Plotly
install.packages("plotly")
library(plotly)
ggplotly(imp_bar_plot)


################################################################################

