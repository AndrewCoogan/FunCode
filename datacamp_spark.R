## 1

# Load sparklyr
library(sparklyr)

# Connect to your Spark cluster
spark_conn <- spark_connect(master = "local")

# Print the version of Spark
print(spark_version(sc = spark_conn))

# Disconnect from Spark
spark_disconnect(sc = spark_conn)

## 2

# Load dplyr
library(dplyr)

# Explore track_metadata structure
str(track_metadata)

# Connect to your Spark cluster
spark_conn <- spark_connect(master = "local")

# Copy track_metadata to Spark
track_metadata_tbl <- copy_to(dest = spark_conn, df = track_metadata, pass_overwrite = TRUE)

# List the data frames available in Spark
src_tbls(spark_conn)

# Disconnect from Spark
spark_disconnect(sc = spark_conn)

## 3

# Link to the track_metadata table in Spark
track_metadata_tbl <- tbl(spark_conn, "track_metadata")

# See how big the dataset is
dim(track_metadata_tbl)

# See how small the tibble is
object_size(track_metadata_tbl)

## 4

# Print 5 rows, all columns
print(track_metadata_tbl, n = 5, width = Inf)

# Examine structure of tibble
str(track_metadata_tbl)

# Examine structure of data
glimpse(track_metadata_tbl)

## 5 (forgot)

## 6 

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year) %>%
  # Filter rows
  filter(year >= 1960, year < 1970) %>%
  # Arrange rows
  arrange(artist_name, desc(year), title)


## 7

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(title, duration) %>%
  # Mutate columns
  mutate(duration_minutes = duration / 60)

###### Second level

## 1
dplyr::starts_with
dplyr::ends_with

## 2

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Select columns containing ti
  select(contains("ti"))

track_metadata_tbl %>%
  # Select columns matching ti.?t
  select(matches("ti.?t"))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Only return rows with distinct artist_name
  distinct(artist_name)


# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Count the artist_name values
  count(artist_name, sort = TRUE) %>%
  # Restrict to top 20
  top_n(20)


## 6

# track_metadata_tbl has been pre-defined
track_metadata_tbl

results <- track_metadata_tbl %>%
  # Filter where artist familiarity is greater than 0.9
  filter(artist_familiarity > 0.9)

# Examine the class of the results
class(results)

# Collect your results
collected <- results %>%
  collect

# Examine the class of the collected results
class(collected)


## 8

# track_metadata_tbl has been pre-defined
track_metadata_tbl

duration_by_artist <- track_metadata_tbl %>%
  # Group by artist
  group_by(artist_name) %>%
  # Calc mean duration
  summarise(mean_duration = mean(duration))

duration_by_artist %>%
  # Sort by ascending mean duration
  arrange(mean_duration)

duration_by_artist %>%
  # Sort by descending mean duration
  arrange(desc(mean_duration))

## 9

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Group by artist
  group_by(artist_name) %>%
  # Calc time since first release
  mutate(time_since_first_release = year - min(year)) %>%
  # Arrange by descending time since first release
  arrange(time_since_first_release)

## 10

# Write SQL query
query <- "SELECT * FROM track_metadata WHERE year < 1935 AND duration > 300"

# Run the query
(results <- dbGetQuery(spark_conn, query))


###### Third level

# track_metadata_tbl has been pre-defined
track_metadata_tbl

hotttnesss <- track_metadata_tbl %>%
  # Select artist_hotttnesss
  select(artist_hotttnesss) %>%
  # Binarize to is_hottt_or_nottt
  ft_binarizer("artist_hotttnesss", "is_hottt_or_nottt", threshold = 0.5) %>%
  # Collect the result
  collect() %>%
  # Convert is_hottt_or_nottt to logical
  mutate(is_hottt_or_nottt = as.logical(is_hottt_or_nottt))

# Draw a barplot of is_hottt_or_nottt
ggplot(hotttnesss, aes(is_hottt_or_nottt)) +
  geom_bar()


# track_metadata_tbl, decades, decade_labels have been pre-defined
track_metadata_tbl
decades
decade_labels

hotttnesss_over_time <- track_metadata_tbl %>%
  # Select artist_hotttnesss and year
  select(artist_hotttnesss, year) %>%
  # Convert year to numeric
  mutate(year = as.numeric(year)) %>%
  # Bucketize year to decade using decades vector
  ft_bucketizer("year", "decade", splits = decades) %>%
  # Collect the result
  collect() %>%
  # Convert decade to factor using decade_labels
  mutate(decade = factor(decade, labels = decade_labels))

# Draw a boxplot of artist_hotttnesss by decade
ggplot(hotttnesss_over_time, aes(decade, artist_hotttnesss)) + geom_boxplot()

# track_metadata_tbl, duration_labels have been pre-defined
track_metadata_tbl
duration_labels

familiarity_by_duration <- track_metadata_tbl %>%
  # Select duration and artist_familiarity
  select(duration, artist_familiarity) %>%
  # Bucketize duration
  ft_quantile_discretizer("duration", "duration_bin", 5) %>%
  # Collect the result
  collect() %>%
  # Convert duration bin to factor
  mutate(duration_bin = factor(duration_bin, labels = duration_labels))

# Draw a boxplot of artist_familiarity by duration_bin
ggplot(familiarity_by_duration, aes(duration_bin, artist_familiarity)) +geom_boxplot()  

####### Lesson 3

# responses has been pre-defined
responses

# Draw a scatterplot of predicted vs. actual
ggplot(responses, aes(actual, predicted)) +
  # Add the points
  geom_point(alpha = 0.1) +
  # Add a line at actual = predicted
  geom_abline(slope = 1, intercept = 0)

residuals <- responses %>%
  # Transmute response data to residuals
  transmute(residual =  predicted - actual)

# Draw a density plot of residuals
residuals %>%
  ggplot(aes(residual)) +
  # Add a density curve
  geom_density(alpha = 0.1) +
  # Add a vertical line through zero
  geom_vline(xintercept = 0)

responses <- track_data_to_predict_tbl %>% 
  select(year) %>% 
  collect() %>% 
  mutate(predicted_year = predict(random_forest_model, track_data_to_predict_tbl))


# both_responses has been pre-defined
both_responses

# Draw a scatterplot of predicted vs. actual
ggplot(both_responses, aes(actual, predicted, color = model)) +
  # Add a smoothed line
  geom_smooth(alpha = 0.1) +
  # Add a line at actual = predicted
  geom_abline(intercept = 0, slope = 1)

# Create a tibble of residuals
residuals <- both_responses %>% mutate(residual =  predicted - actual)

# Draw a density plot of residuals
ggplot(residuals, aes(residual, color = model)) +
  # Add a density curve
  geom_density() +
  # Add a vertical line through zero
  geom_vline(xintercept = 0)


both_responses %>% 
  mutate(residual = predicted - actual) %>% 
  group_by(model) %>% 
  summarise(rmse = sqrt(mean(residual^2)))