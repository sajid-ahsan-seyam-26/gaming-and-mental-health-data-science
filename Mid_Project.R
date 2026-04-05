gaming_data <- read.csv("D:/Study/LAB/Semester-10/Data Science/Project/Gaming and Mental Health.csv", header = TRUE, sep = ",")

#STEP 1:Checking for Missing Values

sum(is.na(gaming_data))

colSums(is.na(gaming_data))

median_gpa <- median(gaming_data$grades_gpa, na.rm = TRUE)

gaming_data$grades_gpa[is.na(gaming_data$grades_gpa)] <- median_gpa


#Step 2:Handling Outliers
num_cols <- c("age", "daily_gaming_hours", "sleep_hours", "grades_gpa", "work_productivity_score", "weight_change_kg", "exercise_hours_weekly", "social_isolation_score",
              "face_to_face_social_hours_weekly", "monthly_game_spending_usd", "years_gaming")

for (col in num_cols) {
  Q1 <- quantile(gaming_data[[col]], 0.25,na.rm = TRUE)
  Q3 <- quantile(gaming_data[[col]], 0.75,na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  count <- sum(gaming_data[[col]] < lower | gaming_data[[col]] > upper)
  cat(col, ":", count, "outliers\n")
}

for (col in num_cols) {
  Q1 <- quantile(gaming_data[[col]], 0.25,na.rm = TRUE)
  Q3 <- quantile(gaming_data[[col]], 0.75,na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  gaming_data[[col]][gaming_data[[col]] < lower] <- lower
  gaming_data[[col]][gaming_data[[col]] > upper] <- upper
}

for (col in num_cols) {
  Q1 <- quantile(gaming_data[[col]], 0.25,na.rm = TRUE)
  Q3 <- quantile(gaming_data[[col]], 0.75,na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  count <- sum(gaming_data[[col]] < lower | gaming_data[[col]] > upper)
  cat(col, ":", count, "outliers\n")
}

#STEP 3:Convert categorical variable to numeric

# Male = 1, Female = 0
gaming_data$gender_num <- ifelse(gaming_data$gender == "Male", 1,
                          ifelse(gaming_data$gender == "Female", 0, NA))

# Convert numeric variable to categorical
gaming_data$age_group <- ifelse(gaming_data$age < 18, "Teen",
                         ifelse(gaming_data$age <= 25, "Young Adult", "Adult"))

print("Converted variables:")
print(head(gaming_data[, c("gender", "gender_num", "age", "age_group")]))


#STEP 4:Normalization of  Continuous Attribute
continuous_cols <- c("daily_gaming_hours", "sleep_hours", "weight_change_kg", "exercise_hours_weekly", "face_to_face_social_hours_weekly",
                     "monthly_game_spending_usd")

print(gaming_data[1:3, continuous_cols])

for (col in continuous_cols) {
  min_val <- min(gaming_data[[col]])
  max_val <- max(gaming_data[[col]])
  gaming_data[[col]] <- (gaming_data[[col]] - min_val) / (max_val - min_val)
}

print(gaming_data[1:3, continuous_cols])

#STEP 5:Removing Duplicate Rows
library(dplyr)

sum(duplicated(gaming_data))
duplicate_removed <- distinct(gaming_data)
duplicate_removed


#STEP 6:FILTERING

mean_work_productivity <- mean(gaming_data$work_productivity_score, na.rm = TRUE)

gaming_data$work_productivity_score[is.na(gaming_data$work_productivity_score)] <- mean_work_productivity

gaming_filtered <- subset(gaming_data, 
                          age >= 10 & age <= 80 &               
                            daily_gaming_hours < 16 &             
                            sleep_hours >= 3 &                    
                            grades_gpa <= 4.0 &                   
                            years_gaming <= age)

#STEP 7:Detect invalid data and handle it

print("Missing values in each column before handling:")
print(colSums(is.na(gaming_data)))

# Invalid age: less than 0
gaming_data$age[gaming_data$age < 0] <- NA

gaming_data$daily_gaming_hours[gaming_data$daily_gaming_hours < 0] <- NA

gaming_data$age[is.na(gaming_data$age)] <- mean(gaming_data$age, na.rm = TRUE)

gaming_data$daily_gaming_hours[is.na(gaming_data$daily_gaming_hours)] <- mean(gaming_data$daily_gaming_hours, na.rm = TRUE)

print("Missing values in each column after handling:")
print(colSums(is.na(gaming_data)))


#STEP 8:converting the imbalanced data set into a balanced data set

table(duplicate_removed$gaming_addiction_risk_level)

balanced_data <- duplicate_removed %>% group_by(gaming_addiction_risk_level) %>% slice_sample(n = 142) %>% ungroup()
balanced_data
table(balanced_data$gaming_addiction_risk_level)


#STEP 9:TESTING AND TRAINING DATASET SPILTING

set.seed(2026)

n_total <- nrow(gaming_data)


train_size <- floor(0.80 * n_total)
train_indices <- sample(seq_len(n_total), size = train_size)

train_set <- gaming_data[train_indices, ]

test_set  <- gaming_data[-train_indices, ]

nrow(train_set) 
nrow(test_set)  

#Step 10:Calculating descriptive statistics and interpreting the results for the numerical variables according to the target classes

target_classes <- unique(gaming_data$gaming_addiction_risk_level)

print(target_classes)

selected_vars <- c("daily_gaming_hours", "sleep_hours", "grades_gpa",
                   "work_productivity_score", "monthly_game_spending_usd")



for (cls in target_classes) {
  subset_data <- subset(gaming_data, gaming_addiction_risk_level == cls)

  cat("Class:", cls, "| Rows:", nrow(subset_data), "\n")

  print(summary(subset_data[, selected_vars]))
}


library(dplyr)

gaming_data %>%
  group_by(gaming_addiction_risk_level) %>%
  summarise(
    sd_daily_gaming   = round(sd(daily_gaming_hours), 4),
    sd_sleep_hours    = round(sd(sleep_hours), 4),
    sd_grades_gpa     = round(sd(grades_gpa), 4),
    sd_productivity   = round(sd(work_productivity_score), 4),
    sd_monthly_spend  = round(sd(monthly_game_spending_usd), 4)
  ) -> sd_by_class

print(sd_by_class)

#STEP 11:Compare average values

male_avg <- mean(gaming_data$daily_gaming_hours[gaming_data$gender == "Male"], na.rm = TRUE)
female_avg <- mean(gaming_data$daily_gaming_hours[gaming_data$gender == "Female"], na.rm = TRUE)

print(paste("Average Daily Gaming Hours for Male:", round(male_avg, 2)))
print(paste("Average Daily Gaming Hours for Female:", round(female_avg, 2)))



if (male_avg > female_avg) {
  print("Male participants spend more average time on gaming than Female participants.")
} else if (female_avg > male_avg) {
  print("Female participants spend more average time on gaming than Male participants.")
} else {
  print("Male and Female participants spend equal average time on gaming.")
}

#STEP 12:Examining and comparing the variability

variability_summary <- balanced_data %>%
  group_by(sleep_quality) %>% 
  summarize(
    IQR = IQR(daily_gaming_hours, na.rm = TRUE),
    Variance = var(daily_gaming_hours, na.rm = TRUE),
    Standard_Deviation = sd(daily_gaming_hours, na.rm = TRUE),
    Minimum = min(daily_gaming_hours, na.rm = TRUE),
    Maximum = max(daily_gaming_hours, na.rm = TRUE),
    Range = max(daily_gaming_hours, na.rm = TRUE) - min(daily_gaming_hours, na.rm = TRUE),
    Count = n()
  )

variability_summary