# ── 1. Load required libraries ───────────────────────────────
# install.packages("readxl")
# install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(dplyr)


enhanced_student_habits_performance_dataset <- read_excel("C:/Users/josh/Downloads/enhanced_student_habits_performance_dataset.xlsx")
View(enhanced_student_habits_performance_dataset)


#-----UNIVARIATE ANALYSIS------

#FUNCTION 1
#Analyzaton using freuency
analyze_major <- function(data) {
  freq_tbl <- data %>%
    count(major, name = "frequency") %>%
    mutate(
      relative_freq = frequency / sum(frequency),
      percentage    = round(relative_freq * 100, 2)
      ) %>%
      arrange(desc(frequency)
    )
  
  cat("\nFrequency Table:\n")
  print(as.data.frame(freq_tbl))
  
  mode_major <- freq_tbl$major[1]
  mode_freq  <- freq_tbl$frequency[1]
  cat(sprintf(
    "\nMode (Most Common Major): %s  |  Frequency: %d  |  %.2f%%\n",
      mode_major, mode_freq,
      freq_tbl$percentage[1])
  )
  
  #Q1 Presentation in ggplot
  p <- ggplot(freq_tbl, aes(x = reorder(major, -frequency), y = frequency, fill = major)) +
    geom_bar(stat = "identity", width = 0.7, color = "white") +
    geom_text(aes(label = comma(frequency)), vjust = -0.4, size = 3.2) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.10))) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = "Q1: Most Common Field of Study Among Students",
      subtitle = sprintf("Mode = %s  (n = %s)", mode_major, comma(mode_freq)),
      x        = "Major",
      y        = "Number of Students",
      fill     = "Major"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      axis.text.x   = element_text(angle = 30, hjust = 1),
      legend.position = "none"
    )
  print(p)          
  invisible(freq_tbl)
}

analyze_major(enhanced_student_habits_performance_dataset)

#FUNCTION2
  #Analyzation using mean
  analyze_sleep_hours <- function(data) {
    #first, we extract the data and coerce it to numeric
    sleep <- as.numeric(data$sleep_hours) 
    sleep <- sleep[!is.na(sleep)]#if theres NA values, drop the from the calculation
    
    stats <- data.frame(
      Statistic = c("n", "Mean", "Median", "Std Dev", "Min", "Max"),
      Value     = c(
        length(sleep),
        round(mean(sleep),   4),
        round(median(sleep), 4),
        round(sd(sleep),     4),
        round(min(sleep),    4),
        round(max(sleep),    4)
      )
    )
    cat("\nDescriptive Statistics for Sleep Hours:\n")
    print(stats, row.names = FALSE)
    
    mean_val <- mean(sleep)
    cat(sprintf("\nAverage Sleep Hours: %.4f hours\n", mean_val)
  ) 
  
  #Presentation usng histogram in ggplot
  p <- ggplot(data.frame(sleep_hours = sleep), aes(x = sleep_hours)) +
    geom_histogram(binwidth = 0.5, fill = "cornflowerblue", color = "white", alpha = 0.85) +
    geom_vline(xintercept = mean_val, color = "firebrick", linewidth = 1.1,
               linetype = "dashed") +
    annotate("text", x = mean_val + 0.15, y = Inf,
             label = sprintf("Mean = %.2f hrs", mean_val),
             vjust = 2, hjust = 0, color = "firebrick", fontface = "bold", size = 3.5) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = "Q2: Distribution of Student Sleep Hours",
      subtitle = sprintf("Mean = %.4f hours  |  n = %s", mean_val, comma(length(sleep))),
      x        = "Sleep Hours per Night",
      y        = "Number of Students"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p)
  invisible(stats)
}

analyze_sleep_hours(enhanced_student_habits_performance_dataset)



#FUNCTION3
#Analization using frequency
analyze_internet_quality <- function(data) {
  ordinal_levels <- c("Low", "Medium", "High")
  
  freq_tbl <- data %>%
    filter(!is.na(internet_quality)
          ) %>%
    mutate(internet_quality = factor(internet_quality, levels = ordinal_levels)) %>%
    count(internet_quality, name = "frequency") %>%
    mutate(
      percentage = round(frequency / sum(frequency) * 100, 2)
    )
  cat("\nFrequency Table (Ordinal Order):\n")
  print(as.data.frame(freq_tbl))
  
  mode_cat  <- as.character(freq_tbl$internet_quality[which.max(freq_tbl$frequency)])
  mode_freq <- max(freq_tbl$frequency)
  cat(sprintf("\nHighest Frequency Category: %s  |  Frequency: %d  |  %.2f%%\n",
              mode_cat, mode_freq,
              freq_tbl$percentage[which.max(freq_tbl$frequency)]
              )
      )
  #Presentation using bar chart in ggplot
  p <- ggplot(freq_tbl, aes(x = internet_quality, y = frequency,
                            fill = internet_quality)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = sprintf("%s\n(%.2f%%)", comma(frequency), percentage)),
              vjust = -0.4, size = 3.2) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = c("Low" = "tomato",
                                 "Medium" = "goldenrod",
                                 "High"   = "seagreen")) +
    labs(
      title    = "Q3: Distribution of Internet Quality Among Students",
      subtitle = sprintf("Highest Frequency: %s  (n = %s)", mode_cat, comma(mode_freq)),
      x        = "Internet Quality (Ordinal)",
      y        = "Number of Students",
      fill     = "Quality Level"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")
  
  print(p)
  invisible(freq_tbl)
}

analyze_internet_quality(enhanced_student_habits_performance_dataset)


#FUNCTION4

analyze_diet_quality <- function(data) {
  
  ordinal_levels <- c("Poor", "Fair", "Good")
  
  freq_tbl <- data %>%
    filter(!is.na(diet_quality)) %>%
    mutate(diet_quality = factor(diet_quality, levels = ordinal_levels)) %>%
    count(diet_quality, name = "frequency") %>%
    mutate(
      percentage = round(frequency / sum(frequency) * 100, 2)
    )
  
  cat("\nFrequency Table (Ordinal Order):\n")
  print(as.data.frame(freq_tbl)
        ) 
  mode_cat  <- as.character(freq_tbl$diet_quality[which.max(freq_tbl$frequency)])
  mode_freq <- max(freq_tbl$frequency)
  cat(sprintf("\nHighest Frequency Category: %s  |  Frequency: %d  |  %.2f%%\n",
              mode_cat, mode_freq,
              freq_tbl$percentage[which.max(freq_tbl$frequency)]
              )
      )
  #Presentation using bar chart
  p <- ggplot(freq_tbl, aes(x = diet_quality, y = frequency,
                            fill = diet_quality)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    geom_text(aes(label = sprintf("%s\n(%.2f%%)", comma(frequency), percentage)),
              vjust = -0.4, size = 3.2) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = c("Poor" = "tomato",
                                 "Fair" = "goldenrod",  
                                 "Good" = "seagreen")) +
    labs(
      title    = "Q4: Distribution of Diet Quality Among Students",
      subtitle = sprintf("Highest Frequency: %s  (n = %s)", mode_cat, comma(mode_freq)),
      x        = "Diet Quality (Ordinal)",
      y        = "Number of Students",
      fill     = "Diet Quality"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")
  
  print(p)
  invisible(freq_tbl)
}

analyze_diet_quality(enhanced_student_habits_performance_dataset)



#FUNCTION5

analyze_social_media_hours <- function(data) {

  #computing the mean of social media hours
  mean_social <- mean(data$social_media_hours, na.rm = TRUE)
  cat(sprintf("Average Social Media Hours per Day: %.4f hrs\n", mean_social))
  
  #Presentation using histogram on ggplot2
  plot <- ggplot(data, aes(x = social_media_hours)) +
    
    geom_histogram(binwidth = 0.5, fill = "cornflowerblue", color = "white", alpha = 0.9) +
    
    geom_vline(xintercept = mean_social, color = "tomato", linewidth = 1.2, linetype = "dashed") +
    
    annotate("text", x = mean_social + 0.2, y = Inf, vjust = 1.8, hjust = 0,
             label = sprintf("Mean = %.2f hrs", mean_social),
             color = "tomato3", fontface = "bold", size = 4.5) +
    
    labs(
      title   = "Q5: Distribution of Student Social Media Hours per Day",
      x       = "Social Media Hours per Day",
      y       = "Number of Students",
      caption = "Source: Student Habits & Academic Performance Dataset (Kaggle - aryan208)"
    ) +
    
    scale_y_continuous(labels = scales::comma) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 15, color = "black"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
  print(plot)
}

analyze_social_media_hours(enhanced_student_habits_performance_dataset)





#----BIVARIATE ANALYSIS----