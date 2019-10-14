library(readr)
library(dplyr)
library(knitr)
library(ggpubr)
books <- read_csv("STAT_Assignment_4/books.csv")
View(books)
my_data$group <- ordered(my_data$group,
                         levels = c("Clancy", "Rowling", "Tolstoy"))

# (a) Side by side box plots
boxplot(books$score ~ books$author, main="Boxplot by Authors")

# (b) mean and standard deviation 
# Method 1:
# Calculating mean
tapply(books$score, INDEX = books$author, FUN = mean)
# Calculating standard deviation
tapply(books$score, INDEX = books$author, FUN = sd)
# Method 2:
group_by(books, author) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )
# 95% confidence intervals for mean for three levels separately
tapply(books$score, INDEX = books$author, FUN = t.test)

# One way ANOVA
res.aov <- aov(score ~ author, data = books)
# Summary of the analysis
summary(res.aov)

# Draw histogram
tapply(books$score, INDEX = books$author, FUN = hist)

tapply(books$score, INDEX = books$author, FUN = qqnorm)
tapply(books$score, INDEX = books$author, FUN = qqline)
