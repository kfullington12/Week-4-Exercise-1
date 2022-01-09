install.packages("pastecs")
library(pastecs)
library(ggplot2)
library(data.table)

scores_df <- read.csv("data/scores.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# What are the observational units in this study?
str(scores_df)
nrow(scores_df)
ncol(scores_df)
# The obersvational units in the study are count and score. The columns are "Count" "Score" and "Section"
# The rows are the measures for each column

# Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
# The variables under "Count" are quantitative. The variables under "Score" are quantitative.
# The variables under "Section" are categorical. 

# Create one variable to hold a subset of your data set
# that contains only the Regular Section and one variable for the Sports Section.

scores_table <- data.table(scores_df)
setkey(scores_table, Section)

regular_scores <- subset(scores_df, scores_df$Section=="Regular")
sports_scores <- subset(scores_df, scores_df$Section=="Sports")


# Use the Plot function to plot each Sections scores and the number of students achieving that score. 
# Use additional Plot Arguments to label the graph and give each axis an appropriate label

hist_scores_regular <- ggplot(regular_scores, aes(Score)) + geom_histogram(aes(y = ..density..), 
                            fill = "white", color = "black", binwidth = 20) + labs(title = "Scores in Regular Section", x = "Score", y = "Density")
hist_scores_sports <- ggplot(sports_scores, aes(Score)) + geom_histogram(aes(y = ..density..), 
                            fill = "white", color = "black", binwidth = 45) + labs(title = "Scores in Sports Section", x = "Score", y = "Density")
# Can you say that one section tended to score more points than the other? Justify and explain your answer.
#   Students in the Sports section tended to score a little higher than those in the Regular section.
#   You can see this in the negative skew of the sports section, where more values are concentrated in the upper half of the graph.
by(scores_df[, c("Score", "Count")], scores_df$Section, stat.desc, basic = FALSE, norm = TRUE)
#   This shows us the skew and kurtosis by section, among other statistics
#   The skew and kurtosis in the regular section are not significant, implying normal distribution
#   The skew in the sports section is not significant, but the kurtosis in the sports section is significant
#   The negative skew of scores in the regular section indicate that most students scored on the higher end
#   The negative skew of scores in the sports section indicates that most students scored on the higher end
#   Looking at the graphs though, the the scores in the sports section are concentrated around a higher point than
#       the regular section.
# Did every student in one section score more points than every student in the other section?
# If not, explain what a statistical tendency means in this context.
#   No, every student in one section did not score higher than every student in the other section.
qplot(sample = scores_df$Score, stat = "qq")
#   These are signs that the distribution of the two sections is normal - there's a range of values.
# What could be one additional variable that was not mentioned in the narrative
# that could be influencing the point distributions between the two sections?
#   One additional variable that could be influencing the point distributions is whether the professor was the same
#   Also, what type of reinforcement the professor used could impact the scores
