DATA_FILE = 'annotated_exp_data.csv'

# Load the data from file
raw.data <- read.csv(DATA_FILE)

# Group by participant with calculated score
# UglyHack (TM). If you find better way to get the participants table please
# update this!
df <- merge(
  aggregate(
    cbind(language, test) ~ participant,
    raw.data,
    FUN=function(x) (as.factor(unique(x)))
  ),
  aggregate(
    correct ~ participant,
    raw.data,
    FUN=function(x) (mean(x) * 36)
  )
)
print(df)

cat("\nScores on Test 1 vs. 2 should be similar (t-test should be non-significant)\n")
t.test(correct ~ test, df)

cat("\nScores on Language 1 should be better than chance (one-sample t-test compared to 50%)\n")
t.test(df$correct[df$language == 1], mu=18)

cat("\nScores on Language 2 should be better than chance (one-sample t-test compared to 50%)\n")
t.test(df$correct[df$language == 2], mu=18)

cat("\nScores on Language 1 vs. 2 should be similar (t-test, should be non-significant)\n")
t.test(correct ~ language, df)
