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

cat("\n1. Are the results similar for Test 1 and Test 2, for each language?\n")
cat("\n1.a. First, test for distributions of Test 1 and Test 2 data in Language 1 for normality.\n")
shapiro.test(df$correct[df$language == 1 & df$test == 1])
shapiro.test(df$correct[df$language == 1 & df$test == 2])
cat("\n1.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ test, df, subset=(df$language == 1))
cat("\nIf p >= .05, no difference between Test 1 and Test 2 data in Language 1, as expected.\n")
cat("\n1.c. Now, test for distributions of Test 1 and Test 2 data in Language 2 for normality.\n")
shapiro.test(df$correct[df$language == 2 & df$test == 1])  # FIXME this one ended significant!
shapiro.test(df$correct[df$language == 2 & df$test == 2])
cat("\n1.d. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ test, df, subset=(df$language == 2))
cat("\nIf p >= .05, no difference between Test 1 and Test 2 data in Language 2, as expected.\n")

cat("\n2. Did people perform better than chance on Language 1?\n")

cat("\n2.a. First, test distribution of Language 1 data for normality.\n")
shapiro.test(df$correct[df$language == 1])
cat("\n2.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct[df$language == 1], mu=18)
cat("\nIf p < .05, people performed better than chance\n")

cat("\n3. Did people perform better than chance on Language 2?\n")

cat("\n3.a. First, test distribution of Language 2 data for normality.\n")
shapiro.test(df$correct[df$language == 2])
cat("\n3.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct[df$language == 2], mu=18)
cat("\nIf p < .05, people performed better than chance\n")

# 4. TO DO - Are scores on individual words better than chance, for Language 1?

# 5. TO DO - Are scores on individual words better than chance, for Language 1?

cat("\n6. Are the results similar for Language 1 and Language 2?\n")

cat("\n6.a. First, test for distributions of Language 1 and Language 2 data for normality.\n")
shapiro.test(df$correct[df$language == 1])
shapiro.test(df$correct[df$language == 2])
cat("\n6.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ language, df)
cat("\nIf p >= .05, no difference between Language 1 and Language 2 data, as expected.\n")

# 7. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 1.

# 8. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 2.

# 9. TO DO - Logistic regression

# 10. TO DO - Scatter plot for transitional probability vs. average score for each word

cat("\n11. How do results compare with Saffran's linguistics study?\n")
cat("\n3.a. First, test distribution of overall scores for normality.\n")
shapiro.test(df$correct)
cat("\n3.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct, mu=27.2)
cat("\nIf p >= .05, results are similar to linguistics study.\n")
