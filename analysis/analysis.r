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

# 1. Are the results similar for Test 1 and Test 2, for each language?
# 1.a. First, test for distributions of Test 1 and Test 2 data in Language 1 for normality.
shapiro.test(df$correct[df$language == 1 & df$test == 1])
shapiro.test(df$correct[df$language == 1 & df$test == 2])
# 1.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.
t.test(correct ~ test, df, subset=(df$language == 1))
# If p >= .05, no difference between Test 1 and Test 2 data in Language 1, as expected.
# 1.c. Now, test for distributions of Test 1 and Test 2 data in Language 2 for normality.
shapiro.test(df$correct[df$language == 2 & df$test == 1])
shapiro.test(df$correct[df$language == 2 & df$test == 2])
# 1.d. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.
t.test(correct ~ test, df, subset=(df$language == 2))
# If p >= .05, no difference between Test 1 and Test 2 data in Language 2, as expected.
# 2. Did people perform better than chance on Language 1?

# 2.a. First, test distribution of Language 1 data for normality.
shapiro.test(df$correct[df$language == 1])
# 2.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. 
t.test(df$correct[df$language == 1], mu=18)
# If p < .05, people performed better than chance

# 3. Did people perform better than chance on Language 2?

# 3.a. First, test distribution of Language 2 data for normality.
shapiro.test(df$correct[df$language == 2])
# 3.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. 
t.test(df$correct[df$language == 2], mu=18)
# If p < .05, people performed better than chance

# 4. TO DO - Are scores on individual words better than chance, for Language 1?

# 5. TO DO - Are scores on individual words better than chance, for Language 1?

# 6. Are the results similar for Language 1 and Language 2?

# 6.a. First, test for distributions of Language 1 and Language 2 data for normality.
shapiro.test(df$correct[df$language == 1])
shapiro.test(df$correct[df$language == 2])
# 6.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.
t.test(correct ~ language, df)
# If p >= .05, no difference between Language 1 and Language 2 data, as expected.

# 7. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 1.

# 8. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 2.

# 9. TO DO - Logistic regression

# 10. TO DO - Scatter plot for transitional probability vs. average score for each word

# 11. How do results compare with Saffran's linguistics study?
# 3.a. First, test distribution of overall scores for normality.
shapiro.test(df$correct)
# 3.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. 
t.test(df$correct, mu=27.2)
# If p >= .05, results are similar to linguistics study.
