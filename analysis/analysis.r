DATA_FILE = 'annotated_exp_data.csv'

# Load the data from file
raw.data <- read.csv(DATA_FILE)

cat("\nGroup by participant with calculated score\n")
df <- aggregate(
  raw.data$correct, 
  list(language=raw.data$language, test=raw.data$test, participant=raw.data$participant), 
  FUN=function(x) (mean(x) * 36)
  )
names(df)[names(df)=="x"] <- "correct"
print (df)

cat("\nGroup by participant with calculated score for each word\n")
df2 <- aggregate(
  raw.data$correct, 
  list(word=raw.data$correct_word, transitional_probability = raw.data$correct_word_trans_prob, language=raw.data$language, participant=raw.data$participant), 
  FUN=function(x) (mean(x) * 6)
)
names(df2)[names(df2)=="x"] <- "correct"
print (df2)

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

cat("\n4. Are scores on individual words better than chance?\n")
words <- unique(df2$word)
results <- matrix(NA, nc = 6, nr = length(words))
rownames(results) <- words
colnames(results) <- c("language", "transitional probability", "score", "shapiro p-value", "wilcoxon/t-test p-value", "better than chance")
for (i in words) {
  results[i, 1] <- mean(df2$language[df2$word==i])
  results[i, 2] <- mean(df2$transitional_probability[df2$word==i])
  results[i, 3] <- mean(df2$correct[df2$word==i])
  n <- shapiro.test(df2$correct[df2$word==i])$p.value
  results[i, 4] <- n
  if (n >= .05) {
    p <- t.test(df2$correct[df2$word==i], mu=3)$p.value
    results[i, 5] <- p
  }
  else {
    p <- wilcox.test(jitter(df2$correct[df2$word==i]), mu=3)$p.value
    results[i, 5] <- p
  }
  if (p < .05) results[i, 6] <- "yes"
  else results[i, 6] <- "no"
}
results
cat("\nNote 1: In original study, all scores except ADB were better than chance\n")
cat("\nNote 2: I used the mean for the first two columns, because I couldn't figure out another way\n")

cat("\n5. Are the results similar for Language 1 and Language 2?\n")
cat("\n5.a. No need to check Language 1 and Language 2 data for normality, already done in 2.a and 3.a.\n")
cat("\n5.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ language, df)
cat("\nIf p >= .05, no difference between Language 1 and Language 2 data, as expected.\n")

# 6. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 1.

# 7. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 2.

# 8. TO DO - Logistic regression

# 9. TO DO - Scatter plot for transitional probability vs. average score for each word

cat("\n10. How do results compare with Saffran's linguistics study?\n")
cat("\n10.a. First, test distribution of overall scores for normality.\n")
shapiro.test(df$correct)
cat("\n10.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct, mu=27.2)
cat("\nIf p >= .05, results are similar to linguistics study.\n")