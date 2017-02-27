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

cat("\n1. Stripchart for participant scores\n")
stripchart(df$correct, method = "stack", vertical = TRUE,
           main = "Number of correct answers for each participant", ylab = "Score",
           ylim = c(0, 36), offset = .5, pch = 16) 
abline(h=18, lty= 2)

cat("\n2. Are the results similar for Test 1 and Test 2, for each language?\n")
cat("\n2.a. First, test for distributions of Test 1 and Test 2 data in Language 1 for normality.\n")
shapiro.test(df$correct[df$language == 1 & df$test == 1])
shapiro.test(df$correct[df$language == 1 & df$test == 2])
cat("\n2.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ test, df, subset=(df$language == 1))
cat("\nIf p >= .05, no difference between Test 1 and Test 2 data in Language 1, as expected.\n")
cat("\n2.c. Now, test for distributions of Test 1 and Test 2 data in Language 2 for normality.\n")
shapiro.test(df$correct[df$language == 2 & df$test == 1])  # FIXME this one ended significant!
shapiro.test(df$correct[df$language == 2 & df$test == 2])
cat("\n2.d. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ test, df, subset=(df$language == 2))
cat("\nIf p >= .05, no difference between Test 1 and Test 2 data in Language 2, as expected.\n")

cat("\n3. Did people perform better than chance on Language 1?\n")
cat("\n3.a. First, test distribution of Language 1 data for normality.\n")
shapiro.test(df$correct[df$language == 1])
cat("\n3.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct[df$language == 1], mu=18)
cat("\nIf p < .05, people performed better than chance\n")

cat("\n4. Did people perform better than chance on Language 2?\n")
cat("\n4.a. First, test distribution of Language 2 data for normality.\n")
shapiro.test(df$correct[df$language == 2])
cat("\n4.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct[df$language == 2], mu=18)
cat("\nIf p < .05, people performed better than chance\n")

cat("\n5. Are scores on individual words better than chance?\n")
words <- aggregate(
  df2$correct,
  list(word=df2$word, transitional.probability=df2$transitional_probability, language=df2$language),
  FUN=function(x) (c(shapiro=shapiro.test(x)$p.value,
                     t.test=t.test(x, mu=3)$p.value,
                     wilcox=wilcox.test(jitter(x), mu=3)$p.value,
                     score=mean(x)))
)
names(words)[names(words)=="x"] <- "correct"
words$better.than.chance <- apply(words[,c('correct')], 1, function(x) (if (x['shapiro'] > 0.05) x['t.test'] < 0.05 else x['wilcox'] < 0.05))
words
cat("\nNote: In original study, all scores except ADB were better than chance\n")

cat("\n6. Are the results similar for Language 1 and Language 2?\n")
cat("\n6.a. No need to check Language 1 and Language 2 data for normality, already done in 2.a and 3.a.\n")
cat("\n6.b. If both p values are >= .05, proceed with t.test. Otherwise, use wilcox.test.\n")
t.test(correct ~ language, df)
cat("\nIf p >= .05, no difference between Language 1 and Language 2 data, as expected.\n")

# 7. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 1.

# 8. TO DO - ANOVA for words with high vs. low transitional probabilities in Language 2.

# 9. TO DO - Logistic regression

cat("\n10. Scatter plot for transitional probability vs. average score for each word\n")
plot(words$transitional.probability, words$correct[,c('score')],
     main="Individual words",
     xlab="Average transitional probability", ylab="Number of correct answers",
     xlim=c(0, 1), ylim=c(0, 6))
abline(lm(words$correct[,c('score')] ~ words$transitional.probability), lty = 2)

cat("\n11. How do results compare with Saffran's linguistics study?\n")
cat("\n11.a. First, test distribution of overall scores for normality.\n")
shapiro.test(df$correct)
cat("\n11.b. If p >= .05, proceed with t.test. Otherwise, use wilcox.test. \n")
t.test(df$correct, mu=27.2)
cat("\nIf p >= .05, results are similar to linguistics study.\n")