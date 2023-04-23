# Assignment 9(i)

dfA <- read.csv("descriptives (Group 2).csv", header = T)


# Summary Statistics
library(fBasics)
summ <- basicStats(dfA[,3])

t_summ <- as.data.frame(t(round(summ,3)))
rownames(t_summ) <- "Age"
library(dplyr)
sm1 <- t_summ[,c(3,5,8,6,4,7,14)] %>% 
  mutate(Range = Maximum - Minimum,
         P90 = quantile(dfA$Age, .9))
names(sm1) <- c("Min", "Q1", "Median", "Q3", "Max", "Mean", "St.dev", "Range", 
                "90th perc")
knitr::kable(sm1, caption = "Summary statistics of Age variable")



# Assignment 9(ii)
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Obtain mode
getmode(dfA$Sports)
# Assignment 9(iii)
# Create frequency table
tb1 <- table(dfA$Gender)
# Print frequency table
knitr::kable(tb1, caption = "Frequency table of Gender variable")

# Assignment 10
dfB <- read.csv("HDI.csv", header = T)

# Assignment 11
library(ggplot2)
ggplot(dfB, aes(group, log(GDP), fill = group)) +
  geom_boxplot() + theme(legend.position = "none") +
  labs(x = "Group")

# Assignment 12
# Select countries in high and low groups
br1 <- dfB %>% filter(group %in% c("high", "low"))

# Plot histograms
br2 <- br1 %>% select(group, GDP) %>% melt(id = "group")

ggplot(br2, aes(value, fill = group)) +
  geom_histogram() + theme(legend.position = "none") +
  facet_wrap(~group, scales = "free") +
  labs(x = "GDP", y = "Frequency")


# Assignment 13
## Subset low and high groups
lw <- subset(br1, group == "low")
hgh <- subset(br1, group == "high")

## Normality test
shapiro.test(lw$GDP) ## Low group
shapiro.test(hgh$GDP) ## High group

# Assignment 14
wilcox.test(lw$GDP, hgh$GDP, alternative = "two.sided")
