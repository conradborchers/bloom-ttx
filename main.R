library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(readr)
library(forcats)
library(tidyr)

# Read and prepare data
df <- read_csv("data-bloom-matched.csv") %>%
  mutate(
    teamID = as.factor(teamID),
    bloom_category_milestone = as.factor(bloom_category_milestone),
    bloom_category_emails = as.factor(bloom_category_emails),
    achieved = as.numeric(achieved)
  )

# Fit linear mixed model with categorical interaction
m0 <- glmer(achieved ~ bloom_category_milestone + (1 | teamID),
                   data = df, family = binomial)
m1 <- glmer(achieved ~ bloom_category_milestone + discrepancy + (1 | teamID),
                   data = df, family = binomial)
m2 <- glmer(achieved ~ bloom_category_milestone * discrepancy + (1 | teamID),
            data = df, family = binomial)
anova(m0, m1, m2)
sjPlot::tab_model(m1) # Selected


library(dplyr)
library(lme4)
library(broom.mixed)

# make sure it's a factor and set "other" as the reference
df <- df %>%
  mutate(
    bloom_category_milestone = factor(bloom_category_milestone),
    bloom_category_milestone = forcats::fct_relevel(bloom_category_milestone, "other")
  )

m1_other <- glmer(
  achieved ~ bloom_category_milestone + discrepancy + (1 | teamID),
  data = df, family = binomial
)

summary(m1_other)

# Odds ratios + 95% CI for convenience
or_tab <- broom.mixed::tidy(m1_other, effects = "fixed", conf.int = TRUE, conf.method = "Wald") %>%
  mutate(OR = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) %>%
  select(term, OR, conf.low, conf.high, p.value)
or_tab


# Add ranked depth values for Bloom categories
bloom_rank <- c(
  "other" = 0,
  "remembering" = 1,
  "understanding" = 2,
  "applying" = 3,
  "analyzing" = 4,
  "evaluating" = 5,
  "creating" = 6
)

df <- df %>%
  mutate(
    milestone_rank = bloom_rank[as.character(bloom_category_milestone)],
    email_rank = bloom_rank[as.character(bloom_category_emails)]
  )

# Fit linear mixed model with ranked predictors
model_rank <- glmer(achieved ~ milestone_rank * email_rank + (1 | teamID),
                    data = df, family = binomial)
summary(model_rank)

model_rank <- glmer(achieved ~ milestone_rank * discrepancy + (1 | teamID),
                    data = df, family = binomial)
summary(model_rank)
sjPlot::tab_model(model_rank)
