library(readr)
library(tidyverse)
library(lme4)
library(gt)


# read and clean factors
chimp_data <- read_csv("chimp-memory.csv")

chimp_data_reshape <- chimp_data %>%
  pivot_longer(cols = starts_with("Week"),
               names_to = "Week",
               values_to = "Score")
chimp_data_reshape$Week <- factor(chimp_data_reshape$Week, levels = c("Week2","Week4","Week8","Week12","Week16"))
chimp_data_reshape$is_ST <- if_else(chimp_data_reshape$Week %in% c("Week2","Week4"), 1, 0)
chimp_data_reshape$Monkey <- as.factor(chimp_data_reshape$Monkey)
chimp_data_reshape$Treatment <- factor(chimp_data_reshape$Treatment, levels = c("Control", "Treated"))

# plot data
ggplot(chimp_data_reshape, aes(x = Week, y = Score, group = Monkey, colour = Treatment)) +
  geom_line(linewidth = 1.2) +                     
  scale_y_continuous(limits = c(0, 100)) +        
  labs(title = "Chimp Memory Scores Over Time") + 
  geom_vline(xintercept = 2.5,    # short v long term memory                
             linetype = "dashed", 
             color = "darkred",
             linewidth = 1.3) +
  geom_text(aes(x = 2.5, y = 100, label = "Short vs Long Term"),
            vjust = 25, color = "darkred") +
  theme_minimal()

# put into binomial format

chimp_data_reshape$total_correct <- (chimp_data_reshape$Score / 100)*20
chimp_data_reshape$trials <- 20

# run models 

model <- glm(cbind(total_correct, trials - total_correct) ~ Week * Treatment,
                data = chimp_data_reshape,
                family = binomial())

model2 <- glm(cbind(total_correct, trials - total_correct) ~ is_ST * Treatment,
               data = chimp_data_reshape,
               family = binomial())


summary(model2)
BIC(model, model2)
logLik(model)
logLik(model2)

# prediction

newdat <- expand.grid(is_ST = c(0, 1),
                      Treatment = factor(c("Control", "Treated"),levels = levels(chimp_data_reshape$Treatment)))

newdat$predict_log <- predict(model2, newdat)
newdat$predict_prob <- predict(model2, newdat, type = "response")

newdat %>%
  gt() %>%
  tab_header(title = md("**Table of Predictions**")) %>%
  fmt_percent(predict_prob, decimals = 0) %>%
  fmt_number(predict_log, decimals = 3) %>%
  cols_label(is_ST = "Is ST",
             predict_log = "Pred Log Odds",
             predict_prob = "Pred Prob")

sessionInfo()

