# Install needed packages

# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("broom")
# install.packages("performance")
# install.packages("gt")
# install.packages("dplyr")

#load needed packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)
library(gt)

#load and clean the dataset

data <- attitude

data.clean <- attitude |>
  select(rating, complaints, learning, raises) |>
  drop_na()

n <- nrow(data.clean)

## 2. Explore the data

### Descriptive Statistics

#| label: descriptive.table
#| tbl-cap: "Descriptive Statistics for All Vaviables"

descriptives <- data.clean |>
  summarize(across(c(rating, complaints, learning, raises), 
            list(mean = mean, 
                 sd = sd, 
                 min = min, 
                 max = max)
            )) |>
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_")

descriptives |>
  gt() |>
  fmt_number(columns = where(is.numeric), decimals = 2)

#complaints distribution
complaints.dist <- ggplot(data.clean, aes(x = complaints)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 10, 
    fill = "gray",
    colour = "white")

complaints.dist

#learning distribution
learning.dist <- ggplot(data.clean, aes(x = learning)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 10, 
    fill = "gray",
    colour = "white")

learning.dist

#raises distribution
raises.dist <- ggplot(data.clean, aes(x = raises)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 10, 
    fill = "gray",
    colour = "white")

raises.dist

### Distribution of rating

#| label: rating.distribution
#| tbl-cap: "Distribution of Departmental Ratings"
  
ggplot(data.clean, aes(x = rating)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 10, 
    fill = "gray",
    colour = "white") +
  geom_density(
    linewidth = 1.0, 
    colour = "black")+
  labs(
    x = "Departmental Ratings", 
    y = "Density") +
  theme_classic(base_size = 12)
 
### Relationship Between Predictors and Overall Rating

#complaints scatterplot
#| label: complaints.scatterplot
#| fig-cap: "Relationship between Handling of Complaints and Overall Rating"

ggplot(data.clean, aes(x = complaints, y = rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "gray"
  ) +
  labs(
    x = "Handling of Complaints Rating",
    y = "Overall Departmental Workplace Rating"
  ) +
  theme_classic(base_size = 12)

#learning scatterplot
#| label: learning.scatterplot
#| fig-cap: "Relationship between Opportunity for Learning and Overall Rating"

ggplot(data.clean, aes(x = learning, y = rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "gray"
  ) +
  labs(
    x = "Opportunity for Learning Rating",
    y = "Overall Departmental Workplace Rating"
  ) +
  theme_classic(base_size = 12)

#raises scatterplot
#| label: raises.scatterplot
#| fig-cap: "Relationship between Pay Raises and Overall Rating"

ggplot(data.clean, aes(x = raises, y = rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "gray"
  ) +
  labs(
    x = "Pay Raises Rating",
    y = "Overall Departmental Workplace Rating"
  ) +
  theme_classic(base_size = 12)

### Fit the Nested Models

#| label: fit.models

# Intercept-only model
model_1 <- lm(rating ~ 1, data = data.clean)

# complaints model
model_2 <- lm(rating ~ complaints, data = data.clean)

# complaints + learning model
model_3 <- lm(rating ~ complaints + learning, data = data.clean)

# Expanded model
model_4 <- lm(rating ~ complaints + learning + raises, data = data.clean)

### Inspect the Nested Models

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

confint(model_4)

## Compare the models
### Nested-Model Comparisons

#| label: nested.tests

nested.tests <- anova(model_1, model_2, model_3, model_4)

nested.tests

nested.tests <- nested.tests |>
  select(Res.Df, Df, F, `Pr(>F)`) |>
  rename(
    Residual.Df = Res.Df,
    Nested.Df = Df,
    Nested.F.stat = F,
    Nested.p.value = `Pr(>F)`
  )

### Overall Model Performance

#| label: model.performance

glance1 <- glance(model_1)
glance2 <- glance(model_2)
glance3 <- glance(model_3)
glance4 <- glance(model_4)

performance <- rbind(glance1, glance2, glance3, glance4)

### Combine Performance Tests, Calculate RMSE and Delta R Squared

#| label: combine.tests

models <- c(
  "M1: Null",
  "M2: Complaints",
  "M3: Complaints + Learning",
  "M4: Complaints + Learning + Raises"
)

comparison.table <- cbind(
  Model = models,
  performance,
  nested.tests
)

comparison.table <- comparison.table |>
  mutate(Delta.R2 = r.squared - lag(r.squared),
         RMSE = c(
           sqrt(mean(residuals(model_1)^2)),
           sqrt(mean(residuals(model_2)^2)),
           sqrt(mean(residuals(model_3)^2)),
           sqrt(mean(residuals(model_4)^2)))
  ) |>
  select(
    Model,
    r.squared,
    adj.r.squared,
    RMSE,
    Delta.R2,
    Residual.Df,
    Nested.Df,
    Nested.F.stat,
    Nested.p.value
  )


### Display the combined model-comparison table

#| label: model.comparison.table
#| tbl-cap: "Overall Model Performance and Incremental Nested Model Improvements"

comparison.table |>
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE",
    Delta.R2 = "ΔR²",
    Residual.Df = "Residual Df",
    Nested.Df = "Nested df",
    Nested.F.stat = "Nested F",
    Nested.p.value = "Nested p"
  ) |>
  fmt_number(
    columns = c(
      r.squared,
      Delta.R2,
      adj.r.squared,
      RMSE,
      Nested.F.stat
    ),
    decimals = 2
  ) |>
  fmt_number(
    columns = Nested.p.value,
    decimals = 3
    ) |>
  sub_missing(
    columns = c(Delta.R2, Nested.Df, Nested.F.stat),
    missing_text = "—"
  ) |>
  tab_spanner(
    label="Overall Performance",
    columns = c(r.squared, adj.r.squared, RMSE)
  )|>
  tab_spanner(
    label="Incremental Improvements",
    columns = c(Delta.R2, Residual.Df, Nested.Df, Nested.F.stat, Nested.p.value)
  )

### Select the Best Model

#| label: select.model

selected.row <- comparison.table |>
  slice_max(
    order_by = adj.r.squared,
    n = 1,
    with_ties = FALSE
  )

selected.name <- selected.row$Model

selected.model <- switch(
  selected.name,
  "M1: Null" = model_1,
  "M2: Complaints" = model_2,
  "M3: Complaints + Learning" = model_3,
  "M4: Complaints + Learning + Raises" = model_4
)

## Check Model Assumptions

#| label: diagnostics
#| fig-cap: "Regression diagnostic plots for Model 3"
#| fig-width: 8
#| fig-height: 7

op <- par(mfrow = c(2, 2))

plot(selected.model)

par(op)

check_collinearity(selected.model)

## Report the selected model

### Compute Model 3's coefficients

#| label: coefficients

model.results <- tidy(
  selected.model,
  conf.int = TRUE,
  conf.level = 0.95
)

### Create a Coefficient Table

#| label: coefficient.table
#| tbl-cap: "Unstandardized Regression Coefficients for Model 3"

model.results |>
  select(
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  ) |>
  gt() |>
  cols_label(
    term = "Predictor Name",
    estimate = "Unstandardized Estimate",
    std.error = "Standard Error",
    conf.low = "95% CI Lower",
    conf.high = "95% CI Upper",
    p.value = "p-value"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high
    ),
    decimals = 2
  ) |>
  fmt_number(
    columns = p.value, decimals = 3
    ) |>
  fmt(
    columns = p.value,
    fns = function(x) {
      ifelse(
        x < .001,
        "< .001",
        sprintf("%.3f", x)
      )
    }
  )

### Overall Model Performance

#| label: selected.model.performance
#| tbl-cap: "Overall performance of Model 3"

selected.row |>
  select(
    Model,
    r.squared,
    adj.r.squared,
    RMSE
  ) |>
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  )
