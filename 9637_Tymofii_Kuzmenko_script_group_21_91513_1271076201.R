# Install the WDI package if not already installed
# if (!require(WDI)) install.packages("WDI")
# if (!require(tidyverse)) install.packages("tidyverse")

# Load libraries
library(WDI)
library(tidyverse)
library(car)
library(lmtest)
library(zoo)
library(ggplot2)
library(sandwich)

# Search for indicators — here you can find the required codes
indicators <- WDIsearch("gdp|population|health|education", field = "name")
head(indicators, 20)  # show first 20 matches

# Download data
data <- WDI(
  country = c("USA", "BRA", "IND", "CHN", "ZAF"),
  indicator = c(
    "NY.GDP.PCAP.CD",          # GDP per capita (current US$)
    "SP.POP.TOTL",             # Total population
    "SE.PRM.ENRR",             # School enrollment, primary (% gross)
    "SH.XPD.CHEX.PC.CD"        # Health exp per capita
  ),
  start = 2010,
  end = 2020,
  extra = TRUE
)

# Rename columns
data <- data %>%
  rename(
    GDP_per_capita = NY.GDP.PCAP.CD,
    Population = SP.POP.TOTL,
    Primary_Enroll = SE.PRM.ENRR,
    Health_Exp = SH.XPD.CHEX.PC.CD
  )

write.csv(data, "data_group_21.csv", row.names = FALSE)

# Fill missing values using LOCF (Last Observation Carried Forward), also backwards for Primary_Enroll
clean_data <- data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    GDP_per_capita = na.locf(GDP_per_capita, na.rm = FALSE),
    Population = na.locf(Population, na.rm = FALSE),
    Primary_Enroll = na.locf(na.locf(Primary_Enroll, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE),
    Health_Exp = na.locf(Health_Exp, na.rm = FALSE)
  ) %>%
  ungroup()

# View structure of the data
glimpse(clean_data)

# Save for future use
# write.csv(clean_data, "wdi_clean_data.csv", row.names = FALSE)

# Remove duplicate rows if any
clean_data <- clean_data %>% distinct()

# Keep only relevant variables
clean_data <- clean_data %>%
  select(country, year, GDP_per_capita, Population, Primary_Enroll, Health_Exp)

# View unique country names
unique(clean_data$country)

# Create logarithms of the variables
clean_data <- clean_data %>%
  mutate(
    log_GDP = log(GDP_per_capita),
    log_Pop = log(Population),
    log_Health = log(Health_Exp),
    log_Enroll = log(Primary_Enroll)
  )

# log_GDP explains GDP_per_capita
model_log_GDP <- lm(GDP_per_capita ~ log_GDP, data = clean_data)
summary(model_log_GDP)  # t-test
anova(model_log_GDP)    # F-test

# log_Health explains Health_Exp
model_log_Health <- lm(Health_Exp ~ log_Health, data = clean_data)
summary(model_log_Health)
anova(model_log_Health)

# log_Enroll explains Primary_Enroll
model_log_Enroll <- lm(Primary_Enroll ~ log_Enroll, data = clean_data)
summary(model_log_Enroll)
anova(model_log_Enroll)

# log_Pop explains Population
model_log_Pop <- lm(Population ~ log_Pop, data = clean_data)
summary(model_log_Pop)
anova(model_log_Pop)

# Scale variables (z-score standardization)
scaled_data <- clean_data %>%
  mutate(across(c(log_GDP, log_Pop, log_Health, log_Enroll), ~ as.numeric(scale(.))))

glimpse(scaled_data)

# Keep only developing countries: Brazil, India, China, South Africa
developing_data <- scaled_data %>%
  filter(country %in% c("Brazil", "India", "China", "South Africa"))

# Calculate annual growth rates (differences of logarithms)
developing_data <- developing_data %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    GDP_growth = log_GDP - lag(log_GDP),
    Health_growth = log_Health - lag(log_Health),
    Enroll_growth = log_Enroll - lag(log_Enroll),
    Pop_growth = log_Pop - lag(log_Pop)
  ) %>%
  ungroup()

# GDP_growth vs log_GDP
model_gdp_growth <- lm(log_GDP ~ GDP_growth, data = developing_data)
summary(model_gdp_growth)
anova(model_gdp_growth)

# Health_growth vs log_Health
model_health_growth <- lm(log_Health ~ Health_growth, data = developing_data)
summary(model_health_growth)
anova(model_health_growth)

# Enroll_growth vs log_Enroll
model_enroll_growth <- lm(log_Enroll ~ Enroll_growth, data = developing_data)
summary(model_enroll_growth)
anova(model_enroll_growth)

# Enroll_growth vs log_Enroll
model_pop_growth <- lm(log_Pop ~ Pop_growth, data = developing_data)
summary(model_pop_growth)
anova(model_pop_growth)

# Quick check: number of rows and structure
nrow(developing_data)
glimpse(developing_data)

# Histogram of log_GDP
ggplot(developing_data, aes(x = log_GDP)) + 
  geom_histogram(bins = 30, fill = "skyblue") + 
  facet_wrap(~ country) + 
  theme_minimal()

# Boxplot of log_Health by country
ggplot(developing_data, aes(x = country, y = log_Health)) + 
  geom_boxplot(fill = "tomato") + 
  theme_minimal()

# Scatterplot: log_Health vs log_GDP
ggplot(developing_data, aes(x = log_Health, y = log_GDP, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Line plot of log_GDP trends by year. It works. Only need to be run seperatly from other code.
ggplot(developing_data %>% arrange(country, year), aes(x = year, y = log_GDP, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "GDP per capita (log) trends by country (2010–2020)",
    x = "Year",
    y = "log(GDP per capita)",
    color = "Country"
  )

# Model: log_GDP ~ log_Health + log_Enroll + log_Pop
model1 <- lm(log_GDP ~ log_Health + log_Enroll + log_Pop, data = developing_data)
summary(model1)

# Drop first row per country (lag created NA)
growth_data <- developing_data %>% drop_na(GDP_growth, Health_growth, Enroll_growth)

# Model: GDP_growth ~ Health_growth + Enroll_growth
model2 <- lm(GDP_growth ~ Health_growth + Enroll_growth, data = growth_data)
summary(model2)

# Full model for stepwise selection
full_model <- lm(GDP_growth ~ Health_growth + Enroll_growth + log_Pop, data = growth_data)

# Null model (intercept only)
null_model <- lm(GDP_growth ~ 1, data = growth_data)

# Stepwise selection based on AIC
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both")
summary(step_model)

# Diagnostics for model1 (log_GDP)
par(mfrow = c(2,2))
plot(model1)

# Diagnostics for model2 (GDP_growth)
par(mfrow = c(2,2))
plot(model2)

# Model with fixed effects by country
model3 <- lm(log_GDP ~ log_Health + log_Enroll + log_Pop + country, data = developing_data)
summary(model3)

# Confidence intervals for models
confint(model1)
confint(model2)
confint(model3)

# --- MODEL DIAGNOSTICS: MULTICOLLINEARITY, HETEROSKEDASTICITY, AUTOCORRELATION ---

# Ramsey RESET test 
resettest(model1, power = 2:3, type = "fitted")
resettest(model2, power = 2:3, type = "fitted")
resettest(model3, power = 2:3, type = "fitted") # maybe have a problem

model3_interact <- lm(
  log_GDP ~ log_Health * country + 
    log_Enroll * country + 
    log_Pop * country,
  data = developing_data
)

summary(model3_interact)
resettest(model3_interact, power = 2:3, type = "fitted") # after check it was found that it fails in multicollinearity

# VIF: multicollinearity
vif(model1)
vif(model2)
vif(model3)

# Breusch-Pagan test: heteroskedasticity
bptest(model1)
bptest(model2)
bptest(model3)

# Special White test
u2 <- resid(model1)^2
yhat <- fitted(model1)
white_model_1 <- lm(u2 ~ yhat + I(yhat^2))
summary(white_model_1)

u2 <- resid(model2)^2
yhat <- fitted(model2)
white_model_2 <- lm(u2 ~ yhat + I(yhat^2))
summary(white_model_2)

u2 <- resid(model3)^2
yhat <- fitted(model3)
white_model_3 <- lm(u2 ~ yhat + I(yhat^2))
summary(white_model_3)

coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
coeftest(model3, vcov = vcovHC(model3, type = "HC1"))

# Durbin-Watson test: autocorrelation
dwtest(model1)
dwtest(model2)
dwtest(model3)

# Residual histograms
par(mfrow = c(1, 3))
hist(resid(model1), main = "Residuals: model1", col = "skyblue", xlab = "Residuals")
hist(resid(model2), main = "Residuals: model2", col = "salmon", xlab = "Residuals")
hist(resid(model3), main = "Residuals: model3", col = "seagreen", xlab = "Residuals")

# Prediction with confidence interval
mean_data <- developing_data %>%
  group_by(country) %>%
  summarise(
    log_Health = mean(log_Health, na.rm = TRUE),
    log_Enroll = mean(log_Enroll, na.rm = TRUE),
    log_Pop = mean(log_Pop, na.rm = TRUE),
    log_GDP = mean(log_GDP, na.rm = TRUE),
    Health_growth = mean(Health_growth, na.rm = TRUE),
    Enroll_growth = mean(Enroll_growth, na.rm = TRUE),
    GDP_growth = mean(GDP_growth, na.rm = TRUE),
    Pop_growth = mean(Pop_growth, na.rm = TRUE)
  )

predict(model1, newdata = mean_data, interval = "confidence")
predict(model2, newdata = mean_data, interval = "confidence")
predict(model3, newdata = mean_data, interval = "confidence")

# Graph of prediction
pred_df <- cbind(mean_data, predict(model3, newdata = mean_data, interval = "confidence"))

ggplot(pred_df, aes(x = country, y = fit)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Predicted log(GDP per capita) by Country (model3)",
    y = "Predicted log(GDP)",
    x = "Country"
  )

# Trend line: population by year
ggplot(developing_data %>% arrange(country, year), aes(x = year, y = log_Pop, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Population (log) trends by country (2010–2020)",
    x = "Year",
    y = "log(Population)",
    color = "Country"
  )

# Trend line: primary enrollment by year
ggplot(developing_data %>% arrange(country, year), aes(x = year, y = log_Enroll, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Primary Enrollment (log) trends by country (2010–2020)",
    x = "Year",
    y = "log(Primary Enrollment)",
    color = "Country"
  )

# Trend line: health expenditure by year
ggplot(developing_data %>% arrange(country, year), aes(x = year, y = log_Health, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Health Expenditure (log) trends by country (2010–2020)",
    x = "Year",
    y = "log(Health Expenditure)",
    color = "Country"
  )

# Boxplot of population by country
ggplot(developing_data, aes(x = country, y = log_Pop, fill = country)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Population (log) by Country",
    x = "Country",
    y = "log(Population)"
  )

# Boxplot of primary enrollment by country
ggplot(developing_data, aes(x = country, y = log_Enroll, fill = country)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Primary Enrollment (log) by Country",
    x = "Country",
    y = "log(Primary Enrollment)"
  )

# Population vs GDP per capita
ggplot(developing_data, aes(x = log_Pop, y = log_GDP, color = country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "log(Population) vs log(GDP per capita)",
    x = "log(Population)",
    y = "log(GDP per capita)"
  )

# Enrollment vs GDP per capita
ggplot(developing_data, aes(x = log_Enroll, y = log_GDP, color = country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "log(Primary Enrollment) vs log(GDP per capita)",
    x = "log(Primary Enrollment)",
    y = "log(GDP per capita)"
  )

# Enrollment vs Health expenditure
ggplot(developing_data, aes(x = log_Enroll, y = log_Health, color = country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "log(Primary Enrollment) vs log(Health Expenditure)",
    x = "log(Primary Enrollment)",
    y = "log(Health Expenditure)"
  )