## External
library(readr)
library(broom)
library(dplyr)

# Settings
hlim <- 200

# Read data
iwine <- read.csv("wine.csv")

# Clean and prepare dataset
owine <- iwine %>%
  dplyr::select(price, WinterRain, temp, HarvestRain, Age) %>%
  dplyr::mutate(Dheavyrain = ifelse(HarvestRain > hlim, 1, 0)) %>%
  na.omit()

### Exercise 1
sds <- summary(owine)
cds <- cor(owine)

### Exercise 2
for1 <- price ~ Age
mod1 <- lm(for1, data = owine)
pv1 <- predict(mod1, newdata = owine)
pv2 <- predict(mod1, newdata = data.frame(Age = 1961), interval = "prediction", level = 0.95)

### Exercise 3
for2 <- price ~ Age + WinterRain + temp + HarvestRain
mod2 <- lm(for2, data = owine)

### Exercise 4
for3 <- price ~ Age + WinterRain + temp + HarvestRain + Dheavyrain
mod3 <- lm(for3, data = owine)

### Exercise 5
res1 <- resid(mod1)
res2 <- resid(mod2)
res3 <- resid(mod3)
resf <- data.frame(res1, res2, res3)
