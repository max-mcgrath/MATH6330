library(dplyr)
library(tidyr)
library(binomTools)
library(ggplot2)

# Get analysis data
# source("Code/3_analysisData.R")

# Clean up workspace
# rm(list = setdiff(ls(), "analysisData"))

# Forward stepwise using BIC - Union for Safety --------------------------------
fullModelSafety <- 
    glm(unionForSafety ~ bioHazards + chemHazards + physHazards + nonWhite + 
            female + age + wage,
        family = binomial, data = analysisData)
interceptModelSafety <- 
    glm(unionForSafety ~ 1, family = binomial, data = analysisData)
forwardSelectionSafety <- 
    MASS::stepAIC(interceptModelSafety, direction = "forward",
                  scope = list(lower = interceptModelSafety, 
                               upper = fullModelSafety),
                  k = log(ncol(analysisData)))
summary(forwardSelectionSafety)

# Assumption checking
probs <- predict(forwardSelectionSafety, type = "response")
asssumptionData <- analysisData %>%
    mutate(logit = log(probs / (1 - probs))) %>%
    tidyr::pivot_longer(c(physHazards, nonWhite), names_to = "predictor")
ggplot(asssumptionData, aes(x = value, y = logit)) +
    geom_point() +
    facet_wrap(~ predictor)
car::vif(forwardSelectionSafety)
resids <- Residuals(forwardSelectionSafety, type = "standard.deviance")
plot(x = probs, y = resids)


# Compile final model into table
safetySummaryData <- as.data.frame(exp(coef(forwardSelectionSafety))) %>%
    bind_cols(as.data.frame(exp(confint.default(forwardSelectionSafety)))) %>%
    mutate(pvalue = coef(summary(forwardSelectionSafety))[, 4],
           model = "safety") %>%
    tibble::rownames_to_column(var = "variable") %>%
    select(model, variable, "estimate" = "exp(coef(forwardSelectionSafety))", 
           "lower" = "2.5 %", "upper" = "97.5 %", pvalue) %>%
    mutate(across(where(is.numeric), round, digits = 3)) %>%
    mutate(CI = paste0("(", lower, ", ", upper, ")"),
           variable = c("Intercept", "Phys. Haz. Concern", "Non-White")) %>%
    select(model, variable, estimate, CI, pvalue)

# Forward stepwise using BIC - Union for Wages ---------------------------------
fullModelWages <- 
    glm(unionForWages ~ bioHazards + chemHazards + physHazards + nonWhite + 
            female + age + wage,
        family = binomial, data = analysisData)
interceptModelWages <- 
    glm(unionForWages ~ 1, family = binomial, data = analysisData)
forwardSelectionWages <- 
    MASS::stepAIC(interceptModelWages, direction = "forward",
                  scope = list(lower = interceptModelWages, 
                               upper = fullModelWages),
                  k = log(ncol(analysisData)))
summary(forwardSelectionWages)

# Assumption checking
probs <- predict(forwardSelectionWages, type = "response")
asssumptionData <- analysisData %>%
    mutate(logit = log(probs / (1 - probs))) %>%
    tidyr::pivot_longer(c(physHazards, nonWhite, bioHazards), 
                        names_to = "predictor")
ggplot(asssumptionData, aes(x = value, y = logit)) +
    geom_point() +
    facet_wrap(~ predictor)
car::vif(forwardSelectionWages)
resids <- Residuals(forwardSelectionSafety, type = "standard.deviance")
plot(x = probs, y = resids)

# Compile final model into table
wagesSummaryData <- as.data.frame(exp(coef(forwardSelectionWages))) %>%
    bind_cols(as.data.frame(exp(confint.default(forwardSelectionWages)))) %>%
    mutate(pvalue = coef(summary(forwardSelectionWages))[, 4],
           model = "wages") %>%
    tibble::rownames_to_column(var = "variable") %>%
    select(model, variable, "estimate" = "exp(coef(forwardSelectionWages))", 
           "lower" = "2.5 %", "upper" = "97.5 %", pvalue) %>%
    mutate(across(where(is.numeric), round, digits = 3)) %>%
    mutate(CI = paste0("(", lower, ", ", upper, ")"),
           variable = c("Intercept", "Phys. Haz. Concern", "Non-White",
                        "Bio. Haz. Concern")) %>%
    select(model, variable, estimate, CI, pvalue)

# Forward stepwise using BIC - Union for Benfits -------------------------------
fullModelBenefits <- 
    glm(unionForBenefits ~ bioHazards + chemHazards + physHazards + nonWhite + 
            female + age + wage,
        family = binomial, data = analysisData)
interceptModelBenefits <- 
    glm(unionForBenefits ~ 1, family = binomial, data = analysisData)
forwardSelectionBenefits <- 
    MASS::stepAIC(interceptModelBenefits, direction = "forward",
                  scope = list(lower = interceptModelBenefits, 
                               upper = fullModelBenefits),
                  k = log(ncol(analysisData)))
summary(forwardSelectionBenefits)

# Assumption checking
probs <- predict(forwardSelectionBenefits, type = "response")
asssumptionData <- analysisData %>%
    mutate(logit = log(probs / (1 - probs))) %>%
    tidyr::pivot_longer(c(physHazards, nonWhite), 
                        names_to = "predictor")
ggplot(asssumptionData, aes(x = value, y = logit)) +
    geom_point() +
    facet_wrap(~ predictor)
car::vif(forwardSelectionBenefits)

# Compile final model into table
benefitsSummaryData <- as.data.frame(exp(coef(forwardSelectionBenefits))) %>%
    bind_cols(as.data.frame(exp(confint.default(forwardSelectionBenefits)))) %>%
    mutate(pvalue = coef(summary(forwardSelectionBenefits))[, 4],
           model = "benefits") %>%
    tibble::rownames_to_column(var = "variable") %>%
    select(model, variable, "estimate" = "exp(coef(forwardSelectionBenefits))", 
           "lower" = "2.5 %", "upper" = "97.5 %", pvalue) %>%
    mutate(across(where(is.numeric), round, digits = 3)) %>%
    mutate(CI = paste0("(", lower, ", ", upper, ")"),
           variable = c("Intercept", "Phys. Haz. Concern", "Non-White")) %>%
    select(model, variable, estimate, CI, pvalue)

# Check variance inflation factor of full model
# car::vif(fullModel)

# Combine tables into single table
unionSupportSummary <- bind_rows(safetySummaryData, wagesSummaryData,
                                 benefitsSummaryData) %>%
    filter(variable != "Intercept")  %>%
    mutate(pvalue = case_when(pvalue == 0 ~ "<0.001*", 
                              pvalue <= 0.05 ~ paste0(pvalue, "*"),
                              TRUE ~ as.character(pvalue)))
