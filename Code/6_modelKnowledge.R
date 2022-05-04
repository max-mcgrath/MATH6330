# Load relevant libraries
# library(MASS)

# Get analysis data
# source("Code/3_analysisData.R")

# Clean up workspace
# rm(list = setdiff(ls(), "analysisData"))

# Forward stepwise using BIC - Union for Benfits
fullModelKnowledge <- lm(knowledge ~ nonWhite + female + age + wage, 
                         data = analysisData)
interceptModelKnowledge <- lm(knowledge ~ 1, data = analysisData)
forwardSelectionKnowledge <- 
    MASS::stepAIC(interceptModelKnowledge, direction = "forward",
                  scope = list(lower = interceptModelKnowledge,
                               upper = fullModelKnowledge),
                  k = log(ncol(analysisData)))

# Assumption checking
summary(fullModelKnowledge)
car::residualPlots(fullModelKnowledge)
car::qqPlot(fullModelKnowledge)

# Compile final model into table
knowledgeSummaryData <- as.data.frame(coef(forwardSelectionKnowledge)) %>%
    bind_cols(as.data.frame(confint.default(forwardSelectionKnowledge))) %>%
    mutate(pvalue = coef(summary(forwardSelectionKnowledge))[, 4],
           model = "knowledge") %>%
    tibble::rownames_to_column(var = "variable") %>%
    select(model, variable, "estimate" = "coef(forwardSelectionKnowledge)", 
           "lower" = "2.5 %", "upper" = "97.5 %", pvalue) %>%
    mutate(across(where(is.numeric), round, digits = 3)) %>%
    mutate(CI = paste0("(", lower, ", ", upper, ")"),
           variable = c("Intercept", "Female/non-binary", "26-30", "31-35",
                        "36-40", "41+", "Non-White", "$15-16", "$17-18",
                        "$19 or more")) %>%
    select(model, variable, estimate, CI, pvalue) %>%
    filter(variable != "Intercept")  %>%
    mutate(pvalue = case_when(pvalue == 0 ~ "<0.001*", 
                              pvalue <= 0.05 ~ paste0(pvalue, "*"),
                              TRUE ~ as.character(pvalue)))

