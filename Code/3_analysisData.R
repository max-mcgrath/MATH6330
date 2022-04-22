# Create (subset) data to be used in analysis
source("Code/1_cleanData.R")
source("Code/2_cleanMultResp.R")

# Create knowledge column (Q87, Q91, Q92, Q93)
allData <- allData %>%
    mutate(knowledge = ifelse(.data$Q91 == "Yes", 1, 0) +
               ifelse(.data$Q92 == "Yes", 1, 0) +
               ifelse(.data$Q93 == "Yes", 1, 0) +
               ifelse(.data$Q87 == "Yes", 1, 0))

# Create workplace hazards column
bioHazards <- ifelse(q47Cols$Q47_D == 0, 1, 0)
chemHazards <- ifelse(q48Cols$Q48_K == 0, 1, 0)
physHazards <- ifelse(q49Cols$Q49_V == 0, 1, 0)

# Create female column
allData <- allData %>%
    mutate(female = case_when(.data$Q4 == "Female" ~ 1,
                              .data$Q4 == "Non-binary/third gender" ~ 1,
                              .data$Q4_TEXT == 
                                  paste0("Female is a sex term. Woman/man is g",
                                         "ender; I am a female that identifies",
                                         " as a woman.") ~ 1,
                              .data$Q4 == "Prefer not to say" ~ as.numeric(NA),
                              TRUE ~ 0))

# Create nonWhite column
allData <- allData %>%
    mutate(nonWhite = case_when(.data$Q5 == "White" ~ 0,
                                .data$Q5 == "Prefer not to say" ~ 
                                    as.numeric(NA),
                                TRUE ~ 1))

# Collapse age
levels(allData$Q3) <- c("21-25", "26-30", "31-35", "36-40", "41+", "41+", "41+")

# Put together to create analysis data
analysisDataWithMissing <- allData %>%
    select(age = Q3, female, nonWhite, wage = Q18, unionForSafety = Q94, 
           unionForWages = Q95, unionForBenefits = Q96, 
           knowledge = knowledge) %>%
    mutate(bioHazards = bioHazards,
           chemHazards = chemHazards,
           physHazards = physHazards)

# Remove missing cases
analysisData <- analysisDataWithMissing %>%
    filter(complete.cases(analysisDataWithMissing))
