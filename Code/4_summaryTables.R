source("Code/2_cleanMultResp.R")
source("Code/3_analysisData.R")
library(dplyr) 

# Missingness Table ------------------------------------------------------------
missingnessSummary <- analysisDataWithMissing %>%
    summarize_all(function(x){ sum(is.na(x)) }) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

missingnessSummary[nrow(missingnessSummary) + 1, ] <- 
    c("incompleteCases", sum(!complete.cases(analysisDataWithMissing)))

missingnessSummary <- missingnessSummary %>%
    mutate(rowname = case_when(.data$rowname == "age" ~ "Age",
                               .data$rowname == "female" ~ "Gender",
                               .data$rowname == "nonWhite" ~ "Race",
                               .data$rowname == "wage" ~ "Wage",
                               .data$rowname == "unionForSafety" ~ 
                                   "Union for Safety",
                               .data$rowname == "unionForWages" ~ 
                                   "Union for Wages",
                               .data$rowname == "unionForBenefits" ~ 
                                   "Union for Benefits",
                               .data$rowname == "knowledge" ~ 
                                   "Knowledge of Workers Rights",
                               .data$rowname == "bioHazards" ~ 
                                   "Exposure to Bio. Hazards",
                               .data$rowname == "chemHazards" ~ 
                                   "Exposure to Chem. Hazards",
                               .data$rowname == "physHazards" ~ 
                                   "Exposure to Phys. Hazards",
                               .data$rowname == "incompleteCases" ~ 
                                   "Total Incomplete Cases"))

# Outcome/Data Summary Table ---------------------------------------------------
# Prepare Support for Unionization Columns
genderSummary <- analysisData %>%
    group_by(female) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = ifelse(.data$female == 1, "Female", "Not Female")) %>%
    relocate(variable) %>%
    select(-female) %>%
    arrange(variable)

raceSummary <- analysisData %>%
    group_by(nonWhite) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = ifelse(.data$nonWhite == 1, "Not White", "White")) %>%
    relocate(variable) %>%
    select(-nonWhite) %>%
    arrange(variable)

ageSummary <- analysisData %>%
    group_by(age) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = age) %>%
    relocate(variable) %>%
    select(-age) %>%
    arrange(variable)

wageSummary <- analysisData %>%
    group_by(wage) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = wage) %>%
    relocate(variable) %>%
    select(-wage) %>%
    arrange(variable)

bioSummary <- analysisData %>%
    group_by(bioHazards) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = ifelse(.data$bioHazards == 1, "Concerned", 
                             "Not Concerned")) %>%
    relocate(variable) %>%
    select(-bioHazards) %>%
    arrange(variable)

chemSummary <- analysisData %>%
    group_by(chemHazards) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = ifelse(.data$chemHazards == 1, "Concerned", 
                             "Not Concerned")) %>%
    relocate(variable) %>%
    select(-chemHazards) %>%
    arrange(variable)

physSummary <- analysisData %>%
    group_by(physHazards) %>%
    summarize(safetyYes = sum(.data$unionForSafety == "Yes"),
              safetyNo = sum(.data$unionForSafety == "No"),
              wagesYes = sum(.data$unionForWages == "Yes"),
              wagesNo = sum(.data$unionForWages == "No"),
              benefitsYes = sum(.data$unionForBenefits == "Yes"),
              benefitsNo = sum(.data$unionForBenefits == "No")) %>%
    mutate(variable = ifelse(.data$physHazards == 1, "Concerned", 
                             "Not Concerned")) %>%
    relocate(variable) %>%
    select(-physHazards) %>%
    arrange(variable)

binaryOutcomeSummary <- bind_rows(bioSummary, chemSummary, physSummary,
                                  genderSummary, raceSummary)
catOutcomeSummary <- bind_rows(wageSummary, ageSummary)
outcomeSummary <- bind_rows(binaryOutcomeSummary, catOutcomeSummary)

# Prepare knowledge of workers' rights columns
genderKnowledge <- analysisData %>%
    group_by(female) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = ifelse(.data$female == 1, "Female", "Not Female")) %>%
    relocate(variable) %>%
    select(-female) %>%
    arrange(variable)

raceKnowledge <- analysisData %>%
    group_by(nonWhite) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = ifelse(.data$nonWhite == 1, "Not White", "White")) %>%
    relocate(variable) %>%
    select(-nonWhite) %>%
    arrange(variable)

ageKnowledge <- analysisData %>%
    group_by(age) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = age) %>%
    relocate(variable) %>%
    select(-age) %>%
    arrange(variable)

wageKnowledge <- analysisData %>%
    group_by(wage) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = wage) %>%
    relocate(variable) %>%
    select(-wage) %>%
    arrange(variable)

bioKnowledge <- analysisData %>%
    group_by(bioHazards) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = ifelse(.data$bioHazards == 1, "Concerned", 
                             "Not Concerned")) %>%
    relocate(variable) %>%
    select(-bioHazards) %>%
    arrange(variable)

chemKnowledge <- analysisData %>%
    group_by(chemHazards) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = ifelse(.data$chemHazards == 1, "Concerned", 
                             "Not Concerned")) %>%
    relocate(variable) %>%
    select(-chemHazards) %>%
    arrange(variable)

physKnowledge <- analysisData %>%
    group_by(physHazards) %>%
    summarize(avgKnowledge = mean(.data$knowledge),
              se = paste0("(", 
                          round(sd(.data$knowledge) / sqrt(n()), 2), 
                          ")")) %>%
    mutate(variable = ifelse(.data$physHazards == 1, "Concerned", 
                             "Not Concerned")) %>%
    relocate(variable) %>%
    select(-physHazards) %>%
    arrange(variable)

knowledgeTable <- rbind(bioKnowledge, chemKnowledge, physKnowledge,
                       genderKnowledge, raceKnowledge, wageKnowledge,
                       ageKnowledge)
    

# Hazards Table ----------------------------------------------------------------
q47Categories <- c("Mold", "Sensitizer/allergens", "Other", "None")
q48Categories <- c("Butane", "Carbon dioxide (CO2)", "Carbon monoxide (CO)",
                   "Indoor air quality", "Pesticides", 
                   "Disinfectants/cleaning chemicals", "Nutrients", 
                   "Corrosive chemicals", "Indoor environmental quality", 
                   "Other", "None")
q49Categories <- c("Flammable/combustible materials", "Compressed gas", 
                   "Occupational injuries", "Ergonomics", 
                   "Walking and working surfaces", "Working at heights", 
                   "Electrical", "Wet locations/excessive water", "Noise", 
                   "Emergencies", "Egress/fire safety", 
                   "Powered industrial trucks (forklifts)", "Lighting hazards",
                   "Machines", "Hand tools", "Personal protective equipment",
                   "Extraction equipment", "Confined spaces",
                   "Workplace violence", "Heat stress", "Other", "None")

q47to49Counts <- cbind(q47Cols, q48Cols, q49Cols) %>%
    summarize(across(everything(), sum)) %>%
    t()

q47to49Percents <- cbind(q47Cols, q48Cols, q49Cols) %>%
    summarize(across(everything(), function (x) { 
        paste0(round(sum(x) / 213, 3) * 100, "%") })) %>%
    t()

hazardsTable <- cbind(c(q47Categories, q48Categories, q49Categories),
                      q47to49Counts, q47to49Percents) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Question") %>%
    rename("Category" = V1, "Count" = V2, "Percent" = V3)
hazardsTable$anyNo <- c(sum(q47Cols$Q47_D), rep(NA, ncol(q47Cols) - 1), 
                        sum(q48Cols$Q48_K), rep(NA, ncol(q48Cols) - 1),
                        sum(q49Cols$Q49_V), rep(NA, ncol(q49Cols) - 1))
hazardsTable$anyYes <- 214 - hazardsTable$anyNo

hazardsTable <- hazardsTable %>% select(Category, Count, Percent, anyYes, anyNo)

