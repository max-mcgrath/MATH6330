library(readxl)
library(dplyr)

# Read in raw data
rawData <- read_excel("DataRaw/Cannabis Workers and COVID_Master_3Feb22.xlsx")

# Remove rows 179 and 180
fullData <- rawData[-c(179, 180), ]

# Reorder / rename columns
reorderedColumns <- fullData %>%
    select(Q1 = QID131, 
           Q2 = QID129, 
           Q3 = "1", 
           Q4 = "2", 
           Q4_TEXT = "2_5_TEXT",
           Q5 = Q4, 
           Q5_TEXT = Q4_6_TEXT, 
           Q6 = Q5, Q7 = Q6, 
           Q7_TEXT = Q6_8_TEXT,
           Q8 = Q7, 
           Q9 = Q8, 
           Q10 = Q88, 
           Q11 = Q89, 
           Q12 = Q10, 
           Q13 = Q11,
           Q14 = Q90, 
           Q14_TEXT = Q90_1_TEXT, 
           Q15 = Q12, 
           Q16 = Q13, 
           Q17 = Q14,
           Q18 = Q15, 
           Q19 = Q16,
           Q20 = Q17,
           Q20_TEXT = Q17_5_TEXT,
           Q21 = Q18,
           Q22 = Q19,
           Q23 = Q20,
           Q23_TEXT = Q20_8_TEXT,
           Q24 = Q21,
           Q25 = Q22,
           Q26 = Q23,
           Q27 = Q25,
           Q27_TEXT = Q25_6_TEXT,
           Q28 = Q26,
           Q28_TEXT = Q26_5_TEXT,
           Q29 = Q86,
           Q29_TEXT = Q86_1_TEXT,
           Q30 = Q28,
           Q31 = Q29,
           Q32 = Q30,
           Q32_TEXT = Q30_1_TEXT,
           Q33 = Q31,
           Q34 = Q128,
           Q35 = Q32,
           Q36 = Q33,
           Q37 = Q34,
           Q38 = Q35,
           Q38_TEXT = Q35_1_TEXT,
           Q39 = Q36,
           Q40 = Q37,
           Q41 = Q38,
           Q42 = Q39,
           Q43 = Q93,
           Q44 = Q94,
           Q45 = Q40,
           Q46 = Q95,
           Q47 = Q44,
           Q47_TEXT = Q44_3_TEXT,
           Q48 = Q45,
           Q48_TEXT = Q45_8_TEXT,
           Q49 = Q46,
           Q49_TEXT = Q46_20_TEXT,
           Q50 = Q48,
           Q51 = Q104,
           Q52 = Q49,
           Q52_TEXT = Q49_6_TEXT,
           Q53 = Q50,
           Q54 = Q51,
           Q51_TEXT_A = Q51_4_TEXT,
           Q51_TEXT_B = Q51_5_TEXT,
           Q51_TEXT_C = Q51_6_TEXT,
           Q55 = Q52,
           Q56 = Q53,
           Q56_TEXT_A = Q53_4_TEXT,
           Q56_TEXT_B = Q53_5_TEXT,
           Q56_TEXT_C = Q53_6_TEXT,
           Q57 = Q85,
           Q58 = Q84,
           Q59 = Q55,
           Q60 = Q113,
           Q61 = Q115,
           Q62 = Q116,
           Q63 = Q117,
           Q64 = Q118,
           Q65 = Q119,
           Q66 = Q114,
           Q67 = Q111,
           Q68 = Q101,
           Q69 = Q103,
           Q69_TEXT = Q103_5_TEXT,
           Q70 = Q107,
           Q71 = Q108,
           Q72 = Q109,
           Q73 = Q105,
           Q74 = Q110,
           Q75 = Q120,
           Q75_TEXT = Q120_5_TEXT,
           Q76 = Q121,
           Q77 = Q123,
           Q78 = Q122,
           Q79 = Q124,
           Q80 = Q98,
           Q80_TEXT = Q98_1_TEXT,
           Q81 = Q96,
           Q81_TEXT = Q96_1_TEXT,
           Q82 = Q99,
           Q82_TEXT = Q99_1_TEXT,
           Q83 = Q97,
           Q83_TEXT = Q97_1_TEXT,
           Q84 = Q129,
           Q84_TEXT = Q129_1_TEXT,
           Q85 = Q125,
           Q86 = Q126,
           Q87 = Q87,
           Q88 = Q57,
           Q89 = Q106,
           Q90 = Q58,
           Q91 = Q59,
           Q92 = Q60,
           Q93 = Q61,
           Q94 = Q62,
           Q95 = Q63,
           Q96 = Q64,
           Q97 = Q65,
           Q98 = Q66,
           Q99 = Q67,
           Q100 = Q68,
           Q101 = Q69,
           Q102 = Q70)

# Fix column Q22 (Excel converted to dates)
fixedQ22Data <- reorderedColumns %>%
    mutate(Q22 = case_when(.data$Q22 == "44689" ~ "5 to 8",
                           .data$Q22 == "44565" ~ "1 to 4",
                           TRUE ~ .data$Q22))

# Fix NAs (Some observations have NA as "N/A")
fixedNAData <- fixedQ22Data
fixedNAData[fixedQ22Data == "N/A"] <- NA
fixedNAData[fixedQ22Data == "N.a"] <- NA
fixedNAData[fixedQ22Data == "N/a"]

# Store and write cleaned data
cleanData <- fixedNAData
# write.csv(cleanData, "DataProcessed/cleanData.csv", row.names = FALSE,
#           na = "")

# Data analysis prep -----------------------------------------------------------

# Remove and store column questions
questions <- cleanData[1, ]
removedQData <- cleanData[-1, ]

# Select TEXT Columns
textColumns <- cleanData %>%
    select(contains("TEXT"))
textQuestions <- textColumns[1, ]
textColumns <- textColumns[-1, ]

# Select categorical columns
catColumns <- cleanData %>%
    select(!contains("TEXT"), -Q47, -Q48, -Q49, -Q52)
catQuestions <- catColumns[1, ]
catColumns <- catColumns[-1, ]

# Break out multiple response questions (Q47, Q48, Q49, Q52)
multRespCols <- cleanData %>%
    select(Q47, Q48, Q49, Q52)
multRespQuestions <- multRespCols[1, ]
multRespCols <- multRespCols[-1, ]

# Mutate all categorical columns into factors
catColumns <- catColumns %>%
    mutate_all(as.factor)

# Create vector of all questions and data frame of  all observations
allQuestions <- cbind(catQuestions, textQuestions, multRespQuestions)
allQuestions <- allQuestions %>% select(colnames(reorderedColumns))
allData <- cbind(catColumns, textColumns, multRespCols)
allData <- allData %>% select(colnames(reorderedColumns))
