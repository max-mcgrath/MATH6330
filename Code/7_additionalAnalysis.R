# Sensitivity analysis for Knowledge model -------------------------------------
set.seed(123)
randomSampleIndeces <- sample(1:205, 195)

randomSample <- analysisData[randomSampleIndeces, ]

# Rerun analysis, see if similar conclusions
fullModel <- lm(knowledge ~ nonWhite + female + age + wage, data = randomSample)
interceptModel <- lm(knowledge ~ 1, data = randomSample)
forwardSelection <- MASS::stepAIC(interceptModel, direction = "forward",
                                  scope = list(lower = interceptModel,
                                               upper = fullModel),
                                  k = log(ncol(randomSample)))
summary(forwardSelection)

# Again - different seed
set.seed(124)
randomSampleIndeces <- sample(1:205, 195)

randomSample <- analysisData[randomSampleIndeces, ]

# Rerun analysis, see if similar conclusions
fullModel <- lm(knowledge ~ nonWhite + female + age + wage, data = randomSample)
interceptModel <- lm(knowledge ~ 1, data = randomSample)
forwardSelection <- MASS::stepAIC(interceptModel, direction = "forward",
                                  scope = list(lower = interceptModel,
                                               upper = fullModel),
                                  k = log(ncol(randomSample)))
summary(forwardSelection)

# Again - different seed
set.seed(125)
randomSampleIndeces <- sample(1:205, 195)

randomSample <- analysisData[randomSampleIndeces, ]

# Rerun analysis, see if similar conclusions
fullModel <- lm(knowledge ~ nonWhite + female + age + wage, data = randomSample)
interceptModel <- lm(knowledge ~ 1, data = randomSample)
forwardSelection <- MASS::stepAIC(interceptModel, direction = "forward",
                                  scope = list(lower = interceptModel,
                                               upper = fullModel),
                                  k = log(ncol(randomSample)))
summary(forwardSelection)

# Again - different seed
set.seed(126)
randomSampleIndeces <- sample(1:205, 195)

randomSample <- analysisData[randomSampleIndeces, ]

# Rerun analysis, see if similar conclusions
fullModel <- lm(knowledge ~ nonWhite + female + age + wage, data = randomSample)
interceptModel <- lm(knowledge ~ 1, data = randomSample)
forwardSelection <- MASS::stepAIC(interceptModel, direction = "forward",
                                  scope = list(lower = interceptModel,
                                               upper = fullModel),
                                  k = log(ncol(randomSample)))
summary(forwardSelection)

# Again - different seed
set.seed(127)
randomSampleIndeces <- sample(1:205, 195)

randomSample <- analysisData[randomSampleIndeces, ]

# Rerun analysis, see if similar conclusions
fullModel <- lm(knowledge ~ nonWhite + female + age + wage, data = randomSample)
interceptModel <- lm(knowledge ~ 1, data = randomSample)
forwardSelection <- MASS::stepAIC(interceptModel, direction = "forward",
                                  scope = list(lower = interceptModel,
                                               upper = fullModel),
                                  k = log(ncol(randomSample)))
summary(forwardSelection)

# Check for associations between "No" answers and each variable to try to 
#   evaluate if any groups are more inclined to guess
fullModel <- lm(knowledgeNo ~ nonWhite + female + age + wage, 
                data = analysisData)
interceptModel <- lm(knowledgeNo ~ 1, data = analysisData)
forwardSelection <- MASS::stepAIC(interceptModel, direction = "forward",
                                  scope = list(lower = interceptModel,
                                               upper = fullModel),
                                  k = log(ncol(analysisData)))
summary(fullModel)
plot(forwardSelection)

analysisData %>%
    group_by(female) %>%
    summarize(mean(knowledgeNo))
analysisData %>%
    group_by(wage) %>%
    summarize(mean(knowledgeNo))
analysisData %>%
    group_by(nonWhite) %>%
    summarize(mean(knowledgeNo))
analysisData %>%
    group_by(age) %>%
    summarize(mean(knowledgeNo))

# Sensitivity test for unionization models -------------------------------------
set.seed(123)
randomSampleIndeces <- sample(1:205, 195)
randomSample <- analysisData[randomSampleIndeces, ]

fullModelSafety <- 
    glm(unionForSafety ~ bioHazards + chemHazards + physHazards + nonWhite + 
            female + age + wage,
        family = binomial, data = randomSample)
interceptModelSafety <- 
    glm(unionForSafety ~ 1, family = binomial, data = randomSample)
forwardSelectionSafety <- 
    MASS::stepAIC(interceptModelSafety, direction = "forward",
                  scope = list(lower = interceptModelSafety, 
                               upper = fullModelSafety),
                  k = log(ncol(randomSample)))
summary(forwardSelectionSafety)

set.seed(127)
randomSampleIndeces <- sample(1:205, 195)
randomSample <- analysisData[randomSampleIndeces, ]

fullModelSafety <- 
    glm(unionForWages ~ bioHazards + chemHazards + physHazards + nonWhite + 
            female + age + wage,
        family = binomial, data = randomSample)
interceptModelSafety <- 
    glm(unionForSafety ~ 1, family = binomial, data = randomSample)
forwardSelectionSafety <- 
    MASS::stepAIC(interceptModelSafety, direction = "forward",
                  scope = list(lower = interceptModelSafety, 
                               upper = fullModelSafety),
                  k = log(ncol(randomSample)))
summary(forwardSelectionSafety)

# Pairwise associations between Unionism/Hazards -------------------------------
# BioHazards retained, positive, significant
fullModel <- 
    glm(unionForSafety ~ bioHazards + nonWhite + female + age + wage,
        family = binomial, data = analysisData)
interceptModel <- 
    glm(unionForSafety ~ 1, family = binomial, data = analysisData)
forwardSelection <- 
    MASS::stepAIC(interceptModel, direction = "forward",
                  scope = list(lower = interceptModel, 
                               upper = fullModel),
                  k = log(ncol(analysisData)))
summary(forwardSelection)

# ChemHazards retained, positive, significant
fullModel <- 
    glm(unionForSafety ~ chemHazards + nonWhite + female + age + wage,
        family = binomial, data = analysisData)
interceptModel <- 
    glm(unionForSafety ~ 1, family = binomial, data = analysisData)
forwardSelection <- 
    MASS::stepAIC(interceptModel, direction = "forward",
                  scope = list(lower = interceptModel, 
                               upper = fullModel),
                  k = log(ncol(analysisData)))
summary(forwardSelection)


# BioHazards retained and positive
fullModel <- 
    glm(unionForWages ~ bioHazards + nonWhite + female + age + wage,
        family = binomial, data = analysisData)
interceptModel <- 
    glm(unionForWages ~ 1, family = binomial, data = analysisData)
forwardSelection <- 
    MASS::stepAIC(interceptModel, direction = "forward",
                  scope = list(lower = interceptModel, 
                               upper = fullModel),
                  k = log(ncol(analysisData)))
summary(forwardSelection)

# ChemHazards retained, positive, significant
fullModel <- 
    glm(unionForWages ~ chemHazards + nonWhite + female + age + wage,
        family = binomial, data = analysisData)
interceptModel <- 
    glm(unionForWages ~ 1, family = binomial, data = analysisData)
forwardSelection <- 
    MASS::stepAIC(interceptModel, direction = "forward",
                  scope = list(lower = interceptModel, 
                               upper = fullModel),
                  k = log(ncol(analysisData)))
summary(forwardSelection)


# BioHazards retained and positive
fullModel <- 
    glm(unionForBenefits ~ bioHazards + nonWhite + female + age + wage,
        family = binomial, data = analysisData)
interceptModel <- 
    glm(unionForBenefits ~ 1, family = binomial, data = analysisData)
forwardSelection <- 
    MASS::stepAIC(interceptModel, direction = "forward",
                  scope = list(lower = interceptModel, 
                               upper = fullModel),
                  k = log(ncol(analysisData)))
summary(forwardSelection)

# ChemHazards retained, positive, not significant
fullModel <- 
    glm(unionForBenefits ~ chemHazards + nonWhite + female + age + wage,
        family = binomial, data = analysisData)
interceptModel <- 
    glm(unionForBenefits ~ 1, family = binomial, data = analysisData)
forwardSelection <- 
    MASS::stepAIC(interceptModel, direction = "forward",
                  scope = list(lower = interceptModel, 
                               upper = fullModel),
                  k = log(ncol(analysisData)))
summary(forwardSelection)

# Look at average number of concerns within each category ----------------------
# Biological
mean(as.numeric(hazardsTable[1:3, ]$Count))

# Chemical
mean(as.numeric(hazardsTable[5:14, ]$Count))

# Physical
mean(as.numeric(hazardsTable[16:35, ]$Count))
