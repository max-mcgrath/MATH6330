source("Code/1_cleanData.R")

# Create additional columns for multiple response questions --------------------
# Q47
q47Cols <- allData %>%
    mutate(Q47_A = as.numeric(grepl("Mold", .data$Q47, fixed = TRUE)),
           Q47_B = as.numeric(grepl("Sensitizer/allergens", .data$Q47, 
                                    fixed = TRUE)),
           Q47_C = as.numeric(grepl("Other", .data$Q47, fixed = TRUE)),
           Q47_D = as.numeric(grepl("None", .data$Q47, fixed = TRUE)),
           .keep = "none")

# Q48
q48Cols <-  allData %>%
    mutate(Q48_A = as.numeric(grepl("Butane", .data$Q48, fixed = TRUE)),
           Q48_B = as.numeric(grepl("Carbon dioxide (CO2)", .data$Q48, fixed = TRUE)),
           Q48_C = as.numeric(grepl("Carbon monoxide (CO)", .data$Q48, fixed = TRUE)),
           Q48_D = as.numeric(grepl("Indoor air quality", .data$Q48, fixed = TRUE)),
           Q48_E = as.numeric(grepl("Pesticides", .data$Q48, fixed = TRUE)),
           Q48_F = as.numeric(grepl("Disinfectants/cleaning chemicals", .data$Q48, fixed = TRUE)),
           Q48_G = as.numeric(grepl("Nutrients", .data$Q48, fixed = TRUE)),
           Q48_H = as.numeric(grepl("Corrosive chemicals", .data$Q48, fixed = TRUE)),
           Q48_I = as.numeric(grepl("Indoor environmental quality", .data$Q48, fixed = TRUE)),
           Q48_J = as.numeric(grepl("Other", .data$Q48, fixed = TRUE)),
           Q48_K = as.numeric(grepl("None", .data$Q48, fixed = TRUE)),
           .keep = "none")

# Q49
q49Cols <-  allData %>%
    mutate(Q49_A = as.numeric(grepl("Flammable/combustible materials", .data$Q49, fixed = TRUE)),
           Q49_B = as.numeric(grepl("Compressed gas", .data$Q49, fixed = TRUE)),
           Q49_C = as.numeric(grepl("Occupational injuries", .data$Q49, fixed = TRUE)),
           Q49_D = as.numeric(grepl("Ergonomics", .data$Q49, fixed = TRUE)),
           Q49_E = as.numeric(grepl("Walking and working surfaces", .data$Q49, fixed = TRUE)),
           Q49_F = as.numeric(grepl("Working at heights", .data$Q49, fixed = TRUE)),
           Q49_G = as.numeric(grepl("Electrical", .data$Q49, fixed = TRUE)),
           Q49_H = as.numeric(grepl("Wet locations/excessive water", .data$Q49, fixed = TRUE)),
           Q49_I = as.numeric(grepl("Noise", .data$Q49, fixed = TRUE)),
           Q49_J = as.numeric(grepl("Emergencies", .data$Q49, fixed = TRUE)),
           Q49_K = as.numeric(grepl("Egress/fire safety", .data$Q49, fixed = TRUE)),
           Q49_L = as.numeric(grepl("Powered industrial trucks (forklifts)", .data$Q49, fixed = TRUE)),
           Q49_M = as.numeric(grepl("Lighting hazards", .data$Q49, fixed = TRUE)),
           Q49_N = as.numeric(grepl("Machines", .data$Q49, fixed = TRUE)),
           Q49_O = as.numeric(grepl("Hand tools", .data$Q49, fixed = TRUE)),
           Q49_P = as.numeric(grepl("Personal protective equipment (respiratory, eye, hearing and hand protections)", .data$Q49, fixed = TRUE)),
           Q49_Q = as.numeric(grepl("Extraction equipment", .data$Q49, fixed = TRUE)),
           Q49_R = as.numeric(grepl("Confined spaces", .data$Q49, fixed = TRUE)),
           Q49_S = as.numeric(grepl("Workplace violence", .data$Q49, fixed = TRUE)),
           Q49_T = as.numeric(grepl("Heat stress", .data$Q49, fixed = TRUE)),
           Q49_U = as.numeric(grepl("Other", .data$Q49, fixed = TRUE)),
           Q49_V = as.numeric(grepl("None", .data$Q49, fixed = TRUE)),
           .keep = "none")

# Q52
q52Cols <-  allData %>%
    mutate(Q52_A = as.numeric(grepl("Products that contain mold", .data$Q52, fixed = TRUE)),
           Q52_B = as.numeric(grepl("Products that contain pesticides", .data$Q52, fixed = TRUE)),
           Q52_C = as.numeric(grepl("Products that contain insects", .data$Q52, fixed = TRUE)),
           Q52_D = as.numeric(grepl("Products that contain rodent droppings", .data$Q52, fixed = TRUE)),
           Q52_E = as.numeric(grepl("Products that are incorrectly labeled", .data$Q52, fixed = TRUE)),
           Q52_F = as.numeric(grepl("Other", .data$Q52, fixed = TRUE)),
           Q52_G = as.numeric(grepl("None", .data$Q52, fixed = TRUE)),
           .keep = "none")

# Add new question text for additional columns