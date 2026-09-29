library(dplyr)

#' Determine handedness with fallback hierarchy
process_handedness <- function(data) {
  data <- data %>%
    mutate(participant_id = as.character(participant_id))
  
  one_handed <- "OneHanded"
  asymmetrical_types <- c("AsymmetricalSameHandshape", 
                          "AsymmetricalDifferentHandshape",
                          "DominanceViolation", 
                          "SymmetryViolation")
  symmetrical_type <- "SymmetricalOrAlternating"
  
  # Build handedness lookup
  handedness_lookup <- data %>%
    group_by(participant_id) %>%
    summarise(
      one_handed_n = sum(SignType == one_handed, na.rm = TRUE),
      one_handed_right_y = median(rwristY[SignType == one_handed], na.rm = TRUE),
      one_handed_left_y = median(lwristY[SignType == one_handed], na.rm = TRUE),
      
      asym_n = sum(SignType %in% asymmetrical_types, na.rm = TRUE),
      asym_right_y = median(rwristY[SignType %in% asymmetrical_types], na.rm = TRUE),
      asym_left_y = median(lwristY[SignType %in% asymmetrical_types], na.rm = TRUE),
      
      sym_n = sum(SignType == symmetrical_type, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      across(ends_with("_y"), ~ifelse(is.nan(.), NA_real_, .)),
      Handedness = case_when(
        one_handed_n > 0 ~ case_when(
          one_handed_right_y > one_handed_left_y ~ "Right",
          one_handed_left_y > one_handed_right_y ~ "Left",
          TRUE ~ "Right"
        ),
        asym_n > 0 ~ case_when(
          asym_right_y > asym_left_y ~ "Right",
          asym_left_y > asym_right_y ~ "Left",
          TRUE ~ "Right"
        ),
        sym_n > 0 ~ "Right",
        TRUE ~ "Right"
      ),
      handedness_reason = case_when(
        one_handed_n > 0 ~ "OneHanded",
        asym_n > 0 ~ "Asymmetrical",
        sym_n > 0 ~ "SymmetricalOrAlternating",
        TRUE ~ "NoData"
      )
    ) %>%
    select(participant_id, Handedness, handedness_reason)
  
  # Explicit join - store result in new object
  joined_data <- left_join(data, handedness_lookup, by = "participant_id")
  
  # Check if join worked
  if (!"Handedness" %in% names(joined_data)) {
    stop("Join failed: Handedness column not created. Check participant_id values.")
  }
  
  # Add dominant hand columns + flip shoulders with temp variables
  result <- joined_data %>%
    mutate(
      dominant_x = if_else(Handedness == "Right", rwristX, lwristX),
      dominant_y = if_else(Handedness == "Right", rwristY, lwristY)
    ) %>%
    # Swap shoulders for left-handed signers (use temp variables)
    mutate(
      lshoulderX_swap = if_else(Handedness == "Left", rshoulderX, lshoulderX),
      rshoulderX_swap = if_else(Handedness == "Left", lshoulderX, rshoulderX),
      lshoulderX = lshoulderX_swap,
      rshoulderX = rshoulderX_swap
    ) %>%
    select(-lshoulderX_swap, -rshoulderX_swap) %>%
    # Flip X coordinates for left-handed signers
    mutate(
      dominant_x = if_else(Handedness == "Left", (1 - dominant_x), dominant_x),
      noseX = if_else(Handedness == "Left", (1 - noseX), noseX),
      lshoulderX = if_else(Handedness == "Left", (1 - lshoulderX), lshoulderX),
      rshoulderX = if_else(Handedness == "Left", (1 - rshoulderX), rshoulderX)
    )
  return(result)
}



count_handedness_na <- function(data) {
  sum(is.na(data$Handedness))
}

summarize_handedness <- function(data) {
  data %>%
    distinct(participant_id, Handedness, handedness_reason) %>%
    count(Handedness, handedness_reason, sort = TRUE)
}