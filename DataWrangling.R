library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(data.table)

# load and merge data ----

rds_path <- here("data", "redux_data.rds") # path to save the RDS file
data_dir <- here("data", "mp_data_run_by_amelia") # path to the directory containing the CSV files

if (file.exists(rds_path)) { # if the RDS file exists, load it instead of reading the CSV files
  message("Loading existing redux_data from RDS...") # and print message saying that the RDS file is being loaded
  redux_data <- readRDS(rds_path) # load the RDS file
} else { # otherwise 
  message("RDS not found. Reading CSV files...") # otherwise, print message saying that the CSV files are being read
  
# save the list of all CSV files in the data_dir and its subdirectories
  
files <- list.files(data_dir, 
                      pattern = "\\.csv$", 
                      full.names = TRUE, 
                      recursive = TRUE)

# print the number of CSV files found
message(paste("Found", length(files), "CSV files"))

# Read all CSV files with custom column names
  redux_data <- lapply(files, function(file) {
    df <- data.table::fread(file, 
                            header = TRUE, 
                            na.strings = c("", "NA"))
    
    df$participant_id <- gsub("_.*", "", basename(file))
    df$cue_gloss <- gsub("^\\d+_", "", basename(file))
    df$cue_gloss <- gsub("_response-1-poses\\.csv$", "", df$cue_gloss)
    
    return(df)
  })
  
  redux_data <- data.table::rbindlist(redux_data, use.names = TRUE, fill = TRUE)
  
  message(paste("Loaded", nrow(redux_data), "rows"))
  
  saveRDS(redux_data, rds_path)
  message("Saved to redux_data.rds")
}

LEX <- read.csv(here("data", "LEX2.0.csv")) # load ASL-LEX data

LEX$MajorLocation.2.0[LEX$EntryID == "lick_2"] <- "Hand" # the sign "lick_2" has incorrect MajorLocation category; change to Hand

# remove SignBankEnglishTranslation column from LEX because it's causing problems
LEX <- LEX %>%
  select(-SignBankEnglishTranslations)

# check for duplicated EntryIDs in LEX
lex_duplicates <- LEX %>%
  group_by(EntryID) %>%
  filter(n() > 1) %>%
  ungroup()

# duplicate codes for take_off, bite, and drag are present in LEX and in metadata; 
# duplicates are identical videos in lex

# keep only first instances of duplicated EntryIDs in LEX
LEX <- LEX %>%
  group_by(EntryID) %>%
  slice(1) %>%
  ungroup()

metadata <- read.csv(here("data", "semassoc_metadata.csv")) # load Sem-Lex metadata

metadata <- metadata %>%
  filter(Response.Order == 0) %>% # keep only response_order 0 in metadata
  filter(Tag.Text != "") # remove rows from metadata where Tag.Text is empty

# for take_off, bite, and drag, change Sign.ID in metadata to match the current non-duplicated Code in LEX for those EntryIDs

metadata$Sign.ID[metadata$Sign.Name == "take_off"] <- LEX$Code[LEX$EntryID == "take_off"]
metadata$Sign.ID[metadata$Sign.Name == "bite"] <- LEX$Code[LEX$EntryID == "bite"]
metadata$Sign.ID[metadata$Sign.Name == "drag"] <- LEX$Code[LEX$EntryID == "drag"]

# clean some inconsistently tagged responses in metadata: remove rows from metadata where there are different Tag.Text labels for the same response 

inconsistent_labels <- metadata %>%
  group_by(Response.ID) %>%
  filter(n() > 1) %>%
  filter(n_distinct(Tag.Text) > 1) %>%
  ungroup()

metadata <- metadata %>%
  filter(!Response.ID %in% inconsistent_labels$Response.ID)

# double check for inconsistent labels

duplicates <- metadata %>%
  group_by(Response.ID) %>%
  filter(n() > 1) %>%
  ungroup()

inconsistent_labels <- duplicates %>% # should have 0 observations
  group_by(Response.ID) %>%
  filter(n_distinct(Tag.Text) > 1) %>%
  ungroup()

# now, keep only the first row for each Response.ID in metadata

metadata <- metadata %>%
  group_by(Response.ID) %>%
  slice(1) %>%
  ungroup()

# check if there are duplicates of unique combinations of participant_id and cue_gloss and remove them
duplicate_cues <- metadata %>%
  group_by(Responder.ID, Sign.ID) %>%
  filter(n() > 1) %>%
  ungroup()

metadata <- metadata %>%
  filter(!Response.ID %in% duplicate_cues$Response.ID)

# keep only rows in metadata where Response.Valid is TRUE and Response.Duration.Flagged is FALSE

metadata <- metadata %>%
  filter(Response.Valid == TRUE & Response.Duration.Flagged == FALSE)

# join from LEX to redux_data based on cue_gloss matching EntryID, and rename the new column as CueID
redux_data <- redux_data %>%
  left_join(LEX %>% select(EntryID, Code), by = c("cue_gloss" = "EntryID")) %>%
  rename(CueID = Code)

# change Responder.ID to participant_id in metadata
metadata <- metadata %>%
  rename(participant_id = Responder.ID)

# make metadata$participant_id a character vector
metadata$participant_id <- as.character(metadata$participant_id)

# merge redux_data with metadata, where CueID == Sign.ID & participant_id == participant_id

redux_data <- redux_data %>%
  left_join(metadata, by = c("CueID" = "Sign.ID", "participant_id" = "participant_id"))

# change name of Tag.Text in redux_data to ResponseCode
redux_data <- redux_data %>%
  rename(ResponseCode = Tag.Text)

# merge redux_data with LEX so that ResponseCode in redux_data matches Code in LEX
redux_LEX <- redux_data %>%
  inner_join(LEX %>% select(Code, EntryID, SignFreq.Z.native., Iconicity.Z., 
                            LexicalClass, MajorLocation.2.0, MinorLocation.2.0, 
                            Neighborhood.Density.2.0, SignType.2.0, 
                            NumberOfMorphemes.2.0, Compound.2.0), 
             by = c("ResponseCode" = "Code"))

# rename some columns
redux_LEX <- redux_LEX %>%
rename(Frequency = SignFreq.Z.native., Iconicity = Iconicity.Z., 
         LexicalClass = LexicalClass, MajorLocation = MajorLocation.2.0, 
         MinorLocation = MinorLocation.2.0, 
         NeighborhoodDensity = Neighborhood.Density.2.0, 
         SignType = SignType.2.0, NumberOfMorphemes = NumberOfMorphemes.2.0, 
         Compound = Compound.2.0)

# keep only needed columns
redux_LEX <- redux_LEX %>%
   select(participant_id, CueID, cue_gloss, Response.ID, 
          Session.Prompt.Number, Tag.ID, EntryID, Frequency, LexicalClass, 
          MajorLocation, MinorLocation, NeighborhoodDensity, SignType, 
          NumberOfMorphemes, Compound, 
          rwristX, rwristY, lwristX, lwristY, rshoulderX, rshoulderY, lshoulderX, 
          lshoulderY, noseX, noseY, rwristPres, lwristPres, rshoulderPres, 
          lshoulderPres, nosePres)

# inspect Presence values by plotting wrist coordinates with color gradient by rwristPres
ggplot(redux_LEX, aes(x = rwristX, y = rwristY,
  color = rwristPres)) +
  geom_point(alpha = 0.5) +
  labs(title = "Right Wrist Coordinates by Presence Values",
       x = "right wrist x",
       y = "right wrist y") +
  scale_color_gradient(low = "black", high = "pink", na.value = "grey50") +
  theme_minimal()                      

ggplot(redux_LEX, aes(x = lwristX, y = lwristY,
                      color = lwristPres)) +
  geom_point(alpha = 0.5) +
  labs(title = "Left Wrist Coordinates by Presence Values",
       x = "left wrist x",
       y = "left wrist y") +
  scale_color_gradient(low = "black", high = "pink", na.value = "grey50") +
  theme_minimal()                      

# replace out of frame keypoints (either X or Y is outside 0 - 1) with NA ----
 
redux_LEX <- redux_LEX %>%
  mutate(
    across(
      c(rwristX, rwristY),
      ~ ifelse(rwristX    < 0 |
                 rwristX    > 1 | rwristY    < 0 | rwristY    > 1, NA, .)
    ),
    across(
      c(lwristX, lwristY),
      ~ ifelse(lwristX    < 0 |
                 lwristX    > 1 | lwristY    < 0 | lwristY    > 1, NA, .)
    ),
    across(
      c(rshoulderX, rshoulderY),
      ~ ifelse(rshoulderX < 0 |
                 rshoulderX > 1 | rshoulderY < 0 | rshoulderY > 1, NA, .)
    ),
    across(
      c(lshoulderX, lshoulderY),
      ~ ifelse(lshoulderX < 0 |
                 lshoulderX > 1 | lshoulderY < 0 | lshoulderY > 1, NA, .)
    ),
    across(
      c(noseX, noseY),
      ~ ifelse(noseX      < 0 |
                 noseX      > 1 | noseY      < 0 | noseY      > 1, NA, .)
    )
  )
 
# check that new ranges are all between 0-1

range(redux_LEX$rwristX, na.rm = TRUE)
range(redux_LEX$rwristY, na.rm = TRUE) 
range(redux_LEX$lwristX, na.rm = TRUE)
range(redux_LEX$lwristY, na.rm = TRUE)
range(redux_LEX$rshoulderX, na.rm = TRUE)
range(redux_LEX$rshoulderY, na.rm = TRUE)
range(redux_LEX$lshoulderX, na.rm = TRUE)
range(redux_LEX$lshoulderY, na.rm = TRUE)
range(redux_LEX$noseX, na.rm = TRUE)
range(redux_LEX$noseY, na.rm = TRUE)

# create a table with #s of rows where relevant keypoints are NA

na_counts <- redux_LEX %>%
  summarise(
    right_wrist_x_na = sum(is.na(rwristX)),
    left_wrist_x_na = sum(is.na(lwristX)),
    right_wrist_y_na = sum(is.na(rwristY)),
    left_wrist_y_na = sum(is.na(lwristY)),
    right_shoulder_x_na = sum(is.na(rshoulderX)),
    left_shoulder_x_na = sum(is.na(lshoulderX)),
    right_shoulder_y_na = sum(is.na(rshoulderY)),
    left_shoulder_y_na = sum(is.na(lshoulderY)),
    nose_x_na = sum(is.na(noseX)),
    nose_y_na = sum(is.na(noseY))
  )

# flip y values ----
# transform y values so bottom of the screen is 0 and top is 1 
redux_LEX <- redux_LEX %>%
  mutate(
    rwristY = 1 - rwristY,
    lwristY = 1 - lwristY,
    rshoulderY = 1 - rshoulderY,
    lshoulderY = 1 - lshoulderY,
    noseY = 1 - noseY
  )

# replot keypoints to inspect inverted y values (y should skew toward 0, i.e., bottom of the screen)

ggplot(redux_LEX, aes(x = rwristX, y = rwristY)) +
  geom_point(alpha = 0.5) +
  labs(title = "Right Wrist Coordinates After Inverting Y",
       x = "right wrist x",
       y = "right wrist y") +
  theme_minimal()

ggplot(redux_LEX, aes(x = lwristX, y = lwristY)) +
  geom_point(alpha = 0.5) +
  labs(title = "Left Wrist Coordinates After Inverting Y",
       x = "left wrist x",
       y = "left wrist y") +
  theme_minimal()

# determine handedness ----

source(here("handedness_function.R"))

redux_LEX <- process_handedness(redux_LEX)

# count how many participants are left or right handed

handedness_counts <- redux_LEX %>%
  group_by(participant_id) %>%
  summarise(Handedness = first(Handedness)) %>%
  group_by(Handedness) %>%
  summarise(count = n())

# list participant_id values by handeness
handedness_participants <- redux_LEX %>%
  group_by(participant_id) %>%
  summarise(Handedness = first(Handedness)) %>%
  group_by(Handedness) %>%
  summarise(participants = list(participant_id))

# fix handedness for participant_id 59 and 60 (both are right-handed); 

redux_LEX <- redux_LEX %>%
  mutate(
    Handedness = ifelse(participant_id %in% c("59", "60"), "Right", Handedness), # change Handedness to Right for participant_id 59 and 60
    handedness_reason = ifelse(participant_id %in% c("59", "60"), "ManualOverride", handedness_reason), # change handedness_reason to ManualOverride for participant_id 59 and 60
    dominant_x = ifelse(participant_id %in% c("59", "60"), rwristX, dominant_x), # change dominant_x values to be rwristX values
    dominant_y = ifelse(participant_id %in% c("59", "60"), rwristY, dominant_y),
    # Un-flip AND un-swap shoulders (using temp variables)
    lshoulderX_temp = ifelse(participant_id %in% c("59", "60"), 1 - rshoulderX, lshoulderX), # change lshoulderX values to be 1 - rshoulderX values
    rshoulderX_temp = ifelse(participant_id %in% c("59", "60"), 1 - lshoulderX, rshoulderX),
    lshoulderX = lshoulderX_temp, # change lshoulderX values to be lshoulderX_temp values
    rshoulderX = rshoulderX_temp,
    noseX = ifelse(participant_id %in% c("59", "60"), 1 - noseX, noseX) # change noseX values to be 1 - noseX values
  ) %>%
  select(-lshoulderX_temp, -rshoulderX_temp) # remove temp variables

# replot to inspect (should now look like everyone is right-handed)

ggplot(redux_LEX, aes(x = dominant_x, y = dominant_y)) +
  geom_point(alpha = 0.5) +
  labs(title = "Dominant Wrist Coordinates after flipping left-handed participants",
       x = "dominant wrist x",
       y = "dominant wrist y") +
  theme_minimal()

# plot nose x and y with color according to signer handedness
ggplot(redux_LEX, aes(x = noseX, y = noseY, color = Handedness)) +
  geom_point(alpha = 0.1) +
  labs(title = "Nose Coordinates by Signer Handedness \nafter flipping left-handed signers",
       x = "nose x",
       y = "nose y") +
  scale_color_manual(values = c("Left" = "blue", "Right" = "red")) +
  theme_minimal()

# left shoulder
ggplot(redux_LEX, aes(x = lshoulderX, y = lshoulderY, color = Handedness)) +
  geom_point(alpha = 0.1) +
  labs(title = "Left Shoulder Coordinates by Signer Handedness \nafter flipping left-handed signers",
       x = "left shoulder x",
       y = "left shoulder y") +
 scale_color_manual(values = c("Left" = "blue", "Right" = "red")) +
  theme_minimal()

# right shoulder
ggplot(redux_LEX, aes(x = rshoulderX, y = rshoulderY, color = Handedness)) +
  geom_point(alpha = 0.1) +
  labs(title = "Right Shoulder Coordinates by Signer Handedness \nafter flipping left-handed signers",
       x = "right shoulder x",
       y = "right shoulder y") +
  scale_color_manual(values = c("Left" = "blue", "Right" = "red")) +
  theme_minimal()

# trim to mid third ----
# add row called frame_number to redux_LEX that is the row number within each Response.ID
redux_LEX <- redux_LEX %>%
  group_by(Response.ID) %>%
  mutate(frame_number = row_number()) %>%
  ungroup()

# create a df with only the middle 1/3 of frames in each video

mid_third <- redux_LEX %>%
  group_by(Response.ID) %>%
  arrange(frame_number) %>%
  mutate(
    total_frames = n(),
    start_cutoff = floor(total_frames / 3),
    end_cutoff = ceiling(2 * total_frames / 3)
  ) %>%
  filter(frame_number > start_cutoff & frame_number <= end_cutoff) %>%
  ungroup() %>%
  select(-total_frames, -start_cutoff, -end_cutoff)

# clean up some NA frames ----

# find responses that are fully NA for the dominant wrist

fully_na_dominant_wrist <- mid_third %>%
  group_by(Response.ID) %>%
  summarise(
    total_rows = n(),
    rows_with_na = sum(is.na(dominant_x) | is.na(dominant_y)),
    all_na = rows_with_na == total_rows,
    .groups = "drop"
  ) %>%
  filter(all_na)

# remove fully na responses from mid_third 
mid_third <- mid_third %>% 
  filter(!Response.ID %in% fully_na_dominant_wrist$Response.ID)

# create column that gives ratio of frames that are NA for dominant wrist for each Response.ID in mid_third
mid_third <- mid_third %>%
  group_by(Response.ID) %>%
  mutate(
    na_ratio = sum(is.na(dominant_x)) / n() # fine to use only dominant_x because if dominant_x is NA, dominant_y will also be NA
  ) %>%
  ungroup()

na_ratio_range <- range(mid_third$na_ratio)

# count unique responses in mid_third for which na_ratio is greater than 0.5
over_half_na <- mid_third %>%
  filter(na_ratio > 0.5) %>%
  summarise(count = n_distinct(Response.ID))

# plot distribution of na_ratio for individual responses
ggplot(mid_third, aes(x = na_ratio)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of NA Ratio in Dominant Wrist Values \n after removing fully NA responses",
       x = "Ratio of Frames with NA in Dominant Wrist",
       y = "Count") +
  theme_minimal()

# normalize keypoints ----

# center nose at 0.5,0.5 and shift wrist and shoulders accordingly

nose_centered <- mid_third %>%
  mutate(
    centered_nose_x = 0.5, # create new nose point
    centered_nose_y = 0.5,
    centered_dominant_x = dominant_x + (centered_nose_x - noseX), # shift dominant wrist by difference between original nose and 0.5,0.5
    centered_dominant_y = dominant_y + (centered_nose_y - noseY),
    centered_right_shoulder_x = rshoulderX + (centered_nose_x - noseX), # shift shoulders by difference between original nose and 0.5,0.5
    centered_right_shoulder_y = rshoulderY + (centered_nose_y - noseY),
    centered_left_shoulder_x = lshoulderX + (centered_nose_x - noseX),
    centered_left_shoulder_y = lshoulderY + (centered_nose_y - noseY)
  )

# scale shoulders so that shoulder distance is always 0.5, and shift dominant wrist accordingly

shoulder_scaled <- nose_centered %>%
  mutate(
    shoulder_distance = sqrt( # get current euclidean distance between shoulders
      (centered_right_shoulder_x - centered_left_shoulder_x)^2 +
        (centered_right_shoulder_y - centered_left_shoulder_y)^2
    ),
    scale_factor = 0.5 / shoulder_distance, # calculate scale factor that makes shoulder distance 0.5
    
    # shoulder midpoint as anchor
    shoulder_mid_x = (centered_right_shoulder_x + centered_left_shoulder_x) / 2, # find x midpoint between shoulders
    shoulder_mid_y = (centered_right_shoulder_y + centered_left_shoulder_y) / 2, # find y midpoint between shoulders
    
    # scale shoulders relative to midpoint
    scaled_right_shoulder_x = shoulder_mid_x + # start with current midpoint
      (centered_right_shoulder_x - shoulder_mid_x) * # find distance from midpoint to right shoulder
      scale_factor, # scale that distance by scale factor to find new right shoulder x
    scaled_right_shoulder_y = shoulder_mid_y + (centered_right_shoulder_y - shoulder_mid_y) * scale_factor,
    scaled_left_shoulder_x = shoulder_mid_x + (centered_left_shoulder_x - shoulder_mid_x) * scale_factor,
    scaled_left_shoulder_y = shoulder_mid_y + (centered_left_shoulder_y - shoulder_mid_y) * scale_factor,
    
    # shift wrist by however much the right shoulder moved
    dominant_shoulder_shift_x = scaled_right_shoulder_x - centered_right_shoulder_x, # calculate how much the shoulder has shifted
    dominant_shoulder_shift_y = scaled_right_shoulder_y - centered_right_shoulder_y,
    normal_dominant_x = centered_dominant_x + dominant_shoulder_shift_x, # shift dominant wrist by the same amount as the right shoulder
    normal_dominant_y = centered_dominant_y + dominant_shoulder_shift_y
    
  )

# calculate medians ----

medians <- shoulder_scaled %>%
  group_by(participant_id) %>%
  mutate( # create columns that are the median of the dominant wrist x and y for each participant
    participant_median_x = median(normal_dominant_x, na.rm = TRUE),
    participant_median_y = median(normal_dominant_y, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Response.ID) %>%
  mutate( # create columns that are the median of the dominant wrist x and y for each response
    response_median_x = median(normal_dominant_x, na.rm = TRUE),
    response_median_y = median(normal_dominant_y, na.rm = TRUE)
  ) %>%
  slice(1) %>% # keep just one row per response
  ungroup() %>%
  mutate( # create columns that are the median of the dominant wrist x and y for all responses
    overall_median_x = median(response_median_x, na.rm = TRUE),
    overall_median_y = median(response_median_y, na.rm = TRUE)
  ) %>%
  group_by(MajorLocation) %>%
  mutate( # create columns that are the median of the dominant wrist x and y for each major location
    major_location_median_x = median(response_median_x, na.rm = TRUE),
    major_location_median_y = median(response_median_y, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate( # create a column that has the euclidean distance between the response median and the overall median
    distance_from_overall_median = sqrt((response_median_x - overall_median_x)^2 + (response_median_y - overall_median_y)^2)
  ) %>%
  mutate( # create a column that has the euclidean distance between the response median and the respective major location median for each response
    distance_from_major_location_median = sqrt((response_median_x - major_location_median_x)^2 + (response_median_y - major_location_median_y)^2)
  ) %>%
  mutate( # create a column that has the y distance between the response median and the overall median
    y_distance_from_median = response_median_y - overall_median_y
  ) %>%
  group_by(MajorLocation) %>%
  mutate( # create a column that has the y distance between the response median and the respective major location median for each response
    y_distance_from_major_location_median = response_median_y - major_location_median_y
  ) %>%
  ungroup()

# plot the dominant wrist

ggplot(shoulder_scaled, aes(x = normal_dominant_x, y = normal_dominant_y)) +
  geom_point(alpha = 0.5) +
  labs(title = "Normalized Dominant Wrist Coordinates",
       x = "Normalized Dominant X Coordinate",
       y = "Normalized Dominant Y Coordinate") +
  theme_minimal() 

