### in this script, I will try to transform lists of crops per year per field into crop rotations per field. 
# as always, lets first get the packages we need.
library(sp)
library(arcgisbinding)
library(raster)
library(sf)
library(dplyr)
library(spatialEco)
library(polylabelr)
library(openxlsx)


# next lets set our working directory
setwd("C:/Users/boazg/OneDrive - Wageningen University & Research/2023-2024/THESIS")

## we'll most likely be applying some kind of for-loop, in which we'll analyse every field by itself. So we'll need a vector of the fieldcodes present in the dataset we are analysing. o
monocrops <- st_read(path <- "important_files/crops_in_point.shp")

fieldcodes <- unique(as.vector(monocrops$V3))

## lets try to create a new column that has a more generic name for a crop, namely aardappelen for all rows with some kind of aardappel. 
monocrops <- monocrops %>%
  mutate(
    generic_crop = case_when(
      grepl("Aardappel", GWS_GEWAS) ~ "Aardappelen",
      grepl("Tarwe", GWS_GEWAS) | grepl("Gerst", GWS_GEWAS)| grepl("Haver", GWS_GEWAS) | grepl("haver", GWS_GEWAS) | grepl("Rogge", GWS_GEWAS) | grepl("Spelt", GWS_GEWAS) | grepl("Overige granen", GWS_GEWAS) | grepl("Quinoa", GWS_GEWAS) | grepl("boekweit", GWS_GEWAS)~ "Granen",
      grepl("Bieten", GWS_GEWAS) ~ "Suikerbieten",
      grepl("Maïs", GWS_GEWAS) | grepl("Mais", GWS_GEWAS)~ "Maïs",
      grepl("Groenten", GWS_GEWAS) ~ "Groenten",
      grepl("Fruit", GWS_GEWAS) ~ "Fruit",
      grepl("Uien", GWS_GEWAS) ~ "Uien",
      grepl("Luzerne", GWS_GEWAS) ~ "Luzerne",
      grepl("Boomkwekerij", GWS_GEWAS) ~ "Boomkwekerij",
      grepl("Agrarisch natuurmengsel", GWS_GEWAS) ~ "Agrarisch natuurmengsel",
      grepl("Overige akkerbouwgewassen", GWS_GEWAS) ~ "Overige akkerbouwgewassen",
      grepl("Erwten", GWS_GEWAS) | grepl("Kapucijners-", GWS_GEWAS)~ "Erwten",
      grepl("Appelen", GWS_GEWAS)| grepl("Kersen", GWS_GEWAS) | grepl("Peren", GWS_GEWAS) | grepl("Pruimen", GWS_GEWAS) | grepl("Vruchtbomen", GWS_GEWAS)~ "Fruitbomen",
      grepl("Sla", GWS_GEWAS) ~ "Sla",
      grepl("Walnoten", GWS_GEWAS) | grepl("Notenbomen", GWS_GEWAS)~ "Notenbomen",
      grepl("Aardbeien", GWS_GEWAS) ~ "Aardbeien",
      grepl("Asperges", GWS_GEWAS) ~ "Asperges",
      grepl("Bloemkool", GWS_GEWAS) ~ "Bloemkool",
      grepl("Bessen", GWS_GEWAS) ~ "Bessen",
      grepl("Bonen", GWS_GEWAS) | grepl("bonen", GWS_GEWAS) | grepl("Peulen", GWS_GEWAS) | grepl("Sojabonen", GWS_GEWAS)~ "Bonen",
      grepl("Bos,", GWS_GEWAS) | grepl("Bos-", GWS_GEWAS) ~ "Bos",
      grepl("Grasland, blijvend", GWS_GEWAS) | grepl("Grasland, natuurlijk", GWS_GEWAS) ~ "Grasland, blijvend",
      grepl("Prei", GWS_GEWAS) ~ "Prei",
      grepl("Pastinaak", GWS_GEWAS) ~ "Pastinaak",
      grepl("Pompoen", GWS_GEWAS) ~ "Pompoen",
      grepl("Bloembollen", GWS_GEWAS) | grepl("bloembollen", GWS_GEWAS)~ "Bloembollen",
      grepl("Waspeen", GWS_GEWAS)| grepl("Wortelen", GWS_GEWAS) | grepl("Bospeen", GWS_GEWAS) | grepl("Winterpeen", GWS_GEWAS) ~ "Wortelen",
      grepl("Broccoli", GWS_GEWAS) ~ "Broccoli",
      grepl("kool", GWS_GEWAS) ~ "Kool",
      grepl("Spinazie", GWS_GEWAS) ~ "Spinazie",
      grepl("Andijvie", GWS_GEWAS) ~ "Andijvie",
      TRUE ~ monocrops$GWS_GEWAS
    )
  )


temporal_diversity <- monocrops
temporal_diversity$frequency <- 1/14

temporal_distribution <- temporal_diversity %>%
  group_by(V3, generic_crop) %>%
  summarize(freq = sum(frequency))

path <- "final_files/temporal_distribution.shp"
st_write(temporal_distribution, path)
  
# Create a table of the column to count occurrences
gewas_counts <- table(monocrops$generic_crop)

# Convert the table to a data frame
gewas_counts_df <- as.data.frame(gewas_counts)

### aardappel = 1, suikerbiet = 77, uien = 83,  wortelen  = 90, kool = 44, prei = 64, bonen = 12, bloemkool = 10, broccoli = 16,
begin <- c(1,77,83,90,44,37,12,21,10)

subset_important <- gewas_counts_df[begin,]
subset_unimportant <- gewas_counts_df[-begin,]
# Order the data frame by the frequency column in descending order
gewas_counts_df_1 <- subset_unimportant[order(-subset_unimportant$Freq), ]
gewas_counts_df_1 <- gewas_counts_df_1[-2,]

gewas_counts_final <- rbind(subset_important, gewas_counts_df_1)

staple_crops <- as.vector(gewas_counts_final$Var1)

## next we need an output file. 
crop_rotations <- data.frame()

# Iterate through each field code

for (i in fieldcodes) {
  
  # Create a subset for the current field code
  subset <- monocrops[monocrops$V3 == i,]
  
  # Flag to check if any option is found
  option_found <- FALSE
  
  # Iterate through each crop option
  for (crop_option in staple_crops) {
    
    # Check if the current crop option exists in the subset
    if (crop_option %in% subset$generic_crop) {
      # Perform operations when the crop option is found
      
      # Find the first occurrence of the crop within the subset
      first_occurrence_index <- which(subset$generic_crop == crop_option)[1]
      
      # Check if a first occurrence is found
      if (!is.na(first_occurrence_index)) {
        # Find the second occurrence of the crop within the subset
        second_occurrence_index <- which(subset$generic_crop == crop_option)[2]
        
        ### if you want a more accurate process, add this in: 
        #crop2_1 <- first_occurrence_index+1
        #crop2_2 <- second_occurrence_index+1
        
        # Check if a second occurrence is found
        if (!is.na(second_occurrence_index )  && second_occurrence_index-first_occurrence_index > 1) { #&& !is.na(subset$generic_crop[crop2_2])
          
          #if(subset$generic_crop[crop2_1] == subset$generic_crop[crop2_2]){
          
          # Print all rows from the first occurrence to the second occurrence
          selected_rows <- subset[(first_occurrence_index:(second_occurrence_index-1)), ]
          
          selected_rows <- unique(selected_rows)
          
          # For every field, determine the row length
          row_length <- nrow(selected_rows)
          
          # Create a new frequency column where the value is 1/over the length
          selected_rows$frequency <- 1 / row_length
          
          # Add the results to the main dataframe
          crop_rotations <- rbind(crop_rotations, selected_rows)
          
          # Set the flag to indicate that an option is found
          option_found <- TRUE
          
          # Exit the loop since we found an option
          break
          #}
          } 
          else {
            third_occurrence_index <- which(subset$generic_crop == crop_option)[3]
            
            if (!is.na(third_occurrence_index)) {
              selected_rows <- subset[(first_occurrence_index:(third_occurrence_index-1)), ]
              
              if(selected_rows$generic_crop[2] != selected_rows$generic_crop[1]){
              
              selected_rows <- unique(selected_rows)
              
              # For every field, determine the row length
              row_length <- nrow(selected_rows)
              
              # Create a new frequency column where the value is 1/over the length
              selected_rows$frequency <- 1 / row_length
              
              # Add the results to the main dataframe
              crop_rotations <- rbind(crop_rotations, selected_rows)
              
              # Set the flag to indicate that an option is found
              option_found <- TRUE
              
              # Exit the loop since we found an option
              break
             
              } 
          }
      }
    }
    }
  }
  
  # Handle the case when none of the options are found
  if (!option_found) {
    # Perform operations when none of the options are present
    next
    # ... (Adjust as needed)
  }
}

length(unique(crop_rotations$V3))

temporal_diversity <- temporal_diversity %>%
  group_by(V3, generic_crop) %>%
  summarise(total_frequency = sum(frequency))

length(unique(temporal_diversity$V3)) 
length(unique(crop_rotations$V3))


path <- "important_files/crop_rotations.shp"
st_write(crop_rotations, path, append = FALSE)

path <- "important_files/temporal_diversity.shp"
st_write(temporal_diversity, path, append = FALSE)
