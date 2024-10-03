library(sp)
library(arcgisbinding)
library(raster)
library(sf)
library(dplyr)
library(spatialEco)
library(polylabelr)

# next lets set our working directory
setwd('D:/BRP')


provincies <- st_read("grenzen/Provinciegrenzen.gml")


provincies <- as.vector(provincies$Provincienaam)
b <- provincies[2]

parcels_final <- st_sfc()
st_crs(parcels_final) <- st_crs(percelen_clean)
fields_final <- data.frame()

for (b in provincies[1:12]) {
  
  ## loading the reference fields
  path <- paste("brpgewaspercelen2023/referentie_percelen_", b , ".shp" , sep = "")
  referentie_percelen <- st_read(path)
  
  ### loading the borders of gemeentes
  gemeentes <- st_read("grenzen/bestuurlijkegrenzen.gpkg")
  
  ### loading the actual parcels
  #path <- paste("brpgewaspercelen2022/percelen_provincies/percelen_", b , ".shp" , sep  = "")
  path <- paste("brpgewaspercelen2023/percelen_", b , ".shp" , sep  = "")
  percelen <- st_read(path)
  
  
  
  path <- paste("subsets_crops/", b , "_bb.shp", sep = "")
  bbox <- st_read(path)
  
  
  percelen$width <- bbox$MBG_Width
  percelen$length <- bbox$MBG_Length
  percelen$orient <- bbox$MBG_Orient
  
  rm(bbox)
  ### next lets make some adjustments to those files. First we add the bounding box information to the parcels. Then we add fieldcodes to the referencefields, so that we can give every parcel a field they belong to. 
  #
  referentie_percelen$index <- seq_len(nrow(referentie_percelen))
  referentie_percelen$fieldcode <- paste(referentie_percelen$Prvncnm, referentie_percelen$index, sep = "_")
  
  #~~~#######~~~#
  ### Counter ###
  #~~~#######~~~#
  step_0 <- c("step_0", nrow(percelen), length(unique(referentie_percelen$fieldcode)))
  
  
  ### Now we do some cleaning up. We take out crop classifications that in our eyes cannot contribute to the strip cropping set. 
  percelen_clean <- percelen %>%
    filter(!(percelen$gewas %in% c('Sloot', 'Grasland, blijvend', 'Bos, blijvend', 'Grasland, natuurlijk. Hoofdfunctie natuur.', 'Natuurvriendelijke oever', 'Struweelhaag', 'Landschapselement, overig', 'Grasland, natuurlijk. Met landbouwactiviteiten.', 'Groene braak, spontane opkomst', '')))
  percelen_clean <- percelen_clean[percelen_clean$category != "Overige", ]
  percelen_clean <- na.omit(percelen_clean)
  
  ## lets make some space in our computer by removing datasets we dont need anymore.
  rm(percelen)

fields_stripcropping <- data.frame()
parcels_stripcropping <- st_sfc()

st_crs(parcels_stripcropping) <- st_crs(percelen_clean)
gemeente_names <- unique(as.vector(referentie_percelen$gemntnm))
step_1 <- data.frame()
step_2 <- data.frame()
step_3 <- data.frame()
step_4 <- data.frame()
step_5 <- data.frame()
step_6 <- data.frame()
step_7 <- data.frame()



for (a in gemeente_names) {
  ### reference fields for this gemeente
  referentie_gemeente <- referentie_percelen[referentie_percelen$gemntnm == a,]
  
  ### parcels within the gemeente
  ### try-catch because usage of spatial.select function
  result <- NULL
  tryCatch({
    percelen_gemeente <- spatial.select(gemeentes[gemeentes$gemeentenaam == a,], percelen_clean, predicate = "intersect")
    percelen_gemeente <- na.omit(percelen_gemeente)
    result <- "Success"  # Assign result inside tryCatch block
  }, error = function(e) {
    #print(paste("Error occurred:", conditionMessage(e)))
    result <- "Error"  # Assign result inside error handler
  })
  if (is.null(result) == TRUE) {
    next
  }
  
  ### Join parcels to reference fields to apply fieldcodes to parcels
  percelen_gemeente_with_fieldcode <- st_join(percelen_gemeente, referentie_gemeente[,7], largest = TRUE)
  
  ### NA.omit becuase sometimes things seem to go wrong
  percelen_gemeente_with_fieldcode <- na.omit(percelen_gemeente_with_fieldcode)
  
  #~~~#######~~~#
  ### Counter ###
  #~~~#######~~~#
  step_1 <- rbind(step_1, percelen_gemeente_with_fieldcode)
  
  ### Remove fields with width smaller than 2.5, seeing as bbox can only overestimate width, we can use bbox only for the lower limit, not the higher limit. 
  percelen_gemeente_with_fieldcode_3m <- percelen_gemeente_with_fieldcode[percelen_gemeente_with_fieldcode$width > 2.5,]
  
  if(nrow(percelen_gemeente_with_fieldcode_3m)>0){
  #~~~#######~~~#
  ### Counter ###
  #~~~#######~~~#
  step_2 <- rbind(step_2, percelen_gemeente_with_fieldcode_3m)
  }
  
 
  
  percelen_gemeente_with_fieldcode_3m$ID <- seq_len(nrow(percelen_gemeente_with_fieldcode_3m))
  
  ### Now we start analysing the parcels per field. We need a vector all fieldcodes to use in the for-loop
  fieldcodes_gemeente <- unique(as.vector(percelen_gemeente_with_fieldcode_3m$fieldcode))
  
   ### And we need an output file
  strip_cropping_parcels_gemeente <- st_sfc()
  
  st_crs(strip_cropping_parcels_gemeente) <- st_crs(percelen_clean)
  
  ### Now we are ready to start the for-loop
  for (b in fieldcodes_gemeente) {
    
    ### First we need to subset the parcels for this field: 
    subset_parcels <- percelen_gemeente_with_fieldcode_3m[percelen_gemeente_with_fieldcode_3m$fieldcode == b,]
    
    ### Next we take the first, most simple step. The fields needs to have more than 5 parcels. Otherwise, move on to the next
    if(nrow(subset_parcels) <= 4){
      next
    }
    if(nrow(subset_parcels)>0){
    #~~~#######~~~#
    ### Counter ###
    #~~~#######~~~#
    step_3 <- rbind(step_3, subset_parcels)
    }
    
    ### step one: creating centroids of parcel subset 
    centroids_subset <- st_centroid(subset_parcels)
    
    ### For parcels that are not rectangular, centroids are placed outside the parcel. If this is the case we replace this centroid by a point_on_surface, which is the next best thing. 
    for (c in 1:nrow(subset_parcels)) {
      centroids_in_parcel <- st_contains(subset_parcels[c,], centroids_subset[c,], sparse = T)
      if (length(centroids_in_parcel[[1]]) == 0) {
        polygon <- subset_parcels[c, ]
        centroid <- st_point_on_surface(polygon)
        centroids_subset[c, ] <- centroid
      }
    }
    
    ### Then we create a buffer around the centroids with range equal to half the max width of a strip
    centroid_buffer <- st_buffer(centroids_subset, dist = 13.5)
    
    ### Using this buffer, we can determine whether/which parcels border this one with a maximum distance half of the total width allowed. This allows us to check the size of the strip ánd select its neighbors. So we start another for-loop to check every parcel within this field. 
    
    ### We also need an output file for the parcels that are within our criteria
    parcels_subset <- data.frame()
    subset_ID <- as.vector(subset_parcels$ID)
      
    
    ### Lets go
    for (d in subset_ID) {
      
      ### Create a subset of the parcels in this field WITHOUT the parcel that belongs to the centroid_buffer that we are using her
      subset <- subset_parcels[subset_parcels$ID != d,]
      ### Check which parcels overlap with the buffer. Because spatial.select, trycatch sequence. 
      result <- NULL
      
      # Use tryCatch to handle potential errors
      tryCatch({
        parcels_overlap <- spatial.select(centroid_buffer[centroid_buffer$ID == d,], subset, predicate = "intersect")
        result <- "Success"  # Assign result inside tryCatch block
      }, error = function(e) {
        #print(paste("Error occurred:", conditionMessage(e)))
        result <- "Error"  # Assign result inside error handler
      })
      if (is.null(result) == TRUE) {
        next
      }
      
      ### get ID of these parcels. 
      id <- as.vector(parcels_overlap$ID)
      
      ### Create new subset without these parcels. 
      subset2 <- subset[!(subset$ID %in% parcels_overlap$ID),]
      
      for (e in id) {
        parcel <- centroid_buffer[centroid_buffer$ID == e,]
        ### check if a centroid of either of these overlaps with any of the remaining parcels within this field. 
        centroids_intersect <- st_intersects(parcel, subset2, sparse = T)
        
        ### If there is overlap, st_contains will output at least 1 value. So if there is more than 0, we conclude the parcel to be a parcel that is bordered by at least one other parcel that also has width < 27m. So we add it to 
        if (length(centroids_intersect[[1]]) != 0) {
          parcels_subset <- rbind(parcels_subset, subset_parcels[subset_parcels$ID == d,])
        }
      }
    }
    parcels_subset <- unique(parcels_subset)
    
    if(nrow(parcels_subset)>0){
    #~~~#######~~~#
    ### Counter ###
    #~~~#######~~~#
    step_4 <- rbind(step_4, parcels_subset)
    }
    
    ### Now we have a subset of parcels that are between 3 and 27m wide and are bordered by at least one parcel that also has the same. 
    
    ### Now again we do a quick operation to take out all fields that have less than 5 individual crops
    if (length(unique(parcels_subset$gewas)) <= 4) {
      next
    }
    
    if(nrow(parcels_subset)>0){
    #~~~#######~~~#
    ### Counter ###
    #~~~#######~~~#
    step_5 <- rbind(step_5, parcels_subset)
    } 
    
    ### Next we need to check if these parcels are directed in the same orientation. Within one field, we need at least 5 parcels that have approximately the same orientation. This way we know that we are not selecting 5 random small parcels that happen to be connected someway or another. 
    ### For every parcel we check the orientation and create a subset of the parcels that have an orientation that is within a certain range of that. Next we verify whether that subset has more than 5 parcels within it. If so, thats cool. 
    ### So again a for-loop and an output file needed. 
    parcels_parralel <- data.frame()
    
    for (e in 1:nrow(parcels_subset)) {
      orient <- parcels_subset[e,]$orient
      range <- 10
      parralel <- parcels_subset[abs(parcels_subset$orient-orient) <= range,]
      if (length(parralel) >= 5){
        parcels_parralel <- rbind(parcels_parralel, parralel)
        parcels_parralel <- unique(parcels_parralel)
      }
    }
    
    if(nrow(parcels_parralel)>0){
    #~~~#######~~~#
    ### Counter ###
    #~~~#######~~~#
    step_6 <- rbind(step_6, parcels_parralel)
    } 
    
    parcels_subset_final <- data.frame()
    subset_ID <- as.vector(parcels_parralel$ID)
    
    
    ### Lets go
    for (d in subset_ID) {
      
      ### Create a subset of the parcels in this field WITHOUT the parcel that belongs to the centroid_buffer that we are using here. 
      subset <- parcels_parralel[parcels_parralel$ID != d,]
      ### Check which parcels overlap with the buffer. Because spatial.select, trycatch sequence. 
      result <- NULL
      
      # Use tryCatch to handle potential errors
      tryCatch({
        parcels_overlap <- spatial.select(centroid_buffer[centroid_buffer$ID == d,], subset, predicate = "intersect")
        result <- "Success"  # Assign result inside tryCatch block
      }, error = function(e) {
        #print(paste("Error occurred:", conditionMessage(e)))
        result <- "Error"  # Assign result inside error handler
      })
      if (is.null(result) == TRUE) {
        next
      }
      
      ### get ID of these parcels. 
      id <- as.vector(parcels_overlap$ID)
      
      ### Create new subset without these parcels. 
      subset2 <- subset[!(subset$ID %in% parcels_overlap$ID),]
      
      
      result <- NULL
      
      # Use tryCatch to handle potential errors
      tryCatch({
        parcels_overlap2 <- spatial.select(centroid_buffer[centroid_buffer$ID %in% id,], subset2, predicate = "intersect")
        result <- "Success"  # Assign result inside tryCatch block
      }, error = function(e) {
        print(paste("Error occurred:", conditionMessage(e)))
        result <- "Error"  # Assign result inside error handler
      })
      if (is.null(result) == TRUE) {
        next
      }
      
      id2 <- as.vector(parcels_overlap2$ID)
      
      ### Create new subset without these parcels. 
      subset3 <- subset2[!(subset2$ID %in% parcels_overlap2$ID),]
      
      for (e in id2) {
        parcel <- centroid_buffer[centroid_buffer$ID == e,]
        ### check if a centroid of either of these overlaps with any of the remaining parcels within this field. 
        centroids_intersect <- st_intersects(parcel, subset3, sparse = T)
        
        ### If there is overlap, st_contains will output at least 1 value. So if there is more than 0, we conclude the parcel to be a parcel that is bordered by at least one other parcel that also has width < 27m. So we add it to 
        if (length(centroids_intersect[[1]]) != 0) {
          parcels_subset_final <- rbind(parcels_subset_final, parcels_parralel[parcels_parralel$ID == d,])
        }
      }
    }
    
    parcels_subset_final <- unique(parcels_subset_final)
    
    if(length(unique(parcels_subset_final$gewas)) <= 4){
      next
    }
    
    if(nrow(parcels_subset_final)>0){
    #~~~#######~~~#
    ### Counter ###
    #~~~#######~~~#
    step_7 <- rbind(step_7, parcels_subset_final)
    } 
    
    strip_cropping_parcels_gemeente <- rbind(strip_cropping_parcels_gemeente, parcels_subset_final)
   
  }
  
  ### Adding this subset to the output file of the main for-loop
  print("parcels_stripcropping3")
  print(parcels_stripcropping)
  
  if (is.null(nrow(strip_cropping_parcels_gemeente)) == TRUE) {
    next
  }
  
  parcels_stripcropping <- rbind(strip_cropping_parcels_gemeente, parcels_stripcropping)
  
  list_fields <- unique(strip_cropping_parcels_gemeente$fieldcode)
  subset_fields <- referentie_gemeente[referentie_gemeente$fieldcode %in% list_fields,]
  fields_stripcropping <- rbind(fields_stripcropping, subset_fields)
}
parcels_final <- rbind(parcels_final, parcels_stripcropping)
fields_final <- rbind(fields_final, fields_stripcropping)
}

a <- c("Step 1", nrow(step_1), length(unique(step_1$fieldcode)))
b <- c("Step 2", nrow(step_2), length(unique(step_2$fieldcode)))
c <- c("Step 3", nrow(step_3), length(unique(step_3$fieldcode)))
d <- c("Step 4", nrow(step_4), length(unique(step_4$fieldcode)))
e <- c("Step 5", nrow(step_5), length(unique(step_5$fieldcode)))
f <- c("Step 6", nrow(step_6), length(unique(step_6$fieldcode)))
g <- c("Step 7", nrow(step_7), length(unique(step_7$fieldcode)))


counter2 <- data.frame()
counter2 <- rbind(counter2, step_0,a,b,c,d,e,f,g)

for (a in unique(parcels_stripcropping$fieldcode)) {
  plot(parcels_stripcropping[parcels_stripcropping$fieldcode == a,]["orient"], main = paste(a, " orient", sep  = ""))
}

for (a in unique(parcels_stripcropping$fieldcode)) {
  plot(parcels_stripcropping[parcels_stripcropping$fieldcode == a,]["gewas"], main = paste(a, " gewas", sep  = "") )
}




for (a in gemeente_names) {
  #subsetting reference fields
  referentie_gemeente <- referentie_percelen[referentie_percelen$gemntnm == a,]
  # subsetting parcels
  result <- tryCatch({
    
    percelen_gemeente <- spatial.select(gemeentes[gemeentes$gemeentenaam == a,], percelen_clean, predicate = "intersect")
    percelen_gemeente <- na.omit(percelen_gemeente)
    
    return("Success")
  }, error = function(e) {
    return("Error")
  })
  
  if (inherits(result, "error")) {
    next
  }
    
    ### THis join assigns the fieldcode of the most overlapping field to the parcels. 
    percelen_gemeente_with_fieldcode <- st_join(percelen_gemeente, referentie_gemeente[,7], largest = TRUE)
  percelen_gemeente_with_fieldcode <- na.omit(percelen_gemeente_with_fieldcode)
    
    
    # filtering our all the fields that are smaller than 3m wide (here I use 2.5 to make up for small errors in drawin and the bbox inaccuracy)
    percelen_gemeente_with_fieldcode_3m <- percelen_gemeente_with_fieldcode[percelen_gemeente_with_fieldcode$width > 2.5,]
    
    ## next we prepare for the next for-loop. lets create a vector of the unique fieldcodes in the subset and lets create an output file. 
    fieldcodes_gemeente <- unique(as.vector(percelen_gemeente_with_fieldcode_3m$fieldcode))
    strip_cropping_gemeente <- data.frame()
    
    
    ## this for-loop inspects every field in the gemeente
    for (i in fieldcodes_gemeente) {
      
      ### becuase this forloop uses the spatial.select function, which produces and error when 0 values are selected, the forloop has a built-in error avoiding system. You will see this repeated in the multiple nested for-loops. it is signified by the trycatch at the start and a return "succes' return "error" etc. bit. 
        # creating the field subset
        subset_parcels <- percelen_gemeente_with_fieldcode_3m[percelen_gemeente_with_fieldcode_3m$fieldcode == i,]
        # selecting only fields with more than 5 parcels within. otherwise moving on to the next. 
        if(nrow(subset_parcels) <= 5){
          next
        }
        ### What we see is that some centroids actually do not fall inside the polygon shape. In such cases, the code is not suited, since to return the correct parcel, a spatial select is performed on the parcel and its centroid. What we need here is a bit of code that replaces the centroid with a st_point_on_surface, seeing as this is the next best thing. The st_point_on_surface function returns a point that is placed quite centrally in the shape, so I assume it to be a good substitute for the st_centroid function. 
        # step one: creating centroids. 
        centroids_subset <- st_centroid(subset_parcels)
        # step 2: checking whether the centroids actually are inside their respective parcels. 
        
        # step 3: replacing centroids that fall outside tjeir polygon.
        for (l in 1:nrow(subset_parcels)) {
          centroids_in_parcel <- st_contains(subset_parcels[l,], centroids_subset[l,], sparse = T)
          if (length(centroids_in_parcel[[1]]) == 0) {
            polygon <- subset_parcels[l, ]
            centroid <- st_point_on_surface(polygon)
            centroids_subset[l, ] <- centroid
          }
        }
        ## creating a buffer around the centroids with range equal to half the max width of a strip
        centroid_buffer <- st_buffer(centroids_subset, dist = 13.5)
        
        ### starting a new for-loop
        intersecting_centroids <- data.frame()
        
        ### this for-loop finds all of these centroids with their buffer areas that intersect with any fields that are NOT their own respective fields.
        for (j in 1:nrow(subset_parcels)) {
         
            subsetje <- subset_parcels[-j, ]
            
             result2 <- tryCatch({
            intersect <- spatial.select(subsetje, centroid_buffer[j, ], predicate = "intersect")
            return("Success")
          }, error = function(e) {
            return("Error")
          })
          
          if (inherits(result2, "error")) {
            next
          }
            
            intersecting_centroids <- rbind(intersecting_centroids, intersect)
            
        }
        
        centroids1 <- st_centroid(intersecting_centroids)
        result4 <- tryCatch({
        first_round <- spatial.select(centroids1, subset_parcels, predicate = "intersect")
        return("Success")
        }, error = function(e) {
          return("Error")
        })
        
        if (inherits(result4, "error")) {
          next
        }
        # Now that we performed this procedure a first time, we need to repeat it again to exclude all strips that are not bordered by other strips. What this means is that if a strip is selected in the previous round, to test wether it has strips next to it (which shows that there is a collection of strips in a field. It selects out the parcels that have the sizes of a strip, but are not part of a stripcropping field)
        
        centroids_first_round <- st_centroid(first_round)
        
        for (m in 1:nrow(first_round)) {
          # step 2: checking whether the centroids actually are inside their respective parcels. 
          centroids_in_parcel1 <- st_contains(first_round[m,], centroids_first_round[m,], sparse = T)
          if (length(centroids_in_parcel1[[1]]) == 0) {
            # step 3: replacing centroids that fall outside their polygon.
            polygon <- first_round[m, ]
            centroid <- st_point_on_surface(polygon)
            centroids_first_round[m, ] <- centroid
          }
        }
        first_round_buffer <- st_buffer(centroids_first_round, dist = 13.5)
        
        intersecting_centroids2 <- data.frame()
        
        for (k in 1:nrow(first_round)) {
          
          subsetje <- first_round[-k, ]
          
          result3 <- tryCatch({
            intersect <- spatial.select(subsetje, first_round_buffer[k, ], predicate = "intersect")
            return("Success")
          }, error = function(e) {
            return("Error")
          })
          
          if (inherits(result3, "error")) {
            next
          }
          intersecting_centroids2 <- rbind(intersecting_centroids2, intersect)
        }
        
        centroids2 <- st_centroid(intersecting_centroids2)
        
        result5 <- tryCatch({
        second_round <- spatial.select(centroids2, subset_parcels, predicate = "intersect")
        return("Success")
        }, error = function(e) {
          return("Error")
        })
        
        if (inherits(result5, "error")) {
          next
        }
        ### Now there is a selection mostly based on parcels neighbouring one another. It seems that another measure is needed to ensure a proper selection of strip cropping field. We need to ensure that the subset that we have acquired now contains more than 5 unique crops. Otherwise we cannot categorise it as strip cropping. 
        
        
        if (length(unique(second_round$gewas)) <= 4) {
          next
        }
        
        strip_cropping_gemeente <- rbind(strip_cropping_gemeente, second_round)
    
    }
    list_fields <- unique(as.vector(strip_cropping_gemeente$fieldcode))
    subset_fields <- referentie_percelen[referentie_percelen$fieldcode == list_fields,]
    fields_stripcropping <- rbind(fields_stripcropping, subset_fields)
    parcels_stripcropping <- rbind(parcels_stripcropping, strip_cropping_gemeente)
  
}
