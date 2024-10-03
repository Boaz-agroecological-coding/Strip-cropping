### This script will perform the final data analysis of the data that I gathered. Lets get it done!!! 

### loading packages and data: 

library(sp)
library(arcgisbinding)
library(raster)
library(sf)
library(tidyr)
library(dplyr)
library(spatialEco)
library(polylabelr)
library(openxlsx)
library(car)
library(ggplot2)
library(ggsignif)
library(forcats)
library(rlang)
library(ggpmisc)
library(mclust)
library(MASS)


setwd("D:/BRP")
provincies <- st_read("important_files/bestuurlijkegrenzen_gml_2021/Provinciegrenzen.gml")

# lets set some working DIR
setwd("C:/Users/boazg/OneDrive - Wageningen University & Research/2023-2024/THESIS")


path <- "important_files/parcels_final_strip_cropping_2023.shp"
stripcropping_parcels <- st_read(path)


####~~~~~~~~~~ ALLEEN CROP ROTATIONS
path <- "final_files/both_methods.xlsx"
both_methods <- read.xlsx(path)

path <- "final_files/temp_ROTA.shp"
monocropping <- st_read(path)

path <- "final_files/stripcropping_adjusted.shp"
stripcropping <- st_read(path)

mode <- "ONLY_CROP_ROTATIONS"

####~~~~~~~~~~ ALLE TEMPORAL DATA 
path <- "final_files/both_dimensions.xlsx"
both_methods <- read.xlsx(path)

path <- "final_files/stripcropping.shp"
stripcropping <- st_read(path)

path <- "final_files/temp_ALL.shp"
monocropping <- st_read(path)

mode <- "ALL_TEMPORAL_DATA"


# Assuming stripcropping is your dataframe
average_width <- mean(stripcropping_parcels$width, na.rm = TRUE)
sd_width <- sd(stripcropping_parcels$width, na.rm = TRUE)
paste("mean: ", format(average_width, digits = 3), " +/-", format(sd_width, digits = 3), sep = "")

freq_diff <- stripcropping$freq - monocropping$freq

mean_freq_strip <- (mean(stripcropping$freq))
mean_freq_mono <- (mean(monocropping$freq))


sd_freq_strip <- sd(stripcropping$freq)
sd_freq_mono <- sd(monocropping$freq)

richness <-data.frame() 

  for (i in unique(both_methods$fieldcd)) {
  sub_spat <- stripcropping[stripcropping$fieldcd == i,]
  sub_temp <- monocropping[monocropping$fieldcd == i,]
  rich_spat <- length(unique(sub_spat$gewas))
  rich_temp <- length(unique(sub_temp$gewas))
  klokje <- c(i,rich_spat, rich_temp)
  richness <- rbind(richness, klokje)
  
}
colnames(richness) <- c("fieldcode", "richness_spatial", "richness_temporal")
richness[, c(2, 3)] <- lapply(richness[, c(2, 3)], as.numeric)

mean_rich_spat <- mean(richness$richness_spatial)
mean_rich_temp <- mean(richness$richness_temporal)
sd_rich_spat <- sd(richness$richness_spatial)
sd_rich_temp <- sd(richness$richness_temporal)

# Normality check
shapiro_test <- shapiro.test(richness$richness_spatial)
shapiro_test2 <- shapiro.test(richness$richness_temporal)


# Homogeneity of variances check using var.test
var_test <- leveneTest(richness_spatial + richness_temporal ~ fieldcode, data = richness)

if (shapiro_test$p.value > 0.05 & shapiro_test2$p.value > 0.05 &  var_test$"Pr(>F)"[1] > 0.05) {
  # Data is approximately normally distributed and variances are equal
  t_test <- t.test(stripcropping$freq, monocropping$freq, var.equal = TRUE)
} else {
  # Use Wilcoxon Rank Sum Test (Mann-Whitney U test)
  wilcox_test <- wilcox.test(richness$richness_spatial, richness$richness_temporal)
}



ggplot(data = stripcropping, aes(x = freq))+ 
  geom_histogram(binwidth = 0.05, fill = "grey40", colour  = "black")+ 
  geom_vline(aes(xintercept = mean_freq_strip), col  ="red")+ 
  theme_minimal()

ggplot(data = monocropping, aes(x = freq))+ 
  geom_histogram(binwidth = 0.05, fill = "grey40", colour  = "black")+ 
  geom_vline(aes(xintercept = mean_freq_mono), col  ="red")+ 
  theme_minimal()


# Normality check
shapiro_test <- shapiro.test(freq_diff)

# Homogeneity of variances check using var.test
var_test <- leveneTest(freq ~ method, data = both_methods)

if (shapiro_test$p.value > 0.05 & var_test$"Pr(>F)"[1] > 0.05) {
  # Data is approximately normally distributed and variances are equal
  t_test <- t.test(stripcropping$freq, monocropping$freq, var.equal = TRUE)
} else {
  # Use Wilcoxon Rank Sum Test (Mann-Whitney U test)
  wilcox_test <- wilcox.test(stripcropping$freq, monocropping$freq)
}

# Print the results
print(shapiro_test)
print(var_test)
if (exists("t_test")) print(t_test) else print(wilcox_test)



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Doing Shannon-Index
for_shannon <- both_methods %>%
  group_by(method, fieldcd) %>%
  summarise(shannon = -sum(freq * log(freq)))
          
# difference shannon index
shannon_dif <- for_shannon[for_shannon$method == "spatial",]$shannon-for_shannon[for_shannon$method == "temporal",]$shannon

           
# Normality check
shapiro_test_shannon <- shapiro.test(shannon_dif)


# Homogeneity of variances check using var.test
var_test_shannon <- leveneTest(shannon ~ method, data = for_shannon)


if (shapiro_test_shannon$p.value > 0.05 && var_test_shannon$"Pr(>F)"[1] > 0.05) {
  # Data is approximately normally distributed and variances are equal
  t__test <- t.test(for_shannon[for_shannon$method == "spatial",]$shannon, for_shannon[for_shannon$method == "temporal",]$shannon, var.equal = TRUE)
  p_value <- t__test$p.value
  print(paste("t-test: ", paste0(format(p_value, digits = 3)), sep = ""))
} else {
  # Use Wilcoxon Rank Sum Test (Mann-Whitney U test)
  wilcox__test <- wilcox.test(for_shannon[for_shannon$method == "spatial",]$shannon, for_shannon[for_shannon$method == "temporal",]$shannon)
  p_value <- wilcox__test$p.value
  print( paste("Mann-Whitney U test: ", paste0(format(p_value, digits = 3)), sep = ""))} 



mean_shannon_spatial <- mean(for_shannon[for_shannon$method == "spatial",]$shannon)
sd_shannon_spatial <- sd(for_shannon[for_shannon$method == "spatial",]$shannon)
mean_shannon_temp <- mean(for_shannon[for_shannon$method == "temporal",]$shannon)
sd_shannon_temp <- sd(for_shannon[for_shannon$method == "temporal",]$shannon)

paste(mode, " spatial diversity (shannon) : ", format(mean_shannon_spatial, digits = 2), " +/- ", format(sd_shannon_spatial, digits = 2), sep = "")
paste(mode, " temporal diversity (shannon) : ", format(mean_shannon_temp, digits = 2), " +/- ", format(sd_shannon_temp, digits = 2), sep = "")



##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  PER CROP DATA 


# Make sure 'method' is a factor variable
both_methods$method <- as.factor(both_methods$method)

# Aggregate by gewas and method, summing up the frequencies
crop_summary <- aggregate(freq ~ gewas + method, data = both_methods, mean)

# Display the summary
print(crop_summary)

#### And now could you run a statistical analysis to see which one of those are significantly different from one another (this means that the crop frequencies are significantly different between the two methods)

# Assuming crop_summary is your data frame from the previous step

# Split the data into separate data frames for each method
crop_summary_method1 <- subset(crop_summary, method == "temporal")
crop_summary_method2 <- subset(crop_summary, method == "spatial")



results_df <- data.frame(gewas = character(), p.value = numeric(), stringsAsFactors = FALSE)

# Iterate over unique crops
for (gewas in unique(both_methods$gewas)) {
  # Create a subset for the current crop
  data_crop <- both_methods[both_methods$gewas == gewas,]
  
  # Check if there are enough observations for both methods
  if (nrow(data_crop[data_crop$method == "temporal", ]) >= 3 && 
      nrow(data_crop[data_crop$method == "spatial", ]) >= 3) {
    
    freq_diff_mono <- data_crop$freq[data_crop$method == "temporal"]
    freq_diff_strip <- data_crop$freq[data_crop$method == "spatial"]
    # Perform t-test
    shapiro_test <- shapiro.test(data_crop$freq)
    
    var_test <- leveneTest(freq ~ method, data = data_crop)
    
    if (shapiro_test$p.value > 0.05 &&
        var_test$"Pr(>F)"[1] > 0.05) {
      # Data is approximately normally distributed and variances are equal
      t__test <- t.test(freq_diff_mono, freq_diff_strip, var.equal = TRUE)
      p_value <- t__test$p.value
    } else {
      # Use Wilcoxon Rank Sum Test (Mann-Whitney U test)
      wilcox__test <- wilcox.test(freq_diff_mono, freq_diff_strip)
      p_value <- wilcox__test$p.value
    }
    
    # Append results to the data frame
    results_df <- rbind(results_df, data.frame(gewas = gewas, p.value = p_value))
  }
}




###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~ For Jaccard similarity index
fieldcodes <- unique(both_methods$fieldcd)
spatial <- both_methods[both_methods$method == "spatial",]
temporal <- both_methods[both_methods$method == "temporal",]

output_Jacc <- data.frame()
for (i in fieldcodes) {
  spatial_sub <- spatial[spatial$fieldcd == i,]
  temporal_sub <- temporal[temporal$fieldcd == i,]
  merge <- merge(spatial_sub, temporal_sub, by = "gewas", all = TRUE, suffixes = c("_spatial", "_temporal"))
  merge[is.na(merge)] <- 0
  min_freq <- pmin(merge$freq_spatial, merge$freq_temporal)
  max_freq <- pmax(merge$freq_spatial, merge$freq_temporal)
  
  weigthed_Jacc <- sum(min_freq)/ sum(max_freq)
  data <- c(i, weigthed_Jacc)
  output_Jacc <- rbind(output_Jacc, data)
}
colnames(output_Jacc) <- c("fieldcode", "Jacc")


output_Jacc$Jacc <- as.numeric(output_Jacc$Jacc)
ggplot(data = output_Jacc, aes(x = Jacc))+ 
  geom_histogram(binwidth = 0.05, fill = "grey60", colour = "black")+ 
  theme_minimal()

paste(mode, " Jaccard mean: ", mean(output_Jacc$Jacc))
paste(mode, " Jaccard sd: ", sd(output_Jacc$Jacc))

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~











# Print the results data frame
print(results_df)
# Assuming you've already run the previous code to create results_df
significant_crops <- results_df[results_df$p.value < 0.05, ]

# Print the subset of significant crops
print(significant_crops)



##############################################################################################################


results_table <- data.frame(
  crop = character(),
  freq_spatial = numeric(),
  sd_spatial = numeric(),
  freq_temporal = numeric(),
  sd_temporal = numeric(),
  p_value = character(),
  stringsAsFactors = FALSE
)

# Loop through each unique crop
for (crop in unique(both_methods$gewas)) {
  # Subset the data for the specific crop
  crop_data <- both_methods[both_methods$gewas == crop, ]
  crop_test <- results_df[results_df$gewas == crop,]
  
  if (nrow(crop_data[crop_data$method == "temporal", ]) >= 1 && 
      nrow(crop_data[crop_data$method == "spatial", ]) >= 1) {
  
  # Extract frequencies for each method
  freq_spatial <- crop_data$freq[crop_data$method == "spatial"]
  freq_temporal <- crop_data$freq[crop_data$method == "temporal"]
  
  # Calculate mean and standard deviation for each method
  mean_spatial <- mean(freq_spatial)
  sd_spatial <- sd(freq_spatial)
  mean_temporal <- mean(freq_temporal)
  sd_temporal <- sd(freq_temporal)
  p_value <- crop_test$p.value
  p_value <- ifelse(is.numeric(p_value) && length(p_value) == 0, NA, p_value)
  
  # Store results in the data frame
  results_table <- rbind(results_table, data.frame(
    crop = crop,
    freq_spatial = ifelse(is.na(mean_spatial), NA, mean_spatial), 
    sd_spatial = ifelse(is.na(sd_spatial), NA, sd_spatial),
    freq_temporal = ifelse(is.na(mean_temporal), NA, mean_temporal), 
    sd_temporal = ifelse(is.na(sd_temporal), NA, sd_temporal),
    p_value = ifelse(is.na(p_value), NA, ifelse(p_value < 0.05, paste0(format(p_value, digits = 2), "*"), format(p_value, digits = 2))),
    stringsAsFactors = FALSE
  ))
  }
}


path <- paste("final_files/results_crops", mode, ".xlsx", sep = "")
write.xlsx(results_table, path)



codes_strip <- unique(as.vector(stripcropping$fieldcd))

lengths_strip <- data.frame()

for (i in codes_strip) {
  subset <- both_methods[both_methods$method == "spatial",]
  subsett <- subset[subset$fieldcd == i,]
  length <- nrow(subsett)
  lengths_strip <- rbind(lengths_strip, length)
}

av_length_strip <- mean(lengths_strip$X9L)
sd_length_strip <- sd(lengths_strip$X9L)

codes_mono <- unique(as.vector(monocropping$fieldcd))

lengths_mono <- data.frame()

for (i in codes_mono) {
  subsett <- monocropping[monocropping$fieldcd == i,]
  length <- nrow(subsett)
  lengths_mono <- rbind(lengths_mono, length)
}

av_length_mono <- mean(lengths_mono[,1])
sd_length_mono <- sd(lengths_mono[,1])


############################################################################################

library(ggplot2)

summary_data <- both_methods %>%
  group_by(method) %>%
  summarize(mean_freq = mean(freq), sd_freq = sd(freq))

# Plot the bar graph with error bars
ggplot(both_methods, aes(x = method, y = freq, fill = method)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(data = summary_data, aes(x = method, y = mean_freq, ymin = mean_freq - sd_freq, ymax = mean_freq + sd_freq),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Mean crop relative frequencies cropping methods",
       x = "",
       y = "Mean Frequency",
       xlabs = c("Temporal", "Spatial")) +
  geom_text(aes(y = 0.37, x = 1.5, label = "***"), size = 4)+
  geom_segment(aes(x = 1.3, y = 0.35, xend = 1.7, yend = 0.35), color = "gray4", size = 1) +
  scale_fill_manual(values = c("temporal" = "gray19", "spatial" = "gray83")) +
  theme_minimal()




######################################################################################
# Select crops of interest

list_all_crops <- unique(both_methods$gewas)

crop_area <- stripcropping_parcels %>%
  group_by(gnrc_cr) %>%
  summarise(total_area = sum(area))

crop_count <- both_methods[both_methods$method == "temporal",] %>%
  count(gewas)
crop_area$gewas <- crop_area$gnrc_cr
crops <- unique(crop_count$gewas)


crop_area <- inner_join(crop_area, crop_count, by = "gewas")
crop_area <- crop_area[order(-crop_area$total_area),]

print(crop_area[1:8,]$gnrc_cr)


selected_crops <- crop_area[1:8,]$gnrc_cr

# Subset the data for selected crops

selected_data <- results_table[results_table$crop %in% selected_crops, ]

head(selected_data)

selected_data <- selected_data %>%
  mutate(crop = recode_factor(crop,
                              "Aardappelen" = "Potatoes",
                              "Granen" = "Grains",
                              "Uien" = "Onions",
                              "Agrarisch natuurmengsel" = "Nature mix",
                              "Maïs" = "Maize",
                              "Bonen" = "Beans",
                              "Grasland, tijdelijk" = "Grassland, temporary", 
                              "Kool" = "Cabbage"))

reshaped_data <- selected_data %>%
  pivot_longer(cols = starts_with("freq") | starts_with("sd"), 
               names_to = c(".value", "type"),
               names_pattern = "(.*)_(.*)") %>%
  pivot_longer(cols = freq:sd, 
               names_to = "variable", 
               values_to = "value") %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(p_value = ifelse(type == "stripcropping", p_value, NA))

# Rearrange the dataframe
reshaped_data <- reshaped_data %>%
  arrange(crop, type) %>%
  select(crop, type, freq, sd, p_value)

reshaped_data$crop <- factor(reshaped_data$crop, levels = c("Cabbage", "Onions", "Maize", "Nature mix","Beans", "Grassland, temporary", "Potatoes", "Grains"))



ggplot(reshaped_data, aes(x = crop, fill = type)) +
  geom_bar(aes(y = freq), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Crop recurrence frequencies in temporal and spatial rotations",
       x = "",
       y = "crop frequency") +
  geom_errorbar(aes(ymin = freq - sd, ymax = freq + sd),
                position = position_dodge(width = 0.8), width = 0.4) +
  scale_fill_manual(values = rep(c("gray19", "gray83"), length(selected_crops)),
                    name = "Rotation scale") +
  coord_flip()+
  theme_light()



###############################################################################

rotation_info <- data.frame(
  general_stat = character(),
  statistic = numeric()
)
rotation_info <- rbind(rotation_info, data.frame(
  general_stat = "rotation length monocropping",
  statistic = format(av_length_mono, digits = 3)
))
rotation_info <- rbind(rotation_info, data.frame(
  general_stat = "rotation length strip cropping",
  statistic = format(av_length_strip, digits = 3)
))

#### in case of different modes testing: 
rotation_info_all <- rotation_info

#######################################################################################
crop_frequency_info <- data.frame(
  title = character(),
  statistic = character()
)
crop_frequency_info <- rbind(crop_frequency_info, data.frame(
  title = "Mean crop frequency monocropping",
  statistic = paste(format(mean_freq_mono, digits = 2), "+/-", format(sd_freq_mono, digits = 2), sep = "")
))
crop_frequency_info <- rbind(crop_frequency_info, data.frame(
  title = "Mean crop frequency stripcropping",
  statistic = paste(format(mean_freq_strip, digits = 2), "+/-", format(sd_freq_strip, digits = 2), sep = "")))
crop_frequency_info <- rbind(crop_frequency_info, data.frame(
    title = "wilcox-test outcome",
    statistic = format(wilcox_test$p.value, digits = 2)
  ))
  
### in case of diffrent mode testing
crop_frequency_info_all <- crop_frequency_info

############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##############
  
  #### Data about fields
  
stripcropping_with_province <- st_join(stripcropping_parcels, provincies[,2], predicate = "intersect") 
shannon_spatial <- for_shannon[for_shannon$method == "spatial",]
  
stripcropping_fields <- data.frame()

for (i in unique(stripcropping_with_province$fieldcd)){
  subset <- stripcropping_with_province[stripcropping_with_province$fieldcd == i,]
  field <- st_union(subset)
  area <- st_area(field)
  strip_area <- mean(subset$area)
  width <- mean(subset$width)
  width_var <- var(subset$width)
  rows <- nrow(subset)
  nr_crops <- length(unique(subset$gewas))
  shannon <- shannon_spatial[shannon_spatial$fieldcd == i,]$shannon
  for (a in subset$ID) {
    if (subset[subset$ID == a,]$orient > 90) {
      subset[subset$ID == a,]$orient <- subset[subset$ID == a,]$orient-180
    }
  }
  
  var_orient <- var(subset$orient)
  province <- unique(subset$Provincienaam)
  thing <- c(i, province, area, strip_area, width, width_var, rows, nr_crops, shannon, var_orient)
  stripcropping_fields <- rbind(stripcropping_fields, thing)
}
  names <- c("fieldcode","province", "area_field","mean_area_strip", "mean_width", "var_width", "num_rows", "nr_crops","shannon", "var_orientation")
  colnames(stripcropping_fields) <- names
stripcropping_fields$area_field <- as.numeric(stripcropping_fields$area_field)
stripcropping_fields$mean_width <- as.numeric(stripcropping_fields$mean_width)
stripcropping_fields$num_rows <- as.numeric(stripcropping_fields$num_rows)
stripcropping_fields$mean_area_strip <- as.numeric(stripcropping_fields$mean_area_strip)
stripcropping_fields$var_width <- as.numeric(stripcropping_fields$var_width)
stripcropping_fields$var_orientation <- as.numeric(stripcropping_fields$var_orientation)
stripcropping_fields$nr_crops <- as.numeric(stripcropping_fields$nr_crops)
stripcropping_fields$shannon <- as.numeric(stripcropping_fields$shannon)
stripcropping_fields$weight <- stripcropping_fields$area_field/(mean(stripcropping_fields$area_field))

mean(stripcropping_fields$num_rows)
sd(stripcropping_fields$num_rows)

plot(stripcropping_parcels[stripcropping_parcels$fieldcd == "Noord-Brabant_484481",]["gewas"])

ggplot(data = stripcropping_fields, aes(x = area_field, y = shannon))+
  geom_point() +
  theme_minimal()

ggplot(data = stripcropping_parcels[stripcropping_parcels$width <10,], aes(x = width)) +
  geom_histogram(binwidth = 0.75, fill = "grey39", color = "black") +
  labs(title = "Distribution width of strips",
       x = "Strip width [m]",
       y = "Frequency") +
  theme_minimal()

ggplot(data = stripcropping_fields, aes(x = mean_width)) + 
  geom_histogram(breaks = seq(0.75, 30, by = 1.5), fill = "grey60", colour = "black") + 
  labs(title  = "Distribution of mean strip width per field", 
       x = "Mean strip width [m]", 
       y = "Amount of fields", 
       size = 5) +
  scale_x_continuous(breaks = seq(3, 30, by = 3)) +
  theme_minimal()



lm_area_width <- lm(area ~ mean_width, data = stripcropping_fields)
summary(lm_area_width)
rsq3 <- summary(lm_area_width)$r.squared
area_width <- ggplot(stripcropping_fields, aes(x = mean_width, y = area)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", x = 30, y = 100000, label = paste("R^2 = ", round(rsq3, 3)), size = 5, color = "red") +
  labs(title = "Mean Width vs Area",
       x = "Mean Width [m]",
       y = "Area [m2]") +
  theme_minimal()
print(area_width)


lm_area_rows <- lm(area ~ num_rows, data = stripcropping_fields)
summary(lm_area_rows)
rsq1 <- summary(lm_area_rows)$r.squared
area_rows <- ggplot(stripcropping_fields, aes(x = area, y = num_rows)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste("R^2 = ", round(rsq1, 3)), 
           hjust = 2, vjust = 19, size = 5, color = "blue") +
  labs(title = "Area vs Nr of Rows",
       x = "Area [m2]",
       y = "Number of rows") +
  theme_minimal()
print(area_rows)


kmeans_result <- kmeans(stripcropping_fields[, c("mean_width", "num_rows")], centers = 2)
lda_result <- lda( ~ mean_width + num_rows + area, data = stripcropping_fields)
lda <- predict(lda_result)
stripcropping_fields$lda <- as.vector(lda$class)
stripcropping_fields$cluster <- as.factor(kmeans_result$cluster)

library(ggplot2)
library(FactoMineR)
library(factoextra)


# Perform PCA
pca_result <- PCA(stripcropping_fields[,c("area_field","mean_area_strip", "mean_width", "var_width", "num_rows", "nr_crops", "var_orientation")], scale.unit = TRUE, graph = FALSE)

# Visualize PCA
fviz_pca_ind(pca_result,
             geom.ind = "point", 
             col.ind = stripcropping_fields$cluster, 
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Clusters")

# View PCA results
summary(pca_result)

manova_result <- manova(cbind(mean_width, num_rows, area_field, nr_crops,var_orientation) ~ lda, data = stripcropping_fields)

# Summary of MANOVA
summary(manova_result)

# Detailed test results for each dependent variable
summary.aov(manova_result)

lm_width_rows <- lm(mean_width ~ num_rows, data = stripcropping_fields)
summary(lm_width_rows)
rsq2 <- summary(lm_width_rows)$r.squared
width_rows <- ggplot(stripcropping_fields, aes(x = mean_width, y = num_rows, colour = cluster)) +
  geom_point()+ 
  scale_y_continuous(trans='log10') + 
  labs(title = "Mean Width vs Nr of Rows",
       x = "Mean width [m]",
       y = "Number of rows") +
  theme_minimal() 
print(width_rows)


anova_model <- aov(mean_width ~ area * num_rows, data = stripcropping_fields)
summary(anova_model)

anova_model2 <- aov(area  ~  mean_width * num_rows, data = stripcropping_fields)
summary(anova_model2)

anova_model3 <- aov(num_rows  ~  mean_width * area, data = stripcropping_fields)
summary(anova_model3)



########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
    
     #### Data about provinces ####

info_provinces <- data.frame()
crops_info_list <- list()

provinces <- unique(stripcropping_fields$province)


for (k in provinces) {
  subset <- stripcropping_fields[stripcropping_fields$province == k,]
  subset2 <- stripcropping_with_province[stripcropping_with_province$Provincienaam == k,]
  nr_fields <- nrow(subset)
  mean_area <- mean(subset$area)
  total_area <- sum(subset$area)
  crops_province <- data.frame()
  crop_list <- unique(subset2$gnrc_cr)
  div <- length(crop_list)
  for (l in crop_list) {
    subset3 <- subset2[subset2$gnrc_cr == l,]
    area <- sum(subset3$area)
    freq <- mean(subset3$frqncy_)
    thing <- c(l, area, freq)
    crops_province <- rbind(crops_province, thing)
  }
  names <- c("crop", "total_area", "mean_freq")
  colnames(crops_province) <- names
  crops_province$total_area <- as.numeric(crops_province$total_area)
  crops_province$mean_freq <- as.numeric(crops_province$mean_freq)
  crops_province <- crops_province[order(-crops_province[,2]),]
  crops_info_list <- append(crops_info_list, list(crops_province))
  most_prev <- (crops_province[1:4,1])
  most_prev <- paste(most_prev, collapse = ", ")
  
  thing <- c(k, as.numeric(nr_fields), as.numeric(mean_area),total_area, most_prev, as.numeric(div))
  info_provinces <- rbind(info_provinces, thing)
}
names <- c("province", "nr of fields", "mean area of fields [ha]","total area stripcropping [ha]", "most prevelent crops", "crop diversity")
colnames(info_provinces) <- names
info_provinces$`nr of fields` <- as.numeric(info_provinces$`nr of fields`)
info_provinces$`mean area of fields [ha]` <- (round(as.numeric(info_provinces$`mean area of fields [ha]`)/10000, digits = 2))
info_provinces$`total area stripcropping [ha]` <- round(as.numeric(info_provinces$`total area stripcropping [ha]`)/10000, digits = 2)
info_provinces$`crop diversity` <- as.numeric(info_provinces$`crop diversity`)
info_provinces <- info_provinces[order(-info_provinces$`total area stripcropping [ha]`),]

path <- "final_files/info_provinces.xlsx"
write.xlsx(info_provinces, path)

sum(info_provinces$`total area stripcropping [ha]`)


##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########