# Read_Points.R

# Future improvements:
# - If too imprecise, one can place red dots (or whatever) next to each row,
#   this would give the very exact y-location of the 25 rows.
# - Further improvement (to recognize distortion), a coordinate system relative 
#   to more than one point could be introduced.
# - one could also make the windows larger and detect the black box itself
# - one can put an anchor at the beginning and the end of the identification
#   numbers and interpolate linearly

# Note: Currently box positions for answers and identification numbers
# are determined manually in the calibration file.

# CAUTION: An error occurs if less than 50 identification numbers are in the 
# file Idendifikationsnummern.xlsx

library(pacman)
p_load(tidyverse, pdftools, magick, EBImage, readxl, writexl, data.table)

# Set working directory
# Example
setwd(".../MC_Exam_answer_reader_COPY_PASTE_FROM_orig_Git_folder")

# READ ----
# Example
pdf_path <- "..../Pruefung_Antritt_1_6.6.24_Scan1.pdf"

# Functions----
choose_anchor_point <- function(object_centers) {
  for(i in 1:4){
    y_middle <- mean(c(min(object_centers[,"m.cy"]), max(object_centers[,"m.cy"])))
    x_middle <- mean(c(min(object_centers[,"m.cx"]), max(object_centers[,"m.cx"])))
    if(object_centers[i,"m.cx"] < x_middle &
       object_centers[i,"m.cy"] < y_middle){
      upper_left <- object_centers[i,c("m.cx","m.cy")]
    }
    if(object_centers[i,"m.cx"] > x_middle &
       object_centers[i,"m.cy"] < y_middle) {
      upper_right <- object_centers[i,c("m.cx","m.cy")]
    }
  }
  return(list(upper_left = upper_left, upper_right = upper_right))
}

is_filled <- function(center_x, center_y, box_size, img, threshold = 0.5) {
  geometry_string <- paste0(box_size, "x", box_size, "+", 
                            center_x - box_size / 2, "+", 
                            center_y - box_size / 2)
  sub_img <- image_crop(img, geometry = geometry_string)
  sub_img_gray <- image_convert(sub_img, colorspace = "gray")
  intensity_values <- as.numeric(image_data(sub_img_gray))
  mean_intensity <- mean(intensity_values)
  return(mean_intensity < threshold)
}

correction_of_answers <- function(original_sheet, correction_sheet){
  corrected_sheet <- original_sheet
  for (i in 1:nrow(original_sheet)) {
    if (!identical(original_sheet[i, ], correction_sheet[i, ])) {
      corrected_sheet[i, ] <- correction_sheet[i, ]
    }
  }
  return(corrected_sheet)
}

# Initialize for loop
num_pages <- pdf_length(pdf_path)
all_answers <- list()
all_IDs <- list()

# 1) LOOP over pages:----
for (page in 1:(num_pages)) {
  # _a) Read image-----
  image_path <- pdf_convert(pdf_path, format = 'png', pages = page, dpi = 300)
  img <- image_read(image_path[[1]])
  img_array <- as.numeric(image_data(img)) / 255  # Skaliere die Werte auf [0, 1]
  red_channel <- img_array[,,1]
  green_channel <- img_array[,,2]
  blue_channel <- img_array[,,3]
  
  red_threshold <- 0.6 * max(red_channel)
  green_threshold <- 0.6 * max(green_channel)
  blue_threshold <- 0.6 * max(blue_channel)
  
  red_mask <- (red_channel > red_threshold) & (green_channel < green_threshold) & (blue_channel < blue_threshold)
  red_mask_image <- EBImage::Image(red_mask)
  
  # _Detect red anchors----
  red_mask_image <- EBImage::opening(red_mask_image, makeBrush(5, shape = 'disc'))
  #EBImage::display(red_mask_image)
  labeled <- EBImage::bwlabel(red_mask_image)
  rotated_image <- EBImage::rotate(labeled, angle = 90)
  corrected_image <- EBImage::flop(rotated_image)
  #EBImage::display(corrected_image, method="raster")
  #points(object_centers[,"m.cx"], object_centers[,"m.cy"], col = "blue", pch = 4)
  object_centers <- EBImage::computeFeatures.moment(corrected_image)
  
  # Coordinate system:
  # ------>
  # |
  # |
  # |
  # v 
  
  # Function to choose the upper left or upper right anchor point (out of 4)
  
  df_answers <- data.frame(first = rep(FALSE,13), 
                           second = rep(FALSE,13),
                           third = rep(FALSE,13),
                           fourth = rep(FALSE,13))
  
  
  
  # _b) Read Questions 1-13----
  
  # _Box distances----
  dist_boxes_x <- 392 - 235
  dist_boxes_y <- 1366/12
  
  # _Choose anchor (upper left)
  anchor <- choose_anchor_point(object_centers)$upper_left
  
  # Use the function for each box, assuming object_centers is available and dist_boxes_x, dist_boxes_y are set
  for (i in 0:12) {
    for (j in 0:3) {
      box_center_x <- anchor[1] + 235 + j * dist_boxes_x
      box_center_y <- anchor[2] + 82 + i * dist_boxes_y
      df_answers[i+1, j+1] <- is_filled(box_center_x, box_center_y, 30, img)
    }
  }
  
  #df_answers
  
  # _c) Read Questions 14-25----
  
  # _Choose anchor (upper right)
  anchor <- choose_anchor_point(object_centers)$upper_right
  
  # _Box distances----
  dist_boxes_y_14_25 <- (1443-81)/11
  
  df_answers_right <- data.frame(first = rep(FALSE,12), # 12 in right column
                                 second = rep(FALSE,12),
                                 third = rep(FALSE,12),
                                 fourth = rep(FALSE,12))
  
  # Use the function for each box, assuming object_centers is available and dist_boxes_x, dist_boxes_y are set
  for (i in 0:11) {
    for (j in 0:3) {
      box_center_x <- anchor[1] - 566 + j * dist_boxes_x
      box_center_y <- anchor[2] + 81 + i * dist_boxes_y_14_25
      df_answers_right[i+1, j+1] <- is_filled(box_center_x, box_center_y, 30, img)
    }
  }
  
  #df_answers_right
  
  df_answers_all <- rbind(df_answers, df_answers_right) # Answers from both columns
  #df_answers_all
  
  # _d) Read Idendification number (Identifikationsnummer)----
  path <- "./Identifikationsnummern.xlsx"
  df <- read_excel(path)
  #View(df)
  
  # _Detect green anchors----
  green_mask <- (green_channel > green_threshold) & (red_channel < red_threshold) & (blue_channel < blue_threshold)
  green_mask_image <- EBImage::Image(green_mask)
  green_mask_image <- EBImage::opening(green_mask_image, makeBrush(5, shape = 'disc'))
  #EBImage::display(green_mask_image)
  labeled <- EBImage::bwlabel(green_mask_image)
  rotated_image <- EBImage::rotate(labeled, angle = 90)
  corrected_image <- EBImage::flop(rotated_image)
  # Display the corrected image if needed
  #EBImage::display(corrected_image, method="raster")
  object_centers <- EBImage::computeFeatures.moment(corrected_image)
  
  left_green_anchor <- object_centers[which.min(object_centers[,"m.cx"]), c("m.cx", "m.cy")]
  right_green_anchor <- object_centers[which.max(object_centers[,"m.cx"]), c("m.cx", "m.cy")]
  
  # _Box distances----
  #dist_boxes_x_ID <- (1728 - 30)/24
  dist_boxes_x_ID <- (1743 - 38)/24 # from scan
  #dist_boxes_x_ID <- (1755 - 50)/24 # from calibration orig pdf file (same!!)
  
  # _Reading----
  ID_numbers <- data.frame(first_row = rep(FALSE,25), 
                           second_row = rep(FALSE,25))
  
  # Use the function for each box, assuming object_centers is available and dist_boxes_x, dist_boxes_y are set
  for (i in 0:24) {
    for (j in 0:1) {
      # First row
      box_center_x <- left_green_anchor[1] + 51 + i*dist_boxes_x_ID
      box_center_y <- left_green_anchor[2]
      ID_numbers[i+1, 1] <- is_filled(box_center_x, box_center_y, 30, img)
      # Second row
      box_center_x <- left_green_anchor[1] + 51 + i*dist_boxes_x_ID
      box_center_y <- left_green_anchor[2] + 106
      ID_numbers[i+1, 2] <- is_filled(box_center_x, box_center_y, 30, img)
    }
  }
  #ID_numbers
  IDs <- data.frame(ID = 1:50, ID_ticked = c(ID_numbers$first_row, ID_numbers$second_row))
  #IDs
  
  # Ersetzen Sie df_answers_all und IDs mit Ihren Ergebnissen
  all_answers[[page]] <- df_answers_all  
  all_IDs[[page]] <- IDs  
}

#all_answers[[1]]
#length(all_answers)

#all_IDs <- lapply(all_IDs, as.data.table)
#all_IDs[[1]][ID_ticked == TRUE, c("ID")]

#length(all_IDs)


# 2) Compare with Solution and determine points-----
IDs_on_pages <- c()
for( i in 1:num_pages ) {
IDs_on_pages <- append(IDs_on_pages, which(all_IDs[[i]]$ID_ticked==TRUE))
}
correct_answers <- readRDS("correct_answers.RDS")

# ANY correction sheets are present
if( any(which(table(IDs_on_pages) > 1)) ) { 
  double_IDs <- as.numeric(names(table(IDs_on_pages)[which(table(IDs_on_pages) > 1)])) # at least double
  print(paste0("Folgende Identifikationsnummern haben mehr als 1 Antwortblatt abgegeben: ", double_IDs))
  
  # Which one (of the both) is the correction sheet?
  sum_ticked <- data.frame(IDs_on_pages = IDs_on_pages, sum_points_on_sheet = NA)
  for(i in 1:num_pages){
    sum_ticked$sum_points_on_sheet[i] <- sum(all_answers[[i]])
  }
  sum_ticked <- sum_ticked %>%
    group_by(IDs_on_pages) %>%
    mutate(correction_sheet = ifelse((IDs_on_pages %in% double_IDs) & 
                                       (sum_points_on_sheet == min(sum_points_on_sheet)), 
                                     TRUE, FALSE)) %>% # mark the correction sheets
    ungroup()
  # Correction_of_answers:
  for( j in 1:length(double_IDs) ) {
    ind_orig_sheet <- which(sum_ticked$IDs_on_pages == double_IDs[j] & sum_ticked$correction_sheet == TRUE)
    ind_corr_sheet <- which(sum_ticked$IDs_on_pages == double_IDs[j] & sum_ticked$correction_sheet == FALSE)
    all_answers[[ind_orig_sheet]] <- correction_of_answers(all_answers[[ind_orig_sheet]], 
                                                           all_answers[[ind_corr_sheet]])
  }
  
  ind_all_orig <- which(sum_ticked$correction_sheet == FALSE)
  all_answers <- all_answers[ind_all_orig]
  IDs_for_points <- IDs_on_pages[ind_all_orig]
  
  df_ID_Points <- data.frame(ID = numeric(50), Points = numeric(50))
  
  for( i in 1:length(ind_all_orig) ){
    df_ID_Points[i,]$ID <- IDs_for_points[i]
    df_ID_Points[i,]$Points <- sum(correct_answers == all_answers[[i]]) # compare and sum correct answers
  }
  IDs_with_points <- df_ID_Points$ID[which(df_ID_Points$ID != 0)]
  
  df$Punkte <- numeric(50)
  df <- as.data.table(df)
  df_ID_Points <- as.data.table(df_ID_Points)
  
  for(i in 1:50){
    if( df_ID_Points$ID[i] > 0){
      ID_number_to_write_to <- df_ID_Points$ID[i]
      df$Punkte[ID_number_to_write_to] <- df_ID_Points$Points[i]
    }
  }
  #df[Identifikationsnummer %in% IDs_with_points,]$Punkte <- df_ID_Points[ID %in% IDs_with_points,]$Points
  
  
  write_xlsx(df, "Namen_Punkte.xlsx")
  
} else { # No correction sheets in the stack
  df_ID_Points <- data.frame(ID = numeric(50), Points = numeric(50))
  
  for( i in 1:num_pages ){
    df_ID_Points[i,]$ID <- which(all_IDs[[i]]$ID_ticked==TRUE)
    df_ID_Points[i,]$Points <- sum(correct_answers == all_answers[[i]])
  }
  
  IDs_with_points <- df_ID_Points$ID[which(df_ID_Points$ID != 0)]
  
  df$Punkte <- numeric(50)
  df <- as.data.table(df)
  df_ID_Points <- as.data.table(df_ID_Points)
  
  for(i in 1:50){
    if( df_ID_Points$ID[i] > 0){
      ID_number_to_write_to <- df_ID_Points$ID[i]
      df$Punkte[ID_number_to_write_to] <- df_ID_Points$Points[i]
    }
  }
  
  #df[Identifikationsnummer %in% IDs_with_points,]$Punkte <- df_ID_Points[ID %in% IDs_with_points,]$Points
  
  write_xlsx(df, "Namen_Punkte.xlsx")
}

# Overview----
df %>% filter(Punkte > 0 & Punkte < 60)
max(df$Punkte)

# Punktehistogram----
df %>% filter(Punkte > 0) %>%
ggplot(aes(x=Punkte)) + 
  geom_histogram()

summary(df$Punkte[df$Punkte>0])


