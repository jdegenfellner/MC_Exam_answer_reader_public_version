# Read_Points.R
# From: https://github.com/jdegenfellner/MC_Exam_answer_reader

# File info:-------
# This script reads scanned multiple-choice exam answer sheets,
# extracts the filled answer boxes and identification numbers,
# and calculates the scores based on a predefined answer key.

# TODO - issues-----------
# - Manuelle Korrekturen der Box-Positionen (OFFSET) sind nötig
# - Automatisierte Kalibrierung der Box-Positionen

library(pacman)
p_load(tidyverse, pdftools, magick, EBImage,
       reticulate, readxl, writexl, data.table, tictoc)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

#use_virtualenv(".venv")  # or complete path:
use_virtualenv("~/Library/Mobile Documents/com~apple~CloudDocs/1_ZHAW/MC_Exam_answer_reader/.venv")
py_config()

getwd()

py_run_string("
import cv2
import numpy as np
from pdf2image import convert_from_path

# PDF in Bild umwandeln
#pages = convert_from_path('4_Antwortblatt_fuer_autoread_for_determination_of_box_positions_SCANNED.pdf', dpi=600)
pages = convert_from_path('4_Antwortblatt_fuer_autoread_for_determination_of_box_positions.pdf', dpi=600)
pages[0].save('Antwortblatt_tmp.png', 'PNG')

# Bild einlesen & drehen (180°)
image = cv2.imread('Antwortblatt_tmp.png', cv2.IMREAD_GRAYSCALE)
image_rotated = cv2.rotate(image, cv2.ROTATE_180)

# Binarisieren (angepasster Schwellwert)
_, thresh = cv2.threshold(image_rotated, 180, 255, cv2.THRESH_BINARY_INV)

# Konturen finden
contours, _ = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

# Kästchen extrahieren (nach Fläche filtern)
boxes = []
box_counter = 0
for cnt in contours:
    x, y, w, h = cv2.boundingRect(cnt)
    area = cv2.contourArea(cnt)
    if 6200 < area < 7800:
        cx = x + w / 2   # center x
        cy = y + h / 2   # center y
        #boxes.append((cx, cy))
        boxes.append((cx, cy, w, h))
        box_counter += 1
        print('found at:', cx, cy, 'box counter:', box_counter)
# ---
#areas = []
#wh = []
#for cnt in contours:
#    x,y,w,h = cv2.boundingRect(cnt)
#    a = cv2.contourArea(cnt)
#    areas.append(a)
#    wh.append((w,h))

#areas = np.array(areas, dtype=float)
#wh = np.array(wh, dtype=float)

#print('n contours:', len(areas))
#print('area min/median/max:', areas.min(), np.median(areas), areas.max())
#print('area quantiles:', np.percentile(areas, [1,5,10,25,50,75,90,95,99]))

# ---

# Sortieren nach Zeile + Spalte
boxes_sorted = sorted(boxes, key=lambda k: (k[1], k[0]))
boxes_array = np.array(boxes_sorted)
")
# should find 150 box positions: 50 identification + 100 answer boxes:
py$box_counter # 150

box_positions <- py$boxes_array # get result
dim(box_positions) # 150 x 4
head(box_positions)

mean_box_width  <- mean(box_positions[,3]) # 84
mean_box_height <- mean(box_positions[,4]) # 84

col_crosses <- "purple"

plot(box_positions[,1], box_positions[,2], pch = 19, col = col_crosses,,
     main = "Found box positions", xlab = "X", ylab = "Y")
# -> works!

# # Read manually filled answer sheet and test positions found above------------

# Coordinate system:
# ------> x
# |
# |
# |
# v
# y

# Lade das eingesannte, manuell ausgefüllte Blatt (erste Seite als PNG)
getwd()
dir.create("ABGABEN/_png", showWarnings = FALSE)
system2("mutool", c(
  "draw",
  "-r", "600",
  "-o", "ABGABEN/_png/TEST_%03d.png",
  "ABGABEN/QM1_17.12.25_Scan1_007.pdf"
))
img_filled <- image_read("ABGABEN/_png/page_001.png")
print(img_filled) # nice

img_info <- image_info(img_filled)
img_height <- img_info$height

# Koordinaten um 180° drehen (Spiegelung in X + Y)
box_positions_rotated <- box_positions
box_positions_rotated[,1] <- img_info$width - box_positions[,1]
box_positions_rotated[,2] <- img_height - box_positions[,2]

# Define OFFSET for ALL 150 answer boxes----------
# This moves all box centers

# a) offset for pdf, not scanned:
x_offset <- 165 # negative Werte nach links/positiv nach rechts
y_offset <- -405  # positive Werte nach unten/negativ nach oben

# b) offset for scanned version:
#---

box_positions_rotated[,1] <- box_positions_rotated[,1] + x_offset
box_positions_rotated[,2] <- box_positions_rotated[,2] + y_offset

plot(box_positions_rotated) # image upside down

#CHECKING box positions-----------
tic()
img_filled <- image_draw(img_filled)
for (i in 1:nrow(box_positions_rotated)) {
  cx <- box_positions_rotated[i, 1]
  cy <- box_positions_rotated[i, 2]

  #img_filled <- image_draw(img_filled)
  #segments(cx - 10, cy - 10, cx + 10, cy + 10, col = "red", lwd = 3)
  #segments(cx - 10, cy + 10, cx + 10, cy - 10, col = "red", lwd = 3)
  segments(cx - mean_box_width/2, cy - mean_box_height/2, 
           cx + mean_box_width/2, cy + mean_box_height/2, 
           col = col_crosses, lwd = 3)
  segments(cx - mean_box_width/2, cy + mean_box_height/2, 
           cx + mean_box_width/2, cy - mean_box_height/2, 
           col = col_crosses, lwd = 3)
  
  #dev.off()
}
dev.off()
toc() # 6s
#image_write(img_filled, "DEBUG_Kreuze_eingezeichnet_rotated.png")
print(img_filled)
# -> this should show red crosses perfectly in all box positions.


# Identify ID-Boxes----------------------
id_boxes <- box_positions_rotated[box_positions_rotated[,2] < 0.4 * img_height, ]
id_boxes_sorted <- id_boxes[order(id_boxes[,2], id_boxes[,1]), ]

# Identify answer boxes----------------
answer_boxes <- box_positions_rotated[box_positions_rotated[,2] >= 0.4 * img_height, ]


# Define OFFSET ID boxes-------------
# This only moces ID box centers (if needed)
x_offset_id <-0 # negative Werte nach links/positiv nach rechts
y_offset_id <- 0  # positive Werte nach unten/negativ nach oben
id_boxes_sorted[,1] <- id_boxes_sorted[,1] + x_offset_id
id_boxes_sorted[,2] <- id_boxes_sorted[,2] + y_offset_id

# CHECK ID boxes--------------
tic()
img_filled <- image_draw(img_filled)
for (i in 1:nrow(id_boxes_sorted)) {
  cx <- id_boxes_sorted[i, 1]
  cy <- id_boxes_sorted[i, 2]
  
  #img_filled <- image_draw(img_filled)
  segments(cx - 10, cy - 10, cx + 10, cy + 10, col = "red", lwd = 3)
  segments(cx - 10, cy + 10, cx + 10, cy - 10, col = "red", lwd = 3)
  #dev.off()
}
dev.off()
toc() # 6s
plot(img_filled)


#----


# Antworten sortieren (nach Zeile/Y und Spalte/X)
answer_boxes_sorted <- answer_boxes[order(answer_boxes[,2], answer_boxes[,1]), ]

df_points <- as.data.frame(answer_boxes_sorted)
colnames(df_points) <- c("V1", "V2", "width", "height")

#plot(df_points$V1, df_points$V2)

# Median X-Wert als Schwelle
x_threshold <- mean(df_points$V1)

# Links und rechts aufteilen
df_left <- df_points %>% dplyr::filter(V1 < x_threshold)
df_right <- df_points %>% dplyr::filter(V1 >= x_threshold)
#dim(df_left) # 48 rows = 12 questions x 4 options
#dim(df_right) # 52 rows = 13 questions x 4 options

# Sortiere links: erst Y (von oben nach unten), dann X
df_left_sorted <- df_left %>%
  dplyr::arrange(V2, V1) %>%
  dplyr::mutate(question = rep(1:12, each = 4),
         option = rep(c("A", "B", "C", "D"), 12))

# Sortiere rechts: auch Y dann X
df_right_sorted <- df_right %>%
  dplyr::arrange(V2, V1) %>%
  dplyr::mutate(question = rep(13:25, each = 4),
         option = rep(c("A", "B", "C", "D"), 13))

df_points_final <- bind_rows(df_left_sorted, df_right_sorted) %>%
  arrange(question, option)



# Functions---------------------------------------------------------------------
check_if_filled <- function(center_x, center_y, box_size, img, threshold = 0.5) {
  geometry_string <- glue::glue("{box_size}x{box_size}+{center_x - box_size/2}+{center_y - box_size/2}")
  sub_img <- image_crop(img, geometry = geometry_string)
  sub_img_gray <- image_convert(sub_img, colorspace = "gray")
  intensity_values <- as.numeric(image_data(sub_img_gray))
  mean_intensity <- mean(intensity_values)
  print("Mean intensity:")
  print(mean_intensity)
  return(mean_intensity < threshold)
}

read_identification_number <- function(img, id_boxes_sorted, 
                                       box_size = 84, 
                                       threshold = 0.5) {
  n_boxes <- nrow(id_boxes_sorted)
  if (n_boxes != 50) {
    warning(glue::glue("⚠️ Erwartet: 50 ID-Boxen, gefunden: {n_boxes}"))
  }
  
  id_bits <- logical(n_boxes)
  
  for (i in seq_len(n_boxes)) {
    cx <- id_boxes_sorted[i, 1]
    cy <- id_boxes_sorted[i, 2]
    id_bits[i] <- check_if_filled(cx, cy, box_size, img, threshold)
  }
  
  id_selected <- which(id_bits)
  
  if (length(id_selected) == 0) {
    warning("⚠️ Keine ID angekreuzt!")
    id_number <- NA
  } else if (length(id_selected) > 1) {
    warning(glue::glue("⚠️ Mehrere IDs angekreuzt: {paste(id_selected, collapse = ', ')}"))
    id_number <- id_selected
  } else {
    id_number <- id_selected
  }
  
  return(list(id_bits = id_bits, id_number = id_number))
}

read_answers <- function(img, df_points, box_size = 10, threshold = 0.5) {
  results <- data.frame(matrix(FALSE, nrow = 25, ncol = 4))
  colnames(results) <- c("A", "B", "C", "D")
  
  for (i in seq_len(nrow(df_points))) {
    cx <- df_points$V1[i]
    cy <- df_points$V2[i]
    
    # Frage und Option bestimmen:
    question_idx <- ceiling(i / 4)
    option_idx <- ((i - 1) %% 4) + 1

    is_filled <- check_if_filled(cx, cy, box_size, img, threshold)
    
    results[question_idx, option_idx] <- is_filled
  }
  
  return(results)
}


save_page_with_id_cross <- function(img,
                                    page,
                                    id_boxes_sorted,
                                    id_result,
                                    outdir = "./Single_Pages_with_ID",
                                    cross_scale = 0.45,
                                    lwd = 6,
                                    file_prefix = "page") {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # id_result kommt aus read_identification_number()
  id_number <- id_result$id_number
  
  # Welche Box(en) sollen markiert werden?
  ids_to_mark <- integer(0)
  if (is.numeric(id_number) && length(id_number) > 0 && !all(is.na(id_number))) {
    ids_to_mark <- as.integer(id_number)
  }
  
  # Bild zeichnen
  img_drawn <- image_draw(img)
  
  if (length(ids_to_mark) > 0) {
    for (idx in ids_to_mark) {
      if (idx >= 1 && idx <= nrow(id_boxes_sorted)) {
        cx <- id_boxes_sorted[idx, 1]
        cy <- id_boxes_sorted[idx, 2]
        
        # Cross size: wenn width/height vorhanden → nutzen, sonst fallback 30
        if (ncol(id_boxes_sorted) >= 4) {
          w <- id_boxes_sorted[idx, 3]
          h <- id_boxes_sorted[idx, 4]
          s <- cross_scale * min(w, h)
        } else {
          s <- 30
        }
        
        segments(cx - s, cy - s, cx + s, cy + s, col = "red", lwd = lwd)
        segments(cx - s, cy + s, cx + s, cy - s, col = "red", lwd = lwd)
      }
    }
  } else {
    # Optional: wenn keine ID gefunden wurde, schreibe Hinweis oben links ins Bild
    text(50, 80, labels = "NO ID DETECTED", col = "red", cex = 2, pos = 4)
  }
  
  dev.off()
  
  outfile <- file.path(outdir, sprintf("%s_%03d.png", file_prefix, page))
  image_write(img_drawn, path = outfile, format = "png")
  
  invisible(outfile)
}


# CHOOSE PDF------------
getwd()
pdf_file <- "./ABGABEN/QM1_17.12.25_Scan1.pdf"
n_pages <- pdf_info(pdf_file)$pages

all_ids <- list() # Identifikationsnummern
all_answers <- list() # 25 x 4 Antworten per Identifikationsnummer

# Zielordner
#dir.create("./ABGABEN/_png", showWarnings = FALSE)

# Anzahl Seiten
n_pages <- pdftools::pdf_info(pdf_file)$pages

# Rendern (robust) mit mutool, falls vorhanden – sonst Fallback auf pdftools
png_pattern <- "./ABGABEN/_png/page_%03d.png"

if (nzchar(Sys.which("mutool"))) {
  system2(
    "mutool",
    args = c(
      "draw",
      "-r", "600",
      "-o", png_pattern,
      pdf_file
    )
  )
} else {
  # Fallback: rendert Seite für Seite
  for (p in 1:n_pages) {
    out <- sprintf(png_pattern, p)
    pdftools::pdf_render_page(pdf_file, page = p, dpi = 600, output = out)
  }
}
# 31 pages converted...

# Liste der erzeugten PNGs
png_files <- sprintf(png_pattern, 1:n_pages)



# ---- Loop über alle Seiten ----
for (page in 1:n_pages) {
  message(glue::glue("🔄 Verarbeite Seite {page}/{n_pages}..."))
  
  # 1️⃣ Seite einlesen
  #img_filled <- image_read_pdf(pdf_file, density = 300, pages = page)
  img_filled <- image_read(png_files[page])
  
  img_info <- image_info(img_filled)
  img_height <- img_info$height
  
  # 2️⃣ Koordinaten anpassen
  box_positions_rotated <- box_positions
  # OFFSET:
  x_offset <- 165 # negative Werte nach links/positiv nach rechts
  y_offset <- -405  # positive Werte nach unten/negativ nach oben
  box_positions_rotated[,1] <- img_info$width - box_positions[,1] + x_offset
  box_positions_rotated[,2] <- img_height - box_positions[,2] + + y_offset
  
  # 3️⃣ Boxen splitten
  id_boxes <- box_positions_rotated[box_positions_rotated[,2] < 0.4 * img_height, ]
  answer_boxes <- box_positions_rotated[box_positions_rotated[,2] >= 0.4 * img_height, ]
  
  id_boxes_sorted <- id_boxes[order(id_boxes[,2], id_boxes[,1]), ]
  
  df_points <- as.data.frame(answer_boxes)
  colnames(df_points) <- c("V1", "V2", "width", "height")
  
  # Trennen in linke & rechte Spalte
  x_threshold <- mean(df_points$V1)
  
  df_left <- df_points %>% dplyr::filter(V1 < x_threshold)
  df_right <- df_points %>% dplyr::filter(V1 >= x_threshold)
  
  df_left_sorted <- df_left %>%
    dplyr::arrange(V2, V1) %>%
    dplyr::mutate(question = rep(1:12, each = 4),
           option = rep(c("A", "B", "C", "D"), 12))
  
  df_right_sorted <- df_right %>%
    dplyr::arrange(V2, V1) %>%
    dplyr::mutate(question = rep(13:25, each = 4),
           option = rep(c("A", "B", "C", "D"), 13))
  
  df_points_final <- bind_rows(df_left_sorted, df_right_sorted) %>%
    dplyr::arrange(question, option)
  
  # 4️⃣ IDs einlesen
  id_result <- read_identification_number(img_filled, id_boxes_sorted)
  all_ids[[page]] <- id_result
  
  # DEBUG-Bild mit rotem Kreuz speichern
  save_page_with_id_cross(
    img = img_filled,
    page = page,
    id_boxes_sorted = id_boxes_sorted,
    id_result = id_result,
    outdir = "./Single_Pages_with_ID"
  )
  
  # 5️⃣ Antworten einlesen
  answers_df <- read_answers(img_filled, df_points_final)
  all_answers[[page]] <- answers_df
}

# ---- Ausgabe zusammenfassen ----
# IDs
print(all_ids)
all_ids[[1]] # ID der ersten Seite
# Extrahiere die ID-Nummern als numeric vector
id_numbers <- map_dbl(all_ids, ~ .x$id_number)
print(id_numbers)

# Antworten (erste Seite als Beispiel)
print(all_answers[[1]])
all_answers



# Punkte berechnen------------
correct_answers <- readRDS("correct_answers.RDS")
scores <- map_dbl(all_answers, ~ sum(.x == correct_answers))
print(scores)


# Ergebnisse----
df <- data.frame(
  Identifikationsnummern = id_numbers,
  Punkte = scores
)
df 

# Read Identifikationsnummern and create Namen_Punkte___.xlsx

df_IDs <- read_excel("Identifikationsnummern.xlsx")

df <- df %>%
  dplyr::left_join(df_IDs, by = "Identifikationsnummern")


df

# ADD points for additional riddle:
# df_riddle <- read_excel("Identifikationsnummern_Zusatzbsp_korrekt.xlsx")
# 
# df <- df %>%
#   dplyr::left_join(df_riddle %>% dplyr::select(-c(Vorname, Nachname)), by = "Identifikationsnummern")
# head(df)
# df$PunkteZusatzBsp <- ifelse(is.na(df$PunkteZusatzBsp), 0, df$PunkteZusatzBsp)
# df
# 
# df <- df %>% 
#   dplyr::mutate(Punkte_incl_bonus = Punkte +PunkteZusatzBsp)
# df

write_xlsx(df, "./ABGABEN/Namen_Punkte_17.7.25_Scan1.xlsx")
