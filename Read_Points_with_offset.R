# Read_Points.R

# From: https://github.com/jdegenfellner/MC_Exam_answer_reader

library(pacman)
p_load(tidyverse, pdftools, magick, EBImage,
       reticulate, readxl, writexl, data.table, tictoc)

getwd()

#use_virtualenv(".venv")  # oder den kompletten Pfad, wenn du nicht im Projektordner bist
use_virtualenv("~/Library/Mobile Documents/com~apple~CloudDocs/1_ZHAW/MC_Exam_answer_reader/.venv")
py_config()

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

py_run_string("
import cv2
import numpy as np
from pdf2image import convert_from_path

# PDF in Bild umwandeln
pages = convert_from_path('4_Antwortblatt_fuer_autoread_for_determination_of_box_positions_SCANNED.pdf', dpi=300)
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
for cnt in contours:
    x, y, w, h = cv2.boundingRect(cnt)
    area = cv2.contourArea(cnt)
    if 200 < area < 3000:
        cx = x + w / 2
        cy = y + h / 2
        boxes.append((cx, cy))

# Sortieren nach Zeile + Spalte
boxes_sorted = sorted(boxes, key=lambda k: (k[1], k[0]))
boxes_array = np.array(boxes_sorted)
")

box_positions <- py$boxes_array # get result

plot(box_positions[,1], box_positions[,2], pch = 19, col = "red",
     main = "Gefundene Kästchen", xlab = "X", ylab = "Y")
# -> works!

# # Read manually filled answer sheet and test positions found above----------

# Coordinate system:
# ------> x
# |
# |
# |
# v
# y

# Lade das eingesannte, manuell ausgefüllte Blatt (erste Seite als PNG)
img_filled <- image_read_pdf("./ABGABEN/TEST.pdf",
                             density = 300)
print(img_filled)

img_info <- image_info(img_filled)
img_height <- img_info$height

# Koordinaten um 180° drehen (Spiegelung in X + Y)
box_positions_rotated <- box_positions
box_positions_rotated[,1] <- img_info$width - box_positions[,1]
box_positions_rotated[,2] <- img_height - box_positions[,2]

# 🛠️ Manuelle Korrektur bei gescanntem Blatt
x_offset <- 75 # negative Werte nach links
y_offset <- -210  # positive Werte nach unten

box_positions_rotated[,1] <- box_positions_rotated[,1] + x_offset
box_positions_rotated[,2] <- box_positions_rotated[,2] + y_offset

plot(box_positions_rotated)

#CHECKING box positions-----------
tic()
img_filled <- image_draw(img_filled)
for (i in 1:nrow(box_positions_rotated)) {
  cx <- box_positions_rotated[i, 1]
  cy <- box_positions_rotated[i, 2]
  
  #img_filled <- image_draw(img_filled)
  segments(cx - 10, cy - 10, cx + 10, cy + 10, col = "red", lwd = 3)
  segments(cx - 10, cy + 10, cx + 10, cy - 10, col = "red", lwd = 3)
  dev.off()
}
toc() # 90s
#image_write(img_filled, "DEBUG_Kreuze_eingezeichnet_rotated.png")
print(img_filled)
# -> this should show red crosses perfectly in all box positions.

# ID-Boxen filtern (alles oberhalb von 40% der Bildhöhe)
id_boxes <- box_positions_rotated[box_positions_rotated[,2] < 0.4 * img_height, ]

# Antwort-Boxen filtern (alles darunter)
answer_boxes <- box_positions_rotated[box_positions_rotated[,2] >= 0.4 * img_height, ]

# IDs schön sortieren (erst nach Zeile/Y, dann nach Spalte/X)
id_boxes_sorted <- id_boxes[order(id_boxes[,2], id_boxes[,1]), ]

# Antworten sortieren (nach Zeile/Y und Spalte/X)
answer_boxes_sorted <- answer_boxes[order(answer_boxes[,2], answer_boxes[,1]), ]

# Median X-Wert als Schwelle
x_threshold <- mean(df_points$V1)

# Links und rechts aufteilen
df_left <- df_points %>% dplyr::filter(V1 < x_threshold)
df_right <- df_points %>% dplyr::filter(V1 >= x_threshold)

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

# Kombiniere alles wieder
df_points_final <- bind_rows(df_left_sorted, df_right_sorted) %>%
  arrange(question, option)











# Functions--------------
check_if_filled <- function(center_x, center_y, box_size, img, threshold = 0.5) {
  geometry_string <- glue::glue("{box_size}x{box_size}+{center_x - box_size/2}+{center_y - box_size/2}")
  sub_img <- image_crop(img, geometry = geometry_string)
  sub_img_gray <- image_convert(sub_img, colorspace = "gray")
  intensity_values <- as.numeric(image_data(sub_img_gray))
  mean_intensity <- mean(intensity_values)
  return(mean_intensity < threshold)
}

read_identification_number <- function(img, id_boxes_sorted, box_size = 30, threshold = 0.5) {
  n_boxes <- nrow(id_boxes_sorted)
  if (n_boxes != 50) {
    warning(glue::glue("⚠️ Erwartet: 50 ID-Boxen, gefunden: {n_boxes}"))
  }
  
  # Ergebnis-Vector initialisieren
  id_bits <- logical(n_boxes)
  
  for (i in seq_len(n_boxes)) {
    cx <- id_boxes_sorted[i, 1]
    cy <- id_boxes_sorted[i, 2]
    id_bits[i] <- check_if_filled(cx, cy, box_size, img, threshold)
  }
  
  # Index der angekreuzten IDs bestimmen:
  id_selected <- which(id_bits)
  
  # Warnen, wenn mehrere IDs markiert sind
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

read_answers <- function(img, df_points, box_size = 30, threshold = 0.5) {
  results <- data.frame(matrix(FALSE, nrow = 25, ncol = 4))
  colnames(results) <- c("A", "B", "C", "D")
  
  for (i in seq_len(nrow(df_points))) {
    cx <- df_points$V1[i]
    cy <- df_points$V2[i]
    
    # Frage und Option bestimmen:
    question_idx <- ceiling(i / 4)
    option_idx <- ((i - 1) %% 4) + 1
    
    # Checken, ob gefüllt
    is_filled <- check_if_filled(cx, cy, box_size, img, threshold)
    
    results[question_idx, option_idx] <- is_filled
  }
  
  return(results)
}


# CHOOSE PDF------------
getwd()
pdf_file <- "./ABGABEN/QM3_Antritt_1_11.6.25_Scan_1.pdf"
n_pages <- pdf_info(pdf_file)$pages

all_ids <- list() # Identifikationsnummern
all_answers <- list() # 25 x 4 Antworten per Identifikationsnummer

# ---- Loop über alle Seiten ----
for (page in 1:n_pages) {
  message(glue::glue("🔄 Verarbeite Seite {page}/{n_pages}..."))
  
  # 1️⃣ Seite einlesen
  img_filled <- image_read_pdf(pdf_file, density = 300, pages = page)
  
  img_info <- image_info(img_filled)
  img_height <- img_info$height
  
  # 2️⃣ Koordinaten anpassen
  box_positions_rotated <- box_positions
  box_positions_rotated[,1] <- img_info$width - box_positions[,1] + x_offset
  box_positions_rotated[,2] <- img_height - box_positions[,2] + y_offset
  
  # 3️⃣ Boxen splitten
  id_boxes <- box_positions_rotated[box_positions_rotated[,2] < 0.4 * img_height, ]
  answer_boxes <- box_positions_rotated[box_positions_rotated[,2] >= 0.4 * img_height, ]
  
  id_boxes_sorted <- id_boxes[order(id_boxes[,2], id_boxes[,1]), ]
  
  df_points <- as.data.frame(answer_boxes)
  colnames(df_points) <- c("V1", "V2")
  
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
length(id_numbers) # Anzahl Identifikationsnummern

# Antworten (erste Seite als Beispiel)
print(all_answers[[1]])




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

write_xlsx(df, "Namen_Punkte_11.6.25.xlsx")
