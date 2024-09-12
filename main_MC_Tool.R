# MC Tool
# Juergen Degenfellner
# Dec. 2023, ZHAW

# TODO ----
# - Clean up paths and folders, 
# - Improve reading correct answers: now, images and line breaks are not handled and leads to errors
# - Work on a question pool and create new exams semi-automatic:
#   -> Questions are read randomly from the pool, answer options are randomly twisted

# How to use this tool----
# 1) In the same folder there MUST be the following files:
#   - 1_Pruefung.tex + images folder (images in the exam)
#     (the exam file in latex, no line breaks in the answer options)
#   - 4_Antwortblatt_fuer_autoread.tex (DO NOT change this file, except for the title)
#   - main_MC_Tool.R
#   - MarkCorrectAnswers.R
#   - Read_Points.R
#   - Send_grades.R
#   - Folder "ABGABEN" containing the scanned answer sheets in one pdf.

# 2) USE:
#    Step 1: Scan the answer sheets in any order and save the pdf into the folder "ABGABEN".
#    Step 2: Adapt the file name to fit the filename in Read_Points.R ca. line 17.
#    Step 3: Adapt the file path to Identifikationsnummern.xlsx in Read_Points.R ca. line 156.
#    Step 4: Execute the code below here.
#    Step 5: The results should be in the file Namen_Punkte.xlsx.
#    Step 6: Go to Send_grades.R, adapt the details and send grades/points to students.

# If you want a template file with the correct solutions on the answer sheet:
# - Open MarkCorrectAnswers.R.
# - Check path at "antwortblatt_content".
# - Execute File.
# - Find the answer template in "3_Antwortblatt_Loesungen.pdf".

library(pacman)
p_load(tidyverse, pdftools, magick, EBImage, readxl, writexl)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./MarkCorrectAnswers.R") 
source("./Read_Points.R")
source("./Notenberechnung.R")
source("./Send_grades.R")
