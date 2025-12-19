# 2_MarkCorrectAnswers.R

library(pacman)
p_load(stringr)

# TODO robustify to capture answers with line breaks and images within answer options

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Pruefungsdatei----
pruefung_content <- readLines("./1_Pruefung_without_images_for_MarkCorrect_Answers.tex") # without images and line-breaks!

correct_answers <- data.frame(first = logical(), 
                              second = logical(), 
                              third = logical(), 
                              fourth = logical())
for (i in seq_along(pruefung_content)) {
  if (str_detect(pruefung_content[i], "choices") & 
      str_detect(pruefung_content[i], "begin") & 
      str_detect(pruefung_content[i+5], "choices") & 
      str_detect(pruefung_content[i+5], "end")) {
    #print(paste("i=",i,pruefung_content[i+1]))
    correct_answers_for_current_question <- c(FALSE, FALSE, FALSE, FALSE)
    print(paste0("Zeile: ",i))
    for(j in 1:4){
      if(str_detect(pruefung_content[i+j], "CorrectChoice")){
        correct_answers_for_current_question[j] <- TRUE
      }
    }
    correct_answers <- rbind(correct_answers, correct_answers_for_current_question)
  }
}
colnames(correct_answers) <- c("first", "second", "third", "fourth")
correct_answers
saveRDS(correct_answers, "correct_answers.RDS")

# Antwortblatt----
antwortblatt_content <- readLines("./4_Antwortblatt_fuer_autoread.tex")

# Suche "item"
items <- c()
for(i in seq_along(antwortblatt_content)){
  if(str_detect(antwortblatt_content[i], "item") &
     str_detect(antwortblatt_content[i], "usepackage") == FALSE &
     str_detect(antwortblatt_content[i], "enumerate") == FALSE){
    items <- append(items, i)
  }
}

# Fragepositionen
answerboxes <- items + 2

replace_nth <- function(string, pattern, replacement, n) {
  match_positions <- gregexpr(pattern, string, fixed = TRUE)[[1]]
  if (length(match_positions) < n || match_positions[n] == -1) return(string)
  before <- substr(string, 1, match_positions[n] - 1)
  after <- substr(string, match_positions[n] + nchar(pattern), nchar(string))
  paste0(before, replacement, after)
}

for (i in seq_along(answerboxes)) {
  current_line <- antwortblatt_content[answerboxes[i]]
  answerbox_count <- 1  # Initialisiere den Zähler für die Position der Antwortboxen
  
  for (j in 1:ncol(correct_answers)) {
    if (correct_answers[i, j]) {
      # Ersetze die aktuelle answerbox
      current_line <- replace_nth(current_line, "\\answerbox", "\\filledbox", answerbox_count)
    } else {
      # Erhöhe den Zähler, wenn die aktuelle answerbox nicht ersetzt wird
      answerbox_count <- answerbox_count + 1
    }
  }
  
  antwortblatt_content[answerboxes[i]] <- current_line
}

# Schreibe das aktualisierte Dokument zurück
writeLines(antwortblatt_content, "./3_Antwortblatt_Loesungen.tex")
