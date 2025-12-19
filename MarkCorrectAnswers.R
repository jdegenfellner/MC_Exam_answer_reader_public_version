# 2_MarkCorrectAnswers.R

library(pacman)
p_load(stringr, dplyr)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Prüfen, welche Datei gelesen wird
pruefung_content <- readLines("./1_Pruefung_without_line_breaks_between_answers.tex")

# Alle Zeilen mit \choice oder \CorrectChoice finden
choice_lines <- pruefung_content[str_detect(pruefung_content, 
                                            "\\\\choice|\\\\CorrectChoice")]
length(choice_lines) # 100 check

# Prüfen, ob die Anzahl ein Vielfaches von 4 ist
if (length(choice_lines) %% 4 != 0) {
  warning(glue::glue("⚠️ Achtung: Es wurden {length(choice_lines)} Antwortmöglichkeiten gefunden, das ist kein Vielfaches von 4!"))
}

# Markiere TRUE, wenn \CorrectChoice vorkommt, sonst FALSE
answer_flags <- str_detect(choice_lines, "\\\\CorrectChoice")

# In DataFrame mit 4 Spalten pro Frage umwandeln
correct_answers <- matrix(answer_flags, ncol = 4, byrow = TRUE) %>%
  as.data.frame()

colnames(correct_answers) <- c("first", "second", "third", "fourth")

# Ausgabe prüfen
print(correct_answers)

# Speichern
saveRDS(correct_answers, "correct_answers.RDS")


# Antwortblatt ausfüllen----
antwortblatt_content <- readLines("./4_Antwortblatt_fuer_autoread_NEW.tex")

# Finde alle Fragen-Zeilen (mit \item UND \answerbox)
answerbox_lines <- which(
  str_detect(antwortblatt_content, "\\\\item") & 
    str_detect(antwortblatt_content, "\\\\answerbox")
)
length(answerbox_lines) # 25 check

# Debug: Sicherstellen, dass 25 Fragen gefunden wurden
if (length(answerbox_lines) != 25) {
  warning(glue::glue("⚠️ Achtung: {length(answerbox_lines)} Fragen gefunden, erwartet: 25"))
}

# Hilfsfunktion zum gezielten Ersetzen der j-ten Antwortbox
# replace_nth <- function(string, pattern, replacement, n) {
#   match_positions <- gregexpr(pattern, string, fixed = TRUE)[[1]]
#   if (length(match_positions) < n || match_positions[n] == -1) return(string)
#   before <- substr(string, 1, match_positions[n] - 1)
#   after <- substr(string, match_positions[n] + nchar(pattern), nchar(string))
#   paste0(before, replacement, after)
# }

for (i in seq_along(answerbox_lines)) {
  current_line <- antwortblatt_content[answerbox_lines[i]]
  
  # Debug: Zeige die Zeile vor dem Ersetzen
  message(glue::glue("Vorher Frage {i}: {current_line}"))
  
  # Splitte nach '&', damit wir die 4 Blöcke haben
  parts <- strsplit(current_line, "&", fixed = TRUE)[[1]]
  
  # Für jede der 4 Positionen prüfen und ggf. ersetzen
  for (j in 1:4) {
    if (correct_answers[i, j]) {
      parts[j] <- sub("\\\\answerbox", "\\\\filledbox", parts[j])
      message(glue::glue("✅ Frage {i} Option {j}: Ersetzt"))
    }
  }
  
  # Baue die Zeile wieder zusammen
  new_line <- paste(parts, collapse = " & ")
  
  antwortblatt_content[answerbox_lines[i]] <- new_line
  
  # Debug: Zeige die Zeile nach dem Ersetzen
  message(glue::glue("Nachher Frage {i}: {new_line}"))
}

# Speichern
writeLines(antwortblatt_content, "./3_Antwortblatt_Loesungen.tex")
