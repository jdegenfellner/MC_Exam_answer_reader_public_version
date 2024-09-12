# Send_grades.R

library(pacman)
p_load(tidyverse,readxl, sendmailR, writexl, 
       data.table, curl, Microsoft365R, genderizeR)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Reshape and merge
df <- read_excel("./1_Noten_calculated_QM2_2.5.24.xlsx") # no emails
df <- df %>% mutate(Vorname_Nachname = paste0(Vorname, " ", Nachname))
df <- left_join(df, df1, by = "Vorname_Nachname") %>% 
  dplyr::select(-c(Vorname_Nachname,Vorname.y, Nachname.y, Institution, Gruppen, "...6", "...7")) %>%
  rename(Vorname = Vorname.x) %>%
  rename(Nachname = Nachname.x) %>%
  rename(Email = `E-Mail-Adresse`) %>%
  as.data.table()

#my_outlook <- get_business_outlook(app = "XXXXXXXXXXXXXXX")
 # Login online before with:
# email
# password
my_outlook <- get_business_outlook()

non_zero_points <- df[df$Punkte > 0, ]

get_first_name <- function(name) {
  split_names <- strsplit(name, " ")[[1]]
  return(split_names[1])
}

non_zero_points$Vorname_lower_case <- tolower(non_zero_points$Vorname) # lower case
non_zero_points$Vorname_only_first <- unlist(lapply(non_zero_points$Vorname_lower_case, get_first_name))

#df_names_gender <- findGivenNames(non_zero_points$Vorname_only_first)
#df_names_gender <- unique(df_names_gender) # caution: less names now
 
#non_zero_points$guessed_gender <- rep("female", length(non_zero_points$Vorname_only_first))

# ugly look-up (improve later):
#for( i in 1:length(non_zero_points$Vorname_only_first) ){
#  ind_name <- min(which(df_names_gender$gender == non_zero_points$Vorname_only_first[i]))
  #non_zero_points$guessed_gender[i] <- df_names_gender$probability[ind_name]
#}

# Check genders:
#non_zero_points %>% dplyr::select(Vorname, Nachname, guessed_gender)

messages <- vector("list", nrow(non_zero_points))
for (i in 1:nrow(non_zero_points)) {
  # Wähle die Anrede basierend auf der Spalte guessed_gender
  #anrede <- ifelse(non_zero_points[i,]$guessed_gender == 'male', 'Lieber', 'Liebe')
  anrede <- "Hallo"
  
  # Erstelle die Nachricht
  messages[[i]] <- sprintf('From: "Jürgen Degenfellner (ZHAW)" <XXXXXXX@outlook.com>
To: "%s %s" <%s>
Subject: Vorläufige Noten Quantitative Methoden 2

%s %s,

Du hast %s Punkte von 100 erreicht und somit die Note %s.

Beste Grüße
Jürgen
                           
[Dies ist eine automatisch generierte Email und kann fehlerbehaftet sein]',
non_zero_points[i,]$Vorname,
non_zero_points[i,]$Nachname,
non_zero_points[i,]$Email,
anrede,
non_zero_points[i,]$Vorname,
non_zero_points[i,]$Punkte,
non_zero_points[i,]$Note)
}  

messages

if(floor(dim(non_zero_points)[1]) > 15){
  # send Package 1 of 2:----
  ind_1 <- floor(dim(non_zero_points)[1]/2) # 21
  for( i in 1:ind_1 ){
    curl::send_mail(mail_from = "XXXXXXXX@outlook.com", 
                    mail_rcpt = non_zero_points[i,]$Email, # Student-EMail-Adresses
                    #mail_rcpt = "XXXXXXXX@gmail.com", # Testing
                    message = messages[[i]], 
                    smtp_server = "smtp-mail.outlook.com:587",
                    username = "XXXXXX@outlook.com", 
                    password  = "XXXXXXXX")
    Sys.sleep(runif(1, min = 8, max = 15))
    if(i == ind_1){
      end_first_package <- Sys.time()
      saveRDS(end_first_package, "end_first_package.RDS")
    }
  }
  
  ind_1 # 21
  
  # Check time difference for sending----
  end_first_package_time <- as.POSIXct(readRDS("end_first_package.RDS"), 
                                       format="%Y-%m-%d %H:%M:%S", 
                                       tz="CET")
  current_time <- Sys.time()
  time_difference <- difftime(current_time, end_first_package_time, units = "mins")
  time_difference
  
  if(time_difference >= 90){ # at least 90 min later.
    # send Package 2 of 2----
    for( i in (ind_1+1):dim(non_zero_points)[1] ){
      curl::send_mail(mail_from = "XXXXXXXX@outlook.com", 
                      mail_rcpt = non_zero_points[i,]$Email, # Student-EMail-Adresses
                      #mail_rcpt = "XXXXXXXX@gmail.com", # Testing
                      message = messages[[i]], 
                      smtp_server = "smtp-mail.outlook.com:587",
                      username = "XXXXXXX@outlook.com", 
                      password  = "XXXXXXX")
      Sys.sleep(runif(1, min = 8, max = 15))
    }
    
  }
  
} else {
  for( i in 1:dim(non_zero_points)[1] ){
    curl::send_mail(mail_from = "XXXXXX@outlook.com", 
                    #mail_rcpt = non_zero_points[i,]$Email, # Student-E-Mail-Adresses
                    mail_rcpt = "XXXXX@gmail.com", # Testing
                    message = messages[[i]], 
                    smtp_server = "smtp-mail.outlook.com:587",
                    username = "XXXXX@outlook.com", 
                    password  = "XXXXXX")
    Sys.sleep(runif(1, min = 8, max = 15))
  }
}







