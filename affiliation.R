library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

url <- "http://newhavenhelpwithcovid.com/data/users.csv"
url <- url(url,"rb")
users <- read_csv(url)
close(url)

users <- users %>%
  mutate(affiliation_modified = case_when(
    str_detect(affiliation, "(?i)husband") ~ "Yale family",
    str_detect(affiliation, "(?i)spouse") ~ "Yale family",
    str_detect(affiliation, "(?i)1988") ~ "Yale Alumni",
    str_detect(affiliation, "(?i)biomedical engineering") ~ "Yale School of Biomedical Engineering", 
    str_detect(affiliation, "(?i)jackson") ~ "Yale Jackson Institute of Global Affairs",
    str_detect(affiliation, "(?i)yale law school") ~ "Yale Law School",
    str_detect(affiliation, "(?i)law school") ~ "Yale Law School",
    str_detect(affiliation, "(?i)yale lae school") ~ "Yale Law School",
    str_detect(affiliation, "(?i)YDS") ~ "Yale Divinity School",
    str_detect(affiliation, "(?i)Divinity") ~ "Yale Divinity School",
    str_detect(affiliation, "(?i)applied physics") ~ "Yale School of Engineering and Applied Science",
    str_detect(affiliation, "(?i)seas") ~ "Yale School of Engineering and Applied Science",
    str_detect(affiliation, "(?i)yale school of engineering") ~ "Yale School of Engineering and Applied Science",
    str_detect(affiliation, "(?i)SOM") ~ "Yale School of Management",
    str_detect(affiliation, "(?i)yale school of management") ~ "Yale School of Management",
    str_detect(affiliation, "(?i)GSAS") ~ "Yale Graduate School of Arts and Sciences",
    str_detect(affiliation, "(?i)graduate school of arts and sci") ~ "Yale Graduate School of Arts and Sciences",
    str_detect(affiliation, "(?i)graduate school of arts and sciences") ~ "Yale Graduate School of Arts and Sciences",
    str_detect(affiliation, "(?i)history") ~ "Yale Department of History",
    str_detect(affiliation, "(?i)earth and planetary") ~ "Yale Department of Earth and Planetary Sciences",
    str_detect(affiliation, "(?i)Ecology") ~ "Yale Department of Ecology and Evolutionary Biology",
    str_detect(affiliation, "(?i)philosophy") ~ "Yale Department of Philosophy",
    str_detect(affiliation, "(?i)nursing") ~ "Yale School of Nursing",
    str_detect(affiliation, "(?i)fes") ~ "Yale School of the Environment",
    str_detect(affiliation, "(?i)forestry") ~ "Yale School of the Environment",
    str_detect(affiliation, "(?i)MacMillan") ~ "Yale MacMillan Center",
    str_detect(affiliation, "(?i)yale school of architecture") ~ "Yale School of Architecture",
    str_detect(affiliation, "(?i)yale school of art") ~ "Yale School of Art",
    str_detect(affiliation, "(?i)yale school of public health") ~ "Yale School of Public Health",
    str_detect(affiliation, "(?i)press") ~ "Yale Press",
    str_detect(affiliation, "(?i)Developmental Biology") ~ "Yale School of Medicine",
    str_detect(affiliation, "(?i)genetics") ~ "Yale School of Medicine",
    str_detect(affiliation, "(?i)Anesthesiology") ~ "Yale School of Medicine",
    str_detect(affiliation, "(?i)neuroscience") ~ "Yale School of Medicine",
    str_detect(affiliation, "(?i)child study center") ~ "Yale School of Medicine",
    str_detect(affiliation, "(?i)medicine") ~ "Yale School of Medicine",
    str_detect(affiliation, "(?i)medical") ~ "Yale School of Medicine", 
    str_detect(affiliation, "(?i)office of development") ~ "Yale Office Of Development",
    str_detect(affiliation, "(?i)graduate school") ~ "Yale Graduate School",
    str_detect(affiliation, "(?i)yale College") ~ "Yale College",
    str_detect(affiliation, "(?i)alum") ~ "Yale Alumni",
    str_detect(affiliation, "(?i)Quinnipiac") ~ "Quinnipiac University",
    str_detect(affiliation, "(?i)n/a") ~ "none",
    str_detect(affiliation, "(?i)none") ~ "none",
    str_detect(affiliation, "(?i)yale") ~ "Yale unspecified",
    TRUE ~ affiliation))

affiliation_table <- users %>%
  group_by(affiliation_modified) %>%
  summarize(count = n())

#users %>%
#  filter(university == 'Yale unspecified')

write_csv(affiliation_table, "affiliation_all_users.csv")