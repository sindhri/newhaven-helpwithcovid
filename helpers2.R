

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

create_reports <- function (){
  
  url <- "http://newhavenhelpwithcovid.com/data/users.csv"
  url <- url(url,"rb")
  users <- read_csv(url)
  
  url <- "http://newhavenhelpwithcovid.com/data/projects.csv"
  url <- url(url,"rb")
  projects <- read_csv(url)
  
  url <- "http://newhavenhelpwithcovid.com/data/volunteers.csv"
  url <- url(url,"rb")
  volunteers <- read_csv(url)
  
  close(url)
  
  start_date <- as.Date('2020-03-30')
  end_date <- Sys.Date()
  
  users_day_table <- users %>%
    separate(created_at, c("date","time")," ") %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarize(users = n(), pair_with_projects = sum(pair_with_projects))
  #end_date <- last(users_day_table$date)
  projects$number_of_volunteers <- factor(projects$number_of_volunteers, levels = c("1-10","10-50","50-100","100+"))
  
  projects_day_table <- projects %>%
    separate(created_at, c("date","time")," ") %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarize(projects = n())
  
  volunteers <- volunteers %>%
    separate(created_at, c("date","time")," ")%>%
    mutate(date = as.Date(date)) 
  
  volunteers_day_table <- volunteers %>%
    group_by(date) %>%
    summarize(times_volunteered = n(), 
              users_volunteered = length(unique(user_id)), 
              projects_volunteered = length(unique(project_id)))
  
  summary_table <- tibble(date = seq.Date(from = start_date, to = end_date , by = 1))
  summary_table <- summary_table %>%
    left_join(users_day_table, by = "date") %>%
    left_join(projects_day_table, by = "date") %>%
    left_join(volunteers_day_table, by = "date")%>%
    replace(is.na(.), 0)
  
  
  summary_table$users_acc <- cumsum(summary_table$users)
  summary_table$pair_with_projects_acc <- cumsum(summary_table$pair_with_projects)
  summary_table$projects_acc <- cumsum(summary_table$projects)
  summary_table$times_volunteered_acc <- cumsum(summary_table$times_volunteered)
  
  summary_table$users_volunteered_acc <- rep(0,nrow(summary_table))
  summary_table$projects_volunteered_acc <- rep(0,nrow(summary_table))
  for (i in c(1:nrow(summary_table))) {
    summary_table$users_volunteered_acc[i] <- 
      as.double(summarize(filter(volunteers, 
                                 date <= summary_table$date[i]), users_volunteered_acc = length(unique(user_id))))
    summary_table$projects_volunteered_acc[i] <- 
      as.double(summarize(filter(volunteers, 
                                 date <= summary_table$date[i]), projects_volunteered_acc = length(unique(project_id))))
  }
  
  dates1 <- seq(start_date,to = end_date,by="months")
  end_of_month <- ceiling_date(dates1, "month") - days(1)
  end_of_month
  if (last(end_of_month) != end_date) {
    end_of_month[length(end_of_month)+1] <- end_date
  }
  
  monthly_report <- summary_table %>%
    filter(date %in% end_of_month) %>%
    select(date, users_acc, pair_with_projects_acc, projects_acc, 
           times_volunteered_acc, users_volunteered_acc, projects_volunteered_acc) %>%
    rename(users = users_acc, paired_up = pair_with_projects_acc, projects = projects_acc,
           times_volunteered = times_volunteered_acc, users_volunteered = users_volunteered_acc,
           projects_volunteered = projects_volunteered_acc) %>%
    mutate(date = format(date), users_volunteered = format(users_volunteered, nsmall = 0),
           projects_volunteered = format(projects_volunteered, nsmall = 0))
  
  projects <- rename(projects, project_id = id, `Volunteers Needed` = number_of_volunteers)
  helped <- projects %>%
    left_join(volunteers, by = "project_id") %>%
    mutate(a_help = ifelse(is.na(user_id.y),0,1)) %>%
    group_by(project_id) %>%
    summarize(total_volunteers = sum(a_help))
  projects_volunteers <- projects %>% left_join(as_tibble(helped), by = "project_id")
  
  returnlist <- list(summary_table, monthly_report, projects_volunteers)
}
