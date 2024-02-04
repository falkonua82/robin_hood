scheduled_task <- function() {
  options(encoding = 'WINDOWS-1252')
  
  options(gargle_oauth_email = "s.motorniy.manager@gmail.com",
          gargle_oauth_cache = ".secret")
  
  gs4_auth_configure(path = "googlesheets4.json")
  gs4_auth(email = TRUE, cache = ".secret")
  key <- as_sheets_id(config$google_sheet$key)
  
  events <- read_sheet(key, sheet = "events", range = "A:G")
  colnames(events) <-
    c("client",
      "vid_tren",
      "date_time",
      "trener",
      "tg_login",
      "chat_id",
      "napomnil")
  events$row_num <- seq.int(nrow(events))
  events1 <-
    as.data.frame(str_split(events$date_time, " ", simplify = TRUE))
  events1 <-
    events1 %>% mutate(datetime = paste(events1$V1, events1$V3)) %>%
    select(4)
  
  events <- bind_cols(events, events1)
  rm(events1)
  
  day <- format(Sys.time(), "%Y-%m-%d %H:%M")
  
  events1 <-
    data.frame(difftime(events$datetime, day, units = "hours"))
  events1 <-
    data.frame(
      str_split(
        events1$difftime.events.datetime..day..units....hours..,
        " ",
        simplify = TRUE
      )
    )
  colnames(events1) <- "diff_h"
  events <- bind_cols(events, events1)
  rm(events1)
  events$diff_h <- as.numeric(events$diff_h)
  events <- events[is.na(events$napomnil),]
  
  if (length(events$client) > 1) {
    for (i in 1:length(events$diff_h)) {
      if (events$diff_h > 1 && events$diff_h < 5) {
        bot$sendMessage(
          events$chat_id,
          text = paste0(
            "Кажеться я должен тебе напомнить о тренеровке ",
            events$date_time,
            "\nЖдем вас вовремя на опушке леса!"
          )
        )
        
        range_write(
          key,
          data = data.frame(paste0("Napomnil ", Sys.time())),
          range = paste0("events!G", (max(
            events$row_num
          ) + 1)),
          col_names = FALSE
        )
        
      } else {
        bot$sendMessage(events$chat_id,
                        text = paste0("NO"))
      }
      
    }
  }
}

repeat {
  tic()
  scheduled_task()
  Sys.sleep(3600)  # every 60 min.
  toc()
}

