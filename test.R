# updater_s <- Updater(token_s)
# bot_s <- Bot(token = token_s)
# bot_s$sendMessage(chat_id = config$telegram_bot_groupid_status$id,
#                   text = paste0('<b>Start new dialog from</b> ',updates[[length(updates)]]$message$from$username, ' in ',Sys.time()),
#                   parse_mode = 'HTML')


# send user action to google sheet
# options(
#   gargle_oauth_email = "s.motorniy.manager@gmail.com",
#   gargle_oauth_cache = ".secret"
# )
# gs4_auth_configure(path = "googlesheets4.json")
# gs4_auth(email = TRUE, cache = ".secret")
# key <- as_sheets_id(config$google_sheet$key)
# 
# t1 <- read_sheet(key, sheet = "new_chat", range = "A:C")
# t1$row_num <- seq.int(nrow(t1))
# 
# range_write(key, data = data, range = paste0("new_chat!A",(length(t1$row_num)+2)), col_names = FALSE)



con <- dbConnect(SQLite(), config$db_settings$db_path)

t1 <- dbGetQuery(con, str_interp("SELECT * FROM chat_state "))
t2 <- dbGetQuery(con, str_interp("SELECT * FROM chat_data "))
t3 <- dbGetQuery(con, str_interp("SELECT distinct (tg_username) FROM chat_data 
                                 Where tg_username != 'NA'"))

dbDisconnect(con)

