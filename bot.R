library("configr")
library("telegram.bot")
library("googlesheets4")
library("DBI")
library("readr")
library("RSQLite")
library("gmailr")
library("stringr")
# library("dplyr")
library("tidyverse")
library("tictoc")

options(encoding = 'WINDOWS-1252')

config <- read.config('config.ini', rcmd.parse = TRUE)

token = config$telegram_bot$bot_token
token_s = config$telegram_bot_status$bot_token

updater <- Updater(token)
bot <- Bot(token = token)
updates <- bot$getUpdates()

source('db_bot_function.R') # read db functions
source('message_filters.R') # message filters

# updater$bot$clean_updates()

start <- function(bot, updates) {
  options(encoding = 'WINDOWS-1252')
  
  updates <- bot$getUpdates()
  # tg_username <- updates[[length(updates)]]$message$from$username
  
  tg_username <- data.frame(
    paste0(
      '=HYPERLINK("https://t.me/',
      updates[[length(updates)]]$message$from$username,
      '","',
      updates[[length(updates)]]$message$from$username,
      '")'
    )
  )
  
  tg_username$paste0...HYPERLINK...https...t.me....updates..length.updates....message.from.username.. <-
    gs4_formula(
      tg_username$paste0...HYPERLINK...https...t.me....updates..length.updates....message.from.username..
    )
  
  data <-
    data.frame(Sys.Date(), tg_username, updates[[length(updates)]]$message$chat_id)
  
  who <-
    get_chat_data(updates[[length(updates)]]$message$chat_id, 'chat_id')
  
  if (length(who) == 0) {
    bot$sendPhoto(
      updates[[length(updates)]]$message$chat_id,
      photo = "rh.jpg",
      caption = paste0(
        "Я - бот, который помогает записаться на тренировку, продлить абонемент или оплатить хранение.
                             \nДавайте знакомиться, как Вас зовут?"
      )
    )
    
    
    set_state(chat_id = updates[[length(updates)]]$message$chat_id, state = 'start')
    set_chat_data(updates[[length(updates)]]$message$chat_id,
                  'tg_username',
                  updates[[length(updates)]]$message$from$username)
    
    
    set_state(chat_id = updates[[length(updates)]]$message$chat_id, state = 'wait_name')
    
  } else {
    uname <-
      data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'name'))
    
    IKM <- InlineKeyboardMarkup(inline_keyboard = list(
      list(
        InlineKeyboardButton("Записать на тренеровку", callback_data = "trenirovka")
      ),
      list(
        InlineKeyboardButton("Записать на пробное занятие с инструктором", callback_data = "probnaya")
      ),
      list(
        InlineKeyboardButton("Купить/продлить абонимент на тренировки", callback_data = "abonement")
      ),
      list(
        InlineKeyboardButton("Оплатить хранение инвентаря", callback_data = "inventar")
      )
    ))
    
    bot$sendMessage(
      updates[[length(updates)]]$message$chat_id,
      text = paste0("Рад снова видеть, ", uname, ".
                                \nВот что я могу:"),
      reply_markup = IKM
    )
    
    set_state(
      chat_id = updates[[length(updates)]]$from_chat_id(),
      state = paste0("wait_", updates[[length(updates)]]$callback_query$data)
    )
    
  }
  
  updater_s <- Updater(token_s)
  bot_s <- Bot(token = token_s)
  bot_s$sendMessage(
    chat_id = config$telegram_bot_groupid_status$id,
    text = paste0(
      '<b>Start new dialog from</b> ',
      updates[[length(updates)]]$message$from$username,
      ' in ',
      Sys.time()
    ),
    parse_mode = 'HTML'
  )
  
  # send user action to google sheet
  options(gargle_oauth_email = "s.motorniy.manager@gmail.com",
          gargle_oauth_cache = ".secret")
  gs4_auth_configure(path = "googlesheets4.json")
  gs4_auth(email = TRUE, cache = ".secret")
  key <- as_sheets_id(config$google_sheet$key)
  
  t1 <- read_sheet(key, sheet = "new_chat", range = "A:C")
  t1$row_num <- seq.int(nrow(t1))
  
  range_write(
    key,
    data = data,
    range = paste0("new_chat!A", (length(t1$row_num) + 2)),
    col_names = FALSE
  )
  
  
  
}

enter_name <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  uname <- updates[[length(updates)]]$message$text
  
  #username <<- uname
  set_chat_data(updates[[length(updates)]]$message$chat_id, 'name', uname)
  
  IKM <- InlineKeyboardMarkup(inline_keyboard = list(
    list(
      InlineKeyboardButton("Записать на тренеровку", callback_data = "trenirovka")
    ),
    list(
      InlineKeyboardButton("Записать на пробное занятие с инструктором", callback_data = "probnaya")
    ),
    list(
      InlineKeyboardButton("Купить/продлить абонимент на тренировки", callback_data = "abonement")
    ),
    list(
      InlineKeyboardButton("Оплатить хранение инвентаря", callback_data = "inventar")
    )
  ))
  
  bot$sendMessage(
    updates[[length(updates)]]$message$chat_id,
    text = paste0("Привет, ", uname, " приятно познакомится.
                                \nВот что я могу:"),
    reply_markup = IKM
  )
  
  set_state(
    chat_id = updates[[length(updates)]]$from_chat_id(),
    state = paste0("wait_", updates[[length(updates)]]$callback_query$data)
  )
  
}

tren <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  bot$sendChatAction(chat_id = updates[[length(updates)]]$from_chat_id(),
                     action = "typing")
  
  RKM <- ReplyKeyboardMarkup(
    keyboard = list(list(
      KeyboardButton("Групповая тренировка")
    ),
    list(
      KeyboardButton("Индивидуальная тренировка")
    )),
    resize_keyboard = TRUE,
    one_time_keyboard = TRUE
  )
  
  # отправляем клавиатуру
  bot$sendMessage(updates[[length(updates)]]$from_chat_id(),
                  text = 'Супер, давай выберем вид тренировки:',
                  reply_markup = RKM)
  
  
  bot$answerCallbackQuery(callback_query_id = updates[[length(updates)]]$callback_query$id)
  
  # set_chat_data(updates[[length(updates)]]$from_chat_id(), 'type_teren', "trenirovka")
  
  set_state(chat_id = updates[[length(updates)]]$from_chat_id(), state = 'wait_vid_tren')
  
  
}

vid_tren <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  vid_tren <- updates[[length(updates)]]$message$text
  
  set_chat_data(updates[[length(updates)]]$message$chat_id, 'type_teren', vid_tren)
  
  bot$sendChatAction(chat_id = updates[[length(updates)]]$message$chat_id,
                     action = "typing")
  
  options(gargle_oauth_email = "s.motorniy.manager@gmail.com",
          gargle_oauth_cache = ".secret")
  gs4_auth_configure(path = "googlesheets4.json")
  gs4_auth(email = TRUE, cache = ".secret")
  key <- as_sheets_id(config$google_sheet$key)
  
  date_tren <- read_sheet(key, sheet = "date", range = "A:F")
  date_tren$row_num <- seq.int(nrow(date_tren))
  date_tren <- date_tren[is.na(date_tren$status_zapisi),]
  
  keys <-
    lapply(1:nrow(date_tren), function(x)
      list(KeyboardButton(
        paste0(
          date_tren$date[x],
          " ",
          date_tren$day_week[x],
          " ",
          date_tren$time[x],
          " ",
          date_tren$trainer[x]
        )
      )))
  
  RKM <- ReplyKeyboardMarkup(
    keyboard = keys,
    resize_keyboard = F,
    one_time_keyboard = TRUE
  )
  
  bot$sendMessage(
    chat_id = updates[[length(updates)]]$from_chat_id(),
    text = paste0("Хороший выбор, давай выберем дату,время и тренера"),
    reply_markup = RKM
  )
  
  set_state(chat_id = updates[[length(updates)]]$from_chat_id(), state = 'wait_zapis')
  
}

try_tren <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  bot$answerCallbackQuery(callback_query_id = updates[[length(updates)]]$callback_query$id)
  
  bot$sendChatAction(chat_id = updates[[length(updates)]]$from_chat_id(),
                     action = "typing")
  
  options(gargle_oauth_email = "s.motorniy.manager@gmail.com",
          gargle_oauth_cache = ".secret")
  gs4_auth_configure(path = "googlesheets4.json")
  gs4_auth(email = TRUE, cache = ".secret")
  key <- as_sheets_id(config$google_sheet$key)
  
  date_tren <- read_sheet(key, sheet = "date", range = "A:F")
  date_tren$row_num <- seq.int(nrow(date_tren))
  date_tren <- date_tren[is.na(date_tren$status_zapisi),]
  
  keys <-
    lapply(1:nrow(date_tren), function(x)
      list(KeyboardButton(
        paste0(
          date_tren$date[x],
          " ",
          date_tren$day_week[x],
          " ",
          date_tren$time[x],
          " ",
          date_tren$trainer[x]
        )
      )))
  
  RKM <- ReplyKeyboardMarkup(
    keyboard = keys,
    resize_keyboard = F,
    one_time_keyboard = TRUE
  )
  
  bot$sendMessage(
    chat_id = updates[[length(updates)]]$from_chat_id(),
    text = paste0("Хороший выбор, давай выберем дату,время и тренера"),
    reply_markup = RKM
  )
  
  
  set_chat_data(updates[[length(updates)]]$from_chat_id(), 'type_teren', "Пробная")
  
  # set_state(chat_id = updates[[length(updates)]]$from_chat_id(), state = 'wait_trener')
  set_state(chat_id = updates[[length(updates)]]$from_chat_id(), state = 'wait_zapis')
  
}

zapis <- function(bot, updates) {
  options(encoding = 'WINDOWS-1252')
  
  updates <- bot$getUpdates()
  
  treiner <- updates[[length(updates)]]$message$text %>%
    str_split_fixed(" ", 4)
  treiner <- treiner[, 4]
  set_chat_data(updates[[length(updates)]]$message$chat_id, 'treiner', treiner)
  
  date_tren <- updates[[length(updates)]]$message$text %>%
    str_split_fixed(" ", 4)
  date_tren <-
    paste0(date_tren[, 1], " ", date_tren[, 2], " ", date_tren[, 3])
  set_chat_data(updates[[length(updates)]]$message$chat_id, 'date_tren', date_tren)
  
  bot$sendChatAction(chat_id = updates[[length(updates)]]$message$chat_id,
                     action = "typing")
  
  username <-
    data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'name'))
  type_teren <-
    data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'type_teren'))
  date_tren1 <-
    data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'date_tren'))
  treiner1 <-
    data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'treiner'))
  # tg_username  <- data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'tg_username'))
  tg_username <- data.frame(paste0(
    '=HYPERLINK("https://t.me/',
    data.frame(get_chat_data(
      updates[[length(updates)]]$message$chat_id, 'tg_username'
    )),
    '","',
    data.frame(get_chat_data(
      updates[[length(updates)]]$message$chat_id, 'tg_username'
    )),
    '")'
  ))
  tg_username$paste0...HYPERLINK...https...t.me....data.frame.get_chat_data.updates..length.updates....message.chat_id.. <-
    gs4_formula(
      tg_username$paste0...HYPERLINK...https...t.me....data.frame.get_chat_data.updates..length.updates....message.chat_id..
    )
  chat_id <-
    data.frame(get_chat_data(updates[[length(updates)]]$message$chat_id, 'chat_id'))
  
  
  options(gargle_oauth_email = "s.motorniy.manager@gmail.com",
          gargle_oauth_cache = ".secret")
  gs4_auth_configure(path = "googlesheets4.json")
  gs4_auth(email = TRUE, cache = ".secret")
  key <- as_sheets_id(config$google_sheet$key)
  
  zapis_data <- read_sheet(key, sheet = "date", range = "A:F")
  zapis_data$row_num <- seq.int(nrow(zapis_data))
  zapis_data <- zapis_data[is.na(zapis_data$status_zapisi),]
  
  t1 <- paste0(date_tren1, " ", treiner1)
  
  zapis_data <- zapis_data %>%
    mutate(filters = paste(date, day_week, time, trainer, sep = " ")) %>%
    subset(filters == t1)
  
  type_teren1 <- if (type_teren == "Пробная") {
    "пробную тренировку"
  } else if (type_teren == "Групповая тренировка") {
    "групповую тренировку"
  } else {
    "индивидуальную тренировку"
  }
  
  range_write(
    key,
    data = data.frame(paste0(
      "Бот записал ", username, " на ", type_teren1
    )),
    range = paste0("date!F", zapis_data$row_num + 1),
    col_names = FALSE
  )
  
  Sys.sleep(1)
  
  zapis_data_t <- read_sheet(key, sheet = "events", range = "A:F")
  zapis_data_t$row_num <- seq.int(nrow(zapis_data_t))
  range_write(
    key,
    data = username,
    range = paste0("events!A", (max(
      zapis_data_t$row_num
    ) + 2)),
    col_names = FALSE
  )
  range_write(
    key,
    data = type_teren,
    range = paste0("events!B", (max(
      zapis_data_t$row_num
    ) + 2)),
    col_names = FALSE
  )
  range_write(
    key,
    data = date_tren1,
    range = paste0("events!C", (max(
      zapis_data_t$row_num
    ) + 2)),
    col_names = FALSE
  )
  range_write(
    key,
    data = treiner1,
    range = paste0("events!D", (max(
      zapis_data_t$row_num
    ) + 2)),
    col_names = FALSE
  )
  range_write(
    key,
    data = tg_username,
    range = paste0("events!E", (max(
      zapis_data_t$row_num
    ) + 2)),
    reformat = F,
    col_names = FALSE
  )
  range_write(
    key,
    data = chat_id,
    range = paste0("events!F", (max(
      zapis_data_t$row_num
    ) + 2)),
    col_names = FALSE
  )
  
  bot$sendMessage(
    updates[[length(updates)]]$message$chat_id,
    text = paste0(
      "Отлично, запись на ",
      date_tren1,
      " подтверждена к ",
      treiner1,
      "\nЖдем вас в полночь на опушке леса!"
    )
  )
  
  set_state(chat_id = updates[[length(updates)]]$message$chat_id, state = 'start')
  
}

abonement <- function(bot, updates) {
  options(encoding = 'WINDOWS-1252')
  
  updates <- bot$getUpdates()
  
  bot$sendMessage(updates[[length(updates)]]$from_chat_id(),
                  text = paste0("Упсс, кажеться я еще это не умею
                             "))
  
  bot$answerCallbackQuery(callback_query_id = updates[[length(updates)]]$callback_query$id)
  
}

invent <- function(bot, updates) {
  options(encoding = 'WINDOWS-1252')
  
  updates <- bot$getUpdates()
  
  bot$sendMessage(
    updates[[length(updates)]]$from_chat_id(),
    text = paste0(
      "Упсс, кажеться я еще это не умею.
                                \nНо вы сможете хранить инвентарь дома или закопать в лесу.
                             "
    )
  )
  
  bot$answerCallbackQuery(callback_query_id = updates[[length(updates)]]$callback_query$id)
  
}

##### admin function #####
state <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  chat_state <-
    get_state(updates[[length(updates)]]$message$chat_id)
  
  # Send state
  bot$sendMessage(updates[[length(updates)]]$message$chat_id,
                  text = unlist(chat_state))
  
}

reset <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  set_state(chat_id = updates[[length(updates)]]$message$chat_id, state = 'start')
  
  chat_state <-
    get_state(updates[[length(updates)]]$message$chat_id)
  
  # Send state
  bot$sendMessage(updates[[length(updates)]]$message$chat_id,
                  text = unlist(chat_state))
  
}

stop <- function(bot, updates) {
  updater$stop_polling()
  
}

chat_data <- function(bot, updates) {
  updates <- bot$getUpdates()
  
  con <- dbConnect(SQLite(), config$db_settings$db_path)
  cd <-
    dbGetQuery(
      con,
      str_interp(
        "SELECT distinct (tg_username) FROM chat_data
                                 Where tg_username != 'NA';"
      )
    )
  dbDisconnect(con)
  
  source("tg_table.R")
  cd <- to_tg_table(cd)
  
  bot$sendMessage(updates[[length(updates)]]$message$chat_id,
                  text = cd,
                  parse_mode = "HTML")
  
}


##### run bot #####

start_h     <- CommandHandler('start', start)
reset_h     <- CommandHandler('reset', reset)
state_h     <- CommandHandler('state', state)
stop_h      <- CommandHandler("stop", stop)
chat_data_h <- CommandHandler("chat_data", chat_data)
wait_name_h <-
  MessageHandler(enter_name,
                 MessageFilters$wait_name & !MessageFilters$command)
wait_vid_tren_h <-
  MessageHandler(vid_tren,
                 MessageFilters$wait_vid_tren & !MessageFilters$command)
wait_zapis_h <-
  MessageHandler(zapis, MessageFilters$wait_zapis &
                   !MessageFilters$command)
qh_tren     <- CallbackQueryHandler(tren, pattern = "trenirovka")
qh_try_tren <- CallbackQueryHandler(try_tren, pattern = "probnaya")
qh_abonem <- CallbackQueryHandler(abonement, pattern = "abonement")
qh_invent <- CallbackQueryHandler(invent, pattern = "inventar")


updater <- updater +
  start_h +
  reset_h +
  state_h +
  stop_h +
  chat_data_h +
  wait_name_h +
  wait_vid_tren_h +
  wait_zapis_h +
  qh_tren +
  qh_try_tren +
  qh_abonem +
  qh_invent

tryCatch(
  expr = updater$start_polling(),
  
  error = function(err) {
    updater_s <- Updater(token_s)
    bot_s <- Bot(token = token_s)
    chat_id_s <- config$telegram_bot_groupid_status$id
    
    msg <- str_glue("*Bot is down*: Error (_{err$message}_).")
    
    bot_s$sendMessage(chat_id = chat_id_s,
                      text = msg,
                      parse_mode = 'Markdown')
    
    bot_s$sendMessage(
      chat_id = chat_id_s,
      text = paste0('*Restart bot* in ', Sys.time()),
      parse_mode = 'Markdown'
    )
    
    updater$bot$clean_updates()
    
  },
  
  finally = {
    updater$stop_polling()
    
    source("bot.R")
    
  }
  
)