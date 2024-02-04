library("configr")
library("googlesheets4")

config <- read.config('config.ini', rcmd.parse = TRUE)

options(
  gargle_oauth_email = "s.motorniy.manager@gmail.com",
  gargle_oauth_cache = ".secret"
)
gs4_auth_configure(path = "googlesheets4.json")
gs4_auth(email = TRUE, cache = ".secret")
key <- as_sheets_id(config$google_sheet$key)