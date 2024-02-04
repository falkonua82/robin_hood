# ###########################################################
# message state filters

MessageFilters$wait_name <- BaseFilter(function(message) {
  get_state( message$chat_id )  == "wait_name"
}
)

MessageFilters$wait_trener <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_trener"
}
)

MessageFilters$wait_zapis <- BaseFilter(function(message) {
  get_state( message$chat_id )  == "wait_zapis"
}
)

MessageFilters$wait_vid_tren <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_vid_tren"
}
)

MessageFilters$wait_days <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_days"
}
)

MessageFilters$wait_day_from <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_day_from"
}
)

MessageFilters$wait_day_to <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_day_to"
}
)

MessageFilters$wait_type <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_type"
}
)

MessageFilters$wait_reason <- BaseFilter(function(message) {
  get_state( message$chat_id )   == "wait_reason"
}
)
