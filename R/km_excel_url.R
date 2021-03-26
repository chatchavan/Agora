# generate keyboard maestro URL
import::from(jsonlite, toJSON)
km_excel_url__ <- function(path, person, cell) {
  trigger_value <- list(path = path,
       person =  person,
       cell = cell)
  the_json <- toJSON(trigger_value, auto_unbox = TRUE)
  paste0("kmtrigger://macro=OpenTranscriptInExcel&value=", URLencode(the_json, reserved = TRUE))
}

km_excel_url <- Vectorize(km_excel_url__)

#===============================================================================
# test
# km_excel_url("~/Seafile/Project Glass/04 Transcripts/Chat/AT/AT.xlsx", "AT", "C76")
#
# output:
#   "kmtrigger://macro=OpenTranscriptInExcel&value=%7B%22path%22%3A%22~%2FSeafile%2FProject%20Glass%2F04%20Transcripts%2FChat%2FAT%2FAT.xlsx%22%2C%22person%22%3A%22AT%22%2C%22cell%22%3A%22C76%22%7D"
