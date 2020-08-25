library(tidyverse)
import::from(DT, datatable, formatStyle)
import::from(readxl, read_xlsx)

data_root_paths <- c(
  # coder_name = data_root_path
  Chat = "../../04 Transcripts/Chat/",
  Nacho = "../../04 Transcripts/Nacho/"
)

# retrieve all data files

## TEMP variables for debugging
root_path <- data_roots[1]
coder_name <- names(data_roots)[1]
a_path <- "../../04 Transcripts/Chat//TM/TM.xlsx"
a_coder <- "C"

list.files(root_path, pattern = "^[[:upper:]][[:upper:]].xlsx", recursive = TRUE, full.names = TRUE) %>% 
  map_dfr(function(a_path) {
    a_coder <- "C"  ## TEMP
    excel_df <- read_xlsx(a_path) %>% 
      
      # add a coder column
      mutate(Coder = a_coder) %>% 
      
      # make code separation visible with bullet points
      mutate(Codes = if_else(is.na(Codes), "",
               paste0(
                 "<ul><li>",
                 str_replace_all(str_trim(Codes), "[\\n\\r]+", "</li><li>"),
                 "</li></ul>")))
    
  })

datatable(excel_df, 
          escape = FALSE,
          filter = "top",
          rownames = FALSE,
          # width = "1080px",
          # fillContainer = TRUE,
          autoHideNavigation = TRUE,
          selection = list(mode = "multiple"),
          extensions = "Responsive",
          options = list(
            pageLength = 100,
            autoWidth = TRUE,
            responsive = TRUE,
            columnDefs = list(
              list(width = '5%', targets = c(0)),
              list(width = '5%', targets = c(1)),
              list(width = '30%', targets = c(2)),
              list(width = '30%', targets = c(3))
              ))) %>% 
  formatStyle(
    c("Time", "Person", "Transcript", "Codes"),
    `vertical-align` = "top") 

# TODO: adjust column widths. Still doesn't work (https://rstudio.github.io/DT/options.html)
# TODO: row 82 timestamp format is weird
