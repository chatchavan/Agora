library(tidyverse)
import::from(tidyxl, xlsx_cells)
import::from(fs, dir_ls)


# read Excel files
read_coded_excel <- function() {
  tibble(path = c(dir_ls("../../04 Transcripts/Chat/", recurse = TRUE, regexp = "/..\\.xlsx$"),
                  dir_ls("../../04 Transcripts/Nacho/", recurse = TRUE, regexp = "/..\\.xlsx$"))) %>% 
    mutate(
      coder = if_else(str_detect(path, "Chat"), "Chat", "Nacho"),
      cells = map(path, xlsx_cells)) %>% 
    unnest(cells) %>% 
    select(path, coder, participant_id = sheet, row, col, character_raw = character, character_formatted) %>% 
    
    # rewrite path to absolute
    mutate(path = str_replace_all(path, fixed("../../"), "~/Seafile/Project Glass/")) %>% 
    
    # remove header row and empty cells
    filter(character_raw != "", row > 1) %>% 
    
    # generate unique id to be used later
    unite(row_id, coder, participant_id, row)
  
}


# reshape data so that one timestamp is one row
tidy_coded_excel <- function(xlsx_df) {
  xlsx_df %>% 
    select(path, row_id, col, character_raw) %>%   
    
    # add excel column name to object name in JSON
    mutate(col = case_when(
      col == 1 ~ "Time",
      col == 2 ~ "Person",
      col == 3 ~ "Transcript",
      col == 4 ~ "Codes",
      col == 5 ~ "Codes (outside agreed codebook)",
      col == 6 ~ "Note (analytical)",
      col == 7 ~ "Note (transcript content)",
      TRUE ~ NA_character_)) %>% 
    
    # pivot to one row = one timestamp =  one data item
    pivot_wider(id_cols = c("path", "row_id"),
                names_from = "col", 
                values_from = "character_raw",
                values_fill = "") 
}
