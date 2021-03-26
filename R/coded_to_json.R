library(tidyverse)
import::from(tidyxl, xlsx_cells)
import::from(fs, dir_ls)
import::from(jsonlite, toJSON)

source("R/km_excel_url.R")

# read Excel files and extract bold and RGB formats 
xlsx_df <- 
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

#-------------------------------------------------------------------------------
# reshape data so that one timestamp is one row

text_only_df <- 
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

#-------------------------------------------------------------------------------
# process format

formats_df <-
  xlsx_df %>% 
  select(row_id, col, character_formatted) %>% 
  
  # format doesn't matter for the `Timestamp` and `Person` columns 
  filter(!is.element(col, c(1, 2))) %>% 
  
  # the cell is not formatted when there is only one row in the format data
  mutate(format_count = map_int(character_formatted, ~nrow(.x))) %>% 
  filter(format_count > 1) %>% 
  select(-format_count) %>% 
  
  # process formats
  mutate(character_formatted = map(character_formatted, function(format_df) {
    
    format_df %>% 
      # keep only formats that we are using (see also: `Ω unused/excel_format_check.R` for reasons to exclude most of the formats)
      select(text = character, bold, color = color_rgb) %>% 
      
      # remove empty rows (happens when we use newline to break codes)
      filter(str_length(text) != 0) %>% 
      
      # remove default formats (black text, not bold)
      mutate(is_default_format = (bold == FALSE & color == "FF000000")) %>% 
      
      mutate(
        bold = if_else(is_default_format, NA, bold),
        color = if_else(is_default_format, NA_character_, color)) %>% 
      
      select(-is_default_format) %>% 
      
      # remove alpha channel from the color code
      mutate(color = str_c("#", str_sub(color, 3), sep = ""))
    })) %>% 
  
  # add excel column name to object name in JSON
  mutate(col = case_when(
    col == 3 ~ "Formatted Transcript",
    col == 4 ~ "Formatted Codes",
    col == 5 ~ "Formatted Codes (outside agreed codebook)",
    col == 6 ~ "Formatted Note (analytical)",
    col == 7 ~ "Formatted Note (transcript content)",
    TRUE ~ NA_character_)) %>%

  # pivot to one row = one timestamp =  one data item
  pivot_wider(id_cols = c("row_id"),
              names_from = "col",
              values_from = "character_formatted")

#-------------------------------------------------------------------------------
# merge the formats with the text data
text_format_df <- 
  text_only_df %>% 
  left_join(formats_df, by = "row_id") %>% 

  # extract coder
  separate(row_id, c("Coder", "Participant ID", "row")) %>% 
  
  # order by participant ID
  arrange(`Participant ID`, Coder, Time) %>% 
  
  # generate URL to trigger Excel
  mutate(ExcelURL = km_excel_url(path, `Participant ID`, str_c("B", row, sep = ""))) %>% 
  
  # select relevant columns
  select(-path, -row)


text_format_df %>% 
  toJSON(pretty = TRUE, null = "null") %>% 
  str_replace_all(fixed(": null"), fixed(": []")) %>% 
  write_file("output/transcript_codes.json")
  
  
