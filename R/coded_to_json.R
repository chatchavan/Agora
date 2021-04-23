library(tidyverse)
import::from(jsonlite, toJSON)

source("R/km_excel_url.R")
source("R/excel_reader.R")

xlsx_df <- read_coded_excel()
text_only_df <- tidy_coded_excel(xlsx_df)

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
text_format_null_df <-
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



#-------------------------------------------------------------------------------
# if formatted… columns are null, fill in one entry with the content

if_null_fill_text <- function(col_to, col_from) {
  retval <-  col_to
  if(is_null(col_to) && str_length(col_from) > 0) {
    retval <- tibble(text = col_from)
  }
  retval
}

text_format_df <- 
  text_format_null_df %>% 
  mutate(
    `Formatted Transcript`        = map2(`Formatted Transcript`,        Transcript,          if_null_fill_text),
    `Formatted Codes`             = map2(`Formatted Codes`,             Codes,               if_null_fill_text),
    `Formatted Note (analytical)` = map2(`Formatted Note (analytical)`, `Note (analytical)`, if_null_fill_text)) %>% 
  
  
  # remove unused columns
  select(-Transcript, -Codes, -`Note (analytical)`)
  

#-------------------------------------------------------------------------------
# save to JSON
text_format_df %>% 
  toJSON(pretty = TRUE, null = "null") %>% 
  str_replace_all(fixed(": null"), fixed(": []")) %>% 
  write_file("output/transcript_codes.json")
  
  
