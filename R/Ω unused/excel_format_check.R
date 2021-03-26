# check formats that are used in the coding to narrow down formats that matters

library(tidyverse)
library(tidyxl)
library(fs)

paths_xlsx <-
  c(dir_ls("../../04 Transcripts/Chat/", recurse = TRUE, regexp = "/..\\.xlsx$"),
    dir_ls("../../04 Transcripts/Nacho/", recurse = TRUE, regexp = "/..\\.xlsx$"))

xlsx_df <- 
  tibble(path = paths_xlsx) %>% 
  mutate(
    coder = if_else(str_detect(path, "Chat"), "Chat", "Nacho"),
    cells = map(path, xlsx_cells),
    format = map(path, xlsx_formats))

#===============================================================================
# check the number of local format

xlsx_df %>%
  mutate(
    unique_local_format_id = map_int(cells, ~ length(unique(.x$local_format_id)))) %>%
  pull(unique_local_format_id) %>%
  hist()

# conclusion: small amount. ignoring them
#===============================================================================
# check the data type of the cells

xlsx_df %>%
  mutate(data_type_freq = map(cells, function(cells) {
      cells %>%
        group_by(data_type) %>%
        summarize(n = n())
    })) %>%
  unnest(data_type_freq) %>%
  pivot_wider(id_cols = path, names_from = data_type, values_from = n) %>%
  view()

# observation: only three data types: "blank", "character", and "date". Only two cells out of all files are "date"

xlsx_df %>%
  unnest(cells) %>%
  filter(data_type == "date")

# conclusion: manually fixed them to be a character type for a fuss-free processing

#===============================================================================
# narrow down the formats used

char_span_df <- 
  xlsx_df %>% 
  unnest(cells) %>% 
  select(path, coder, participant_id = sheet, row, col, character_raw = character, character_formatted) %>% 
  unnest(character_formatted)

char_span_df %>% 
  mutate(across(bold:family, ~as.character(.x))) %>% 
  pivot_longer(bold:family, names_to = "format_type", values_to = "format_value") %>% 
  filter(!is.na(format_value), 
         format_value != "FALSE",
         !is.element(format_type, c("size", "font", "family", "color_tint", "color_theme"))) %>% 
  group_by(format_type, format_value) %>% 
  summarize(n = n())
  


# conclusions: only formats that were used are "color_rgb" and "bold"
#===============================================================================
# check valid cell range
char_span_df %>% 
  pull(col) %>% unique()
