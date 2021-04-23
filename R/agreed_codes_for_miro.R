library(tidyverse)

source("R/excel_reader.R")

path_codes_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_codes_miro <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed-shuffled-first100.txt"
path_used <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"

path_batch_output <- "output/code_batches.csv"

set.seed(20)


# open a file in Apple Numbers
open_numbers <- function(file_path) {
  system2("open", c("-a", "Numbers", paste0("\"", normalizePath(file_path), "\"")))
}

#===============================================================================
# entry point

# Excel processing
#    1. extract rows in Excel with analytical notes
df_xlsx_filtered <- 
  read_coded_excel() %>% 
  tidy_coded_excel() %>% 
  filter(`Note (analytical)` != "")

#    2. extract codes
codes_with_notes <- 
  df_xlsx_filtered %>% 
  select(Codes) %>% 
  mutate(codes_splitted = str_split(Codes, "\\r\\n")) %>% 
  select(codes_splitted) %>% 
  unnest(codes_splitted) %>% 
  mutate(codes_splitted = str_trim(codes_splitted)) %>% 
  filter(str_length(codes_splitted) > 0) %>% 
  distinct() %>% 
  pull(codes_splitted)

# read codes 
agreed_codes <- read_lines(path_codes_agreed)
miro_codes  <- read_lines(path_codes_miro)

# prepare batch ids
n_remaining <- length(agreed_codes) - length(miro_codes)
n_batch2 <- floor(n_remaining / 2)
n_batch3 <- n_remaining - n_batch2
batch_ids <- c(rep(2, times = n_batch2), rep(3, times = n_batch3))

# check codes with analytical notes and add annotations
df_codes <- 
  tibble(code = agreed_codes) %>% 
  mutate(on_miro = if_else(code %in% miro_codes, TRUE, FALSE)) %>% 
  mutate(has_note = if_else(code %in% codes_with_notes, TRUE, FALSE)) %>% 
  mutate(code = if_else(has_note == TRUE, str_c(code, " ***"), code)) %>% 
  mutate(batch = if_else(on_miro == TRUE, 1, 0)) %>% 
  
  # create id 
  arrange(code) %>% 
  mutate(code_id = 1:n()) %>% 
  
  # shuffle all codes
  sample_frac()

# assign codes into batches
df_id_batch_2_3 <-   
  df_codes %>% 
  filter(batch != 1) %>% 
  select(code_id) %>% 
  mutate(batch_new = batch_ids)

df_codes_batch <- 
  df_codes %>% 
  left_join(df_id_batch_2_3, by = "code_id") %>% 
  mutate(batch = if_else(batch == 0, batch_new, batch)) %>% 
  select(-batch_new) %>% 
  arrange(batch, code_id)
rm(df_codes)

# save batch
write_csv(df_codes_batch, path_batch_output)

open_numbers(path_batch_output)
