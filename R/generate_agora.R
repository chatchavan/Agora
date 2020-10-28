# generate a spreadsheet to facilitate discussion of qualitative codes

library(tidyverse)

path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_coder1 <- "../../04 Transcripts/Chat/glass-codebook/codebook-CW.txt"
path_coder2 <- "../../04 Transcripts/Chat/glass-codebook/codebook-IA.txt"
coder1_name <- "Chat"
coder2_name <- "Nacho"
path_output <- "output/agora.csv"


# remove agreed codes from own file's code
agreed_codes <- read_lines(path_agreed)
coder1_codes <- 
  read_lines(path_coder1) %>% 
  setdiff(agreed_codes)
coder2_codes <- 
  read_lines(path_coder1) %>% 
  setdiff(agreed_codes)


bind_rows(
  tibble(code = coder1_codes, coder = coder1_name),
  tibble(code = coder2_codes, coder = coder2_name),
  tibble(code = agreed_codes, coder = "Agreed")) %>% 
  arrange(code) %>% 
  mutate(line_no = 1:n()) %>% 
  mutate(
    "{{coder1_name}}'s trash" := "",
    "{{coder1_name}}" := if_else(coder == coder1_name, code, ""),
    "agreed" = if_else(coder == "Agreed", code, ""),
    "{{coder2_name}}" := if_else(coder == coder2_name, code, ""),
    "{{coder2_name}}'s trash" := "") %>% 
  select(-code, -coder) %>% 
  write_excel_csv(path_output)

system2("open", paste0("\"", dirname(normalizePath(path_output)), "\""))
