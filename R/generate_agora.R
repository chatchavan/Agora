# generate a spreadsheet to facilitate discussion of qualitative codes

library(tidyverse)

path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_coder1 <- "../../04 Transcripts/Chat/glass-codebook/codebook-CW.txt"
path_coder2 <- "../../04 Transcripts/Chat/glass-codebook/codebook-IA.txt"
coder1_name <- "Chat"
coder2_name <- "Nacho"

read_codes <- function(a_path, coder_name) {
 tibble(code = read_lines(a_path)) %>% 
    mutate(
      coder = coder_name)
}

bind_rows(
  read_codes(path_coder1, coder1_name),
  read_codes(path_coder2, coder2_name),
  read_codes(path_agreed, "Agreed")) %>% 
  arrange(code) %>% 
  mutate(line_no = 1:n()) %>% 
  mutate(
    "{{coder1_name}}'s trash" := "",
    "{{coder1_name}}" := if_else(coder == coder1_name, code, ""),
    "agreed" = if_else(coder == "Agreed", code, ""),
    "{{coder2_name}}" := if_else(coder == coder2_name, code, ""),
    "{{coder2_name}}'s trash" := "") %>% 
  select(-code, -coder) %>% 
  write_excel_csv("output/agora.csv")

