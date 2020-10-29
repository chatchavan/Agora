# after discussion, process agora CSV and update the code book
library(tidyverse)
library(readxl)

# path for the output files
path_agreed <- "../../04 Transcripts/Chat/glass-codebook/codebook-agreed.txt"
path_coder1 <- "../../04 Transcripts/Chat/glass-codebook/codebook-CW.txt"
path_coder2 <- "../../04 Transcripts/Chat/glass-codebook/codebook-IA.txt"
coder1_name <- "Chat"
coder2_name <- "Nacho"



# ask for the agora file
agora_path <- file.choose()

# extract columns and sort
agora_df <- read_excel(agora_path)

# save the code files
extract_and_save <- function(column_name, output_path) {
  agora_df %>% 
    filter(!is.na(.data[[column_name]])) %>% 
    arrange(.data[[column_name]]) %>% 
    pull(.data[[column_name]]) %>% 
    write_lines(output_path)
}
extract_and_save(coder1_name, path_coder1)
extract_and_save(coder2_name, path_coder2)
extract_and_save("agreed", path_agreed)


# show the code files
system2("open", paste0("\"", dirname(normalizePath(path_agreed)), "\""))
