library(readr)
library(dplyr)

phonology_structure <- data.frame(read_tsv("phonology_structure"))
rownames(phonology_structure) <- phonology_structure$...1
phonology_structure$...1 <- NULL

morphology_structure <- data.frame(read_tsv("morphology_structure"))
rownames(morphology_structure) <- morphology_structure$...1
morphology_structure$...1 <- NULL

syntax_structure <- data.frame(read_tsv("syntax_structure"))
rownames(syntax_structure) <- syntax_structure$...1
syntax_structure$...1 <- NULL

all <- bind_cols(phonology_structure, morphology_structure, syntax_structure)

all$glottocode <- rownames(all)

all_relocated <- all %>%
  relocate(glottocode)

rownames(all_relocated) <- NULL

write_tsv(all_relocated, "all_levels")
