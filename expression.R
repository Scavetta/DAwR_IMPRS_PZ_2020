# A further example with the meditation dat set:

medi <- read_tsv("data/Expression.txt")

medi %>% 
  pivot_longer(everything(),
               names_to = c("treatment", "gene", "time"),
               names_sep = "_")
