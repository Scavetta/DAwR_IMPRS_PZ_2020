# SILAC Analysis
# Protein profiles during myocardial cell differentiation

# Load packages ----
library(tidyverse)
library(glue)

# Part 0: Import data ----
# protein_df <- read.delim("data/Protein.txt")
protein_df <- read_tsv("data/Protein.txt")

# Examine the data:
class(protein_df)
summary(protein_df)
str(protein_df)
glimpse(protein_df)
# protein_df %>% 
#   group_by(Peptides)

# Quantify the contaminants ----
protein_df %>% 
  as_tibble() %>% 
  filter(Contaminant == "+") %>% 
  nrow() / nrow(protein_df) * 100

tot_con <- sum(protein_df$Contaminant == "+", na.rm = TRUE)

# Proportion of contaminants
tot_con/nrow(protein_df)

# Percentage of contaminants (just multiply proportion by 100)
tot_con/nrow(protein_df) * 100

# Transformations & cleaning data ----

# Remove contaminants ====
# If your non-contaminants are ""
# protein_df <- read.delim("data/Protein.txt")
# protein_df <- protein_df[protein_df$Contaminant != "+",]
# protein_df <- protein_df %>% 
#   filter(Contaminant != "+")

# If you have a bunch of NAs instead of "" then you 
# need to keep the NAs in contaminant
protein_df <- read_tsv("data/Protein.txt")
protein_df <- protein_df[is.na(protein_df$Contaminant),]

# log 10 transformations of the intensities ====

protein_df$Intensity.L <- log10(protein_df$Intensity.L)
protein_df$Intensity.M <- log10(protein_df$Intensity.M)
protein_df$Intensity.H <- log10(protein_df$Intensity.H)

# Add the intensities ====
protein_df$Intensity.H.M <- protein_df$Intensity.H + protein_df$Intensity.M
protein_df$Intensity.M.L <- protein_df$Intensity.M + protein_df$Intensity.L

# The tidyverse way (one example):
# protein_df <- protein_df %>%
#   mutate(Intensity.L = log10(Intensity.L),
#          Intensity.M = log10(Intensity.M),
#          Intensity.H = log10(Intensity.H),
#          Intensity.H.M = Intensity.H + Intensity.M)


# log2 transformations of the ratios ====
protein_df$Ratio.M.L <- log2(protein_df$Ratio.M.L)
protein_df$Ratio.H.L<- log2(protein_df$Ratio.H.L)
protein_df$Ratio.H.M <- log2(protein_df$Ratio.H.M)


# Part 2: Query data using filter() ----
# Exercise 9.3 (Find protein values) ====
protein_df %>% 
  filter(Uniprot  %in%  c("GOGA7", "PSA6", "S10AB")      )

# Solutions:
# 1 - Add _MOUSE to query
protein_df %>% 
  filter(Uniprot  %in%  paste0(c("GOGA7", "PSA6", "S10AB"),"_MOUSE"))

# 2 - Remove _MOUSE from the search space
protein_df %>% 
  mutate(Uniprot_clean = str_remove_all(Uniprot, "_MOUSE")) %>% 
  filter(Uniprot_clean  %in%  c("GOGA7", "PSA6", "S10AB"))

# 3 - Fuzzy matching with "Regular Expressions"
protein_df %>% 
  filter(str_detect(Uniprot, c("GOGA7|PSA6|S10AB")))

# The literal positions:
grep(c("GOGA7|PSA6|S10AB"), protein_df$Uniprot)

# A logical vector:
grepl(c("GOGA7|PSA6|S10AB"), protein_df$Uniprot)
str_detect(protein_df$Uniprot, c("GOGA7|PSA6|S10AB"))

# Exercise 9.3 (Find significant hits) and 10.2 ====
# For the H/M ratio column, create a new data 
# frame containing only proteins that have 
# a p-value less than 0.05


# Exercise 10.4 (Find top 20 values) ==== 
# Which proteins (i.e. Uniprot IDs) have the 20 highest  log2 H/M and M/L ratios?
protein_df %>% 
  arrange(-Ratio.H.M) %>% 
  slice(1:20) -> top20_arrange
top20_arrange

protein_df %>% 
  slice_max(Ratio.H.M, n = 20) -> top20_slice
top20_slice


# Exercise 10.5 (Find intersections) ====
intersect(top20_arrange, top20_slice)
