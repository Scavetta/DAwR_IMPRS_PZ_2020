# Element 7: Tidyverse -- tidyr ----

# Load packages ----
# This should already be loaded if you executed the commands in the previous file.
library(tidyverse)

# Get a play data set:
PlayData <- read_tsv("data/PlayData.txt")

# Let's see the scenarios we discussed in class:
# Scenario 1: Transformation height & width
PlayData$height/PlayData$width
# mean(c(PlayData$height[1], PlayData$width[1])) # not nice :/

# For the other scenarios, tidy data would be a 
# better starting point:
# we'll use gather()
# 4 arguments
# 1 - data
# 2,3 - key,value pair - i.e. name for OUTPUT
# 4 - the ID or the MEASURE variables

# using ID variables ("exclude" using -)
# gather(PlayData, "key", "value", -c("type", "time"))

PlayData %>% 
  pivot_longer(-c("type", "time"), 
               names_to = "key", 
               values_to = "value")

# using MEASURE variables
# gather(PlayData, "key", "value", c("height", "width"))

PlayData_t <- PlayData %>%
  pivot_longer(c("height", "width"), 
               names_to = "key", 
               values_to = "value")

# Scenario 2: Transformation across time 1 & 2
# difference from time 1 to time 2 for each type and key
# we only want one value as output
# Just to view what's happening:
PlayData_t %>% 
  group_by(type, key) %>% 
  group_split()

PlayData_t %>% 
  group_by(type, key) %>% 
  summarise(time_diff = value[time == 1] / value[time == 2])

# PlayData_t$value[PlayData_t$time == 1 &
#                    PlayData_t$type == "A" &
#                    PlayData_t$key == "height"]
# 
# PlayData_t %>% 
#   group_by(type, key) %>% 
#   summarise(output = value[time == 2] )
# Another example: standardize to time 1
# Scenario 3: Transformation across type A & B
# A/B for each time and key

# just to view
PlayData_t %>% 
  group_by(time, key) %>% 
  group_split()


PlayData_t %>% 
  group_by(time, key) %>% 
  summarise(type_ratio = value[type == "A"] / value[type == "B"])

# A/B, the results should be:
# for height 1 : 10/30 = 0.33
# for height 2 : 20/40 = 0.50
# for width  1 : 50/70 = 0.714
# for width  2 : 60/80 = 0.75



