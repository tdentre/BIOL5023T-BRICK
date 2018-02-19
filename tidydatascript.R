### Investigating factors affecting nest selection for different bird species
### Q1: Is there any evidence that sea birds are choosing locations to nest based on rock size? Is there differences between species?
### Q2: Is there any evidence that sea bird nesting location is affected by substrate type in a given area? Is there differences between species?

# STEP 1: Replace "NA" with "0"
# STEP 2: For coloumns 7-16, "0" should be replaced with "NA"
# Step 3: Remove 'home' row
# STEP 4: Edit 'species' coloumn to have no numbers
# STEP 5: The two different data sets need to be seperated into two different dataframes
# STEP 6: Rename columns X1-X10, 1-10 in rocksize.df
# STEP 7: In rocksize.df, create seperate columns for 'rock.num' and 'rock.circumference'
# STEP 8: Create boxplot of rocksize in relation to species
# STEP 9: In percentcoverage.df, create seperate columns for 'ground.type' and 'amount.of.cover'
# STEP 10: Create graph of percent coverage in relation to species

library(tidyverse)
original.df <- read_csv("./originalbirddata.csv")

# STEP 1: Replace "NA" with "0"
original.df[is.na(original.df)] <- 0

# STEP 2: For coloumns 7-16, "0" should be replaced with "NA"
original.df[, 7:16][original.df[, 7:16] == 0] <- NA

# Step 3: Remove 'home' row
original.df <- original.df[original.df$species != 'home', ]
### Removes all rows with 'home' value

#STEP 4: Edit 'species' coloumn to have no numbers
original.df <- mutate(original.df, species = gsub("[[:digit:]]","",original.df$species))

# STEP 5: The two different data sets need to be seperated into two different dataframes
percentcoverage.df <- select(original.df, species, photo, '%rock', '%veg', '%mud/dirt', '%other')
rocksize.df <- select(original.df, species, photo, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)

# STEP 6: Rename columns X1-X10, 1-10 in rocksize.df
rocksize.df <- rocksize.df %>%
  rename("1" = "X1", "2" = "X2","3" = "X3","4" = "X4","5" = "X5","6" = "X6","7" = "X7","8" = "X8","9" = "X9","10" = "X10")

# STEP 7: In rocksize.df, create seperate columns for 'rock.num' and 'rock.circumference'
rocksize.df <- rocksize.df %>%
  gather('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', key = "rock#", value = "rock.circumference")

# STEP 8: Create boxplot of rocksize in relation to species
ggplot(data = rocksize.df, mapping = aes(x = species, y = rock.circumference)) +
  geom_boxplot(varwidth = TRUE) +
  ggtitle("Diameter of Rocks Found Near Nests of Various Species on _______ Island") + 
  xlab("Species") + 
  ylab("Diameter (cm)") +
  scale_x_discrete(labels = c('Common Eider', 'Arctic Tern', 'Sabine Gull', 'Long-Tailed Duck', 'Purple Sandpiper', 'Random'),
                   limits = c('coei', 'arte', 'sagu', 'ltdu', 'pusa', 'rand'))
### creates boxplot with width proportional to the number of observations

# STEP 9: Rename columns in percentcoverage.df 
percentcoverage.df <- percentcoverage.df %>%
  rename("rock" = "%rock", "vegetation" = "%veg", "mud.dirt" = "%mud/dirt", "other" = "%other")

# STEP 9: In percentcoverage.df, create seperate columns for 'ground.type' and 'amount.of.cover'
percentcoverage.df <- percentcoverage.df %>%
  gather('rock', 'vegetation', 'mud.dirt', 'other', key = "ground.type", value = "percent.cover")

# STEP 10: Create graph of percent coverage in relation to species

percentcoverageSPECIES.df <- percentcoverage.df # Turns out theres no command for facet wraps to label each facet individually yet so a new df was made with corrected names
  percentcoverageSPECIES.df[percentcoverageSPECIES.df == "arte"] <- 'Arctic Tern'
  percentcoverageSPECIES.df[percentcoverageSPECIES.df == "coei"] <- 'Common Eider'
  percentcoverageSPECIES.df[percentcoverageSPECIES.df == "ltdu"] <- 'Long-Tailed Duck'
  percentcoverageSPECIES.df[percentcoverageSPECIES.df == "pusa"] <- 'Purple Sandpiper'
  percentcoverageSPECIES.df[percentcoverageSPECIES.df == "rand"] <- 'Random'
  percentcoverageSPECIES.df[percentcoverageSPECIES.df == "sagu"] <- 'Sabine Gull'

# must reorder species in percentcoverageSPECIES.df to follow the order in the previous graph
percentcoverageSPECIES.df <- percentcoverageSPECIES.df %>%
  mutate(species = factor(species, levels = c('Common Eider', 'Arctic Tern', 'Sabine Gull', 'Long-Tailed Duck', 'Purple Sandpiper', 'Random'))) %>%
  arrange(species)
  
ggplot(data = percentcoverageSPECIES.df, mapping = aes(x = ground.type, y = percent.cover)) +
  geom_boxplot() +
  facet_wrap(~ species, nrow = 2) +
  coord_flip() +
  ggtitle("Amount of Coverage of Different Substrates Around the Nests of Various Species on _______ Island") +
  xlab("Substrate Type") + 
  ylab("Percent Cover") +
  scale_x_discrete(labels = c('Other', 'Mud or Dirt', 'Rock', 'Vegetation'), 
                   limits = c('other', 'mud.dirt','rock', 'vegetation'))
