### Investigating factors affecting nest selection for different bird species
### Q1: Do different species of birds perfer different substrate types for nest selection?
### Q2: Do different species of birds perfer different rock sizes for nest selection?

# STEP 1: Replace "NA" with "0"
# STEP 2: For coloumns 7-16, "0" should be replaced with "NA"
# STEP 3: The two different data sets need to be seperated into two different dataframes
# STEP 4: Rename columns X1-X10, 1-10 in rocksize.df
# STEP 5: Edit 'species' coloumn to have no numbers in both dataframes
# STEP 6: In rocksize.df, create seperate columns for 'rock.num' and 'rock.circumference'
# STEP 7: Create boxplot of rocksize in relation to species
# STEP 8: Edit 'species' column to have no numbers in percentcoverage.df
# STEP 9: In percentcoverage.df, create seperate columns for 'ground.type' and 'amount.of.cover'
# STEP 10: Create graph of percent coverage in relation to species

library(tidyverse)
original.df <- read_csv("./data_re_meth2.csv")
percentcoverage.df <- read_csv("./percentcoverage.csv")
rocksize.df <- read_csv("./rocksize.csv")

# STEP 1: Replace "NA" with "0"
original.df[is.na(original.df)] <- 0

# STEP 2: For coloumns 7-16, "0" should be replaced with "NA"
original.df[, 7:16][original.df[, 7:16] == 0] <- NA

# STEP 3: The two different data sets need to be seperated into two different dataframes
percentcoverage.df <- select(original.df, species, photo, X.rock, X.veg, X.mud.dirt, X.other)
rocksize.df <- select(original.df, species, photo, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)

# STEP 4: Rename columns X1-X10, 1-10 in rocksize.df
names(rocksize.df)[names(rocksize.df) == 'X1'] <- '1'
names(rocksize.df)[names(rocksize.df) == 'X2'] <- '2' 
names(rocksize.df)[names(rocksize.df) == 'X3'] <- '3' 
names(rocksize.df)[names(rocksize.df) == 'X4'] <- '4' 
names(rocksize.df)[names(rocksize.df) == 'X5'] <- '5' 
names(rocksize.df)[names(rocksize.df) == 'X6'] <- '6' 
names(rocksize.df)[names(rocksize.df) == 'X7'] <- '7' 
names(rocksize.df)[names(rocksize.df) == 'X8'] <- '8' 
names(rocksize.df)[names(rocksize.df) == 'X9'] <- '9' 
names(rocksize.df)[names(rocksize.df) == 'X10'] <- '10'

# STEP 5: Edit 'species' coloumn to have no numbers for both dataframes

correctedspecies <- gsub("[[:digit:]]","",rocksize.df$species)
### creates a vector of the correct alpha only values

correctedspecies <- data.frame(correctedspecies)
### turns correctedspecies into a dataframe

correctedspecies$ID <- 1:nrow(correctedspecies)
rocksize.df$ID <- 1:nrow(rocksize.df)
### creates a common column 'ID' for both dataframes to allow them to merge without changing the order of the rows

merged <- merge(x = rocksize.df, y = correctedspecies, by = "ID", sort = FALSE)
### merges rocksize.df with the correctedspecies

merged$species <- NULL
### removes old species column

rocksize.df <- merged[, c(1, 13, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]
### reorders columns

rocksize.df$ID <- NULL
### removes ID column

names(rocksize.df)[names(rocksize.df) == 'correctedspecies'] <- 'species'
### renames the 'correctedspecies' column to 'species'
  
# STEP 6: In rocksize.df, create seperate columns for 'rock.num' and 'rock.circumference'
rocksize.df <- rocksize.df %>%
  gather('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', key = "rock#", value = "rock.circumference")

rocksize.df <- rocksize.df[rocksize.df$species != 'home', ]
### Removes all rows with 'home' variable

# STEP 7: Create boxplot of rocksize in relation to species
ggplot(data = rocksize.df, mapping = aes(x = species, y = rock.circumference)) +
  geom_boxplot(varwidth = TRUE) +
  ggtitle("Rock Diameter") + 
  xlab("Species") + 
  ylab("Diameter (cm)")
### creates boxplot with width proportional to the number of observations

# STEP 8: Edit 'species' column to have no numbers in percentcoverage.df

percentcoverage.df$ID <- 1:nrow(percentcoverage.df)
### Adds 'ID' column

merged2 <- merge(x = percentcoverage.df, y = correctedspecies, by = "ID", sort = FALSE)
### merges percentcoverage.df with the correctedspecies

merged2$species <- NULL
### removes old species column

percentcoverage.df <- merged2[, c(1, 7, 2, 3, 4, 5, 6)]
### reorders columns

percentcoverage.df$ID <- NULL
### removes 'ID' column

names(percentcoverage.df)[names(percentcoverage.df) == 'correctedspecies'] <- 'species'
### renames the 'correctedspecies' column to 'species'

percentcoverage.df <- percentcoverage.df[percentcoverage.df$species != 'home', ]
### Removes all rows with 'home' value

# STEP 9: In percentcoverage.df, create seperate columns for 'ground.type' and 'amount.of.cover'

names(percentcoverage.df)[names(percentcoverage.df) == 'X.rock'] <- 'rock'
names(percentcoverage.df)[names(percentcoverage.df) == 'X.veg'] <- 'vegetation'
names(percentcoverage.df)[names(percentcoverage.df) == 'X.mud.dirt'] <- 'mud.dirt'
names(percentcoverage.df)[names(percentcoverage.df) == 'X.other'] <- 'other'

percentcoverage.df <- percentcoverage.df %>%
  gather('rock', 'vegetation', 'mud.dirt', 'other', key = "ground.type", value = "percent.cover")

# STEP 10: Create graph of percent coverage in relation to species
ggplot(data = percentcoverage.df, mapping = aes(x = ground.type, y = percent.cover)) +
  geom_boxplot() +
  facet_wrap(~ species, nrow = 2) +
  coord_flip() +
  xlab("Substrate Type") + 
  ylab("Percent Cover")
