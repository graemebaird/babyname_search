# Originally written by Hadley Wickham, copied from https://github.com/hadley/data-baby-names/ --------

library(plyr)

files <- dir("raw", full = T)
names(files) <- gsub("\\.csv", "", dir("raw"))

# Load all csv files into a single data frame and give informative column
# names

bnames <- ldply(files, read.csv, header = F, skip = 1, nrows = 1000,
                stringsAsFactors = FALSE)
names(bnames) <- c("year", "rank", "boy_name", "boy_percent", "girl_name", "girl_percent")

# Change from wide to long form
boys <- bnames[c("year", "boy_name", "boy_percent")]
girls <- bnames[c("year", "girl_name", "girl_percent")]

names(boys) <- names(girls) <- c("year", "name", "percent")
boys$sex <- "boy"
girls$sex <- "girl"

all <- rbind(boys, girls)

# Turn year and percent into a real numbers
all$percent <- as.numeric(gsub("%", "", all$percent)) / 100
all$year <- as.numeric(as.character(all$year))

consonants <- letters[! letters %in% c("a", "e", "i", "o", "u")]

all$vowels <- all %>% 
  pull(name) %>% 
  sapply(.,function(x) str_count(tolower(x),c("a", "e", "i", "o", "u")) %>% sum) %>% as.vector()

all %<>% mutate(consonants = str_count(name) - vowels, 
                length = str_count(name), 
                lname = tolower(name))

# Save as csv
write.table(all, "./name_generator/baby-names.csv", sep=",", row = F)
