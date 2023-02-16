## R-Fresher ##

# Part 2 -- activity script
# Remember to use everything you learnt in Part 1
# (Especially for naming objects!)


#### Working directories ####

# 1. Get your current working directory
getwd()
# "/Users/hannahbunting/Documents/SMART Skills/R-Fresher"

# 2. Set your working directory to your Downloads folder
setwd("/Users/hannahbunting/Downloads")

#### Loading in data ####

# 3. Download the NYC Squirrels data and load it into R
squirrels <- read.csv("nyc_squirrels.csv")

# 4. Check the data
head(squirrels)
summary(squirrels)
colnames(squirrels)

# 5. Download the Metal Bands data and load it into R
metal_bands_2017 <- read.csv("metal_bands_2017.csv")

# 6. Check the data
head(metal_bands_2017)
summary(metal_bands_2017)
colnames(metal_bands_2017)

#### Wrangle the Squirrels data ####

# 7. Create a new variable that tells us if a squirrel is Grey or not Grey
table(squirrels$primary_fur_color)
# Black Cinnamon     Gray    
  # 103      392     2473        
squirrels$grey_bin <- ifelse(squirrels$primary_fur_color == "Gray", "Grey", "Not grey")

# 8. Create a binary variable that shows 1 when a squirrel was running AND chasing
squirrels$run_chase <- ifelse(squirrels$running == "TRUE" &
                                squirrels$chasing == "TRUE", 1, 0)

# 9. Create a new categorical variable from the 'location' variable
# The new categories should be Above Ground, On Ground and Unknown
table(squirrels$location, useNA = "always")
# Above Ground Ground Plane         <NA> 
  # 843         2116           64 
squirrels$new_ground[squirrels$location == "Above Ground"] <- "Above Ground"
squirrels$new_ground[squirrels$location == "Ground Plane"] <- "On Ground"
squirrels$new_ground[is.na(squirrels$location)] <- "Unknown"


#### Wrangle the Metal Bands data ####

# 10. Create a new variable that shows which decade the band was formed
metal_bands_2017$formed_decade[metal_bands_2017$formed < 1970] <- "60s"
metal_bands_2017$formed_decade[metal_bands_2017$formed > 1969 & metal_bands_2017$formed < 1980] <- "70s"
metal_bands_2017$formed_decade[metal_bands_2017$formed > 1979 & metal_bands_2017$formed < 1990] <- "80s"
metal_bands_2017$formed_decade[metal_bands_2017$formed > 1989 & metal_bands_2017$formed < 2000] <- "90s"
metal_bands_2017$formed_decade[metal_bands_2017$formed > 1999 & metal_bands_2017$formed < 2009] <- "00s"
metal_bands_2017$formed_decade[metal_bands_2017$formed > 2009] <- "10s"


# 11. Create a binary variable that indicates whether a band has split or not
metal_bands_2017$together <- ifelse(metal_bands_2017$split != "-", 0, 1)

# 12. Create a variable showing when bands have higher than mean fans AND
# they were formed after 2010
metal_bands_2017$hi_new <- ifelse(metal_bands_2017$fans > mean(metal_bands_2017$fans) &
                                    metal_bands_2017$formed > 2009, "Yes", "No")

#### Creating your own data ####

# 13. Generate your own data set that has three rows and five columns
my_data <- data.frame(one = c(1,2,3),
                      two = c(4,5,6),
                      three = c(7,8,9),
                      four = c(10,11,12),
                      five = c(13,14,15))

print(my_data)
# one two three four five
# 1   1   4     7   10   13
# 2   2   5     8   11   14
# 3   3   6     9   12   15

# 14. Generate a second data set
# At least one variable must be created using each of these functions:
# # # a) seq()
# # # b) rnorm()
# # # c) factor

tests <- data.frame(wages = rnorm(10, 300, 100),
                    food = seq(20, 40, length.out = 10),
                    week = factor(rep(1:5, times = 2), levels = c(1:5),
                                  labels = c("Week 1", "Week 2", "Week 3",
                                             "Week 4", "Week 5")))
tests
# wages     food   week
# 1  243.9524 20.00000 Week 1
# 2  276.9823 22.22222 Week 2
# 3  455.8708 24.44444 Week 3
# 4  307.0508 26.66667 Week 4
# 5  312.9288 28.88889 Week 5
# 6  471.5065 31.11111 Week 1
# 7  346.0916 33.33333 Week 2
# 8  173.4939 35.55556 Week 3
# 9  231.3147 37.77778 Week 4
# 10 255.4338 40.00000 Week 5