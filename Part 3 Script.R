
# Loading packages
library('tidyverse')
library('ggplot2')

# Getting the dataset
df <- mtcars

summary(df)

names(df)

# Motor Trend Car Road Tests data
# The data was extracted from the 1974 Motor Trend US magazine,
# and comprises fuel consumption and 10 aspects of automobile design and performance
# for 32 automobiles (1973–74 models).

# mpg - Miles/(US) gallon
# cyl - Number of cylinders
# disp - Displacement (cu.in.)
# hp - Gross horsepower
# drat - Rear axle ratio
# wt - Weight (1000 lbs)
# qsec - 1/4 mile time
# vs - Engine (0 = V-shaped, 1 = straight)
# am - Transmission (0 = automatic, 1 = manual)
# gear - Number of forward gears
# carb - Number of carburetors

### Boxplot

# Baseline boxplot

ggplot(data = df, aes(y = hp)) +
  geom_boxplot()

# Add y axis label and plot title

ggplot(data = df, aes(y = hp)) +
  geom_boxplot() +
  labs(y = "Gross horsepower (hp)",
       title = "The distribution of car gross horsepower")
  
# Change the limits and ticks of the y axis

ggplot(data = df, aes(y = hp)) +
  geom_boxplot() +
  labs(y = "Gross horsepower (hp)",
       title = "The distribution of car gross horsepower") +
  scale_y_continuous(limits = c(50, 350),
                     breaks = seq(50, 350, 50))

# Plotting the distribution of gross horsepower separately for V-shaped and straight engines (vs)

# Grouping variable (x) should be a factor variable, so I recode it and give proper labels
df$vs_rec = factor(df$vs, labels = c('V-shaped', 'Straight'))

# Note that I added x = vs_rec to the aes argument

ggplot(data = df, aes(x = vs_rec, y = hp)) +
  geom_boxplot() +
  labs(y = "Gross horsepower (hp)",
       title = "The distribution of car gross horsepower by engine type",
       x = "Engine type") +
  scale_y_continuous(limits = c(50, 350),
                     breaks = seq(50, 350, 50))

# Adding some color to the graph

ggplot(data = df, aes(x = vs_rec, y = hp, fill = vs_rec)) +
  geom_boxplot() +
  labs(y = "Gross horsepower (hp)",
       title = "The distribution of car gross horsepower by engine type",
       x = "Engine type") +
  scale_y_continuous(limits = c(50, 350),
                     breaks = seq(50, 350, 50))

# You can then tweak the legend, colors of the graph, and much more

### Histogram

# Baseline histogram

ggplot(data = df, aes(x = mpg)) + 
  geom_histogram()

# Picking a better bin width (hard when there's few data points)

ggplot(data = df, aes(x = mpg)) + 
  geom_histogram(binwidth = 1)

# Adding axes labels and plot title

ggplot(data = df, aes(x = mpg)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = "The distribution of car mileage per US gallon", 
       x = "Miles / (US) gallon",
       y = "Number of observations")

# Changing x axis ticks

ggplot(data = df, aes(x = mpg)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = "The distribution of car mileage per US gallon", 
       x = "Miles / (US) gallon",
       y = "Number of observations") +
  scale_x_continuous(limits = c(8, 35),
                     breaks = seq(8, 35, 1))
  
# Adding color to the graph

ggplot(data = df, aes(x = mpg)) + 
  geom_histogram(binwidth = 1,
                 fill = 'darkblue',
                 color = 'white') + 
  labs(title = "The distribution of car mileage per US gallon", 
       x = "Miles / (US) gallon",
       y = "Number of observations") +
  scale_x_continuous(limits = c(8, 35),
                     breaks = seq(8, 35, 1))

# Facetting by transmission type (am)

# Recoding am into factor
df$am_rec = factor(df$am, labels = c('Automatic', 'Manual'))

ggplot(data = df, aes(x = mpg)) + 
  geom_histogram(binwidth = 1,
                 fill = 'darkblue',
                 color = 'white') + 
  labs(title = "The distribution of car mileage per US gallon by transmission type", 
       x = "Miles / (US) gallon",
       y = "Number of observations") +
  scale_x_continuous(limits = c(8, 35),
                     breaks = seq(8, 35, 1)) +
  facet_grid(. ~ am_rec)

# Facetting by transmission and subsetting by engine

# Note than if fill argument is used is main aes(), then it should not be defined within the geom

ggplot(data = df, aes(x = mpg, fill = vs_rec)) + 
  geom_histogram(binwidth = 1,
                 color = 'white',
                 alpha = 0.7,
                 position = 'identity') + 
  labs(title = "The distribution of car mileage per US gallon by transmission type", 
       x = "Miles / (US) gallon",
       y = "Number of observations") +
  scale_x_continuous(limits = c(8, 35),
                     breaks = seq(8, 35, 1)) +
  facet_grid(. ~ am_rec)

##### Bar plot

# gear - Number of forward gears

table(df$gear)

# Baseline bar plot

ggplot(data = df, aes(x = gear)) +
  geom_bar()

# Adding lables

ggplot(data = df, aes(x = gear)) +
  geom_bar() + 
  labs(title = "Number of forward gears per car", 
       x = "Number of forward gears",
       y = "Number of observations")

# Changing the y axis

ggplot(data = df, aes(x = gear)) +
  geom_bar() + 
  labs(title = "Number of forward gears per car", 
       x = "Number of forward gears",
       y = "Number of observations") +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, 3))

# Splitting by engine type

ggplot(data = df, aes(x = gear, fill = vs_rec)) +
  geom_bar() + 
  labs(title = "Number of forward gears and type of engine", 
       x = "Number of forward gears",
       y = "Number of observations") +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, 3))

# Use position = 'dodge' to put bars next to each other

ggplot(data = df, aes(x = gear, fill = vs_rec)) +
  geom_bar(position = 'dodge') + 
  labs(title = "Number of forward gears and type of engine", 
       x = "Number of forward gears",
       y = "Number of observations") +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, 3))

# Use position = 'fill' to switch to proportions

ggplot(data = df, aes(x = gear, fill = vs_rec)) +
  geom_bar(position = 'fill') + 
  labs(title = "Number of forward gears and type of engine", 
       x = "Number of forward gears",
       y = "% of observations")

# In fact, if we assume that the type of engine affects the number of forward gears,
# then it would be more correct to treat vs_rec as x, and gear as fill

# gear should be coecrced to factor

ggplot(data = df, aes(x = vs_rec, fill = as.factor(gear))) +
  geom_bar(position = 'fill') + 
  labs(title = "Number of forward gears and type of engine", 
       x = "Type of engine",
       y = "% of observations")

# Modifying the labels of the y scale

ggplot(data = df, aes(x = vs_rec, fill = as.factor(gear))) +
  geom_bar(position = 'fill') + 
  labs(title = "Number of forward gears and type of engine", 
       x = "Type of engine",
       y = "% of observations") +
  scale_y_continuous(labels = c('0', '25%', '50%', '75%', '100%'))

# Modifying the legend

ggplot(data = df, aes(x = vs_rec, fill = as.factor(gear))) +
  geom_bar(position = 'fill') + 
  labs(title = "Number of forward gears and type of engine", 
       x = "Type of engine",
       y = "% of observations") +
  scale_y_continuous(labels = c('0', '25%', '50%', '75%', '100%')) +
  scale_fill_discrete(name = "Number of forward gears") +
  theme(legend.position = "bottom")

### Scatterplot

# Baseline scattterplot

ggplot(data = df, aes(x = wt, y = qsec)) +
  geom_point()

# Adding labels and adjusting the size of the points

ggplot(data = df, aes(x = wt, y = qsec)) +
  geom_point(size = 2.5) + 
  labs(title = "The relationship between car weight and 1/4 mile time", 
       x = "Weight (1000 lbs)",
       y = "1/4 mile time")

# Adjusting the axes

ggplot(data = df, aes(x = wt, y = qsec)) +
  geom_point(size = 2.5) + 
  labs(title = "The relationship between car weight and 1/4 mile time", 
       x = "Weight (1000 lbs)",
       y = "1/4 mile time") +
  scale_x_continuous(limits = c(1,6),
                     breaks = seq(1, 6, 1)) +
  scale_y_continuous(limits = c(14, 24),
                     breaks = seq(14, 24, 2))

# Splitting by engine type

ggplot(data = df, aes(x = wt, y = qsec, color = vs_rec, shape = vs_rec)) +
  geom_point(size = 2.5) + 
  labs(title = "The relationship between car weight and 1/4 mile time", 
       x = "Weight (1000 lbs)",
       y = "1/4 mile time") +
  scale_x_continuous(limits = c(1,6),
                     breaks = seq(1, 6, 1)) +
  scale_y_continuous(limits = c(14, 24),
                     breaks = seq(14, 24, 2))

# Adding the regression lines

ggplot(data = df, aes(x = wt, y = qsec, color = vs_rec, shape = vs_rec)) +
  geom_point(size = 2.5) + 
  labs(title = "The relationship between car weight and 1/4 mile time", 
       x = "Weight (1000 lbs)",
       y = "1/4 mile time") +
  scale_x_continuous(limits = c(1,6),
                     breaks = seq(1, 6, 1)) +
  scale_y_continuous(limits = c(14, 24),
                     breaks = seq(14, 24, 2)) +
  geom_smooth(method = "lm", se = FALSE)

# Adjusting the legend, changing the plot theme, changing the default colors

ggplot(data = df, aes(x = wt, y = qsec, color = vs_rec, shape = vs_rec)) +
  geom_point(size = 2.5) + 
  labs(title = "The relationship between car weight and 1/4 mile time", 
       x = "Weight (1000 lbs)",
       y = "1/4 mile time") +
  scale_x_continuous(limits = c(1,6),
                     breaks = seq(1, 6, 1)) +
  scale_y_continuous(limits = c(14, 24),
                     breaks = seq(14, 24, 2)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")


