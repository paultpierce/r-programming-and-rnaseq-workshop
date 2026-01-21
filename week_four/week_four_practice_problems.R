library(tidyverse)
library(ggpubr)

as_tibble(iris)  # This is your data set for these problems
View(iris)  # View it in RStudio

### dplyr problems ###

# NOTE: Don't forget, you there's a cheat sheet available on Box under "Resources"

# A. Complete the filter so that it only returns the "virginica" species
iris %>%
  filter(Species == "virginica")

# B. Use summarise to get the mean sepal length for each species
iris %>%
  group_by(Species) %>%
  summarise(sepal_mean = mean(Sepal.Length))

# C. Get the ratio of sepal length to sepal width for all samples
iris %>%
  mutate(sepal_ratio = Sepal.Length/Sepal.Width)

# D. Get the average ratio of petal length to petal width for each species
iris %>%
  mutate(petal_ratio = Petal.Length/Petal.Width) %>%
  group_by(Species) %>%
  summarise(petal_ratio_avg = mean(petal_ratio))

### ggplot problems ###

# A. Make a scatter plot comparing sepal length to petal length
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()

# B. Color the plot by the Species 
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point()

# C. Change to theme_classic(), st the base font size to 16, 
# set the x label to "Sepal Length (cm)", and set the y label to "Petal Length (cm)",
# set the title to "Sepal vs Petal Length in Iris Flowers"
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16)

# D. Plot the linear correlation between Sepal and Petal length stat_smooth 
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) + 
  stat_smooth(method = "lm")

# E. Add the pearson correlation coefficient using ggpubr's stat_cor()
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) + 
  stat_smooth(method = "lm") +
  stat_cor(method = "pearson")

# F. Save the plot to a pdf file
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) + 
  stat_smooth(method = "lm") +
  stat_cor(method = "pearson") +
  ggsave(filename = "my_figure.pdf")


### Challenge Problems ###

# Answer the following using dplyr and ggplot2 (with significance testing via ggpubr):


# 1. Do 6 cylinder cars have better MPG than 8 cylinder cars?
as_tibble(mtcars)
View(mtcars)

mtcars %>% 
  filter(cyl == 6 | cyl == 8) %>% 
    ggplot(mapping = aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
    geom_boxplot() +
      stat_compare_means(method = "t.test", label = "p.signif", 
                         comparisons = list(c("6", "8"))) +
      theme_classic(base_size = 16) +
      theme(legend.position = "none") +
      xlab("Number of Cylinders") +
      ylab("Miles per Gallon (MPG)") +
      labs(title = "Number of Cylinders and MPG")
    ggsave(filename = "challenge1_done.png")

# 2. Is there a correlation between urban population and Murder rate?
as_tibble(USArrests)
View(USArrests)

USArrests %>% 
  ggplot(mapping = aes(x = UrbanPop, y = Murder)) +
  geom_point() +
    theme_classic() +
    xlab("Percentage of urban population") +
    ylab("Murder rate") +
    labs(title = "Urban pop and per capita murder rate") +
      stat_smooth(method = "lm") +
      stat_cor()
  ggsave(filename = "challenge2_done.png")

# 3. Did the treatment #1 or #2 make the plants grow more or less than control?
as_tibble(PlantGrowth)
View(PlantGrowth)

PlantGrowth %>% 
  group_by(group) %>% 
    ggplot(mapping = aes(x = factor(group, labels = c("Control", "Treatment 1", "Treatment 2")),
                         y = weight, fill = group)) +
    geom_boxplot() +
      theme_classic(base_size = 14) +
      xlab(NULL) +
      ylab("Plant weight") +
      theme(legend.position = "none") +
      labs(title = "Plant Growth with Treatment") +
    stat_compare_means(method = "t.test", label = "p.signif",
                       comparisons = list(c("Control", "Treatment 1"), c("Control", "Treatment 2")))
    ggsave(filename = "challenge3_done.png")

# 4. Does Vitamin C supplementation improve tooth growth? Is OJ better than Vitamin C at dose of 2?
as_tibble(ToothGrowth)
View(ToothGrowth)

ToothGrowth %>% 
  ggplot(mapping = aes(x = factor(supp), y = len, fill = supp)) +
  geom_boxplot() +
    facet_wrap(~ paste(dose, "mg/day")) +
    theme_bw(base_size = 14) +
    xlab(NULL) +
    ylab("Tooth length") +
    labs(title = "Tooth Growth with Treatment") +
    theme(legend.position = "none") +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     comparisons = list(c("OJ", "VC")))
  ggsave(filename = "challenge4_done.png")
