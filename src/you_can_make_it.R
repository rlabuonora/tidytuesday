library(tidyverse)
library(here)

# murders <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/international_murders.csv")

# murders %>% 
#   write_csv(here("data", "2019", "2019-12-10","international_murders.csv"))

murders <- read_csv(here("data", "2019", "2019-12-10","international_murders.csv"))

# install_github("rensa/ggflags")
library(ggflags)
library(ggthemes)


# plot basico
g <- murders %>% 
  ggplot(aes(country, count)) +
  geom_col() + 
  labs(x="", y="# of gun-related homicides \n per 100,000 people", 
       caption = "Source: \nUNODC Homicide Statistics",
       title = "Homicide rates in G-8 countries")


# fct_inorder is cool

# agrego texto sobre las columnas con geom_text y nudge_y


# theme_economist

murders %>% 
  arrange(-count) %>% 
  mutate(country = fct_inorder(country)) %>% # fct_inorder
  ggplot(aes(country, count)) + 
  geom_col(fill="darkred") + 
  geom_flag(y = -.5, aes(country = code), size = 12) + # geom_flag
  geom_text(aes(label = if_else(country == "RUSSIA", "No Data", as.character(count))), 
                nudge_y = .15) + # nudge, label
  scale_x_discrete(labels=NULL) + # remove labels from x-axis
  labs(x="", y="# of gun-related homicides \n per 100,000 people", 
       caption = "Source: \nUNODC Homicide Statistics",
       title = "Homicide rates in G-8 countries") + 
  theme_economist() +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.caption = element_text(vjust = 70) # move caption
  )


# nyc_regents <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/nyc_regents.csv")

# nyc_regents %>% 
#   write_csv(here("data", "2019", "2019-12-10","nyc_regents.csv"))

nyc_regents <-  read_csv(here("data", "2019", "2019-12-10","nyc_regents.csv"))

# reshape

nyc_reshape <- nyc_regents %>% 
  gather(k="subject", v="tests", -score) %>% 
  group_by(score) %>% 
  summarize(tests = sum(tests, na.rm = TRUE))

# plot basico
g <- ggplot(nyc_reshape, aes(score, tests)) + 
  geom_col()

# Texto

g + 
  labs(caption = "Source: New York City Department of Education",
       title = "Scraping By",
       subtitle = "2010 Regents scores on Algebra, \nGlobal History, Biology, English & US History", 
       y = "", x = "")
  
# Ejes
# Scales en el grammar of graphics
# Customizando los ejes con scale_x_* y scale_y_*
# le pasamos una función a labels
# expand es contraintuitivamente importante
# labels, breaks, expand
g + 
  labs(caption = "Source: New York City Department of Education",
       title = "Scraping By",
       subtitle = "2010 Regents scores on Algebra, \nGlobal History, Biology, English & US History", 
       y = "", x = "") + 
  scale_x_continuous(breaks = seq(0, 100, 5), expand = expand_scale(add = c(0, 0))) + 
  scale_y_continuous(position = "right", 
                     labels = scales::number_format(big.mark = ","), # function
                     breaks = c(1e4, 2e4, 3e4))


# Theme
g + 
  labs(caption = "Source: New York City Department of Education",
       title = "Scraping By",
       subtitle = "2010 Regents scores on Algebra, \nGlobal History, Biology, English & US History", 
       y = "", x = "") + 
  scale_x_continuous(breaks = seq(0, 100, 5), expand = expand_scale(add = c(0, 0))) + 
  scale_y_continuous(position = "right", 
                     labels = scales::number_format(big.mark = ","), # function
                     breaks = c(1e4, 2e4, 3e4)) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(vjust = 1.5),
    axis.text.x = element_text(vjust = 7),
    plot.margin = margin(unit(c(20, 20, 20, 20), "cm"))
  )

# color a las columnas
ggplot(nyc_reshape, aes(score, tests)) + 
  geom_col(color = "black", fill = "#C4843C") + 
  labs(caption = "Source: New York City Department of Education",
       title = "Scraping By",
       subtitle = "2010 Regents scores on Algebra, \nGlobal History, Biology, English & US History", 
       y = "", x = "") + 
  scale_x_continuous(breaks = seq(0, 100, 5), expand = expand_scale(add = c(0, 0))) + 
  scale_y_continuous(position = "right", 
                     labels = scales::number_format(big.mark = ","), # function
                     breaks = c(1e4, 2e4, 3e4)) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(vjust = 1.5),
    axis.text.x = element_text(vjust = 7),
    plot.margin = margin(unit(c(20, 20, 20, 20), "cm"))
  )
  

# Todo junto
nyc_regents %>% 
  gather(k="k", v="v", -score) %>% 
  group_by(score) %>% 
  summarize(v = sum(v, na.rm = TRUE)) %>% 
  ggplot(aes(score, v)) +
  annotate("rect", xmin = 65, xmax=Inf, ymin=0, ymax=Inf, alpha = 0.6, fill = "grey80") + 
  annotate("text", 
           label = "Miniumum Regents \nDiploma Score",
           x = 45, y = 2.5e4) + 
  annotate("segment", arrow=arrow(type = "closed", length = unit(0.2, "cm")), x=50, y=2.8e4, xend=64, yend=3.2e4) + 
  geom_col(color = "black", fill = "#C4843C") + 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(0, 100, 5), expand = expand_scale(add = c(0, 0))) + 
  scale_y_continuous(position = "right", 
                     labels = scales::number_format(big.mark = ","), # function
                     breaks = c(1e4, 2e4, 3e4)) + 
  labs(caption = "Source: New York City Department of Education",
       title = "Scraping By",
       subtitle = "2010 Regents scores on Algebra, \nGlobal History, Biology, English & US History") + 
  labs(y = "", x = "") +
  theme(
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(vjust = 1.5),
    axis.text.x = element_text(vjust = 7),
    plot.margin = margin(unit(c(20, 20, 20, 20), "cm")) # 
  )

# Measels
# devtools::install_github("rafalab/dslabs")
library(dslabs)
# diseases <- dslabs::us_contagious_diseases
diseases <- read_csv(here("data", "2019", "2019-12-10","diseases.csv"))



# Research!
jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)


diseases %>% 
  filter(disease == "Measles") %>% 
  filter(!state %in% c("Hawaii", "Alaska")) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% 
  mutate(state = reorder(state, desc(state))) %>% 
  ggplot(aes(year, state, fill=rate)) + 
  geom_tile(color = "white") + 
  scale_fill_gradientn(colors = jet.colors(16), na.value = "white", name = "Tasa") + ## research!
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", title = "Sarampión", subtitle = "Enfermos cada 10000 habitantes") +
  geom_vline(xintercept = 1963) + 
  annotate(geom = "text", x=1970, y = 50, label = "Introducción de la vacuna") + 
  theme_minimal() + 
  coord_cartesian(clip = 'off') + 
  theme(
    legend.position = "bottom",
  )
