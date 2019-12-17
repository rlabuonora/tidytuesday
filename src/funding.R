library(tidyverse)
library(forcats)

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
#energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
#climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")


fed_rd <- fed_rd %>% 
  mutate(department = fct_collapse(department,
                                   DOD = "DOD",
                                   DOE = "DOE",
                                   NASA = "NASA",
                                   NSF = "NSF",
                                   USDA = "USDA",
                                   NIH = "NIH",
                                   Other = c("VA", "DOT", "DHS", "EPA", "Other", "Interior"))) %>% 
  group_by(department, year) %>% 
  summarize(rd_budget = sum(rd_budget))



fed_rd %>% 
  ggplot(aes(year, rd_budget/1e6, fill=department)) + 
  geom_area() + 
  scale_fill_discrete(name = "Agency") +
  scale_y_continuous(labels = scales::dollar_format()) + 
  labs(title = "Federal R&D Funding by Agency",
       subtitle = "(budget authority, millions of dollars)") +
  labs(x = "Fiscal Year", y = NULL) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
