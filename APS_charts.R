library(tidyverse)
library(ggthemes)
library(scales)

download.file("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_1.data.csv?geography=2092957697&date=latestMINUS3-latest&cell=403899393...403899399,403899649...403899655,403899905...403899911,403900161...403900167,403900417...403900423,403900673...403900679,403901697...403901703,403901953...403901959,403902209...403902215,403902465...403902471,403902721...403902727,403902977...403902983&measures=20100,20701", "age_qual.csv")
age_qual_api <- read_csv("age_qual.csv") %>% 
  filter(!is.na(OBS_VALUE), MEASURES_NAME== "Value") %>% 
  select(DATE_CODE, GEOGRAPHY_NAME, CELL_NAME, OBS_VALUE)

age_qual_api <- age_qual_api %>% 
  separate(CELL_NAME, c("table", "cell", "age", "qual"), " - |:") %>% 
  separate(cell, c("cell","group")) %>% 
  select(-table, -cell)
age_qual_api$age <- str_trim(str_remove(age_qual_api$age, "All people aged "))
age_qual_api$qual <- str_trim(str_remove(age_qual_api$qual, "\\)"))
total <- age_qual_api %>% filter(group=="Total") %>% select(-group, -GEOGRAPHY_NAME, -DATE_CODE)
econact <- age_qual_api %>% filter(group!="Total")%>% select(-group, -GEOGRAPHY_NAME, -DATE_CODE)
age_qual_wide <- left_join(total, econact, by=c("age", "qual"), suffix=c(".Total", ".Econ.active")) %>% 
  mutate(Econ.inactive = OBS_VALUE.Total - OBS_VALUE.Econ.active, 
         Econ.inactive.percent = Econ.inactive/OBS_VALUE.Total, 
         Econ.active.percent = OBS_VALUE.Econ.active/OBS_VALUE.Total)
age_qual_long <- age_qual_wide %>% select(age, 
                                          qual, 
                                          "Economically inactive"=Econ.inactive.percent, 
                                          "Economically active"=Econ.active.percent) %>% 
  pivot_longer(!c(age,qual), names_to="Econactivity", values_to= "Percent")

plot_age_qual_inact <- age_qual_long %>% 
  filter(Econactivity== "Economically inactive",age != "16-19")%>% 
  ggplot(aes(x=age, y=Percent)) + 
  geom_bar(stat="identity") + 
  facet_wrap(vars(qual)) + 
  theme_economist_white(gray_bg=FALSE) + 
  scale_y_continuous(labels=scales::percent_format())+ 
  labs(title="Economic inactivity rate by qualification and age", subtitle = "Percentage of those in age-group with qualification", caption="Annual Population Survey, latest full year, data sourced from www.nomisweb.co.uk")

ggsave("age_qual_inact_tate.png", plot_age_qual_inact, width= 12, height=8, units="in")
