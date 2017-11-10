library(readxl); library(dplyr); library(ggplot2); library(tidyr)

# Download the file shown below, extract zipped xls file, open it in Excel, and save as xlsx (Excel Workbook)
# http://results.enr.clarityelections.com/GA/Fulton/71487/191092/reports/detailxls.zip

# Read Table of Contents
results_toc <- read_excel("data/detail_20171108_014146.xlsx", "Table of Contents", skip = 3)

results_mayor_candidates <- t(read_excel("data/detail_20171108_014146.xlsx", sheet = "5", n_max = 1))
results_mayor_candidates <- as.character(results_mayor_candidates)[which(!is.na(as.character(results_mayor_candidates)))]
results_mayor_candidates2 <- data_frame(id = 0:10, candidate = results_mayor_candidates)

results_mayor <- read_excel("data/detail_20171108_014146.xlsx", "5", skip = 2)
names(results_mayor)[3:9] <- paste0(names(results_mayor)[3:9], "__0")
results_mayor %>% select(-Total) %>% gather(field, value, `Election Day__0`:`Total Votes__10`)

# precincts is a data frame with county & registered voters
precincts <- results_mayor %>% select(County, `Registered Voters`) %>% distinct() %>%
  arrange(County) %>% filter(County != "Total:")

# results_mayor2 is a data frame with fields County, metric, value, candidate
results_mayor2 <- results_mayor %>% select(-Total, -`Registered Voters`) %>% 
  gather(field, value, `Election Day__0`:`Total Votes__10`) %>% 
  separate(field, into = c("metric", "id"), sep = "__") %>% 
  mutate(id = as.integer(id)) %>% left_join(results_mayor_candidates2, by = "id") %>% 
  select(-id) %>% filter(County != "Total:")

results_mayor2_wide <- results_mayor2 %>% 
  spread(metric, value) %>% 
  arrange(candidate, County) %>% 
  mutate(absentee.pct = `Absentee by Mail` / `Total Votes`, 
         early.voting.pct = (`Advance in Person 1` + `Advance in Person 2`+ `Advance in Person 3`) / `Total Votes`)