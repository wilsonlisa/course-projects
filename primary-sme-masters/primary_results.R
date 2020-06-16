# https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d

library(tidyverse)
library(rvest)
library(rlist)
library(stringi)
library(htmltab)

url <- 'https://en.wikipedia.org/wiki/2020_Democratic_Party_presidential_primaries#Schedule_and_results'

primary <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
    html_table(fill = TRUE)

colnames(primary) <- primary[1,]

primary <- primary[-c(1:2, 60:61),]

primary <- primary %>%
    mutate(Biden = as.numeric(str_match(`Joe Biden`, "\\((.*?)\\%")[,2]),
           Sanders = as.numeric(str_match(`Bernie Sanders`, "\\((.*?)\\%")[,2]),
           Warren = as.numeric(str_match(`Elizabeth Warren`, "\\((.*?)\\%")[,2]),
           Bloomberg = as.numeric(str_match(`Michael Bloomberg`, "\\((.*?)\\%")[,2]),
           Buttigieg = as.numeric(str_match(`Pete Buttigieg`, "\\((.*?)\\%")[,2]),
           Klobuchar = as.numeric(str_match(`Amy Klobuchar`, "\\((.*?)\\%")[,2]),
           Gabbard = as.numeric(str_match(`Tulsi Gabbard`, "\\((.*?)\\%")[,2]),
           Date = `Date(daily totals)`,
           Delegates = as.numeric(`Total pledgeddelegates`),
           State = str_replace(Contest, "(\\[.*?\\])", "")) %>%
    select(Date, Delegates, State, Biden, Sanders, Warren, Bloomberg, Buttigieg, Klobuchar, Gabbard)

write.csv(primary, "primary_res.csv", row.names = FALSE)
