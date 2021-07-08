---
title: "gt footnote in .md"
author: "John Goldin"
date: "5/19/2021"
output: blogdown::html_page
---




```r
library(tidyverse)
library(gt)
# Take the `islands` dataset and use some
# dplyr functionality to obtain the ten
# biggest islands in the world
islands_tbl <-
  tibble(
    name = names(islands),
    size = islands
  ) %>%
  arrange(desc(size)) %>%
  slice(1:10)
gt_tbl <- gt(data = islands_tbl) %>%
  tab_footnote(
    footnote = "The Americas.",
    locations = cells_body(columns = 1, rows = 3:4)
  )
print(gt_tbl)
```
