setwd("/Users/aantosz/Dysk Google/0 SYNCHRONIZACJA/eksperyment HR/HR_skrawki/szczurzo-ludzki_all/")

library(openxlsx)
df = read.xlsx("eksport_tabelek/all_data_stats.xlsx")

library(data.table)
setDT(df)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)

a = df %>%
  group_by(Category) %>%
  identify_outliers(Density)

# 4 observations identified as extreme outliers - we're cutting them out

df_without_extreme_outliers <- df %>%
  filter(Code != "TJ" | Category != "CON_CeL" | Order != "o" | Skrawek != "4") %>%
  filter(Code != "TJ" | Category != "CON_LA" | Order != "o" | Skrawek != "4") %>%
  filter(Code != "TJ" | Category != "CON_LA" | Order != "n" | Skrawek != "1") %>%
  filter(Code != "TJ" | Category != "EXP_LA" | Order != "n" | Skrawek != "1")

df.summary <- df_without_extreme_outliers %>%
  group_by(Code, Category) %>%
  summarise(Density.mean = mean(Density, na.rm = TRUE)) %>%
  ungroup()

res.aov = anova_test(data=df.summary, dv=Density.mean, wid=Code, within=Category)

pwc <- df.summary %>%
  pairwise_t_test(
    Density.mean ~ Category, paired = TRUE,
    p.adjust.method = "fdr"
  )
pwc
