setwd("/Users/aantosz/Dysk Google/0 SYNCHRONIZACJA/eksperyment HR/HR behawior/ponowna analiza behawioru 1+2 tura/")

library(openxlsx)
df_wide = read.xlsx("behavior_alldata.xlsx")

library(data.table)
setDT(df_wide)

# delete rows RP_exp due to incomplete recordings
df_wide <- df_wide[!grepl("RP_exp", df_wide$file),]

# delete rows in the CONTR group, for which there are no paired EXP rows (due to lacks of data)
df_wide <- df_wide[!grepl("RP_contr", df_wide$file),]
df_wide <- df_wide[!grepl("RF_contr", df_wide$file),]
df_wide <- df_wide[!grepl("AG_contr", df_wide$file),]

# add index column
df_wide$Index = seq.int(nrow(df_wide))

# delete latency column
library(dplyr)
df_wide <- select(df_wide, -contains("_l"))

# convert 'exploration' into 'cage-exploration'
colnames(df_wide)[7] <- "cage_exploration_n"
colnames(df_wide)[8] <- "cage_exploration_d"
colnames(df_wide)[2] <- "human_id"

### IMPORTANT STEP - SELECTING PHASE

# delete columns with behaviors of no interest - we leave only 'in the cage' phase
df_wide <- df_wide %>% select(matches(c("Index", "file", "human_id", "group", "order", "cage_exploration", "human-exploration-0")))

# delete columns with behaviors of no interest - we leave only 'in the arms' phase
#df_wide <- select(df_wide, -contains(c("cage", "0", "hands.on", "approach", "wait", "avoidance", "tested.rat", "human.interest", "half", "hide.together", "freeze", "rat.interaction")))

# list the behaviors
library(stringr)
behaviors = str_remove(colnames(df_wide)[6:9], "_n") # columns [5:12] in the "in the arms" version of analysis 
behaviors = as.data.frame(behaviors)
behaviors <- behaviors [!grepl("_d", behaviors$behaviors),]

df_long <-melt(df_wide, id=c("Index", "file", "human_id", "group", "order"),
               measure=patterns("_d$", "_n$"),
               value.name=c("duration", "number"),
               variable.name = "behavior")

#one_to_ten = unique(df_long$behavior)

library(plyr)
#df_long$behavior <- mapvalues(df_long$behavior, from=1:4, to=c("armpit-hide-1", "human-exploration-1", "armpit-hide-2",
#                                                               "human-exploration-2"))
df_long$behavior <- mapvalues(df_long$behavior, from=1:2, to=c("cage_exploration", "human-exploration-0")) # "in the cage" version


# DURATION
# normality plots
library(ggpubr)
ggqqplot(df_long, "duration", facet.by = c("group", "behavior"))

ggplot(df_long, aes(x=behavior, y=duration, fill=group)) + geom_boxplot()

# mixed model ANOVA
library(afex)
a1 <- aov_ez("file", "duration", df_long, between = c("group", "human_id"), 
             within = "behavior")
knitr::kable(nice(a1))

# For a significant interaction:
# Effect of group for each behavior
library(rstatix)
one.way <- df_long %>%
  group_by(behavior) %>%
  anova_test(dv = duration, wid = Index, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels
pwc <- df_long %>%
  group_by(behavior) %>%
  pairwise_t_test(duration ~ group, p.adjust.method = "bonferroni", detailed = TRUE)
pwc

library(emmeans)
m1 <- emmeans(a1, ~ behavior)
p1 <- pairs(m1)


