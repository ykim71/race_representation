library(tidyverse)
library(readxl)
library(lme4)
library(MASS)
library(sandwich)
library(lmtest)
library(broom)

data <- read_excel('topic_spread_merge_freq_binary.xlsx')
data <- data %>% mutate(BW = ifelse(grepl("Black", Race)==TRUE, "Black", ifelse(grepl("White", Race)==TRUE, "White", NA)))
data_st <- data %>% dplyr::select(MatchID, Name, BW, Abortion, Crime, Economy, Politics, Race_Inequality, Sports)
data_long <- data_st %>% pivot_longer(cols = Abortion:Sports, names_to = "topic", values_to = "freq")

data_long <- data_long %>% mutate(freq.ln = log(freq+1))

aggregate(freq~topic, data=data_long, FUN=sd)

# all topics
model_lm<-lm(freq.ln ~ as.factor(topic) + as.factor(Name) + BW, data=data_long, na.action = na.omit)
model_lm_robust_clustered<-coeftest(model_lm,
                                      vcov = vcovCL,
                                      type = "HC1",
                                      cluster = ~topic)
tidy(model_lm_robust_clustered, conf.int = TRUE)

model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name) + as.factor(topic), data=data_long , na.action = na.omit)
summary(model_nb)

# separate topic -- negative binomial

# Abortion (not converged)
model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name), data=data_long %>% filter(topic =="Abortion") , na.action = na.omit)
summary(model_nb)

# Crime
model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name), data=data_long %>% filter(topic =="Crime") , na.action = na.omit)
summary(model_nb)

# Economy
model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name), data=data_long %>% filter(topic =="Economy") , na.action = na.omit)
summary(model_nb)

# Politics
model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name), data=data_long %>% filter(topic =="Politics") , na.action = na.omit)
summary(model_nb)

# Race_Inequality
model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name), data=data_long %>% filter(topic =="Race_Inequality") , na.action = na.omit)
summary(model_nb)

# Sports
model_nb <-glm.nb(freq ~  as.factor(BW) + as.factor(Name), data=data_long %>% filter(topic =="Sports") , na.action = na.omit)
summary(model_nb)


# random & fixed effects

# Abortion (not converged)
model_glm <- glmer.nb(freq ~ as.factor(BW) + (1|Name),  data=data_long %>% filter(topic =="Abortion"), na.action = na.omit)
summary(model_glm) # n.s.

# Crime
model_glm <- glmer.nb(freq ~ as.factor(BW) + (1|Name),  data=data_long %>% filter(topic =="Crime"), na.action = na.omit)
summary(model_glm) # n.s.

# Economy
model_glm <- glmer.nb(freq ~ as.factor(BW) + (1|Name),  data=data_long %>% filter(topic =="Economy"), na.action = na.omit)
summary(model_glm) # n.s.

# Politics
model_glm <- glmer.nb(freq ~ as.factor(BW) + (1|Name),  data=data_long %>% filter(topic =="Politics"), na.action = na.omit)
summary(model_glm) # n.s.

# Race_Inequality
model_glm <- glmer.nb(freq ~ as.factor(BW) + (1|Name),  data=data_long %>% filter(topic =="Race_Inequality"), na.action = na.omit)
summary(model_glm) # n.s.

# Sports
model_glm <- glmer.nb(freq ~ as.factor(BW) + (1|Name),  data=data_long %>% filter(topic =="Sports"), na.action = na.omit)
summary(model_glm) # n.s.


# binary vars.
data_st_bin <- data %>% dplyr::select(MatchID, Name, BW, ends_with("_bin"))
names(data_st_bin) = gsub(pattern = "_bin", replacement = "", x = names(data_st_bin))

data_bin_long <- data_st_bin %>% pivot_longer(cols = Abortion:Sports, names_to = "topic", values_to = "freq.bin")

model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name) + as.factor(topic), data = data_bin_long, family = "binomial")
summary(model_bin)

# separate topic -- logistic regression

# Abortion (not converged)
model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name), data=data_bin_long %>% filter(topic =="Abortion"), family = "binomial")
summary(model_bin)

# Crime
model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name), data=data_bin_long %>% filter(topic =="Crime"), family = "binomial")
summary(model_bin) # n.s.

# Economy
model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name), data=data_bin_long %>% filter(topic =="Economy"), family = "binomial")
summary(model_bin) # n.s.

# Politics
model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name), data=data_bin_long %>% filter(topic =="Politics"), family = "binomial")
summary(model_bin) # n.s.

# Race
model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name), data=data_bin_long %>% filter(topic =="Race"), family = "binomial")
summary(model_bin) # n.s. (p <.10)

# Sports
model_bin <- glm(freq.bin ~ as.factor(BW) + as.factor(Name), data=data_bin_long %>% filter(topic =="Sports"), family = "binomial")
summary(model_bin) # n.s.

