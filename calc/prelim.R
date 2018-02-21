library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quanteda)

## load data
rm(list=ls())
load("out/data_pair.Rdata")

## TODO: clean original posts (links etc)!

## load dictionary
dict <- dictionary(file="in/moral foundations dictionary.dic", format="LIWC")

## create corpus $ dfm for positive posts
countMFT <- function(x, dic){
  corpus <- corpus(x)
  txt <- dfm(corpus, dictionary = dic)
  data <- (data.frame(txt)/ntoken(corpus)) %>% 
    mutate(Harm = HarmVirtue + HarmVice,
           Fairness = FairnessVirtue + FairnessVice,
           Ingroup = IngroupVirtue + IngroupVice,
           Authority = AuthorityVirtue + AuthorityVice,
           Purity = PurityVirtue + PurityVice,
           MFTcombined = Harm + Fairness + Ingroup + Authority + Purity) %>%
    select(-HarmVirtue:-PurityVice)
  data
}

tmp <- countMFT(data_pair$pos_text, dict) - countMFT(data_pair$neg_text, dict)

plot_df <- tmp %>% 
  gather() %>%
  group_by(key) %>%
  summarise(mean = mean(value), sd = sd(value), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - se, cihi = mean + se)

ggplot(plot_df, aes(x=key, y=mean, ymin=cilo, ymax=cihi)) +
  geom_point() + geom_errorbar() + geom_hline(yintercept = 0)

## select political topics
cases <- sort(union(grep("polit", data_pair$op_text), grep("polit", data_pair$op_title)))
cases <- sort(union(grep("liberal", data_pair$op_text), grep("liberal", data_pair$op_title)))
cases <- sort(union(grep("conservative", data_pair$op_text), grep("conservative", data_pair$op_title)))

plot_df <- tmp[cases,] %>% 
  gather() %>%
  group_by(key) %>%
  summarise(mean = mean(value), sd = sd(value), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - se, cihi = mean + se)

ggplot(plot_df, aes(x=key, y=mean, ymin=cilo, ymax=cihi)) +
  geom_point() + geom_errorbar() + geom_hline(yintercept = 0)

