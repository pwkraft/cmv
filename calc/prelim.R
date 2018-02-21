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
  mutate(key = factor(key, levels = c("MoralityGeneral", "MFTcombined"
                                      , "Authority", "Ingroup", "Purity"
                                      , "Fairness", "Harm"))) %>%
  group_by(key) %>%
  summarise(mean = mean(value), sd = sd(value), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - se, cihi = mean + se)

ggplot(plot_df, aes(x=key, y=mean, ymin=cilo, ymax=cihi)) +
  geom_point() + geom_errorbar(width=0) + geom_hline(yintercept = 0) + theme_bw()

## select political topics
sort(union(grep("politic", data_pair$op_text), grep("politic", data_pair$op_title)))

checkTerm <- function(x){
  cases <- map(x, grep, data_pair$op_text) %>% unlist() %>% unique() %>% sort()
  
  plot_df <- tmp[cases,] %>% 
    gather() %>%
    mutate(key = factor(key, levels = c("MoralityGeneral", "MFTcombined"
                                        , "Authority", "Ingroup", "Purity"
                                        , "Fairness", "Harm"))) %>%
    group_by(key) %>%
    summarise(mean = mean(value), sd = sd(value), n = n()) %>%
    mutate(se = sd/sqrt(n), cilo = mean - se, cihi = mean + se)
  
  ggplot(plot_df, aes(x=key, y=mean, ymin=cilo, ymax=cihi)) + ggtitle(x) +
    geom_point() + geom_errorbar(width=0) + geom_hline(yintercept = 0) + theme_bw()
}

checkTerm("politic")
checkTerm("liberal")
checkTerm("democrat")
checkTerm("conservative")
checkTerm("republican")
checkTerm("climate")
checkTerm("global warming")


