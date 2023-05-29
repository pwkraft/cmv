library(jsonlite)
library(tidyverse)
library(quanteda)
library(tidytext)

rm(list=ls())

## load dictionary
dict <- dictionary(file="~/Dropbox/Uni/projects/2017/cmv/calc/in/moral foundations dictionary.dic", format="LIWC")

## Function to compute percentage of moral word counts
countMFT <- function(x, dic){
  corpus <- corpus(x)
  dat <- dfm(corpus, dictionary = dic) %>%
    convert(to = "data.frame") %>%
    transmute(Care = HarmVirtue + HarmVice,
              Fairness = FairnessVirtue + FairnessVice,
              Loyalty = IngroupVirtue + IngroupVice,
              Authority = AuthorityVirtue + AuthorityVice,
              Sanctity = PurityVirtue + PurityVice,
              General = MoralityGeneral)
  dat/ntoken(corpus) * 100
}



############################################
### Paired data (only look at training set!)
############################################

## load paired data
raw_pair <- stream_in(file("~/Dropbox/Uni/Data/CMV/pair_task/train_pair_data.jsonlist"))

raw_pair$positive$comments[1][[1]]$body



########################################
### OP data (only look at training set!)
########################################

## load op data
raw_op <- stream_in(file("~/Dropbox/Uni/Data/CMV/op_task/train_op_data.jsonlist"))

tmp <- countMFT(raw_op$selftext, dic = dict)

tibble(tmp)
