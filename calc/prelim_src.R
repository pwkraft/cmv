library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quanteda)

## load data
rm(list=ls())
load("out/cmv_data.Rdata")

## TODO: clean original posts (links etc)!
## TODO: adjust confidence intervals to correct for multiple comparisons
## TODO: add more comments in functions!

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



#####################
### Examine pair data
#####################

mft_pair <- countMFT(data_pair$pos_text, dict) - countMFT(data_pair$neg_text, dict)

plot_df <- mft_pair %>% 
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

checkTerm <- function(x, return_data = FALSE){
  cases <- c(map(x, grep, data_pair$op_text), map(x, grep, data_pair$op_title)) %>% 
               unlist() %>% unique() %>% sort()
  
  plot_df <- mft_pair[cases,] %>% 
    gather() %>%
    mutate(key = factor(key, levels = rev(c("Authority", "Ingroup", "Purity", "Fairness", "Harm"
                                            , "MFTcombined", "MoralityGeneral"))
                        , labels = rev(c("Authority", "Ingroup", "Purity", "Fairness", "Harm"
                                         , "Foundations\nCombined", "General\nMorality")))) %>%
    group_by(key) %>%
    summarise(mean = mean(value), sd = sd(value), n = n()) %>%
    mutate(se = sd/sqrt(n), cilo = mean - se, cihi = mean + se)
  
  if(return_data) return(plot_df)
  
  xlim <- max(abs(c(plot_df$cilo,plot_df$cihi)))
  xlim <- c(-xlim,xlim)
  
  ggplot(plot_df, aes(y=key, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = paste0("Search Query (",length(cases)," hits):"), subtitle = paste(x, collapse="; ")
         , x = NULL, y = "Moral Foundation") +
    scale_x_continuous(limits=xlim, breaks=.9*xlim, labels = c("Less Persuasive", "More Persuasive")) +
    theme(axis.ticks.x=element_blank())
}

checkTerm("")
checkTerm("politic")
checkTerm("politic", T)
checkTerm(c("liberal","conservati"))
checkTerm(c("democrat","republican"))
checkTerm("climate")
checkTerm("climate change")
checkTerm(c("climate change","global warming"))
checkTerm(c("politic","liberal","conservati","democrat","republican","president"
            ,"climate change","global warming"))

mftTerms <- function(x){
  map(x, grep, names(dict)) %>% 
    unlist() %>% unique %>% sort() %>%
    map(~gsub("*","", dict[[.]], fixed=T)) %>%
    unlist()
}

checkTerm(mftTerms("HarmVirtue"))
checkTerm(mftTerms("HarmVice"))
checkTerm(mftTerms(c("HarmVice","HarmVirtue")))
checkTerm(mftTerms("Harm"))
checkTerm(mftTerms("FairnessVirtue"))
checkTerm(mftTerms("FairnessVice"))
checkTerm(mftTerms("Purity"))

checkTerm(mftTerms("Vice"))
checkTerm(mftTerms("Virtue"))



###################
### Examine OP data
###################

mft_op <- data_op[,c("title","text")] %>% 
  apply(1, paste, collapse=" ") %>%
  countMFT(dict) %>% bind_cols(data_op) %>% as_tibble()

plot_df <- mft_op %>%
  select(-name:-text, -MFTcombined) %>%
  gather(key=foundation, value=proportion,-Delta) %>%
  mutate(foundation = factor(foundation, levels = rev(c("Authority", "Ingroup", "Purity"
                                                        , "Fairness", "Harm", "MoralityGeneral"))
                             , labels = rev(c("Authority", "Ingroup", "Purity"
                                              , "Fairness", "Harm", "General\nMorality")))) %>%
  group_by(foundation,Delta) %>%
  summarise(mean = mean(proportion), sd = sd(proportion), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - se, cihi = mean + se)

ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=Delta, shape=Delta)) + 
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + 
  geom_point(position = position_nudge(y=-.1+.2*plot_df$Delta)) + 
  geom_errorbarh(height=0, position = position_nudge(y=-.1+.2*plot_df$Delta)) + 
  ylab("Moral Foundation") + xlab("Proportion of Dictionary Terms") +
  ggtitle("Moral Foundations and Persuadability")
