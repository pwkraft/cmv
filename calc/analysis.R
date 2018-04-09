library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quanteda)
library(topicmodels)

## load data
load("out/cmv_data.Rdata")

## load dictionary
dict <- dictionary(file="in/moral foundations dictionary.dic", format="LIWC")

## load auxiliary functions
source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))



#######################################
### Moral reasoning and persuadability
#######################################

### Compute percentage of moral word counts in original posts
mft_op <- data_op[,c("title","text")] %>% 
  apply(1, paste, collapse=" ") %>%
  countMFT(dict) %>% bind_cols(data_op) %>% as_tibble()

### Prepare data for plotting
plot_df <- mft_op %>%
  select(-name:-text) %>%
  gather(key=foundation, value=proportion,-Delta) %>%
  mutate(foundation = factor(foundation, levels = rev(c("Care", "Fairness", "Loyalty",
                                                        "Authority", "Sanctity", "General"))),
         Change = factor(Delta, labels=c("No Opinion Change","Opinion Change"))) %>%
  group_by(foundation, Delta, Change) %>%
  summarise(mean = mean(proportion), sd = sd(proportion), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - 1.96 * se, cihi = mean + 1.96 * se)

### Creat plot
ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=Change, shape=Change)) + 
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + 
  geom_point(position = position_nudge(y=.1-.2*plot_df$Delta)) + 
  geom_errorbarh(height=0, position = position_nudge(y=.1-.2*plot_df$Delta)) + 
  ylab("Moral Foundation") + xlab("Percentage of Dictionary Terms") +
  ggtitle("Moral Foundations and Persuadability") + theme(legend.title = element_blank())
ggsave("fig/persuadability.pdf", height=2.5, width=6)


### Examine pair data

mft_pair <- countMFT(data_pair$pos_text, dict) - countMFT(data_pair$neg_text, dict)

checkTerm <- function(x, return_data = FALSE){
  cases <- c(map(x, grep, data_pair$op_text), map(x, grep, data_pair$op_title)) %>% 
               unlist() %>% unique() %>% sort()
  
  plot_df <- mft_pair[cases,] %>% 
    gather(key=foundation, value=proportion) %>%
    mutate(valence = paste0("V",gsub(".*V","", foundation))
           , foundation = factor(gsub("V.*","", foundation)
                                 , levels = rev(c("Harm", "Fairness", "Ingroup", "Authority", "Purity"))
                                 , labels = rev(c("Care", "Fairness", "Loyalty", "Authority", "Sanctity")))) %>%
    group_by(foundation, valence) %>%
    summarise(mean = mean(proportion), sd = sd(proportion), n = n()) %>%
    mutate(se = sd/sqrt(n), cilo = mean - 1.96 * se, cihi = mean + 1.96 * se
           , numcases = length(cases))
  
  if(return_data) return(plot_df)
  
  xlim <- max(abs(c(plot_df$cilo,plot_df$cihi)))
  xlim <- c(-xlim,xlim)
  
  ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = paste0("Search Query (",length(cases)," argument pairs):"), subtitle = paste(x, collapse="; ")
         , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)", y = "Moral Foundation") +
    #scale_x_continuous(limits=xlim, breaks=.9*xlim, labels = c("Less Persuasive", "More Persuasive")) +
    #theme(axis.ticks.x=element_blank()) +
    scale_x_continuous(limits=xlim) + facet_grid(valence~.)
    
}

mftTerms <- function(x){
  map(x, grep, names(dict)) %>% 
    unlist() %>% unique %>% sort() %>%
    map(~gsub("*","", dict[[.]], fixed=T)) %>%
    unlist()
}

## ----check all discussion pairs, fig.cap="All argument pairs: Average difference in percentage of dictionary terms relative to the total number of words between matched discussion contributions that persuaded the author of the original post vs. not (including 95% confidence intervals). Negative values indicate that arguments were less persuasive (and vice versa)."----
checkTerm("") + labs(title = paste0("All Argument Pairs (n = ",nrow(mft_pair),")")
                     , subtitle = NULL, y = "Moral Foundation"
                     , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)")

## ----prepare foundations in pair data------------------------------------
mftCombine <- function(x){
  out <- checkTerm(mftTerms(x), return_data = T) %>%
    mutate(op_valence = paste0("V",gsub(".*V","", x))
           , op_foundation = gsub("V.*","", x))
  out
}

plot_df <- names(dict)[-11] %>% 
  map_dfr(mftCombine)

## ----op harm, fig.cap="Persuasiveness of moral arguments in response to original posts that mention the care foundation: Average difference in percentage of dictionary terms relative to the total number of words between matched discussion contributions that persuaded the author of the original post vs. not (including 95% confidence intervals). Negative values indicate that arguments were less persuasive (and vice versa)."----

plot_df %>% filter(op_foundation == "Harm") %>%
  ggplot(aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = "Moral Foundation of Original Post: Care/Harm"
         , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)"
         , y = "Moral Foundation of Counterargument") +
    facet_grid(valence~op_valence)

## ----op fairness, fig.cap="Persuasiveness of moral arguments in response to original posts that mention the fairness foundation: Average difference in percentage of dictionary terms relative to the total number of words between matched discussion contributions that persuaded the author of the original post vs. not (including 95% confidence intervals). Negative values indicate that arguments were less persuasive (and vice versa)."----

plot_df %>% filter(op_foundation == "Fairness") %>%
  ggplot(aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = "Moral Foundation of Original Post: Fairness/Cheating"
         , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)"
         , y = "Moral Foundation of Counterargument") +
    facet_grid(valence~op_valence)

## ----op ingroup, fig.cap="Persuasiveness of moral arguments in response to original posts that mention the loyalty foundation: Average difference in percentage of dictionary terms relative to the total number of words between matched discussion contributions that persuaded the author of the original post vs. not (including 95% confidence intervals). Negative values indicate that arguments were less persuasive (and vice versa)."----

plot_df %>% filter(op_foundation == "Ingroup") %>%
  ggplot(aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = "Moral Foundation of Original Post: Loyalty/Betrayal"
         , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)"
         , y = "Moral Foundation of Counterargument") +
    facet_grid(valence~op_valence)

## ----op authority, fig.cap="Persuasiveness of moral arguments in response to original posts that mention the authority foundation: Average difference in percentage of dictionary terms relative to the total number of words between matched discussion contributions that persuaded the author of the original post vs. not (including 95% confidence intervals). Negative values indicate that arguments were less persuasive (and vice versa)."----

plot_df %>% filter(op_foundation == "Authority") %>%
  ggplot(aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = "Moral Foundation of Original Post: Authority/Subversion"
         , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)"
         , y = "Moral Foundation of Counterargument") +
    facet_grid(valence~op_valence)

## ----op purity, fig.cap="Persuasiveness of moral arguments in response to original posts that mention the sanctity foundation: Average difference in percentage of dictionary terms relative to the total number of words between matched discussion contributions that persuaded the author of the original post vs. not (including 95% confidence intervals). Negative values indicate that arguments were less persuasive (and vice versa)."----

plot_df %>% filter(op_foundation == "Purity") %>%
  ggplot(aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
    theme_classic() + theme(panel.border = element_rect(fill=NA)) +
    geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
    labs(title = "Moral Foundation of Original Post: Sanctity/Degradation"
         , x = "Difference in Percentage of Dictionary Terms (Delta - No Delta)"
         , y = "Moral Foundation of Counterargument") +
    facet_grid(valence~op_valence)

## ----climate change------------------------------------------------------
checkTerm(c("climate change","global warming","temperature","extreme weather"))

## ----abortion------------------------------------------------------------
checkTerm(c("pro-life","prolife","pro life","pro-choice","prochoice","pro choice"))

## ----politics------------------------------------------------------------
checkTerm(c("politic","liberal","conservati","democrat","republican","president","senate","congress","representative","donat","obama","mccain","romney","clinton","bush"))

## ----test----------------------------------------------------------------
cosine <- function(x,y){
  x %*% y / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

mft_pos <- as.matrix(countMFT(data_pair$pos_text, dict))
mft_neg <- as.matrix(countMFT(data_pair$neg_text, dict))
mft_op <- as.matrix(countMFT(data_pair$op_text, dict))

test_pos <- rep(NA, nrow(mft_pos))
test_neg <- rep(NA, nrow(mft_neg))
for(i in 1:length(test_pos)){
  test_pos[i] <- cosine(mft_pos[i,], mft_op[i,])
  test_neg[i] <- cosine(mft_neg[i,], mft_op[i,])
}

t.test(test_pos, test_neg, paired = T)

test_pos[is.na(test_pos)] <- 0
test_neg[is.na(test_neg)] <- 0

t.test(test_pos, test_neg, paired = T)

mean(test_pos)
mean(test_neg)

