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



#####################################
### Overview: General persuadability
#####################################

data_op %>% 
  mutate(Change = factor(Delta, labels = c("No", "Yes"))) %>%
  ggplot(aes(x=Change)) + ylim(0, nrow(data_op)) +
  labs(y="Number of Original Posts", x="Opinion Change") +
  geom_bar() + plot_default
ggsave("fig/delta.pdf", width = 2.5, height = 3)



#######################################
### Moral reasoning and persuadability
#######################################

## Compute percentage of moral word counts in original posts
mft_op <- data_op[,c("title","text")] %>% 
  apply(1, paste, collapse=" ") %>%
  countMFT(dict) %>% bind_cols(data_op) %>% as_tibble()

## Prepare data for plotting
plot_df <- mft_op %>%
  select(-name:-text) %>%
  gather(key=foundation, value=proportion,-Delta) %>%
  mutate(foundation = factor(foundation, levels = rev(c("Care", "Fairness", "Loyalty",
                                                        "Authority", "Sanctity", "General"))),
         Change = factor(Delta, labels=c("No Opinion Change","Opinion Change"))) %>%
  group_by(foundation, Delta, Change) %>%
  summarise(mean = mean(proportion), sd = sd(proportion), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - 1.96 * se, cihi = mean + 1.96 * se)

## Create plot
ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=Change, shape=Change)) + 
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + 
  geom_point(position = position_nudge(y=.1-.2*plot_df$Delta)) + 
  geom_errorbarh(height=0, position = position_nudge(y=.1-.2*plot_df$Delta)) + 
  ylab("Moral Foundation") + xlab("Percentage of Dictionary Terms") +
  ggtitle("Moral Foundations and Persuadability") + theme(legend.title = element_blank())
ggsave("fig/persuadability.pdf", height=2.5, width=6)

## Test differences for each foundation (w/ Bonferroni correction)
mft_op %>% 
  gather(key = Foundation, value = Percentage, -name:-Delta) %>%
  split(.$Foundation) %>%
  map(~t.test(.$Percentage~.$Delta)) %>%
  map_df("p.value") %>%
  map_df(p.adjust, method = "bonferroni", n = 6)

## Test difference across all terms
t.test(apply(mft_op[,1:6],1,sum)~mft_op$Delta)



###########################################
### Moral consistency b/w OP and arguments
###########################################

## function to compute cosine similarity
cosine <- function(x,y){
  x %*% y / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

## create matrices of dictionary counts
mft_pos <- as.matrix(countMFT(data_pair$pos_text, dict))
mft_neg <- as.matrix(countMFT(data_pair$neg_text, dict))
mft_op <- as.matrix(countMFT(paste(data_pair$op_title, data_pair$op_text), dict))

## compute cosine similarities
cos_pos <- rep(NA, nrow(mft_pos))
cos_neg <- rep(NA, nrow(mft_neg))
for(i in 1:length(cos_pos)){
  cos_pos[i] <- cosine(mft_pos[i,], mft_op[i,])
  cos_neg[i] <- cosine(mft_neg[i,], mft_op[i,])
}

## test differences in cosine similarities
# (excluding all pairs w/o moral terms in original post OR reply)
t.test(cos_pos, cos_neg, paired = T)
mean(cos_pos, na.rm = T) - mean(cos_neg, na.rm = T)

t.test(cos_pos-cos_neg)

## plot densities
plot(density(cos_pos - cos_neg, na.rm = T))
abline(v=mean_cl_normal(cos_pos - cos_neg))

## test differences in cosine similarities
# (including all pairs w/o moral temrs in original post OR reply as zero)
cos_pos_zero <- cos_pos
cos_pos_zero[is.na(cos_pos_zero)] <- 0
cos_neg_zero <- cos_neg
cos_neg_zero[is.na(cos_neg_zero)] <- 0
t.test(cos_pos_zero, cos_neg_zero, paired = T)

## plot densities
plot(density(cos_pos_zero - cos_neg_zero))
abline(v=mean_cl_normal(cos_pos_zero - cos_neg_zero))

## plot differences in violin plot
tibble(cos_neg = cos_neg_zero,
       cos_pos = cos_pos_zero) %>%
  gather(key = Delta, value = Similarity) %>%
  ggplot(aes(x = Delta, y=Similarity)) + geom_violin() + plot_default +
  stat_summary(fun.data=mean_cl_normal, 
               geom="linerange", color="red")

## proportion of cases that that do not included moral terms in original post
mft_op_0 <- apply(mft_op,1,sum) == 0

t.test(apply((mft_pos - mft_neg)[mft_op_0,],1,sum))
t.test(apply((mft_pos)[mft_op_0,],1,sum),
       apply((mft_neg)[mft_op_0,],1,sum), paired = T)



################################
### Emphasis on virtue vs. vice
################################

## check virtue vs. vice count
virt_pos <- countVirtue(data_pair$pos_text, dict)
virt_neg <- countVirtue(data_pair$neg_text, dict)
virt_op <- countVirtue(paste(data_pair$op_title, data_pair$op_text), dict)

t.test(virt_pos$Virtue, virt_neg$Virtue, paired = T)
t.test(virt_pos$Vice, virt_neg$Vice, paired = T)
t.test(virt_pos$VVDiff, virt_neg$VVDiff, paired = T)

t.test(virt_pos$Virtue + virt_pos$Vice, virt_neg$Virtue + virt_neg$Vice, paired = T)

## total number of moral terms does not have an effect! -> against moral conviction literature


####################################
### Word Count by positive/negative
####################################

