library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quanteda)
library(topicmodels)
library(lmtest)
library(sandwich)
library(xtable)


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
  labs(y="Number of Discussion", x="Opinion Change") +
  geom_bar() + plot_default
ggsave("fig/delta.pdf", width = 2.5, height = 2)



#######################################
### Moral reasoning and persuadability
#######################################

## Compute percentage of moral word counts in original posts
mft_op <- data_op[,c("title","text")] %>% 
  apply(1, paste, collapse=" ") %>%
  countMFT(dict) %>% bind_cols(data_op) %>% as_tibble()

## Prepare data for plotting
plot_df <- mft_op %>%
  select(Care:General, Delta) %>%
  gather(key=foundation, value=proportion,-Delta) %>%
  mutate(foundation = factor(foundation, levels = rev(c("Care", "Fairness", "Loyalty",
                                                        "Authority", "Sanctity", "General"))),
         Change = factor(Delta, labels=c("No Opinion Change","Opinion Change"))) %>%
  group_by(foundation, Delta, Change) %>%
  summarise(mean = mean(proportion), sd = sd(proportion), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - 1.96 * se, cihi = mean + 1.96 * se)

## Create plot
ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=Change, shape=Change)) + 
  geom_point(position = position_nudge(y=.1-.2*plot_df$Delta)) + plot_default +
  geom_errorbarh(height=0, position = position_nudge(y=.1-.2*plot_df$Delta)) + 
  ylab("Moral Foundation") + xlab("Percentage of Dictionary Terms") +
  theme(legend.title = element_blank())
ggsave("fig/persuadability.pdf", height=2.5, width=6)

## Test differences for each foundation (w/ Bonferroni correction)
mft_op %>% select(Care:General, Delta) %>%
  gather(key = Foundation, value = Percentage, -Delta) %>%
  split(.$Foundation) %>%
  map(~t.test(.$Percentage~.$Delta)) %>%
  map_df("p.value") %>%
  map_df(p.adjust, method = "bonferroni", n = 6)

## Test difference across all terms
t.test(apply(mft_op[,1:6],1,sum)~mft_op$Delta)



##########################
### Prepare response data
##########################

## Compute mft percentages for full response path
mft_op_text <- countMFT(paste(data_pair$op_title, data_pair$op_text), dict)
mft_pos_text <- countMFT(data_pair$pos_text, dict)
mft_neg_text <- countMFT(data_pair$neg_text, dict)

## Compute mft percentages for root response
mft_pos_root <- countMFT(data_pair$pos_root, dict)
mft_neg_root <- countMFT(data_pair$neg_root, dict)

## Compute mft percentages for truncated root response
mft_pos_trunc <- countMFT(data_pair$pos_trunc, dict)
mft_neg_trunc <- countMFT(data_pair$neg_trunc, dict)



#######################################
### Response length and persuasiveness
#######################################

## Plot differences
tibble(text_nterm = data_pair$pos_text_nterm - data_pair$neg_text_nterm,
       root_nterm = data_pair$pos_root_nterm - data_pair$neg_root_nterm) %>%
  gather(key = group, value = nterm) %>% group_by(group) %>%
  summarize_all(funs(avg = mean, sd=sd, n=n())) %>%
  mutate(se = sd/sqrt(n),
         cilo = avg - 1.96 * se,
         cihi = avg + 1.96 * se,
         glabel = factor(group, labels=c("Root Response","Full Response Path"))) %>%
  ggplot(aes(x = avg, xmin = cilo, xmax = cihi, y = glabel)) +
  geom_point() + geom_errorbarh(height=0) + 
  geom_vline(xintercept = 0, col = "grey") + plot_default +
  labs(y = NULL, x="Difference in Word Count\n(Change - No Change)")
ggsave("fig/wordcount.pdf", height=1.5, width=3)

tibble(text_nterm = data_pair$pos_text_nterm - data_pair$neg_text_nterm,
       root_nterm = data_pair$pos_root_nterm - data_pair$neg_root_nterm) %>%
  gather(key = group, value = nterm) %>%
  mutate(glabel = factor(group, labels=c("Root\nResponse","Full\nResponse\nPath"))) %>%
  ggplot(aes(y=nterm, x=glabel, fill=glabel)) + 
  geom_hline(yintercept = 0, col="grey") +
  geom_violin(alpha = .4) + plot_default +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width = .2) +
  labs(y="Difference in Word Count\n(Change - No Change)", x=NULL) + 
  theme(legend.position="none") + coord_flip()
ggsave("fig/wordcount_violin.pdf", width = 6.5, height = 2)

## testing differences (note: t-test of difference is equivalent of paired test)
t.test(data_pair$pos_text_nterm, data_pair$neg_text_nterm, paired =T)
t.test(data_pair$pos_text_nterm - data_pair$neg_text_nterm)
t.test(data_pair$pos_root_nterm-data_pair$neg_root_nterm)



#######################################
### proportion of cases that that do not included moral terms in original post
#######################################

tibble(mft = factor(apply(mft_op_text, 1, sum) != 0,
                    labels = c("No", "Yes"))) %>%
  ggplot(aes(x=mft)) + ylim(0, nrow(mft_op_text)) +
  labs(y="Number of Discussions", x="Any MFT Term Mentioned") +
  geom_bar() + plot_default
ggsave("fig/mft_op_all.pdf", width = 2.5, height = 2)



#######################################
### Moral reasoning in selected original posts
#######################################

## Prepare data for plotting
plot_df <- mft_op_text %>%
  gather(key=foundation, value=proportion) %>%
  mutate(foundation = factor(foundation, levels = rev(c("Care", "Fairness", "Loyalty",
                                                        "Authority", "Sanctity", "General")))) %>%
  group_by(foundation) %>%
  summarise(mean = mean(proportion), sd = sd(proportion), n = n()) %>%
  mutate(se = sd/sqrt(n), cilo = mean - 1.96 * se, cihi = mean + 1.96 * se)

## Create plot
ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi)) + 
  geom_point() + plot_default + geom_errorbarh(height=0) + 
  ylab("Moral Foundation") + xlab("Percentage of Dictionary Terms") +
  theme(legend.title = element_blank())
ggsave("fig/mft_op_individual.pdf", height=2.5, width=4)



###########################
### Difference in moral reasoning between positive and negative arguments
###########################

## Compute differences for individual foundations
mft_diff <- mutate(mft_pos_text - mft_neg_text, type="1. text") %>%
  bind_rows(mutate(mft_pos_root - mft_neg_root, type="2. root")) %>%
  bind_rows(mutate(mft_pos_trunc - mft_neg_trunc, type="3. trunc"))

## Prepare data for plotting
plot_df <- mft_diff %>%
  select(Care:General, type) %>%
  gather(key=foundation, value=proportion, -type) %>%
  mutate(foundation = factor(foundation, levels = rev(c("Care", "Fairness", "Loyalty",
                                                        "Authority", "Sanctity", "General"))),
         type = factor(type, labels=c("Full Response Path", "Root Response",
                                      "Truncated Root Response"))) %>%
  group_by(foundation, type) %>%
  summarise(mean = mean(proportion), sd = sd(proportion), n = length(na.omit(proportion))) %>%
  mutate(se = sd/sqrt(n), cilo = mean - 1.96 * se, cihi = mean + 1.96 * se)

## Create plot
ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=type, shape=type)) + 
  geom_vline(xintercept = 0, col="grey") + plot_default +
  geom_point(position = position_nudge(y=.2-(as.numeric(plot_df$type)-1)/5)) + 
  geom_errorbarh(height=0, position = position_nudge(y=.2-(as.numeric(plot_df$type)-1)/5)) + 
  ylab("Moral Foundation") + xlab("Difference in MFT Percentages (Change - No Change)") +
  theme(legend.title = element_blank())
ggsave("fig/persuasiveness.pdf", height=2.5, width=6)

ggplot(plot_df, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=type, shape=type)) + 
  geom_vline(xintercept = 0, col="grey") + plot_empty +
  geom_point(position = position_nudge(y=.2-(as.numeric(plot_df$type)-1)/5)) + 
  geom_errorbarh(height=0, position = position_nudge(y=.2-(as.numeric(plot_df$type)-1)/5)) + 
  ylab("Moral Foundation") + xlab("Difference in MFT Percentages (Change - No Change)") +
  theme(legend.title = element_blank())
ggsave("fig/persuasiveness_empty.pdf", height=2.5, width=6)


## Test differences on individual foundations
mft_diff %>% filter(type=="text") %>%
  gather(key = Foundation, value = Proportion, -type) %>%
  split(.$Foundation) %>% map(~t.test(.$Proportion))

mft_diff %>% filter(type=="root") %>%
  gather(key = Foundation, value = Proportion, -type) %>%
  split(.$Foundation) %>% map(~t.test(.$Proportion))

mft_diff %>% filter(type=="trunc") %>%
  gather(key = Foundation, value = Proportion, -type) %>%
  split(.$Foundation) %>% map(~t.test(.$Proportion))

## Test differences on all foundations combined
mft_diff %>% 
  select(-type) %>%
  apply(1, sum) %>%
  split(mft_diff$type) %>% 
  map(t.test)



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
# the lack of effects persists for Virtue vs. Vice as well


###########################################
### Moral consistency b/w OP and arguments
###########################################

## function to compute cosine similarity
cosine <- function(x,y, replaceNA = T){
  x <- as.numeric(x)
  y <- as.numeric(y)
  out <- x %*% y / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
  if(is.na(out) & replaceNA) out <- 0
  as.numeric(out)
}

## compute differences in cosine similarities (Change - No Change)
cos_res <- tibble(
  '1. text' = rep(NA, nrow(data_pair)),
  '2. root' = NA,
  '3. trunc' = NA
)
for(i in 1:nrow(data_pair)){
  cos_res[i,'1. text'] <- (cosine(mft_pos_text[i,], mft_op_text[i,], T)
                           - cosine(mft_neg_text[i,], mft_op_text[i,], T))
  cos_res[i,'2. root'] <- (cosine(mft_pos_root[i,], mft_op_text[i,], T)
                           - cosine(mft_neg_root[i,], mft_op_text[i,], T))
  cos_res[i,'3. trunc'] <- (cosine(mft_pos_trunc[i,], mft_op_text[i,], T)
                            - cosine(mft_neg_trunc[i,], mft_op_text[i,], T))
}

## Prepare data for plotting
plot_df <- cos_res %>%
  gather(key = type, value = cos) %>%
  mutate(type = factor(type, labels=c("Full Response Path", "Root Response",
                                      "Truncated Root Response"))) %>% 
  group_by(type) %>%
  summarize(avg = mean(cos, na.rm = T), 
            sd = sd(cos, na.rm = T),
            n = sum(!is.na(cos)),
            se = sd/sqrt(n),
            cilo = avg - 1.96 * se, 
            cihi = avg + 1.96 * se)

## Create plot
ggplot(plot_df, aes(x=avg, xmin=cilo, xmax=cihi, y=reorder(type, 3:1), col = type, shape = type)) + 
  geom_vline(xintercept = 0, col = "grey") + geom_point() + geom_errorbarh(height = 0) + 
  plot_default + labs(y = NULL, x = "Difference in MFT Congruence\n(Change - No Change)") +
  theme(legend.position="none")
ggsave("fig/cosine.pdf", width = 4, height = 2)

ggplot(plot_df, aes(x=avg, xmin=cilo, xmax=cihi, y=reorder(type, 3:1), col = type, shape = type)) + 
  geom_vline(xintercept = 0, col = "grey") + geom_point() + geom_errorbarh(height = 0) + 
  plot_empty + labs(y = NULL, x = "Difference in MFT Congruence\n(Change - No Change)") +
  theme(legend.position="none")
ggsave("fig/cosine_empty.pdf", width = 4, height = 2)

## test differences in cosine similarities
t.test(cos_res$`1. text`)
t.test(cos_res$`2. root`)
t.test(cos_res$`3. trunc`)

## plot densities
tmp <- rbind(mean_cl_normal(cos_res$`1. text`),
             mean_cl_normal(cos_res$`2. root`),
             mean_cl_normal(cos_res$`3. trunc`))
tmp$type = factor(colnames(cos_res), labels=c("Full Response Path", "Root Response",
                                              "Truncated Root Response"))

cos_res %>% gather(key = type, value = cos) %>%
  mutate(type = factor(type, labels=c("Full Response Path", "Root Response",
                                      "Truncated Root Response"))) %>%
  ggplot(aes(x=cos, fill = type)) + 
  geom_density(alpha = .2) + plot_default + 
  geom_vline(aes(xintercept = y, col = type), data = tmp) +
  geom_vline(aes(xintercept = ymin, col = type), lty="dashed", data = tmp) +
  geom_vline(aes(xintercept = ymax, col = type), lty="dashed", data = tmp) +
  facet_wrap(~type, ncol = 1, scale = "free_x") + ylab("Density") + 
  xlab("Difference in MFT Congruence\n(Change - No Change)") +
  theme(legend.position="none")
ggsave("fig/cosine_density.pdf", width = 6, height = 5)

## plot differences in violin plot
cos_res %>% gather(key = type, value = cos) %>%
  mutate(type = factor(type, labels=c("Full Response Path", "Root Response",
                                      "Truncated Root Response"))) %>%
  ggplot(aes(y=cos, x=type, fill=type)) + 
  geom_hline(yintercept = 0, col="grey") +
  geom_violin(alpha = .4) + plot_default +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width = .2) +
  labs(y="Difference in MFT Congruence\n(Change - No Change)", x=NULL) + 
  theme(legend.position="none")
ggsave("fig/cosine_violin.pdf", width = 6, height = 5)



###########################################
### Effect of Moral consistency on persuasiveness (-> Michael)
###########################################

## compute absolute cosine similarities (Change - No Change)
cos_abs <- tibble(
  'caseid' = rep(1:nrow(data_pair), 2),
  'opid' = rep(as.numeric(as.factor(data_pair$op_text_raw)), 2),
  'delta' = rep(c(0,1), each=nrow(data_pair)),
  'text' = NA, 'root' = NA, 'trunc' = NA
)

for(i in 1:nrow(data_pair)){
  cos_abs[i,'text'] <- cosine(mft_neg_text[i,], mft_op_text[i,], T)
  cos_abs[i,'root'] <- cosine(mft_neg_root[i,], mft_op_text[i,], T)
  cos_abs[i,'trunc'] <- cosine(mft_neg_trunc[i,], mft_op_text[i,], T)
  cos_abs[i+nrow(data_pair),'text'] <- cosine(mft_pos_text[i,], mft_op_text[i,], T)
  cos_abs[i+nrow(data_pair),'root'] <- cosine(mft_pos_root[i,], mft_op_text[i,], T)
  cos_abs[i+nrow(data_pair),'trunc'] <- cosine(mft_pos_trunc[i,], mft_op_text[i,], T)
}

## estimate basic logit without any clustering
m1 <- glm(delta ~ text, data = cos_abs, family = binomial("logit"))
summary(m1)

## test clustered SEs
coeftest(m1, vcov = vcovCL(m1, cluster=cos_abs$caseid))
coeftest(m1, vcov = vcovCL(m1, cluster=cos_abs$opid)) # by opid makes most sense
coeftest(m1, vcov = vcovCL(m1, cluster=cos_abs$delta))

## remove duplicated negative posts
table(duplicated(data_pair$pos_text_raw))
table(duplicated(data_pair$neg_text_raw))
neg_unique <- !duplicated(data_pair$neg_text_raw)
cos_red <- cos_abs[c(neg_unique,neg_unique),]

## estimate logits w/ reduced data
m2 <- NULL
m2[[1]] <- glm(delta ~ text, data = cos_red, family = binomial("logit"))
m2[[2]] <- glm(delta ~ root, data = cos_red, family = binomial("logit"))
m2[[3]] <- glm(delta ~ trunc, data = cos_red, family = binomial("logit"))
lapply(m2, summary)
lapply(m2, function(x) coeftest(x, vcov = vcovCL(x, cluster=cos_red$opid)))

## function to compute range of x values for sim
minmax <- function(x, k=2){
  seq(min(x),max(x),length=k)
}

## simulate predicted probabilities
res <- rbind(sim(m2[[1]], iv=data.frame(text=minmax(cos_red$text)), cluster = cos_red$opid),
             sim(m2[[2]], iv=data.frame(root=minmax(cos_red$root)), cluster = cos_red$opid),
             sim(m2[[3]], iv=data.frame(trunc=minmax(cos_red$trunc)), cluster = cos_red$opid))
res$ivlab <- factor(res$iv, labels = c("Full Response Path","Root Response","Truncated Root Response"))

ggplot(res, aes(x=mean, xmin=cilo, xmax=cihi, y=reorder(ivlab, 3:1), col = ivlab, shape = ivlab)) + 
  geom_vline(xintercept = 0, col = "grey") + geom_point() + geom_errorbarh(height = 0) + 
  plot_default + labs(y = NULL, x = "Change in P(Opinion Change)") +
  theme(legend.position="none")
ggsave("fig/logit_cosine.pdf", width = 4, height = 2)

## adjust name of independent vars
names(m2[[1]]$coefficients)[2] <- "cosine"
names(m2[[2]]$coefficients)[2] <- "cosine"
names(m2[[3]]$coefficients)[2] <- "cosine"

## table for appendix
latexTable(m2, cluster = cos_red$opid, caption=c("Logit models predicting argument persuasiveness as a 
            function of moral congruence with OPs' opening statements (measured via cosine similarity in 
            MFT dictionary results). Positive coefficients indicate higher probability of changing the OPs'
            mind (i.e., receiving a delta). 
            Standard errors (clustered by discussion thread) in parentheses. Estimates are used for Figure
            \\ref{fig:cosine} in the main text.", "Logit models predicting argument persuasiveness as a 
            function of moral congruence with OPs' opening statements")
           , label="tab:cosine", align="lccc"
           , varlabs=list("cosine"="Moral Congruence","(Intercept)"="Intercept")
           , mlabs=c("Full Response Path","Root Response","Truncated Root Response")
           , file="tab/cosine.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")



###########################
### Effect of moral arguments on persuasiveness (-> Michael)
###########################

## prepare reduced data (remove duplicated negative posts)
mft_text <- bind_rows(mutate(mft_neg_text, delta = 0, type = "1. text"),
                      mutate(mft_pos_text, delta = 1, type = "1. text")) %>%
  mutate(opid = rep(as.numeric(as.factor(data_pair$op_text_raw)), 2)) %>%
  filter(c(neg_unique,neg_unique))
mft_root <- bind_rows(mutate(mft_neg_root, delta = 0, type = "2. root"),
                      mutate(mft_pos_root, delta = 1, type = "2. root")) %>%
  mutate(opid = rep(as.numeric(as.factor(data_pair$op_text_raw)), 2)) %>%
  filter(c(neg_unique,neg_unique))
mft_trunc <- bind_rows(mutate(mft_neg_trunc, delta = 0, type = "3. trunc"),
                       mutate(mft_pos_trunc, delta = 1, type = "3. trunc")) %>%
  mutate(opid = rep(as.numeric(as.factor(data_pair$op_text_raw)), 2)) %>%
  filter(c(neg_unique,neg_unique))

## estimate logits w/ reduced data
m3 <- NULL
m3[[1]] <- glm(delta ~ Care + Fairness + Loyalty + Authority + Sanctity + General, 
               data = mft_text, family = binomial("logit"))
m3[[2]] <- glm(delta ~ Care + Fairness + Loyalty + Authority + Sanctity + General, 
               data = mft_root, family = binomial("logit"))
m3[[3]] <- glm(delta ~ Care + Fairness + Loyalty + Authority + Sanctity + General, 
               data = mft_trunc, family = binomial("logit"))
lapply(m3, summary)
lapply(m3, function(x) coeftest(x, vcov = vcovCL(x, cluster=mft_text$opid)))

## simulate predicted probabilities
res <- rbind(sim(m3[[1]], iv=data.frame(Care=minmax(mft_text$Care)), cluster = mft_text$opid),
             sim(m3[[1]], iv=data.frame(Fairness=minmax(mft_text$Fairness)), cluster = mft_text$opid),
             sim(m3[[1]], iv=data.frame(Loyalty=minmax(mft_text$Loyalty)), cluster = mft_text$opid),
             sim(m3[[1]], iv=data.frame(Authority=minmax(mft_text$Authority)), cluster = mft_text$opid),
             sim(m3[[1]], iv=data.frame(Sanctity=minmax(mft_text$Sanctity)), cluster = mft_text$opid),
             sim(m3[[1]], iv=data.frame(General=minmax(mft_text$General)), cluster = mft_text$opid),
             
             sim(m3[[2]], iv=data.frame(Care=minmax(mft_root$Care)), cluster = mft_root$opid),
             sim(m3[[2]], iv=data.frame(Fairness=minmax(mft_root$Fairness)), cluster = mft_root$opid),
             sim(m3[[2]], iv=data.frame(Loyalty=minmax(mft_root$Loyalty)), cluster = mft_root$opid),
             sim(m3[[2]], iv=data.frame(Authority=minmax(mft_root$Authority)), cluster = mft_root$opid),
             sim(m3[[2]], iv=data.frame(Sanctity=minmax(mft_root$Sanctity)), cluster = mft_root$opid),
             sim(m3[[2]], iv=data.frame(General=minmax(mft_root$General)), cluster = mft_root$opid),
             
             sim(m3[[3]], iv=data.frame(Care=minmax(mft_trunc$Care)), cluster = mft_trunc$opid),
             sim(m3[[3]], iv=data.frame(Fairness=minmax(mft_trunc$Fairness)), cluster = mft_trunc$opid),
             sim(m3[[3]], iv=data.frame(Loyalty=minmax(mft_trunc$Loyalty)), cluster = mft_trunc$opid),
             sim(m3[[3]], iv=data.frame(Authority=minmax(mft_trunc$Authority)), cluster = mft_trunc$opid),
             sim(m3[[3]], iv=data.frame(Sanctity=minmax(mft_trunc$Sanctity)), cluster = mft_trunc$opid),
             sim(m3[[3]], iv=data.frame(General=minmax(mft_trunc$General)), cluster = mft_trunc$opid)
) %>% mutate(type = rep(c("1. text", "2. root", "3. trunc"), each = 6),
             typelab = factor(type, labels = c("Full Response Path","Root Response","Truncated Root Response")),
             foundation = factor(iv, levels = rev(c("Care", "Fairness", "Loyalty",
                                                    "Authority", "Sanctity", "General"))))

## Create plot
ggplot(res, aes(y=foundation, x=mean, xmin=cilo, xmax=cihi, col=typelab, shape=typelab)) + 
  geom_vline(xintercept = 0, col="grey") + plot_default +
  geom_point(position = position_nudge(y=.2-(as.numeric(res$typelab)-1)/5)) + 
  geom_errorbarh(height=0, position = position_nudge(y=.2-(as.numeric(res$typelab)-1)/5)) + 
  ylab("Moral Foundation") + xlab("Difference in P(Opinion Change)") +
  theme(legend.title = element_blank())
ggsave("fig/logit_persuasiveness.pdf", height=2.5, width=6)

## table for appendix
latexTable(m3, cluster = cos_red$opid, caption=c("Logit models predicting argument persuasiveness as a 
            function of moral word use (measured via MFT dictionary proportions). 
            Positive coefficients indicate higher probability of changing the OPs'
            mind (i.e., receiving a delta). 
            Standard errors (clustered by discussion thread) in parentheses. Estimates are used for Figure
            \\ref{fig:persuasiveness} in the main text.", "Logit models predicting argument persuasiveness as a 
            function of moral word use")
           , label="tab:persuasiveness", align="lccc"
           , varlabs=list(Care="Care",Fairness="Fairness",Loyalty="Loyalty",Authority="Authority"
                          , Sanctity="Sanctity",General="General","(Intercept)"="Intercept")
           , mlabs=c("Full Response Path","Root Response","Truncated Root Response")
           , file="tab/persuasiveness.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")