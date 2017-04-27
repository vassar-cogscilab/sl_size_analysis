library(dplyr)
library(sciplot)
library(ggplot2)

# loading raw data ####
all_data=read.csv2('sl_size_data.csv')

# data cleaning and annotating ####
all_data$value <- NULL

subject_set_size_condition = all_data %>% filter(sequence_type == 'predictable') %>%
  group_by(subject_id, grid) %>% summarize()

subject_set_size_condition$set_size <- as.numeric(factor(as.character(subject_set_size_condition$grid)))*2 + 2

all_data$set_size <- sapply(all_data$subject_id, function(x){
  return(subset(subject_set_size_condition, subject_id == x)$set_size)
})

all_data$subject_id <- as.numeric(all_data$subject_id)

# count the number of subjects ####
n.subjects <- length(unique(all_data$subject_id))

# get test data ####
test_data=subset(all_data, sequence_type=='unpredictable'|sequence_type=='predictable')
test_data$responses <- NULL
test_data$session_id <- NULL
test_data$survey_part <- NULL
test_data$button_pressed <- NULL

# quick plot of group-level RTs by sequence type ####
mean(test_data$rt)
sd(test_data$rt)
hist(test_data$rt,breaks=1000,xlim=c(0,3000))

summary_data=test_data%>%group_by(subject_id, sequence_type, block)%>%filter(correct==1 & rt<2500)%>%summarize(mean.rt=mean(rt))
lineplot.CI(block, mean.rt, sequence_type, data=summary_data)

# look at effect of set size ####
summary.set.size <- test_data %>% group_by(subject_id, sequence_type, block, set_size) %>%
  filter(correct==1 & rt < 2500) %>% summarize(mean.rt = mean(rt))

ggplot(summary.set.size, aes(x=block, y=mean.rt, color=sequence_type)) +
  stat_summary(fun.y=mean, fun.ymax=function(x){return(mean(x)+se(x))}, fun.ymin = function(x){return(mean(x)-se(x))})+
  facet_wrap(~set_size)

# t-tests by set_size ####

summary.set.size.no.block <- test_data %>% group_by(subject_id, sequence_type,set_size) %>%
  filter(correct==1 & rt < 2500) %>% summarize(mean.rt = mean(rt))

bargraph.CI(set_size, mean.rt, sequence_type, data=summary.set.size.no.block)

t.test(mean.rt ~ sequence_type, data=subset(summary.set.size.no.block, set_size==4), paired=T)
t.test(mean.rt ~ sequence_type, data=subset(summary.set.size.no.block, set_size==6), paired=T)
t.test(mean.rt ~ sequence_type, data=subset(summary.set.size.no.block, set_size==8), paired=T)

# individual subject ####

summary.pairs.subject <- test_data %>% filter(block==0) %>% 
  group_by(subject_id, set_size) %>% filter(row_number() <= 16) %>% select(subject_id, set_size, target)

summary.pairs.subject$pair <- rep(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8), n.subjects)

summary.pairs.subject <- summary.pairs.subject %>% group_by(subject_id, target, set_size) %>% filter(row_number() <= 1)

test_data$pair <- mapply(function(sid, targ){
  return(subset(summary.pairs.subject, subject_id == sid & target == targ)$pair)
}, test_data$subject_id, test_data$target)

test_data <- test_data %>% group_by(subject_id, target) %>% mutate(total.obs = row_number())

test_data <- test_data %>% filter(rt < 2500, correct == 1)

# subjects with piecewise learning:
# 14, 19, 20

which_subject_id <- 14
ggplot(subset(test_data, subject_id == which_subject_id), aes(x=total.obs, y=rt, color=sequence_type))+
  geom_point()+
  facet_wrap(~pair)+
  theme_bw()
