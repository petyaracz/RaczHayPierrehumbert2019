setwd("/Users/pracz/Work/NZILBB/socialpaper2_latest/RaczHayPierrehumbert2019/")
library(tidyverse)
library(xtable)
library(lme4)
library(effects)
load('paper2data_tidy.rda')
set.seed(0211)

# save.image('paper2data_tidy.rda')
# all subjects who handed in the task on MTurk and got paid
total.subjects = d %>% select(subject) %>% unique %>% nrow

##########################################
# data frames
##########################################


d = d %>% filter(kill == F) # slow participants

# training trial counts per participant
training = d %>% 
  filter(phase == 'training') %>% 
  group_by(subject, cond, pattern, main.cue, competitor.cue, age, gender) %>% 
  summarise(trial.count = n()) %>% 
  ungroup()

# test phase, with trial count per subject
test = d %>% 
  filter(phase == 'test') %>% 
  inner_join(training[,c('subject', 'trial.count')]) %>% 
  mutate(item.seen = as.factor(item.seen))

# test phase, with perceptual distances, diminutive pattern only
dim.test = test %>% 
  filter(pattern == 'dim') %>% 
  select(subject, correct) %>% 
  inner_join(perc.distance)

##########################################
# summaries
##########################################

# subjects whose trial count is below the 0.975 quantile of the per-group trial count:
rem.subjects = d %>% select(subject) %>% unique %>% nrow

##########################################
# summary statistics for groups
##########################################

# sum.table = d %>%
#   mutate(
#     gender.num = as.numeric(gender) - 1
#     ) %>% 
#   select(subject, cond, pattern, age, gender.num) %>% 
#   unique %>% 
#   group_by(cond, pattern) %>% 
#   summarise(
#     participant.count = n(),
#     men.ratio = gender.num %>% na.omit %>% mean,
#     mean.age = age %>% na.omit %>% mean,
#     sd.age = age %>% na.omit %>% sd
#     )
# sum.table %>% xtable

##########################################
# training results
##########################################

min(training$trial.count)
max(training$trial.count)
mean(training$trial.count)
sd(training$trial.count)

training.fit1 = glm(trial.count ~ gender + age + main.cue + competitor.cue + pattern, data = training)
summary(training.fit1)
training.fit1b = glm(trial.count ~ gender + age + main.cue + competitor.cue + pattern, data = training, family = 'poisson')
#AIC(training.fit1);AIC(training.fit1b)
plot(effect('main.cue', training.fit1))
plot(effect('competitor.cue', training.fit1))

summary(training.fit1)$coef[,-4] %>% xtable(digits = 2)

training %>%
  select(subject, trial.count, main.cue, competitor.cue) %>% 
  gather(cue.type, cue.name, -subject, -trial.count) %>% 
  mutate(
    cue.type = factor(cue.type, levels = c('main.cue', 'competitor.cue')),
    cue.name = factor(cue.name, levels = c('View', 'Age', 'Ethnicity', 'Gender'))
    ) %>%  
  ggplot(aes(x = cue.name, y = trial.count, fill = cue.name)) +
    geom_violin() +
    geom_rug() +
    stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
    scale_fill_brewer(palette="Set2") +
    theme(
      text = element_text(size=30), 
      axis.text.x = element_text(angle=90, vjust=0.5), 
      axis.title.x=element_blank(), 
      legend.position = 'none'
      ) +
    facet_wrap( ~ cue.type, 
      labeller = labeller(cue.type = c(main.cue = 'main cue', competitor.cue = 'competitor cue'))
    ) +
    ylab('participant trial count') +
    ggtitle('Training phase')
ggsave('training_plot1.pdf', width = 10, height = 7, device = cairo_pdf())
 
##########################################
# test results: trial count
##########################################

test.fit7 = glm(correct ~ trial.count, data = test, family = "binomial")
summary(test.fit7)

##########################################
# test results: plots
##########################################

test %>%
  select(subject, correct, main.cue, competitor.cue) %>% 
  group_by(subject, main.cue, competitor.cue) %>% 
  summarise(correct = mean(na.omit(correct))) %>% 
  gather(cue.type, cue.name, -subject, -correct) %>% 
  mutate(
    cue.type = factor(cue.type, levels = c('main.cue', 'competitor.cue')),
    cue.name = factor(cue.name, levels = c('View', 'Age', 'Ethnicity', 'Gender'))
    ) %>%  
  ggplot(aes(x = cue.name, y = correct, fill = cue.name)) +
    geom_rug() +
    geom_violin() +    
    stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
    scale_fill_brewer(palette="Set2") +
    theme(
      text = element_text(size=30), 
      axis.text.x = element_text(angle=90, vjust=0.5), 
      axis.title.x=element_blank(), 
      legend.position = 'none'
      ) +
    facet_wrap( ~ cue.type, 
      labeller = labeller(cue.type = c(main.cue = 'main cue', competitor.cue = 'competitor cue'))
    ) +
    ylab('mean participant accuracy') +
    ggtitle('Test phase')
ggsave('test_plot1.pdf', width = 10, height = 7, device = cairo_pdf())

##########################################
# test results: learners
##########################################

test %>%
  select(subject, correct, main.cue) %>% 
  group_by(subject, main.cue) %>% 
  summarise(correct = mean(na.omit(correct))) %>% 
  mutate(
    group = ifelse(correct > 0.75, 'over .75', 'below .75')
  ) %>% 
  group_by(main.cue, group) %>% 
  summarise(n = n()) %>% 
  spread(group, n) %>% 
  mutate(total = `below .75` + `over .75`, learner.ratio = `over .75` / total) %>% 
  xtable
  
test %>%
  filter(main.cue %in% c('Gender', 'Ethnicity')) %>% 
  select(subject, correct, main.cue, pattern) %>% 
  group_by(subject, main.cue, pattern) %>% 
  summarise(correct = mean(na.omit(correct))) %>% 
  mutate(
    group = ifelse(correct > 0.75, 'over .75', 'below .75')
  ) %>% 
  group_by(main.cue, pattern, group) %>% 
  summarise(n = n()) %>% 
  spread(group, n) %>% 
  mutate(total = `below .75` + `over .75`, learner.ratio = `over .75` / total)

##########################################
# test results: big model
##########################################

# this takes ages so i put it in a function
testFit = function(){  
test.fit1 = glmer(correct ~ c.age + main.cue * conv.partner.seen + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
test.fit2 = glmer(correct ~ c.age + main.cue * conv.partner.seen + pattern + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
test.fit3 = glmer(correct ~ c.age + main.cue + conv.partner.seen + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
test.fit4 = glmer(correct ~ c.age + main.cue + item.seen + conv.partner.seen + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
test.fit6 = glmer(correct ~ c.age + main.cue * conv.partner.seen + pattern + item.seen + competitor.cue + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))

test$main_cue = test$main.cue
test$conversation_partner = test$conv.partner.seen %>% as.character
test[test$conversation_partner=='TRUE',]$conversation_partner = 'familiar'
test[test$conversation_partner=='FALSE',]$conversation_partner = 'unfamiliar'
test$conversation_partner = factor(test$conversation_partner, levels = c('unfamiliar', 'familiar'))
test.fit6b = glmer(correct ~ c.age + main_cue * conversation_partner + pattern + item.seen + competitor.cue + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=20000)))

# anova(test.fit1,test.fit2)
anova(test.fit1,test.fit3)
anova(test.fit1,test.fit4)
anova(test.fit1,test.fit5)
anova(test.fit1,test.fit6)
AIC(test.fit1);AIC(test.fit2);AIC(test.fit3);AIC(test.fit4);AIC(test.fit5)
# [1] 40368.11
# [1] 50645.04
# [1] 40370.03
# [1] 40372.28
# [1] 40373.8
save(test.fit6b, file = 'testFit.rda')
min.fit1 = glmer(correct ~ main.cue * conv.partner.seen + (1|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
min.fit2 = glmer(correct ~ main.cue * conv.partner.seen + (conv.partner.seen|subject), data = test, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
anova(min.fit1,min.fit2)
summary(min.fit2)
}

load('testFit.rda') 

plot(allEffects(test.fit6, vcov. = car::hccm))
pdf('test_plot2.pdf', width = 5, height = 5)
plot(effect('main_cue:conversation_partner', test.fit6b, vcov. = car::hccm), multiline = T, ci.style = 'band', lines=list(lty=c(1,2)))
dev.off()

summary(test.fit6)$coef[,-4] %>% xtable(digits = 2)

##########################################
# dim test results: perc dist
##########################################

# does perceptual distance beat cue type in predicting participant behaviour?
fitDimTest = function(){
dim.test.fit8a <- glmer(correct ~ main.cue + competitor.cue + (1|subject), data=dim.test, family="binomial")
dim.test.fit8b <- glmer(correct ~ r.main.dist + r.competitor.dist + (1|subject), data=dim.test, family="binomial")
save(dim.test.fit8a, file = 'dim.test.fit8a')
save(dim.test.fit8b, file = 'dim.test.fit8b')
}

load('dim.test.fit8a')
load('dim.test.fit8b')

# no.
anova(dim.test.fit8a,dim.test.fit8b)

##########################################
# test results: group sizes
##########################################

test.people = test %>% 
  select(subject, group) %>% 
  unique()

# min number of people in group
min.count =  test.people %>% 
  group_by(group) %>% 
  summarise(n = n()) %>% 
  pull(n) %>% 
  min

# sample other groups down to this size. 
sample.people = plyr::ddply(test.people, "group" , function(x) x[sample(nrow(x), min.count),])
sample = test %>% filter(subject %in% sample.people$subject)

# test the conv partner seen : main cue interaction with samples
runSamples = function(){
  
  library(doParallel)
  registerDoParallel(cores = 4)
  
  sample.fits = foreach(i = 1:100) %dopar% {
  
    sample.people = plyr::ddply(test.people, "group" , function(x) x[sample(nrow(x), min.count),])
    sample = test %>% filter(subject %in% sample.people$subject)
    sample.fit = glmer(correct ~ conv.partner.seen * main.cue + (1|subject), family = 'binomial', data = sample)
    summary(sample.fit)$coef
  }
  save(sample.fits, file = 'samplefits.rda')
}

# see how many times z indicates 5%
# load('samplefits.rda')
# viewCPS = as.list(NULL);ageCPS = as.list(NULL);ethnicityCPS = as.list(NULL);genderCPS = as.list(NULL)
# for (i in 1:100){
# fit.summary = sample.fits[[i]]
#   viewCPS[[i]] = fit.summary[rownames(fit.summary) == 'conv.partner.seenTRUE', 3]
#   ageCPS[[i]] = fit.summary[rownames(fit.summary) == 'conv.partner.seenTRUE:main.cueAge', 3]
#   ethnicityCPS[[i]] = fit.summary[rownames(fit.summary) == 'conv.partner.seenTRUE:main.cueEthnicity', 3]
#   genderCPS[[i]] = fit.summary[rownames(fit.summary) == 'conv.partner.seenTRUE::main.cueGender', 3]
# }
# viewCPS = unlist(viewCPS);ageCPS = unlist(ageCPS);ethnicityCPS = unlist(ethnicityCPS);genderCPS = unlist(genderCPS)
# viewCPS[viewCPS > 1.8 | viewCPS < -1.8] %>% length
# ageCPS[ageCPS > 1.8 | ageCPS < -1.8] %>% length
# ethnicityCPS[ethnicityCPS > 1.8 | ethnicityCPS < -1.8] %>% length
# genderCPS[genderCPS > 1.8 | genderCPS < -1.8] %>% length
# 0, 45, 94, 0
