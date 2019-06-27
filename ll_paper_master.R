try(setwd("/Users/pracz/Work/NZILBB/socialpaper2_latest/RaczHayPierrehumbert2019/"))
try(setwd("~/Github/RaczHayPierrehumbert2019/"))

library(xtable)
library(lme4)
library(merTools)
library(tidyverse)
library(broom)

set.seed(2017)

options(mc.cores=parallel::detectCores())
# options(mc.cores=1)
Sys.setenv(TZ="Europe/Rome") # a szivemben örök tavasz van

load('paper2data_tidy.rda')
set.seed(0211)

vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam 
  v 
  }

##########################################
# data frames
##########################################


d = d %>% filter(kill == F) # slow participants

# training trial counts per participant
training = d %>% 
  filter(phase == 'training', pattern == 'dim') %>% 
  group_by(subject, cond, main.cue, competitor.cue, age, gender) %>% 
  summarise(trial.count = n()) %>% 
  ungroup()

# test phase, with trial count per subject
perc.distance$training.partner.pair = with(perc.distance, paste(training, main.cue, competitor.cue))

test = d %>% 
  filter(phase == 'test', pattern == 'dim') %>% 
  inner_join(training[,c('subject', 'trial.count')]) %>% 
  inner_join(perc.distance[,c('subject', 'training.partner.pair', 'r.main.dist', 'r.competitor.dist')]) %>% 
  mutate(item.seen = as.factor(item.seen))

# training summary

training.sum = training %>%
  select(main.cue, competitor.cue, subject, trial.count) %>% 
  gather(cue.type, cue.name, -subject, -trial.count)

# test summary

test.sum = test %>%
  group_by(subject, trial.count, main.cue, competitor.cue) %>% 
  summarise(correct = mean(na.omit(correct)))

dimsum = d %>% 
  filter(phase == 'test', cond %in% c('gender (ethnicity)','ethnicity (gender)'))

##########################################
# visualisations
##########################################

training.sum %>% 
  mutate(
    cue.type = factor(cue.type, levels = c('main.cue', 'competitor.cue')),
    cue.name = factor(cue.name, levels = c('View', 'Age', 'Ethnicity', 'Gender'))
    ) %>%  
  ggplot(aes(x = cue.name, y = trial.count)) +
    geom_jitter(aes(colour = cue.name), width = 0.3) +
    geom_violin(aes(fill = cue.name, alpha = 0.5)) +
    stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
    scale_fill_brewer(palette="Set2") +
    scale_colour_brewer(palette="Set2") +
    theme(
      text = element_text(size=30), 
      axis.text.x = element_text(angle=90, vjust=0.5), 
      axis.title.x=element_blank(), 
      legend.position = 'none'
      ) +
    facet_wrap( ~ cue.type, 
      labeller = labeller(cue.type = c(main.cue = 'main cue', competitor.cue = 'competitor cue')),
      ncol = 1
    ) +
    ylab('participant trial count') +
    ggtitle('Training phase')
ggsave('images/training_plot1.pdf', width = 10, height = 10)

test.sum %>% 
  group_by(main.cue) %>% 
  mutate(
    above.average = ifelse(correct > mean(correct), 'above test mean', 'below test mean')
  ) %>% 
  ggplot(aes(x = above.average, y = trial.count)) + 
  geom_jitter(aes(colour = main.cue), width = 0.3) +
  geom_violin(aes(fill = main.cue, alpha = 0.5)) +
  stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
  scale_fill_brewer(palette="Set2") +
  scale_colour_brewer(palette="Set2") +
  facet_wrap( ~ main.cue) +
  theme(
    text = element_text(size=30), 
    axis.text.x = element_text(angle=90, vjust=0.5), 
    axis.title.x=element_blank(), 
    legend.position = 'none'
  ) +
  ylab('training trial count')
ggsave('images/training_plot2.pdf', width = 10, height = 10)

test.sum %>% 
  ggplot(aes(x = trial.count, y = correct, colour = main.cue)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  xlab('trial count in training') +
  ylab('mean accuracy in test') +
  scale_colour_brewer(palette="Set2",
                    name="Main cue") +
  ggtitle('Test and training accuracy')
ggsave('images/training_plot3.pdf', width = 5, height = 5)

test.sum %>%
  gather(cue.type, cue.name, -subject, -correct, -trial.count) %>% 
  mutate(
    cue.type = factor(cue.type, levels = c('main.cue', 'competitor.cue')),
    cue.name = factor(cue.name, levels = c('View', 'Age', 'Ethnicity', 'Gender'))
    ) %>% 
  ggplot(aes(x = cue.name, y = correct)) +
    geom_jitter(aes(colour = cue.name), width = 0.3) +
    geom_violin(aes(fill = cue.name, alpha = 0.5)) +
    stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
    scale_fill_brewer(palette="Set2") +
    scale_colour_brewer(palette="Set2") +
    theme(
      text = element_text(size=30), 
      axis.text.x = element_text(angle=90, vjust=0.5), 
      axis.title.x=element_blank(), 
      legend.position = 'none'
      ) +
    facet_wrap( ~ cue.type, 
      labeller = labeller(cue.type = c(main.cue = 'main cue', competitor.cue = 'competitor cue')),
      ncol = 1
    ) +
    ylab('mean participant accuracy') +
    ggtitle('Test phase')
ggsave('images/test_plot1.pdf', width = 10, height = 10)

test %>% 
  group_by(
    subject, main.cue, conv.partner.seen
  ) %>% 
  summarise(correct = mean(na.omit(correct))) %>% 
  ggplot(aes(x = main.cue, y = correct)) +
  # geom_jitter(aes(colour = conv.partner.seen), width = 0.3) +
  geom_point(aes(colour = conv.partner.seen), position = position_jitterdodge()) +
  geom_violin(aes(fill = conv.partner.seen, alpha = 0.5), position = position_dodge(0.75)) +
  stat_summary(aes(group = conv.partner.seen), fun.y=mean, geom="point", shape=16, size=4, position = ) +
  # coord_cartesian(ylim = c(0.35,1)) +
    scale_fill_brewer(palette="Set3",
      name="Conversation\npartner",
      labels=c("Unfamiliar", "Familiar")) +
    scale_colour_brewer(palette="Set3") +
    theme(
      text = element_text(size=30), 
      axis.text.x = element_text(angle=90, vjust=0.5), 
      axis.title.x=element_blank(), 
      # legend.position = 'none'
      ) +
  guides(alpha = F, colour = F) +
  ggtitle("Test: main cue and conversation partner")
ggsave('images/test_plot2.pdf', width = 11, height = 10)

pj = position_jitter(width = 0.1)

test.sum = test %>% 
  group_by(subject, main.cue, conv.partner.seen) %>% 
  summarise(correct = mean(correct)) %>% 
  ungroup() %>% 
  group_by(conv.partner.seen) %>% 
  mutate(id = row_number())

test.sum %>% 
  ggplot(aes(x = interaction(conv.partner.seen, main.cue), y = correct)) +
  geom_violin(aes(fill = conv.partner.seen), alpha = 0.5) +
  geom_point(aes(colour = conv.partner.seen), position = pj) +
  geom_line(aes(group = interaction(id, main.cue)), alpha = 0.5, colour = 'grey', position = pj) +
  stat_summary(aes(group = conv.partner.seen), fun.y=mean, geom="point", shape=16, size=4) +
  scale_fill_brewer(palette="Set3",
                    name="Conversation\npartner",
                    labels=c("Unfamiliar", "Familiar")) +
  scale_colour_brewer(palette="Set3") +
  theme(
    text = element_text(size=30), 
    axis.text.x = element_text(angle=90, vjust=0.5), 
    axis.title.x=element_blank(), 
    # legend.position = 'none'
  ) +
  guides(alpha = F, colour = F) +
  ggtitle("Test: main cue and conversation partner") +
  scale_x_discrete(labels = rep(levels(test.sum$main.cue), each = 2)) +
  ylab('mean participant accuracy')
ggsave('images/test_plot3.pdf', width = 11, height = 10)

test %>% 
  group_by(subject, r.main.dist, main.cue) %>%
  summarise(correct = mean(correct)) %>% 
  ggplot(aes(x = r.main.dist, y = correct, colour = main.cue)) +
  geom_point(position = position_jitter(width = 0.05))
# not that visual

##########################################
# models
##########################################

# main model

# only diminutives

fit1 = glmer(correct ~ 1 + main.cue + conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit2 = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit3 = glmer(correct ~ 1 + main.cue * item.seen + conv.partner.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit4 = glmer(correct ~ 1 + main.cue + item.seen + conv.partner.seen * competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit5 = glmer(correct ~ 1 + main.cue + conv.partner.seen + item.seen * competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

mc1 = anova(fit1,fit2) %>% tidy
mc2 = anova(fit1,fit3) %>% tidy
mc3 = anova(fit1,fit4) %>% tidy
mc4 = anova(fit1,fit5) %>% tidy
rbind(mc1,mc2,mc3,mc4) %>% 
  select(-statistic,-Chi.Df,-p.value) %>% 
  xtable


## singular fit:
fit2b = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 + main.cue * conv.partner.seen | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
## singular fit:
fit2c = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 + main.cue + conv.partner.seen | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
## confint profiling finds lower deviance: the p = 0.05 is probably not a massive diff
fit2d = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 + main.cue | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# summary(fit2d)
anova(fit2,fit2d)


save(fit1, file = 'models/fit1.rda');save(fit2, file = 'models/fit2.rda');save(fit3, file = 'models/fit3.rda');save(fit4, file = 'models/fit4.rda');save(fit5, file = 'models/fit5.rda');save(fit2d, file = 'models/fit2d.rda')
load('fit1.rda');load('fit2.rda');load('fit3.rda');load('fit4.rda');load('fit5.rda');load('fit2d.rda')
t1 = broom::tidy(fit2)

confints = confint(fit2, method = 'Wald') %>% broom::tidy()
confints = confints[-1,]
names(confints) = c('term', '2.5%', '97.5%')
t1 = inner_join(t1, confints) %>% 
  dplyr::select(-p.value,-group)

t1 = t1 %>% 
  mutate(
    term = str_replace(term, 'main.cue', 'main cue = '),
    term = str_replace(term, 'competitor.cue', 'competitor cue = '),
    term = str_replace(term, 'item.seen', 'familiar item'),
    term = str_replace(term, 'conv.partner.seen', 'familiar conversation partner'),
    term = str_replace(term, 'TRUE', '')
  )
t1 %>% xtable

# secondary models

fit7 = glm(correct ~ 1 + trial.count * main.cue, family = binomial, data = test)
confint.default(fit7)

fit7b = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

anova(fit2,fit7b)
vif.mer(fit2)
vif.mer(fit7b)

fit8 = glmer(correct ~ 1 + r.main.dist + r.competitor.dist + ( 1 | subject ) + ( 1 | training.partner.pair ), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

fit9 = glmer(correct ~ 1 + r.main.dist * r.competitor.dist + ( 1 | subject ) + ( 1 | training.partner.pair ), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

anova(fit8,fit9)
summary(fit9)
anova(fit2,fit9)
# t1 = test %>% 
#   do( tidy(t(quantile(.$r.main.dist, probs = seq(0, 1, 0.1)))) ) %>% 
#   gather(quantile,value) %>% 
#   mutate(
#     distance = 'r.main.dist'
#   )

fit10 = glmer(correct ~ 1 + cond * pattern + ( 1 | subject ), family = binomial, data = dimsum, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit10b = glmer(correct ~ 1 + cond + pattern + ( 1 | subject ), family = binomial, data = dimsum, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit10c = glmer(correct ~ 1 + cond  + ( 1 | subject ), family = binomial, data = dimsum, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
anova(fit10b,fit10c)

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
    sample.fit = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 | subject), family = binomial, data = sample, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
    summary(sample.fit)$coef
  }
  save(sample.fits, file = 'samplefits.rda')
}

runSamples()

# see how many times z indicates 5%
load('samplefits.rda')
viewCPS = as.list(NULL);ageCPS = as.list(NULL);ethnicityCPS = as.list(NULL);genderCPS = as.list(NULL)
for (i in 1:100){
fit.summary = sample.fits[[i]]
  viewCPS[[i]] = fit.summary[rownames(fit.summary) == 'conv.partner.seenTRUE', 3]
  ageCPS[[i]] = fit.summary[rownames(fit.summary) == 'main.cueAge:conv.partner.seenTRUE', 3]
  ethnicityCPS[[i]] = fit.summary[rownames(fit.summary) == 'main.cueEthnicity:conv.partner.seenTRUE', 3]
  genderCPS[[i]] = fit.summary[rownames(fit.summary) == 'main.cueGender:conv.partner.seenTRUE', 3]
}
viewCPS = unlist(viewCPS);ageCPS = unlist(ageCPS);ethnicityCPS = unlist(ethnicityCPS);genderCPS = unlist(genderCPS)
viewCPS[viewCPS > 1.8 | viewCPS < -1.8] %>% length
ageCPS[ageCPS > 1.8 | ageCPS < -1.8] %>% length
ethnicityCPS[ethnicityCPS > 1.8 | ethnicityCPS < -1.8] %>% length
genderCPS[genderCPS > 1.8 | genderCPS < -1.8] %>% length
sample.fits
