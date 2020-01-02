##########################################
# Data and code for the paper "Not all indexical cues are equal..."
# Péter Rácz
# v1.0
# 25.10.19
##########################################
##########################################

try(setwd("~/Github/RaczHayPierrehumbert2019/"))

library(xtable)
library(lme4)
library(merTools)
library(tidyverse)
library(broom)

set.seed(0211)

options(mc.cores=parallel::detectCores())
# options(mc.cores=1)
Sys.setenv(TZ="Europe/Rome") # a szivemben örök tavasz van

##########################################
load('data/paper2data_tidy.rda')

# d is the data for all participants across experiments who completed the experiment on Mechanical Turk (see data dictionary)
# perc.distance is the visual distance between interlocutor images calculated for each participant
# since various constellations of participants repeat across participants, distances will, as well.

##########################################
# from Florian Jaeger

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
# we only report data on the diminutive pattern

d$training = NULL # this is replaced below.

alltest = d %>% 
  filter(phase == 'test', pattern == 'dim')
  
d = d %>% filter(kill == F) # slow participants are removed

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

test.old.sum = d %>% 
  filter(phase == 'test', pattern == 'dim', cond %in% c('gender (view)', 'view (gender)'), str_detect(interlocutor, 'adult')) %>% 
  group_by(subject, cond) %>% 
  summarise(correct = mean(na.omit(correct)))

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
# sanity checks
##########################################

d %>% 
  select(subject, main.cue, competitor.cue, pattern) %>% 
  unique %>% 
  count(main.cue, competitor.cue, pattern)

perc.distance %>% # no plural
  count(main.cue, competitor.cue)

perc.distance %>% # no plural
  count(main.cue, competitor.cue, training)

##########################################
# visualisations
##########################################

# violin plots with means: trial counts per participant for cue types across whether this is a main or competitor cue
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

# training trial counts for good learners and less good learners
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

# test accuracy x training trial count per participant
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

# violin plots with means: mean test accuracy per participant for cue types across whether this is a main or competitor cue
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

# violin plots of participant averages with seen and new conv partners across four main cues
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

# violin plots of participant averages with seen and new conv partners across four main cues, with lines connecting the two averages per participant.
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

##########################################
# models
##########################################

# main model

# only diminutives

# 1. We fit models for all meaningful interactions

fit1 = glmer(correct ~ 1 + main.cue + conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit2 = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit3 = glmer(correct ~ 1 + main.cue * item.seen + conv.partner.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit4 = glmer(correct ~ 1 + main.cue + item.seen + conv.partner.seen * competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit5 = glmer(correct ~ 1 + main.cue + conv.partner.seen + item.seen * competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

# 2. We do a lot of goodness-of-fit tests

mc1 = anova(fit1,fit2) %>% tidy
mc2 = anova(fit1,fit3) %>% tidy
mc3 = anova(fit1,fit4) %>% tidy
mc4 = anova(fit1,fit5) %>% tidy
rbind(mc1,mc2,mc3,mc4) %>% 
  select(-statistic,-Chi.Df,-p.value) %>% 
  xtable

# we like fit2 the most.

# 3. We explore random slopes for the best model, which is fit2

## singular fit:
fit2b = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 + main.cue * conv.partner.seen | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
## singular fit:
fit2c = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 + main.cue + conv.partner.seen | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
## confint profiling finds lower deviance: the p = 0.05 is probably not a massive diff
fit2d = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + ( 1 + main.cue | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# summary(fit2d)
anova(fit2,fit2d)

fit2e = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = alltest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

# 4. We save all these models

save(fit1, file = 'models/fit1.rda');save(fit2, file = 'models/fit2.rda');save(fit3, file = 'models/fit3.rda');save(fit4, file = 'models/fit4.rda');save(fit5, file = 'models/fit5.rda');save(fit2d, file = 'models/fit2d.rda')

# now we load them!

load('models/fit1.rda');load('models/fit2.rda');load('models/fit3.rda');load('models/fit4.rda');load('models/fit5.rda');load('models/fit2d.rda')

# 5. Since we like fit2 the most, this is the one we're reporting.

t1 = broom::tidy(fit2)

# we get some wald confidence intervals

confints = confint(fit2, method = 'Wald') %>% broom::tidy()
confints = confints[-1,]
names(confints) = c('term', '2.5%', '97.5%')
t1 = inner_join(t1, confints) %>% 
  dplyr::select(-p.value,-group)

# we glue these to the summary.
# we make the labels nicer.

t1 = t1 %>% 
  mutate(
    term = str_replace(term, 'main.cue', 'main cue = '),
    term = str_replace(term, 'competitor.cue', 'competitor cue = '),
    term = str_replace(term, 'item.seen', 'familiar item'),
    term = str_replace(term, 'conv.partner.seen', 'familiar conversation partner'),
    term = str_replace(term, 'TRUE', '')
  )
t1 %>% xtable

# we also make a confint figure

t1 %>% 
  filter(term != '(Intercept)') %>% 
  mutate(
    row_id = 12:1,
  ) %>% 
  ggplot(aes(x = term %>% reorder(row_id), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(x = term, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  xlab('term') +
  coord_flip() +
  ggtitle('Estimates and 95% confidence intervals in model for test results')



# 6. We add in training trial count.

fit7 = glm(correct ~ 1 + trial.count * main.cue, family = binomial, data = test)
confint.default(fit7)

fit7b = glmer(correct ~ 1 + main.cue * conv.partner.seen + item.seen + competitor.cue + rescale(trial.count) + ( 1 | subject), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

anova(fit2,fit7b)
vif.mer(fit2)
vif.mer(fit7b)

# 8. We check whether perceptual distance between interlocutor images is a better predictor...

fit8 = glmer(correct ~ 1 + r.main.dist + r.competitor.dist + ( 1 | subject ) + ( 1 | training.partner.pair ), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

fit9 = glmer(correct ~ 1 + r.main.dist * r.competitor.dist + ( 1 | subject ) + ( 1 | training.partner.pair ), family = binomial, data = test, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

anova(fit8,fit9)
summary(fit9)
anova(fit2,fit9)

# ...it isn't

# 9. We check whether the pattern type (diminutive / plural) makes any difference...

fit10 = glmer(correct ~ 1 + cond * pattern + ( 1 | subject ), family = binomial, data = dimsum, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit10b = glmer(correct ~ 1 + cond + pattern + ( 1 | subject ), family = binomial, data = dimsum, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
fit10c = glmer(correct ~ 1 + cond  + ( 1 | subject ), family = binomial, data = dimsum, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
anova(fit10b,fit10c)

# ...it doesn't

##########################################
# test results: group sizes
##########################################

# the problem is that sample sizes vary. 

test.people = test %>% 
  select(subject, group) %>% 
  unique()

# let's take the smallest sample size
min.count =  test.people %>% 
  group_by(group) %>% 
  summarise(n = n()) %>% 
  pull(n) %>% 
  min

# and sample other groups down to this size. 
sample.people = plyr::ddply(test.people, "group" , function(x) x[sample(nrow(x), min.count),])
sample = test %>% filter(subject %in% sample.people$subject)

# let's do this a lot
# and test the conv partner seen : main cue interaction with these samples
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

# see how many times z indicates that the result is robust

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
