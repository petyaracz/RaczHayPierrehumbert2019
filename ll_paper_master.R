setwd("/Users/pracz/Work/NZILBB/socialpaper2_latest/RaczHayPierrehumbert2019/")
library(tidyverse)
library(xtable)
library(lme4)
library(effects)
load('paper2data_tidy.rda')
set.seed(0211)

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

# training summary

training.sum = training %>%
  select(subject, trial.count, main.cue, competitor.cue) %>% 
  gather(cue.type, cue.name, -subject, -trial.count)

# test summary

test.sum = test %>%
  group_by(subject, trial.count, main.cue, competitor.cue) %>% 
  summarise(correct = mean(na.omit(correct)))

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
ggsave('training_plot1.pdf', width = 10, height = 10, device = cairo_pdf())

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
ggsave('test_plot1.pdf', width = 10, height = 10, device = cairo_pdf())

test %>% 
  group_by(
    subject, main.cue, conv.partner.seen
  ) %>% 
  summarise(correct = mean(na.omit(correct))) %>% 
  ggplot(aes(x = main.cue, y = correct)) +
  # geom_jitter(aes(colour = conv.partner.seen), width = 0.3) +
  geom_point(aes(colour = conv.partner.seen), position = position_jitterdodge()) +
  geom_violin(aes(fill = conv.partner.seen, alpha = 0.5), position = position_dodge(0.75)) +
  stat_summary(aes(group = conv.partner.seen), fun.y=mean, geom="point", shape=16, size=4, position = position_dodge(0.75)) +
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
ggsave('test_plot2.pdf', width = 11, height = 10, device = cairo_pdf())
