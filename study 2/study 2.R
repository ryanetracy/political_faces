############################
# liberal/conservative RC
# study 2
# affect misattribution
############################


# packages
pckgs <- c('rstatix', 'lme4', 'lmerTest', 'effectsize', 'interactions', 'jtools', 'tidyverse')


for (i in 1:length(pckgs)) {
  if(!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}


# read in data
df <- list.files(path = 'study 2/data/', pattern = '*.csv', full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = 'c')))

colnames(df)


# for main analysis, will need the following columns
# charRating.keys
# stimID
# charID
# generatorPol
# ciGroup
# ciLevel
# prolificID

# participants should have 109 responses
p_counts <- df %>% count(prolificID) %>% filter(n != 109)

# remove 13 participants (either did not finish or took study multiple times)
df <- df %>% filter(!(prolificID %in% p_counts$prolificID))

# one participant named 'test' needs to be removed
df <- df %>% filter(prolificID != 'test')

# now grab the test columns
s2_df <- df %>%
  select(prolificID, stimID, charID, ciLevel, generatorPol, ciGroup, charRating.keys) %>%
  # rename charRating.keys column to 'rating'
  rename(rating = charRating.keys) %>%
  # set contrasts for generatorPol and ciGroup
  mutate(pol_c = ifelse(generatorPol == 'liberal', -1, 1),
         group_c = ifelse(ciGroup == 'outgroup', -1, 1)) %>%
  # set factors (participant/stimIDs)
  mutate_at(c('prolificID', 'stimID', 'charID'), .funs = as.factor) %>%
  mutate_at(c('rating'), .funs = as.numeric) %>%
  na.omit()


# separate data by individual and group-level cis
s2_inv <- s2_df %>% filter(ciLevel == 'individual')

s2_group <- s2_df %>% filter(ciLevel == 'group')


# how were the responses distributed
ggplot(s2_inv, aes(generatorPol, rating, fill = ciGroup)) +
  geom_boxplot(color = 'black', alpha = .7, outlier.colour = 'red', outlier.shape = 4) +
  theme_bw() +
  scale_fill_manual(values = c('red', 'blue'))

ggplot(s2_inv, aes(rating, fill = ciGroup)) +
  geom_histogram(color = 'black', alpha = .7, binwidth = 1) +
  theme_bw() +
  facet_wrap(~ generatorPol) +
  scale_fill_manual(values = c('red', 'blue'))

# get descriptives
s2_inv %>%
  group_by(generatorPol, ciGroup) %>%
  get_summary_stats(rating, type = 'mean_sd')

# get demographics
s2_demos <- read.csv('study 2/study 2 - demographics.csv')

s2_demos <- s2_demos %>%
  select(prolific_id, age, sex, race, politics_1)

# get unique ids
demo_ids <- unique(s2_inv$prolificID)

s2_demos <- s2_demos %>% filter(prolific_id %in% demo_ids)

# age
mean(s2_demos$age, na.rm = T); sd(s2_demos$age, na.rm = T)

# sex
s2_demos %>%
  count(sex) %>%
  mutate(prop = round(100 * (n / sum(n)), 2)) %>%
  mutate(sex = case_when(
    sex == 1 ~ 'male',
    sex == 2 ~ 'female',
    sex == 3 ~ 'non-binary',
    sex == 4 ~ 'other',
    TRUE ~ 'did not identify'
  ))

# race
s2_demos %>%
  count(race) %>%
  mutate(prop = round(100 * (n / sum(n)), 2)) %>%
  mutate(race = case_when(
    race == 1 ~ 'asian',
    race == 2 ~ 'black',
    race == 3 ~ 'latino/a',
    race == 4 ~ 'middle eastern',
    race == 5 ~ 'native american',
    race == 6 ~ 'pacific islander',
    race == 7 ~ 'white',
    race == 8 ~ 'other',
    TRUE ~ 'did not identify'
  ))

mean(s2_demos$politics_1, na.rm = T); sd(s2_demos$politics_1, na.rm = T)

# make a helper function to get all model parameters
get_reg <- function(model) {
  # print model summary
  print(summary(model)) 
  
  # print model standardized parameters
  print(standardize_parameters(model))
}

# create model
m1 <- lmer(rating ~ pol_c * group_c + (group_c|prolificID) + (0 + group_c|stimID:charID), data = s2_inv)
get_reg(m1) # sweet, a significant effect of ingroup/outgroup


# generate summary stats to plot
# get summary stats and make a plot
inv_sum <- s2_inv %>%
  group_by(generatorPol, ciGroup) %>%
  get_summary_stats(rating, type = 'mean_ci') %>%
  rename(m_rating = mean)


# get summary stats per participant
inv_sum_p <- s2_inv %>%
  group_by(prolificID, generatorPol, ciGroup) %>%
  get_summary_stats(rating, type = 'mean_ci') %>%
  rename(m_rating = mean)


ggplot(inv_sum_p, aes(ciGroup, m_rating, fill = generatorPol)) +
  # participant-level data
  geom_violin(alpha = .5, color = 'black') +
  geom_point(aes(color = generatorPol), 
             size = .5, shape = 4, alpha = .7, position = position_jitterdodge(.15, .05, .9)) +
  # add in mean-level data
  geom_point(data = inv_sum, aes(ciGroup, m_rating), 
             size = 2, shape = 7, color = 'black', position = position_dodge(.9)) +
  geom_errorbar(data = inv_sum, aes(ciGroup, m_rating, ymin = m_rating - ci, ymax = m_rating + ci),
                width = .1, color = 'black', position = position_dodge(.9)) +
  theme_classic() +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c("Conservative\nParticipants' CIs", "Liberal\nParticipants' CIs")) +
  scale_color_manual(values = c('red', 'blue'),
                     labels = c("Conservative\nParticipants' CIs", "Liberal\nParticipants' CIs")) +
  labs(x = '',
       y = 'Mean Pleasantness Rating',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')

# ggsave('study 2 - pleasantness ratings (2 x 2) - individual-level CIs.jpg', device = 'jpeg', units = 'cm', path = 'study 2')

# make another plot showing just the data by ci group
# get summary stats and make a plot
inv_sum_2 <- s2_inv %>%
  group_by(ciGroup) %>%
  get_summary_stats(rating, type = 'mean_ci') %>%
  rename(m_rating = mean)


# get summary stats per participant
inv_sum_p_2 <- s2_inv %>%
  group_by(prolificID, ciGroup) %>%
  get_summary_stats(rating, type = 'mean_ci') %>%
  rename(m_rating = mean)


ggplot(inv_sum_p_2, aes(ciGroup, m_rating, 
                        #fill = ciGroup, color = ciGroup
                        )) +
  # participant-level data
  geom_violin(alpha = .8, color = 'black', fill = '#0b2265') +
  geom_point(size = .5, shape = 4, alpha = .5, position = position_jitter(.15, .05), color = '#a71930') +
  # add in mean-level data
  geom_point(data = inv_sum_2, aes(ciGroup, m_rating), 
             size = 2, shape = 7, color = 'white') +
  geom_errorbar(data = inv_sum_2, aes(ciGroup, m_rating, ymin = m_rating - ci, ymax = m_rating + ci),
                width = .1, color = 'white') +
  theme_classic() +
  # scale_fill_manual(values = c('red', 'blue')) +
  # scale_color_manual(values = c('red', 'blue')) +
  labs(x = '',
       y = 'Pleasantness Rating',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'none')

# ggsave('study 2 - mean pleasantness rating (ingroup v outgroup) - individual-level CIs.jpg', device = 'jpeg', units = 'cm', path = 'study 2')



# load ratings data for individual-level cis to test any effects of perceived traits
inv_traits <- read.csv('ratings data - individual cis.csv')



# left-join this by stimID
s2_inv_traits <- s2_inv %>%
  left_join(inv_traits, by = c('stimID' = 'stimID')) %>%
  # don't need redundant politics/group columns
  select(-c(polParty, ciGroup.y))

colnames(s2_inv_traits)


# run all traits in a controlled model
m2 <- lmer(rating ~ pol_c * group_c + trustworthy + dominant + warm + competent + openMinded + intelligent + moral + hardWorking + patriotic + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m2)

# test interactions with group status
m3 <- lmer(rating ~ group_c*trustworthy + group_c*dominant + group_c*warm + group_c*competent + group_c*openMinded + group_c*intelligent + group_c*moral + group_c*hardWorking + group_c*patriotic + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m3)

# possible effect of trustworthiness
m4 <- lmer(rating ~ pol_c * group_c * trustworthy + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m4)

# possible effect of competence
m5 <- lmer(rating ~ pol_c * group_c * competent + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m5)

# possible effect of morality
m6 <- lmer(rating ~ pol_c * group_c * moral + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m6)

# possible effect of politics
m7 <- lmer(rating ~ pol_c * group_c * politics + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m7)

# drop experimental factors and only run traits
m8 <- lmer(rating ~ trustworthy + dominant + warm + competent + openMinded + intelligent + moral + hardWorking + patriotic + (1|prolificID) + (1|stimID:charID), data = s2_inv_traits)
get_reg(m8)

# run model with group-level cis
m1.1 <- lmer(rating ~ pol_c * group_c + (group_c|prolificID), data = s2_group)
get_reg(m1.1)

# run simple effects tests at each level of participant political group
# liberal
m1.11 <- lmer(rating ~ group_c + (1|prolificID), data = filter(s2_group, pol_c == -1))
get_reg(m1.11)

# conservative
m1.12 <- lmer(rating ~ group_c + (1|prolificID), data = filter(s2_group, pol_c == 1))
get_reg(m1.12)

# no significant effect of group for cis generated by conservative participants
# significant effect of group (in > out) for liberal participants' cis










