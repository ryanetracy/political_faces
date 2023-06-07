############################
# liberal/conservative RC
# study 3
# ingroup inclusion task
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
df <- list.files(path = 'study 3/data/', pattern = '*.csv', full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = 'c')))

colnames(df)

# for initial cleaning, grab:
# group key column (participants' mgp assignment) (will have to left join this back)
# group cat column (their decisions)
# stimID
# prolificID
# participantPol (ci generators' affiliation)
# ciGroup (in or outgroup)
# ci type (individual or group-level cis)

# participants should have 109 responses
p_list <- df %>% count(ProlificID) %>% filter(n != 109)

# remove these participants from analyses
df <- df %>% filter(!(ProlificID %in% p_list$ProlificID))

# get group key and id columns
group_assign <- df %>% select(groupKey.keys, ProlificID) %>% rename(assigned_group = groupKey.keys) %>% na.omit()

# one 'test' response got in there, remove
group_assign <- filter(group_assign, ProlificID != 'test')

# now grab test columns
s3_df <- df %>% 
  select(ProlificID, participantPol, ciGroup, stimID, groupCat.keys, ciType) %>%
  # remove 'test' id
  filter(ProlificID != 'test') %>%
  # left join group_assign df
  left_join(group_assign, by = c('ProlificID' = 'ProlificID')) %>%
  # rename the group inclusion column
  rename(inclusion = groupCat.keys) %>%
  # reformat the response column so that responses are scored as ingroup = 1, outgroup = 0
  mutate(inclusion = case_when(
    assigned_group == 'o' & inclusion == 'o' ~ 1,
    assigned_group == 'o' & inclusion == 'u' ~ 0,
    assigned_group == 'u' & inclusion == 'o' ~ 0,
    assigned_group == 'u' & inclusion == 'u' ~ 1
  )) %>%
  # create contrast codes for the IVs
  mutate(pol_c = ifelse(participantPol == 'liberal', -1, 1),
         group_c = ifelse(ciGroup == 'outgroup', -1, 1)) %>%
  # set participant and stimID cols as factors
  mutate_at(c('ProlificID', 'stimID'), .funs = as.factor)


# create separate dfs, one for the individual-level cis and one for the group-level cis
s3_inv <- s3_df %>% filter(ciType == 'individual')
s3_group <- s3_df %>% filter(ciType == 'group')


# get descriptives based on ci factors
s3_inv %>%
  group_by(participantPol, ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_sd')

s3_group %>%
  group_by(participantPol, ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_sd')

# get demographics
s3_demos <- read.csv('study 3/study 3 - demographics.csv')

s3_demos <- s3_demos %>%
  select(prolific_id, age, sex, race, politics_1)

# get unique ids
demo_ids <- unique(s3_inv$ProlificID)

s3_demos <- s3_demos %>% filter(prolific_id %in% demo_ids)

# age
mean(s3_demos$age, na.rm = T); sd(s3_demos$age, na.rm = T)

# sex
s3_demos %>%
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
s3_demos %>%
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

mean(s3_demos$politics_1, na.rm = T); sd(s3_demos$politics_1, na.rm = T)


# make helper function to get summary plus ORs plus OR CIs
get_reg <- function(model) {
  # print model summary
  print(summary(model))
  
  # get model CIs
  # start by estimating the model's SEs
  se_est <- sqrt(diag(vcov(model)))
  
  # input this into a resulting dataframe
  se_df <- cbind(or_est = fixef(model), or_ll = fixef(model) - (1.96*se_est), or_ul = fixef(model) + (1.96*se_est))
  
  # return the exponentiated output
  print(exp(se_df))
}

# run models on individual-level ci responses
m1 <- glmer(inclusion ~ pol_c * group_c + (group_c|ProlificID) + (0 + group_c|stimID), family = binomial(), 
            control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_inv)
get_reg(m1)
# awesome -- people more likely to include 'ingroup' cis as outgroup members (cf 'outgroup' cis)


# get summary stats and make a plot
inv_sum <- s3_inv %>%
  group_by(participantPol, ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_ci') %>%
  rename(m_inclusion = mean)


# get summary stats per participant
inv_sum_p <- s3_inv %>%
  group_by(ProlificID, participantPol, ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_ci') %>%
  rename(m_inclusion = mean)


ggplot(inv_sum_p, aes(ciGroup, m_inclusion, fill = participantPol)) +
  # participant-level data
  geom_violin(alpha = .5, color = 'black') +
  geom_point(aes(color = participantPol), 
             size = .5, shape = 4, alpha = .7, position = position_jitterdodge(.15, .05, .9)) +
  # add in mean-level data
  geom_point(data = inv_sum, aes(ciGroup, m_inclusion), 
             size = 2, shape = 7, color = 'black', position = position_dodge(.9)) +
  geom_errorbar(data = inv_sum, aes(ciGroup, m_inclusion, ymin = m_inclusion - ci, ymax = m_inclusion + ci),
                width = .1, color = 'black', position = position_dodge(.9)) +
  theme_classic() +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c("Conservative\nParticipants' CIs", "Liberal\nParticipants' CIs")) +
  scale_color_manual(values = c('red', 'blue'),
                     labels = c("Conservative\nParticipants' CIs", "Liberal\nParticipants' CIs")) +
  labs(x = '',
       y = 'Mean Inclusion Rate',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')

# ggsave('study 3 - mean inclusion rate 2 x 2 - individual-level CIs.jpg', device = 'jpeg', units = 'cm', path = 'study 3 data')

# make another plot showing just the data by ci group
# get summary stats and make a plot
inv_sum_2 <- s3_inv %>%
  group_by(ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_ci') %>%
  rename(m_inclusion = mean)


# get summary stats per participant
inv_sum_p_2 <- s3_inv %>%
  group_by(ProlificID, ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_ci') %>%
  rename(m_inclusion = mean)


ggplot(inv_sum_p_2, aes(ciGroup, m_inclusion, 
                        #fill = ciGroup, color = ciGroup
                        )) +
  # participant-level data
  geom_violin(alpha = .8, color = 'black', fill = '#0b2265') +
  geom_point(size = .5, shape = 4, alpha = .7, position = position_jitter(.15, .05), color = '#a71930') +
  # add in mean-level data
  geom_point(data = inv_sum_2, aes(ciGroup, m_inclusion), 
             size = 2, shape = 7, color = 'white') +
  geom_errorbar(data = inv_sum_2, aes(ciGroup, m_inclusion, ymin = m_inclusion - ci, ymax = m_inclusion + ci),
                width = .1, color = 'white') +
  theme_classic() +
  geom_hline(yintercept = .5, color = 'blue') +
  # scale_fill_manual(values = c('red', 'blue')) +
  # scale_color_manual(values = c('red', 'blue')) +
  labs(x = '',
       y = 'Inclusion Rate',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'none')

# ggsave('study 3 - mean inclusion rate (ingroup v outgroup) - individual-level CIs.jpg', device = 'jpeg', units = 'cm', path = 'study 3')

# additional analyses to determine trends against chance
t.test(inv_sum_p_2$m_inclusion[inv_sum_p_2$ciGroup == 'ingroup'], mu = .5)
effsize::cohen.d(inv_sum_p_2$m_inclusion[inv_sum_p_2$ciGroup == 'ingroup'], f = rep(.5, each = nrow(inv_sum_p_2)))

t.test(inv_sum_p_2$m_inclusion[inv_sum_p_2$ciGroup == 'outgroup'], mu = .5)
effsize::cohen.d(inv_sum_p_2$m_inclusion[inv_sum_p_2$ciGroup == 'outgroup'], f = rep(.5, each = nrow(inv_sum_p_2)))

# load ratings data for individual-level cis to test any effects of perceived traits
inv_traits <- read.csv('ratings data - individual cis.csv')

# create new stimID column to match how stim IDs are saved in the current data (ci01, ci02, etc)
stim_ids <- paste0(
  rep('ci'),
  rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:105))
)

inv_traits$stim_id <- stim_ids


# left-join this by stimID
s3_inv_traits <- s3_inv %>%
  left_join(inv_traits, by = c('stimID' = 'stim_id')) %>%
  # don't need redundant politics/group columns
  select(-c(polParty, ciGroup.y, stimID.y))

colnames(s3_inv_traits)



# test effect of trustworthiness (conceptual replication of trustworthiness inclusion effect)
m2 <- glmer(inclusion ~ pol_c * group_c * trustworthy + (1|ProlificID) + (1|stimID), family = binomial(), 
            control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_inv_traits)
get_reg(m2) # only an effect of trustworthiness

# competence effect
m3 <- glmer(inclusion ~ pol_c * group_c * competent + (1|ProlificID) + (1|stimID), family = binomial(), 
            control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_inv_traits)
get_reg(m3) # only a competence effect

# morality effect
m4 <- glmer(inclusion ~ pol_c * group_c * moral + (1|ProlificID) + (1|stimID), family = binomial(), 
            control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_inv_traits)
get_reg(m4) # only a morality effect

# politics effect
m5 <- glmer(inclusion ~ pol_c * group_c * politics + (1|ProlificID) + (1|stimID), family = binomial(), 
            control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_inv_traits)
get_reg(m5) # only a politics effect (more liberal --> more inclusion)

# drop experimental factors and include only traits
m6 <- glmer(inclusion ~ trustworthy + dominant + warm + competent + openMinded + intelligent + moral + hardWorking + patriotic + masculine + afrocentric + politics + (1|ProlificID) + (1|stimID),
            family = binomial(), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_inv_traits)
get_reg(m6) # only an effect of masculinity (negative)



# test group-level ci responses
m1.2 <- glmer(inclusion ~ pol_c * group_c + (group_c * pol_c|ProlificID), family = binomial(), 
              control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = s3_group)
get_reg(m1.2)

# test effect of group at each level of political id
# liberal
m1.21 <- glmer(inclusion ~ group_c + (group_c|ProlificID), family = binomial(), 
               control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = filter(s3_group, pol_c == -1))
get_reg(m1.21) # people more likely to include liberals' ingroup cis than outgroup

# conservative
m1.22 <- glmer(inclusion ~ group_c + (group_c|ProlificID), family = binomial(), 
               control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e7)), data = filter(s3_group, pol_c == 1))
get_reg(m1.22) # people equally likely to include conservatives' ingroup and outgroup cis


# visualize
group_sum <- s3_group %>%
  group_by(participantPol, ciGroup) %>%
  get_summary_stats(inclusion, type = 'mean_ci') %>%
  rename(m_inclusion = mean)


# # get summary stats per participant
# group_sum_p <- s3_group %>%
#   group_by(ProlificID, participantPol, ciGroup) %>%
#   get_summary_stats(inclusion, type = 'mean_ci') %>%
#   rename(m_inclusion = mean)


ggplot(group_sum, aes(ciGroup, m_inclusion, fill = participantPol)) +
  geom_bar(stat = 'identity', position = position_dodge(.9), alpha = .8, color = 'black') +
  geom_errorbar(aes(ymin = m_inclusion - ci, ymax = m_inclusion + ci),
                width = .1, color = 'black', position = position_dodge(.9), alpha = .6) +
  theme_classic() +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c("Conservative\nParticipants' CIs", "Liberal\nParticipants' CIs")) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, .75, .05)) +
  expand_limits(y = .75) +
  labs(x = '',
       y = 'Mean Inclusion Rate',
       fill = '',
       color = '') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')






