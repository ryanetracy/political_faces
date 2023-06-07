##################
# study 1 
# lib/con cis
# ratings task
##################


# packages
pckgs <- c('lme4', 'lmerTest', 'afex', 'emmeans', 'rstatix', 'effectsize', 'interactions', 'ggcorrplot', 'correlation', 'psych', 'tidyverse')

# check installation and load
for (i in 1:length(pckgs)) {
  if(!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}


# data
df <- read.csv('study 1/study 1 - ratings data.csv')


#### data cleaning ####


# rename condition column (FL_10_DO)

names(df)[1653] <- 'faceBlock'


# how many participants completed the study?
df %>%
  count(Progress)

# remove participants with less than 97% completion
df <- df %>%
  filter(Progress >= 97)


# how many unique prolific ids?
df %>%
  count(prolificID)

# remove blank ids
blankCheck <- df %>% count(prolificID) %>% filter(n == 1)
df <- df %>% filter(prolificID %in% blankCheck$prolificID)

# remove any participants who did not give consent
df <- df %>% filter(consent == 1)

# check demographics
# age
df %>%
  get_summary_stats(age, type = 'mean_sd')

# gender
df %>%
  count(gender) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(gender = case_when(
    gender == 1 ~ 'male',
    gender == 2 ~ 'female',
    gender == 3 ~ 'nonbinary',
    gender == 4 ~ 'other'
  ))

# race
df %>%
  count(race) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(race = case_when(
    race == 1 ~ 'asian',
    race == 2 ~ 'black',
    race == 3 ~ 'latino/a',
    race == 4 ~ 'native american',
    race == 5 ~ 'middle eastern',
    race == 6 ~ 'pacific islander',
    race == 7 ~ 'white',
    race == 8 ~ 'bi/multiracial',
    race == 9 ~ 'other'
  ))

# politics
df %>%
  get_summary_stats(polOrient_1, type = 'mean_sd')


# remove unneeded columns
df <- df %>%
  select(-contains(c('Date', 'Status', 'IPAddress', 'Progress', 'Duration', 'Finished', 'consent',
                     'ResponseId','Recipient', 'External', 'Location', 'Channel', 'Language', '_DO')))


# get column names
names(df)[1:755]
names(df)[756:1510]


# separate the different counterbalance blocks
b1 <- df %>%
  select(prolificID, X1_traits_1:X25_politics_1, age, gender, race, polOrient_1, faceBlock) %>%
  filter(faceBlock == 'faces1')

b2 <- df %>%
  select(prolificID, X1_traits_1.1:X25_politics_1.1, age, gender, race, polOrient_1, faceBlock) %>%
  filter(faceBlock == 'faces2')

b3 <- df %>%
  select(prolificID, X1_traits_1.2:X25_politics_1.2, age, gender, race, polOrient_1, faceBlock) %>%
  filter(faceBlock == 'faces3')

b4 <- df %>%
  select(prolificID, X1_traits_1.3:X25_politics_1.3, age, gender, race, polOrient_1, faceBlock) %>%
  filter(faceBlock == 'faces4')

b5 <- df %>%
  select(prolificID, X1_traits_1.4:X25_politics_1.4, age, gender, race, polOrient_1, faceBlock) %>%
  filter(faceBlock == 'faces5')


# now create vectors of new names for the columns
# this is tricky, as the number of cis per condition is slightly unbalanced

# block 1 - conservative - conservative
b1ConIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('001', '002', '003', '004', '005'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 1 - conservative - liberal
b1ConOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('026', '027', '028', '029', '030'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 1 - liberal - liberal
b1LibIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('052', '053', '054', '055', '056'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 1 - liberal - conservative
b1LibOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('076', '077', '078', '079', '080', '081'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# apply the name vectors to the columns of the first block
names(b1)[2:61] <- b1ConIn
names(b1)[62:121] <- b1ConOut
names(b1)[122:181] <- b1LibIn
names(b1)[182:253] <- b1LibOut


# now do the same for block 2
# block 2 - conservative - conservative
b2ConIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('006', '007', '008', '009', '010'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 2 - conservative - liberal
b2ConOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('031', '032', '033', '034', '035'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 2 - liberal - liberal
b2LibIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('057', '058', '059', '060', '061'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 2 - liberal - conservative
b2LibOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('082', '083', '084', '085', '086', '087'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# apply the name vectors to the columns of the second block
names(b2)[2:61] <- b2ConIn
names(b2)[62:121] <- b2ConOut
names(b2)[122:181] <- b2LibIn
names(b2)[182:253] <- b2LibOut


# now block 3 
# block 3 - conservative - conservative
b3ConIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('011', '012', '013', '014', '015'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 3 - conservative - liberal
b3ConOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('036', '037', '038', '039', '040'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 3 - liberal - liberal
b3LibIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('062', '063', '064', '065', '066'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 3 - liberal - conservative
b3LibOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('088', '089', '090', '091', '092', '093'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# apply the name vectors to the columns of the second block
names(b3)[2:61] <- b3ConIn
names(b3)[62:121] <- b3ConOut
names(b3)[122:181] <- b3LibIn
names(b3)[182:253] <- b3LibOut


# now block 4
# block 4 - conservative - conservative
b4ConIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('016', '017', '018', '019', '020'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 4 - conservative - liberal
b4ConOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('041', '042', '043', '044', '045'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 4 - liberal - liberal
b4LibIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('067', '068', '069', '070', '071'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 4 - liberal - conservative
b4LibOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('094', '095', '096', '097', '098', '099'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# apply the name vectors to the columns of the second block
names(b4)[2:61] <- b4ConIn
names(b4)[62:121] <- b4ConOut
names(b4)[122:181] <- b4LibIn
names(b4)[182:253] <- b4LibOut


# and block 5
# block 5 - conservative - conservative
b5ConIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('021', '022', '023', '024', '025'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 5 - conservative - liberal
b5ConOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('046', '047', '048', '049', '050', '051'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('ConOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 5 - liberal - liberal
b5LibIn <- paste0(
  # stim IDs
  rep('stim'),
  rep(c('072', '073', '074', '075'), each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibIn'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# block 5 - liberal - conservative
b5LibOut <- paste0(
  # stim IDs
  rep('stim'),
  rep(100:105, each = 12), # 12 ratings per stimulus
  
  # stimulus type (conservative cis by conservative participants)
  rep('LibOu'),
  
  # traits
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# apply the name vectors to the columns of the second block
names(b5)[2:61] <- b5ConIn
names(b5)[62:133] <- b5ConOut
names(b5)[134:181] <- b5LibIn
names(b5)[182:253] <- b5LibOut


# finally, make a name vector for the group-level cis [254:301]
groupNames <- paste0(
  rep('gl'),
  rep(1:4, each = 12),
  
  rep(c('ConIn', 'ConOu', 'LibIn', 'LibOu'), each = 12),
  
  rep(c('Tr', 'Do', 'Wm', 'Co', 'OM', 'In', 'Mo', 'HW', 'Pa', 'Ms', 'Af', 'PO'))
)

# apply to each df
names(b1)[254:301] <- groupNames
names(b2)[254:301] <- groupNames
names(b3)[254:301] <- groupNames
names(b4)[254:301] <- groupNames
names(b5)[254:301] <- groupNames



# now reshape each one
b1L <- b1 %>%
  pivot_longer(cols = stim001ConInTr:gl4LibOuPO,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'ciGroup'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'polParty'), sep = -3) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


b2L <- b2 %>%
  pivot_longer(cols = stim006ConInTr:gl4LibOuPO,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'ciGroup'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'polParty'), sep = -3) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


b3L <- b3 %>%
  pivot_longer(cols = stim011ConInTr:gl4LibOuPO,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'ciGroup'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'polParty'), sep = -3) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


b4L <- b4 %>%
  pivot_longer(cols = stim016ConInTr:gl4LibOuPO,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'ciGroup'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'polParty'), sep = -3) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


b5L <- b5 %>%
  pivot_longer(cols = stim021ConInTr:gl4LibOuPO,
               names_to = 'stimID', values_to = 'rating') %>%
  separate(col = 'stimID', into = c('stimID', 'trait'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'ciGroup'), sep = -2) %>%
  separate(col = 'stimID', into = c('stimID', 'polParty'), sep = -3) %>%
  pivot_wider(names_from = 'trait', values_from = 'rating')


# merge these into a single df
dfL <- rbind(b1L, b2L, b3L, b4L, b5L)

# rename traits columns
names(dfL)[10:21] <- c('trustworthy', 'dominant', 'warm', 'competent', 'openMinded', 'intelligent', 'moral', 'hardWorking', 'patriotic', 'masculine', 'afrocentric', 'politics')


# rename participant's political orientation column
names(dfL)[5] <- 'politicalOrientation'

# set the id, stimid, block, gen politics, ci group columns as factors
dfL <- dfL %>% mutate_at(c('prolificID', 'stimID', 'faceBlock', 'polParty', 'ciGroup'), .funs = as.factor)
sapply(dfL, class)





#### data analyses ####
# focus on the individual-level CIs first
groupCIs <- c('gl1', 'gl2', 'gl3', 'gl4')
dfInv <- dfL %>% filter(!(stimID %in% groupCIs))

# first check the correlations across the ratings items
dfCorrs <- dfInv %>%
  group_by(prolificID) %>%
  summarize(
    trustworthy = mean(trustworthy, na.rm = T),
    dominant = mean(dominant, na.rm = T),
    warm = mean(warm, na.rm = T),
    competent = mean(competent, na.rm = T),
    openMinded = mean(openMinded, na.rm = T),
    intelligent = mean(intelligent, na.rm = T),
    moral = mean(moral, na.rm = T),
    hardWorking = mean(hardWorking, na.rm = T),
    patriotic = mean(patriotic, na.rm = T),
    masculine = mean(masculine, na.rm = T),
    afrocentric = mean(afrocentric, na.rm = T),
    politics = mean(politics, na.rm = T)
  )

cors <- corr.test(dfCorrs[,2:13])

ggcorrplot(cors$r,
           type = 'lower',
           p.mat = cors$p,
           lab = T)


# create contrast codes for lmm models
dfInv <- dfInv %>%
  mutate(partyC = ifelse(polParty == 'Con', -1, 1),
         groupC = ifelse(ciGroup == 'In', 1, -1))

# conduct analyses for the traits
# run stimulus-level analyses
stim_level <- dfL %>%
  group_by(stimID, polParty, ciGroup) %>%
  summarize(
    trustworthy = mean(trustworthy, na.rm = T),
    dominant = mean(dominant, na.rm = T),
    warm = mean(warm, na.rm = T),
    competent = mean(competent, na.rm = T),
    openMinded = mean(openMinded, na.rm = T),
    intelligent = mean(intelligent, na.rm = T),
    moral = mean(moral, na.rm = T),
    hardWorking = mean(hardWorking, na.rm = T),
    patriotic = mean(patriotic, na.rm = T),
    masculine = mean(masculine, na.rm = T),
    afrocentric = mean(afrocentric, na.rm = T),
    politics = mean(politics, na.rm = T)
  ) %>%
  filter(!(stimID %in% groupCIs)) %>%
  as.data.frame()

# manova on traits
full_manova <- manova(cbind(trustworthy,
                            dominant,
                            warm,
                            competent,
                            openMinded,
                            intelligent,
                            moral,
                            hardWorking,
                            patriotic,
                            masculine,
                            afrocentric,
                            politics) ~ ciGroup * polParty, data = stim_level)
summary(full_manova, 'Wilks')
# univariate tests
summary.aov(full_manova)

# get means of all trait ratings across groups
group_means <- stim_level %>%
  group_by(ciGroup, polParty) %>%
  rstatix::get_summary_stats(trustworthy,
                             dominant,
                             warm,
                             competent,
                             openMinded,
                             intelligent,
                             moral,
                             hardWorking,
                             patriotic,
                             masculine,
                             afrocentric,
                             politics,
                             type = 'mean_sd')
group_means


# function for estimating cohen's d
get_cohen_d <- function(m_1, m_2, sd_1, sd_2){
  # first get pooled standard deviation
  pooled_sd <- sqrt((sd_1^2 + sd_2^2)/2)
  # input this into cohen d formula
  cohen_d_out <- (m_2 - m_1)/pooled_sd
  return(cohen_d_out)
}

trait_labs <- unique(group_means$variable)

# get values from group means
con_in <- group_means %>% filter(polParty == 'Con' & ciGroup == 'In')
con_out <- group_means %>% filter(polParty == 'Con' & ciGroup == 'Ou')

# get d values
con_d_vals <- cbind(
  trait_labs,
  round(
    get_cohen_d(
      m_1 = con_out$mean,
      m_2 = con_in$mean,
      sd_1 = con_out$sd, 
      sd_2 = con_in$sd
    ), 2)
)
con_d_vals

lib_in <- group_means %>% filter(polParty == 'Lib' & ciGroup == 'In')
lib_out <- group_means %>% filter(polParty == 'Lib' & ciGroup == 'Ou')

lib_d_vals <- cbind(
  trait_labs,
  round(
    get_cohen_d(
      m_1 = lib_out$mean,
      m_2 = lib_in$mean,
      sd_1 = lib_out$sd, 
      sd_2 = lib_in$sd
    ), 2)
)
lib_d_vals

# fully crossed models (i.e., analyses at trial-level)
# trustworthy
mTr <- lmer(trustworthy ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mTr)

t_to_d(t = c(.943, 5.659, 3.698),
       df_error = c(96.435, 96.437, 96.393),
       paired = T)

sim_slopes(mTr, pred = 'groupC', modx = 'partyC')

t_to_d(t = c(1.37, 6.69),
       df_error = 96.393,
       paired = T)

# dominant
mDom <- lmer(dominant ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mDom)

t_to_d(t = c(-.093, -2.895, -3.276),
       df_error = c(96.469, 96.472, 96.422),
       paired = T)

sim_slopes(mDom, pred = 'groupC', modx = 'partyC')

t_to_d(t = c(.26, -4.41),
       df_error = 96.422,
       paired = T)

# warm
mWarm <- lmer(warm ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mWarm)

t_to_d(t = c(1.129, 5.135, 4.164),
       df_error = c(97.642, 97.643, 97.573),
       paired = T)

sim_slopes(mWarm, pred = 'groupC', modx = 'partyC')

t_to_d(t = c(.68, 6.65),
       df_error = 96.422,
       paired = T)

# competent
mComp <- lmer(competent ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mComp)

t_to_d(t = c(1.567, 2.080, 2.953),
       df_error = c(96.141, 96.145, 96.119),
       paired = T)

sim_slopes(mComp, pred = 'groupC', modx = 'partyC')

t_to_d(t = c(1.49, 5.75),
       df_error = 96.422,
       paired = T)

# open-minded
mOpen <- lmer(openMinded ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mOpen)

t_to_d(t = c(.087, 5.665, 42.88),
       df_error = c(96.97, 96.97, 96.92),
       paired = T)

sim_slopes(mOpen, pred = 'groupC', modx = 'partyC')

t_to_d(t = c(.96, 7.12),
       df_error = 96.92,
       paired = T)

# intelligent
mIntel <- lmer(intelligent ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mIntel)

sim_slopes(mIntel, pred = 'groupC', modx = 'partyC')

# moral
mMoral <- lmer(moral ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mMoral)

sim_slopes(mMoral, pred = 'groupC', modx = 'partyC')

# hard-working
mHardW <- lmer(hardWorking ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mHardW)

sim_slopes(mHardW, pred = 'groupC', modx = 'partyC')

# patriotic
mPat <- lmer(patriotic ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mPat)

sim_slopes(mPat, pred = 'groupC', modx = 'partyC')

# masculine
mMasc <- lmer(masculine ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mMasc)

sim_slopes(mMasc, pred = 'groupC', modx = 'partyC')

# afrocentric
mAfro <- lmer(afrocentric ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mAfro)

sim_slopes(mAfro, pred = 'groupC', modx = 'partyC')

# politics
mPol <- lmer(politics ~ partyC * groupC + (1|prolificID) + (1|stimID), data = dfInv)
summary(mPol)

sim_slopes(mPol, pred = 'groupC', modx = 'partyC')


# prep data for saving to be used in study 2 and 3
invExp <- dfInv %>%
  select(prolificID, stimID, polParty, ciGroup, trustworthy:politics) %>%
  group_by(stimID, polParty, ciGroup) %>%
  summarize(
    trustworthy = mean(trustworthy, na.rm = T),
    dominant = mean(dominant, na.rm = T),
    warm = mean(warm, na.rm = T),
    competent = mean(competent, na.rm = T),
    openMinded = mean(openMinded, na.rm = T),
    intelligent = mean(intelligent, na.rm = T),
    moral = mean(moral, na.rm = T),
    hardWorking = mean(hardWorking, na.rm = T),
    patriotic = mean(patriotic, na.rm = T),
    masculine = mean(masculine, na.rm = T),
    afrocentric = mean(afrocentric, na.rm = T),
    politics = mean(politics, na.rm = T)
  ) %>%
  mutate(polParty = ifelse(polParty == 'Lib', 'liberal', 'conservative'),
         ciGroup = ifelse(ciGroup == 'In', 'ingroup', 'outgroup'))

# save to csv file
# write.csv(invExp, 'ratings data - individual cis.csv', row.names = F)

# get the means and cis of the traits for plotting
invSummary <- dfInv %>%
  group_by(polParty, ciGroup) %>%
  get_summary_stats(trustworthy, dominant, warm, competent,
                    openMinded, intelligent, moral, hardWorking,
                    patriotic, masculine, afrocentric, politics,
                    type = 'mean_ci')

invSummary$variable <- factor(invSummary$variable, labels = c('Trustworthy', 'Dominant', 'Warm', 'Competent',
                                                              'OpenMinded', 'Intelligent', 'Moral', 'HardWorking',
                                                              'Patriotic', 'Masculine', 'Afrocentric', 'Politics'))


# filter out only the traits
invTraits <- invSummary %>% filter(!(variable %in% c('Masculine', 'Afrocentric', 'Politics')))

ggplot(invTraits, aes(ciGroup, mean, fill = polParty)) +
  geom_bar(stat = 'identity', color = 'black', alpha = .8, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = .25, alpha = .8, position = position_dodge(.9)) +
  theme_classic() +
  facet_wrap(~ variable) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(1, 10.5, 1)) +
  expand_limits(y = 10.5) +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c('Conservative', 'Liberal')) +
  labs(x = '',
       y = 'Rating',
       fill = 'Participant Political Orientation') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')


# now demographics
invDemos <- invSummary %>% filter(variable %in% c('Masculine', 'Afrocentric', 'Politics'))

ggplot(invDemos, aes(ciGroup, mean, fill = polParty)) +
  geom_bar(stat = 'identity', color = 'black', alpha = .8, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = .25, alpha = .8, position = position_dodge(.9)) +
  theme_classic() +
  facet_wrap(~ variable) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 10.5, 1)) +
  expand_limits(y = 10.5) +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c('Conservative', 'Liberal')) +
  labs(x = '',
       y = 'Rating',
       fill = 'Participant Political Orientation') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')



### conduct analyses of the group-level CIs
dfGroup <- dfL %>% filter(stimID %in% groupCIs)

# correlations
dfCorrsG <- dfGroup %>%
  group_by(prolificID) %>%
  summarize(
    trustworthy = mean(trustworthy, na.rm = T),
    dominant = mean(dominant, na.rm = T),
    warm = mean(warm, na.rm = T),
    competent = mean(competent, na.rm = T),
    openMinded = mean(openMinded, na.rm = T),
    intelligent = mean(intelligent, na.rm = T),
    moral = mean(moral, na.rm = T),
    hardWorking = mean(hardWorking, na.rm = T),
    patriotic = mean(patriotic, na.rm = T),
    masculine = mean(masculine, na.rm = T),
    afrocentric = mean(afrocentric, na.rm = T),
    politics = mean(politics, na.rm = T)
  )

corsG <- corr.test(dfCorrsG[,2:13])

ggcorrplot(corsG$r,
           type = 'lower',
           p.mat = corsG$p,
           lab = T)


# trustworthy
anova_test(trustworthy ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(trustworthy ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(trustworthy ~ polParty, paired = T, correction = 'bonferroni')

# dominant
anova_test(dominant ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(dominant ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(dominant ~ polParty, paired = T, correction = 'bonferroni')

# warm
anova_test(warm ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(warm ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(warm ~ polParty, paired = T, correction = 'bonferroni')

# competent
anova_test(competent ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(competent ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(competent ~ polParty, paired = T, correction = 'bonferroni')

# open minded
anova_test(openMinded ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(openMinded ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(openMinded ~ polParty, paired = T, correction = 'bonferroni')

# intelligent
anova_test(intelligent ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(intelligent ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(intelligent ~ polParty, paired = T, correction = 'bonferroni')

# moral
anova_test(moral ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(moral ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(moral ~ polParty, paired = T, correction = 'bonferroni')

# hard working
anova_test(hardWorking ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(hardWorking ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(hardWorking ~ polParty, paired = T, correction = 'bonferroni')

# patriotic
anova_test(patriotic ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(patriotic ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(patriotic ~ polParty, paired = T, correction = 'bonferroni')

# masculine
anova_test(masculine ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(masculine ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(masculine ~ polParty, paired = T, correction = 'bonferroni')

# afrocentric
anova_test(afrocentric ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(afrocentric ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(afrocentric ~ polParty, paired = T, correction = 'bonferroni')

# politics
anova_test(politics ~ polParty * ciGroup + Error(prolificID/polParty * ciGroup), data = dfGroup)

dfGroup %>%
  group_by(ciGroup) %>%
  anova_test(politics ~ polParty + Error(prolificID/polParty))

dfGroup %>% group_by(ciGroup) %>% pairwise_t_test(politics ~ polParty, paired = T, correction = 'bonferroni')


# get the means and cis of the traits for plotting
groupSummary <- dfGroup %>%
  group_by(polParty, ciGroup) %>%
  get_summary_stats(trustworthy, dominant, warm, competent,
                    openMinded, intelligent, moral, hardWorking,
                    patriotic, masculine, afrocentric, politics,
                    type = 'mean_ci')

groupSummary$variable <- factor(groupSummary$variable, labels = c('Trustworthy', 'Dominant', 'Warm', 'Competent',
                                                                  'OpenMinded', 'Intelligent', 'Moral', 'HardWorking',
                                                                  'Patriotic', 'Masculine', 'Afrocentric', 'Politics'))



# filter out only the traits
groupTraits <- groupSummary %>% filter(!(variable %in% c('Masculine', 'Afrocentric', 'Politics')))

ggplot(groupTraits, aes(ciGroup, mean, fill = polParty)) +
  geom_bar(stat = 'identity', color = 'black', alpha = .8, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = .25, alpha = .8, position = position_dodge(.9)) +
  theme_classic() +
  facet_wrap(~ variable) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 10.5, 1)) +
  expand_limits(y = 10.5) +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c('Conservative', 'Liberal')) +
  labs(x = '',
       y = 'Rating',
       fill = 'Participant Political Orientation') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')


# now demographics
groupDemos <- groupSummary %>% filter(variable %in% c('Masculine', 'Afrocentric', 'Politics'))

ggplot(groupDemos, aes(ciGroup, mean, fill = polParty)) +
  geom_bar(stat = 'identity', color = 'black', alpha = .8, position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = .25, alpha = .8, position = position_dodge(.9)) +
  theme_classic() +
  facet_wrap(~ variable) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 10.5, 1)) +
  expand_limits(y = 10.5) +
  scale_fill_manual(values = c('red', 'blue'),
                    labels = c('Conservative', 'Liberal')) +
  labs(x = '',
       y = 'Rating',
       fill = 'Participant Political Orientation') +
  scale_x_discrete(labels = c('Ingroup CIs', 'Outgroup CIs')) +
  theme(legend.position = 'top')









