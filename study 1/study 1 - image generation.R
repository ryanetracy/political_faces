#################
# study 1
# ci generation
#################


library(rcicr)
library(tidyverse)

df <- read.csv('study 1/political image gen data.csv')

str(df)

# set columns to appropriate class type
df$idNo <- as.factor(df$idNo)
df$stimNum <- as.numeric(df$stimNum)
df$condition <- as.factor(df$condition)
df$polAffil <- as.factor(df$polAffil)


# in this dataset, condition refers to whether participants were tasked with generating images of liberals or conservatives
# polAffil is participants' own political affiliation
# thus, when polAffil and condition match, participants are generating ingroup cis
# when polAffil and condition don't match, participants are generating outgroup cis


# load image and RData for ci generation
baseimg <- 'base'
rDat <- 'study 1/stimuli/rcic_seed_1_time_Feb_17_2022_15_04.Rdata'
load(rDat)



# start with conservatives' ingroup cis
conIn <- df %>%
  filter(condition == 'conservative' & polAffil == 'conservative')

conIn %>% count(idNo) # 25 total participants

batchGenerateCI2IFC(conIn,
                    by = 'idNo',
                    stimuli = 'stimNum',
                    responses = 'response',
                    baseimage = baseimg,
                    rdata = rDat,
                    targetpath = 'study 1/cis/conservative - ingroup')


# conservatives' outgroup cis
conOut <- df %>%
  filter(condition == 'liberal' & polAffil == 'conservative')

conOut %>% count(idNo) # 26 total participants

batchGenerateCI2IFC(conOut,
                    by = 'idNo',
                    stimuli = 'stimNum',
                    responses = 'response',
                    baseimage = baseimg,
                    rdata = rDat,
                    targetpath = 'study 1/cis/conservative - outgroup')


# liberals' ingroup cis
libIn <- df %>%
  filter(condition == 'liberal' & polAffil == 'liberal')

libIn %>% count(idNo) # 24 total participants

batchGenerateCI2IFC(libIn,
                    by = 'idNo',
                    stimuli = 'stimNum',
                    responses = 'response',
                    baseimage = baseimg,
                    rdata = rDat,
                    targetpath = 'study 1/cis/liberal - ingroup')


# liberals' outgroup cis
libOut <- df %>%
  filter(condition == 'conservative' & polAffil == 'liberal')

libOut %>% count(idNo) # 30 total participants

batchGenerateCI2IFC(libOut,
                    by = 'idNo',
                    stimuli = 'stimNum',
                    responses = 'response',
                    baseimage = baseimg,
                    rdata = rDat,
                    targetpath = 'study 1/cis/liberal - outgroup')



# generate group-level cis
df <- df %>%
  mutate(group = case_when(
    condition == 'conservative' & polAffil == 'conservative' ~ 'conservativeIngroup',
    condition == 'liberal' & polAffil == 'conservative' ~ 'conservativeOutgroup',
    condition == 'liberal' & polAffil == 'liberal' ~ 'liberalIngroup',
    condition == 'conservative' & polAffil == 'liberal' ~ 'liberalOutgroup'
  ))

df$group <- as.factor(df$group)

batchGenerateCI2IFC(df,
                    by = 'group',
                    stimuli = 'stimNum',
                    responses = 'response',
                    baseimage = baseimg,
                    rdata = rDat,
                    targetpath = 'study 1/cis/group-level cis')
