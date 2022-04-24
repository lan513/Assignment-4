rm(list = ls())
# load packages
library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(ggsci)
# install.packages('R.utils')
library(R.utils)
unzip('WV6_Data_R_v20201117.rdata.zip')

# import data
load('WV6_Data_R_v20201117.rdata')

# the data for democracy
df_b <- WV6_Data_R_v20201117[, c('V2', paste0('V228', LETTERS[1:9]))] 
# rename
colnames(df_b) <- c('Code', 'Votes are counted fairly', 'Opposition candidates are prevented from running',
                    'TV news favors the governing party', 'Voters are bribed',
                    'Journalists provide fair coverage of elections', 'Election officials are fair',
                    'Rich people buy elections', 'Voters are threatened with violence at the polls', 
                    'Voters are offered a genuine choice in the elections')

# the data for news
df_c <- WV6_Data_R_v20201117[, paste0('V', c(2, 217:224))]
# rename
colnames(df_c) <- c('Code', 'Daily newspaper', 'Printed magazines', 'TV news', 
                    'Radio news', 'Mobile phone', 'Email', 'Internet', 
                    'Talk with friends or colleagues')
# the data for science
df_d <- WV6_Data_R_v20201117[, paste0('V', c(2, 192:197))]
# rename
colnames(df_d) <- c('Code', 'Science and technology are making our lives healthier, easier, and more comfortable',
                    'Because of science and technology, there will be more opportunities for the next generation',
                    'We depend too much on science and not enough on faith', 
                    'One of the bad effects of science is that it breaks down people’s ideas of right and wrong',
                    'It is not important for me to know about science in my daily life',
                    'The world is better off, or worse off, because of science and technology')
rm(WV6_Data_R_v20201117)  # clear the WV6_Data_R_v20201117, save space

# the data for country name
df_country <- read.table(text = 'Code;Country
12;Algeria
31;Azerbaijan
32;Argentina
36;Australia
48;Bahrain
51;Armenia
76;Brazil
112;Belarus
152;Chile
156;China
158;Taiwan
196;Cyprus (G)
218;Ecuador
233;Estonia
268;Georgia
275;Palestine
288;Ghana
332;Haiti
344;Hong Kong
356;India
368;Iraq
398;Kazakhstan
400;Jordan
414;Kuwait
417;Kyrgyzstan
422;Lebanon
434;Libya
458;Malaysia
484;Mexico
504;Morocco
554;New Zealand
566;Nigeria
586;Pakistan
604;Peru
608;Philippines
616;Poland
642;Romania
643;Russia
646;Rwanda
702;Singapore
710;South Africa
716;Zimbabwe
764;Thailand
788;Tunisia
804;Ukraine
818;Egypt
840;United States
858;Uruguay
860;Uzbekistan
887;Yemen
900;West Germany
901;East Germany', sep = ';', header = T)

# add country variable, remove value < 0 since it has no statistical analysis significance
df_b1 <- df_b %>% left_join(df_country) %>% # merege
  select(-Code) %>% # remove variable code
  pivot_longer(-Country, names_to = 'Question', values_to = 'Value') %>%
  mutate(Value = ifelse(Value > 0, Value, NA)) %>% na.omit() %>%  # remove outlier
  group_by(Question) %>%
  summarise(`Average Score` = round(mean(Value), 2))

df_b <- df_b %>% left_join(df_country) %>% # merege
  select(-Code) %>% # remove variable code
  pivot_longer(-Country, names_to = 'Question', values_to = 'Value') %>%
  mutate(Value = ifelse(Value > 0, Value, NA)) %>% na.omit() %>%  # remove outlier
  group_by(Country, Question) %>%
  summarise(`Average Score` = round(mean(Value), 2))

df_c <- df_c %>% left_join(df_country) %>% # merege
  select(-Code) %>% # remove variable code
  pivot_longer(-Country, names_to = 'Question', values_to = 'Value') %>%
  mutate(Value = factor(Value, levels = 1:5, 
                        labels = c('Daily', 'Weekly', 'Monthly', 'Less than monthly', 'Never'))) %>% 
  na.omit()  # remove outlier

df_c1 <- df_c %>% group_by(Question, Value) %>%
  count %>% right_join(
    df_c %>% group_by(Question) %>%
      summarise(N = n())
  ) %>%
  mutate(Proportion = round(n/N, 3)) %>%
  select(-n, -N) %>% rename(Frequency = Value)

df_c <- df_c %>% group_by( Country, Question, Value) %>%
  count %>% right_join(
    df_c %>% group_by(Country, Question) %>%
      summarise(N = n())
  ) %>%
  mutate(Proportion = round(n/N, 3)) %>%
  select(-n, -N) %>% rename(Frequency = Value)

# add country variable
df_d1 <- df_d %>% left_join(df_country) %>% # merege
  select(-Code) %>% # remove variable code
  pivot_longer(-Country, names_to = 'Question', values_to = 'Value') %>%
  mutate(Value = ifelse(Value > 0, Value, NA)) %>% na.omit() %>%  # remove outlier
  group_by(Question) %>%
  summarise(`Average Score` = round(mean(Value), 2))

df_d <- df_d %>% left_join(df_country) %>% # merege
  select(-Code) %>% # remove variable code
  pivot_longer(-Country, names_to = 'Question', values_to = 'Value') %>%
  mutate(Value = ifelse(Value > 0, Value, NA)) %>% na.omit() %>%  # remove outlier
  group_by(Country, Question) %>%
  summarise(`Average Score` = round(mean(Value), 2))
