library(dplyr)

nc_vf = read.delim('data/ncvoter_Statewide.txt', sep='\t')

nc_vh = read.delim('data/ncvhis_Statewide.txt', sep='\t') %>%
  filter(election_lbl == '11/03/2020')

nc_vf = nc_vf %>%
  select(voter_reg_num, race_code, gender_code, birth_age, ethnic_code) %>%
  mutate(state_abb='NC') %>%
  mutate(race=case_when(race_code == 'W' ~ 'White',
                        race_code == 'B' ~ 'Black or African-American',
                        race_code == 'A' ~ 'Asian or Asian-American',
                        race_code == 'I' ~ 'other',
                        race_code == 'O' ~ 'other',
                        race_code == 'P' ~ 'other',
                        race_code == 'U' ~ 'other')) %>%
  mutate(race=case_when(ethnic_code == 'HL' ~ 'Hispanic or Latino',
                        TRUE ~ race)) %>%
  mutate(age_bucket=cut(birth_age, breaks=c(18, 29, 44, 65, Inf))) %>%
  mutate(gender=case_when(gender_code == 'M' ~ 'Male',
                          gender_code == 'F' ~ 'Female',
                          gender_code == 'U' ~ 'unknown')) %>%
  filter(gender != 'unknown') %>%
  select(voter_reg_num, state_abb, race, gender, age_bucket)

nc_vf = nc_vf %>%
  mutate(voted_2020=as.integer(voter_reg_num %in% nc_vh$voter_reg_num))

write.csv(nc_vf, 'data/nc_voterfile_processed.csv', row.names = FALSE)
