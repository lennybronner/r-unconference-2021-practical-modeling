

county_election_results = read_csv('data/potus_2020_county.csv') %>%
  filter(`Geographic Subtype` == 'County') %>% # there is an additional header row that we drop
  select(FIPS, `Total Vote`, `Joseph R. Biden Jr.`, `Donald J. Trump`) %>%
  rename(total_vote=`Total Vote`, biden_vote=`Joseph R. Biden Jr.`, trump_vote=`Donald J. Trump`, county_fips=FIPS) %>%
  mutate(total_vote=as.integer(total_vote), trump_vote=as.integer(trump_vote), biden_vote=as.integer(biden_vote)) %>%
  mutate(county_fips=str_pad(county_fips, 5, pad='0')) %>%
  mutate(trump_vote_frac=trump_vote/total_vote, biden_vote_frac=biden_vote/total_vote)

state_election_results = county_election_results %>%
  mutate(state_fips=str_sub(county_fips, 1, 2)) %>%
  group_by(state_fips) %>%
  summarize(total_vote=sum(total_vote),
            biden_vote=sum(biden_vote),
            trump_vote=sum(trump_vote),
            .groups='drop') %>%
  mutate(trump_vote_frac=trump_vote/total_vote, biden_vote_frac=biden_vote/total_vote) %>%
  select(state_fips, trump_vote_frac) %>%
  rename(trump_vote_frac_state=trump_vote_frac)

county_election_results = county_election_results %>%
  select(county_fips, trump_vote_frac) %>%
  rename(trump_vote_frac_county=trump_vote_frac)

# state average income is from census
# region is taken from census
# state pvi is taken from wikipedia
additional_state_data = read_csv('data/additional_state_data.csv') %>%
  select(state_fips, z.average_income, pvi, region)

state_abbrevations = read_csv('data/state_abbreviations.csv')

cces = read_csv('data/CCES20_Common_OUTPUT.csv') %>%
  select(commonpostweight, CC20_401, CC20_410, birthyr, gender, race, educ, inputstate, countyfips) %>%
  filter(CC20_401 == 5) %>% # CC20_401 is whether they voted, CC20_401 == 5 is yes
  mutate(age=2020-birthyr) %>%
  mutate(age_bucket=cut(age, breaks=c(18, 29, 44, 65, Inf))) %>% # bucket ages
  select(-age) %>%
  select(-birthyr) %>%
  mutate(state_fips=str_pad(inputstate, width=2, pad='0')) %>%
  select(-inputstate) %>%
  mutate(county_fips=countyfips) %>%
  select(-countyfips) %>%
  mutate(gender=case_when(gender == 1 ~ "Male",
                          gender == 2 ~ "Female",
                          gender == 8 ~ "unknown",
                          gender == 9 ~ "unknown")) %>%
  mutate(race=case_when(race == 1 ~ "White", # White
                        race == 2 ~ "Black or African-American", # Black or African-American
                        race == 3 ~ "Hispanic or Latino", # Hispanic or Latino
                        race == 4 ~ "Asian or Asian-American", # Asian or Asian-American
                        race == 5 ~ "other", # Native American, which we change to other because L2
                        race == 6 ~ "other", # Middle Eastern, which we change to other because L2
                        race == 7 ~ "other", # Other
                        race == 8 ~ "other", # Middle Eastern, which we change to other because L2
                        race == 98 ~ "unknown", # Skipped
                        race == 99 ~ "unknown")) %>% # not asked 
  mutate(educ=case_when(educ == 1 ~ "No college",
                        educ == 2 ~ "No college",
                        educ == 3 ~ "No college",
                        educ == 4 ~ "college",
                        educ == 5 ~ "college",
                        educ == 6 ~ "postgrad")) %>%
  filter(CC20_410 == 1 | CC20_410 == 2) %>% # this is who they voted for, 1 is Biden and 2 is Trump
  mutate(voted_trump= as.integer(CC20_410 == 2)) %>% 
  tidylog::left_join(additional_state_data, by='state_fips') %>%
  tidylog::left_join(county_election_results, by='county_fips') %>%
  tidylog::left_join(state_election_results, by='state_fips') %>%
  mutate(state_abb=fips(state_fips, to='Abbreviation')) %>%
  drop_na(gender, race, age_bucket)
