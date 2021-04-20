library(rsconnect)
rsconnect::setAccountInfo(name='parkerseeg',
                          token='5F30A479AC5A2F13FFCC76D8FE6B900A',
                          secret='auzI3WDnryL2jJzOlhUiQEvK6xPpJyUtSYpLlQNj')
library(rsconnect)
rsconnect::deployApp('/cloud/project/nba-twitter-prospect-analysis')
