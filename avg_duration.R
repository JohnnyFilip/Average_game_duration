library(jsonlite)
library(plotly)
library(magrittr)
library(plyr)

# AVerage game duration by patch

avg1 <- fromJSON("https://api.opendota.com/api/explorer?sql=SELECT%0Apatch%20%2C%0Around(sum(duration)%3A%3Anumeric%2Fcount(1)%2C%202)%20avg%2C%0Acount(distinct%20matches.match_id)%20count%2C%0Asum(case%20when%20(player_matches.player_slot%20%3C%20128)%20%3D%20radiant_win%20then%201%20else%200%20end)%3A%3Afloat%2Fcount(1)%20winrate%2C%0Asum(duration)%20sum%2C%0Amin(duration)%20min%2C%0Amax(duration)%20max%2C%0Around(stddev(duration)%2C%202)%20stddev%0AFROM%20matches%0AJOIN%20match_patch%0AUSING%20(match_id)%0AJOIN%20leagues%0AUSING(leagueid)%0AJOIN%20player_matches%0AUSING(match_id)%0ALEFT%20JOIN%20notable_players%0AUSING(account_id)%0ALEFT%20JOIN%20teams%0AUSING(team_id)%0AJOIN%20heroes%0AON%20player_matches.hero_id%20%3D%20heroes.id%0AWHERE%20TRUE%0AAND%20duration%20IS%20NOT%20NULL%0AGROUP%20BY%20patch%0AHAVING%20count(distinct%20matches.match_id)%20%3E%200%0AORDER%20BY%20avg%20DESC%2Ccount%20DESC%20NULLS%20LAST%0ALIMIT%20150")
avg1a <- avg1$rows %>% arrange(desc(patch))

print(avg1a[avg1a$patch > 7.00,])

avg_patch <- mean(as.numeric(avg1a[avg1a$patch > 7.00,]$avg))
print(avg_patch)

p_avg_patch <-
  plot_ly(
    avg1a,
    x = avg1a$patch,
    y = as.numeric(avg1a$avg)/60,
    name = 'Avg_game_duration',
    type = 'bar',
    marker = list(
      color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',
                  width = 1.5)
    )
  ) %>%
layout(yaxis = list(range = c(32,44), title = 'Average_game_duration'), xaxis = list(title = 'Patch'))

# Average game duration by league

avg2 <- fromJSON("https://api.opendota.com/api/explorer?sql=SELECT%0Aleagues.name%20leaguename%2C%0Around(sum(duration)%3A%3Anumeric%2Fcount(1)%2C%202)%20avg%2C%0Acount(distinct%20matches.match_id)%20count%0AFROM%20matches%0AJOIN%20match_patch%0AUSING%20(match_id)%0AJOIN%20leagues%0AUSING(leagueid)%0AJOIN%20player_matches%0AUSING(match_id)%0ALEFT%20JOIN%20notable_players%0AUSING(account_id)%0ALEFT%20JOIN%20teams%0AUSING(team_id)%0AJOIN%20heroes%0AON%20player_matches.hero_id%20%3D%20heroes.id%0AWHERE%20TRUE%0AAND%20duration%20IS%20NOT%20NULL%0AAND%20match_patch.patch%20%3D%20%277.00%27%20OR%20match_patch.patch%20%3D%20%277.01%27%20OR%20match_patch.patch%20%3D%20%277.02%27%0AGROUP%20BY%20leagues.name%0AHAVING%20count(distinct%20matches.match_id)%20%3E%200%0AORDER%20BY%20avg%20DESC%2Ccount%20DESC%20NULLS%20LAST%0ALIMIT%20150")
avg2a <- avg2$rows %>% arrange(desc(avg))
avg2a$average <- as.numeric(avg2a$avg)/60

avg_league <- avg2a[avg2a$count >= 26,] %>% subset(select = -avg)

print(avg_league)

# AVerage game duration by team

avg3 <- fromJSON("https://api.opendota.com/api/explorer?sql=SELECT%0Ateams.name%20%2C%0Around(sum(duration)%3A%3Anumeric%2Fcount(1)%2C%202)%20avg%2C%0Acount(distinct%20matches.match_id)%20count%2C%0Asum(case%20when%20(player_matches.player_slot%20%3C%20128)%20%3D%20radiant_win%20then%201%20else%200%20end)%3A%3Afloat%2Fcount(1)%20winrate%0AFROM%20matches%0AJOIN%20match_patch%0AUSING%20(match_id)%0AJOIN%20leagues%0AUSING(leagueid)%0AJOIN%20player_matches%0AUSING(match_id)%0ALEFT%20JOIN%20notable_players%0AUSING(account_id)%0ALEFT%20JOIN%20teams%0AUSING(team_id)%0AJOIN%20heroes%0AON%20player_matches.hero_id%20%3D%20heroes.id%0AWHERE%20TRUE%0AAND%20duration%20IS%20NOT%20NULL%0AAND%20match_patch.patch%20%3D%20%277.00%27%20OR%20match_patch.patch%20%3D%20%277.01%27%20OR%20match_patch.patch%20%3D%20%277.02%27%0AGROUP%20BY%20teams.name%0AHAVING%20count(distinct%20matches.match_id)%20%3E%200%0AORDER%20BY%20avg%20DESC%2Ccount%20DESC%20NULLS%20LAST%0ALIMIT%20150")
avg3a <- avg3$rows %>% arrange(desc(avg))
avg3a$average <- as.numeric(avg3a$avg)/60 

avg_match <- avg3a[avg3a$count >= 10,] %>% subset(select = -avg)

print(avg_match)

write.csv(avg1a, file = 'D:\\RTS\\Scripts\\avg_patch.csv')
write.table(avg_league, file = 'D:\\RTS\\Scripts\\avg_league.csv', sep = ' ', row.names = FALSE)
write.table(avg_match, file = 'D:\\RTS\\Scripts\\avg_match.csv', sep = ' ', row.names = FALSE)
