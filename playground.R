# remove summary
require(plyr)
require(dplyr)
require(ggplot2)
require(RColorBrewer)

ev_df <- df %>% 
    filter(EVTYPE %in% tevt) %>%
    group_by(year(BGN_DATE), EVTYPE) %>%
    summarise(num_evt = n()) %>% 
    arrange(desc(num_evt)) %>%
    top_n(10, num_evt)  

names(ev_df) <- c("year", "eventType", "num_evt")
hp0 <- ggplot(ev_df, aes(eventType, num_evt, fill = year)) +
    geom_bar(stat = 'identity', position = 'stack') + 
    coord_flip() +
    scale_fill_continuous(name = "Year") +
    labs (title = "Most Frequent Storm Events"
          , x = "Storm Event Type"
          , y = "Number of Events")
print(hp0)

th_df <- df %>% 
    filter(EVTYPE %in% tevt) %>%
    group_by(year(BGN_DATE), EVTYPE) %>%
    summarise(sum_effect = sum(humEffect)) %>% 
    arrange(desc(sum_effect)) %>%
    top_n(10, sum_effect)  

names(th_df) <- c("year", "eventType", "sum_effect")
hp1 <- ggplot(th_df, aes(eventType, sum_effect, fill = year)) +
    geom_bar(stat = 'identity', position = 'stack') + 
    coord_flip() +
    scale_fill_continuous(name = "Year") +
    labs (title = "Human Effects (Fatalities + Injuries) from Storms"
          , x = "Storm Event Type"
          , y = "Number of Fatalities and Injuries")
print(hp1)


td_df <- df %>% 
    filter(EVTYPE %in% tevt) %>%
    group_by(year(BGN_DATE),EVTYPE) %>%
    summarise(sum_damage = sum(damage)) %>% 
    arrange(desc(sum_damage)) %>%
    top_n(10, sum_damage)
names(td_df) <- c("year", "eventType", "sum_damage")

hp2 <- ggplot(td_df, aes(eventType, sum_damage/1000000, fill = year)) +
    geom_bar(stat = 'identity', position = 'stack') + 
    coord_flip() +
    scale_fill_continuous(name = "Year") +
    labs (title = "Storm Damage (Property + Crop)"
          , x = "Storm Event Type"
          , y = "Damage in Dollars (Millions)")
print(hp2)
