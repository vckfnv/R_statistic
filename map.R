setwd('c:/Rtest/project')
library(readxl)

world <- read.csv('world.csv',header=T)

library(plotly)

add_sf()
install.packages("sf")
library(sf)
map2 <- plot_geo() %>% 
  layout(geo = list(projection = list(type = "mercator"))) %>% 
  add_sf(
    data = world, 
    color = ~suic,
    split = ~address, 
    text = ~paste(address, scales::number_si(suic)), 
    hoverinfo = "text", 
    hoveron = "fills")

library(htmltools)
browsable(map2)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
names(df)


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250))
fig <- fig %>% add_markers(
  x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
  text = ~paste(df$name, "<br />", df$pop/1e6, " million")
)
fig <- fig %>% layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

fig

library(maps)
dfb <- world.cities[world.cities$country.etc=="Brazil",]
library(plotly)
dfb$poph <- paste(dfb$name, "Pop", round(dfb$pop/1e6,2), " millions")
dfb$q <- with(dfb, cut(pop, quantile(pop), include.lowest = T))
levels(dfb$q) <- paste(c("1st", "2nd", "3rd", "4th"), "Quantile")
dfb$q <- as.ordered(dfb$q)

ge <- list(
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 2,
  countrywidth = 2,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
names(world)
names(dfb)
plot_geo(world, lon = ~Longitude, lat = ~Latitude, text = ~address,
         marker = ~list(size = suic*2, line = list(width = 0)),
         color = ~-suic,colorscale=c('Viridis'), locationmode = 'country names') %>%
  layout(geo = ge, title = 'Populations<br>(Click legend to toggle)')

