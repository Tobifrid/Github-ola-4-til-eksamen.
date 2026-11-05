# =====================================================================
# OLA 4 – Opgave 3.1: Hent fly over Nordsøen og plot hej
# =====================================================================
source("Funktioner til OLA 4.R")                               # henter alle funktionerne

token        <- getToken()                                       # nyt token
kolonnenavne <- hent_kolonnenavne()                              # navnene fra dokumentationen

# ---- Fly over Nordsøen (lærerens bbox) ----
nordsoen_url <- bbox_url(lamin = 52.055129,                      # latitude min  = syd
                         lomin = -6.065140,                      # longitude min = vest
                         lamax = 56.196869,                      # latitude max  = nord
                         lomax = 4.305954)                       # longitude max = øst
fly_nordsoen <- hent_fly(nordsoen_url, token, kolonnenavne)
nrow(fly_nordsoen)
# antal fly over Nordsøen lige nu

# ---- Barplot: antal fly pr. land ----
fly_pr_land <- fly_nordsoen |>
  count(origin_country, sort = TRUE)                             # antal fly pr. land

ggplot(fly_pr_land, aes(x = reorder(origin_country, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +                                                 # landene på y-aksen
  labs(title = "Fly over Nordsøen fordelt på lande", x = "Land", y = "Antal fly")

# ---- Gem icao24'erne til opgave 3.3 ----
icao_vektor <- fly_nordsoen$icao24
saveRDS(icao_vektor, "icao_nordsoen.rds")                        # gem, så 3.3 kan bruge den samme liste