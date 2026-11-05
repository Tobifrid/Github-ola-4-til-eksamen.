# =====================================================================
# Funktioner til Opensky.R: alle funktioner til OpenSky samlet ét sted
# Hentes ind i hovedscriptet med source("Funktioner til Opensky.R")
# =====================================================================
#
# OVERSIGT OVER FUNKTIONERNE
#
# hent_kolonnenavne()
#   Henter navnene på kolonnerne i states fra OpenSkys dokumentation,
#   fx "icao24", "callsign", "origin_country", "longitude", "latitude".
#
# hent_fly(url, token, kolonnenavne)
#   Henter fly fra en states-URL og laver dem til en pæn data frame:
#   rigtige kolonnenavne, longitude/latitude som tal, on_ground som TRUE/FALSE,
#   plus de nye kolonner i_luften ("i luften"/"på jorden") og flyselskab (3 første tegn).
#
# bbox_url(lamin, lomin, lamax, lomax)
#   Bygger en states-URL for et område (bbox).
#   lamin = latitude min (syd), lomin = longitude min (vest),
#   lamax = latitude max (nord), lomax = longitude max (øst).
#
# hent_rute(icao24, token)
#   Henter ruten for ét fly fra /tracks/all og giver en data frame med
#   tid, latitude, longitude, hoejde, kurs og paa_jorden.
#   Giver NULL, hvis OpenSky ikke har en rute for flyet.
#
# km_flyet_er_floejet(icao24, token)
#   Beregner, hvor mange km ét fly har fløjet på sin nuværende flyvning.
#   Bruger hent_rute() og lægger afstanden mellem alle punkter sammen.
#   Giver NA, hvis der ikke er en rute.
#
# kort_fly(fly)
#   Viser alle fly i en data frame som røde punkter på et leaflet-kort.
#   Klik på et fly for at se callsign, land og om det er i luften.
#
# kort_rute(rute)
#   Viser ruten for ét fly som en blå linje med punkter på et leaflet-kort.
#
# FORKORTELSER
#
#  lamin    latitude minimum   = sydligste grænse i en bbox
#  lomin    longitude minimum  = vestligste grænse
#  lamax    latitude maximum   = nordligste grænse
#  lomax    longitude maximum  = østligste grænse
#  bbox     bounding box       = det firkantede område, man vil se fly i
#  icao24   International Civil Aviation Organization, 24-bit adresse = flyets unikke id
#  vars     variables          = tabellen med kolonnenavnene fra dokumentationen
#  rawres   raw result         = det rå svar fra GET (statuskode, headers og indhold)
#  resjson  result JSON        = svaret lavet om fra JSON-tekst til en R-liste
#  url      uniform resource locator = en webadresse
#  API      application programming interface = en "dør", hvor et program kan hente data
#  JSON     JavaScript Object Notation = tekstformatet, API'er sender data i
#  GET      hent data fra en URL
#  POST     send data til en URL (bruges i getToken() til at logge ind)
#  token    adgangsbillet til API'et, virker ca. 30 minutter
#  rad      radianer           = vinkelmål, som sin() og cos() regner i
#  lat/lon  latitude/longitude = breddegrad/længdegrad
#  lng      longitude, som leaflet kalder den
#
# KOLONNERNE I STATES (navnene fra hent_kolonnenavne())
#
#  X1  icao24           flyets unikke id (transponderens adresse)
#  X2  callsign         kaldesignal, fx "SAS1234"
#  X3  origin_country   landet, flyet er registreret i
#  X4  time_position    hvornår flyets position sidst blev målt (sekunder siden 1970)
#  X5  last_contact     sidste signal fra flyet (sekunder siden 1970)
#  X6  longitude        længdegrad (øst/vest)
#  X7  latitude         breddegrad (nord/syd)
#  X8  baro_altitude    højde målt med lufttryk, i meter
#  X9  on_ground        TRUE = flyet står på jorden
#  X10 velocity         fart i meter pr. sekund
#  X11 true_track       kurs i grader, 0 = nord
#  X12 vertical_rate    stigehastighed i m/s, negativ = flyet går ned
#  X13 sensors          hvilke modtagere der så flyet (oftest NULL)
#  X14 geo_altitude     højde målt med GPS, i meter
#  X15 squawk           transponderkode, fx 7700 = nødsituation
#  X16 spi              piloten har markeret flyet for flyvelederen
#  X17 position_source  0 = ADS-B, 1 = ASTERIX, 2 = MLAT, 3 = FLARM
#
# Fra util.R:
# getToken()                     henter et nyt adgangstoken fra OpenSky
# getRawFromeurl(url, token)     laver et GET-kald og prøver igen med nyt token ved 401
#
# =====================================================================
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(leaflet)     # interaktive kort
source("util OLA 4.R")     # getToken() og getRawFromeurl()

# ---- Hent kolonnenavnene til states fra OpenSkys dokumentation ----
hent_kolonnenavne <- function() {
  side     <- read_html("https://openskynetwork.github.io/opensky-api/rest.html")
  tabeller <- side |> html_elements("table") |> html_table()    # alle tabeller på siden
  return(tabeller[[5]]$Property)                                 # navnene i states
}


# ---- Hent fly fra en states-URL og lav dem om til en pæn data frame ----
hent_fly <- function(url, token, kolonnenavne) {
  rawres  <- getRawFromeurl(url, token)
  if (rawres$status_code != 200) {                                        # 200 = ok
    stop(paste("OpenSky svarede med statuskode", rawres$status_code))     # stop med en tydelig besked
  }
  resjson <- fromJSON(content(rawres, as = "text", encoding = "UTF-8"))   # JSON -> liste
  fly     <- data.frame(resjson$states)                                   # liste -> data frame
  colnames(fly) <- kolonnenavne[1:ncol(fly)]                              # rigtige navne
  fly <- fly |>
    mutate(longitude  = as.numeric(longitude),                            # tekst -> tal
           latitude   = as.numeric(latitude),
           on_ground  = as.logical(on_ground),
           i_luften   = ifelse(on_ground == FALSE, "i luften", "på jorden"),
           flyselskab = substr(callsign, 1, 3))                           # de 3 første tegn
  return(fly)
}


# ---- Byg en states-URL for et område (bbox) ----
bbox_url <- function(lamin, lomin, lamax, lomax) {
  # lamin = latitude min (syd), lomin = longitude min (vest)
  # lamax = latitude max (nord), lomax = longitude max (øst)
  paste0("https://opensky-network.org/api/states/all",
         "?lamin=", lamin, "&lomin=", lomin,
         "&lamax=", lamax, "&lomax=", lomax)                    # ? starter filtrene, & adskiller dem
}


# ---- Hent ruten for ét fly ----
hent_rute <- function(icao24, token) {
  rute_url <- paste0("https://opensky-network.org/api/tracks/all?icao24=", icao24, "&time=0")
  rawres   <- getRawFromeurl(rute_url, token)
  if (rawres$status_code != 200) {                               # ingen rute fundet
    return(NULL)
  }
  rute_json <- fromJSON(content(rawres, as = "text", encoding = "UTF-8"))
  rute <- data.frame(rute_json$path)                             # path = alle punkter på ruten
  colnames(rute) <- c("tid", "latitude", "longitude", "hoejde", "kurs", "paa_jorden")
  rute <- rute |>
    mutate(latitude  = as.numeric(latitude),                     # tekst -> tal
           longitude = as.numeric(longitude),
           kurs      = as.numeric(kurs))
  return(rute)
}


# ---- Hvor mange km har ét fly fløjet? ----
km_flyet_er_floejet <- function(icao24, token) {
  rute <- hent_rute(icao24, token)                               # genbrug funktionen ovenfor
  if (is.null(rute)) {                                           # ingen rute
    return(NA)
  }
  jordens_radius_km <- 6371
  rute <- rute |>
    mutate(lat_rad     = latitude  * pi / 180,                   # grader -> radianer
           lon_rad     = longitude * pi / 180,
           forrige_lat = lag(lat_rad),                           # punktet før
           forrige_lon = lag(lon_rad),
           km_siden_sidst = jordens_radius_km * acos(pmin(1,     # pmin(1, ...) undgår afrundingsfejl over 1
                                                          sin(forrige_lat) * sin(lat_rad) +
                                                            cos(forrige_lat) * cos(lat_rad) * cos(lon_rad - forrige_lon))))
  return(round(sum(rute$km_siden_sidst, na.rm = TRUE), 0))       # læg alle stykker sammen
}


# ---- Kort: alle fly som punkter (leaflet) ----
kort_fly <- function(fly) {
  leaflet(fly) |>
    addTiles() |>                                                # baggrundskort (OpenStreetMap)
    addCircleMarkers(lng = ~longitude, lat = ~latitude,          # ~ = kolonnen i data
                     radius = 4, color = "red",
                     popup = ~paste(callsign, "-", origin_country, "-", i_luften))   # vises ved klik
}


# ---- Kort: ruten for ét fly (leaflet) ----
kort_rute <- function(rute) {
  leaflet(rute) |>
    addTiles() |>
    addPolylines(lng = ~longitude, lat = ~latitude, color = "blue") |>   # ruten som en streg
    addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2)      # punkterne på ruten
}