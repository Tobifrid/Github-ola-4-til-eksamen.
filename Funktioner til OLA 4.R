# =====================================================================
# Funktioner til OLA 4.R
# Hentes ind i hovedscriptet med source("Funktioner til OLA 4.R")
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
# hent_cirklende_fly(filnavn)
#   Indlæser et af Wulfs cirklende fly fra en rds-fil og giver kolonnerne
#   de samme navne som hent_rute(), så de kan sammenlignes.
#
# kort_fly(fly)
#   Viser alle fly i en data frame som røde punkter på et leaflet-kort.
#   Klik på et fly for at se callsign, land og om det er i luften.
#
# kort_rute(rute)
#   Viser ruten for ét fly som en blå linje med punkter på et leaflet-kort.
#
# kurs_maal(rute)
#   Opgave 3.3: giver én række med sdcourse (standardafvigelsen på kursen),
#   sd_kursaendring og samlet_drej_grader for én rute.
#
# lm_r2(rute)
#   Opgave 3.4 (lmres): R² for en ret linje gennem ruten. Høj = lige ud, lav = cirkler.
#
# samlet_drejning_i_omgange(rute)
#   Opgave 3.4 (myOwnAlg), min egen algoritme: lægger alle drej sammen og dividerer med 360,
#   så resultatet er antal hele omgange. Over 2 omgange = mistænkeligt.
#
# alle_maal(rute, icao, isOff)
#   Opgave 3.4: samler sdcourse, lmres og myOwnAlg for ét fly i én række,
#   så alle fly kan sættes sammen i én tabel med bind_rows().
#
# vis_rute(callsign_eller_icao, fly, alle_ruter)
#   Viser ruten for ét fly på kortet. Man kan skrive enten callsign (fx "SAS1234")
#   eller icao24 (fx "40814a"). Bruger de gemte ruter fra 3.3, så der laves ingen kald til OpenSky.
#
# overvaag_omraade(lamin, lomin, lamax, lomax)
#   Opgave 3.5: henter friske fly i et område og nyt token, henter hver rute
#   og giver en data frame med HELE ruten for de fly, vores algoritme mener cirkler (> 2 omgange).
#   Kolonnen icao fortæller, hvilket fly hvert punkt hører til.
#
# FORKORTELSER
#
#  lamin    latitude minimum   = sydligste grænse i en bbox
#  lomin    longitude minimum  = vestligste grænse
#  lamax    latitude maximum   = nordligste grænse
#  lomax    longitude maximum  = østligste grænse
#  bbox     bounding box       = det firkantede område, man vil se fly i
#  icao24   International Civil Aviation Organization, 24-bit adresse = flyets unikke id
#  rawres   raw result         = det rå svar fra GET (statuskode, headers og indhold)
#  resjson  result JSON        = svaret lavet om fra JSON-tekst til en R-liste
#  url      uniform resource locator = en webadresse
#  API      application programming interface = en "dør", hvor et program kan hente data
#  JSON     JavaScript Object Notation = tekstformatet, API'er sender data i
#  GET      hent data fra en URL
#  POST     send data til en URL (bruges i getToken() til at logge ind)
#  token    adgangsbillet til API'et, virker ca. 30 minutter
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
library(leaflet)     
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

#til opgave 3.2
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

#til opgave 3.2
# ---- Indlæs et cirklende fly fra lærerens rds-filer ----
hent_cirklende_fly <- function(filnavn) {
  rute <- readRDS(filnavn)
  rute <- rute |>
    rename(tid        = time,                                    # samme navne som hent_rute()
           latitude   = lat,
           longitude  = lng,
           hoejde     = alt,
           kurs       = crs,
           paa_jorden = grd) |>
    mutate(latitude  = as.numeric(latitude),                     # tekst -> tal
           longitude = as.numeric(longitude),
           kurs      = as.numeric(kurs))
  return(rute)
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

# til opgave 3.3
# vi er nødtil at bygge en funktion som viser kurs og kurs ændringer
# ---- Kursen for én rute: standardafvigelse og ændring ----
kurs_maal <- function(rute) {
  rute <- rute |>
    mutate(kursaendring = kurs - lag(kurs),                    # ændring fra punktet før
           kursaendring = ((kursaendring + 180) %% 360) - 180) # 358 -> 2 bliver +4, ikke -356
  return(data.frame(sdcourse           = sd(rute$kurs, na.rm = TRUE),
                    sd_kursaendring    = sd(rute$kursaendring, na.rm = TRUE),
                    samlet_drej_grader = sum(abs(rute$kursaendring), na.rm = TRUE)))
}

#til opgave 3.4

# ---- lmres: hvor godt passer ruten på en ret linje? ----
lm_r2 <- function(rute) {
  model <- lm(latitude ~ longitude, data = rute)        # en ret linje gennem ruten
  return(summary(model)$r.squared)                      # høj = lige ud, lav = cirkler
}

# ---- myOwnAlg: hvor meget har flyet drejet i alt, omregnet til hele omgange? ----
# abs() gør, at drej til højre og venstre begge tæller som positive.
# Derfor fanger den både fly, der cirkler, og fly, der drejer meget frem og tilbage (zigzag).
# Den måler altså den samlede drejning, ikke kun rigtige cirkler.
samlet_drejning_i_omgange <- function(rute) {
  kursaendring <- rute$kurs - lag(rute$kurs)            # drej fra punktet før
  kursaendring <- ((kursaendring + 180) %% 360) - 180   # 358 -> 2 er +4 grader, ikke -356
  samlet_drej  <- sum(abs(kursaendring), na.rm = TRUE)  # læg alle drej sammen (abs: højre og venstre tæller ens)
  return(samlet_drej / 360)                             # 360 grader = én hel omgang
}
# ---- Saml alle mål for ét fly i én række til tabellen i opgave 3.4 ----
# “alle_maal() tager ruten for ét fly og beregner de tre mål på én gang. 
# Resultatet er én række, så jeg kan bruge bind_rows() i et loop og få hele tabellen med alle fly.”
# Bruger sd(), lm_r2() og samlet_drejning_i_omgange() på samme rute, så målene kan sammenlignes
alle_maal <- function(rute, icao, isOff) {
  data.frame(icao     = icao,                           # flyets id (eller filnavnet for lærerens fly)
             sdcourse = sd(rute$kurs, na.rm = TRUE),    # standardafvigelse på kursen: høj = varierende kurs
             lmres    = lm_r2(rute),                    # R² for en ret linje: lav = cirkler
             myOwnAlg = samlet_drejning_i_omgange(rute),# vores egen algoritme: samlet drejning i hele omgange
             isOff    = isOff)                          # facit: 1 = cirkler, 0 = almindeligt
}


# til opgave 3.5
# ---- Overvåg et område og find mistænkelige fly ----
# Henter friske fly og nyt token hver gang (tokenet virker kun ca. 30 min),
# henter hver rute og bruger vores algoritme på den del af ruten, der ligger i området.
# Gemmer hele ruten for de mistænkelige fly, så vi kan vise dem bagefter uden at hente igen.
overvaag_omraade <- function(lamin, lomin, lamax, lomax) {
  token        <- getToken()                                     # nyt token hver gang
  kolonnenavne <- hent_kolonnenavne()
  omraade_url  <- bbox_url(lamin, lomin, lamax, lomax)
  fly          <- hent_fly(omraade_url, token, kolonnenavne)     # friske fly lige nu
  
  mistaenkelige_ruter <- data.frame()                            # tom tabel, før loopet
  for (icao in fly$icao24) {
    rute <- hent_rute(icao, token)
    if (!is.null(rute)) {                                        # kun fly med en rute
      rute_i_omraadet <- rute |>
        filter(latitude  >= lamin, latitude  <= lamax,           # kun den del af ruten,
               longitude >= lomin, longitude <= lomax)           # der ligger i området
      if (samlet_drejning_i_omgange(rute_i_omraadet) > 2) {      # vores algoritme
        mistaenkelige_ruter <- bind_rows(mistaenkelige_ruter,
                                         rute |> mutate(icao = icao))   # gem hele ruten + hvilket fly
      }
    }
    Sys.sleep(1)                                                 # så OpenSky ikke afviser os
  }
  return(mistaenkelige_ruter)
}


# til opgave 3.5
# ---- Vis ruten for ét fly ud fra callsign ELLER icao24 (bruger de gemte ruter, ingen kald til OpenSky) ----
vis_rute <- function(callsign_eller_icao, fly, alle_ruter) {
  fundet <- fly |>
    filter(str_trim(callsign) == callsign_eller_icao |            # callsign har ofte mellemrum til sidst, derfor str_trim
             icao24 == callsign_eller_icao)                          # eller icao24
  if (nrow(fundet) == 0) {
    stop(paste("Fandt ikke", callsign_eller_icao, "blandt flyene"))         # tydelig besked i stedet for en mærkelig fejl
  }
  rute <- alle_ruter |> filter(icao == fundet$icao24[1])          # flyets gemte rute
  if (nrow(rute) == 0) {
    stop(paste("Ingen gemt rute for", callsign_eller_icao, "- kør opgave 3.3 først"))
  }
  print(paste("Viser", str_trim(fundet$callsign[1]), "/", fundet$icao24[1]))   # så man kan se, hvilket fly det er
  kort_rute(rute)
}