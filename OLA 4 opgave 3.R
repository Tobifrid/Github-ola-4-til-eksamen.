# =====================================================================
# OLA 4 – Opgave 3: Hent fly over Nordsøen og plot og track et specifikt fly
# =====================================================================
source("Funktioner til OLA 4.R")                                 # henter alle funktionerne
# =====================================================================
# Opgave 3.1: Hent fly over Nordsøen og plot
# =====================================================================
token        <- getToken()                                       # nyt token
kolonnenavne <- hent_kolonnenavne()                              # navnene fra dokumentationen

# ---- Fly over Nordsøen (lærerens bbox) ----
lamin <- 52.055129                                               # latitude min  = syd
lomin <- -6.065140                                               # longitude min = vest
lamax <- 56.196869                                               # latitude max  = nord
lomax <- 4.305954                                                # longitude max = øst
nordsoen_url <- bbox_url(lamin, lomin, lamax, lomax)             # brug variablerne
fly_nordsoen <- hent_fly(nordsoen_url, token, kolonnenavne)

nrow(fly_nordsoen)                                               # antal fly over Nordsøen lige nu
# 19. november kl. 9.20: 116 fly
# 19. november kl. 12.57: 185 fly
# 19. november kl. 18.58: 144 fly
fly_nordsoen |> count(origin_country, sort = TRUE) |> head(5)    # hvor mange fly fra hvilke lande?
# kl. 9.20:  Storbritannien 53, Irland 13, Holland 9, USA 9, Malta 8
# kl. 12.57: Storbritannien 86, Irland 23, Holland 15, Tyskland 11, USA 11

# ---- Barplot: antal fly pr. land ----
fly_pr_land <- fly_nordsoen |>
  count(origin_country, sort = TRUE)                             # antal fly pr. land

ggplot(fly_pr_land, aes(x = reorder(origin_country, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +                                                 # landene på y-aksen
  labs(title = "Fly over Nordsøen fordelt på lande", x = "Land", y = "Antal fly")

# ---- Gem icao24'erne til opgave 3.3 ----
icao_vektor <- fly_nordsoen$icao24
saveRDS(icao_vektor, "icao_nordsoen.rds")                        # gem, så 3.3 og 3.4 bruger den samme liste


# =====================================================================
# Opgave 3.2: Ét almindeligt fly og ét cirklende fly
# =====================================================================
mit_fly_icao24 <- fly_nordsoen$icao24[1]                         # første fly fra listen lige nu (et gammelt icao24 kan være landet)

almindeligt_fly    <- hent_rute(mit_fly_icao24, token)           # vores eget fly, husk token skal med i hent_rute
Wulf_cirklende_fly <- hent_cirklende_fly("data/circMT.rds")      # cirklende fly fra Wulfs rds-filer

kort_rute(almindeligt_fly)                                       # almindeligt fly på kortet
kort_rute(Wulf_cirklende_fly)                                    # Wulfs cirklende fly på kortet

# kursen over tid: her ses forskellen tydeligst
par(mfrow = c(1, 2))                                             # to plots ved siden af hinanden
plot(almindeligt_fly$tid,    almindeligt_fly$kurs,    main = "Almindeligt fly", xlab = "Tid", ylab = "Kurs (grader)")
plot(Wulf_cirklende_fly$tid, Wulf_cirklende_fly$kurs, main = "Cirklende fly",   xlab = "Tid", ylab = "Kurs (grader)")
par(mfrow = c(1, 1))

#========== svar på opgave 3.2
# almindeligt fly: kursen er stabil omkring 290-315 grader, bortset fra et drej ved start
# cirklende fly: kursen går fra 360 til 0 grader to gange = flyet flyver to hele cirkler


# =====================================================================
# Opgave 3.3: Hent tracks for alle fly og beregn standardafvigelsen på kursen
# =====================================================================
icao_vektor <- readRDS("icao_nordsoen.rds")                      # de samme fly som i opgave 3.1

resultater <- data.frame()                                       # tom tabel, før loopet
alle_ruter <- data.frame()                                       # her gemmer vi alle ruterne, så 3.5 kan bruge dem
for (i in 1:length(icao_vektor)) {
  rute <- hent_rute(icao_vektor[i], token)
  if (!is.null(rute)) {                                          # kun fly med en rute, fly uden rute springes over
    maal <- kurs_maal(rute)
    maal$icao <- icao_vektor[i]                                  # flyets icao24
    resultater <- bind_rows(resultater, maal)                    # sæt en ny række på med bind_rows
    alle_ruter <- bind_rows(alle_ruter, rute |> mutate(icao = icao_vektor[i]))   # gem ruten + hvilket fly
  }
  Sys.sleep(1)                                                   # vent 1 sekund, så OpenSky ikke afviser os (statuskode 429)
}
saveRDS(resultater, "resultater_3_3.rds")                        # gem, så vi ikke skal hente igen
saveRDS(alle_ruter, "alle_ruter_3_3.rds")                        # gem ruterne til opgave 3.5
resultater |> arrange(desc(sdcourse)) |> head()                  # flyene med den mest varierede kurs øverst
# HUSK: hver gang loopet kører kurs_maal(rute), kommer der en række ud med kolonnen sdcourse.
# bind_rows() samler alle rækkerne i resultater, så vi bagefter kan sortere efter den.


# =====================================================================
# Opgave 3.4: Vores egen algoritme sammenlignet med sd og lm
# =====================================================================
sammenligning_med_vore_algoritme <- data.frame()

# Wulfs cirklende fly (isOff = 1)
for (fil in list.files("data", pattern = "rds$", full.names = TRUE)) {
  rute <- hent_cirklende_fly(fil)
  sammenligning_med_vore_algoritme <- bind_rows(sammenligning_med_vore_algoritme,
                                                alle_maal(rute, icao = basename(fil), isOff = 1))
}

# vores fly fra Nordsøen (isOff = 0)
icao_vektor <- readRDS("icao_nordsoen.rds")
for (i in 1:length(icao_vektor)) {
  rute <- hent_rute(icao_vektor[i], token)
  if (!is.null(rute)) {                                          # kun fly med en rute
    sammenligning_med_vore_algoritme <- bind_rows(sammenligning_med_vore_algoritme,
                                                  alle_maal(rute, icao = icao_vektor[i], isOff = 0))
  }
  Sys.sleep(1)
}

saveRDS(sammenligning_med_vore_algoritme, "tabel_3_4.rds")       # gem, så vi ikke skal hente igen
sammenligning_med_vore_algoritme |> arrange(desc(myOwnAlg))
# kl. 18.58: 144 fly, 136 med rute. Alle 9 af Wulfs fly er over 2 omgange og ligger i top 17;
#            8 af vores 136 egne fly er også over 2 (højest c2b5b7 med 8,8 omgange).
#            408320 og 40831f har næsten samme icao24 og drejer lige meget: måske to fly, der flyver sammen.
# =====================================================================
# Opgave 3.5: uden automatisering Overvåg mistænkelige fly
# =====================================================================
# ---- Kig i de fly, vi allerede har hentet i 3.1 og 3.3 (ingen nye kald til OpenSky) ----
alle_ruter <- readRDS("alle_ruter_3_3.rds")                                         # ruterne fra 3.3
nrow(alle_ruter)                                                                   # TJEK: skal være større end 0, ellers har OpenSky bremset 3.3

mistaenkelige <- data.frame(icao = character(0), omgange = numeric(0))            # tom tabel med kolonnerne, før loopet
for (icao_nr in unique(alle_ruter$icao)) {                                         # ét fly ad gangen
  rute_i_bbox <- alle_ruter |>
    filter(icao == icao_nr,                                                        # kun dette fly
           latitude  >= lamin, latitude  <= lamax,                                 # og kun den del af ruten,
           longitude >= lomin, longitude <= lomax)                                 # der ligger over Nordsøen
  omgange <- samlet_drejning_i_omgange(rute_i_bbox)                                # vores algoritme
  if (omgange > 2) {
    mistaenkelige <- bind_rows(mistaenkelige, data.frame(icao = icao_nr, omgange = round(omgange, 1)))
  }
}
mistaenkelige <- mistaenkelige |>
  left_join(fly_nordsoen |> select(icao24, callsign), by = c("icao" = "icao24")) |>   # sæt callsign på, så flyene er nemme at slå op
  mutate(callsign = str_trim(callsign)) |>                                           # fjern mellemrum til sidst
  arrange(desc(omgange))                                                             # mest mistænkelige øverst

filnavn <- paste0("mistaenkelige_fly_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv")   # dato og klokkeslæt i filnavnet
write.csv(mistaenkelige, filnavn, row.names = FALSE)                               # gem listen i en fil
nrow(mistaenkelige)                                                                # antal mistænkelige fly
mistaenkelige # se de mistænkelige flys icao nr og callsign plus omgange de cirklede  

# =====================================================================
# >>> SLÅ ET FLY OP: skriv callsign eller icao24 her og kør de to linjer <<<
# =====================================================================
mit_fly <- "SAVAGE81"                                                                # SKRIV HER: callsign (fx "RYR8KL") eller icao24 (fx "40814a")
vis_rute(mit_fly, fly_nordsoen, alle_ruter)                                        # ruten på kortet, ingen kald til OpenSky



# Log over runderne (skriv en ny linje, hver gang blokken køres):
# 19. november kl. 9.00: 0 mistænkelige fly
# 19. november kl. 12.45: 18 mistænkelige fly
# 19. november kl. 19.10: 5 mistænkelige fly – CFC2547 (canadisk militær, 8,8), GBBKA, SAVAGE82/SAVAGE81 (to fly, der flyver sammen) og GRVNA
# CFC2547 cirkler tæt sydvest for Glasgow, sandsynligvis ved Prestwick Airport (militær mellemlanding/øvelse).
# Algoritmen finder mønsteret, men et menneske skal vurdere, om det er mistænkeligt.
# SAVAGE81 flyver fra land syd for Sunderland ud over Nordsøen, laver sløjfer og skarpe vendinger og flyver tilbage:
# ikke pæne cirkler, men et afsøgningsmønster. Det viser, at algoritmen også fanger zigzaggende fly.


