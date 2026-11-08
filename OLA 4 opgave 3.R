# =====================================================================
# OLA 4 – Opgave 3: Hent fly over Nordsøen og plot og track et specifikt fly
# =====================================================================
source("Funktioner til OLA 4.R")                               # henter alle funktionerne

# =====================================================================
#opgave 3.1Hent fly over Nordsøen og plot
# =====================================================================
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

# =====================================================================
#opgave 3.2 track et specifikt fly
# =====================================================================
# vores specifikke fly placere du i linje: 43

# ---- Indlæs et cirklende fly fra wulfs rds-filer ----
# =====================================================================
# Opgave 3.2: Ét almindeligt fly og ét cirklende fly
# =====================================================================
mit_fly_icao24 <- "a5823e"                             # SKIFT FLY HER (6 tegn, fx "4acb04")

almindeligt_fly <- hent_rute(mit_fly_icao24, token)              # vores eget fly husk vores token skal med i hent rute ellers vil den ikke hente kører
cirklende_fly   <- hent_cirklende_fly("data/circMT.rds")         # cirklende fly fra Wulfs rds-filer

kort_rute(almindeligt_fly)                                       # almindeligt fly på kortet
kort_rute(cirklende_fly)                                         # cirklende fly på kortet

# kursen over tid: her ses forskellen tydeligst
# vi har her til bare brugt grimt plot
par(mfrow = c(1, 2))                                             # to plots ved siden af hinanden
plot(almindeligt_fly$tid, almindeligt_fly$kurs, main = "Almindeligt fly", xlab = "Tid", ylab = "Kurs (grader)")
plot(cirklende_fly$tid,   cirklende_fly$kurs,   main = "Cirklende fly",   xlab = "Tid", ylab = "Kurs (grader)")
par(mfrow = c(1, 1))
# almindeligt fly: kursen er næsten den samme hele tiden
# cirklende fly: kursen går rundt fra 0 til 360 grader igen og igen

#========== svar på opgave 3.2
# almindeligt fly: kursen er stabil omkring 290-315 grader, bortset fra et drej ved start
# cirklende fly: kursen går fra 360 til 0 grader to gange = flyet flyver to hele cirkler

# =====================================================================
#opgave 3.3 lav loop på alle fly! 
# =====================================================================
# # ---- Gem icao24'erne til opgave 3.3 ----
icao_vektor <- fly_nordsoen$icao24
saveRDS(icao_vektor, "icao_nordsoen.rds")                        # gem, så 3.3 kan bruge den samme liste
# =====================================================================
# Opgave 3.3: Hent tracks for alle fly og beregn standardafvigelsen på kursen
# =====================================================================
icao_vektor <- readRDS("icao_nordsoen.rds")                    # de samme fly som i opgave 3.1

resultater <- data.frame()                                     # tom tabel, før loopet
for (i in 1:length(icao_vektor)) {
  rute <- hent_rute(icao_vektor[i], token)
  if (!is.null(rute)) {                                        # kun fly med en rute husk is.null tjekke om ruten er lig nul, men med ! tager den alle som ikke er lig nul 
    maal <- kurs_maal(rute)
    maal$icao <- icao_vektor[i]                                # icao vektor havde vi defineret som være icao24 kolonnen fra fly_nordsoen
    resultater <- bind_rows(resultater, maal)                  # sæt en ny række på med bind_rows
  }
  Sys.sleep(1)                                                 # vent 1 sekund, så OpenSky ikke bremser os, hvis den ikke er der, så vil r nogle gange fejle
}
saveRDS(resultater, "resultater_3_3.rds")                      # gem, så vi ikke skal hente igen
resultater |> arrange(desc(sdcourse)) |> head()
# vi har arrangeret så flyene med den mest varierede kurs kommer øverst i resultater
#HUSK! Hver gang loopet kører kurs_maal(rute), kommer der en række ud med kolonnen sdcourse. 
#bind_rows() samler alle rækkerne i resultater. Derfor kan du bagefter sortere efter den:


# =====================================================================
# Opgave 3.4: Vores egen algoritme sammenlignet med sd og lm
# =====================================================================
sammenligning_med_vore_algoritme <- data.frame()

# lærerens cirklende fly (isOff = 1)
for (fil in list.files("data", pattern = "rds$", full.names = TRUE)) {
  rute  <- hent_cirklende_fly(fil)
  sammenligning_med_vore_algoritme <- bind_rows(sammenligning_med_vore_algoritme, 
                                      alle_maal(rute, icao = basename(fil), isOff = 1))
}

# vores fly fra Nordsøen (isOff = 0)
icao_vektor <- readRDS("icao_nordsoen.rds")
for (i in 1:length(icao_vektor)) {
  rute <- hent_rute(icao_vektor[i], token)
  if (!is.null(rute)) {                                 # kun fly med en rute
    sammenligning_med_vore_algoritme<- bind_rows(sammenligning_med_vore_algoritme, 
                                      alle_maal(rute, icao = icao_vektor[i], isOff = 0))
  }
  Sys.sleep(1)
}

saveRDS(sammenligning_med_vore_algoritme, "tabel_3_4.rds")                        # gem, så vi ikke skal hente igen
sammenligning_med_vore_algoritme|> arrange(desc(myOwnAlg))
# de cirklende fly bør ligge øverst

