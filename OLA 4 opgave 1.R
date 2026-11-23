# =====================================================================
# OLA 4 – Opgave 1: Biler fra Bilbasen (1.1) og autoscout24 (1.2)
# Alle loops ligger i "Loops til OLA 4 opgave 1.R"
# =====================================================================
source("Loops til OLA 4 opgave 1.R")                                          # henter alle loop-funktionerne (kører ingenting)


# =====================================================================
# Opgave 1.1: Hent BMW'er fra Bilbasen
# =====================================================================
# Vores lukkede serie: brugte BMW-elbiler fra 2020, stationcar/SUV/CUV/sedan/hatchback, kun kontantpris
# fuel=3 = el
bilbasen_url <- "https://www.bilbasen.dk/brugt/bil/bmw?cartype=stationcar&cartype=suv&cartype=cuv&cartype=sedan&cartype=hatchback&fuel=3&pricetype=Retail&yearfrom=2020&page="
# ---- Hent søgesiderne (KØR KUN 1 GANG, ca. 4 min) ----
#alle_sider <- hent_bilbasen_sider(bilbasen_url, 46)                           # 46 sider med 30 biler
#saveRDS(alle_sider, "alle_vores_BMW.rds")                                     # gem, så vi ikke skal hente igen
alle_sider <- readRDS("alle_vores_BMW.rds")                                    # indlæs i stedet for at hente

# ---- Pil bilerne ud af siderne ----
alle_biler <- udtraek_bilbasen_biler(alle_sider)
nrow(alle_biler)                                                               # 1.380 rækker
n_distinct(alle_biler$carid)                                                   # men kun 1.349 unikke carid'er = 31 dubletter

# ---- Rens data ----
alle_biler <- alle_biler |>
  distinct(carid, .keep_all = TRUE) |>                                         # fjern dubletter (annoncer flyttede side under hentningen)
  filter(!str_detect(link, "klikxtra")) |>                                     # fjern betalte reklamer
  mutate(pris = as.numeric(str_remove_all(pris, "\\D")),                       # "419.900 kr" → 419900
         km   = as.numeric(str_remove_all(km, "\\D")),                         # "15.500 km"  → 15500
         registrerings_aargang = as.numeric(str_extract(registrerings_dato, "\\d{4}")),   # "6/2024" → 2024
         forhandler_by         = str_extract(region, "^[^,]+"),                # alt før kommaet
         forhandler_landsdel   = str_trim(str_extract(region, "[^,]+$")),      # alt efter kommaet
         scrapedate            = as.Date("2026-11-09"),
         model_kort = str_remove(model,"BMW "),                                         # 1. fjern "BMW " foran husk mellemrummet da der er et mellemrum imellem eks. "BMW i3s"
         model_kort = str_to_lower(model_kort),                                         # 2. lav om til små bogstaver
         model_kort = case_when(model_kort == "i3s" ~ "i3",                    # 3. i3s → i3
                                TRUE ~ model_kort)) |>                         # alle andre beholder deres navn
  filter(pris < 999999) |>                                                     # fjern pladsholder-pris (1 i5 til 999.999 kr.)
  relocate(registrerings_aargang, .after = registrerings_dato)
# ---- Kontrol ----
nrow(alle_biler)                                            # 1.347 biler
sum(is.na(alle_biler$model))                                # 0 biler uden model
summary(alle_biler$pris)                                    # billigste 129.900 kr., median 354.800 kr., dyreste 999.900 kr. (en i7, realistisk)
summary(alle_biler$registrerings_aargang)                   # 2020 til 2026, som vores filter
saveRDS(alle_biler, "alle_biler_renset.rds")

# ---- Biler, der "kommer snart" ----
kommer_snart <- find_kommer_snart(alle_sider)
length(kommer_snart)                                                           # 3 biler
# de 3 biler har rigtige priser og km, så de er beholdt


# ---- Hent forhandlerinfo fra hver bils egen side (KØR KUN 1 GANG, ca. 1½ time) ----
#alle_forhandlere <- hent_forhandlere(alle_biler)
#saveRDS(alle_forhandlere, "alle_forhandlere.rds")
alle_forhandlere <- readRDS("alle_forhandlere.rds")                            # indlæs i stedet for at hente

# ---- Kontrol af forhandlere ----
nrow(alle_forhandlere)                                                         # 1.348
sum(is.na(alle_forhandlere$forhandler_cvr))                                    # 132 uden CVR = private sælgere
anti_join(alle_biler, alle_forhandlere, by = "carid") |> nrow()               # 0 biler sprunget over

alle_forhandlere |>                                                            # samme CVR med flere navne?
  group_by(forhandler_cvr) |>
  summarise(antal_navne = n_distinct(forhandler_navn)) |>
  filter(antal_navne > 1)
# 14 CVR-numre har flere navne, men det er kæder med flere afdelinger (fx Ejner Hessel: 18 afdelinger)

# ---- Sælgertype: ingen CVR = privat sælger (navn/adresse gemmes ikke, GDPR) ----
alle_forhandlere <- alle_forhandlere |>
  mutate(saelger_type = ifelse(is.na(forhandler_cvr), "Privat sælger", "Forhandler"))
alle_forhandlere |> count(saelger_type)                                        # 1.216 forhandlere, 132 private

# ---- Kobl forhandlerne på bilerne, og behold kun forhandlere ----
alle_biler_med_forhandler <- left_join(alle_biler, alle_forhandlere, by = "carid") |>
  filter(saelger_type == "Forhandler")                                         # private sælgere fjernes
nrow(alle_biler_med_forhandler)                                                # 1.215 biler
saveRDS(alle_biler_med_forhandler, "alle_biler_med_forhandler.rds")            # den færdige tabel til opgave 1.2 og 2
alle_biler_med_forhandler |> filter(pris >= 999900) |> select(model_kort, pris, km, registrerings_aargang)   # 1 i7 fra 2023 til 999.900 kr. → realistisk, beholdes

# =====================================================================
# Opgave 1.2: Hent BMW'er fra autoscout24.de
# =====================================================================
# Samme filtre som Bilbasen + kun forhandlere (custtype=D), kun Tyskland (cy=D), kun brugte (/ot_gebraucht) og kun elbiler (fuel=E)
# NB: vores første URL brugte ustate=U, men den parameter filtrerer ikke på brugte biler. Derfor kom 135 nye biler med.
# Den rigtige parameter fandt vi ved at klikke "Gebraucht" af på siden og aflæse URL'en.
# Vi fokuserer kun på elbiler, fordi vores afgiftsberegning kun gælder for elbiler (fuel=E i stedet for 2,E)
autoscout_url <- "https://www.autoscout24.de/lst/bmw/ot_gebraucht?body=1%2C4%2C5%2C6&fuel=E&fregfrom=2020&custtype=D&cy=D&damaged_listing=exclude&atype=C&sort=standard&page="

# Test (kørt én gang): 20 biler pr. side, og side 61 findes (data-position 1201-1220), så der er ingen sidegrænse
antal_sider <- ceiling(nrow(alle_biler_med_forhandler) / 20)                   # 1.215 / 20 = 60,75 → 61 sider

# ---- Hent søgesiderne (KØR KUN 1 GANG, ca. 5 min) ----
#alle_tyske_sider <- hent_autoscout_sider(autoscout_url, antal_sider)          # henter 61 sider med tyske elbiler
#saveRDS(alle_tyske_sider, "alle_tyske_BMW_elbiler.rds")                       # nyt filnavn, de gamle filer bevares
alle_tyske_sider <- readRDS("alle_tyske_BMW_elbiler.rds")                      # indlæs i stedet for at hente

# ---- Tjek før rensning ----
tyske_biler_raa <- udtraek_tyske_biler(alle_tyske_sider)                       # urenset udgave til sammenligning
nrow(tyske_biler_raa)                                                          # 1.220 rækker
n_distinct(tyske_biler_raa$tysk_bil_id)                                        # 1.220 unikke = 0 dubletter
sum(tyske_biler_raa$forste_registrering == "new")                              # 0 nye biler → /ot_gebraucht virker

# ---- Pil de tyske biler ud og rens dem ----
alle_tyske_biler <- udtraek_tyske_biler(alle_tyske_sider) |>
  distinct(tysk_bil_id, .keep_all = TRUE) |>                                   # fjern evt. dubletter (data-guid er unik pr. bil)
  filter(forste_registrering != "new") |>                                      # ekstra sikkerhed: ingen nye biler
  mutate(across(c(pris_eur, km), as.numeric),                                  # tekst → tal
         registrerings_aargang = as.numeric(str_extract(forste_registrering, "\\d{4}")))   # "10-2020" → 2020

# ---- Smyle-biler: giv navn og by i stedet for NA ----
alle_tyske_biler |> filter(is.na(forhandler_navn))                             # 56 biler uden navn og by
alle_tyske_biler |> count(saelger_type)                                        # d = 1.164, smyle = 56
# de 56 sælges via autoscout24's online-tjeneste Smyle, som ikke viser forhandlernavn og by
alle_tyske_biler <- alle_tyske_biler |>
  mutate(forhandler_navn = ifelse(is.na(forhandler_navn), "Autoscout24", forhandler_navn),   # mangler navn → "Autoscout24"
         forhandler_by   = ifelse(is.na(forhandler_by), "Online", forhandler_by))             # mangler by → "Online"

# ---- Kontrol ----
nrow(alle_tyske_biler)                                                         # 1.220 brugte tyske elbiler
alle_tyske_biler |> count(braendstof)                                          # e = 1.220, kun elbiler
sum(is.na(alle_tyske_biler$forhandler_navn))                                   # 0 (efter Smyle-biler fik navnet "Autoscout24")
summary(alle_tyske_biler$registrerings_aargang)                                # 2020–2026, median 2023

#----- omdøb modelnavnene så de hedder det korrekte
alle_tyske_biler <- alle_tyske_biler |>
  mutate(model = case_when(model == "x3" ~ "ix3",                              # X3 med el = iX3
                           model == "x1" ~ "ix1",                              # X1 med el = iX1
                           model == "x2" ~ "ix2",                              # X2 med el = iX2
                           TRUE ~ model)) |>                                   # alle andre beholder deres navn
  filter(!model %in% c("330", "sonstige"))                                     # fjern de 3 biler med ukendt model

# gem det rensede dataframe
saveRDS(alle_tyske_biler, "alle_tyske_biler_renset.rds")                       # den færdige tyske tabel til 1.2 og 2

# ---- Modelnavne i de to lande ----
alle_biler_med_forhandler |> count(model_kort, sort = TRUE)                    # DK: 8 modeller (ix3, i4, ix1, i5, ix, ix2, i3, i7)

alle_tyske_biler |> count(model, sort = TRUE)                                  # DE: samme 8 modeller, ingen x1, x2, x3, 330 eller sonstige
nrow(alle_tyske_biler)                                                         # 1.220 − 3 = 1.217
euro_kurs         <- 7.46
tysk_moms         <- 1.19                                                     # tysk moms, allerede med i den tyske pris
dansk_moms        <- 1.25                                                     # dansk moms på nye transportmidler
omregistrering_kr <- 1025 + 1180                                             # toldsyn hos FDM (ikke-medlem) + nye danske nummerplader
alle_tyske_biler <- alle_tyske_biler |>
  mutate(pris_dkk            = pris_eur * euro_kurs,
         nyt_transportmiddel = km < 6000,                                     # under 6.000 km = nyt transportmiddel
         pris_dkk_med_moms   = if_else(nyt_transportmiddel,
                                       pris_dkk / tysk_moms * dansk_moms,     # skift tysk moms ud med dansk
                                       pris_dkk))                             # ellers samme pris
alle_tyske_biler |> count(nyt_transportmiddel)
# TRUE = 41
summary(alle_tyske_biler$pris_dkk)

#----------------medianpris pr. model og årgang i hvert land
danske_priser <- alle_biler_med_forhandler |>
  group_by(model_kort, registrerings_aargang) |>                               # én gruppe pr. model og årgang
  summarise(dansk_median_pris = median(pris),                                  # median pris i gruppen
            antal_danske      = n(),                                           # antal biler i gruppen
            .groups = "drop")                                                  # fjern grupperingen, før vi joiner
tyske_priser <- alle_tyske_biler |>
  group_by(model, registrerings_aargang) |>
  summarise(tysk_median_pris = median(pris_dkk_med_moms),                      # inkl. dansk moms på nye transportmidler
            antal_tyske      = n(),
            .groups = "drop")
danske_priser
tyske_priser

#---------------- vi laver et sammenligningsark
prissammenligning <- inner_join(danske_priser, tyske_priser,
                                by = c("model_kort" = "model", "registrerings_aargang"))

# vi vil gerne have prisforskellen vist i pct og i kr.
prissammenligning <- prissammenligning |>
  mutate(prisforskel_kr  = dansk_median_pris - tysk_median_pris,               # hvor mange kr. billigere i DE
         prisforskel_pct = prisforskel_kr / dansk_median_pris * 100)           # forskellen i procent af den danske pris

#prisforskellen med afgift og omregistrering
prissammenligning <- prissammenligning |>
  mutate(anslaaet_afgift      = pmax(0, dansk_median_pris - 419300) * 0.60,                # afgift af den del af den danske værdi, der er over bundgrænsen
         tysk_pris_med_afgift = tysk_median_pris + anslaaet_afgift + omregistrering_kr,    # samlet pris købt i DE og indregistreret i DK (inkl. toldsyn og nummerplader)
         besparelse_kr        = dansk_median_pris - tysk_pris_med_afgift)                  # positiv = billigere at købe i Tyskland


print(prissammenligning)
sum(prissammenligning$besparelse_kr > 0)                                       
# 24 af 35 er billigere i Tyskland efter afgift, moms og omregistrering
# problemet vi nu står overfor er at nogle biler har vi kun medianprisen fra 1 eller 3 biler
# det er udfra deres årgang så vi har eks. en bil i tyskland fra årgang 2022 ix1 men den bliver sammenlignet med
# 2 tilsvarende biler og det er ikke fair, mit forslag lad os fjerne dem
#-----------fjern alle bil medianer som har under 5 biler
prissammenligning_sikker <- prissammenligning |>
  filter(antal_danske >= 5, antal_tyske >= 5)                              # kun grupper med mindst 5 biler i begge lande

nrow(prissammenligning_sikker)                                                 # 28 kombinationer tilbage
sum(prissammenligning_sikker$besparelse_kr > 0)                                # 19 af 28 (68 %) er billigere i Tyskland efter afgift, moms og omregistrering
# sidequest: hvilke biltyper kan betale sig at købe i Tyskland?
prissammenligning_sikker <- prissammenligning_sikker |>
  mutate(biltype = case_when(str_detect(model_kort, "^ix") ~ "SUV",            # alle modeller der starter med ix (iX, iX1, iX2, iX3)
                             model_kort %in% c("i5", "i7") ~ "Sedan",          # i5 og i7
                             model_kort == "i4" ~ "Gran Coupé",                # i4 (4-dørs coupé)
                             model_kort == "i3" ~ "Hatchback"))                # i3 (lille bybil)
print(prissammenligning_sikker$biltype)

prissammenligning_sikker |>
  group_by(biltype) |>                                                             # én gruppe pr. biltype
  summarise(median_besparelse    = median(besparelse_kr),                                # typisk besparelse for biltypen
            antal_kan_betale_sig = sum(besparelse_kr > 0),                     # antal kombinationer med besparelse efter afgift
            antal_i_alt          = n())                                        # antal kombinationer i alt
# Gran Coupé 34.168 kr. (4 af 4), SUV 20.285 kr. (12 af 17), Hatchback 459 kr. (1 af 2), Sedan −1.502 kr. (2 af 5)

prissammenligning_sikker |>
  summarise(dk_median        = median(dansk_median_pris),     # dansk pris
            de_foer_afgift   = median(tysk_median_pris),      # tysk pris inkl. moms
            de_efter_afgift  = median(tysk_pris_med_afgift))  # tysk pris + afgift + omregistrering
summarise(dk_median        = median(dansk_median_pris),     # dansk pris
tyskebiler_foer_afgift   = median(tysk_median_pris),      # tysk pris inkl. moms
tyskebiler_efter_afgift  = median(tysk_pris_med_afgift))  # tysk pris + afgift + omregistrering

# =====================================================================
# Opgave 1.3: Kan Powers kampagneside webscrapes?
# =====================================================================
power_url <- "https://www.power.dk/kampagne/aktuelt/"                          # Powers kampagneside

# ---- Forsøg 1: almindelig webscraping ----
power_side <- read_html(power_url)                                             # hent siden uden browser
power_side |> html_elements("article.product-item") |> length()
# 0 produkter: siden er bygget med Angular, så HTML'en er kun et skelet

# ---- Forsøg 2: chromote (en rigtig browser) ----
hent_power_side <- function(basis_url) {                                       # samme som før, bare uden antal_sider
  browser <- ChromoteSession$new()                                             # én usynlig Chrome
  browser$Page$navigate(basis_url)                                             # kun én side, så intet sidetal bagpå
  Sys.sleep(10)                                                                # vent lidt længere, Power er en tung side
  power_html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value   # gem siden som tekst
  print("Power-siden hentet")                                                  # så man kan følge med
  browser$close()                                                              # luk browseren
  return(power_html)
}
power_html <- hent_power_side(power_url)
read_html(power_html) |> html_elements("pwr-product-item-wrapper") |> length()
# 36 produktkort, men de er tomme: Power bygger først indholdet, når kortet er synligt på skærmen (lazy loading)

# ---- Forsøg 3: Powers interne API (fundet i Network → Fetch/XHR) ----
# Kampagnesiden henter selv tilbuddene herfra med JavaScript, når den åbnes
library(httr)
library(jsonlite)
power_api_url <- "https://www.power.dk/api/v2/products?ids=2165908%2C2323626%2C2560639%2C3199486%2C3275702%2C4008995%2C4123359%2C4125280%2C4126596%2C4148875%2C4157177%2C4157294%2C4161440%2C4204328%2C4206178%2C4206198%2C4231859%2C4238275%2C4251464%2C4252867%2C4253039%2C4266300%2C4266301%2C4266317%2C4268029%2C4313164%2C4317058%2C4317069%2C4317071%2C4318138%2C4523413%2C4526586%2C4528037&allowWebStatus8=false"   # Request URL fra Network-fanen
power_svar    <- GET(power_api_url)
power_tilbud  <- fromJSON(content(power_svar, as = "text", encoding = "UTF-8"))
power_tilbud  <- power_tilbud |> select(title, price, previousPrice, categoryName, manufacturerName, productId, url)
nrow(power_tilbud)
view(power_tilbud)
# 33 tilbud = passer med opgavens seks grupper med 4–6 tilbud

# ---- Vurdering ----
# Ja, tilbuddene kan hentes, men ikke ved at scrape kampagnesidens HTML.
# Siden henter selv tilbuddene fra Powers interne API, og det kan vi også.
# API'et er ikke officielt, så det kan ændre sig uden varsel, og id'erne skifter, når Power skifter tilbud.