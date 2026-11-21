# =====================================================================
# OLA 4 – Opgave 2: Bilbasen i MySQL
# Kør først "opgave 2 skema.sql" i DBeaver, så tabellerne findes.
#  
# =====================================================================
library(DBI)
library(RMariaDB)
library(dplyr)
library(stringr)

alle_biler_med_forhandler <- readRDS("alle_biler_med_forhandler.rds")         # resultatet fra opgave 1.1

forbindelse <- dbConnect(MariaDB(), host = "localhost", user = "root",
                         password = rstudioapi::askForPassword("MySQL-kodeord"),  # kodeordet står ikke i scriptet
                         db = "bilbasen", port = 3306)


# =====================================================================
# Opgave 2.2: Gem første kørsel i databasen
# =====================================================================
# ---- Del data op i de tre tabeller ----
forhandler_tabel <- alle_biler_med_forhandler |>
  distinct(forhandler_navn, forhandler_adresse, forhandler_by, forhandler_landsdel, forhandler_cvr) |>   # én række pr. forhandler-afdeling
  mutate(forhandler_id = row_number())# husk vi giver forhandlere deres eget id, fordi kæder deler CVR

bil_tabel <- alle_biler_med_forhandler |>
  left_join(forhandler_tabel, by = c("forhandler_navn", "forhandler_adresse", "forhandler_by",
                                     "forhandler_landsdel", "forhandler_cvr")) |>   # giv hver bil sin forhandlers id
  mutate(carid = as.integer(carid), sold = FALSE) |>
  select(carid, model, model_kort, registrerings_aargang, km, link, forhandler_id, sold)

pris_tabel <- alle_biler_med_forhandler |>
  mutate(carid = as.integer(carid)) |>
  select(carid, pris, scrapedate)                                              # prisen + hvornår den blev set

# ---- Gem INSERT-statements som .sql-fil husk writeLines gemmer lokalt på ens prjektmappe ikke i github ----
# sqlAppendTable laver INSERT INTO ... VALUES ... den er super smart, den laver vores insert_statements til et sprog som SQL forstår
insert_statements <- c(sqlAppendTable(forbindelse, "forhandler", forhandler_tabel, row.names = FALSE),
                       sqlAppendTable(forbindelse, "bil",        bil_tabel,        row.names = FALSE),
                       sqlAppendTable(forbindelse, "pris",       pris_tabel,       row.names = FALSE))
writeLines(paste0(insert_statements, ";"), "opgave 2.2 insert.sql")          

# ---- Indsæt i databasen (rækkefølgen er vigtig pga. fremmednøglerne) ----
dbAppendTable(forbindelse, "forhandler", forhandler_tabel)                     # først forhandlere
dbAppendTable(forbindelse, "bil",        bil_tabel)                            # så biler (peger på forhandler)
dbAppendTable(forbindelse, "pris",       pris_tabel)                           # til sidst priser (peger på bil)

dbGetQuery(forbindelse, "SELECT COUNT(*) AS antal_biler FROM bil")             # 1.215


# =====================================================================
# Opgave 2.3: Simulér kørsel-II og opdatér databasen
# =====================================================================
# ---- Byg kørsel-II ud fra kørsel 1 ----
set.seed(1)                                                                    # samme tilfældige valg hver gang
koersel_2 <- alle_biler_med_forhandler |>
  mutate(carid = as.integer(carid),
         scrapedate = scrapedate + 1)                                          # én dag senere

solgte_id  <- sample(koersel_2$carid, 5)                                       # 5 biler forsvinder = solgt
koersel_2  <- koersel_2 |> filter(!carid %in% solgte_id)

aendret_id <- sample(koersel_2$carid, 3)                                       # 3 biler får ny pris
koersel_2  <- koersel_2 |> mutate(pris = ifelse(carid %in% aendret_id, pris - 10000, pris))

nye_biler  <- koersel_2 |> slice(1:2) |> mutate(carid = c(9999991L, 9999992L)) # 2 nye biler (kopi med nyt carid)
koersel_2  <- bind_rows(koersel_2, nye_biler)

# ---- Hent seneste kørsel fra databasen ----
seneste_koersel <- dbGetQuery(forbindelse, "
  SELECT b.carid, p.pris
  FROM bil b
  JOIN pris p ON p.carid = b.carid
  WHERE b.sold = FALSE
    AND p.scrapedate = (SELECT MAX(p2.scrapedate) FROM pris p2 WHERE p2.carid = b.carid);")   # nyeste pris pr. bil

# ---- a) b) c): Find nye, ændrede og solgte ----
nye      <- anti_join(koersel_2, seneste_koersel, by = "carid")                # i kørsel-II, men ikke i databasen
solgte   <- anti_join(seneste_koersel, koersel_2, by = "carid")                # i databasen, men ikke i kørsel-II
aendrede <- inner_join(koersel_2, seneste_koersel, by = "carid", suffix = c("_ny", "_gammel")) |>
  filter(pris_ny != pris_gammel)                                               # samme bil, ny pris

nrow(nye)                                                                      # 2
nrow(aendrede)                                                                 # 3
nrow(solgte)                                                                   # 5

# ---- Opdatér databasen ----
forhandlere_i_db <- dbGetQuery(forbindelse, "SELECT * FROM forhandler")

nye_bil_tabel <- nye |>
  left_join(forhandlere_i_db, by = c("forhandler_navn", "forhandler_adresse")) |>   # find forhandlerens id
  mutate(sold = FALSE) |>
  select(carid, model, model_kort, registrerings_aargang, km, link, forhandler_id, sold)
dbAppendTable(forbindelse, "bil", nye_bil_tabel)                               # a) nye biler

dbAppendTable(forbindelse, "pris", nye |> select(carid, pris, scrapedate))     # a) pris for nye biler
dbAppendTable(forbindelse, "pris", aendrede |>
                select(carid, pris = pris_ny, scrapedate))                     # b) ny pris = ny række (gammel pris bevares)

dbExecute(forbindelse, str_c("UPDATE bil SET sold = TRUE WHERE carid IN (",
                             str_c(solgte$carid, collapse = ", "), ");"))      # c) solgte markeres, slettes ikke

# ---- Tjek ----
dbGetQuery(forbindelse, "SELECT COUNT(*) AS solgte FROM bil WHERE sold = TRUE")                 # 5
dbGetQuery(forbindelse, "SELECT carid, pris, scrapedate FROM pris
                         WHERE carid IN (SELECT carid FROM pris GROUP BY carid HAVING COUNT(*) > 1)
                         ORDER BY carid, scrapedate")                          # de 3 biler med to priser

dbDisconnect(forbindelse)