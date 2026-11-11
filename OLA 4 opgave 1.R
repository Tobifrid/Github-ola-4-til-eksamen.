library(chromote)
library(rvest)
library(stringr)
library(dplyr)
Bilbasen_BMW <- "https://www.bilbasen.dk/brugt/bil/bmw?cartype=stationcar&cartype=suv&cartype=cuv&cartype=sedan&cartype=hatchback&fuel=3&fuel=6&pricetype=Retail&yearfrom=2020&page="   # page= til sidst, uden tal

Biler_BMW <- ChromoteSession$new()                     # én browser til alle sider
alle_sider <- list()                             # tom liste før loopet

for (i in 1:46) {
  side_url <- str_c(Bilbasen_BMW, i)                 # fx ...&page=1, ...&page=2
  Biler_BMW$Page$navigate(side_url)                    # gå til siden
  Sys.sleep(5)                                   # vent på loading, og vær høflig over for Bilbasen
  alle_sider[[i]] <- Biler_BMW$Runtime$evaluate("document.documentElement.outerHTML")$result$value   # gem siden i listen
  print(str_c("Side ", i, " hentet"))            # så du kan følge med
}

Biler_BMW$close()                                      # luk browseren
saveRDS(alle_sider, "alle_vores_BMW.rds")   # gem alle 46 sider i én fil, KØR KUN 1 GANG

alle_sider_fra_Bilbasen_BMW <- readRDS("alle_vores_BMW.rds")         # det nye filnavn

for (i in 1:length(alle_sider_fra_Bilbasen_BMW)) {
  side_i_Bilbasen_BMW <- read_html(alle_sider_fra_Bilbasen_BMW[[i]])   # læs side i fra filen, ikke fra nettet
  bil_liste <- side_i_Bilbasen_BMW |> html_elements("article")         # article = én bil, fundet med command + option + I
  for (bil in bil_liste) {
  }
}

side1 <- read_html(alle_sider_fra_Bilbasen_BMW[[1]])
bil_liste <- side1 |> html_elements("article")
length(bil_liste)                                       
forste_bil <- bil_liste[[1]]
forste_bil |> html_text2()#tryk her for at se bilen
bil_tekst <- forste_bil |> html_text2()   # HTML til tekst ellers kan vi ikke lave str_
pris <- str_extract(bil_tekst, "[\\d.]+ kr")  # cifre ELLER punktum, et eller flere, så " kr"
pris
# "419.900 kr"
km <- str_extract(bil_tekst,"[\\d.]+ km")
km
#15.500 km
registrerings_dato <- str_extract(bil_tekst, "\\d{1,2}/\\d{4}")
#                                            måned  /  år
registrerings_dato

#sammenlign biler Find de biler, der var til salg i går, men som er væk i dag. De er sandsynligvis solgt.

korsel_1 <- data.frame( #igår
  carid = c(101, 102, 103, 104, 105),
  model = c("BMW iX3", "BMW i4", "BMW i3", "BMW iX1", "BMW i5"),
  pris  = c(419900, 349900, 189900, 379900, 549900)
)

korsel_2 <- data.frame( #idag se hvilke biler der idag ikke er til salg længere
  carid = c(102, 103, 105, 106, 107),
  model = c("BMW i4", "BMW i3", "BMW i5", "BMW iX", "BMW i4"),
  pris  = c(339900, 189900, 549900, 629900, 359900)
)

solgte_biler <- anti_join(korsel_1, korsel_2, by = "carid")
nye_biler_i_dag <- anti_join(korsel_2,korsel_1,by="carid")
sammenlign_bilerne_i_butikken<- inner_join(korsel_1,korsel_2,by="carid")
sammenlign_bilerne_i_butikken <- sammenlign_bilerne_i_butikken|>select(-model.y)
sammenlign_bilerne_i_butikken <- sammenlign_bilerne_i_butikken |>
  rename(
    model=model.x,
    pris_i_gaar = pris.x,
    pris_i_dag = pris.y
  )
sammenlign_bilerne_i_butikken <- sammenlign_bilerne_i_butikken|>mutate(
 prisaendringer=pris_i_dag-pris_i_gaar
)
# Vis kun biler, hvor prisen har ændret sig (ændringen er IKKE 0, det er bare hvor meget de har ændret sig fra deres start)
biler_med_prisaendring<- sammenlign_bilerne_i_butikken |> 
  filter(prisaendringer != 0)
# nu kobler jeg bilforhandlere til
biler <- data.frame(
  carid         = c(102, 103, 105, 106, 107),
  model         = c("BMW i4", "BMW i3", "BMW i5", "BMW iX", "BMW i4"),
  pris          = c(339900, 189900, 549900, 629900, 359900),
  forhandler_id = c(1, 2, 1, 3, 4)
)

forhandlere <- data.frame(
  forhandler_id = c(1, 2, 3, 5),
  forhandler    = c("Bilhuset Hammel", "BMW Lyngby", "Auto Aarhus", "Bil Odense"),
  region        = c("Østjylland", "Sjælland", "Østjylland", "Fyn")
)
biler_med_forhandler <- left_join(biler, forhandlere, by = "forhandler_id")
biler_med_forhandler
nrow(biler_med_forhandler)
biler_med_forhandler |> count(region)
biler_med_forhandler |>
  group_by(region) |>
  summarise(gennemsnitspris = round(mean(pris)))
