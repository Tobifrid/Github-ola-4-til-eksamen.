library(DBI)
library(RMariaDB)
condb <- dbConnect(MariaDB(),
                   db="bilbasen",
                   host="localhost",
                   user="root",
                   password=Sys.getenv("MYSQLPW"),
                   port=3306)
Sys.getenv("MYSQLPW") 
usethis::edit_r_environ()

dbGetQuery(condb, "SELECT carid FROM bil LIMIT 5")
dbGetQuery(condb, "SELECT carid, model, registrerings_aargang FROM bil WHERE registrerings_aargang=2023 AND model_kort='ix3'")
dbGetQuery(condb, "SELECT forhandler_navn,forhandler_by FROM forhandler LIMIT 10")
dbGetQuery(condb, "SELECT forhandler_navn,forhandler_landsdel FROM forhandler WHERE forhandler_landsdel='FYN'")
dbGetQuery(condb, "select pris FROM pris order by pris desc limit 10")
dbGetQuery(condb, "select COUNT(carid) FROM bil")
dbGetQuery(condb, "select count(*) From bil where sold = '1'")
dbGetQuery(condb, "select model_kort, count(*) FROM bil GROUP BY model_kort")
dbGetQuery(condb, "select model_kort,count(*) as antal_biler from bil group by model_kort order by antal_biler desc")
dbGetQuery(condb, "select forhandler_landsdel, count(*) as antal_forhandlere from forhandler group by forhandler_landsdel order by antal_forhandlere desc")
