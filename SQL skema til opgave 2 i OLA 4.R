-- =====================================================================
  -- OLA 4 – Opgave 2.1: Skema til bilbasen-databasen (DDL)
-- =====================================================================
  -- Invariante entiteter (ændrer sig ikke):  forhandler, bil
-- Varierende data (tidsserie):             pris  – ny række, når prisen ændrer sig
-- forhandler 1 ── * bil 1 ── * pris
-- Kør denne fil først og derefter "opgave 2.2 insert.sql" for at få de samme data.
-- =====================================================================
  CREATE DATABASE IF NOT EXISTS bilbasen;
USE bilbasen;

-- Slet gamle tabeller, så scriptet kan køres igen (pris først pga. fremmednøglerne)
DROP TABLE IF EXISTS pris;
DROP TABLE IF EXISTS bil;
DROP TABLE IF EXISTS forhandler;

-- Forhandleren. Eget id, fordi kæder (fx Ejner Hessel) deler CVR-nummer.
CREATE TABLE forhandler (
  forhandler_id       INT PRIMARY KEY,
  forhandler_navn     VARCHAR(255),
  forhandler_adresse  VARCHAR(255),
  forhandler_by       VARCHAR(100),
  forhandler_landsdel VARCHAR(100),
  forhandler_cvr      VARCHAR(8)
);

-- Bilen. carid kommer fra Bilbasen og er primærnøgle. sold markerer solgte biler.
CREATE TABLE bil (
  carid                 INT PRIMARY KEY,
  model                 VARCHAR(100),
  model_kort            VARCHAR(20),
  registrerings_aargang INT,
  km                    INT,
  link                  VARCHAR(500),
  forhandler_id         INT,
  sold                  BOOLEAN DEFAULT FALSE,
  FOREIGN KEY (forhandler_id) REFERENCES forhandler(forhandler_id)
);

-- Prisen som tidsserie: en ny række, når en bil er ny, eller når prisen ændrer sig.
CREATE TABLE pris (
  pris_id    INT AUTO_INCREMENT PRIMARY KEY,
  carid      INT,
  pris       INT,
  scrapedate DATE,
  FOREIGN KEY (carid) REFERENCES bil(carid)
);