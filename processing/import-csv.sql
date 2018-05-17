-- drop table if exists
DROP TABLE IF EXISTS firmenakten

-- import csv
.mode csv

-- import csv
CREATE TABLE firmenakten(
    "archivaliensignatur" TEXT,
    "datierung_von" INTEGER,
    "datierung_bis" INTEGER,
    "datierung" TEXT,
    "klassifikationsgruppe" TEXT,
    "aktentitel" TEXT,
    "enthaelt_vermerk" TEXT,
    "umfang_der_akte" TEXT,
    "sortierfeld_intern"
);

.mode csv
.import data/firmenakten_headless.csv firmenakten
