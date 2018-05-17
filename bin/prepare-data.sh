 #!/bin/sh

# strip first header line from original csv
echo -n "removing csv header.."
tail -n +2 data/firmenakten.csv > data/firmenakten_headless.csv
echo " done."

# import csv into relational db
echo -n "importing csv data into sqlite db.."
sqlite3 data/firmenakten.sqlite < processing/import-csv.sql
echo " done."

