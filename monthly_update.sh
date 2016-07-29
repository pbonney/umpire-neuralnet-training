#!/bin/bash
Rscript monthly_update.R

EMAIL=$1
TODAY=`date +%Y-%m-%d`
YESTERDAY=`date -v-1d +%Y-%m-%d`
mail -s "$YESTERDAY umpire strike zone area" $EMAIL < "umpire_uzr/sz_area_$TODAY.txt"
mail -s "$YESTERDAY umpire UZR - individual" $EMAIL < "umpire_uzr/results_$TODAY.txt"
mail -s "$YESTERDAY umpire UZR - aggregate" $EMAIL < "umpire_uzr/results_tot_$TODAY.txt"
