#!/bin/bash
EMAIL=$1

RSCRIPT_CMD="/opt/local/bin/Rscript"
WORKING_DIR="/users/peter/Documents/baseball/analysis/umpires"

TODAY=`date +%Y-%m-%d`
YESTERDAY=`date -v-1d +%Y-%m-%d`
TIMESTAMP=`date +%Y%m%d.%H%M%S`
YEAR=`date +%Y`

SAVE_DIR="$WORKING_DIR/umpire_uzr"
AREA_PREFIX="sz_area"
RES_PREFIX="results_"
RES_TOT_PREFIX="results_tot_"
FILE_TYPE=".txt"
MODEL_DIR="$WORKING_DIR/models.umpire"
BATTER_DIR="$WORKING_DIR/models.batter"
MODEL_PREFIX="generic.$YEAR"

echo "emailing to $EMAIL"

# Process:
# 0. Backup old umpire model files to directory
# 1. Update batter models (load batter_nn.R)
# 2. Repair any bad models (load fix_bad_batters, run fix.bad.models.f("./models.batter"))
# 3. Update database with batter strike zone data (load batter_sz.R)
# 4. Update umpire NN models (load umpire_generic_yearly.R)
# 5. Repair any bad models (load fix_bad_models, run fix.bad.models.f("./models.umpire"))
# 6. Calculate NN zone area (load sz_area.R, run dt.me <- area.all.years.f(min.year=YYYY, max.year=YYYY))
# 7. Calculate Roeg zone area (run dt.roeg <- roegele.all.years.f(min.year=YYYY, max.year=YYYY))
# 8. Calculate diff (diff <- dt.me$area - dt.roeg$area)
# 9. Save diff to file
# 10. Update umpire evaluations (load umpire_eval.R)
# 11. Run SQL queries to get umpire data
# 12. Save umpire data to file

# Make backup directory for old ump models
BACKUP_DIR="$MODEL_DIR/backup.$TIMESTAMP"

echo $BACKUP_DIR
mkdir $BACKUP_DIR

# Copy old ump models to backup directory
CP_SOURCE="$MODEL_DIR/$MODEL_PREFIX*"
CP_TARGET="$BACKUP_DIR/"
cp $CP_SOURCE $CP_TARGET

# Copy old batter models to backup directory
BAT_BACKUP_DIR="$BATTER_DIR/backup.$TIMESTAMP"
mkdir $BAT_BACKUP_DIR
CP_SOURCE="$BATTER_DIR/nn.bat*$YEAR*.rda"
CP_TARGET="$BAT_BACKUP_DIR/"
cp $CP_SOURCE $CP_TARGET
rm $CP_SOURCE

TS=`date`
echo "$TS Updating batter models"
$RSCRIPT_CMD "$WORKING_DIR/automated_batter_update.R"

TS=`date`
echo "$TS Updating umpire models"
$RSCRIPT_CMD "$WORKING_DIR/umpire_generic_yearly.R"

$RSCRIPT_CMD "$WORKING_DIR/automated_umpire_fix.R"

$RSCRIPT_CMD "$WORKING_DIR/automated_sz_area.R"

TS=`date`
echo "$TS Evaluating umpire performance"
$RSCRIPT_CMD "$WORKING_DIR/umpire_eval.R"

$RSCRIPT_CMD "$WORKING_DIR/automated_ump_uzr_calc.R"

# Send emails with results
TS=`date`
echo "$TS sending results emails"
SZAREA_FILE="$SAVE_DIR/sz_area_$TODAY.txt"
UZR_FILE="$SAVE_DIR/results_$TODAY.txt"
UZR_AGG_FILE="$SAVE_DIR/results_tot_$TODAY.txt"

mail -v -s "$YESTERDAY umpire strike zone area" $EMAIL < $SZAREA_FILE
mail -v -s "$YESTERDAY umpire UZR - individual" $EMAIL < $UZR_FILE
mail -v -s "$YESTERDAY umpire UZR - aggregate" $EMAIL < $UZR_AGG_FILE
