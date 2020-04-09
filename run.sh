# load required modules
module load gcc-4.9.4

# run analysis job
cd ~/tlverse-covid-forecast
R CMD BATCH --no-save --no-restore \
  R/run_analysis.R logs/analysis.Rout
