#!/bin/bash    
#SBATCH -p short              # Partition or queue. In this case, short!
#SBATCH --job-name=StanLCMCR_CO_fit    # Job name
#SBATCH --mail-type=NONE               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --cpus-per-task=4                    # Four per task, one for each thread
#SBATCH --mem-per-cpu=4G                    
#SBATCH --time=15:00:00               # Time limit hrs:min:sec
#SBATCH --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_fit%j.out   # Standard output and error log
#SBATCH --error=/Users/shzh8108/StanLCMCR/fit/logs/slurm_fit%j.err   # %j inserts job number

srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g1 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g1 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g1 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g2 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g2 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g2 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g3 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g3 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g3 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g4 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g4 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g4 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g5 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g5 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g5 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g6 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g6 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g6 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g7 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g7 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g7 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g8 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g8 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g8 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g9 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g9 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g9 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g10 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g10 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g10 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g11 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g11 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g11 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g12 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g12 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g12 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g13 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g13 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g13 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g14 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g14 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g14 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g15 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g15 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g15 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g16 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g16 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g16 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g17 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g17 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g17 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g18 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g18 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g18 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g19 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g19 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g19 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g20 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g20 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g20 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g21 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g21 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g21 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g22 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g22 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g22 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g23 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g23 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g23 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g24 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g24 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_strata_g24 --q975 5 &

srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g1-g2 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g1-g2 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g1-g2 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g3-g6 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g3-g6 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g3-g6 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g7-g9 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g7-g9 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g7-g9 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g10-g12 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g10-g12 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g10-g12 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g13-g17 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g13-g17 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g13-g17 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g18-g19 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g18-g19 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g18-g19 --q975 5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g20-24 --q975 1.5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g20-24 --q975 3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_7 --dataset CO_superstrata_g20-24 --q975 5 &

srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out--ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g1 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g2 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g4 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g6 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g7 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g8 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g9 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g10 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g11 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g12 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g13 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g14 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g15 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g16 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g17 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g18 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g19 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g20 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g21 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g22 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g23 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_strata_g24 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g1-g2 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g3-g6 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g7-g9 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g10-g12 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g13-g17 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g18-g19 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model LCMCR_4 --dataset CO_superstrata_g20-24 &

srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g1 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g2 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g3 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g4 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g5 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g6 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g7 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g8 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g9 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g10 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g11 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g12 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g13 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g14 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g15 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g16 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g17 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g18 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g19 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g20 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g21 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g22 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g23 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_strata_g24 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g1-g2 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g3-g6 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g7-g9 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g10-g12 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g13-g17 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g18-g19 &
srun --output=/Users/shzh8108/StanLCMCR/fit/logs/slurm_%A_%a.out --ntasks=1 --nodes=1 /opt/R/4.2.0/bin/Rscript /Users/shzh8108/StanLCMCR/fit/src/fit_stan_cli.R --model R --dataset CO_superstrata_g20-24 &
