#!/bin/bash

#SBATCH -J my_LCS
#SBATCH -N 1
#SBATCH -n 8
#SBATCH -p shared
#SBATCH -t 70:00:00
#SBATCH --mem 55G
#SBATCH --array 0-5
#SBATCH -o ./slurm/lcs.%a.out
#SBATCH --mail-user=ellen_considine@g.harvard.edu
#SBATCH --mail-type=END

#
source ~/.bashrc
module load R/4.0.2-fasrc01
export R_LIBS_USER=$HOME/apps/R_4-0-2:$R_LIBS_USER
module load python/3.9.12-fasrc01
python On_cluster/pylauncher.py --job_file="./On_cluster/LCS_sims" --i $SLURM_ARRAY_TASK_ID

