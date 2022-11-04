#!/bin/bash
#SBATCH -J my_LCS
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p shared
#SBATCH -t 0-6:00
#SBATCH --mem 55G 
#SBATCH --array 0-161
#SBATCH -o ./slurm/lcs.%a.out
#SBATCH --mail-user=ellen_considine@g.harvard.edu
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#
source ~/.bashrc
module load R/4.0.2-fasrc01
export R_LIBS_USER=$HOME/apps/R_4-0-2:$R_LIBS_USER
module load python/3.9.12-fasrc01
python LCS_placement_sims/Analysis/On_cluster/pylauncher.py --job_file="./LCS_placement_sims/Analysis/On_cluster/LCS_sims" --i $SLURM_ARRAY_TASK_ID

