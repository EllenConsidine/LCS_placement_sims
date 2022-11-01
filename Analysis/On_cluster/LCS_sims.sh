#!/bin/bash

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
python On_cluster/pylauncher.py --job_file="./On_cluster/LCS_sims" --i $SLURM_ARRAY_TASK_ID

