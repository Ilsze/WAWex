#!/bin/bash 
#SBATCH --nodes=1 			#R/JAGS typically works better on a single node
#SBATCH --ntasks=1               	# One main task
#SBATCH --ntasks-per-node=1 
#SBATCH --cpus-per-task=4 	#increased CPUs / task for parallel processing
#SBATCH --mem=64G                	# Added explicit memory request 
#SBATCH --partition=medium 
#SBATCH --time=12:00:00 
#SBATCH --job-name=rosenberg
#SBATCH --mail-type=ALL 
#SBATCH --mail-user=jojoskilee@gmail.com
#SBATCH --constraint='cpu_gen:Cascade_Lake'
#SBATCH --output=%x-%j.out 
#SBATCH --error=%x-%j.err
# Set up R library path 
export R_LIBS=~/local/rlibs 
# Clean up modules and load required ones
module purge 
module load R/4.1.2-foss-2021b 
module load JAGS/4.3.0-foss-2022a # Adjust version as needed
# Run the R script
Rscript --no-restore --no-save Main_script_including_data_prep_and_modeling.R
