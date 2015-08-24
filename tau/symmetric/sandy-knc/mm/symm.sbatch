#!/bin/bash
#SBATCH -J tau_symm           # job name
#SBATCH -o tau_symm.o%j       # output and error file name (%j expands to jobID)
#SBATCH -n 2 # total number of mpi tasks requested for Sandybridge E5 cpus
#SBATCH -N 1 # number of nodes
#SBATCH -p development     # queue (partition) -- normal, development, etc.
#SBATCH -t 01:30:00        # run time (hh:mm:ss) - 1.5 hours
#SBATCH --mail-user=srinathv@paratools.com
#SBATCH --mail-type=begin  # email me when the job starts
#SBATCH --mail-type=end    # email me when the job finishes

export MIC_PPN=4

ibrun.symm -m ./tauRun.matmult.mic.sh  -c ./tauRun.matmult.sh


