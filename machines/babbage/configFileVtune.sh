#!/bin/bash 

# to be used with 2 or more nodes 
#use : configFileVtune <ppn> <arg for amplexe-cl -queit -collect > <exec> 
if [ -f configFile.$PBS_JOBID ]
then
  rm configFile.$PBS_JOBID
fi
i=1
for a in $(cat $PBS_NODEFILE) 
do 
  if [ $i -eq 2 ]
  then
    echo -host $a-ib -n $1 amplxe-cl -quiet -collect $2 $3
  else
    echo -host $a-ib -n $1 $3
  fi
  i=$(($i+1))
done >> configFile.$PBS_JOBID
