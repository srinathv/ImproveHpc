#!/bin/bash 

# to be used with 2 or more nodes 
#use : configFileVtune <ppn> <arg for amplexe-cl -queit -collect > <exec> 



function usage {

  echo " "
  echo " This script runs soley on babbage with vtune on SNBx.  Will be expanded "
  echo " It requires 2 or greater nodes. "
  echo " It will start up 1 amplxe-cl on the second node of the pool"
  echo " To use:  "
  echo " ./configFileVtune.sh "
  echo " -p <number of mpi ranks node: ppn>  "
  echo " -a <arg for amplexe-cl -queit -collect>"
  echo " -e <exe> -1 <if wanting to run Vtune on single rank of the node>. ""
  echo " "
}







if [ -f configFile.$PBS_JOBID ]
then
  rm configFile.$PBS_JOBID
fi


numRanks=
execName=
single="FALSE"


while getopts "hp:a:e:1" OPTION
do
  case $OPTION in
    h)
      usage
      exit 1
      ;;
    p)
      ppn=$OPTARG
      ;;	
    a)
      args=$OPTARG
      ;;
    e)
      execName=$OPTARG
    1)
      single="TRUE"		  
      ;;
  esac       	 
done




i=1
for a in $(cat $PBS_NODEFILE) 
do
  if [ $single -eq "FALSE" ] 
  then
    if [ $i -eq 2 ]
    then
      echo -host $a-ib -n $ppn amplxe-cl -quiet -collect $args $execName
    else
      echo -host $a-ib -n $ppn $execName
    fi
  else
    if [ $i -eq 2 ]
    then
      echo -host $a-ib -n 1 amplxe-cl -quiet -collect $args $execName
      echo -host $a-ib -n $(($ppn-1)) $execName
    else
      echo -host $a-ib -n $ppn $execName
  fi 
  i=$(($i+1))
done >> configfile.$PBS_JOBID
