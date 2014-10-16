#! /usr/bin/env python

#make for the all the simds
# go throug the list of ifdefs
#create an object file namespaced
#link object files namespaced to namespaced executable

#after all are built, run them in suquence writing out their name first

##ifdef SIMD
#!$omp declare simd(init)
##ifdef SIMDU
#!$omp declare simd(init) uniform(n) 
##ifdef SIMDUL
#!$omp declare simd(init) uniform(n) simdlen(4)
##ifdef SIMDL
#!$omp declare simd(init) simdlen(4)
##ifdef SIMDA
#!$omp declare simd(init) alinged(a)
##ifdef SIMDAU
#!$omp declare simd(init) alinged(a) uniform(n) simdlen(4)
##ifdef SIMDAL
#!$omp declare simd(init) alinged(a) simdlen(4)
##ifdef SIMDAUL
#!$omp declare simd(init) alinged(a) uniform(n) simdlen(4)
##endif

import subprocess
import os,sys,getopt

def shellCommand(command,errorMessage):
#command initiated where this script is ran
  try:
    print command
    subprocess.check_call(command, stderr=subprocess.STDOUT, shell=True)
  except :
    print errorMessage
    pass
  return


ifdefs=['SIMD','SIMDU','SIMDUL','SIMDL','SIMDA','SIMDAU','SIMDAL','SIMDAUL','NONE']
files=['init','sum','main']

def main(argv):
  howToUse = 'just run it and it will make and run the variants'
  try:
    opts, args = getopt.getopt(argv,"h")
  except getopt.GetoptError:
        print howToUse
        sys.exit(2)
        pass
  for opt, arg in opts:
      if opt == '-h':
         print howToUse
         sys.exit()
  print 'Exectuting build and run system'
  caseName = '' #initialize


if __name__ == "__main__":
   main(sys.argv[1:])


   
