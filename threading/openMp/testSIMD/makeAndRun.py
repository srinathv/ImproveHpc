#! /usr/bin/env python

#make for the all the simds
# go throug the list of ifdefs
#create an object file namespaced
#link object files namespaced to namespaced executable

#after each build then run
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


#ifdefsList=['AVX','SIMDAULX','NOVEC']
ifdefsList=['SIMD','SIMDU','SIMDUL','SIMDL','SIMDA','SIMDAU','SIMDAL','SIMDAUL','NONE','AVX','SIMDAULX','NOVEC']
filesList=['init','sum','main']

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
  fileName = '' #initialize
  
  for ifdef in ifdefsList:
    if ifdef == 'NONE':
       ifdefMacro = ' '
       cmdBase='ifort'
    elif ifdef == 'AVX' :
       ifdefMacro = ' '
       cmdBase='ifort -xAVX'
    elif ifdef == 'NOVEC' :
       ifdefMacro = ' '
       cmdBase='ifort -no-vec -no-simd'
    elif ifdef == 'SIMDAULX' :
       ifdefMacro = '-D' + ifdef
       cmdBase='ifort -openmp -xAVX'
    else:
       ifdefMacro = '-D' + ifdef
       cmdBase='ifort -openmp'

    objFileList=[]
    exe=''

    for fileBase in filesList:
            
      reportFlag = ' -opt-report=5 -opt-report-file=' + fileBase + '_' + ifdef + '.optrpt '
      objFile=fileBase + '_'+ ifdef + '.o'
      objFileList.append(objFile)
      command1 = cmdBase + ' ' + ifdefMacro + reportFlag + ' -c ' + fileBase+'.F90' + ' -o ' + objFile
      errorMessage = ' compilation failed for ' + objFile
      shellCommand(command1,errorMessage)
#      print command1
    command2 = cmdBase

    for obj in objFileList:
      command2 = command2 + ' ' +obj
    exe =  'main_' + ifdef
    command2= command2 + ' -o ' + exe
    errorMessage=' Linking failed for ' + exe
    shellCommand(command2,errorMessage)
#    print command2
    
    print "***Running  " + exe + " ***"
    command3='./' + exe
    errorMessage=' Failed to run ' + exe
    shellCommand(command3,errorMessage)
#    print command3

if __name__ == "__main__":
   main(sys.argv[1:])


   
