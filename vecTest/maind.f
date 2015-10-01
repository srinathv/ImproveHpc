c***********************************************************************
c                TEST SUITE FOR VECTORIZING COMPILERS                  *
c                        (File 1 of 2)                                 *
c                                                                      *
c  Version:   2.0                                                      *
c  Date:      3/14/88                                                  *
c  Authors:   Original loops from a variety of                         *
c             sources. Collection and synthesis by                     *
c                                                                      *
c             David Callahan  -  Tera Computer                         *
c             Jack Dongarra   -  University of Tennessee               *
c             David Levine    -  Argonne National Laboratory           *
c***********************************************************************
c  Version:   3.0                                                      *
c  Date:      1/4/91                                                   *
c  Authors:   David Levine    -  Executable version                    *
c***********************************************************************
c                         ---DESCRIPTION---                            *
c                                                                      *
c  This test consists of a variety of  loops that represent different  *
c  constructs  intended   to  test  the   analysis  capabilities of a  *
c  vectorizing  compiler.  Each loop is  executed with vector lengths  *
c  of 10, 100, and 1000.   Also included  are several simple  control  *
c  loops  intended  to  provide  a  baseline  measure  for  comparing  *
c  compiler performance on the more complicated loops.                 *
c                                                                      *
c  The  output from a  run  of the test  consists of seven columns of  *
c  data:                                                               *
c     Loop:        The name of the loop.                               *
c     VL:          The vector length the loop was run at.              *
c     Seconds:     The time in seconds to run the loop.                *
c     Checksum:    The checksum calculated when running the test.      *
c     PreComputed: The precomputed checksum (64-bit IEEE arithmetic).  *
c     Residual:    A measure of the accuracy of the calculated         *
c                  checksum versus the precomputed checksum.           *
c     No.:         The number of the loop in the test suite.           *
c                                                                      *
c  The  residual  calculation  is  intended  as  a  check  that   the  *
c  computation  was  done  correctly  and  that 64-bit arithmetic was  *
c  used.   Small   residuals    from    non-IEEE    arithmetic    and  *
c  nonassociativity  of   some calculations  are   acceptable.  Large  *
c  residuals  from   incorrect  computations or  the  use   of 32-bit  *
c  arithmetic are not acceptable.                                      *
c                                                                      *
c  The test  output  itself  does not report   any  results;  it just  *
c  contains data.  Absolute  measures  such as Mflops and  total time  *
c  used  are  not   appropriate    metrics  for  this  test.   Proper  *
c  interpretation of the results involves correlating the output from  *
c  scalar and vector runs  and the  loops which  have been vectorized  *
c  with the speedup achieved at different vector lengths.              *
c                                                                      *
c  These loops  are intended only  as  a partial test of the analysis  *
c  capabilities of a vectorizing compiler (and, by necessity,  a test  *
c  of the speed and  features  of the underlying   vector  hardware).  *
c  These loops  are  by no means  a  complete  test  of a vectorizing  *
c  compiler and should not be interpreted as such.                     *
c                                                                      *
c***********************************************************************
c                           ---DIRECTIONS---                           *
c                                                                      *
c  To  run this  test,  you will  need  to  supply  a  function named  *
c  second() that returns user CPU time.                                *
c                                                                      *
c  This test is distributed as two separate files, one containing the  *
c  driver  and  one containing the loops.   These  two files MUST  be  *
c  compiled separately.                                                *
c                                                                      *
c  Results must  be supplied from  both scalar and vector  runs using  *
c  the following rules for compilation:                                *
c                                                                      *
c     Compilation   of the  driver  file must  not  use any  compiler  *
c     optimizations (e.g., vectorization, function  inlining,  global  *
c     optimizations,...).   This   file   also must  not  be analyzed  *
c     interprocedurally to  gather information useful  in  optimizing  *
c     the test loops.                                                  *
c                                                                      *
c     The file containing the  loops must be compiled twice--once for  *
c     a scalar run and once for a vector run.                          *
c                                                                      *
c        For the scalar  run, global (scalar) optimizations should be  *
c        used.                                                         *
c                                                                      *
c        For  the  vector run,  in  addition   to  the  same   global  *
c        optimizations specified  in the scalar   run,  vectorization  *
c        and--if available--automatic  call generation to   optimized  *
c        library  routines,  function inlining,  and  interprocedural  *
c        analysis should be  used.  Note again that function inlining  *
c        and interprocedural  analysis  must  not be  used to  gather  *
c        information  about any of the  program  units  in the driver  *
c        program.                                                      *
c                                                                      *
c     No changes  may  be made  to   the   source code.   No compiler  *
c     directives may be used, nor may  a file  be split into separate  *
c     program units.  (The exception is  filling  in  the information  *
c     requested in subroutine "info" as described below.)              *
c                                                                      *
c     All files must be compiled to use 64-bit arithmetic.             *
c                                                                      *
c     The  outer  timing  loop  is  included  only   to increase  the  *
c     granularity of the calculation.  It should not be vectorizable.  *
c     If it is found to be so, please notify the authors.              *
c                                                                      *
c  All runs  must be  made  on a standalone  system  to minimize  any  *
c  external effects.                                                   *
c                                                                      *
c  On virtual memory computers,  runs should be  made with a physical  *
c  memory and working-set  size  large enough  that  any  performance  *
c  degradation from page  faults is negligible.   Also,  the  timings  *
c  should be repeatable  and you  must  ensure  that timing anomalies  *
c  resulting from paging effects are not present.                      *
c                                                                      *
c  You should edit subroutine "info"   (the  last subroutine  in  the  *
c  driver program) with  information specific to your  runs, so  that  *
c  the test output will be annotated automatically.                    *
c                                                                      *
c  Please return the following three files in an electronic format:    *
c                                                                      *
c  1. Test output from a scalar run.                                   *
c  2. Test output from a vector run.                                   *
c  3. Compiler output listing (source echo, diagnostics, and messages) *
c     showing which loops have been vectorized.                        *
c                                                                      *
c  The preferred media  for receipt, in order  of preference, are (1)  *
c  electronic mail, (2) 9-track  magnetic or  cartridge tape in  Unix  *
c  tar  format, (3) 5" IBM PC/DOS  floppy   diskette, or  (4) 9-track  *
c  magnetic  tape in  ascii  format,   80 characters per card,  fixed  *
c  records, 40 records per block, 1600bpi.  Please return to           *
c                                                                      *
c  David Levine       		                                       *
c  Mathematics and Computer Science Division                           *
c  Argonne National Laboratory                                         *
c  Argonne, Illinois 60439                                             *
c  levine@mcs.anl.gov                                                  *
c***********************************************************************
      integer ld, nloops
      parameter (ld=1000,nloops=135)
      real dtime, ctime, c471s
      double precision s1, s2, array
      integer ip(ld),indx(ld),n1,n3,n,i,ntimes
      common /cdata/ array(ld*ld)
      double precision a(ld),b(ld),c(ld),d(ld),e(ld),aa(ld,ld),
     +                 bb(ld,ld),cc(ld,ld)

      call title
      n      = 10
      ntimes = 100000
      call set(dtime,ctime,c471s,ip,indx,n1,n3,s1,s2,
     +           n,a,b,c,d,e,aa,bb,cc)

      do 1000 i = 1,3
 
      call s111 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s112 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s113 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s114 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s115 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s116 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s118 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s119 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s121 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s122 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1,n3)
      call s123 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s124 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s125 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s126 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s127 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s128 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s131 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s132 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s141 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s151 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s152 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s161 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s162 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1)
      call s171 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1)
      call s172 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1,n3)
      call s173 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s174 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s175 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1)
      call s176 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s211 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s212 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s221 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s222 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s231 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s232 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s233 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s234 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s235 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s241 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s242 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,s1,s2)
      call s243 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s244 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s251 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s252 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s253 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s254 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s255 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s256 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s257 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s258 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s261 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s271 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s272 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,s1)
      call s273 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s274 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s275 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s276 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s277 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s278 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s279 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s2710(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,s1)
      call s2711(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s2712(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s281 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s291 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s292 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s293 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s2101(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s2102(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s2111(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s311 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s312 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s313 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s314 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s315 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s316 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s317 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s318 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1)
      call s319 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s3110(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s3111(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s3112(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s3113(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s321 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s322 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s323 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s331 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s332 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,s1)
      call s341 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s342 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s343 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s351 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s352 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s353 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip)
      call s411 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s412 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,n1)
      call s413 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s414 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s415 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s421 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s422 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s423 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s424 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s431 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s432 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s441 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s442 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,indx)
      call s443 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s451 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s452 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s453 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s471 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,c471s)
      call s481 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s482 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s491 (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip)
      call s4112(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip,s1)
      call s4113(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip)
      call s4114(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip,n1)
      call s4115(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip)
      call s4116(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip,n/2,n1)
      call s4117(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call s4121(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call va   (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vag  (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip)
      call vas  (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,ip)
      call vif  (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vpv  (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vtv  (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vpvtv(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vpvts(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc,s1)
      call vpvpv(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vtvtv(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vsumr(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vdotr(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      call vbor (ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)

      n      = n*10
      ntimes = ntimes/10

1000  continue

      call info(ctime,dtime,c471s)

      stop
      end

      block data
c
c --  initialize precomputed checksums and array of names
c --  resary contains checksums for vectors of length 10,100, and 1000
c --  snames contains the 5 character string names of the loops
c --  time gets set in subroutine check with execution times
c --  ans  gets set in subroutine check with calculated checksums
c --  nit, number of iterations of inner loop, is not currently used
c
      integer i,nloops,ld,j,nvl
      parameter(ld=1000,nloops=135,nvl=3)
      integer nit(nloops,nvl)
      double precision ans(nloops,nvl),resary(nloops,nvl)
      real            time(nloops,nvl)
      character*5 snames(nloops)
      common /acom/ans,resary
      common /bcom/nit
      common /ccom/snames
      common /tcom/time
c
c --  precomputed checksums 
c
      data ( (resary(i,j), j=1,nvl),i=1,10 ) /
     &     10.36590277778d0,    100.40628318341d0,   1000.41073401638d0,
     &     22.56870905770d0,    258.31101250085d0,   2636.44909582101d0,
     &     10.54976773117d0,    100.63498390018d0,   1000.64393456668d0,
     &     29.63974080373d0,    353.12363498321d0,   3628.96362496028d0,
     &      9.12372283291d0,     99.01324486818d0,    999.00149833537d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &     10.92428292812d0,    100.99325548216d0,   1000.99949866671d0,
     &    122.58323255228d0,  13099.31638829156d0,1321154.90032936420d0,
     &     12.82896825397d0,    105.17737751764d0,   1007.48447086055d0,
     & 154986.77311666883d0,  16449.83900184871d0,   2643.93456668156d0/

      data ( (resary(i,j), j=1,nvl),i=11,20 ) /
     &     12.92722222222d0,    103.25026546724d0,   1003.28587213103d0,
     &      6.46361111111d0,     51.62513273362d0,    501.64293606551d0,
     &    200.00000000000d0,  20000.00000000000d0,2000000.00000000000d0,
     &    122.58409263671d0,  10460.02285184050d0,1006907.25586229020d0,
     &     12.92722222222d0,    103.25026546724d0,   1003.28587213103d0,
     &     25.00000000000d0,    250.00000000000d0,   2500.00000000000d0,
     &     12.82896825397d0,    105.17737751764d0,   1007.48447086055d0,
     &    100.96448412698d0,  10002.09368875882d0,1000003.24273543020d0,
     & 141284.76788863543d0,  25994.59964010348d0,1001638.09303032420d0,
     &     12.82896825397d0,    105.17737751764d0,   1007.48447086055d0/

      data ( (resary(i,j), j=1,nvl),i=21,30 ) /
     & 119763.19856741340d0,  12120.07400659667d0,   2202.05640365934d0,
     &     21.88004550894d0,    202.03631475108d0,   2002.05416908073d0,
     &     12.82896825397d0,    105.17737751764d0,   1007.48447086055d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     &     11.46361111111d0,    101.62513273362d0,   1001.64293606552d0,
     &     11.46361111111d0,    101.62513273362d0,   1001.64293606551d0,
     &     12.82896825397d0,    105.17737751764d0,   1007.48447086055d0,
     &  94863.96825398761d0,   1756.26368022382d0,   1023.06352283494d0,
     &     15.97371236458d0,    191.54999869568d0,   1986.96285998095d0/

      data ( (resary(i,j), j=1,nvl),i=31,40 ) /
     &  90012.92896823291d0,  10005.18737751802d0,   2006.48547086055d0,
     & 466901.67065395752d0, 611283.72814145486d0,1145729.92128578290d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &    169.73954790249d0,  18093.17030591522d0,1821145.31605738050d0,
     &    100.00000000000d0,  10000.00000000000d0,1000000.00000000000d0,
     &    318.99007936508d0,  63220.03164047703d0,9733449.02625389960d0,
     &    169.73954790249d0,  18093.17030591522d0,1821145.31605738050d0,
     & 502593.87943175732d0, 554106.50684144662d0,2362767.41506457240d0,
     &     20.00000000000d0,    200.00000000000d0,   2000.00000000000d0,
     &    135.00014500000d0,  14850.01494999997d0,1498501.49950003670d0/

      data ( (resary(i,j), j=1,nvl),i=41,50 ) /
     & 703168.96900448343d0,  74452.86201741260d0,   9467.85242521049d0,
     &     21.89999209985d0,    201.98990200994d0,   2001.99800200180d0,
     &     14.18157204583d0,    104.35229070571d0,   1004.37019236674d0,
     &     19.00000000000d0,    199.00000000000d0,   1999.00000000000d0,
     & 985374.71938453394d0, 999940.55331344355d0,1001996.24603636260d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &      9.99000000000d0,     99.90000000000d0,    999.00000000002d0,
     &    210.00000000000d0,  20100.00000000000d0,2001000.00000000000d0,
     &    210.00000000000d0,  20100.00000000000d0,2001000.00000000000d0,
     &      8.25300047928d0,     12.77876983660d0,     17.37505452842d0/

      data ( (resary(i,j), j=1,nvl),i=51,60 ) /
     & 208965.09600098155d0,  22800.31298759832d0,   3289.51206792981d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 309973.54623333761d0,  32899.67800369742d0,   5287.86913336312d0,
     &     31.30754181311d0,    301.05213316999d0,   3001.00748921734d0,
     &1154998.32288425600d0,1016551.47398574440d0,1003645.57850125060d0,
     &    100.00000000041d0,  10000.00000048991d0,1000000.00050001150d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &     24.47873598513d0,    206.82236141782d0,   2009.12940542723d0,
     &     25.77728228143d0,    208.47634861140d0,   2010.81938737257d0/

      data ( (resary(i,j), j=1,nvl),i=61,70 ) /
     &     33.09953546233d0,    303.26996780037d0,   3003.28786913336d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     &  54986.77311666882d0,   6449.83900184871d0,   1643.93456668156d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &      9.99000000000d0,     99.90000000000d0,    999.00000000002d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     & 155076.77311666880d0,  26349.83900184871d0,1001643.93456668160d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &      3.00000000000d0,      3.00000000000d0,      3.00000000000d0/

      data ( (resary(i,j), j=1,nvl),i=71,80 ) /
     &      2.92896825397d0,      5.18737751764d0,      7.48547086055d0,
     &      1.00001000004d0,      1.00010000495d0,      1.00100049967d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &      1.00000000000d0,      1.00000000000d0,      1.00000000000d0,
     &      2.00000000000d0,      2.00000000000d0,      2.00000000000d0,
     &      0.10000000000d0,      0.01000000000d0,      0.00100000000d0,
     &      0.99990000450d0,      0.99900049484d0,      0.99004978425d0,
     &     12.00000000000d0,    102.00000000000d0,   1002.00000000000d0,
     &     11.71587301587d0,     20.74951007056d0,     29.94188344220d0,
     &     22.00000000000d0,    202.00000000000d0,   2002.00000000000d0/

      data ( (resary(i,j), j=1,nvl),i=81,90 ) /
     &      2.92896825397d0,      5.18737751764d0,      7.48547086055d0,
     &     15.66824452003d0,    161.58098030122d0,   1639.73696495437d0,
     &      2.00000000000d0,      2.00000000000d0,      2.00000000000d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &     35.92413942429d0,    439.14900170395d0,   4551.72818698408d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &     12.00000000000d0,    102.00000000000d0,   1002.00000000000d0,
     &      2.92896825397d0,      5.18737751764d0,      7.48547086055d0,
     &      2.92896825397d0,      5.18737751764d0,      7.48547086055d0/

      data ( (resary(i,j), j=1,nvl),i=91,100 ) /
     &     29.28968253968d0,    518.73775176393d0,   7485.47086054862d0,
     &5000010.00000000000d0,5000100.00000000000d0,5001000.00000000000d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &5000010.00000000000d0,5000100.00000000000d0,5001000.00000000000d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 153996.32288440072d0,  16548.47398574890d0,   3643.57750124824d0,
     &    228.31891298186d0,  19130.64580944312d0,1836116.25777848130d0,
     & 153984.77311666956d0,  16446.83900184871d0,   2641.93356668156d0,
     &     12.82896825397d0,    105.17737751764d0,   1007.48447086055d0/

      data ( (resary(i,j), j=1,nvl),i=101,110 ) /
     &      6.69827003023d0,     52.29313637543d0,    502.87104683277d0,
     &     10.53976773117d0,    100.64083339222d0,   1000.68224820984d0,
     &     10.53976773117d0,    103.25165081509d0,   1026.25239026486d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     &     12.00663579797d0,    104.22170317263d0,   1006.51531302881d0,
     &     10.00005500000d0,    100.00505000000d0,   1000.50050000000d0/

      data ( (resary(i,j), j=1,nvl),i=111,120 ) /
     &      5.85793650794d0,     10.37475503528d0,     14.97094172110d0,
     &     24.64930319350d0,    204.90495170055d0,   2004.93180370004d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     &     11.54976773117d0,    101.63498390018d0,   1001.64393456668d0,
     & 292906.82539681665d0,  51973.77517639728d0,   8485.47086055035d0,
     &     11.54976773117d0,    101.63498390018d0,   1001.64393456668d0,
     &     10.64027777778d0,    100.10352524037d0,   1000.01496357775d0,
     &      0.94448853616d0,      1.02889405454d0,      1.03784373764d0,
     &      0.65398478836d0,      0.74716878794d0,      0.75617087823d0/

      data ( (resary(i,j), j=1,nvl),i=121,130 ) /
     &     10.24053571429d0,    100.41622200819d0,   1000.43417317164d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     & 154976.77311666883d0,  16349.83900184871d0,   1643.93456668156d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0,
     & 154986.77311666880d0,  16449.83900184871d0,   2643.93456668156d0/

      data ( (resary(i,j), j=1,nvl),i=131,nloops ) /
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &     10.00000000000d0,    100.00000000000d0,   1000.00000000000d0,
     &      2.92896825397d0,      5.18737751764d0,      7.48547086055d0,
     &      1.54976773117d0,      1.63498390018d0,      1.64393456668d0,
     &    180.04429557950d0,    180.04429557960d0,    180.04429557960d0/
c
c --  subroutine name used in function nindex
c
      data ( snames(i),i=1,nloops) /
     &'s111 ','s112 ','s113 ','s114 ','s115 ','s116 ','s118 ','s119 ',
     &'s121 ','s122 ','s123 ','s124 ','s125 ','s126 ','s127 ','s128 ',
     &'s131 ','s132 ','s141 ','s151 ','s152 ','s161 ','s162 ','s171 ',
     &'s172 ','s173 ','s174 ','s175 ','s176 ','s211 ','s212 ','s221 ',
     &'s222 ','s231 ','s232 ','s233 ','s234 ','s235 ','s241 ','s242 ',
     &'s243 ','s244 ','s251 ','s252 ','s253 ','s254 ','s255 ','s256 ',
     &'s257 ','s258 ','s261 ','s271 ','s272 ','s273 ','s274 ','s275 ',
     &'s276 ','s277 ','s278 ','s279 ','s2710','s2711','s2712','s281 ',
     &'s291 ','s292 ','s293 ','s2101','s2102','s2111','s311 ','s312 ',
     &'s313 ','s314 ','s315 ','s316 ','s317 ','s318 ','s319 ','s3110',
     &'s3111','s3112','s3113','s321 ','s322 ','s323 ','s331 ','s332 ',
     &'s341 ','s342 ','s343 ','s351 ','s352 ','s353 ','s411 ','s412 ',
     &'s413 ','s414 ','s415 ','s421 ','s422 ','s423 ','s424 ','s431 ',
     &'s432 ','s441 ','s442 ','s443 ','s451 ','s452 ','s453 ','s471 ',
     &'s481 ','s482 ','s491 ','s4112','s4113','s4114','s4115','s4116',
     &'s4117','s4121','va   ','vag  ','vas  ','vif  ','vpv  ','vtv  ',
     &'vpvtv','vpvts','vpvpv','vtvtv','vsumr','vdotr','vbor '/

      end


      subroutine set(dtime,ctime,c471s,ip,indx,n1,n3,s1,s2,
     +               n,a,b,c,d,e,aa,bb,cc)
c
c --  initialize miscellaneous data
c
      integer ld
      parameter(ld=1000)
      integer ip(ld),indx(ld),n1,n3,k,n,i
      real dtime, ctime, c471s, tdummy, tcall, t471s
      double precision s1, s2
      double precision a(ld),b(ld),c(ld),d(ld),e(ld),aa(ld,ld),
     +                 bb(ld,ld),cc(ld,ld)

      dtime     = tdummy(ld,n,a,b,c,d,e,aa,bb,cc)
      ctime     = tcall()
      c471s     = t471s()

      k = 0
      do 5 i = 1,ld,5
         ip(i)   = (i+4)
         ip(i+1) = (i+2)
         ip(i+2) = (i)  
         ip(i+3) = (i+3)
         ip(i+4) = (i+1)
         k = k + 1
5     continue
      do 6 i = 1,ld
         indx(i) = mod(i,4) + 1
6     continue
      n1   = 1
      n3   = 1
      s1   = 1.0d0
      s2   = 2.0d0

      return
      end


      subroutine title
      write(*,40)
 40   format(/,' Loop    VL     Seconds',
     +'     Checksum      PreComputed  Residual(1.e-10)   No.')

      return
      end

      subroutine set1d(n,array,value,stride)
c
c  -- initialize one-dimensional arrays
c
      integer i, n, stride, frac, frac2
      double precision array(n), value
      parameter(frac=-1,frac2=-2)
      if ( stride .eq. frac ) then
         do 10 i=1,n
            array(i) = 1.0d0/dble(i)
10       continue
      elseif ( stride .eq. frac2 ) then
         do 15 i=1,n
            array(i) = 1.0d0/dble(i*i)
15       continue
      else
         do 20 i=1,n,stride
            array(i) = value
20       continue
      endif
      return
      end

      subroutine set2d(n,array,value,stride)
c
c  -- initialize two-dimensional arrays
c
      integer i, j, n, stride, frac, frac2, ld
      parameter(frac=-1, frac2=-2, ld=1000)
      double precision array(ld,n),value
      if ( stride .eq. frac ) then
         do 10 j=1,n
            do 20 i=1,n
               array(i,j) = 1.0d0/dble(i)
20          continue
10       continue
      elseif ( stride .eq. frac2 ) then
         do 30 j=1,n
            do 40 i=1,n
               array(i,j) = 1.0d0/dble(i*i)
40          continue
30       continue
      else
         do 50 j=1,n,stride
            do 60 i=1,n
               array(i,j) = value
60          continue
50       continue
      endif
      return
      end

      subroutine check (chksum,totit,n,t2,name)
c
c --  called by each loop to record and report results
c --  chksum is the computed checksum
c --  totit is the number of times the loop was executed
c --  n  is the length of the loop
c --  t2 is the time to execute loop 'name'
c
      integer nloops, nvl, i, j, totit, n, nindex
      double precision epslon, chksum, rnorm
      real t2
      parameter (nloops=135,nvl=3,epslon=1.d-10)
      character*5 name
      external nindex
      integer nit (nloops,nvl)
      double precision ans(nloops,nvl),resary(nloops,nvl)
      real            time(nloops,nvl)
      common /acom/ans,resary
      common /bcom/nit
      common /tcom/time
c
c -- get row index based on vector length
c
      if     ( n .eq. 10   ) then
         j = 1
      elseif ( n .eq. 100  ) then
         j = 2
      elseif ( n .eq. 1000 ) then
         j = 3
      else
         print*,'ERROR COMPUTING COLUMN INDEX IN SUB. CHECK, n= ',n
      endif
c
c --  column index is the kernel number from function nindex
c
      i = nindex(name)

      ans (i,j)  = chksum
      nit (i,j)  = totit
      time(i,j)  = t2

      rnorm = sqrt((resary(i,j)-chksum)*(resary(i,j)-chksum))/chksum
      if ( ( rnorm .gt. epslon) .or. ( rnorm .lt. -epslon) ) then
        write(*,98)name,n,t2,chksum,resary(i,j),rnorm,i
      else
        write(*,99)name,n,t2,chksum,resary(i,j),i
      endif

98    format(a6,i5,1x,f12.6,1x,1p,d13.4,1x,1p,d13.4,1p,d13.4,9x,i3)
99    format(a6,i5,1x,f12.6,1x,1p,d13.4,1x,1p,d13.4,22x,i3)

      return
      end


      double precision function cs1d(n,a)
c
c --  calculate one-dimensional checksum
c
      integer i,n
      double precision a(n), sum
      sum = 0.0d0
      do 10 i = 1,n
         sum = sum + a(i)
10    continue
      cs1d = sum
      return
      end

      double precision function cs2d(n,aa)
c
c --  calculate two-dimensional checksum
c
      integer i,j,n,ld
      parameter(ld=1000)
      double precision aa(ld,n), sum
      sum = 0.0d0
      do 10 j = 1,n
         do 20 i = 1,n
            sum = sum + aa(i,j)
20       continue
10    continue
      cs2d = sum
      return
      end

      real function tcall()
c
c --  time the overhead of a call to function second()
c
      integer i, ncalls
      real t1, t2, second, s
      parameter(ncalls = 100000)

      call cpu_time(t1)
      do 1 i = 1,ncalls
         call cpu_time(s)
  1   continue
      call cpu_time(t2);t2 = t2 - t1
      tcall = t2/float(ncalls)
      return
      end

      real function t471s()
c
c --  time the overhead of a call to subroutine s471s
c
      integer ncalls, i
      real t1, t2, second
      parameter(ncalls = 100000)
      call cpu_time(t1)
      do 1 i = 1,ncalls
         call s471s
  1   continue
      call cpu_time(t2);t2 = t2 - t1
      t471s = t2/float(ncalls)
      return
      end

      real function tdummy(ld,n,a,b,c,d,e,aa,bb,cc)
c
c --  time the overhead of a call to subroutine dummy
c
      integer ld, n, i, ncalls
      double precision a(n), b(n), c(n), d(n), e(n), aa(ld,n),
     +                 bb(ld,n), cc(ld,n)
      real t1, t2, second
      parameter(ncalls = 100000)
      call cpu_time(t1)
      do 1 i = 1,ncalls
         call dummy(ld,n,a,b,c,d,e,aa,bb,cc,1.d0)
  1   continue
      call cpu_time(t2);t2 = t2 - t1
      tdummy = t2/float(ncalls)
      return
      end

      subroutine dummy(ld,n,a,b,c,d,e,aa,bb,cc,s)
c
c --  called in each loop to make all computations appear required
c
      integer ld, n
      double precision a(n), b(n), c(n), d(n), e(n), aa(ld,n),
     +                 bb(ld,n), cc(ld,n)
      double precision s
      return
      end

      subroutine s471s
c
c --  dummy subroutine call made in s471
c
      return
      end


      integer function nindex(name)
c
c --  returns the (integer) loop index given the (character) name
c
      integer i, nloops
      parameter(nloops=135)
      character*5 name
      character*5 snames(nloops)
      common /ccom/snames

      do 10 i=1,nloops
        if ( name .eq. snames(i) ) then
           nindex = i
           return
        endif
10    continue
      print*,'ERROR COMPUTING ROW INDEX IN FUNCTION NINDEX()'
      nindex = -1
      return
      end

      subroutine init(ld,n,a,b,c,d,e,aa,bb,cc,name)
      double precision zero, small, half, one, two, any, array
      parameter(any=0.0d0,zero=0.0d0,half=.5d0,one=1.0d0,
     +          two=2.0d0,small=.000001d0)
      integer unit, frac, frac2, ld, n
      parameter(unit=1, frac=-1, frac2=-2)
      double precision a(n), b(n), c(n), d(n), e(n), aa(ld,n),
     +                 bb(ld,n), cc(ld,n)
      common /cdata/ array(1000*1000)
      character*5 name

      if     ( name .eq. 's111 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's112 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's113 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's114 ' ) then
         call set2d(n,aa, any,frac)
         call set2d(n,bb, any,frac2)
      elseif ( name .eq. 's115 ' ) then
         call set1d(n,  a, one,unit)
         call set2d(n, aa,small,unit)
      elseif ( name .eq. 's116 ' ) then
         call set1d(n,  a, one,unit)
      elseif ( name .eq. 's118 ' ) then
         call set1d(n,  a, one,unit)
         call set2d(n, bb,small,unit)
      elseif ( name .eq. 's119 ' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb, any,frac2)
      elseif ( name .eq. 's121 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's122 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's123 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's124 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's125 ' ) then
         call set1d(n*n,array,zero,unit)
         call set2d(n,aa, one,unit)
         call set2d(n,bb,half,unit)
         call set2d(n,cc, two,unit)
      elseif ( name .eq. 's126 ' ) then
         call set2d(n,  bb, one,unit)
         call set1d(n*n,array,any,frac)
         call set2d(n,  cc, any,frac)
      elseif ( name .eq. 's127 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's128 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, two,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, one,unit)
      elseif ( name .eq. 's131 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's132 ' ) then
         call set2d(n, aa, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's141 ' ) then
         call set1d(n*n,array, one,unit)
         call set2d(n,bb, any,frac2)
      elseif ( name .eq. 's151 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's152 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b,zero,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's161 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n/2,b(1), one,2)
         call set1d(n/2,b(2),-one,2)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's162 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's171 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's172 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's173 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's174 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's175 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's176 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's211 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's212 ' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's221 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's222 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
      elseif ( name .eq. 's231 ' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb, any,frac2)
      elseif ( name .eq. 's232 ' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb,zero,unit)
      elseif ( name .eq. 's233 ' ) then
         call set2d(n,aa, any,frac)
         call set2d(n,bb, any,frac)
         call set2d(n,cc, any,frac)
      elseif ( name .eq. 's234 ' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb, any,frac)
         call set2d(n,cc, any,frac)
      elseif ( name .eq. 's235 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         call set2d(n,aa, one,unit)
         call set2d(n,bb, any, frac2)
      elseif ( name .eq. 's241 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, one,unit)
      elseif ( name .eq. 's242 ' ) then
         call set1d(n,  a,small,unit)
         call set1d(n,  b,small,unit)
         call set1d(n,  c,small,unit)
         call set1d(n,  d,small,unit)
      elseif ( name .eq. 's243 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's244 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c,small,unit)
         call set1d(n,  d,small,unit)
      elseif ( name .eq. 's251 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's252 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
      elseif ( name .eq. 's253 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b,small,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's254 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
      elseif ( name .eq. 's255 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
      elseif ( name .eq. 's256 ' ) then
         call set1d(n, a, one,unit)
         call set2d(n,aa, two,unit)
         call set2d(n,bb, one,unit)
      elseif ( name .eq. 's257 ' ) then
         call set1d(n, a, one,unit)
         call set2d(n,aa, two,unit)
         call set2d(n,bb, one,unit)
      elseif ( name .eq. 's258 ' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b,zero,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e,zero,unit)
         call set2d(n, aa, any,frac)
      elseif ( name .eq. 's261 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
         call set1d(n,  c, any,frac2)
         call set1d(n,  d, one,unit)
      elseif ( name .eq. 's271 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's272 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, two,unit)
      elseif ( name .eq. 's273 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d,small,unit)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's274 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's275 ' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb,small,unit)
         call set2d(n,cc,small,unit)
      elseif ( name .eq. 's276 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's277 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n/2,b, one,unit)
         call set1d(n/2,b(n/2+1),-one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's278 ' ) then
         call set1d(n/2,a,-one,unit)
         call set1d(n/2,a(n/2+1),one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's279 ' ) then
         call set1d(n/2,a,-one,unit)
         call set1d(n/2,a(n/2+1),one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's2710' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's2711' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's2712' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's281 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
      elseif ( name .eq. 's291 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
      elseif ( name .eq. 's292 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
      elseif ( name .eq. 's293 ' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 's2101' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb, any,frac)
         call set2d(n,cc, any,frac)
      elseif ( name .eq. 's2102' ) then
         call set2d(n,aa,zero,unit)
      elseif ( name .eq. 's2111' ) then
         call set2d(n,aa,zero,unit)
      elseif ( name .eq. 's311 ' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 's312 ' ) then
         call set1d(n,a,1.000001d0,unit)
      elseif ( name .eq. 's313 ' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 's314 ' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 's315 ' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 's316 ' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 's317 ' ) then
         continue
      elseif ( name .eq. 's318 ' ) then
         call set1d(n,  a, any,frac)
         a(n) = -two
      elseif ( name .eq. 's319 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b,zero,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's3110' ) then
         call set2d(n,aa, any,frac)
         aa(n,n) = two
      elseif ( name .eq. 's3111' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 's3112' ) then
         call set1d(n,  a, any,frac2)
         call set1d(n,  b,zero,unit)
      elseif ( name .eq. 's3113' ) then
         call set1d(n,  a, any,frac)
         a(n) = -two
      elseif ( name .eq. 's321 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b,zero,unit)
      elseif ( name .eq. 's322 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b,zero,unit)
         call set1d(n,  c,zero,unit)
      elseif ( name .eq. 's323 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's331 ' ) then
         call set1d(n,  a, any,frac)
         a(n) = -one
      elseif ( name .eq. 's332 ' ) then
         call set1d(n,  a, any,frac2)
         a(n) = two
      elseif ( name .eq. 's341 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 's342 ' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 's343 ' ) then
         call set2d(n,aa, any,frac)
         call set2d(n,bb, one,unit)
      elseif ( name .eq. 's351 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         c(1) = 1.
      elseif ( name .eq. 's352 ' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 's353 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         c(1) = 1.
      elseif ( name .eq. 's411 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's412 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's413 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's414 ' ) then
         call set2d(n,aa, one,unit)
         call set2d(n,bb, any,frac)
         call set2d(n,cc, any,frac)
      elseif ( name .eq. 's415 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         a(n) = -one
      elseif ( name .eq. 's421 ' ) then
         call set1d(n,  a, any,frac2)
      elseif ( name .eq. 's422 ' ) then
         call set1d(n,array,one,unit)
         call set1d(n,  a, any,frac2)
      elseif ( name .eq. 's423 ' ) then
         call set1d(n,array,zero,unit)
         call set1d(n,  a, any,frac2)
      elseif ( name .eq. 's424 ' ) then
         call set1d(n,array,one,unit)
         call set1d(n,  a, any,frac2)
      elseif ( name .eq. 's431 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's432 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's441 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         call set1d(n/3,   d(1),        -one,unit)
         call set1d(n/3,   d(1+n/3),    zero,unit)
         call set1d(n/3+1, d(1+(2*n/3)), one,unit)
      elseif ( name .eq. 's442 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's443 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's451 ' ) then
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's452 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c,small,unit)
      elseif ( name .eq. 's453 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 's471 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, one,unit)
         call set1d(n,  d, any,frac)
         call set1d(n,  e, any,frac)
      elseif ( name .eq. 's481 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's482 ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 's491 ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's4112' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 's4113' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac2)
      elseif ( name .eq. 's4114' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's4115' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 's4116' ) then
         call set1d(n, a, any,frac)
         call set2d(n,aa, any,frac)
      elseif ( name .eq. 's4117' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, one,unit)
         call set1d(n,  c, any,frac)
         call set1d(n,  d, any,frac)
      elseif ( name .eq. 's4121' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 'va   ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 'vag  ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 'vas  ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 'vif  ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 'vpv  ' ) then
         call set1d(n,  a,zero,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 'vtv  ' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, one,unit)
      elseif ( name .eq. 'vpvtv' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, any,frac)
      elseif ( name .eq. 'vpvts' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, any,frac2)
      elseif ( name .eq. 'vpvpv' ) then
         call set1d(n,  a, any,frac2)
         call set1d(n,  b, one,unit)
         call set1d(n,  c,-one,unit)
      elseif ( name .eq. 'vtvtv' ) then
         call set1d(n,  a, one,unit)
         call set1d(n,  b, two,unit)
         call set1d(n,  c,half,unit)
      elseif ( name .eq. 'vsumr' ) then
         call set1d(n,  a, any,frac)
      elseif ( name .eq. 'vdotr' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, any,frac)
      elseif ( name .eq. 'vbor ' ) then
         call set1d(n,  a, any,frac)
         call set1d(n,  b, any,frac)
         call set1d(n,  c, one,frac)
         call set1d(n,  d, two,frac)
         call set1d(n,  e,half,frac)
         call set2d(n, aa, any,frac)
      else
        print*,'COULDN''T FIND ',name,' TO INITIALIZE'
      endif

      return
      end


      subroutine info(ctime,dtime,c471s)
      real ctime, dtime, c471s
c
c --  Please fill in the information below as best you can.  Additional
c --  information you feel useful may be entered in the comments section.
c --  Thanks to the SLALOM benchmark for the idea for this subroutine.
c
      character*72 who(7), run(3), cmpter(15), coment(6)
      data who   /
     &' Run by:                  Mr./Ms. Me',
     &' Address:                 My_Company',
     &' Address:                 My_Address',
     &' Address:                 My_City, My_State, My_Zipcode',
     &' Phone:                   (123)-456-7890',
     &' FAX:                     (123)-456-7890',
     &' Electronic mail:         me@company.com'/    
      data run    /
     &' Scalar/Vector run:       Scalar',
     &' Timer:                   User CPU, etime()',
     &' Standalone:              Yes'/
      data cmpter /
     &' Computer:                Fast_Computer 1',
     &' Compiler/version:        f77 3.1',
     &' Compiler options:        -O',
     &' Availability date:       Now',
     &' OS/version:              Un*x, 1.0',
     &' Cache size:              none',
     &' Main memory size:        128MB',
     &' No. vec. registers:      8',
     &' Vec. register length:    128',
     &' No. functional units:    2 add, 2 multiply',
     &' Chaining supported:      no',
     &' Overlapping supported:   independent add and mutiply units',
     &' Memory paths:            2 load, 1 store',
     &' Memory path width        4 64-bit words per clock, per pipe',
     &' Clock speed:             4ns.'/
c
c -- Enter any comments you think may be important.
c -- Feel free to increase the number of comment lines 
c
      data coment /
     &' Comments:',
     &' Comments:',
     &' Comments:',
     &' Comments:',
     &' Comments:',
     &' Comments:'/

      write (*, *) ' '
      write (*, '(a72)') who
      write (*, '(a72)') run
      write (*, '(a72)') cmpter
      write (*, 99) 'Cost of timing call:', ctime
      write (*, 99) 'Cost of dummy  call:', dtime
      write (*, 99) 'Cost of c471s  call:', c471s
      write (*, '(a72)') coment
99    format(1x,a20,5x,f12.10)
      return
      end

