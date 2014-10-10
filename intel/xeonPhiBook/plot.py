import matplotlib.pyplot as plt



loopcount=[8,16,24,32,40,48,56,64,72,128]
dflops=[424.115,1054.468, 942.408, 1055.332, 1055.169, 1056.172, 1054.762, 1056.063, 950.113,  238.893]




plt.plot(loopcount,dflops,'or--')
plt.rc(('xtick','ytick','axes'), labelsize=16.0)
plt.xlabel('number of doubles per array per thread using 122 threads total')
plt.ylabel('GFLOPS')
plt.title('Near limits of KNC achieved for certain vector sizes')

plt.show()


#code:
# allocate( fa(FLOPS_ARRAY_SIZE) )
#  allocate( fb(FLOPS_ARRAY_SIZE) )
#
#  ! initialize the compute arrays 
#
#  !$OMP PARALLEL
#  !$OMP MASTER
#  numthreads = omp_get_num_threads()
#  !$OMP end MASTER
#  !$OMP end PARALLEL
#
#  write(*,*) "Initializing"
#
#  !$OMP PARALLEL DO
#  do i = 1, FLOPS_ARRAY_SIZE
#     fa(i) = i + 0.1_dp;
#     fb(i) = i + 0.2_dp;
#  end do
#
#  write(*,*) 'Starting Compute on ', numthreads, ' threads'
#  tstart = mytime()
#        
#  ! scale the calculation across threads requested 
#  ! need to set environment variables OMP_NUM_THREADS and KMP_AFFINITY
#  !$OMP PARALLEL do PRIVATE(j,k,offset)
#  do i=1, numthreads
#      ! each thread will work it's own array section
#      ! calc offset into the right section
#      offset = i*LOOP_COUNT
#
#      ! loop many times to get lots of calculations
#      do j=1, MAXFLOPS_ITERS
#          ! scale 1st array and add in the 2nd array
#          !dir$ vector aligned 
#          do k=1, LOOP_COUNT
#              fa(k+offset) = a * fa(k+offset) + fb(k+offset)
#          end do
#      end do
#  end do
#
#  tstop = mytime()
#
#  ! # of gigaflops we just calculated 
#  gflops = 1.0e-9 * numthreads * LOOP_COUNT * MAXFLOPS_ITERS * FLOPSPERCALC

#FLOPSPERCACL=3, MAXFLOPS_ITERS=10E5

