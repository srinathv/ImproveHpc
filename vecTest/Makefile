FC?=gfortran

ifeq ($(FC),pgfortran)
COMPILER = pgfortran
OPT = -O4
SCALAR_FLAGS = -c ${OPT} -Minfo=all
VECTOR_FLAGS = -c ${OPT} -Mvect -Minfo=all
endif


ifeq ($(FC),gfortran)
COMPILER = gfortran
OPT = -O3
SCALAR_FLAGS = -c ${OPT} -ftree-vectorizer-verbose=6
VECTOR_FLAGS = -c ${OPT} -finline-functions -ftree-vectorize -ftree-vectorizer-verbose=6
endif

ifeq ($(FC),gfortran-4.9)
COMPILER = gfortran-4.9
OPT = -O3
SCALAR_FLAGS = -c ${OPT} -ftree-vectorizer-verbose=6
VECTOR_FLAGS = -c ${OPT} -finline-functions -ftree-vectorize -ftree-vectorizer-verbose=6
endif

ifeq ($(FC),ifort)
COMPILER = ifort
OPT = -O3
SCALAR_FLAGS = -c ${OPT} -vec-report3
VECTOR_FLAGS = -c ${OPT} -vec-report3
###VECTOR_FLAGS = -c ${OPT} -vec -finline-functions -ip -vec -vec-report3
endif

run: info runs runv

runs: scalard_${COMPILER}
	./$< &> ${<}.log

runv: vectord_${COMPILER}
	./$< &> ${<}.log

scalard_${COMPILER}: maind_scalar_${COMPILER}.o loopd_scalar_${COMPILER}.o
	${COMPILER} ${OPT} -o $@ $^

maind_scalar_${COMPILER}.o: maind.f
	${COMPILER} -c -O0 -o $@ $<

loopd_scalar_${COMPILER}.o: loopd.f
	${COMPILER} ${SCALAR_FLAGS} -o $@ $< &> $@.vec

vectord_${COMPILER}: maind_vector_${COMPILER}.o loopd_vector_${COMPILER}.o
	${COMPILER} ${OPT} -o $@ $^

maind_vector_${COMPILER}.o: maind.f
	${COMPILER} -c -O0 -o $@ $<

loopd_vector_${COMPILER}.o: loopd.f
	${COMPILER} ${VECTOR_FLAGS} -o $@ $< &> $@.vec

info:
	@${COMPILER} --version | head -n 2
	@echo ""
	@uname -a

clean:
	rm -f *_${COMPILER}.o *_${COMPILER}.o.vec *_${COMPILER}.log scalard_${COMPILER} vectord_${COMPILER}
