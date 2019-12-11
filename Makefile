##########EDIT PROPERLY!!!##########
FC=mpif90 -f90=ifort

MKL_ROOT=$(MKLROOT)/lib/intel64

LAPACKBLAS = -L$(MKL_ROOT) -Wl,--start-group -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -Wl,--end-group -lpthread

FFLAGS =  -O3 -ip -check bounds -check uninit -check pointers -traceback -g -fpe0


##########DO NOT TOUCH THEM!!!##########
SOURCE = mympi.f90 myparams.f90 cfdm_setup.f90 dipolepots.f90 scs_short.f90 cfdm_longrange.f90 vdw_cfdm.f90 main.f90

install :
	$(FC) $(FFLAGS) -o CFSDM.x $(SOURCE) $(LAPACKBLAS)

clean :
	rm -f *.o *.mod *.x
