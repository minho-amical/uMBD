!! CFDM dispersion energy calculation code, ver 5.0
!! Made by WJ Kim.
!! This code gets the structure of system and the vdW-related coefficient
!! calculated from VASP, and calculates the many-body dispersion energy by
!! solving CFDM Hamiltonian.
!! Also, the short-range screening, which is not involved in TS of TS/HI
!! dispersion coefficients, is considered by solving self-consistent screening
!! equation (SCS equation) before enter the CFDM diagonalization.

!! You need two input files.
!! 1) input file to control your current job - See "" file.
!! 2) modified VASP POSCAR file : you should give the TS of TS/HI relative
!!    volume after the three coordinates of each atom
!! In current version, the formats of input files are not flexible, so you
!! should follow the format (especially ordering of quantities) in example
!! directory.

!!Main program
PROGRAM CFSDM_Main
!USED MODULES
USE MyParams      !1. Unit conversion factor, free-atom coeff.s
USE CFDM_Setup    !2. User defined type, I/O
USE VDW_CFDM      !3. Main subroutine for CFDM dispersion energy calculation
USE Mympi
IMPLICIT NONE
 INTEGER :: narg, i
 CHARACTER(LEN=1000) :: ifname1, ifname2
 LOGICAL :: file_exists

 TYPE (SYS_INP) :: mysys ! in CFDM_Setup

 CALL start_mpi()

 IF (myid == 0) THEN 
  !Check there are all input files, and read them
  WRITE(*, '(A)') '--------------------CFSDM code version.5.0--------------------'
  WRITE(*, '(A)') '-----------------------Made by W.J. Kim-----------------------'
 ENDIF

 narg = COMMAND_ARGUMENT_COUNT()
 IF (narg /= 2) THEN
  WRITE(*, '(A)') 'ERROR: too many or too small input files'
  CALL stop_mpi()
  STOP
 ENDIF

 CALL GET_COMMAND_ARGUMENT(1, ifname1) ! job-control parameters
 CALL GET_COMMAND_ARGUMENT(2, ifname2) ! system information
 INQUIRE(FILE=ifname1, EXIST=file_exists)
 IF (.NOT.(file_exists)) THEN
  WRITE(*, '(A,A)') 'ERROR: no such file - ', TRIM(ifname1)
  CALL stop_mpi()
  STOP
 ENDIF
 INQUIRE(FILE=ifname2, EXIST=file_exists)
 IF (.NOT.(file_exists)) THEN
  WRITE(*, '(A,A)') 'ERROR: no such file - ', TRIM(ifname2)
  CALL stop_mpi()
  STOP
 ENDIF

 CALL ReadInputs(ifname1, ifname2, mysys)
 
 !Convert structure information into fully Cartesian unit
 CALL ConvertToFullCart(mysys)

 !Summarize the information about system and job-control parameters
 IF (myid == 0) THEN
  WRITE(*, *) '###LATTICE VECTORS (Ang)###'
  DO i = 1, 3
   WRITE(*, '(3F15.9)') mysys%lat_(i,1:3)*bohr_to_ang
  END DO
  WRITE(*, *) ''
  WRITE(*, *) '###INPUT GEOMETRY AND TS(/HI) RELATIVE ATOMIC VOLUME###'
  WRITE(*, '(A10, 3A15, A9)') 'ELEMENT', 'X (Ang)', 'Y (Ang)', 'Z (Ang)', 'RELVOL'
  DO i = 1, mysys%natom
   WRITE(*, '(A10, 3F15.9, F9.3)') mysys%spec_(i), mysys%r_(i,1:3)*bohr_to_ang, mysys%relvol_(i)
  END DO
  WRITE(*, *) ''

  WRITE(*, *) '###JOB-CONTROL PARAMETERS###'
  WRITE(*, '(A30, I10)') 'POTENTIAL TYPE : ', mysys%type_pot
  WRITE(*, '(A30, I10)') 'DAMPING TYPE : ', mysys%type_damp
  WRITE(*, '(A30, F10.3)') 'PARAMETER A : ', mysys%param_a
  WRITE(*, '(A30, F10.3)') 'PARAMETER B : ', mysys%param_b
  WRITE(*, '(A30, F10.3)') 'PARAMETER D : ', mysys%param_d
  WRITE(*, '(A30, F10.3)') 'SUPERCELL CUTOFF RADIUS : ', mysys%sprcell_rcut*bohr_to_ang
  WRITE(*, '(A30, F10.3)') 'SCS CUTOFF RADIUS : ', mysys%scs_rcut*bohr_to_ang
  WRITE(*, '(A30, F10.3)') 'CFDM CUTOFF RADIUS : ', mysys%cfdm_rcut*bohr_to_ang
  WRITE(*, '(A30, X, 3(X,L))') 'VACUUM AXIS : ', mysys%vac_axis
  WRITE(*, '(A30, X, L)') 'FI METHOD : ', mysys%fi_on
  WRITE(*, *) ''
  WRITE(*, *) '###INPUT SUMMARY END###'
  WRITE(*, *) ''
 ENDIF

 !Go into CFDM calculation
 CALL OnCFDM(mysys)

 !Deallocate all dynamic arrays in mysys
 CALL RemoveInputs(mysys)

 IF (myid == 0) THEN
  WRITE(*, '(A)') '--------------------------End of Run--------------------------'
 ENDIF
 CALL stop_mpi()
 STOP

END PROGRAM CFSDM_Main
