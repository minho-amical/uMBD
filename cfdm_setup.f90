!!CFDM_Setup module
MODULE CFDM_Setup
USE MyParams
USE Mympi
IMPLICIT NONE
 TYPE SYS_INP
  ! Read from first input - job control parameters
  INTEGER :: type_pot ! potential
  INTEGER :: type_damp ! damping
  REAL*8 :: param_a ! parameter on the exponent of potential
  REAL*8 :: param_b ! beta in damping
  REAL*8 :: param_d ! scaling parameter in damping
  REAL*8 :: sprcell_rcut ! cutoff for supercell construction
  REAL*8 :: scs_rcut ! cutoff for scs equation
  REAL*8 :: cfdm_rcut ! cutoff for cfdm Hamiltonian
  LOGICAL, DIMENSION(1:3) :: vac_axis ! vacuum axis
  LOGICAL :: fi_on ! turn on FI method?

  ! Read from second input - system information
  REAL*8 :: scale_coord ! scaling (2nd line)
  REAL*8, DIMENSION(1:3,1:3) :: lat_ ! lattice vectors
  INTEGER :: nspec ! number of species
  CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: elem_ ! species
  INTEGER, DIMENSION(:), ALLOCATABLE :: nelem_ ! No. of atoms for each species
  INTEGER :: natom ! tot. No. of atoms
  CHARACTER(LEN=100) :: format_coord ! format of coordinates
  CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: spec_ ! spec. of ith atom
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: r_ ! coord. of ith atom
  REAL*8, DIMENSION(:), ALLOCATABLE :: relvol_ ! rel. vol. of ith atom
  REAL*8, DIMENSION(:), ALLOCATABLE :: q_hi_

  REAL*8, DIMENSION(:), ALLOCATABLE :: alp0_SCS_
  REAL*8, DIMENSION(:), ALLOCATABLE :: omega_SCS_
  INTEGER :: nnega ! number of atoms with negative SCS polarizability
 END TYPE SYS_INP

 TYPE INFO_SPRCELL       
  INTEGER :: natom                                           
  REAL*8, DIMENSION(1:3,1:3) :: lat_                         
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: r_                  
  CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: spec_
  REAL*8, DIMENSION(:), ALLOCATABLE :: alp0_SCS_
  REAL*8, DIMENSION(:), ALLOCATABLE :: omega_SCS_
  INTEGER :: fac_dupli                                       
 END TYPE INFO_SPRCELL

CONTAINS
 SUBROUTINE ReadInputs(fname1, fname2, this_sys)
  CHARACTER(LEN=1000), INTENT(IN) :: fname1, fname2
  TYPE (SYS_INP), INTENT(OUT) :: this_sys

  CHARACTER(LEN=100) :: buff
  INTEGER :: i, j, ierr
  CHARACTER(LEN=100) :: specline
  INTEGER, DIMENSION(:), ALLOCATABLE :: specreg_

  OPEN(11, FILE=fname1, STATUS='OLD', ACCESS='SEQUENTIAL', ACTION='READ')
  READ(11, *) buff, this_sys%type_pot
  READ(11, *) buff, this_sys%type_damp
  READ(11, *) buff, this_sys%param_a
  READ(11, *) buff, this_sys%param_b
  READ(11, *) buff, this_sys%param_d
  READ(11, *) buff, this_sys%sprcell_rcut
  READ(11, *) buff, this_sys%scs_rcut
  READ(11, *) buff, this_sys%cfdm_rcut
  READ(11, *) buff, this_sys%vac_axis
  READ(11, *) buff, this_sys%fi_on
  CLOSE(11)
  IF (myid == 0) THEN
   WRITE(*, *) 'Reading file ', TRIM(fname1), ' : OK!'
  ENDIF

  OPEN(12, FILE=fname2, STATUS='OLD', ACCESS='SEQUENTIAL', ACTION='READ')
  READ(12, '(A)') buff
  READ(12, *) this_sys%scale_coord
  DO i = 1, 3
   READ(12, *) this_sys%lat_(i,1), this_sys%lat_(i,2), this_sys%lat_(i,3)
  END DO
  READ(12, '(A)') specline
  this_sys%nspec = Nitem(specline)
  IF (.NOT.ALLOCATED(this_sys%elem_)) ALLOCATE(this_sys%elem_(1:this_sys%nspec), STAT=ierr)
  READ(specline, *) this_sys%elem_
  IF (.NOT.ALLOCATED(this_sys%nelem_)) ALLOCATE(this_sys%nelem_(1:this_sys%nspec), STAT=ierr)
  READ(12, *) this_sys%nelem_
  this_sys%natom = SUM(this_sys%nelem_)
  READ(12, '(A)') buff
  READ(12, '(A)') this_sys%format_coord
  IF (.NOT.ALLOCATED(this_sys%spec_)) ALLOCATE(this_sys%spec_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(this_sys%r_)) ALLOCATE(this_sys%r_(1:this_sys%natom,1:3), STAT=ierr)
  IF (.NOT.ALLOCATED(this_sys%relvol_)) ALLOCATE(this_sys%relvol_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(this_sys%q_hi_)) ALLOCATE(this_sys%q_hi_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(specreg_)) ALLOCATE(specreg_(1:this_sys%nspec), STAT=ierr)
  DO i = 1, this_sys%nspec
   specreg_(i) = SUM(this_sys%nelem_(1:i))
  END DO

  DO i = 1, this_sys%natom
   IF (this_sys%fi_on) THEN
    READ(12, *) this_sys%r_(i,1), this_sys%r_(i,2), this_sys%r_(i,3), this_sys%relvol_(i), this_sys%q_hi_(i)
   ELSE
    READ(12, *) this_sys%r_(i,1), this_sys%r_(i,2), this_sys%r_(i,3), this_sys%relvol_(i)
   ENDIF
   DO j = 1, this_sys%nspec
    IF (i <= specreg_(j)) THEN
     this_sys%spec_(i) = this_sys%elem_(j)
     EXIT
    ENDIF
   END DO
  END DO
  CLOSE(12)
  IF (myid == 0) THEN
   WRITE(*, *) 'Reading file ', TRIM(fname2), ' : OK!'
  ENDIF

  this_sys%nnega = 0

  IF (ALLOCATED(specreg_)) DEALLOCATE(specreg_, STAT=ierr)
  RETURN
 END SUBROUTINE ReadInputs

 SUBROUTINE RemoveInputs(this_sys)
  TYPE (SYS_INP), INTENT(INOUT) :: this_sys

  INTEGER :: ierr

  IF (ALLOCATED(this_sys%elem_)) DEALLOCATE(this_sys%elem_, STAT=ierr)
  IF (ALLOCATED(this_sys%nelem_)) DEALLOCATE(this_sys%nelem_, STAT=ierr)
  IF (ALLOCATED(this_sys%spec_)) DEALLOCATE(this_sys%spec_, STAT=ierr)
  IF (ALLOCATED(this_sys%r_)) DEALLOCATE(this_sys%r_, STAT=ierr)
  IF (ALLOCATED(this_sys%relvol_)) DEALLOCATE(this_sys%relvol_, STAT=ierr)
  IF (ALLOCATED(this_sys%q_hi_)) DEALLOCATE(this_sys%q_hi_, STAT=ierr)
  RETURN
 END SUBROUTINE RemoveInputs

 SUBROUTINE ConvertToFullCart(this_sys)
  TYPE (SYS_INP), INTENT(INOUT) :: this_sys

  REAL*8, DIMENSION(:,:), ALLOCATABLE :: rtmp_
  INTEGER :: i, ierr

  !If r_ is in direct format, convet to (scaled) Cartesian format
  IF ((this_sys%format_coord(1:1) == 'D') .OR. (this_sys%format_coord(1:1) == 'd')) THEN
   IF(.NOT.ALLOCATED(rtmp_)) ALLOCATE(rtmp_(1:this_sys%natom,1:3), STAT=ierr)
   rtmp_ = MATMUL(this_sys%r_, this_sys%lat_)
   this_sys%r_ = rtmp_
  ENDIF

  !Final scaling, according to the global scaling factor
  this_sys%lat_ = this_sys%lat_ * this_sys%scale_coord * ang_to_bohr
  this_sys%r_ = this_sys%r_ * this_sys%scale_coord * ang_to_bohr
  this_sys%sprcell_rcut = this_sys%sprcell_rcut * ang_to_bohr
  this_sys%scs_rcut = this_sys%scs_rcut * ang_to_bohr
  this_sys%cfdm_rcut = this_sys%cfdm_rcut * ang_to_bohr

  IF (ALLOCATED(rtmp_)) DEALLOCATE(rtmp_, STAT=ierr)
  RETURN
 END SUBROUTINE ConvertToFullCart

 INTEGER FUNCTION Nitem(str)
  CHARACTER(LEN=*), INTENT(IN) :: str

  INTEGER :: lw
  LOGICAL, DIMENSION(:), ALLOCATABLE :: isblank
  INTEGER :: i, ierr

  lw = LEN(str)
  IF (.NOT.ALLOCATED(isblank)) ALLOCATE(isblank(1:lw), STAT=ierr)
  IF (ierr /= 0) THEN
   WRITE(*, '(A)') 'ERROR: allocation in Nitem'
   CALL stop_mpi()
   STOP
  ENDIF

  DO i = 1, lw
   IF (str(i:i) == ' ') THEN
    isblank(i) = .TRUE.
   ELSE
    isblank(i) = .FALSE.
   ENDIF
  END DO

  IF (isblank(1) == .TRUE.) THEN
   Nitem = 0
  ELSE
   Nitem = 1
  ENDIF

  DO i = 2, lw
   IF ((isblank(i-1) == .TRUE.) .AND. (isblank(i) == .FALSE.)) THEN
    Nitem = Nitem + 1
   ENDIF
  END DO

  IF (ALLOCATED(isblank)) DEALLOCATE(isblank, STAT=ierr)
 END FUNCTION Nitem
END MODULE CFDM_Setup
