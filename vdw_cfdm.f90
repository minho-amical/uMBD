!!VDW_CFDM module
MODULE VDW_CFDM
USE CFDM_Setup
USE SCS_ShortRange
USE CFDM_LongRange
USE Mympi
IMPLICIT NONE

CONTAINS
 SUBROUTINE OnCFDM(this_sys)
  TYPE (SYS_INP), INTENT(INOUT) :: this_sys

  TYPE (INFO_SPRCELL) :: ex_cell
  INTEGER :: ierr, i
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: mat_cfdm_

  !Solve SCS equation for short-range screening
  CALL allocate_task(3*this_sys%natom)
  CALL Solve_SCS_sr(this_sys) ! In SCS_ShortRange module
  CALL deallocate_task

  !Make supercell, by using sprcell_rcut
  IF (myid == 0) THEN
   WRITE(*, *) '###LONG-RANGE DISPERSION ENERGY CALCULATION###'
   WRITE(*, *) ''
  ENDIF
  CALL MakeSprcell(this_sys, ex_cell) ! In this module

  !Construct CFDM Hamiltonian matrix
  ALLOCATE(mat_cfdm_(1:3*ex_cell%natom,1:3*ex_cell%natom), STAT=ierr)
  CALL allocate_task(3*ex_cell%natom)
  CALL Make_CFDM_Mat_lr(this_sys, ex_cell, mat_cfdm_) ! In CFDM_LongRange module
  CALL deallocate_task

  !Solve CFDM Hamilitonian for long-range screening
  IF (myid == 0) THEN
   CALL GetDispersionE(mat_cfdm_, ex_cell)
   WRITE(*, *) '###LONG-RANGE DISPERSION ENERGY CALCULATION END###'
  ENDIF

  !Deallocate array values in ex_cell, and mat_cfdm_
  CALL RemoveSprcell(ex_cell)
  IF (ALLOCATED(mat_cfdm_)) DEALLOCATE(mat_cfdm_, STAT=ierr)
  RETURN
 END SUBROUTINE OnCFDM

 SUBROUTINE MakeSprcell(this_sys, ex_cell)
  TYPE (SYS_INP), INTENT(IN) :: this_sys
  TYPE (INFO_SPRCELL), INTENT(OUT) :: ex_cell

  INTEGER :: x_hi, y_hi, z_hi
  INTEGER :: ia, ib, ic, i, ierr, cnt
  REAL*8, DIMENSION(1:3) :: latvec_
  REAL*8 :: a1, a2, a3
  REAL*8, PARAMETER :: tol = 1.0D-5

  !Determine supercell size
  IF (ABS(this_sys%sprcell_rcut) < tol) THEN
   x_hi = 1; y_hi = 1; z_hi = 1
  ELSE
   IF (this_sys%vac_axis(1) == .TRUE.) THEN
    x_hi = 1
   ELSE
    latvec_ = this_sys%lat_(1,1:3)
    a1 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    x_hi = CEILING(this_sys%sprcell_rcut/a1)
   ENDIF

   IF (this_sys%vac_axis(2) == .TRUE.) THEN
    y_hi = 1
   ELSE
    latvec_ = this_sys%lat_(2,1:3)
    a2 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    y_hi = CEILING(this_sys%sprcell_rcut/a2)
   ENDIF

   IF (this_sys%vac_axis(3) == .TRUE.) THEN
    z_hi = 1
   ELSE
    latvec_ = this_sys%lat_(3,1:3)
    a3 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    z_hi = CEILING(this_sys%sprcell_rcut/a3)
   ENDIF
  ENDIF
  ex_cell%fac_dupli = x_hi*y_hi*z_hi
  ! Exclude the atoms with negative screened polarizability
  IF (myid == 0) THEN
   IF (this_sys%nnega /= 0) THEN
    WRITE(*, '(5X, A, I4, A)') 'REMARK!!! ', this_sys%nnega, ' atoms have negative screened polarizability' 
    WRITE(*, '(5X, A)') 'Exclude them in the CFSDM matrix'
    WRITE(*, *) ''
   ENDIF
  ENDIF
  ex_cell%natom = (this_sys%natom-this_sys%nnega) * ex_cell%fac_dupli

  IF (myid == 0) THEN
   WRITE(*, '(3(X,A,X,I4), A)') '', x_hi, '*', y_hi, '*', z_hi, ' supercell has constructed'
   WRITE(*, *) ''
  ENDIF

  !Make supercell structure
  !Lattice vectors
  ex_cell%lat_(1,1:3) = this_sys%lat_(1,1:3)*x_hi
  ex_cell%lat_(2,1:3) = this_sys%lat_(2,1:3)*y_hi
  ex_cell%lat_(3,1:3) = this_sys%lat_(3,1:3)*z_hi

  !Atomic coordinates
  IF (.NOT.ALLOCATED(ex_cell%r_)) ALLOCATE(ex_cell%r_(1:ex_cell%natom,1:3), STAT=ierr)
  IF (.NOT.ALLOCATED(ex_cell%spec_)) ALLOCATE(ex_cell%spec_(1:ex_cell%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(ex_cell%alp0_SCS_)) ALLOCATE(ex_cell%alp0_SCS_(1:ex_cell%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(ex_cell%omega_SCS_)) ALLOCATE(ex_cell%omega_SCS_(1:ex_cell%natom), STAT=ierr)

  cnt = 1
  DO ia = 1, x_hi
   DO ib = 1, y_hi
    DO ic = 1, z_hi
     DO i = 1, this_sys%natom
      IF (this_sys%alp0_SCS_(i) >= 0) THEN
       ex_cell%r_(cnt,1:3) = this_sys%r_(i,1:3) + (ia-1)*this_sys%lat_(1,1:3) + &
                            (ib-1)*this_sys%lat_(2,1:3) + (ic-1)*this_sys%lat_(3,1:3)
       ex_cell%spec_(cnt) = this_sys%spec_(i)
       ex_cell%alp0_SCS_(cnt) = this_sys%alp0_SCS_(i)
       ex_cell%omega_SCS_(cnt) = this_sys%omega_SCS_(i)
       cnt = cnt + 1
      ENDIF
     END DO
    END DO
   END DO
  END DO

  RETURN
 END SUBROUTINE MakeSprCell

 SUBROUTINE RemoveSprcell(ex_cell)
  TYPE (INFO_SPRCELL), INTENT(INOUT) :: ex_cell

  INTEGER :: ierr

  IF (ALLOCATED(ex_cell%r_)) DEALLOCATE(ex_cell%r_, STAT=ierr)
  IF (ALLOCATED(ex_cell%spec_)) DEALLOCATE(ex_cell%spec_, STAT=ierr)
  IF (ALLOCATED(ex_cell%alp0_SCS_)) DEALLOCATE(ex_cell%alp0_SCS_, STAT=ierr)
  IF (ALLOCATED(ex_cell%omega_SCS_)) DEALLOCATE(ex_cell%omega_SCS_, STAT=ierr)

  RETURN
 END SUBROUTINE RemoveSprcell
END MODULE VDW_CFDM
