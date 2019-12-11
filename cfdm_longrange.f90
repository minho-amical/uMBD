!!CFDM_LongRange module
MODULE CFDM_LongRange
USE MyParams
USE CFDM_setup
USE DipolePots
USE SCS_ShortRange
USE Mympi
IMPLICIT NONE

CONTAINS
 SUBROUTINE Make_CFDM_Mat_lr(this_sys, ex_cell, mat_cfdm_)
  TYPE (SYS_INP), INTENT(IN) :: this_sys
  TYPE (INFO_SPRCELL), INTENT(INOUT) :: ex_cell
  REAL*8, DIMENSION(:,:), INTENT(INOUT) :: mat_cfdm_

  INTEGER :: x_hi, y_hi, z_hi
  REAL*8, DIMENSION(1:3) :: latvec_
  REAL*8 :: a1, a2, a3
  REAL*8, PARAMETER :: tol = 1.0D-5

  INTEGER :: is_periodic, i, j, ia, ib, ic
  INTEGER :: at_a, at_b, at_a0, at_b0, dir_a, dir_b
  REAL*8, DIMENSION(1:3) :: R_ab_, U_ab_
  REAL*8 :: R, U_sca, lscale
  REAL*8 :: R0_SCS_a, R0_SCS_b, R0_ab
  REAL*8 :: C6_free, alp0_free, R0_free
  REAL*8 :: T_lr_aibj

  !Apply PBE using cfdm_rcut  
  IF (ABS(this_sys%cfdm_rcut) < tol) THEN
   x_hi = 0; y_hi = 0; z_hi = 0
  ELSE
   IF (this_sys%vac_axis(1) == .TRUE.) THEN
    x_hi = 0
   ELSE
    latvec_ = ex_cell%lat_(1,1:3)
    a1 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    x_hi = CEILING(this_sys%cfdm_rcut/a1)
   ENDIF

   IF (this_sys%vac_axis(2) == .TRUE.) THEN
    y_hi = 0
   ELSE
    latvec_ = ex_cell%lat_(2,1:3)
    a2 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    y_hi = CEILING(this_sys%cfdm_rcut/a2)
   ENDIF

   IF (this_sys%vac_axis(3) == .TRUE.) THEN
    z_hi = 0
   ELSE
    latvec_ = ex_cell%lat_(3,1:3)
    a3 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    z_hi = CEILING(this_sys%cfdm_rcut/a3)
   ENDIF
  ENDIF

  IF (myid == 0) THEN
   WRITE(*, '(3(X,A,X,I4),A)') '', 2*x_hi+1, '*', 2*y_hi+1, '*', 2*z_hi+1, ' auxiliary supercell has considered for CFDM calculation'
   WRITE(*, *) ''
  ENDIF

  mat_cfdm_ = 0.0D0

  !Add the square of characteristic frequency to the diagonal part of cfdm matrix.
  DO i = 1, 3*ex_cell%natom
   IF (myid == task_list(i)) THEN
    at_a = (i+2)/3
    mat_cfdm_(i,i) = ex_cell%omega_SCS_(at_a)**2.0
   ENDIF
  END DO

  !Calculate long-range dipole-dipole tensor T_lr --> add to cfdm matrix.
  !First, interactions in single supercell
  DO i = 1, 3*ex_cell%natom
   IF (myid == task_list(i)) THEN
    DO j = 1, 3*ex_Cell%natom
     at_a = (i+2)/3
     at_b = (j+2)/3
     dir_a = MOD(i-1,3)+1
     dir_b = MOD(j-1,3)+1

     IF (at_a /= at_b) THEN
       R_ab_ = ex_cell%r_(at_b,1:3) - ex_cell%r_(at_a,1:3)
      R = SQRT(DOT_PRODUCT(R_ab_, R_ab_))
      lscale = 1.0/SQRT((ex_cell%alp0_SCS_(at_a)*ex_cell%alp0_SCS_(at_a))**(1.0/3.0) + (ex_cell%alp0_SCS_(at_b)*ex_cell%alp0_SCS_(at_b))**(1.0/3.0))
      U_ab_ = R_ab_ * lscale
      U_sca = R * lscale

      T_lr_aibj = ex_cell%omega_SCS_(at_a)*ex_cell%omega_SCS_(at_b)*SQRT(ex_cell%alp0_SCS_(at_a)*ex_cell%alp0_SCS_(at_b))
      T_lr_aibj = T_lr_aibj * (lscale**3.0) * Pot_Lr(U_ab_, U_sca, this_sys%param_a, dir_a, dir_b, this_sys%type_pot)

      CALL GetvdWParam(ex_cell%spec_(at_a), C6_free, alp0_free, R0_free)
      R0_SCS_a = ((ex_cell%alp0_SCS_(at_a)/alp0_free)**(1.0/3.0))*R0_free
      CALL GetvdWParam(ex_cell%spec_(at_b), C6_free, alp0_free, R0_free)
      R0_SCS_b = ((ex_cell%alp0_SCS_(at_b)/alp0_free)**(1.0/3.0))*R0_free
      R0_ab = R0_SCS_a + R0_SCS_b

      T_lr_aibj = T_lr_aibj * Damping_Lr(R, this_sys%param_d, this_sys%param_b, R0_ab, this_sys%type_damp)

      mat_cfdm_(i,j) = mat_cfdm_(i,j) + T_lr_aibj
     ENDIF
    END DO
   ENDIF
  END DO

  !Second, if the system is periodic, do the loop for auxiliaty supercell images.
  is_periodic = x_hi + y_hi + z_hi
  IF (is_periodic > 0) THEN
   DO ia = -x_hi, x_hi
    DO ib = -y_hi, y_hi
     DO ic = -z_hi, z_hi
      IF (ia /= 0 .OR. ib /= 0 .OR. ic /= 0) THEN
       DO i = 1, 3*ex_cell%natom
        IF (myid == task_list(i)) THEN
         DO j = 1, 3*ex_cell%natom
          at_a = (i+2)/3
          at_b = (j+2)/3
          dir_a = MOD(i-1,3)+1
          dir_b = MOD(j-1,3)+1

          R_ab_ = ex_cell%r_(at_b,1:3) - ex_cell%r_(at_a,1:3) &
                + ia*ex_cell%lat_(1,1:3) + ib*ex_cell%lat_(2,1:3) + ic*ex_cell%lat_(3,1:3)
          R = SQRT(DOT_PRODUCT(R_ab_, R_ab_))
          lscale = 1.0/SQRT((ex_cell%alp0_SCS_(at_a)*ex_cell%alp0_SCS_(at_a))**(1.0/3.0) + (ex_cell%alp0_SCS_(at_b)*ex_cell%alp0_SCS_(at_b))**(1.0/3.0))
          U_ab_ = R_ab_ * lscale
          U_sca = R * lscale

          T_lr_aibj = ex_cell%omega_SCS_(at_a)*ex_cell%omega_SCS_(at_b)*SQRT(ex_cell%alp0_SCS_(at_a)*ex_cell%alp0_SCS_(at_b))
          T_lr_aibj = T_lr_aibj * (lscale**3.0) * Pot_Lr(U_ab_, U_sca, this_sys%param_a, dir_a, dir_b, this_sys%type_pot)

          CALL GetvdWParam(ex_cell%spec_(at_a), C6_free, alp0_free, R0_free)
          R0_SCS_a = ((ex_cell%alp0_SCS_(at_a)/alp0_free)**(1.0/3.0))*R0_free
          CALL GetvdWParam(ex_cell%spec_(at_b), C6_free, alp0_free, R0_free)
          R0_SCS_b = ((ex_cell%alp0_SCS_(at_b)/alp0_free)**(1.0/3.0))*R0_free
          R0_ab = R0_SCS_a + R0_SCS_b

          T_lr_aibj = T_lr_aibj * Damping_Lr(R, this_sys%param_d, this_sys%param_b, R0_ab, this_sys%type_damp)
          mat_cfdm_(i,j) = mat_cfdm_(i,j) + T_lr_aibj
         END DO
        ENDIF
       END DO
      ENDIF
     END DO
    END DO
   END DO
  ENDIF

  CALL sync_tensors(mat_cfdm_,3*ex_cell%natom)
 END SUBROUTINE Make_CFDM_Mat_lr

 SUBROUTINE GetDispersionE(mat_cfdm_, ex_cell)
  REAL*8, DIMENSION(:,:), INTENT(INOUT) :: mat_cfdm_
  TYPE (INFO_SPRCELL), INTENT(IN) :: ex_cell

  REAL*8, DIMENSION(:), ALLOCATABLE :: eival_cfdm_
  REAL*8 :: sum_eig, sum_ni, E_disp
  INTEGER :: neg_eig

  REAL*8, DIMENSION(:), ALLOCATABLE :: WORK
  INTEGER :: i, ierr, errorflag, LWORK

  IF (.NOT.ALLOCATED(eival_cfdm_)) ALLOCATE(eival_cfdm_(1:3*ex_cell%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(WORK)) ALLOCATE(WORK(1:3*ex_cell%natom*(3+3*ex_cell%natom/2)), STAT=ierr)
  errorflag=0
  LWORK=3*ex_cell%natom*(3+3*ex_cell%natom/2)
  CALL DSYEV('V','U',3*ex_cell%natom,mat_cfdm_,3*ex_cell%natom,eival_cfdm_,WORK,LWORK,errorflag)

  !Sum of sqrt(eigenvalue)s - exclude negative eigenvalues.
  sum_eig = 0.0D0
  neg_eig = 0
  DO i = 1, 3*ex_cell%natom
   IF (eival_cfdm_(i) >= 0.0D0) THEN
    sum_eig = sum_eig + SQRT(eival_cfdm_(i))
   ELSE
    neg_eig = neg_eig + 1
   ENDIF
  END DO
  sum_eig = 0.5*sum_eig

  !Sum of 3*screened atomic characteristic freq.s
  sum_ni = 0.0D0
  DO i = 1, ex_cell%natom
   sum_ni = sum_ni + ex_cell%omega_SCS_(i)
  END DO
  sum_ni = 1.5*sum_ni

  E_disp = (sum_eig - sum_ni)/ex_cell%fac_dupli

  WRITE(*, '(5X,A,I,A)') 'Total ', neg_eig, ' negative eigenvalues are found.'
  WRITE(*, *) ''

  WRITE(*, *) '###TOTAL DISPERSION ENERGY PER UNIT CELL'
  WRITE(*, *) ''
  WRITE(*, '(5X,A,F15.9)') 'E_disp (a.u.)     = ', E_disp
  WRITE(*, '(5X,A,F15.9)') 'E_disp (eV)       = ', E_disp*27.2107
  WRITE(*, '(5X,A,F15.9)') 'E_disp (kcal/mol) = ', E_disp*627.503
  WRITE(*, '(5X,A,F15.9)') 'E_disp (kJ/mol)   = ', E_disp*2625.5
  WRITE(*, *) ''

  IF (ALLOCATED(eival_cfdm_)) DEALLOCATE(eival_cfdm_, STAT=ierr)
  IF (ALLOCATED(WORK)) DEALLOCATE(WORK, STAT=ierr)
 END SUBROUTINE GetDispersionE
END MODULE CFDM_LongRange
