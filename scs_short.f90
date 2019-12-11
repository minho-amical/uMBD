!!SCS_ShortRange module
MODULE SCS_ShortRange
USE MyParams
USE CFDM_Setup
USE DipolePots
USE Mympi
IMPLICIT NONE

CONTAINS
 SUBROUTINE Solve_SCS_sr(this_sys)
  TYPE (SYS_INP), INTENT(INOUT) :: this_sys

  INTEGER :: x_hi, y_hi, z_hi
  REAL*8, DIMENSION(1:3) :: latvec_
  REAL*8 :: a1, a2, a3
  REAL*8, PARAMETER :: tol = 1.0D-5

  REAL*8, DIMENSION(0:20) :: omega_CP_, weight_CP_
  REAL*8, DIMENSION(:), ALLOCATABLE :: C6_free_, alp0_TS_, R0_TS_, alp_TS_omega_
  REAL*8, DIMENSION(:), ALLOCATABLE :: omega_free_
  REAL*8, DIMENSION(:), ALLOCATABLE :: C6_SCS_
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: relay_mat_
  REAL*8, DIMENSION(1:3,1:3) :: alp_SCS_omega_, mol_pol_SCS_omega_, mol_pol0_SCS_
  REAL*8 :: ialp_SCS_omega, imol_pol_SCS_omega, mol_C6_SCS

  INTEGER :: i, j, ifreq, ierr
  INTEGER :: at_a, at_b, dir_a, dir_b, ia, ib, ic
  REAL*8, DIMENSION(1:3) :: R_ab_
  REAL*8 :: alp_tmp, R, sig_a, sig_b, Sig_ab, R0_ab
  REAL*8 :: T_sr_aibj
  INTEGER :: is_periodic
  REAL*8, PARAMETER :: factor1 = (1.0/3.0)*SQRT(2.0/pi)

  INTEGER, DIMENSION(3*this_sys%natom) :: IPIV
  REAL*8, DIMENSION(3*this_sys%natom) :: WORK

  IF (myid == 0) THEN
   WRITE(*, *) '###SHORT-RANGE SCS CALCULATION###'
   WRITE(*, *) ''
  END IF

  !Apply PBC using scs_rcut (also consider vacuum axis)
  IF (ABS(this_sys%scs_rcut) < tol) THEN
   x_hi = 0; y_hi = 0; z_hi = 0
  ELSE
   IF (this_sys%vac_axis(1) == .TRUE.) THEN
    x_hi = 0
   ELSE
    latvec_ = this_sys%lat_(1,1:3)
    a1 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    x_hi = CEILING(this_sys%scs_rcut/a1)
   ENDIF

   IF (this_sys%vac_axis(2) == .TRUE.) THEN
    y_hi = 0
   ELSE
    latvec_ = this_sys%lat_(2,1:3)
    a2 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    y_hi = CEILING(this_sys%scs_rcut/a2)
   ENDIF

   IF (this_sys%vac_axis(3) == .TRUE.) THEN
    z_hi = 0
   ELSE
    latvec_ = this_sys%lat_(3,1:3)
    a3 = SQRT(DOT_PRODUCT(latvec_, latvec_))
    z_hi = CEILING(this_sys%scs_rcut/a3)
   ENDIF
  ENDIF

  IF (myid == 0) THEN
   WRITE(*, '(3(X,A,X,I4), A)') '', 2*x_hi+1, '*', 2*y_hi+1, '*', 2*z_hi+1, ' auxiliary supercell has considered for SCS calculation'
   WRITE(*, *) ''
  ENDIF

  !Get Gauss-Legendr quadratue frequency and their weight
  omega_CP_ = 0.0D0
  weight_CP_ = 0.0D0
  CALL Get_gauss_legendre_grid(omega_CP_, weight_CP_)

  !Read free-atom values and calculate free-atom characteristic freq. 
  !Then, do TS scaling for polarizabilities and vdW radii
  IF (.NOT.ALLOCATED(C6_free_)) ALLOCATE(C6_free_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(alp0_TS_)) ALLOCATE(alp0_TS_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(R0_TS_)) ALLOCATE(R0_TS_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(omega_free_)) ALLOCATE(omega_free_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(alp_TS_omega_)) ALLOCATE(alp_TS_omega_(1:this_sys%natom), STAT=ierr)
  DO i = 1, this_sys%natom
   CALL GetvdWParam(this_sys%spec_(i), C6_free_(i), alp0_TS_(i), R0_TS_(i))
   IF (this_sys%fi_on) THEN
    omega_free_(i) = (4.0/3.0)*C6_free_(i)/(alp0_TS_(i)**2.0)
    alp_tmp = alp0_TS_(i) + 0.0
    alp0_TS_(i) = alp_freq_fi(0.0D0, this_sys%spec_(i), this_sys%q_hi_(i), this_sys%relvol_(i))
    R0_TS_(i) = R0_TS_(i)*((alp0_TS_(i)/alp_tmp)**(1.0/3.0))
   ELSE
    omega_free_(i) = (4.0/3.0)*C6_free_(i)/(alp0_TS_(i)**2.0)
    alp0_TS_(i) = alp0_TS_(i)*this_sys%relvol_(i)
    R0_TS_(i) = R0_TS_(i)*(this_sys%relvol_(i)**(1.0/3.0))
   ENDIF
  END DO

  !Initialize the Carsimir-Polder integration for C6 values
  !And allocate the screened static polarizabilities and screened characteristic freq.s
  IF (.NOT.ALLOCATED(C6_SCS_)) ALLOCATE(C6_SCS_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(this_sys%alp0_SCS_)) ALLOCATE(this_sys%alp0_SCS_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(this_sys%omega_SCS_)) ALLOCATE(this_sys%omega_SCS_(1:this_sys%natom), STAT=ierr)
  IF (.NOT.ALLOCATED(relay_mat_)) ALLOCATE(relay_mat_(1:3*this_sys%natom,1:3*this_sys%natom), STAT=ierr)
  C6_SCS_ = 0.0D0
  this_sys%alp0_SCS_ = 0.0D0
  this_sys%omega_SCS_ = 0.0D0
  mol_C6_SCS = 0.0D0

  !Loop for frequency: C-P integration
  DO ifreq = 0, 20
   !Frequency-dependent TS polarizabilities
   DO i = 1,this_sys%natom
    IF (this_sys%fi_on) THEN
     alp_TS_omega_(i) = alp_freq_fi(omega_CP_(ifreq), this_sys%spec_(i), this_sys%q_hi_(i), this_sys%relvol_(i))
    ELSE
     alp_TS_omega_(i) = alp_freq(omega_CP_(ifreq), omega_free_(i), alp0_TS_(i))
    ENDIF
   END DO

   !Initialize relay matrix
   relay_mat_ = 0.0D0

   !Calculate inverse of freq-dependent polarizabilities --> store to matix A
   DO i = 1, 3*this_sys%natom
    IF (myid == task_list(i)) THEN
     at_a = (i+2)/3
     relay_mat_(i,i) = 1.0/alp_TS_omega_(at_a)
    END IF
   END DO

   !Calculate short-range dipole-dipole tensor T_sr --> add to matrix A
   !First, interactions in single supercell

   DO i = 1, 3*this_sys%natom
    IF (myid == task_list(i)) THEN
     DO j = 1, 3*this_sys%natom
      at_a = (i+2)/3
      at_b = (j+2)/3
      dir_a = MOD(i-1,3)+1
      dir_b = MOD(j-1,3)+1

      IF (at_a /= at_b) THEN
       R_ab_ =this_sys%r_(at_b,1:3) - this_sys%r_(at_a,1:3)
       R = SQRT(DOT_PRODUCT(R_ab_, R_ab_))
       sig_a = (factor1*alp_TS_omega_(at_a))**(1.0/3.0)
       sig_b = (factor1*alp_TS_omega_(at_b))**(1.0/3.0)
       Sig_ab = SQRT(sig_a*sig_a+sig_b*sig_b)

       T_sr_aibj = Pot_Gau_Sr(R_ab_, R, Sig_ab, dir_a, dir_b)

       R0_ab = R0_TS_(at_a) + R0_TS_(at_b)

       T_sr_aibj = T_sr_aibj*(1.0-Damping_Sr(R, this_sys%param_d, this_sys%param_b, R0_ab))

       relay_mat_(i,j) = relay_mat_(i,j) + T_sr_aibj
      ENDIF
     END DO
    ENDIF
   END DO

   !Second, if the system is periodic, do the loop for auxiliary supercell images 
   is_periodic = x_hi + y_hi + z_hi
   IF (is_periodic > 0) THEN
    DO ia = -x_hi, x_hi
     DO ib = -y_hi, y_hi
      DO ic = -z_hi, z_hi
       IF (ia /= 0 .OR. ib /= 0 .OR. ic /= 0) THEN
        DO i = 1, 3*this_sys%natom
         IF (myid == task_list(i)) THEN
          DO j = 1, 3*this_sys%natom
           at_a = (i+2)/3
           at_b = (j+2)/3
           dir_a = MOD(i-1,3)+1
           dir_b = MOD(j-1,3)+1
 
           R_ab_ = this_sys%r_(at_b,1:3) - this_sys%r_(at_a,1:3) &
                 + ia*this_sys%lat_(1,1:3) + ib*this_sys%lat_(2,1:3) + ic*this_sys%lat_(3,1:3)
           R = SQRT(DOT_PRODUCT(R_ab_, R_ab_))
           sig_a = (factor1*alp_TS_omega_(at_a))**(1.0/3.0)
           sig_b = (factor1*alp_TS_omega_(at_b))**(1.0/3.0)
           Sig_ab = SQRT(sig_a*sig_a+sig_b*sig_b)
 
           T_sr_aibj = Pot_Gau_Sr(R_ab_, R, Sig_ab, dir_a, dir_b)
 
           R0_ab = R0_TS_(at_a) + R0_TS_(at_b)
 
           T_sr_aibj = T_sr_aibj*(1.0-Damping_Sr(R, this_sys%param_d, this_sys%param_b, R0_ab))
           relay_mat_(i,j) = relay_mat_(i,j) + T_sr_aibj
          END DO
         ENDIF
        END DO
       ENDIF
      END DO
     END DO
    END DO
   ENDIF

   CALL sync_tensors(relay_mat_,3*this_sys%natom)

   !Calculate relay matrix: inverse of (A^-1+T_sr)
   CALL DGETRF(3*this_sys%natom, 3*this_sys%natom, relay_mat_, 3*this_sys%natom, IPIV, ierr)
   IF (ierr /= 0) THEN
    WRITE(*, *) 'ERROR: Inversion of relay matrix is failed'
    CALL stop_mpi()
    STOP
   ENDIF

   CALL DGETRI(3*this_sys%natom, relay_mat_, 3*this_sys%natom, IPIV, WORK, 3*this_sys%natom, ierr)
   IF (ierr /= 0) THEN
    WRITE(*, *) 'ERROR: Inversion of relay matrix is failed'
    CALL stop_mpi()
    STOP
   ENDIF

   !Relay matrix has been obtained. Now, calculate screened polarizabilities and integrate C6 values
   !First, for each atom.
   mol_pol_SCS_omega_ = 0.0D0
   DO i = 1, this_sys%natom
    !Contract the relay matrix: atomic polarizabilities loop construction for partial contraction
    alp_SCS_omega_ = 0.0D0
    DO j = 1, this_sys%natom
     alp_SCS_omega_ = alp_SCS_omega_ + relay_mat_(3*i-2:3*i,3*j-2:3*j)
    END DO

    !Contraction to the molecular polarizability togather in this step
    mol_pol_SCS_omega_ = mol_pol_SCS_omega_ + alp_SCS_omega_

    !Take the trace (isotropized screened polarizability) and add it on the C-P integration
    ialp_SCS_omega = (alp_SCS_omega_(1,1) + alp_SCS_omega_(2,2) + alp_SCS_omega_(3,3))/3.0
    C6_SCS_(i) = C6_SCS_(i) + three_over_pi*weight_CP_(ifreq)*(ialp_SCS_omega**2.0)

    !For zero frequency, store the isotropized screened staic atomic polarizability
    IF (ifreq == 0) THEN
     this_sys%alp0_SCS_(i) = ialp_SCS_omega
    ENDIF
   END DO

   !Second, for whole system (i.e. molecular properties)
   !Take the trace (isotropized screened polarizability) and add it on the C-P integration
   imol_pol_SCS_omega = (mol_pol_SCS_omega_(1,1) + mol_pol_SCS_omega_(2,2) + mol_pol_SCS_omega_(3,3))/3.0
   mol_C6_SCS = mol_C6_SCS + three_over_pi*weight_CP_(ifreq)*(imol_pol_SCS_omega**2.0)
   !For zero frequency, store the screened static molecular polarizability
   IF (ifreq == 0) THEN
    mol_pol0_SCS_ = mol_pol_SCS_omega_
   ENDIF
  END DO

  !Integration ends. Calculate screened characteristic freq.s
  DO i = 1, this_sys%natom
   this_sys%omega_SCS_(i) = (4.0/3.0)*C6_SCS_(i)/(this_sys%alp0_SCS_(i)**2.0)
  END DO

  !Count the number of atoms with negative screened polarizability
  DO i = 1, this_sys%natom
   IF (this_sys%alp0_SCS_(i) < 0) THEN
    this_sys%nnega = this_sys%nnega + 1
   ENDIF
  END DO

  IF (myid == 0) THEN
   WRITE(*, '(5X,A)') 'SCREENED ATOMIC PARAMETERS IN A.U.'
   WRITE(*, '(5X,A)') '----------------------------------' 
   WRITE(*, '(5X,A8,3A9)') 'ELEMENTS', 'C6', 'ALP0', 'OMEGA0'
   DO i = 1, this_sys%natom
    WRITE(*, '(5X,A8,3F9.3)') this_sys%spec_(i), C6_SCS_(i), this_sys%alp0_SCS_(i), this_sys%omega_SCS_(i)
   END DO
   WRITE(*, '(5X,A)') '----------------------------------'
   WRITE(*, *) ''

   WRITE(*, '(5X,A,F9.3)') 'SCREENED MOLECULAR C6                    = ', mol_C6_SCS
   WRITE(*, '(5X,A,F9.3)') 'SCREENED STATIC MOLECULAR POLARISABILITY = ', (mol_pol0_SCS_(1,1) + mol_pol0_SCS_(2,2) + mol_pol0_SCS_(3,3))/3.0
   WRITE(*, *) ''

   WRITE(*, *) '###SHORT-RANGE SCS CALCULATION END###'
   WRITE(*, *) ''
  ENDIF

  IF (ALLOCATED(C6_free_)) DEALLOCATE(C6_free_, STAT=ierr)
  IF (ALLOCATED(alp0_TS_)) DEALLOCATE(alp0_TS_, STAT=ierr)
  IF (ALLOCATED(R0_TS_)) DEALLOCATE(R0_TS_, STAT=ierr)
  IF (ALLOCATED(omega_free_)) DEALLOCATE(omega_free_, STAT=ierr)
  IF (ALLOCATED(alp_TS_omega_)) DEALLOCATE(alp_TS_omega_, STAT=ierr)
  IF (ALLOCATED(C6_SCS_)) DEALLOCATE(C6_SCS_, STAT=ierr)
  IF (ALLOCATED(relay_mat_)) DEALLOCATE(relay_mat_, STAT=ierr)
  RETURN
 END SUBROUTINE Solve_SCS_sr

 SUBROUTINE Get_gauss_legendre_grid(omega_, weight_)
  REAL*8, DIMENSION(0:20), INTENT(INOUT) :: omega_, weight_

  omega_(0) = 0.0000000 ; weight_(0) = 0.0000000
  omega_(1) = 0.0392901 ; weight_(1) = 0.0786611
  omega_(2) = 0.1183580 ; weight_(2) = 0.0796400
  omega_(3) = 0.1989120 ; weight_(3) = 0.0816475
  omega_(4) = 0.2820290 ; weight_(4) = 0.0847872
  omega_(5) = 0.3689190 ; weight_(5) = 0.0892294
  omega_(6) = 0.4610060 ; weight_(6) = 0.0952317
  omega_(7) = 0.5600270 ; weight_(7) = 0.1031720
  omega_(8) = 0.6681790 ; weight_(8) = 0.1136050
  omega_(9) = 0.7883360 ; weight_(9) = 0.1273500
  omega_(10) = 0.9243900 ; weight_(10) = 0.1456520
  omega_(11) = 1.0817900 ; weight_(11) = 0.1704530
  omega_(12) = 1.2684900 ; weight_(12) = 0.2049170
  omega_(13) = 1.4966100 ; weight_(13) = 0.2544560
  omega_(14) = 1.7856300 ; weight_(14) = 0.3289620
  omega_(15) = 2.1691700 ; weight_(15) = 0.4480920
  omega_(16) = 2.7106200 ; weight_(16) = 0.6556060
  omega_(17) = 3.5457300 ; weight_(17) = 1.0659600
  omega_(18) = 5.0273400 ; weight_(18) = 2.0635700
  omega_(19) = 8.4489600 ; weight_(19) = 5.6851000
  omega_(20) = 25.451700 ; weight_(20) = 50.955800

  RETURN
 END SUBROUTINE Get_gauss_legendre_grid

 REAL*8 FUNCTION alp_freq(omega, omega_free, alpha)
  REAL*8, INTENT(IN) :: omega, omega_free, alpha
  alp_freq = alpha/(1.0 + (omega/omega_free)**2.0)
 END FUNCTION alp_freq

 REAL*8 FUNCTION alp_freq_fi(omega, spec, q_hi, relvol)
  REAL*8, INTENT(IN) :: omega, q_hi, relvol
  REAL*8 :: a1, a2, o1, o2, alp_upper, alp_lower
  REAL*8 :: q_upper, q_lower, fq
  CHARACTER(LEN=2), INTENT(IN) :: spec

  q_lower = FLOOR(q_hi)
  q_upper = q_lower + 1.0
  fq = q_hi - q_lower

  CALL GetFIParam(spec, q_upper, a1, a2, o1, o2)
  alp_upper = a1/(omega**2.0+o1**2.0) + a2/(omega**2.0+o2**2.0)
  CALL GetFIParam(spec, q_lower, a1, a2, o1, o2)
  alp_lower = a1/(omega**2.0+o1**2.0) + a2/(omega**2.0+o2**2.0)
  alp_freq_fi = (fq*alp_upper + (1.0-fq)*alp_lower)*relvol
 END FUNCTION alp_freq_fi
END MODULE SCS_ShortRange
