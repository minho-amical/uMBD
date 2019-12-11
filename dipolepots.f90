MODULE DipolePots
USE MyParams
USE Mympi
IMPLICIT NONE

CONTAINS
 REAL*8 FUNCTION Pot_Gau_Sr(R_ab_, R, Sig_ab, dir_a, dir_b)
  REAL*8, DIMENSION(1:3), INTENT(IN) :: R_ab_
  REAL*8, INTENT(IN) :: R, Sig_ab
  INTEGER, INTENT(IN) :: dir_a, dir_b
  REAL*8 :: zeta

  zeta = R / Sig_ab

  Pot_Gau_Sr = 0.0D0
  IF (dir_a == dir_b) THEN
   Pot_Gau_Sr = Pot_Gau_Sr + 1.0/(R**3.0)
  ENDIF
  Pot_Gau_Sr = Pot_Gau_Sr - 3.0*R_ab_(dir_a)*R_ab_(dir_b)/(R**5.0)
  Pot_Gau_Sr = (ERF(zeta) - (2.0/SQRT(pi))*zeta*EXP(-(zeta**2.0))) * Pot_Gau_Sr
  Pot_Gau_Sr = Pot_Gau_Sr + (4.0/SQRT(pi))*(R_ab_(dir_a)*R_ab_(dir_b)/(R**5.0))*(zeta**3.0)*EXP(-(zeta**2.0))
 END FUNCTION Pot_Gau_Sr

 REAL*8 FUNCTION Damping_Sr(R, param_d, param_b, R0_ab)
  REAL*8, INTENT(IN) :: R, param_d, param_b, R0_ab
  REAL*8 :: Z_ab

  Z_ab = param_d*(R/param_b/R0_ab - 1.0)
  Damping_Sr = 1.0/(1.0+EXP(-Z_ab))
 END FUNCTION Damping_Sr

 REAL*8 FUNCTION Pot_Lr(U_ab_, U_sca, param_a, dir_a, dir_b, type_pot)
  REAL*8, DIMENSION(1:3), INTENT(IN) :: U_ab_
  REAL*8, INTENT(IN) :: U_sca, param_a
  INTEGER, INTENT(IN) :: dir_a, dir_b, type_pot

  REAL*8 :: wpou, wpp
  REAL*8 :: ua, ua2, ua3, ua6

  ua = U_sca / param_a
  ua2 = ua*ua
  ua3 = ua2*ua
  ua6 = ua3*ua3

  SELECT CASE (type_pot)
   CASE (10) !MBD@rsSCS of T&S
    wpou = -1.0/(U_sca**3.0)
    wpp = 2.0/(U_sca**3.0)
   CASE (21) !CFSDM[1]
    wpou = -(1.0/(U_sca**3.0))*(1.0-(1.0+ua+0.5*ua2)*EXP(-ua))
    wpp = (2.0/(U_sca**3.0))*(1.0-(1.0+ua+0.5*ua2+0.25*ua3)*EXP(-ua))
   CASE (22) !CFSDM[2]
    wpou = -(1.0/(U_sca**3.0))*(ERF(ua) - (2.0/SQRT(pi))*ua*EXP(-ua2))
    wpp = (2.0/(U_sca**3.0))*(ERF(ua) - (2.0/SQRT(pi))*(ua+ua3)*EXP(-ua2))
   CASE (23) !CFSDM[3]
    wpou = -(1.0/(U_sca**3.0))*(1.0-EXP(-ua3))
    wpp = (2.0/(U_sca**3.0))*(1.0-(1.0+1.5*ua3)*EXP(-ua3))
   CASE (26) !CFSDM[6]
    wpou = -(1.0/(U_sca**3.0))*ERF(ua3)
    wpp = (2.0/(U_sca**3.0))*(ERF(ua3) - (3.0/SQRT(pi))*ua3*EXP(-ua6))
   CASE DEFAULT
    WRITE(*, *) 'ERROR: No such type of potential - ', type_pot
    CALL stop_mpi()
    STOP
  END SELECT

  Pot_Lr = 0.0D0
  IF (dir_a == dir_b) THEN
   Pot_Lr = Pot_Lr - wpou
  ENDIF

  Pot_Lr = Pot_Lr + (wpou - wpp)*U_ab_(dir_a)*U_ab_(dir_b)/(U_sca**2.0)
 END FUNCTION Pot_Lr

 REAL*8 FUNCTION Damping_Lr(R, param_d, param_b, R0_ab, type_damp)
  REAL*8, INTENT(IN) :: R, param_d, param_b, R0_ab
  INTEGER, INTENT(IN) :: type_damp

  REAL*8 :: Z_ab

  SELECT CASE (type_damp)
   CASE (1) !No damping
    Damping_Lr = 1.0D0
   CASE (2) !Fermi damping
    Z_ab = param_d*(R/param_b/R0_ab -1.0)
    Damping_Lr = 1.0/(1.0+EXP(-Z_ab))
   CASE DEFAULT
    WRITE(*, *) 'ERROR: No such type of damping - ', type_damp
    CALL stop_mpi()
    STOP
  END SELECT
 END FUNCTION Damping_Lr
END MODULE DipolePots
