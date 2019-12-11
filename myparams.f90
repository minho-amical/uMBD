!!MyParams module
MODULE MyParams
USE Mympi
IMPLICIT NONE
 REAL*8, PARAMETER :: bohr_to_ang = 0.5291772109D0
 REAL*8, PARAMETER :: ang_to_bohr = 1.8897261246D0
 REAL*8, PARAMETER :: pi = 3.14159265358979323846D0
 REAL*8, PARAMETER :: three_over_pi = 3.0/pi

CONTAINS
 SUBROUTINE GetvdWParam(elem, C6_fr, alpha_fr, R0_fr)
  CHARACTER(LEN=2), INTENT(IN) :: elem
  REAL*8, INTENT(INOUT) :: C6_fr, alpha_fr, R0_fr

  SELECT CASE (elem)
  CASE('H')
   alpha_fr=4.500000; C6_fr=6.500000; R0_fr=3.100000
  CASE('He')
   alpha_fr=1.380000; C6_fr=1.460000; R0_fr=2.650000
  CASE('Li')
   alpha_fr=164.200000; C6_fr=1387.000000; R0_fr=4.160000
  CASE('Be')
   alpha_fr=38.000000; C6_fr=214.000000; R0_fr=4.170000
  CASE('B')
   alpha_fr=21.000000; C6_fr=99.500000; R0_fr=3.890000
  CASE('C')
   alpha_fr=12.000000; C6_fr=46.600000; R0_fr=3.590000
  CASE('N')
   alpha_fr=7.400000; C6_fr=24.200000; R0_fr=3.340000
  CASE('O')
   alpha_fr=5.400000; C6_fr=15.600000; R0_fr=3.190000
  CASE('F')
   alpha_fr=3.800000; C6_fr=9.520000; R0_fr=3.040000
  CASE('Ne')
   alpha_fr=2.670000; C6_fr=6.380000; R0_fr=2.910000
  CASE('Na')
   alpha_fr=162.700000; C6_fr=1556.000000; R0_fr=3.730000
  CASE('Mg')
   alpha_fr=71.000000; C6_fr=627.000000; R0_fr=4.270000
  CASE('Al')
   alpha_fr=60.000000; C6_fr=528.000000; R0_fr=4.330000
  CASE('Si')
   alpha_fr=37.000000; C6_fr=305.000000; R0_fr=4.200000
  CASE('P')
   alpha_fr=25.000000; C6_fr=185.000000; R0_fr=4.010000
  CASE('S')
   alpha_fr=19.600000; C6_fr=134.000000; R0_fr=3.860000
  CASE('Cl')
   alpha_fr=15.000000; C6_fr=94.600000; R0_fr=3.710000
  CASE('Ar')
   alpha_fr=11.100000; C6_fr=64.300000; R0_fr=3.550000
  CASE('K')
   alpha_fr=292.900000; C6_fr=3897.000000; R0_fr=3.710000
  CASE('Ca')
   alpha_fr=160.000000; C6_fr=2221.000000; R0_fr=4.650000
  CASE('Sc')
   alpha_fr=120.000000; C6_fr=1383.000000; R0_fr=4.590000
  CASE('Ti')
   alpha_fr=98.000000; C6_fr=1044.000000; R0_fr=4.510000
  CASE('V')
   alpha_fr=84.000000; C6_fr=832.000000; R0_fr=4.440000
  CASE('Cr')
   alpha_fr=78.000000; C6_fr=602.000000; R0_fr=3.990000
  CASE('Mn')
   alpha_fr=63.000000; C6_fr=552.000000; R0_fr=3.970000
  CASE('Fe')
   alpha_fr=56.000000; C6_fr=482.000000; R0_fr=4.230000
  CASE('Co')
   alpha_fr=50.000000; C6_fr=408.000000; R0_fr=4.180000
  CASE('Ni')
   alpha_fr=48.000000; C6_fr=373.000000; R0_fr=3.820000
  CASE('Cu')
   alpha_fr=42.000000; C6_fr=253.000000; R0_fr=3.760000
  CASE('Zn')
   alpha_fr=40.000000; C6_fr=284.000000; R0_fr=4.020000
  CASE('Ga')
   alpha_fr=60.000000; C6_fr=498.000000; R0_fr=4.190000
  CASE('Ge')
   alpha_fr=41.000000; C6_fr=354.000000; R0_fr=4.200000
  CASE('As')
   alpha_fr=29.000000; C6_fr=246.000000; R0_fr=4.110000
  CASE('Se')
   alpha_fr=25.000000; C6_fr=210.000000; R0_fr=4.040000
  CASE('Br')
   alpha_fr=20.000000; C6_fr=162.000000; R0_fr=3.930000
  CASE('Kr')
   alpha_fr=16.800000; C6_fr=129.600000; R0_fr=3.820000
  CASE('Rb')
   alpha_fr=319.200000; C6_fr=4691.000000; R0_fr=3.720000
  CASE('Sr')
   alpha_fr=199.000000; C6_fr=3170.000000; R0_fr=4.540000
  CASE('Y')
   alpha_fr=126.7370; C6_fr=1968.580; R0_fr=4.81510
  CASE('Zr')
   alpha_fr=119.97; C6_fr=1677.91; R0_fr=4.53
  CASE('Nb')
   alpha_fr=101.603; C6_fr=1263.61; R0_fr=4.2365
  CASE('Mo')
   alpha_fr=88.4225785; C6_fr=1028.73; R0_fr=4.099
  CASE('Tc')
   alpha_fr=80.083; C6_fr=1390.87; R0_fr=4.076
  CASE('Ru')
   alpha_fr=65.8950; C6_fr=609.754; R0_fr=3.99530
  CASE('Rh')
   alpha_fr=56.1; C6_fr=469.0; R0_fr=3.95
  CASE('Pd')
   alpha_fr=23.680000; C6_fr=157.500000; R0_fr=3.66000
  CASE('Ag')
   alpha_fr=50.600000; C6_fr=339.000000; R0_fr=3.820000
  CASE('Cd')
   alpha_fr=39.7; C6_fr=452.0; R0_fr=3.99
  CASE('In')
   alpha_fr=70.22000; C6_fr=707.046000; R0_fr=4.23198000
  CASE('Sn')
   alpha_fr=55.9500; C6_fr=587.41700; R0_fr=4.303000
  CASE('Sb')
   alpha_fr=43.671970; C6_fr=459.322; R0_fr=4.2760
  CASE('Te')
   alpha_fr=37.65; C6_fr=396.0; R0_fr=4.22
  CASE('I')
   alpha_fr=35.000000; C6_fr=385.000000; R0_fr=4.170000
  CASE('Xe')
   alpha_fr=27.300000; C6_fr=285.900000; R0_fr=4.080000
  CASE('Cs')
   alpha_fr=427.12; C6_fr=6582.08; R0_fr=3.78
  CASE('Ba')
   alpha_fr=275.0; C6_fr=5727.0; R0_fr=4.77
  CASE('Hf')
   alpha_fr=99.52; C6_fr=1274.8; R0_fr=4.21
  CASE('Ta')
   alpha_fr=82.53; C6_fr=1019.92; R0_fr=4.15
  CASE('W')
   alpha_fr=71.041; C6_fr=847.93; R0_fr=4.08
  CASE('Re')
   alpha_fr=63.04; C6_fr=710.2; R0_fr=4.02
  CASE('Os')
   alpha_fr=55.055; C6_fr=596.67; R0_fr=3.84
  CASE('Ir')
   alpha_fr=42.51; C6_fr=359.1; R0_fr=4.00
  CASE('Pt')
   alpha_fr=39.68; C6_fr=347.1; R0_fr=3.92
  CASE('Au')
   alpha_fr=36.5; C6_fr=298.0; R0_fr=3.86
  CASE('Hg')
   alpha_fr=33.9; C6_fr=392.0; R0_fr=3.98
  CASE('Tl')
   alpha_fr=69.92; C6_fr=717.44; R0_fr=3.91
  CASE('Pb')
   alpha_fr=61.8; C6_fr=697.0; R0_fr=4.31
  CASE('Bi')
   alpha_fr=49.02; C6_fr=571.0; R0_fr=4.32
  CASE('Po')
   alpha_fr=45.013; C6_fr=530.92; R0_fr=4.097
  CASE('At')
   alpha_fr=38.93; C6_fr=457.53; R0_fr=4.07
  CASE('Rn')
   alpha_fr=33.54; C6_fr=390.63; R0_fr=4.23
  CASE default
   WRITE(*,*) 'ERROR: VdW parameters not defined for atom: ', elem
   CALL stop_mpi()
   STOP
  END SELECT

  RETURN
 END SUBROUTINE GetvdWParam

 SUBROUTINE GetFIParam(elem, qion, a1, a2, o1, o2)
  CHARACTER(LEN=2), INTENT(IN) :: elem
  REAL*8, INTENT(IN) :: qion
  REAL*8, INTENT(INOUT) :: a1, a2, o1, o2

  SELECT CASE (elem)                                                                                  
  CASE('H')                                                                                           
   IF (qion == 0.0) THEN
    a1=0.0000; o1=1.0000; a2=0.0000; o2=1.0000
   ELSE IF (qion == 1.0) THEN
    a1=0.6825; o1=0.4037; a2=0.3175; o2=1.0098
   ELSE IF (qion == 2.0) THEN
    a1=1.2407; o1=0.4301; a2=0.7593; o2=1.0672
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP    
   ENDIF
  CASE('He')                                                                                          
   IF (qion == 1.0) THEN
    a1=0.6812; o1=1.5787; a2=0.3188; o2=3.9325
   ELSE IF (qion == 2.0) THEN
    a1=1.0856; o1=0.9359; a2=0.9144; o2=2.5508
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Li')                                                                                          
   IF (qion == 1.0) THEN
    a1=0.8075; o1=3.7968; a2=0.1925; o2=15.9130
   ELSE IF (qion == 2.0) THEN
    a1=1.1925; o1=2.6059; a2=0.8075; o2=6.8125
   ELSE IF (qion == 3.0) THEN
    a1=0.7860; o1=0.0693; a2=2.2140; o2=2.1502
   ELSE IF (qion == 4.0) THEN
    a1=1.6087; o1=0.1000; a2=2.3913; o2=1.9523
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Be')                                                                                          
   IF (qion == 1.0) THEN
    a1=0.9989; o1=8.3896; a2=0.0011; o2=43.3079
   ELSE IF (qion == 2.0) THEN
    a1=1.9663; o1=12.8238; a2=0.0337; o2=27.9589
   ELSE IF (qion == 3.0) THEN
    a1=0.5688; o1=0.1531; a2=2.4312; o2=3.1924
   ELSE IF (qion == 4.0) THEN
    a1=1.4767; o1=0.1984; a2=2.5233; o2=3.6141
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('B')                                                                                           
   IF (qion == 2.0) THEN
    a1=1.9986; o1=11.5815; a2=0.0014; o2=48.2565
   ELSE IF (qion == 3.0) THEN
    a1=0.4234; o1=0.2750; a2=2.5766; o2=5.3578
   ELSE IF (qion == 4.0) THEN
    a1=1.1775; o1=0.3512; a2=2.8225; o2=4.7950
   ELSE IF (qion == 5.0) THEN
    a1=1.9004; o1=0.3067; a2=3.0996; o2=3.2165
   ELSE IF (qion == 6.0) THEN
    a1=2.4251; o1=0.2745; a2=3.5749; o2=2.1685
   ELSE IF (qion == 7.0) THEN
    a1=2.9209; o1=0.2778; a2=4.0791; o2=1.8098
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('C')                                                                                           
   IF (qion == 3.0) THEN
    a1=0.3295; o1=0.3512; a2=2.6705; o2=7.0831
   ELSE IF (qion == 4.0) THEN
    a1=0.9269; o1=0.4966; a2=2.0731; o2=6.1607
   ELSE IF (qion == 5.0) THEN
    a1=1.5022; o1=0.5236; a2=3.4978; o2=4.4052
   ELSE IF (qion == 6.0) THEN
    a1=2.2621; o1=0.4460; a2=3.7379; o2=3.3797
   ELSE IF (qion == 7.0) THEN
    a1=2.3793; o1=0.4289; a2=4.2607; o2=2.7043
   ELSE IF (qion == 8.0) THEN
    a1=3.1656; o1=0.4478; a2=4.8344; o2=2.4461
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('N')                                                                                           
   IF (qion == 4.0) THEN
    a1=0.7442; o1=0.6073; a2=3.2558; o2=7.5332
   ELSE IF (qion == 5.0) THEN
    a1=1.1045; o1=0.6931; a2=3.8955; o2=5.4584
   ELSE IF (qion == 6.0) THEN
    a1=1.8414; o1=0.7267; a2=4.1586; o2=4.6452
   ELSE IF (qion == 7.0) THEN
    a1=2.5922; o1=0.6111; a2=4.4078; o2=3.7758
   ELSE IF (qion == 8.0) THEN
    a1=3.0246; o1=0.6305; a2=4.9754; o2=3.4029
   ELSE IF (qion == 9.0) THEN
    a1=3.4408; o1=0.6499; a2=5.5592; o2=3.1875
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('O')                                                                                           
   IF (qion == 5.0) THEN
    a1=0.8357; o1=0.7397; a2=4.1643; o2=6.0441
   ELSE IF (qion == 6.0) THEN
    a1=1.3355; o1=0.9364; a2=4.6645; o2=5.6323
   ELSE IF (qion == 7.0) THEN
    a1=2.2652; o1=0.9759; a2=4.7348; o2=5.2510
   ELSE IF (qion == 8.0) THEN
    a1=2.7732; o1=0.7534; a2=5.2268; o2=4.0761
   ELSE IF (qion == 9.0) THEN
    a1=3.1213; o1=0.7893; a2=5.8787; o2=3.8957
   ELSE IF (qion == 10.0) THEN
    a1=3.4683; o1=0.8030; a2=6.5317; o2=3.7054
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('F')                                                                                           
   IF (qion == 6.0) THEN
    a1=0.9706; o1=1.0038; a2=5.0294; o2=6.2811
   ELSE IF (qion == 7.0) THEN
    a1=1.7248; o1=1.2598; a2=5.2752; o2=6.2917
   ELSE IF (qion == 8.0) THEN
    a1=2.8302; o1=1.3079; a2=5.1698; o2=6.4189
   ELSE IF (qion == 9.0) THEN
    a1=2.9450; o1=0.9413; a2=6.0550; o2=4.6821
   ELSE IF (qion == 10.0) THEN
    a1=1.7195; o1=0.3597; a2=8.2805; o2=2.2023
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ne')                                                                                          
   IF (qion == 7.0) THEN
    a1=1.2529; o1=1.3919; a2=5.7471; o2=7.0406
   ELSE IF (qion == 8.0) THEN
    a1=2.4999; o1=1.7014; a2=5.5001; o2=7.6366
   ELSE IF (qion == 9.0) THEN
    a1=3.4082; o1=1.5946; a2=5.5918; o2=7.3857
   ELSE IF (qion == 10.0) THEN
    a1=3.1603; o1=1.1397; a2=6.8397; o2=5.3709
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Na')                                                                                          
   IF (qion == 8.0) THEN
    a1=2.0271; o1=2.2781; a2=5.9729; o2=9.6947
   ELSE IF (qion == 9.0) THEN
    a1=3.4605; o1=2.4981; a2=5.5395; o2=10.7596
   ELSE IF (qion == 10.0) THEN
    a1=4.0189; o1=2.1561; a2=5.9811; o2=9.5547
   ELSE IF (qion == 11.0) THEN
    a1=0.9798; o1=0.0777; a2=10.0202; o2=3.5645
   ELSE IF (qion == 12.0) THEN
    a1=1.9219; o1=0.0991; a2=10.0781; o2=3.3345
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Mg')                                                                                          
   IF (qion == 9.0) THEN
    a1=5.6814; o1=13.6347; a2=3.3186; o2=3.3900
   ELSE IF (qion == 10.0) THEN
    a1=5.4365; o1=14.5651; a2=4.5635; o2=3.3594
   ELSE IF (qion == 11.0) THEN
    a1=0.9379; o1=0.1647; a2=10.0621; o2=4.8647
   ELSE IF (qion == 12.0) THEN
    a1=1.8727; o1=0.1624; a2=10.1273; o2=4.8672
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Al')                                                                                          
   IF (qion == 10.0) THEN
    a1=5.0987; o1=21.1055; a2=4.9013; o2=4.9717
   ELSE IF (qion == 11.0) THEN
    a1=0.8739; o1=0.2921; a2=10.1261; o2=7.3424
   ELSE IF (qion == 12.0) THEN
    a1=1.8117; o1=0.3057; a2=10.1883; o2=7.2336
   ELSE IF (qion == 13.0) THEN
    a1=2.4419; o1=0.2068; a2=10.5581; o2=5.1776
   ELSE IF (qion == 14.0) THEN
    a1=3.1773; o1=0.1710; a2=10.8227; o2=3.8515
   ELSE IF (qion == 15.0) THEN
    a1=3.9183; o1=0.1692; a2=11.0817; o2=3.4090
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Si')                                                                                          
   IF (qion == 11.0) THEN
    a1=0.8081; o1=0.3902; a2=10.1919; o2=9.1571
   ELSE IF (qion == 12.0) THEN
    a1=1.6977; o1=0.4278; a2=10.3023; o2=8.9974
   ELSE IF (qion == 13.0) THEN
    a1=2.5386; o1=0.3749; a2=10.4614; o2=8.3522
   ELSE IF (qion == 14.0) THEN
    a1=3.2254; o1=0.2962; a2=10.7746; o2=6.5942
   ELSE IF (qion == 15.0) THEN
    a1=3.9664; o1=0.2589; a2=11.0336; o2=5.2134
   ELSE IF (qion == 16.0) THEN
    a1=4.6454; o1=0.2712; a2=11.3546; o2=4.7081
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('P')                                                                                           
   IF (qion == 12.0) THEN
    a1=1.5780; o1=0.5406; a2=10.4220; o2=10.7251
   ELSE IF (qion == 13.0) THEN
    a1=2.3731; o1=0.5176; a2=10.6269; o2=10.0260
   ELSE IF (qion == 14.0) THEN
    a1=3.2288; o1=0.4778; a2=10.7712; o2=9.4439
   ELSE IF (qion == 15.0) THEN
    a1=3.9364; o1=0.3999; a2=11.0636; o2=7.8376
   ELSE IF (qion == 16.0) THEN
    a1=4.6260; o1=0.3720; a2=11.3740; o2=6.3845
   ELSE IF (qion == 17.0) THEN
    a1=5.3036; o1=0.3821; a2=11.6964; o2=5.8885
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('S')                                                                                           
   IF (qion == 13.0) THEN
    a1=2.1793; o1=0.6213; a2=10.8207; o2=11.0435
   ELSE IF (qion == 14.0) THEN
    a1=3.0191; o1=0.6130; a2=10.9809; o2=10.6074
   ELSE IF (qion == 15.0) THEN
    a1=3.8887; o1=0.5738; a2=11.1113; o2=10.1104
   ELSE IF (qion == 16.0) THEN
    a1=4.4693; o1=0.4810; a2=11.5307; o2=8.0336
   ELSE IF (qion == 17.0) THEN
    a1=5.1213; o1=0.4631; a2=11.8787; o2=6.9618
   ELSE IF (qion == 18.0) THEN
    a1=5.7618; o1=0.4709; a2=12.2382; o2=6.4618
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Cl')                                                                                          
   IF (qion == 14.0) THEN
    a1=2.7766; o1=0.7524; a2=11.2234; o2=11.8885
   ELSE IF (qion == 15.0) THEN
    a1=3.6551; o1=0.7443; a2=11.3449; o2=11.6315
   ELSE IF (qion == 16.0) THEN
    a1=4.5137; o1=0.7014; a2=11.4863; o2=11.0533
   ELSE IF (qion == 17.0) THEN
    a1=5.0208; o1=0.5876; a2=11.9792; o2=8.7396
   ELSE IF (qion == 18.0) THEN
    a1=4.3597; o1=0.3840; a2=13.6403; o2=4.3097
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ar')                                                                                          
   IF (qion == 15.0) THEN
    a1=3.3808; o1=0.9113; a2=11.6192; o2=12.9658
   ELSE IF (qion == 16.0) THEN
    a1=4.3116; o1=0.9106; a2=11.6884; o2=12.9025
   ELSE IF (qion == 17.0) THEN
    a1=5.1219; o1=0.8438; a2=11.8781; o2=12.0348
   ELSE IF (qion == 18.0) THEN
    a1=5.5830; o1=0.7136; a2=12.4170; o2=9.5878
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('K')                                                                                           
   IF (qion == 16.0) THEN
    a1=4.0408; o1=1.1771; a2=11.9592; o2=15.2897
   ELSE IF (qion == 17.0) THEN
    a1=4.9540; o1=1.1498; a2=12.0460; o2=15.0624
   ELSE IF (qion == 18.0) THEN
    a1=5.7291; o1=1.0717; a2=12.2709; o2=14.0876
   ELSE IF (qion == 19.0) THEN
    a1=1.0429; o1=0.0603; a2=17.9571; o2=2.2869
   ELSE IF (qion == 20.0) THEN
    a1=2.0406; o1=0.0735; a2=17.9594; o2=2.0961
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ca')                                                                                          
   IF (qion == 17.0) THEN
    a1=4.6979; o1=1.3467; a2=12.3021; o2=16.3816
   ELSE IF (qion == 18.0) THEN
    a1=5.5966; o1=1.3644; a2=12.4034; o2=16.8334
   ELSE IF (qion == 19.0) THEN
    a1=1.0879; o1=0.1217; a2=17.9121; o2=2.9780
   ELSE IF (qion == 20.0) THEN
    a1=2.0345; o1=0.1137; a2=17.9655; o2=2.9077
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Sc')                                                                                          
   IF (qion == 18.0) THEN
    a1=5.3554; o1=1.5486; a2=12.6446; o2=17.8124
   ELSE IF (qion == 19.0) THEN
    a1=1.1062; o1=0.1870; a2=17.8938; o2=3.8376
   ELSE IF (qion == 20.0) THEN
    a1=2.0831; o1=0.1883; a2=17.9169; o2=3.8058
   ELSE IF (qion == 21.0) THEN
    a1=2.1582; o1=0.1336; a2=18.8418; o2=3.0215
   ELSE IF (qion == 22.0) THEN
    a1=2.2944; o1=0.1323; a2=19.7056; o2=2.6705
   ELSE IF (qion == 23.0) THEN
    a1=2.4263; o1=0.1360; a2=20.5737; o2=2.4972
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ti')                                                                                          
   IF (qion == 19.0) THEN
    a1=1.1103; o1=0.2523; a2=17.8897; o2=4.7438
   ELSE IF (qion == 20.0) THEN
    a1=2.0655; o1=0.2607; a2=17.9345; o2=4.7243
   ELSE IF (qion == 21.0) THEN
    a1=1.3256; o1=0.1756; a2=19.6744; o2=3.4516
   ELSE IF (qion == 22.0) THEN
    a1=2.1944; o1=0.1481; a2=19.8056; o2=3.2022
   ELSE IF (qion == 23.0) THEN
    a1=2.2890; o1=0.1461; a2=20.7110; o2=2.8977
   ELSE IF (qion == 24.0) THEN
    a1=2.3807; o1=0.1494; a2=21.6193; o2=2.7536
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('V')
   IF (qion == 20.0) THEN
    a1=2.0251; o1=0.3338; a2=17.9749; o2=5.7168
   ELSE IF (qion == 21.0) THEN
    a1=2.0861; o1=0.2838; a2=18.9139; o2=5.0064
   ELSE IF (qion == 22.0) THEN
    a1=1.3656; o1=0.1974; a2=20.6344; o2=3.7291
   ELSE IF (qion == 23.0) THEN
    a1=2.1868; o1=0.1599; a2=20.8132; o2=3.4139
   ELSE IF (qion == 24.0) THEN
    a1=2.2518; o1=0.1562; a2=21.7482; o2=3.1158
   ELSE IF (qion == 25.0) THEN
    a1=2.3150; o1=0.1590; a2=22.6850; o2=2.9898
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Cr')
   IF (qion == 21.0) THEN
    a1=1.1549; o1=0.2732; a2=19.8451; o2=4.8373
   ELSE IF (qion == 22.0) THEN
    a1=1.2406; o1=0.2354; a2=20.7594; o2=4.2194
   ELSE IF (qion == 23.0) THEN
    a1=1.3825; o1=0.1944; a2=21.6175; o2=3.6066
   ELSE IF (qion == 24.0) THEN
    a1=1.5333; o1=0.1422; a2=22.4667; o2=2.9290
   ELSE IF (qion == 25.0) THEN
    a1=2.2098; o1=0.1647; a2=22.7902; o2=3.3468
   ELSE IF (qion == 26.0) THEN
    a1=2.2506; o1=0.1671; a2=23.7494; o2=3.2431
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Mn')
   IF (qion == 22.0) THEN
    a1=2.0292; o1=0.3865; a2=19.9708; o2=6.4832
   ELSE IF (qion == 23.0) THEN
    a1=2.1011; o1=0.3305; a2=20.8989; o2=5.7337
   ELSE IF (qion == 24.0) THEN
    a1=1.3865; o1=0.2375; a2=22.6135; o2=4.3845
   ELSE IF (qion == 25.0) THEN
    a1=2.1365; o1=0.1808; a2=22.8635; o2=3.9456
   ELSE IF (qion == 26.0) THEN
    a1=2.1669; o1=0.1724; a2=23.8331; o2=3.6032
   ELSE IF (qion == 27.0) THEN
    a1=2.1967; o1=0.1743; a2=24.8033; o2=3.5090
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Fe')
   IF (qion == 23.0) THEN
    a1=2.0271; o1=0.4054; a2=20.9729; o2=6.7802
   ELSE IF (qion == 24.0) THEN
    a1=2.1004; o1=0.3469; a2=21.8996; o2=6.0174
   ELSE IF (qion == 25.0) THEN
    a1=1.4022; o1=0.2521; a2=23.5978; o2=4.6337
   ELSE IF (qion == 26.0) THEN
    a1=2.1353; o1=0.1902; a2=23.8647; o2=4.1427
   ELSE IF (qion == 27.0) THEN
    a1=2.1621; o1=0.1827; a2=24.8379; o2=3.8366
   ELSE IF (qion == 28.0) THEN
    a1=2.1884; o1=0.1845; a2=25.8116; o2=3.7512
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Co')
   IF (qion == 24.0) THEN
    a1=2.0296; o1=0.4293; a2=21.9704; o2=7.2467
   ELSE IF (qion == 25.0) THEN
    a1=2.1036; o1=0.3675; a2=22.8964; o2=6.4570
   ELSE IF (qion == 26.0) THEN
    a1=1.4124; o1=0.2685; a2=24.5876; o2=4.9791
   ELSE IF (qion == 27.0) THEN
    a1=2.1348; o1=0.2014; a2=24.8652; o2=4.4716
   ELSE IF (qion == 28.0) THEN
    a1=2.1584; o1=0.1925; a2=25.8416; o2=4.1409
   ELSE IF (qion == 29.0) THEN
    a1=2.1817; o1=0.1942; a2=26.8183; o2=4.0589
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ni')
   IF (qion == 25.0) THEN
    a1=2.0291; o1=0.4536; a2=22.9709; o2=7.7181
   ELSE IF (qion == 26.0) THEN
    a1=2.1117; o1=0.3898; a2=23.8883; o2=6.8741
   ELSE IF (qion == 27.0) THEN
    a1=1.4139; o1=0.2839; a2=25.5861; o2=5.3297
   ELSE IF (qion == 28.0) THEN
    a1=2.1268; o1=0.2121; a2=25.8732; o2=4.7850
   ELSE IF (qion == 29.0) THEN
    a1=2.1475; o1=0.2017; a2=26.8525; o2=4.4247
   ELSE IF (qion == 30.0) THEN
    a1=2.1679; o1=0.2032; a2=27.8321; o2=4.3482
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Cu')
   IF (qion == 26.0) THEN
    a1=1.2071; o1=0.4190; a2=24.7929; o2=7.2679
   ELSE IF (qion == 27.0) THEN
    a1=1.3095; o1=0.3627; a2=25.6905; o2=6.4322
   ELSE IF (qion == 28.0) THEN
    a1=1.4125; o1=0.2909; a2=26.5875; o2=5.5337
   ELSE IF (qion == 29.0) THEN
    a1=1.3938; o1=0.1861; a2=27.6062; o2=4.3864
   ELSE IF (qion == 30.0) THEN
    a1=2.1360; o1=0.2109; a2=27.8640; o2=4.7195
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Zn')
   IF (qion == 27.0) THEN
    a1=2.0388; o1=0.5153; a2=24.9612; o2=8.8483
   ELSE IF (qion == 28.0) THEN
    a1=2.1232; o1=0.4429; a2=25.8768; o2=7.9150
   ELSE IF (qion == 29.0) THEN
    a1=2.1806; o1=0.3545; a2=26.8194; o2=6.8787
   ELSE IF (qion == 30.0) THEN
    a1=2.1031; o1=0.2368; a2=27.8969; o2=5.5499
   ELSE IF (qion == 31.0) THEN
    a1=0.7625; o1=0.0305; a2=30.2375; o2=1.0822
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ga')
   IF (qion == 28.0) THEN
    a1=2.1617; o1=0.3831; a2=25.8383; o2=7.1449
   ELSE IF (qion == 29.0) THEN
    a1=2.1712; o1=0.3839; a2=26.8288; o2=7.3601
   ELSE IF (qion == 30.0) THEN
    a1=2.1803; o1=0.3847; a2=27.8197; o2=7.5738
   ELSE IF (qion == 31.0) THEN
    a1=2.3789; o1=0.2156; a2=28.6211; o2=5.6285
   ELSE IF (qion == 32.0) THEN
    a1=3.0888; o1=0.1742; a2=28.9112; o2=4.5182
   ELSE IF (qion == 33.0) THEN
    a1=3.8472; o1=0.1704; a2=29.1528; o2=4.2373
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ge')
   IF (qion == 29.0) THEN
    a1=2.1218; o1=0.4922; a2=26.8782; o2=8.7545
   ELSE IF (qion == 30.0) THEN
    a1=2.1322; o1=0.4933; a2=27.8678; o2=9.0081
   ELSE IF (qion == 31.0) THEN
    a1=2.9125; o1=0.4015; a2=28.0875; o2=8.4117
   ELSE IF (qion == 32.0) THEN
    a1=3.3873; o1=0.2924; a2=28.6127; o2=7.0292
   ELSE IF (qion == 33.0) THEN
    a1=4.1616; o1=0.2580; a2=28.8384; o2=5.9974
   ELSE IF (qion == 34.0) THEN
    a1=4.8953; o1=0.2686; a2=29.1047; o2=5.7419
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('As')
   IF (qion == 30.0) THEN
    a1=2.0543; o1=0.6006; a2=27.9457; o2=10.5906
   ELSE IF (qion == 31.0) THEN
    a1=2.9466; o1=0.5523; a2=28.0534; o2=10.2779
   ELSE IF (qion == 32.0) THEN
    a1=3.7618; o1=0.4827; a2=28.2382; o2=9.7484
   ELSE IF (qion == 33.0) THEN
    a1=4.3165; o1=0.3845; a2=28.6835; o2=8.5002
   ELSE IF (qion == 34.0) THEN
    a1=5.0713; o1=0.3583; a2=28.9287; o2=7.3464
   ELSE IF (qion == 35.0) THEN
    a1=5.8168; o1=0.3664; a2=29.1832; o2=7.0683
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Se')
   IF (qion == 31.0) THEN
    a1=2.8693; o1=0.6451; a2=28.1307; o2=11.2188
   ELSE IF (qion == 32.0) THEN
    a1=3.7830; o1=0.6093; a2=28.2170; o2=10.9803
   ELSE IF (qion == 33.0) THEN
    a1=4.6076; o1=0.5462; a2=28.3924; o2=10.4845
   ELSE IF (qion == 34.0) THEN
    a1=5.0009; o1=0.4400; a2=28.9991; o2=8.9124
   ELSE IF (qion == 35.0) THEN
    a1=5.7322; o1=0.4280; a2=29.2678; o2=8.1621
   ELSE IF (qion == 36.0) THEN
    a1=6.4559; o1=0.4342; a2=29.5441; o2=7.8481
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Br')
   IF (qion == 32.0) THEN
    a1=3.6970; o1=0.7371; a2=28.3030; o2=12.3380
   ELSE IF (qion == 33.0) THEN
    a1=4.6205; o1=0.7002; a2=28.3795; o2=12.1240
   ELSE IF (qion == 34.0) THEN
    a1=5.3694; o1=0.6324; a2=28.6306; o2=11.4591
   ELSE IF (qion == 35.0) THEN
    a1=5.7315; o1=0.5188; a2=29.2685; o2=9.7717
   ELSE IF (qion == 36.0) THEN
    a1=5.1091; o1=0.3486; a2=30.8909; o2=6.3150
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Kr')
   IF (qion == 33.0) THEN
    a1=4.5323; o1=0.8684; a2=28.4677; o2=13.9859
   ELSE IF (qion == 34.0) THEN
    a1=5.4326; o1=0.8304; a2=28.5674; o2=13.7366
   ELSE IF (qion == 35.0) THEN
    a1=6.1400; o1=0.7480; a2=28.8600; o2=12.8988
   ELSE IF (qion == 36.0) THEN
    a1=6.4911; o1=0.6261; a2=29.5089; o2=11.0857
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Rb')
   IF (qion == 34.0) THEN
    a1=5.3733; o1=1.1094; a2=28.6267; o2=17.1217
   ELSE IF (qion == 35.0) THEN
    a1=6.2393; o1=1.0488; a2=28.7607; o2=16.7090
   ELSE IF (qion == 36.0) THEN
    a1=6.9204; o1=0.9190; a2=29.0796; o2=15.1742
   ELSE IF (qion == 37.0) THEN
    a1=1.1214; o1=0.0598; a2=35.8786; o2=3.0336
   ELSE IF (qion == 38.0) THEN
    a1=2.1777; o1=0.0688; a2=35.8223; o2=2.7150
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Sr')
   IF (qion == 35.0) THEN
    a1=6.2048; o1=1.1908; a2=28.7952; o2=17.7901
   ELSE IF (qion == 36.0) THEN
    a1=7.0465; o1=1.1396; a2=28.9535; o2=17.5745
   ELSE IF (qion == 37.0) THEN
    a1=1.2836; o1=0.1209; a2=35.7164; o2=3.8511
   ELSE IF (qion == 38.0) THEN
    a1=2.2204; o1=0.1066; a2=35.7796; o2=3.7041
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Y')
   IF (qion == 36.0) THEN
    a1=7.0336; o1=1.2537; a2=28.9664; o2=18.2512
   ELSE IF (qion == 37.0) THEN
    a1=1.4135; o1=0.1759; a2=35.5865; o2=4.5537
   ELSE IF (qion == 38.0) THEN
    a1=2.4068; o1=0.1670; a2=35.5932; o2=4.4986
   ELSE IF (qion == 39.0) THEN
    a1=2.5170; o1=0.1254; a2=36.4830; o2=3.5614
   ELSE IF (qion == 40.0) THEN
    a1=2.8007; o1=0.1279; a2=37.1993; o2=3.1721
   ELSE IF (qion == 41.0) THEN
    a1=3.0808; o1=0.1320; a2=37.9192; o2=2.9202
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Zr')
   IF (qion == 37.0) THEN
    a1=1.5169; o1=0.2386; a2=35.4831; o2=5.4867
   ELSE IF (qion == 38.0) THEN
    a1=2.5089; o1=0.2303; a2=35.4911; o2=5.4438
   ELSE IF (qion == 39.0) THEN
    a1=1.9831; o1=0.1821; a2=37.0169; o2=4.0671
   ELSE IF (qion == 40.0) THEN
    a1=1.9616; o1=0.1344; a2=38.0384; o2=3.3339
   ELSE IF (qion == 41.0) THEN
    a1=2.9765; o1=0.1506; a2=38.0235; o2=3.4179
   ELSE IF (qion == 42.0) THEN
    a1=3.2259; o1=0.1563; a2=38.7741; o2=3.2150
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Nb')
   IF (qion == 38.0) THEN
    a1=2.5693; o1=0.2814; a2=35.4307; o2=6.1522
   ELSE IF (qion == 39.0) THEN
    a1=2.7012; o1=0.2484; a2=36.2988; o2=5.3211
   ELSE IF (qion == 40.0) THEN
    a1=2.1691; o1=0.2023; a2=37.8309; o2=4.0835
   ELSE IF (qion == 41.0) THEN
    a1=2.0105; o1=0.1459; a2=38.9895; o2=3.3469
   ELSE IF (qion == 42.0) THEN
    a1=2.9482; o1=0.1634; a2=39.0518; o2=3.5190
   ELSE IF (qion == 43.0) THEN
    a1=3.1339; o1=0.1687; a2=39.8661; o2=3.3373
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Mo')
   IF (qion == 39.0) THEN
    a1=1.9502; o1=0.3001; a2=37.0498; o2=5.5807
   ELSE IF (qion == 40.0) THEN
    a1=2.1175; o1=0.2664; a2=37.8825; o2=4.8871
   ELSE IF (qion == 41.0) THEN
    a1=2.2584; o1=0.2230; a2=38.7416; o2=4.1959
   ELSE IF (qion == 42.0) THEN
    a1=2.2510; o1=0.1640; a2=39.7490; o2=3.4143
   ELSE IF (qion == 43.0) THEN
    a1=2.8419; o1=0.1710; a2=40.1581; o2=3.5940
   ELSE IF (qion == 44.0) THEN
    a1=2.9588; o1=0.1756; a2=41.0412; o2=3.4404
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Tc')
   IF (qion == 40.0) THEN
    a1=2.8072; o1=0.3725; a2=37.1928; o2=7.0398
   ELSE IF (qion == 41.0) THEN
    a1=2.8707; o1=0.3249; a2=38.1293; o2=6.1337
   ELSE IF (qion == 42.0) THEN
    a1=2.2767; o1=0.2710; a2=39.7233; o2=4.8596
   ELSE IF (qion == 43.0) THEN
    a1=2.6221; o1=0.1842; a2=40.3779; o2=4.1380
   ELSE IF (qion == 44.0) THEN
    a1=2.7026; o1=0.1757; a2=41.2974; o2=3.6932
   ELSE IF (qion == 45.0) THEN
    a1=2.7811; o1=0.1792; a2=42.2189; o2=3.5503
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ru')
   IF (qion == 41.0) THEN
    a1=2.8568; o1=0.3975; a2=38.1432; o2=7.1600
   ELSE IF (qion == 42.0) THEN
    a1=2.8859; o1=0.3444; a2=39.1141; o2=6.2587
   ELSE IF (qion == 43.0) THEN
    a1=2.8259; o1=0.2783; a2=40.1741; o2=5.3282
   ELSE IF (qion == 44.0) THEN
    a1=2.5712; o1=0.1917; a2=41.4288; o2=4.2577
   ELSE IF (qion == 45.0) THEN
    a1=2.6325; o1=0.1820; a2=42.3675; o2=3.8186
   ELSE IF (qion == 46.0) THEN
    a1=2.6921; o1=0.1849; a2=43.3079; o2=3.6858
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Rh')
   IF (qion == 42.0) THEN
    a1=2.8751; o1=0.4198; a2=39.1249; o2=7.2960
   ELSE IF (qion == 43.0) THEN
    a1=2.8732; o1=0.3615; a2=40.1268; o2=6.3989
   ELSE IF (qion == 44.0) THEN
    a1=2.8006; o1=0.2912; a2=41.1994; o2=5.4747
   ELSE IF (qion == 45.0) THEN
    a1=2.5098; o1=0.1977; a2=42.4902; o2=4.3922
   ELSE IF (qion == 46.0) THEN
    a1=2.5560; o1=0.1870; a2=43.4440; o2=3.9562
   ELSE IF (qion == 47.0) THEN
    a1=2.6006; o1=0.1895; a2=44.3994; o2=3.8326
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Pd')
   IF (qion == 43.0) THEN
    a1=2.8709; o1=0.4399; a2=40.1291; o2=7.4499
   ELSE IF (qion == 44.0) THEN
    a1=2.8601; o1=0.3783; a2=41.1399; o2=6.5635
   ELSE IF (qion == 45.0) THEN
    a1=2.7614; o1=0.3026; a2=42.2386; o2=5.6348
   ELSE IF (qion == 46.0) THEN
    a1=2.4453; o1=0.2026; a2=43.5547; o2=4.5428
   ELSE IF (qion == 47.0) THEN
    a1=2.4796; o1=0.1912; a2=44.5204; o2=4.1073
   ELSE IF (qion == 48.0) THEN
    a1=2.5129; o1=0.1932; a2=45.4871; o2=3.9920
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ag')
   IF (qion == 44.0) THEN
    a1=2.1964; o1=0.5091; a2=41.8036; o2=7.7447
   ELSE IF (qion == 45.0) THEN
    a1=2.1549; o1=0.4289; a2=42.8451; o2=6.8055
   ELSE IF (qion == 46.0) THEN
    a1=1.9861; o1=0.3264; a2=44.0139; o2=5.7788
   ELSE IF (qion == 47.0) THEN
    a1=1.5368; o1=0.1870; a2=45.4632; o2=4.5003
   ELSE IF (qion == 48.0) THEN
    a1=2.4075; o1=0.1947; a2=45.5925; o2=4.2724
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Cd')
   IF (qion == 45.0) THEN
    a1=2.8436; o1=0.5157; a2=42.1564; o2=8.4402
   ELSE IF (qion == 46.0) THEN
    a1=2.7994; o1=0.4398; a2=43.2006; o2=7.4862
   ELSE IF (qion == 47.0) THEN
    a1=2.6628; o1=0.3473; a2=44.3372; o2=6.4754
   ELSE IF (qion == 48.0) THEN
    a1=2.3227; o1=0.2270; a2=45.6773; o2=5.2807
   ELSE IF (qion == 49.0) THEN
    a1=0.9612; o1=0.0338; a2=48.0388; o2=1.5017
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('In')
   IF (qion == 46.0) THEN
    a1=2.8163; o1=0.5513; a2=43.1837; o2=8.9429
   ELSE IF (qion == 47.0) THEN
    a1=2.7589; o1=0.4684; a2=44.2411; o2=7.9589
   ELSE IF (qion == 48.0) THEN
    a1=2.6100; o1=0.3678; a2=45.3900; o2=6.9107
   ELSE IF (qion == 49.0) THEN
    a1=2.6699; o1=0.2099; a2=46.3301; o2=5.5424
   ELSE IF (qion == 50.0) THEN
    a1=3.4077; o1=0.1614; a2=46.5923; o2=4.3773
   ELSE IF (qion == 51.0) THEN
    a1=4.2055; o1=0.1578; a2=46.7945; o2=4.2133
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Sn')
   IF (qion == 47.0) THEN
    a1=2.7841; o1=0.5166; a2=44.2159; o2=8.3437
   ELSE IF (qion == 48.0) THEN
    a1=2.7163; o1=0.4374; a2=45.2837; o2=7.4499
   ELSE IF (qion == 49.0) THEN
    a1=3.3356; o1=0.3438; a2=45.6644; o2=6.8874
   ELSE IF (qion == 50.0) THEN
    a1=3.7426; o1=0.2524; a2=46.2574; o2=6.0360
   ELSE IF (qion == 51.0) THEN
    a1=4.5586; o1=0.2283; a2=46.4414; o2=5.4056
   ELSE IF (qion == 52.0) THEN
    a1=5.3569; o1=0.2386; a2=46.6431; o2=5.3028
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Sb')
   IF (qion == 48.0) THEN
    a1=2.7486; o1=0.5496; a2=45.2514; o2=8.8727
   ELSE IF (qion == 49.0) THEN
    a1=3.5416; o1=0.4836; a2=45.4584; o2=8.5046
   ELSE IF (qion == 50.0) THEN
    a1=4.2698; o1=0.4150; a2=45.7302; o2=8.0546
   ELSE IF (qion == 51.0) THEN
    a1=4.7705; o1=0.3326; a2=46.2295; o2=7.2738
   ELSE IF (qion == 52.0) THEN
    a1=5.5973; o1=0.3093; a2=46.4027; o2=6.4009
   ELSE IF (qion == 53.0) THEN
    a1=6.4069; o1=0.3170; a2=46.5931; o2=6.2891
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Te')
   IF (qion == 49.0) THEN
    a1=3.6097; o1=0.5666; a2=45.3903; o2=9.2444
   ELSE IF (qion == 50.0) THEN
    a1=4.4688; o1=0.5214; a2=45.5312; o2=8.9842
   ELSE IF (qion == 51.0) THEN
    a1=5.2253; o1=0.4621; a2=45.7747; o2=8.5797
   ELSE IF (qion == 52.0) THEN
    a1=5.5653; o1=0.3768; a2=46.4347; o2=7.6354
   ELSE IF (qion == 53.0) THEN
    a1=6.3646; o1=0.3635; a2=46.6354; o2=7.0701
   ELSE IF (qion == 54.0) THEN
    a1=7.1540; o1=0.3693; a2=46.8460; o2=6.9326
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('I')
   IF (qion == 50.0) THEN
    a1=4.5436; o1=0.6338; a2=45.4564; o2=10.1325
   ELSE IF (qion == 51.0) THEN
    a1=5.4206; o1=0.5909; a2=45.5794; o2=9.8935
   ELSE IF (qion == 52.0) THEN
    a1=6.1056; o1=0.5318; a2=45.8944; o2=9.4128
   ELSE IF (qion == 53.0) THEN
    a1=6.4123; o1=0.4412; a2=46.5877; o2=8.3802
   ELSE IF (qion == 54.0) THEN
    a1=5.8609; o1=0.3114; a2=48.1391; o2=6.2987
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Xe')
   IF (qion == 51.0) THEN
    a1=5.5036; o1=0.7274; a2=45.4964; o2=11.3585
   ELSE IF (qion == 52.0) THEN
    a1=6.3624; o1=0.6880; a2=45.6376; o2=11.1200
   ELSE IF (qion == 53.0) THEN
    a1=6.9992; o1=0.6185; a2=46.0008; o2=10.5190
   ELSE IF (qion == 54.0) THEN
    a1=7.2968; o1=0.5230; a2=46.7032; o2=9.4043
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Cs')
   IF (qion == 52.0) THEN
    a1=6.4954; o1=0.9739; a2=45.5046; o2=14.8134
   ELSE IF (qion == 53.0) THEN
    a1=7.3059; o1=0.9131; a2=45.6941; o2=14.4205
   ELSE IF (qion == 54.0) THEN
    a1=7.9165; o1=0.7342; a2=46.0835; o2=12.1313
   ELSE IF (qion == 55.0) THEN
    a1=1.1667; o1=0.0546; a2=53.8333; o2=3.2491
   ELSE IF (qion == 56.0) THEN
    a1=2.2659; o1=0.0591; a2=53.7341; o2=2.7274
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ba')
   IF (qion == 53.0) THEN
    a1=7.4671; o1=0.9768; a2=45.5329; o2=14.5625
   ELSE IF (qion == 54.0) THEN
    a1=8.2468; o1=0.9200; a2=45.7532; o2=14.2144
   ELSE IF (qion == 55.0) THEN
    a1=1.4022; o1=0.1091; a2=53.5978; o2=3.9825
   ELSE IF (qion == 56.0) THEN
    a1=2.3372; o1=0.0923; a2=53.6628; o2=3.6990
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('La')
   IF (qion == 54.0) THEN
    a1=8.4338; o1=1.1924; a2=45.5662; o2=17.5054
   ELSE IF (qion == 55.0) THEN
    a1=1.6129; o1=0.1795; a2=53.3871; o2=5.2051
   ELSE IF (qion == 56.0) THEN
    a1=2.6011; o1=0.1680; a2=53.3989; o2=5.1362
   ELSE IF (qion == 57.0) THEN
    a1=2.3275; o1=0.1051; a2=54.6725; o2=4.2194
   ELSE IF (qion == 58.0) THEN
    a1=2.3318; o1=0.0862; a2=55.6682; o2=3.4085
   ELSE IF (qion == 59.0) THEN
    a1=2.3421; o1=0.0867; a2=56.6579; o2=3.3786
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ce')
   IF (qion == 55.0) THEN
    a1=1.7948; o1=0.2475; a2=53.2052; o2=6.2406
   ELSE IF (qion == 56.0) THEN
    a1=2.7751; o1=0.2332; a2=53.2249; o2=6.1655
   ELSE IF (qion == 57.0) THEN
    a1=2.5862; o1=0.1722; a2=54.4138; o2=5.3163
   ELSE IF (qion == 58.0) THEN
    a1=2.3107; o1=0.1070; a2=55.6893; o2=4.3811
   ELSE IF (qion == 59.0) THEN
    a1=2.3132; o1=0.0872; a2=56.6868; o2=3.5328
   ELSE IF (qion == 60.0) THEN
    a1=2.3163; o1=0.0874; a2=57.6837; o2=3.5064
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Pr')
   IF (qion == 56.0) THEN
    a1=2.9000; o1=0.2837; a2=53.1000; o2=6.8375
   ELSE IF (qion == 57.0) THEN
    a1=2.7583; o1=0.2271; a2=54.2417; o2=6.0630
   ELSE IF (qion == 58.0) THEN
    a1=2.5720; o1=0.1678; a2=55.4280; o2=5.2400
   ELSE IF (qion == 59.0) THEN
    a1=2.3025; o1=0.1040; a2=56.6975; o2=4.3352
   ELSE IF (qion == 60.0) THEN
    a1=2.3050; o1=0.0885; a2=57.6950; o2=3.6625
   ELSE IF (qion == 61.0) THEN
    a1=2.3076; o1=0.0887; a2=58.6924; o2=3.6429
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Nd')
   IF (qion == 57.0) THEN
    a1=2.8832; o1=0.2889; a2=54.1168; o2=7.0314
   ELSE IF (qion == 58.0) THEN
    a1=2.7429; o1=0.2313; a2=55.2571; o2=6.2413
   ELSE IF (qion == 59.0) THEN
    a1=2.5588; o1=0.1708; a2=56.4412; o2=5.4042
   ELSE IF (qion == 60.0) THEN
    a1=2.2937; o1=0.1056; a2=57.7063; o2=4.4825
   ELSE IF (qion == 61.0) THEN
    a1=2.2956; o1=0.0898; a2=58.7044; o2=3.7899
   ELSE IF (qion == 62.0) THEN
    a1=2.2975; o1=0.0899; a2=59.7025; o2=3.7753
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Pm')
   IF (qion == 58.0) THEN
    a1=2.8679; o1=0.2950; a2=55.1321; o2=7.2626
   ELSE IF (qion == 59.0) THEN
    a1=2.7290; o1=0.2362; a2=56.2710; o2=6.4526
   ELSE IF (qion == 60.0) THEN
    a1=2.5469; o1=0.1743; a2=57.4531; o2=5.5965
   ELSE IF (qion == 61.0) THEN
    a1=2.2851; o1=0.1076; a2=58.7149; o2=4.6509
   ELSE IF (qion == 62.0) THEN
    a1=2.2864; o1=0.0910; a2=59.7136; o2=3.9182
   ELSE IF (qion == 63.0) THEN
    a1=2.2877; o1=0.0911; a2=60.7123; o2=3.9077
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Sm')
   IF (qion == 59.0) THEN
    a1=2.8542; o1=0.3016; a2=56.1458; o2=7.5094
   ELSE IF (qion == 60.0) THEN
    a1=2.7165; o1=0.2413; a2=57.2835; o2=6.6781
   ELSE IF (qion == 61.0) THEN
    a1=2.5361; o1=0.1780; a2=58.4639; o2=5.8004
   ELSE IF (qion == 62.0) THEN
    a1=2.2770; o1=0.1096; a2=59.7230; o2=4.8278
   ELSE IF (qion == 63.0) THEN
    a1=2.2780; o1=0.0922; a2=60.7220; o2=4.0487
   ELSE IF (qion == 64.0) THEN
    a1=2.2789; o1=0.0923; a2=61.7211; o2=4.0433
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Eu')
   IF (qion == 60.0) THEN
    a1=2.8418; o1=0.3083; a2=57.1582; o2=7.7706
   ELSE IF (qion == 61.0) THEN
    a1=2.7052; o1=0.2466; a2=58.2948; o2=6.9166
   ELSE IF (qion == 62.0) THEN
    a1=2.5266; o1=0.1818; a2=59.4734; o2=6.0156
   ELSE IF (qion == 63.0) THEN
    a1=2.2697; o1=0.1117; a2=60.7303; o2=5.0134
   ELSE IF (qion == 64.0) THEN
    a1=2.2703; o1=0.0934; a2=61.7297; o2=4.1829
   ELSE IF (qion == 65.0) THEN
    a1=2.2710; o1=0.0934; a2=62.7290; o2=4.1804
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Gd')
   IF (qion == 61.0) THEN
    a1=2.8307; o1=0.3319; a2=58.1693; o2=8.4743
   ELSE IF (qion == 62.0) THEN
    a1=2.6951; o1=0.2655; a2=59.3049; o2=7.5494
   ELSE IF (qion == 63.0) THEN
    a1=2.5179; o1=0.1956; a2=60.4821; o2=6.5735
   ELSE IF (qion == 64.0) THEN
    a1=2.2654; o1=0.1204; a2=61.7346; o2=5.4808
   ELSE IF (qion == 65.0) THEN
    a1=2.2659; o1=0.0948; a2=62.7341; o2=4.3140
   ELSE IF (qion == 66.0) THEN
    a1=2.2664; o1=0.0949; a2=63.7336; o2=4.3137
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Tb')
   IF (qion == 62.0) THEN
    a1=2.8206; o1=0.3206; a2=59.1794; o2=8.2893
   ELSE IF (qion == 63.0) THEN
    a1=2.6860; o1=0.2563; a2=60.3140; o2=7.3904
   ELSE IF (qion == 64.0) THEN
    a1=2.5118; o1=0.1890; a2=61.4882; o2=6.4380
   ELSE IF (qion == 65.0) THEN
    a1=2.2611; o1=0.1162; a2=62.7389; o2=5.3730
   ELSE IF (qion == 66.0) THEN
    a1=2.2616; o1=0.0962; a2=63.7384; o2=4.4476
   ELSE IF (qion == 67.0) THEN
    a1=2.2621; o1=0.0963; a2=64.7379; o2=4.4494
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Dy')
   IF (qion == 63.0) THEN
    a1=2.8116; o1=0.3273; a2=60.1884; o2=8.5735
   ELSE IF (qion == 64.0) THEN
    a1=2.6788; o1=0.2619; a2=61.3212; o2=7.6451
   ELSE IF (qion == 65.0) THEN
    a1=2.5061; o1=0.1931; a2=62.4939; o2=6.6664
   ELSE IF (qion == 66.0) THEN
    a1=2.2570; o1=0.1185; a2=63.7430; o2=5.5684
   ELSE IF (qion == 67.0) THEN
    a1=2.2574; o1=0.0976; a2=64.7426; o2=4.5835
   ELSE IF (qion == 68.0) THEN
    a1=2.2579; o1=0.0976; a2=65.7421; o2=4.5871
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ho')
   IF (qion == 64.0) THEN
    a1=2.8040; o1=0.3342; a2=61.1960; o2=8.8596
   ELSE IF (qion == 65.0) THEN
    a1=2.6722; o1=0.2674; a2=62.3278; o2=7.9054
   ELSE IF (qion == 66.0) THEN
    a1=2.5008; o1=0.1970; a2=63.4992; o2=6.8996
   ELSE IF (qion == 67.0) THEN
    a1=2.2531; o1=0.1208; a2=64.7469; o2=5.7681
   ELSE IF (qion == 68.0) THEN
    a1=2.2535; o1=0.0989; a2=65.7465; o2=4.7218
   ELSE IF (qion == 69.0) THEN
    a1=2.2539; o1=0.0989; a2=66.7461; o2=4.7270
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Er')
   IF (qion == 65.0) THEN
    a1=2.7969; o1=0.3412; a2=62.2031; o2=9.1533
   ELSE IF (qion == 66.0) THEN
    a1=2.6662; o1=0.2730; a2=63.3338; o2=8.1729
   ELSE IF (qion == 67.0) THEN
    a1=2.4958; o1=0.2010; a2=64.5042; o2=7.1389
   ELSE IF (qion == 68.0) THEN
    a1=2.2493; o1=0.1231; a2=65.7507; o2=5.9726
   ELSE IF (qion == 69.0) THEN
    a1=2.2497; o1=0.1002; a2=66.7503; o2=4.8619
   ELSE IF (qion == 70.0) THEN
    a1=2.2501; o1=0.1002; a2=67.7499; o2=4.8688
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Tm')
   IF (qion == 66.0) THEN
    a1=2.7905; o1=0.3483; a2=63.2095; o2=9.4578
   ELSE IF (qion == 67.0) THEN
    a1=2.6606; o1=0.2786; a2=64.3394; o2=8.4503
   ELSE IF (qion == 68.0) THEN
    a1=2.4913; o1=0.2051; a2=65.5087; o2=7.3869
   ELSE IF (qion == 69.0) THEN
    a1=2.2457; o1=0.1255; a2=66.7543; o2=6.1840
   ELSE IF (qion == 70.0) THEN
    a1=2.2461; o1=0.1014; a2=67.7539; o2=5.0042
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Yb')
   IF (qion == 67.0) THEN
    a1=2.7847; o1=0.3553; a2=64.2153; o2=9.7631
   ELSE IF (qion == 68.0) THEN
    a1=2.6555; o1=0.2840; a2=65.3445; o2=8.7281
   ELSE IF (qion == 69.0) THEN
    a1=2.4871; o1=0.2091; a2=66.5129; o2=7.6354
   ELSE IF (qion == 70.0) THEN
    a1=2.2424; o1=0.1278; a2=67.7576; o2=6.3961
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Lu')
   IF (qion == 68.0) THEN
    a1=2.7794; o1=0.3133; a2=65.2206; o2=8.7121
   ELSE IF (qion == 69.0) THEN
    a1=2.6508; o1=0.2504; a2=66.3492; o2=7.7930
   ELSE IF (qion == 70.0) THEN
    a1=2.4832; o1=0.1842; a2=67.5168; o2=6.8219
   ELSE IF (qion == 71.0) THEN
    a1=2.5665; o1=0.1379; a2=68.4335; o2=5.6125
   ELSE IF (qion == 72.0) THEN
    a1=2.8631; o1=0.1305; a2=69.1369; o2=4.7416
   ELSE IF (qion == 73.0) THEN
    a1=3.1699; o1=0.1340; a2=69.8301; o2=4.4029
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Hf')
   IF (qion == 69.0) THEN
    a1=2.7746; o1=0.3633; a2=66.2254; o2=10.2254
   ELSE IF (qion == 70.0) THEN
    a1=2.6467; o1=0.2903; a2=67.3533; o2=9.1515
   ELSE IF (qion == 71.0) THEN
    a1=2.8526; o1=0.2478; a2=68.1474; o2=8.0926
   ELSE IF (qion == 72.0) THEN
    a1=2.9254; o1=0.1886; a2=69.0746; o2=6.7894
   ELSE IF (qion == 73.0) THEN
    a1=3.2523; o1=0.1594; a2=69.7477; o2=5.1754
   ELSE IF (qion == 74.0) THEN
    a1=3.5785; o1=0.1659; a2=70.4215; o2=4.9179
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ta')
   IF (qion == 70.0) THEN
    a1=2.7701; o1=0.3633; a2=67.2299; o2=10.3507
   ELSE IF (qion == 71.0) THEN
    a1=2.9651; o1=0.3255; a2=68.0349; o2=9.3122
   ELSE IF (qion == 72.0) THEN
    a1=3.1148; o1=0.2756; a2=68.8852; o2=8.2033
   ELSE IF (qion == 73.0) THEN
    a1=3.0825; o1=0.2064; a2=69.9175; o2=6.8326
   ELSE IF (qion == 74.0) THEN
    a1=3.3633; o1=0.1764; a2=70.6367; o2=5.3185
   ELSE IF (qion == 75.0) THEN
    a1=3.6443; o1=0.1834; a2=71.3557; o2=5.0869
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('W')
   IF (qion == 71.0) THEN
    a1=3.0474; o1=0.4043; a2=67.9526; o2=10.6188
   ELSE IF (qion == 72.0) THEN
    a1=3.2065; o1=0.3599; a2=68.7935; o2=9.5395
   ELSE IF (qion == 73.0) THEN
    a1=3.2849; o1=0.3015; a2=69.7151; o2=8.3582
   ELSE IF (qion == 74.0) THEN
    a1=3.1142; o1=0.2200; a2=70.8858; o2=6.8652
   ELSE IF (qion == 75.0) THEN
    a1=3.3385; o1=0.1868; a2=71.6615; o2=5.3621
   ELSE IF (qion == 76.0) THEN
    a1=3.5453; o1=0.1937; a2=72.4547; o2=5.1587
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Re')
   IF (qion == 72.0) THEN
    a1=3.2683; o1=0.4415; a2=68.7317; o2=10.8470
   ELSE IF (qion == 73.0) THEN
    a1=3.3821; o1=0.3902; a2=69.6179; o2=9.7230
   ELSE IF (qion == 74.0) THEN
    a1=3.3812; o1=0.3231; a2=70.6188; o2=8.4704
   ELSE IF (qion == 75.0) THEN
    a1=3.0710; o1=0.2287; a2=71.9290; o2=6.8655
   ELSE IF (qion == 76.0) THEN
    a1=3.2274; o1=0.1932; a2=72.7726; o2=5.3922
   ELSE IF (qion == 77.0) THEN
    a1=3.3838; o1=0.1989; a2=73.6162; o2=5.1944
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Os')
   IF (qion == 73.0) THEN
    a1=3.4393; o1=0.4751; a2=69.5607; o2=11.0421
   ELSE IF (qion == 74.0) THEN
    a1=3.5017; o1=0.4168; a2=70.4983; o2=9.8737
   ELSE IF (qion == 75.0) THEN
    a1=3.4201; o1=0.3409; a2=71.5799; o2=8.5578
   ELSE IF (qion == 76.0) THEN
    a1=3.0448; o1=0.2380; a2=72.9552; o2=6.9215
   ELSE IF (qion == 77.0) THEN
    a1=3.1725; o1=0.2000; a2=73.8275; o2=5.4492
   ELSE IF (qion == 78.0) THEN
    a1=3.2995; o1=0.2049; a2=74.7005; o2=5.2596
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Ir')
   IF (qion == 74.0) THEN
    a1=3.5658; o1=0.5056; a2=70.4342; o2=11.2105
   ELSE IF (qion == 75.0) THEN
    a1=3.5744; o1=0.4402; a2=71.4256; o2=10.0017
   ELSE IF (qion == 76.0) THEN
    a1=3.4493; o1=0.3578; a2=72.5507; o2=8.6660
   ELSE IF (qion == 77.0) THEN
    a1=2.9899; o1=0.2450; a2=74.0101; o2=6.9803
   ELSE IF (qion == 78.0) THEN
    a1=3.0922; o1=0.2050; a2=74.9078; o2=5.5085
   ELSE IF (qion == 79.0) THEN
    a1=3.1935; o1=0.2092; a2=75.8065; o2=5.3275
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Pt')
   IF (qion == 75.0) THEN
    a1=3.6534; o1=0.5332; a2=71.3466; o2=11.3596
   ELSE IF (qion == 76.0) THEN
    a1=3.6308; o1=0.4626; a2=72.3692; o2=10.1408
   ELSE IF (qion == 77.0) THEN
    a1=3.4451; o1=0.3723; a2=73.5549; o2=8.7657
   ELSE IF (qion == 78.0) THEN
    a1=2.9170; o1=0.2504; a2=75.0830; o2=7.0486
   ELSE IF (qion == 79.0) THEN
    a1=2.9977; o1=0.2087; a2=76.0023; o2=5.5756
   ELSE IF (qion == 80.0) THEN
    a1=3.0774; o1=0.2123; a2=76.9226; o2=5.4031
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Au')
   IF (qion == 76.0) THEN
    a1=3.7229; o1=0.5596; a2=72.2771; o2=11.5163
   ELSE IF (qion == 77.0) THEN
    a1=3.6563; o1=0.4825; a2=73.3437; o2=10.2689
   ELSE IF (qion == 78.0) THEN
    a1=3.4151; o1=0.3847; a2=74.5849; o2=8.8641
   ELSE IF (qion == 79.0) THEN
    a1=2.8348; o1=0.2542; a2=76.1652; o2=7.1313
   ELSE IF (qion == 80.0) THEN
    a1=2.8978; o1=0.2113; a2=77.1022; o2=5.6538
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Hg')
   IF (qion == 77.0) THEN
    a1=3.7651; o1=0.6622; a2=73.2349; o2=13.2283
   ELSE IF (qion == 78.0) THEN
    a1=3.6558; o1=0.5673; a2=74.3442; o2=11.7872
   ELSE IF (qion == 79.0) THEN
    a1=3.3660; o1=0.4481; a2=75.6340; o2=10.1709
   ELSE IF (qion == 80.0) THEN
    a1=2.7495; o1=0.2915; a2=77.2505; o2=8.2014
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Tl')
   IF (qion == 78.0) THEN
    a1=3.7832; o1=0.6767; a2=74.2168; o2=13.1862
   ELSE IF (qion == 79.0) THEN
    a1=3.6343; o1=0.5762; a2=75.3657; o2=11.7477
   ELSE IF (qion == 80.0) THEN
    a1=3.3037; o1=0.4511; a2=76.6963; o2=10.1417
   ELSE IF (qion == 81.0) THEN
    a1=2.9273; o1=0.2416; a2=78.0727; o2=7.9392
   ELSE IF (qion == 82.0) THEN
    a1=3.6409; o1=0.1601; a2=78.3591; o2=5.5160
   ELSE IF (qion == 83.0) THEN
    a1=4.4473; o1=0.1556; a2=78.5527; o2=5.3492
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Pb')
   IF (qion == 79.0) THEN
    a1=3.7811; o1=0.6599; a2=75.2189; o2=12.5949
   ELSE IF (qion == 80.0) THEN
    a1=3.5964; o1=0.5586; a2=76.4036; o2=11.2239
   ELSE IF (qion == 81.0) THEN
    a1=3.8769; o1=0.4132; a2=77.1231; o2=10.0686
   ELSE IF (qion == 82.0) THEN
    a1=4.0789; o1=0.2949; a2=77.9211; o2=8.7751
   ELSE IF (qion == 83.0) THEN
    a1=4.9009; o1=0.2225; a2=78.0991; o2=6.6473
   ELSE IF (qion == 84.0) THEN
    a1=5.7427; o1=0.2322; a2=78.2573; o2=6.5860
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Bi')
   IF (qion == 80.0) THEN
    a1=3.7621; o1=0.6436; a2=76.2379; o2=12.0795
   ELSE IF (qion == 81.0) THEN
    a1=4.3077; o1=0.5355; a2=76.6923; o2=11.3134
   ELSE IF (qion == 82.0) THEN
    a1=4.8535; o1=0.4451; a2=77.1465; o2=10.5650
   ELSE IF (qion == 83.0) THEN
    a1=5.2033; o1=0.3506; a2=77.7967; o2=9.5276
   ELSE IF (qion == 84.0) THEN
    a1=6.0759; o1=0.2982; a2=77.9241; o2=7.7567
   ELSE IF (qion == 85.0) THEN
    a1=6.9238; o1=0.3049; a2=78.0762; o2=7.6804
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Po')
   IF (qion == 81.0) THEN
    a1=4.5521; o1=0.6651; a2=76.4479; o2=12.8796
   ELSE IF (qion == 82.0) THEN
    a1=5.2684; o1=0.5921; a2=76.7316; o2=12.3560
   ELSE IF (qion == 83.0) THEN
    a1=5.8858; o1=0.5139; a2=77.1142; o2=11.6935
   ELSE IF (qion == 84.0) THEN
    a1=6.0810; o1=0.4145; a2=77.9190; o2=10.4605
   ELSE IF (qion == 85.0) THEN
    a1=6.9215; o1=0.3462; a2=78.0785; o2=8.4735
   ELSE IF (qion == 86.0) THEN
    a1=7.7497; o1=0.3510; a2=78.2503; o2=8.3702
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('At')
   IF (qion == 82.0) THEN
    a1=5.5268; o1=0.7290; a2=76.4732; o2=13.9816
   ELSE IF (qion == 83.0) THEN
    a1=6.2897; o1=0.6644; a2=76.7103; o2=13.5190
   ELSE IF (qion == 84.0) THEN
    a1=6.8420; o1=0.5887; a2=77.1580; o2=12.7918
   ELSE IF (qion == 85.0) THEN
    a1=7.0210; o1=0.4852; a2=77.9790; o2=11.4823
   ELSE IF (qion == 86.0) THEN
    a1=6.3975; o1=0.3458; a2=79.6025; o2=9.0442
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE('Rn')
   IF (qion == 83.0) THEN
    a1=6.5582; o1=0.7235; a2=76.4418; o2=13.6239
   ELSE IF (qion == 84.0) THEN
    a1=7.3085; o1=0.6723; a2=76.6915; o2=13.2297
   ELSE IF (qion == 85.0) THEN
    a1=7.8211; o1=0.5977; a2=77.1789; o2=12.4817
   ELSE IF (qion == 86.0) THEN
    a1=8.0052; o1=0.5034; a2=77.9948; o2=11.2696
   ELSE
    WRITE(*,*) 'ERROR: no model parameters for atom ', elem, ' with charge ', qion
    CALL stop_mpi()
    STOP
   ENDIF
  CASE default
   WRITE(*,*) 'ERROR: no model parameters for atom: ', elem
   CALL stop_mpi()
   STOP
  END SELECT

  RETURN
 END SUBROUTINE GetFIParam
END MODULE MyParams
