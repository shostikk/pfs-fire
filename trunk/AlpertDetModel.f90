!*********************************************************
!*****   PFS2 v2.0 fire model  subroutine library    *****
!*********************************************************
!
! VTT Building and Transport                          2003
! VTT Technical Research Centre of Finland
!
! Author:  Timo Korhonen
! Date:    3.9.2003 (version v0.0 started 18.02.2003)
! Version: v2.0 (same as v0.3, only some VB macros are changed)
!
! DISCLAIMER: This source file comes as it is. There is no waranty
!             that it should operate 'correctly'.
!
!*********************************************************
! This module (file) contains subroutine libraries for
! PFS2. These libraries (DLL) are called by the PFS2
! Excel Workbook by Visual Basic macros.
!
! File contains following subroutines:
!
! * HeatDetAlpert
!   - calculates heat detector activation time using Alpert's
!     correlations. This is used as 'the fire model' during the
!     developmnent of the PFS2 (it is fast to calculate). Input
!     related to the fire is tg (growth time) and maxRHR. This
!     routine is also use by PFS2 when NFSC2-type fire is used
!     in the OZone simulations.
!
! * HeatDetOzone
!   - calculates heat detector activation time using Alpert's
!     correlations. Input related to the fire is the FireInput
!     sheet RHR(t), i.e. FireType=1 for OZone model.
!
!*********************************************************
! BUGS FOUND
!
! * 27.05.2003: t2-growth does not end so RHR can grow larger than
!               the maximum RHR
!
! BUG FIXES
!
! * 27.05.2003: t2-growth: maximum RHR (maxRHR variable) is given to
!               the routine 'HeatDetAlpert' to limit the fire growth.
!
!*********************************************************

!*********************************************************
!***** Subroutine HeatDetAlpert Starts               *****
!*********************************************************
!
! This routine uses tg as input
!
!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: HeatDetAlpert
!DEC$ ATTRIBUTES ALIAS:'HeatDetAlpert' :: HeatDetAlpert
Subroutine HeatDetAlpert(Tamb, Tact, RTI, r_max, Hroom, Hfire, &
                         tg, maxRHR, talarm)
  Use dflib
  Use dfport
  Implicit None
  !----------------------------------------------
  ! Argument definitions
  !----------------------------------------------
  ! Input variables (by value)
  Real(8) Tamb, Tact, RTI, r_max, Hroom, Hfire, tg, maxRHR
  ! Output variables (by reference)
  Real(8) talarm
  !DEC$ ATTRIBUTES REFERENCE :: talarm

  !----------------------------------------------
  ! Local variables
  !----------------------------------------------
  Integer(4) i, itmax
  Real(8) H_alp, r, T_gas, T2_gas, T_amb, T_h
  Real(8) vir_nop, vir2_nop, h, k1, k2, t_RKauto
  Real(8) tmax, Qmax
  Real(8), Allocatable :: t(:), Q_IMC(:), Ts(:)

  !------------------------------------------------------------
  !  External Subroutines and Functions
  !------------------------------------------------------------
  H_alp = Hroom - Hfire
  T_amb = Tamb
  T_h   = Tact
  r = r_max
  t_RKauto = 0.d0

  !------------------------------------------------------------
  ! Generate the RHR(t) and time axis
  !------------------------------------------------------------
!!!  Qmax  = 100.d0*1000.d0   ! Max RHR 100 MW, in kW
  Qmax  = maxRHR   ! Max RHR in kW
  tmax  = Sqrt( (Qmax/1000.d0)*tg**2 ) + RTI*10.d0
  itmax = Int(tmax/5.d0) + 1
  Allocate(t(itmax))
  Allocate(Q_IMC(itmax))
  Allocate(Ts(itmax))
  Do i = 1, itmax
     t(i) = (i-1)*(tmax/(itmax-1))  ! Time axis in seconds
     Q_IMC(i) = Min(maxRHR*1000.d0,1.d6*(t(i)/tg)**2)   ! RHR in W
  End Do

  !-------------------------------------------------------------
  !  Lasketaan automaattisen hälytyksen onnistuminen:
  !-------------------------------------------------------------
  !  1) kerro lopputulos 0.9:llä (toimintavarmuus)
  !  2) Käytä Alpertin kaavaa kattolämpötilan laskemiseen
  !     paikka: arvo (x,y) molemmat tasajakautuneena 0-3.5 m
  !     eli tällöin pahimmillaan noin 5 m päässä varoittimesta
  !     Laske temp_gas ja virtausnopeus Alpertilla
  !  3) RTI = 100, T_h = 68 C, T_amb = 20 C
  !
  !  Alpert: H = 8.0 m, T_amb=20, palotehon Q yksikot kW
  !  r > 0.18*H_alp
  !  T_gas - T_amb = ( 5.38*(0.001d0*Q_IMC(i)/r)**(2/3) )/H_alp
  !  r < 0.18*H_alp
  !  T_gas - T_amb = ( 16.9*(0.001d0*Q_IMC(i))**(2/3) )/H_alp**(5/3)
  !
  Ts(1) = T_amb
  Do i = 1, itmax-1
     ! Alpertin kaava kattosuihkun lämpötilalle
     If ( r .gt. 0.18d0*H_alp) Then
        !       r > 0.18*H_alp
        T_gas=(5.38d0*(0.001d0*Q_IMC(i)/r)**(2.d0/3.d0))/H_alp + T_amb 
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        T2_gas = ( 5.38d0*( 0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             /r)**(2.d0/3.d0))/H_alp  + T_amb 
     Else    
        !       r < 0.18*H_alp
        T_gas = (16.9d0*(0.001d0*Q_IMC(i))**(2.d0/3.d0)) / &
             H_alp**(5.d0/3.d0) + T_amb 
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        T2_gas = (16.9d0*(0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             )**(2.d0/3.d0)) / H_alp**(5.d0/3.d0) + T_amb 
     End If
     ! Alpertin kaava kattosuihkun virtausnopeudelle
     If ( r .gt. 0.15d0*H_alp) Then
        !       r > 0.15*H_alp
        vir_nop = 0.196d0*(0.001d0*Q_IMC(i))**(1.d0/3.d0)* &
             Dsqrt(H_alp)/(r**(5.d0/6.d0))
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        vir2_nop = 0.196d0*(0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             )**(1.d0/3.d0)*Dsqrt(H_alp)/(r**(5.d0/6.d0))
     Else    
        !       r < 0.15*H_alp
        vir_nop = 0.952d0*( 0.001d0*Q_IMC(i) / H_alp )**(1.d0/3.d0)
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        vir2_nop=0.952d0*( 0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             / H_alp )**(1.d0/3.d0)
     End If
     !-------------------------------------------------------------
     ! Lasketaan automaattisen hälytyksen onnistuminen ratkaisemalla
     ! differentiaaliyhtälö:
     ! Käytetään 'mid-point method' eli 2nd order Runge-Kutta
     ! Ts on hälyttimen lämpötila, T_gas kaasun lämpötila ja u on
     ! virtausnopeus.
     ! d(Ts)/dt = (sqrt(u)/rti) ( T_gas - Ts )
     !-------------------------------------------------------------
     h = t(i+1) - t(i)
     k1 = h * ( (Dsqrt(vir_nop)/RTI)*T_gas - &
          (Dsqrt(vir_nop)/RTI)*Ts(i) )
     k2 = h * ( (Dsqrt(vir2_nop)/RTI)*T2_gas - &
          ( Dsqrt(vir2_nop)/RTI )*( Ts(i) + 0.5d0*k1 ) )
     ! Seuraava Euler
     !        Ts(i+1) = Ts(i) + k1
     ! Seuraava 2nd order Runge-Kutta ('mid-point' method)
     !     Ts(i+1) = Ts(i) + k2
     ! Jos aika-askel on pitkä ja aikavakio lyhyt, niin RK tuottaa 
     ! numeerisia onglemia. Ratkaisu: Tällöin detektorin lämpötila
     ! seuraa hyvin tarkasti kaasun lämpötilaa. EULERILLA ALLA
     ! nopeus/RTIsavu [1/s] aikavakio
     If ( Max( Dsqrt(vir_nop)/RTI, Dsqrt(vir2_nop)/RTI ) .gt. &
          1.d0/h ) Then
        Ts(i+1) = Min( (Ts(i) + k1), T2_gas)
     Else
        ! Runge-Kutta (aikavakio ei ole liian pieni)
        Ts(i+1) = Ts(i) + k2
     End If
     If ( (Ts(i) .lt. T_h) .and. (Ts(i+1) .ge. T_h) ) Then
        t_RKauto =  t(i) + (Ts(i+1)-T_h)*(t(i+1)-t(i))/ &
             (Ts(i+1)-Ts(i))
        Exit
     End If
     
  End Do
  talarm = t_RKauto

  Deallocate(t)
  Deallocate(Q_IMC)
  Deallocate(Ts)

  Return
End Subroutine HeatDetAlpert
!*********************************************************
!***** Subroutine HeatDetAlpert Ends                 *****
!*********************************************************

!*********************************************************
!***** Subroutine HeatDetOzone Starts               *****
!*********************************************************
!
! This routine uses RHR(t) as input
!
!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: HeatDetOzone
!DEC$ ATTRIBUTES ALIAS:'HeatDetOzone' :: HeatDetOzone
Subroutine HeatDetOzone(Nrows, Tamb, Tact, RTI, r_max, Hroom, Hfire, &
                         Taxis, RHRdata, talarm)
  Use dflib
  Use dfport
  Implicit None
  !----------------------------------------------
  ! Argument definitions
  !----------------------------------------------
  ! Input variables (by value)
  Integer(2) Nrows
  Real(8) Tamb, Tact, RTI, r_max, Hroom, Hfire, tg
  ! Note: RHRdata is in W (excel sheet has kW but VB change to W)
  ! Output variables (by reference)
  Real(8) talarm, Taxis(Nrows), RHRdata(Nrows)
  !DEC$ ATTRIBUTES REFERENCE :: talarm, Taxis, RHRdata

  !----------------------------------------------
  ! Local variables
  !----------------------------------------------
  Integer(4) i, itmax
  Real(8) H_alp, r, T_gas, T2_gas, T_amb, T_h
  Real(8) vir_nop, vir2_nop, h, k1, k2, t_RKauto
  Real(8) tmax, Qmax, rhr
  Real(8), Allocatable :: t(:), Q_IMC(:), Ts(:)

  !------------------------------------------------------------
  !  External Subroutines and Functions
  !------------------------------------------------------------
  External Interpolate1d

  H_alp = Hroom - Hfire
  T_amb = Tamb
  T_h   = Tact
  r = r_max
  t_RKauto = 0.d0

  !------------------------------------------------------------
  ! Generate the RHR(t) and time axis
  !------------------------------------------------------------
  tmax = Taxis(Nrows)
  itmax = Int(tmax/5.d0) + 1    ! delta t is 5 seconds
  Allocate(t(itmax))
  Allocate(Q_IMC(itmax))
  Allocate(Ts(itmax))
!  Open(67,file='D:\Tmp\fort_67.txt', position='append')
!  Write(67,*) 'input rhr'
!  Do i = 1,Nrows
!     Write(67,*) Taxis(i),RHRdata(i)
!  End Do
  Do i = 1, itmax
     t(i) = (i-1)*(tmax/(itmax-1))  ! Time axis in seconds
     Call Interpolate1d(Nrows,Taxis, RHRdata, t(i), rhr)
     Q_IMC(i) = rhr   ! RHR in W
     !     Write(67,*) t(i), rhr
  End Do
 ! Close(67)
  !-------------------------------------------------------------
  !  Lasketaan automaattisen hälytyksen onnistuminen:
  !-------------------------------------------------------------
  !  1) kerro lopputulos 0.9:llä (toimintavarmuus)
  !  2) Käytä Alpertin kaavaa kattolämpötilan laskemiseen
  !     paikka: arvo (x,y) molemmat tasajakautuneena 0-3.5 m
  !     eli tällöin pahimmillaan noin 5 m päässä varoittimesta
  !     Laske temp_gas ja virtausnopeus Alpertilla
  !  3) RTI = 100, T_h = 68 C, T_amb = 20 C
  !
  !  Alpert: H = 8.0 m, T_amb=20, palotehon Q yksikot kW
  !  r > 0.18*H_alp
  !  T_gas - T_amb = ( 5.38*(0.001d0*Q_IMC(i)/r)**(2/3) )/H_alp
  !  r < 0.18*H_alp
  !  T_gas - T_amb = ( 16.9*(0.001d0*Q_IMC(i))**(2/3) )/H_alp**(5/3)
  !
  Ts(1) = T_amb
  Do i = 1, itmax-1
     ! Alpertin kaava kattosuihkun lämpötilalle
     If ( r .gt. 0.18d0*H_alp) Then
        !       r > 0.18*H_alp
        T_gas=(5.38d0*(0.001d0*Q_IMC(i)/r)**(2.d0/3.d0))/H_alp + T_amb 
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        T2_gas = ( 5.38d0*( 0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             /r)**(2.d0/3.d0))/H_alp  + T_amb 
     Else    
        !       r < 0.18*H_alp
        T_gas = (16.9d0*(0.001d0*Q_IMC(i))**(2.d0/3.d0)) / &
             H_alp**(5.d0/3.d0) + T_amb 
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        T2_gas = (16.9d0*(0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             )**(2.d0/3.d0)) / H_alp**(5.d0/3.d0) + T_amb 
     End If
     ! Alpertin kaava kattosuihkun virtausnopeudelle
     If ( r .gt. 0.15d0*H_alp) Then
        !       r > 0.15*H_alp
        vir_nop = 0.196d0*(0.001d0*Q_IMC(i))**(1.d0/3.d0)* &
             Dsqrt(H_alp)/(r**(5.d0/6.d0))
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        vir2_nop = 0.196d0*(0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             )**(1.d0/3.d0)*Dsqrt(H_alp)/(r**(5.d0/6.d0))
     Else    
        !       r < 0.15*H_alp
        vir_nop = 0.952d0*( 0.001d0*Q_IMC(i) / H_alp )**(1.d0/3.d0)
        ! Runge-Kuttaa varten tarvitaan arvo puolivälissä
        vir2_nop=0.952d0*( 0.001d0*0.5d0*(Q_IMC(i)+Q_IMC(i+1)) &
             / H_alp )**(1.d0/3.d0)
     End If
     !-------------------------------------------------------------
     ! Lasketaan automaattisen hälytyksen onnistuminen ratkaisemalla
     ! differentiaaliyhtälö:
     ! Käytetään 'mid-point method' eli 2nd order Runge-Kutta
     ! Ts on hälyttimen lämpötila, T_gas kaasun lämpötila ja u on
     ! virtausnopeus.
     ! d(Ts)/dt = (sqrt(u)/rti) ( T_gas - Ts )
     !-------------------------------------------------------------
     h = t(i+1) - t(i)
     k1 = h * ( (Dsqrt(vir_nop)/RTI)*T_gas - &
          (Dsqrt(vir_nop)/RTI)*Ts(i) )
     k2 = h * ( (Dsqrt(vir2_nop)/RTI)*T2_gas - &
          ( Dsqrt(vir2_nop)/RTI )*( Ts(i) + 0.5d0*k1 ) )
     ! Seuraava Euler
     !        Ts(i+1) = Ts(i) + k1
     ! Seuraava 2nd order Runge-Kutta ('mid-point' method)
     !     Ts(i+1) = Ts(i) + k2
     ! Jos aika-askel on pitkä ja aikavakio lyhyt, niin RK tuottaa 
     ! numeerisia onglemia. Ratkaisu: Tällöin detektorin lämpötila
     ! seuraa hyvin tarkasti kaasun lämpötilaa. EULERILLA ALLA
     ! nopeus/RTIsavu [1/s] aikavakio
     If ( Max( Dsqrt(vir_nop)/RTI, Dsqrt(vir2_nop)/RTI ) .gt. &
          1.d0/h ) Then
        Ts(i+1) = Min( (Ts(i) + k1), T2_gas)
     Else
        ! Runge-Kutta (aikavakio ei ole liian pieni)
        Ts(i+1) = Ts(i) + k2
     End If
     If ( (Ts(i) .lt. T_h) .and. (Ts(i+1) .ge. T_h) ) Then
        t_RKauto =  t(i) + (Ts(i+1)-T_h)*(t(i+1)-t(i))/ &
             (Ts(i+1)-Ts(i))
        Exit
     End If
     
  End Do
  talarm = t_RKauto

  Deallocate(t)
  Deallocate(Q_IMC)
  Deallocate(Ts)

  Return
End Subroutine HeatDetOzone
!*********************************************************
!***** Subroutine HeatDetOzone Ends                 *****
!*********************************************************
