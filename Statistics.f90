!*********************************************************
!*****   PFS2 v2.0 Statistical subroutine library    *****
!*********************************************************
!
! VTT Building and Transport                          2004
! VTT Technical Research Centre of Finland
!
! Author:  Timo Korhonen
! Date:    19.4.2004 (version v0.0 started 18.02.2003)
! Version: v2.1 
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
! * RandomNumbers
!   - generates random numbers from the specified distributions
!     (implemented: uniform), USES RANMAR uniform generator
!     (ranmar.f file, fortran77, 32 bit random numbers)
!   - Many subprograms are included: different distributions for
!     both the LHS-sampling (xxx_lhs) and MC-sampling (xxx_rnd).
!   - User given density: now a discrete density can also be given
!
! * DoTheStats
!   - calculates correlations, histograms etc
!
! * ...
!
!*********************************************************
! TO DO:
!
! * test the speed of 'sort' in the spearpfs.
!
! BUGS FOUND
!
! * SpearPFS scales as N**2, this is too slow for 100 000
!   mc runs. Sort should be as N*log(N) scaling, so is this
!   a 'bug'?  
! * SpearPFS makes an error, if the input values are all the
!   same.
!
! BUG FIXES
!
! * Found bug in SpearPFS, corrected 27.2.2003 by T.K.
! * SpearPFS uses now an IMSL sort routine and is much
!   faster, corrected 19.4.2004 by T.K.
!
!
!*********************************************************
!
!
!
!*********************************************************
!***** Subroutine RandomNumbers Starts               *****
!*********************************************************
!
! nmc:             Number of MC steps
! iSamp:           1) MC; 2) Latin HyperCube
! RandomType(:):   1) Uniform, 2) ...
! RandomNpts(:):   Number of distribution parameters
! RandomPara(:,:): Parameters of the distributions
!
! RandomVector(:,:): The random sample
!
!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: RandomNumbers
!DEC$ ATTRIBUTES ALIAS:'RandomNumbers' :: RandomNumbers
Subroutine RandomNumbers( nmc, iSeed, iRanNum2, iMaxPar2, iSamp2, &
           RandomPara, RandomType, RandomNpts, iNptsMax, iUserGiven, &
           iMode2, iNpts2, xi, fuser, RandomVector, time_ran )
  Use dflib
!  Use dfwin
!  Use dfport
  Implicit None

  !----------------------------------------------
  ! Argument definitions
  !----------------------------------------------
  ! Input variables (by value)
  Integer(4) nmc, iSeed
  Integer(2) iRanNum2, iMaxPar2, iSamp2, iNptsMax, iUserGiven
  Integer(2) RandomType(iRanNum2), RandomNpts(iRanNum2)
  Integer(2) iMode2(iUserGiven), iNpts2(iUserGiven) 
  Real(8)    RandomPara(iRanNum2,iMaxPar2), xi(iNptsMax,iUserGiven)
  Real(8)    fuser(iNptsMax,iUserGiven)
  ! Output variables (by reference)
  Real(8)    RandomVector(nmc, iRanNum2), time_ran
  !DEC$ ATTRIBUTES REFERENCE :: RandomVector, time_ran
  !DEC$ ATTRIBUTES REFERENCE :: RandomNpts, RandomPara, xi
  !DEC$ ATTRIBUTES REFERENCE :: fuser, RandomType, iMode2, iNpts2

  !----------------------------------------------
  ! Local variables
  !----------------------------------------------

  Integer(4) i, imc, mode, npts, itmp
  Integer(4) iRanNum, iMaxPar, iSamp, iRanTypTmp
  Logical LatinHCS
  Real(8) te_ran, ts_ran
  ! RanMar uniform generator parameters:
  !    i1:    seed
  !    n1,n2: restart conters
  !    rvec:  32 bit random numbers as a vector
  Integer(4) i1, n1, n2, crate, cmax
  Real(4)    timecpu
  Real(4), Allocatable :: rvec(:)
  Integer(4), Allocatable :: ivec(:)
  !------------------------------------------------------------
  !  External Subroutines and Functions
  !------------------------------------------------------------
  External rmarin
!
  iRanNum = iRanNum2    ! 2-byte to 4-byte integers
  iMaxPar = iMaxPar2
  iSamp   = iSamp2
  
!  Open(20,file='D:\Tmp\fort.20', position='append')

  !-------------------------------------------------------------
  ! Initialize the RanMar generator
  !-------------------------------------------------------------
  Call Cpu_time(timecpu)
  Call System_Clock( i1, crate, cmax )
  i1 = i1 + Nint( 97*timecpu/31 )
  If ( iSeed .gt. 0 ) Then
     i1 = iSeed   ! User has supplied a seed
  End If
     
  Call rmarin(i1,n1,n2)

  If ( iSamp .eq. 2 ) Then
     LatinHCS = .true.
  Else
     LatinHCS = .false.
  End If
  
  Call Cpu_time(ts_ran)
  If ( LatinHCS) Then
     !  1: uniform (TESTED: OK)
     !  2: beta (TESTED: OK)
     !  3: gamma  (TESTED: OK)
     !  4: normal (TESTED: OK)
     !  5: lognormal (TESTED: OK)
     !  6: Truncated normal (TESTED: OK)
     !  7: Triangular (TESTED: OK)
     !  8: Weibull (TESTED: OK) (alpha=1: Exponential)
     !  9: Gumbel (TESTED: OK)
     ! 10: User specified (density or distribution given)
     Allocate( rvec(nmc) )
     Allocate( ivec(nmc) )
     itmp = 0
     Do i = 1, iRanNum
        If ( RandomType(i) .gt. 100 ) Then
           itmp = itmp + 1  ! counter: user given functions
           iRanTypTmp = 10
           mode      = iMode2(itmp)
           npts      = iNpts2(itmp)
        Else
          iRanTypTmp = RandomType(i)
        End If

        Select Case (iRanTypTmp)
        Case (1)    ! Uniform distribution
           ! nmc, rvec: 4 bytes, others 8 bytes
           Call Uniform_lhs(nmc, RandomPara(i,2), RandomPara(i,3), &
                            rvec)
        Case (2)   ! Beta
           ! nmc, rvec: 4 bytes, others 8 bytes
           Call Beta_lhs(nmc, RandomPara(i,2), RandomPara(i,3), &
                            rvec)
        Case (3)   ! Gamma
           ! nmc, rvec: 4 bytes, others 8 bytes
           Call Gamma_lhs(nmc, RandomPara(i,2), RandomPara(i,3), &
                            rvec)
        Case (4)   ! Normal
           ! nmc, rvec: 4 bytes, others 8 bytes
           Call Normal_lhs(nmc, RandomPara(i,1), RandomPara(i,2), &
                            rvec)
        Case (5)   ! LogNormal
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! mean and variance of log(x) should be given
           Call LogNormal_lhs(nmc, RandomPara(i,1), RandomPara(i,2), &
                            rvec)
        Case (6)   ! Truncated Normal
           ! nmc, rvec: 4 bytes, others 8 bytes
           Call TNormal_lhs(nmc, RandomPara(i,1), RandomPara(i,2), &
                            RandomPara(i,3), RandomPara(i,4), rvec)
        Case (7)   ! Triangular
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (peak,min,max)
           Call Triang_lhs(nmc, RandomPara(i,1), RandomPara(i,2), &
                            RandomPara(i,3), rvec)
        Case(8)    ! Weibull  (alpha=1: Exponential)
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,alpha,lambda)
           Call Weibull_lhs(nmc, RandomPara(i,2), &
                            RandomPara(i,3), rvec)
        Case(9)    ! Gumbel
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,alpha)
           Call Gumbel_lhs(nmc, RandomPara(i,2), rvec)
        Case(10)    ! User specified function
           ! nmc, mode, npts, rvec: 4 bytes, others 8 bytes
           ! Parameters:
           !   mode:  1) density given
           !          2) distribution given
           !          3) discrete distribution (as histogram)
           !   npts:  number of points in the input function
           !   xi:    the points on the x-axis
           !   fuser: the density or distribution values
           Call User_lhs(nmc, mode, npts, xi(1,itmp), fuser(1,itmp), &
                rvec)
        Case Default
           Stop
        End Select

        Do imc = 1, nmc
           ivec(imc) = imc
        End Do
!!!        If ( i .gt. 1 ) Then
        Call genprm(ivec, nmc)  ! random permutation
!!!        End If
        Do imc = 1, nmc
           RandomVector(imc,i) = rvec(ivec(imc))
        End Do

     End Do
     Deallocate( ivec )
     Deallocate( rvec )

  Else
     ! 1: uniform (TESTED: OK)
     ! 2: beta (TESTED: OK)
     ! 3: gamma  (TESTED: OK)
     ! 4: normal (TESTED: OK)
     ! 5: lognormal (TESTED: OK)
     ! 6: Truncated normal (TESTED: OK)
     ! 7: Triangular (TESTED: OK)
     ! 8: Weibull (TESTED: OK) (alpha=1: Exponential)
     ! 9: Gumbel (TESTED: OK)
     ! 10: User specified (density or distribution given)
     Allocate( rvec(nmc) )
     itmp = 0
     Do i = 1, iRanNum
        If ( RandomType(i) .gt. 100 ) Then
           itmp = itmp + 1  ! counter: user given functions
           iRanTypTmp = 10
           mode      = iMode2(itmp)
           npts      = iNpts2(itmp)
        Else
           iRanTypTmp = RandomType(i)
        End If

        Select Case (iRanTypTmp)
        Case (1)    ! Uniform distribution
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,min,max) ave not used
           Call Uniform_rnd(nmc, RandomPara(i,2), RandomPara(i,3), &
                            rvec)
        Case (2)   ! Beta
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,a,b) ave not used
           Call Beta_rnd(nmc, RandomPara(i,2), RandomPara(i,3), &
                            rvec)
        Case (3)   ! Gamma
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,alpha,beta) ave not used
           Call Gamma_rnd(nmc, RandomPara(i,2), RandomPara(i,3), &
                            rvec)
        Case (4)   ! Normal
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,sigma)
           Call Normal_rnd(nmc, RandomPara(i,1), RandomPara(i,2), &
                            rvec)
        Case (5)   ! LogNormal
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! mean and variance of log(x) should be given
           ! Parameters: (ave,sigma) of ln(x)
           Call LogNormal_rnd(nmc, RandomPara(i,1), RandomPara(i,2), &
                            rvec)
        Case (6)   ! Truncated Normal
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,sigma,min,max)
           Call TNormal_rnd(nmc, RandomPara(i,1), RandomPara(i,2), &
                            RandomPara(i,3), RandomPara(i,4), rvec)
        Case (7)   ! Triangular
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (peak,min,max)
           Call Triang_rnd(nmc, RandomPara(i,1), RandomPara(i,2), &
                            RandomPara(i,3), rvec)
        Case(8)    ! Weibull  (alpha=1: Exponential)
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,alpha,lambda)
           Call Weibull_rnd(nmc, RandomPara(i,2), &
                            RandomPara(i,3), rvec)
        Case(9)    ! Gumbel
           ! nmc, rvec: 4 bytes, others 8 bytes
           ! Parameters: (ave,alpha)
           Call Gumbel_rnd(nmc, RandomPara(i,2), rvec)
        Case(10)    ! User specified function
           ! nmc, mode, npts, rvec: 4 bytes, others 8 bytes
           ! Parameters:
           !   mode:  1) density given
           !          2) distribution given
           !          3) discrete distribution (as histogram)
           !   npts:  number of points in the input function
           !   xi:    the points on the x-axis
           !   fuser: the density or distribution values
           Call User_rnd(nmc, mode, npts, xi(1,itmp), fuser(1,itmp), &
                rvec)
        Case Default
           Stop
        End Select

        Do imc = 1, nmc
           RandomVector(imc,i) = rvec(imc)
        End Do
     End Do
     Deallocate( rvec )

  End If
  Call Cpu_time(te_ran)
  time_ran = te_ran - ts_ran
  Return

Contains

  Subroutine Beta_rnd(nmc,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode, status
    Real(8) p,q,x,y, bound
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       q = 1.d0-p
       Call cdfbet(imode,p,q,x,y,a,b,status,bound)
       rvec(i) = x
    End Do
    Return
  End Subroutine Beta_rnd
  

  Subroutine Gamma_rnd(nmc,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode, status
    Real(8) p,q,x,y, bound, binv
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       q = 1.d0-p
       binv = 1.d0/b
       Call cdfgam(imode,p,q,x,a,binv,status,bound)
       rvec(i) = x
    End Do
    Return
  End Subroutine Gamma_rnd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Lognormal distribution
! If X has a lognormal distribution, then log(X) is normally distributed.
! Here the logarithm is the natural logarithm, that is to base e, sometimes
! denoted as ln.  To generate random variates from this distribution, generate
! a random deviate from the normal distribution with mean and variance equal
! to the mean and variance of the logarithms of X, then take its exponential.

! Relationship between the mean & variance of log(X) and the mean & variance
! of X, when X has a lognormal distribution.
! Let m = mean of log(X), and s^2 = variance of log(X)
! Then
! mean of X     = exp(m + 0.5s^2)
! variance of X = (mean(X))^2.[exp(s^2) - 1]

! In the reverse direction (rarely used)
! variance of log(X) = log[1 + var(X)/(mean(X))^2]
! mean of log(X)     = log(mean(X) - 0.5var(log(X))

! N.B. The above formulae relate to population parameters; they will only be
!      approximate if applied to sample values.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  Subroutine LogNormal_rnd(nmc,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode, status
    Real(8) p,q,x,y,bound
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       q = 1.d0-p
       Call cdfnor(imode,p,q,x,a,b,status,bound)
       rvec(i) = Exp(x)    ! lognormal
    End Do
    Return
  End Subroutine LogNormal_rnd
  
  Subroutine TNormal_rnd(nmc,a,b,xmin,xmax,rvec)
    ! Truncated normal distribution
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b, xmin, xmax
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode, status
    Real(8) p,q,x,y,bound, pmax, pmin
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 1   ! calculate cdf
    x = xmin
    Call cdfnor(imode,p,q,x,a,b,status,bound)
    pmin = p
    x = xmax
    Call cdfnor(imode,p,q,x,a,b,status,bound)
    pmax = p
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)*(pmax-pmin) + pmin  
       q = 1.d0-p
       Call cdfnor(imode,p,q,x,a,b,status,bound)
       rvec(i) = Max(x,xmin)
       rvec(i) = Min(x,xmax)
    End Do
    Return
  End Subroutine TNormal_rnd
  
  Subroutine Normal_rnd(nmc,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode, status
    Real(8) p,q,x,y,bound
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       q = 1.d0-p
       Call cdfnor(imode,p,q,x,a,b,status,bound)
       rvec(i) = x
    End Do
    Return
  End Subroutine Normal_rnd
  
  Subroutine Triang_rnd(nmc,peak,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b, peak
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode, status
    Real(8) p,x,y,bound
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       Call cdftri(imode,p,x,peak,a,b)
       rvec(i) = x
    End Do
    Return
  End Subroutine Triang_rnd
  
  Subroutine cdftri(imode,p,x,peak,a,b)
    ! Cumulative (and inverse) triangular distribution
    ! Written by T.K. (25/2/2003), works correctly
    Implicit None
    Integer imode
    Real(8) p, x, peak, a, b
    ! Local variables
    Real(8) y_peak, p_peak, xkk1, xkk2, y2
    y_peak   = 2.d0 / (b-a)  ! maximum of the density
    p_peak = (peak-a)/(b-a)  ! cdf at the 'peak'
    xkk1 = y_peak/(peak-a)   ! derivate 1
    xkk2 = y_peak/(b-peak)  ! derivate 2
    If ( imode .eq. 1 ) Then   ! cdf
       If ( x .le. a ) Then
          p = 0.d0
       Else If ( x .le. peak ) Then
          p = 0.5d0*(x-a)*(x-a)*xkk1
       Else If ( x .lt. b ) Then
          y2 = y_peak - (x-peak)*xkk2
          p = p_peak + (x-peak)*0.5d0*(y_peak+y2)
       Else 
          p = 1.d0
       End If
    Else If (imode .eq. 2 ) Then   ! inverse cdf
       If ( p .le. 0 ) Then
          x = a
       Else If ( p .le. p_peak ) Then
          x = a + Sqrt( 2.d0*p / xkk1 )
       Else If ( p .lt. 1 ) Then
          x = b - Sqrt( 2.d0*(1.d0-p)/xkk2)
       Else 
          x = b
       End If
    Else
       Stop
    End If
  End Subroutine cdftri
  
  Subroutine Weibull_rnd(nmc,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode
    Real(8) p,x,y
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       Call cdfwei(imode,p,x,a,b)
       rvec(i) = x
    End Do
    Return
  End Subroutine Weibull_rnd
   
  Subroutine cdfwei(imode,p,x,alpha,lambda)
    ! Cumulative (and inverse) Weibull distribution
    ! Written by T.K. (25/2/2003)
    ! f(x) = alpha*lambda*(lamda*x)^(alpha-1)*exp(-(lambda*x)^alpha)
    Implicit None
    Integer imode
    Real(8) p, x, alpha, lambda

    If ( (alpha .le. 0.d0) .or. (lambda .le. 0.d0 ) ) Then
       Stop
    End If
    If ( imode .eq. 1 ) Then   ! cdf
       If ( x .le. 0.d0 ) Then
          p = 0.d0  !  Weibull defined: x > 0
       Else
          p = 1.d0 - Exp(-((lambda*x)**alpha))
       End If
    Else If (imode .eq. 2 ) Then   ! inverse cdf
       If ( p .le. 0 ) Then
          x = 0
       Else If ( p .lt. 1 ) Then
          x = (1.d0/lambda)*((-Log(1.d0-p))**(1.d0/alpha))
       Else 
          x = Huge(p)  ! largest positive number
       End If
    Else
       Stop
    End If
 
    Return
  End Subroutine cdfwei

  Subroutine Gumbel_rnd(nmc,a,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: first parameter, b: second parameter, nmc: vector lenght
    Integer i, imode
    Real(8) p,x,y
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc
       p = rvec(i)
       Call cdfgum(imode,p,x,a)
       rvec(i) = x
    End Do
    Return
  End Subroutine Gumbel_rnd
   
  Subroutine cdfgum(imode,p,x,alpha)
    ! Cumulative (and inverse) Gumbel distribution
    ! Written by T.K. (25/2/2003)
    ! f(x) = alpha*exp(-alpha*x)*exp( -exp(-alpha*x) )
    Implicit None
    Integer imode
    Real(8) p, x, alpha
    ! Local variables

    If ( alpha .le. 0.d0 ) Then
       Stop
    End If
    If ( imode .eq. 1 ) Then   ! cdf
       If ( x .lt. 0.d0 ) Then
          p = Exp( -1.d0/Exp(alpha*x) ) ! numerically better
!          p = Exp( -1.d0*Exp(-alpha*x) )
       Else
          p = Exp( -1.d0*Exp(-alpha*x) )
       End If
    Else If (imode .eq. 2 ) Then   ! inverse cdf
       If ( p .le. 0 ) Then
          x = -Huge(p)  ! smallest possible value
       Else If ( p .lt. 1 ) Then
          x = (-1.d0/alpha)*Log(-1.d0*Log(p))
       Else 
          x = Huge(p)  ! largest positive number
       End If
    Else
       Stop
    End If
 
    Return
  End Subroutine cdfgum

  !-------------------------------------------------------------
  ! Uniform Random Numbers: Ranmar generator proposed by
  ! Marsaglia and Zaman in report FSU-SCRI-87-50
  ! Monte Carlo Sampling
  !-------------------------------------------------------------
  Subroutine Uniform_rnd(nmc,a,b,rvec)
    Implicit None
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    External ranmar
    !
    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis
    rvec(:) = rvec(:)*(b-a) + a  ! Inverse CDF to get value on the x axis
    Return
  End Subroutine Uniform_rnd

  !-------------------------------------------------------------
  ! Uniform Random Numbers: Ranmar generator proposed by
  ! Marsaglia and Zaman in report FSU-SCRI-87-50
  ! Latin HyperCube Sampling
  !-------------------------------------------------------------
  Subroutine Uniform_lhs(nmc,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       Call Uni_invcdf(a,b,pi,xi)  ! Inverse CDF to get sample on the x axis
       xi = Max(a,xi) ! To be sure, i.e. do not rely on inv.CDF
       xi = Min(b,xi) ! To be sure, i.e. do not rely on inv.CDF
       rvec(i) = xi
    End Do
    Return

  End Subroutine Uniform_lhs

  Subroutine Beta_lhs(nmc,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, yi
    Integer imode, status
    Real(8) pi,qi, bound
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       qi = 1.d0-pi
       Call cdfbet(imode,pi,qi,xi,yi,a,b,status,bound) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = xi
    End Do
    Return

  End Subroutine Beta_lhs

  Subroutine Gamma_lhs(nmc,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi, binv
    Integer imode, status
    Real(8) qi, bound
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       qi = 1.d0-pi
       binv = 1.d0/b
       Call cdfgam(imode,pi,qi,xi,a,binv,status,bound) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = xi
    End Do
    Return

  End Subroutine Gamma_lhs

  Subroutine TNormal_lhs(nmc,a,b,xmin,xmax,rvec)
    ! Truncated normal distribution
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b, xmin, xmax
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi, pmax, pmin, dp
    Integer imode, status
    Real(8) qi, bound
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 1   ! calculate cdf
    xi = xmin
    Call cdfnor(imode,pi,qi,xi,a,b,status,bound)
    pmin = pi
    xi = xmax
    Call cdfnor(imode,pi,qi,xi,a,b,status,bound)
    pmax = pi
    imode = 2   ! calculate inverse cdf
    dp = pmax - pmin
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = pmin + (dp/nmc)*(i-1) ! start of an interval on y axis
       bi = pmin + (dp/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       qi = 1.d0-pi
       Call cdfnor(imode,pi,qi,xi,a,b,status,bound) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = Max(xi,xmin)
       rvec(i) = Min(xi,xmax)
    End Do
    Return

  End Subroutine TNormal_lhs

  Subroutine Normal_lhs(nmc,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi
    Integer imode, status
    Real(8) qi, bound
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       qi = 1.d0-pi
       Call cdfnor(imode,pi,qi,xi,a,b,status,bound) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = xi
    End Do
    Return

  End Subroutine Normal_lhs


  Subroutine LogNormal_lhs(nmc,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi
    Integer imode, status
    Real(8) qi, bound
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       qi = 1.d0-pi
       Call cdfnor(imode,pi,qi,xi,a,b,status,bound) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = Exp(xi)
    End Do
    Return

  End Subroutine LogNormal_lhs


  Subroutine Triang_lhs(nmc,peak,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b, peak
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi
    Integer imode
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       Call cdftri(imode,pi,xi,peak,a,b) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = xi
    End Do
    Return

  End Subroutine Triang_lhs

  Subroutine Weibull_lhs(nmc,a,b,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a, b
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi
    Integer imode
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       Call cdfwei(imode,pi,xi,a,b) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = xi
    End Do
    Return

  End Subroutine Weibull_lhs

  Subroutine Gumbel_lhs(nmc,a,rvec)
    Implicit None
    !
    ! Latin HyperCube Sampling
    !
    ! Passed Variables
    Integer(4) nmc
    Real(8) a
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! a: lower bound, b: upper bound, nmc: vector lenght
    ! RanMar uniform generator is used
    ! Local Variables
    Integer(4) i
    Real(8) ai, bi, xi, pi
    Integer imode
    !
    External ranmar
    !
    Call ranmar(rvec,nmc)   ! uniform (0,1)
    imode = 2   ! calculate inverse cdf
    Do i = 1, nmc                      ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1) ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i     ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       Call cdfgum(imode,pi,xi,a) 
       ! Inverse CDF to get sample on the x axis
       rvec(i) = xi
    End Do
    Return

  End Subroutine Gumbel_lhs

  Subroutine Uni_invcdf(a,b,y,x)
    Implicit None
    Real(8) a, b, y, x 
    ! Inverse of uniform cumulative distribution function
    ! defined on the interval (a,b)
    x = a + (b-a)*y
    Return
  End Subroutine Uni_invcdf

  Subroutine User_rnd(nmc, mode, npts, xi, f, rvec)
    Implicit None

    ! Passed Variables
    Integer(4) nmc, mode, npts
    Real(8) xi(npts), f(npts)
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! nmc:   vector lenght

    ! Local variables
    Integer(2) npts2
    Integer i
    Real(8) p, x
    Real(8), Allocatable :: yi(:), invdist(:)
    ! RanMar uniform generator is used
    External ranmar, Interpolate1d, Dens2InvDistr 

    npts2 = npts    ! two byte integer needed for Interpolate1d

    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis

    ! Calculate next the inverse cdf
    Allocate( yi(npts) )
    Allocate( invdist(npts) )
    Call Dens2InvDistr(mode, npts, xi, f, yi, invdist)

    Do i = 1, nmc
       p = rvec(i)
       If ( mode .eq. 3 ) Then   ! Discrete density
          ! Next if statement is needed if the first point of the given
          ! density is not equal to zero (interpolation routine can not
          ! interpolate below the first value...)
          If ( p .lt. yi(1) ) Then
             rvec(i) = xi(1)
          Else
             Call Interpolate1d(npts2, yi, invdist, p, x)
             ! Now a discrete distribution
             rvec(i) = xi( Max( 1,Min( npts,Int(1.d0 + x) ) ) )
          End If
       Else
          Call Interpolate1d(npts2, yi, invdist, p, x)
          rvec(i) = x
       End If
    End Do

    Deallocate( invdist)
    Deallocate( yi)
    Return
  End Subroutine User_rnd

  Subroutine User_lhs(nmc, mode, npts, xi, f, rvec)
    Implicit None

    ! Passed Variables
    Integer(4) nmc, mode, npts
    Real(8) xi(npts), f(npts)
    Real(4) rvec(nmc)
    ! rvec:  32 bit random numbers as a vector
    ! nmc:   vector lenght

    ! Local variables
    Integer(2) npts2
    Integer i
    Real(8) ai, bi, pi, x
    Real(8), Allocatable :: yi(:), invdist(:)
    ! RanMar uniform generator is used
    External ranmar, Interpolate1d, Dens2InvDistr 

    npts2 = npts    ! two byte integer needed for Interpolate1d

    Call ranmar(rvec,nmc)        ! Uniform (0,1) on the y axis

    ! Calculate the inverse cdf
    Allocate( yi(npts) )
    Allocate( invdist(npts) )
    Call Dens2InvDistr(mode, npts, xi, f, yi, invdist)

    Do i = 1, nmc                     ! split the interval (0,1)
       ai = 0.d0 + (1.d0/nmc)*(i-1)   ! start of an interval on y axis
       bi = 0.d0 + (1.d0/nmc)*i       ! end of an interval on y axis
       pi = rvec(i)*( bi - ai ) + ai  ! n'th sample on the y axis
       If ( mode .eq. 3 ) Then   ! Discrete density
          ! Next if statement is needed if the first point of the given
          ! density is not equal to zero (interpolation routine can not
          ! interpolate below the first value...)
          If ( pi .lt. yi(1) ) Then
             rvec(i) = xi(1)
          Else
             Call Interpolate1d(npts2, yi, invdist, pi, x)
             ! Now a discrete distribution
             rvec(i) = xi( Max( 1,Min( npts,Int(1.d0 + x) ) ) )
          End If
       Else
          Call Interpolate1d(npts2, yi, invdist, pi, x)
          rvec(i) = x
       End If
    End Do

    Deallocate( invdist)
    Deallocate( yi)
    Return
  End Subroutine User_lhs
  
End Subroutine RandomNumbers

!*********************************************************
!***** Subroutine RandomNumbers Ends                 *****
!*********************************************************



!*********************************************************
!***** Subroutine DoTheStats Starts                  *****
!*********************************************************

!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: DoTheStats
!DEC$ ATTRIBUTES ALIAS:'DoTheStats' :: DoTheStats
Subroutine DoTheStats(nmc, iRanNum2, iOutNum2, InputValues, &
                      Output, RankRs, RankProbrs, time_cor)
  Use dflib
!  Use dfport
  Implicit None
  !----------------------------------------------
  ! Argument definitions
  !----------------------------------------------
  ! Input variables (by value)
  Integer(4) nmc
  Integer(2) iRanNum2, iOutNum2
  ! Output variables and all arrays (by reference)
  Real(8) InputValues(nmc, iRanNum2), Output(nmc, iOutNum2), time_cor
  Real(8) RankRs(iRanNum2, iOutNum2), RankProbrs(iRanNum2, iOutNum2)
  !DEC$ ATTRIBUTES REFERENCE :: InputValues, Output, time_cor
  !DEC$ ATTRIBUTES REFERENCE :: RankRs, RankProbrs
  !----------------------------------------------
  ! Local variables
  !----------------------------------------------
  Integer(4) iInp, iOut, iRanNum, iOutNum, i
  Real(8) d, zd, probd, rs, probrs, ts_cor, te_cor, sum1
  ! Workspaces for rank corelation (Spearman rank
  ! correlation, 'spear.f' Fortran 77 routine)
  Real(8), Allocatable :: wksp1(:), wksp2(:)

  iRanNum = iRanNum2      ! SpearPFS uses integer*4
  iOutNum = iOutNum2      ! SpearPFS uses integer*4
  Allocate(wksp1(nmc))
  Allocate(wksp2(nmc))

  !
  ! This is not the fastest way of doing the things. It would
  ! be better to do all the inputs at the same time for a given
  ! output variable. This would reduce the number of sorts needed.
  ! Now the scaling is about NMC*log2(NMC) times iOutNum*iRanNum.
  Call Cpu_time(ts_cor)
  Do iOut = 1, iOutNum
     sum1 = 0.d0
     Do i = 2, nmc
        sum1 = sum1 + ( Output(i,iOut) - Output(1,iOut) )**2
     End Do
     If ( sum1 .gt. 0.d0 ) Then    ! all iOut outputs are not the same
        Do iInp = 1, iRanNum
           sum1 = 0.d0
           Do i = 2, nmc
              sum1 = sum1 + ( InputValues(i,iInp) - InputValues(1,iInp) )**2
           End Do
           If ( sum1 .gt. 0.d0 ) Then
              Call SpearPFS(iInp, iOut, iRanNum, iOutNum, InputValues, Output, &
                   nmc, wksp1, wksp2, d, zd, probd, rs, probrs)
              !  Spear(input,output,nmc,wk1,wk2,d,zd,probd,rs,probrs)
              !  rs: rank correltaion, probrs: P[rs .ne. 0]
              RankRs(iInp,iOut) = rs
              RankProbrs(iInp,iOut) = probrs
           Else    ! This iInp input is constant
              RankRs(:,iOut) = 0.d0
              RankProbrs(:,iOut) = 0.d0
           End If
        End Do
     Else    ! This iOut output is constant
        RankRs(:,iOut) = 0.d0
        RankProbrs(:,iOut) = 0.d0
     End If
  End Do
  Call Cpu_time(te_cor)
  time_cor = te_cor - ts_cor

  Return
End Subroutine DoTheStats

!*********************************************************
!***** Subroutine DoTheStats Ends                    *****
!*********************************************************


!*********************************************************
!***** Subroutine Dens2InvDistr Starts               *****
!*********************************************************

Subroutine Dens2InvDistr(mode, npts, xi, f, yi, invdist)
  Implicit None
  !
  ! Calculates the cumulative distribution function from
  ! the probability density f(x). The inverse is also
  ! calculated.
  !
  ! Assumptions: f(x) >= 0 for all x
  !              f(x)  > 0 for some x  
  ! These assumptions are not checked, because they should
  ! hold, if f(x) is a probability density function.
  !
  !----------------------------------------------
  ! Argument definitions
  !----------------------------------------------
  Integer(4) mode     ! 1: density given, 2: distr. given
  Integer(4) npts     ! number of points on the x-axis
  Real(8) xi(npts), f(npts), yi(npts), invdist(npts)
  !----------------------------------------------
  ! Local variables
  !----------------------------------------------
  Integer(4) i
  Real(8) sum, dx
  
  invdist = 0.d0

  If ( mode .eq. 1 ) Then
     ! Calculate the distribution (trapezoidal rule)
     sum  = 0.d0
     Do i = 2, npts
        dx      = xi(i) - xi(i-1)
        sum     = sum + 0.5d0*dx*( f(i) + f(i-1) )
        invdist(i) = sum         ! the distribution function
        ! invdist used as tmp array to save memory
     End Do
     invdist = (1.d0/sum)*invdist   ! now it is normalized
     invdist = Min(invdist,1.d0)    ! one or less (do not trust numerical accuracy
     ! of the normalization)
     ! Make the inverse of the distribution
     yi      = invdist
     invdist = xi
  Else If ( mode .eq. 3 ) Then
     ! Now a discrere distribution density is given:
     ! It is assumed that before the first point the density is zero and
     ! after the last given point the density is also zero
     sum  = 0.d0
     Do i = 1, npts
        sum     = sum +  f(i)
        invdist(i) = sum         ! the distribution function
        ! invdist used as tmp array to save memory
     End Do
     invdist = (1.d0/sum)*invdist   ! now it is normalized
     invdist = Min(invdist,1.d0)    ! one or less (do not trust numerical accuracy
     ! of the normalization)
     ! Make the inverse of the distribution
     yi      = invdist
     Do i = 1, npts
        invdist(i) = Dble(i)
     End Do
  Else
     ! Make the inverse of the distribution
     yi      = f
     invdist = xi
  End If

  Return
End Subroutine Dens2InvDistr

!*********************************************************
!***** Subroutine Dens2InvDistr Ends                 *****
!*********************************************************

Subroutine Interpolate1d(nx,x,y,xi,ans)
  Integer(2) nx
  Real(8)    x(nx), y(nx), xi, ans
  Integer jl, jm, ju
  !
  ! Find the index of the value just below XI
  !
  jl = 0
  ju = nx+1
  Do While ( (ju-jl) .gt. 1 )
     jm = ( ju + jl )/2
     If ( (x(nx) .gt. x(1) ) .eqv. ( xi .gt. x(jm) ) ) Then
        jl = jm
     Else
        ju = jm
     End If
  End Do
  !
  ! Interpolate between JL and JL+1
  !
  If ( jl .ge. nx ) Then
     ans = y(nx)
  Else If ( jl .lt. 1 ) Then
     ans = y(1)
  Else
     ans = y(jl) + (xi-x(jl)) * (y(jl+1)-y(jl)) &
        / (x(jl+1)-x(jl)+1.0e-30*(x(2)-x(1)))
  End If
End Subroutine Interpolate1d
