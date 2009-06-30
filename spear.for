c     Numerical Recipes
c     Spearman Rank-Order Correlation Coefficient
c     Routine: SPEAR
c     Uses: Betai, Crank, Erfc, Sort2
c     This file includes: Sort2, Crank
c     Uses IMSL routines: Dbetai, Derfc, Dsvrgp
c
c     NOTE: Sort2 does not scale like N*log2(N)
c     Now a IMSL sort routine Dsvrgp is used.
c     The new method (imsl sort) is tested and works correctly (T.K.)
c
      Subroutine SpearPFS(iInp, iOut, iRanNum, iOutNum, data1, data2, 
     $     n, wksp1, wksp2, d, zd, probd, rs, probrs)
      Use dflib
      Implicit None
c
c     Passed Variables
      Integer iInp, iOut, iRanNum, iOutNum, n
      Double Precision d, probd, probrs, rs, zd
c
c     Passed Arrays
      Double Precision data1(n,iRanNum), data2(n,iOutNum)
      Double Precision wksp1(n), wksp2(n)
c
c     Local Variables
      Integer j
      Double Precision aved, df, en, en3n, fac, sf, sg, t, vard
      Double Precision, Allocatable :: work3(:)
      Integer*4, Allocatable :: iperm(:)
c
c     External Functions
c      Double Precision Betai, Erfcc, Dsvrgp
c      External Betai, Erfcc, Dsvrgp
c     Betai: Returns the incomplete beta function I_x (a,b)
c     Erfcc: Returns the complementary error function erfc(x)
c     Do not use Numerical Recipes, use IMSL library routines:
c     Incomplete beta function: IMSL betai(x,p,q) = I_x (p,q)
c     Complementary error function: IMSL erfc(x)
c     ('D' stands for double precision)
      Double Precision Dbetai, Derfc, Dsvrgp
      External Dbetai, Derfc, Dsvrgp
c     
c     External Subroutines
c
      External Sort2, Crank
c
      wksp1(:) = data1(:,iInp)
      wksp2(:) = data2(:,iOut)

      
      Allocate( iperm(n) )
      Allocate( work3(n) )
      Do j = 1, n
        iperm(j) = j
      End Do
      Call DSVRGP(n, wksp1, wksp1, iperm)
      Do j = 1, n
        wksp2(j) = data2(iperm(j),iOut)
      End Do
c      Call Sort2(n, wksp1, wksp2)
      Call Crank(n, wksp1, sf)

      Do j = 1, n
        iperm(j) = j
      End Do
      Call DSVRGP(n, wksp2, wksp2, iperm)
      Do j = 1, n
        work3(j) = wksp1(iperm(j))
      End Do
      wksp1(:) = work3(:)
c      Call Sort2(n, wksp2, wksp1)
      Call Crank(n, wksp2, sg)
      DeAllocate(work3)
      DeAllocate(iperm)
      
c
      d = 0.d0
      Do j = 1,n
        d = d + (wksp1(j)-wksp2(j))**2
      End Do
      en = n
      en3n = en**3 - en
      aved = en3n/6.d0 - (sf+sg)/12.d0
      fac = (1.d0-sf/en3n) * (1.d0-sg/en3n)
      vard = ( (en-1.d0)*en**2*(en+1.d0)**2/36.d0 )*fac
      zd = (d-aved)/Dsqrt(vard)
c     NumRecipes: Erfcc returns the complementary error function erfc(x)
c      probd = Erfcc(abs(zd)/Dsqrt(2.d0))
c     IMSL: Derfc returns the complementary error function erfc(x)
      probd = Derfc(abs(zd)/Dsqrt(2.d0))
      rs = ( 1.d0 - (6.d0/en3n)*(d+(sf+sg)/12.d0) )/Dsqrt(fac)
      fac = (1.d0+rs)*(1.d0-rs)
c
      If ( fac .gt. 0.d0 ) Then
        t = rs*Dsqrt((en-2.d0)/fac)
        df = en-2.d0
c     NumRecipes: function call: betai(a,b,x)
c     Betai returns the incomplete beta function I_x (a,b)
c        probrs = Betai( 0.5d0*df, 0.5d0, df/(df+t**2) )
c     IMSL: function call: betai(x,p,q)
c     Betai returns the incomplete beta function I_x (p,q)
c     NumRecipes vs IMSL:   a <=> p and b <=> q
        probrs = Dbetai( df/(df+t**2), 0.5d0*df, 0.5d0 )
      Else
        probrs = 0.d0
      End If
c
      Return
      End
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     End of subprogram SPEAR
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Numerical Recipes
c     Spearman Rank-Order Correlation Coefficient
c     Routine: CRANK
c
      Subroutine Crank(n,w,s)
      Implicit None
c
c     Passed Variables
      Integer n
      Double Precision s
c
c     Passed Arrays
      Double Precision w(n)
c
c     Local Variables
      Integer j, ji, jt
      Double Precision rank, t
c
      s = 0.d0
      j = 1
c
 1    Continue
      If ( j .lt. n ) Then
        If ( w(j+1) .ne. w(j) ) Then
          w(j) = j
          j = j + 1
        Else
          Do jt = j+1,n
            If ( w(jt) .ne. w(j) ) Goto 2
          End Do
          jt = n + 1
 2        rank = 0.5d0*(j+jt-1)
          Do ji = j,jt-1
            w(ji) = rank
          End Do
          t = jt - j
          s = s + t**3 - t
          j = jt
        End If
        Goto 1
      End If
c
      If ( j .eq. n ) w(n) = n
c
      Return
      End
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     End of subprogram CRANK
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Numerical Recipes
c     Sorts an array RA of length N into ascending numerical
c     order using Heapsort algorithm, while making the corresponding
c     rearrangement of the array RB
c     Routine: SORT2
c
      Subroutine sort2(n, ra, rb)
      Implicit None
c     Passed Variables
      Integer n
c
c     Passed Arrays
      Double Precision ra(n), rb(n)
c
c     Locar Variables
      Integer L, i, j, ir
      Double Precision rra, rrb
c
      L = n/2 + 1
      ir = n
 10   Continue
      If ( L .gt. 1 ) Then
        L = L - 1
        rra = ra(L)
        rrb = rb(L)
      Else
        rra = ra(ir)
        rrb = rb(ir)
        ra(ir) = ra(1)
        rb(ir) = rb(1)
        ir = ir - 1
        If ( ir .eq. 1 ) Then
          ra(1) = rra
          rb(1) = rrb
          Return
        End If
      End If
      i = L
      j = L + L
c
 20   Continue
      If ( j .le. ir ) Then
        If ( j .lt. ir ) Then
          If ( ra(j) .lt. ra(j+1) ) j = j + 1
        End If
        If ( rra .lt. ra(j) ) Then
          ra(i) = ra(j)
          rb(i) = rb(j)
          i = j
          j = j + 1
        Else
          j = ir + 1
        End If
        Goto 20
      End If
c
      ra(i) = rra
      rb(i) = rrb
      Goto 10
c
      End
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     End of subprogram SORT2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
