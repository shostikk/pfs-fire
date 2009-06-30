      SUBROUTINE genprm(iarray,larray)
C From the 'Ranlib' package
C**********************************************************************
C
C    SUBROUTINE GENPRM( IARRAY, LARRAY )
C               GENerate random PeRMutation of iarray
C
C
C                              Arguments
C
C
C     IARRAY <--> On output IARRAY is a random permutation of its
C                 value on input
C                         INTEGER IARRAY( LARRAY )
C
C     LARRAY <--> Length of IARRAY
C                         INTEGER LARRAY
C
C**********************************************************************
C     .. Scalar Arguments ..
      INTEGER larray
C     ..
C     .. Array Arguments ..
      INTEGER iarray(larray)
C     ..
C     .. Local Scalars ..
      INTEGER i,itmp,iwhich
      Integer*4 k
      Real*4 vtmp
C     ..
C     .. External Functions ..
      EXTERNAL ranmar
C     ..
C     .. Executable Statements ..
      k = 1
      Do i = 1,larray
        Call ranmar(vtmp,k)
        iwhich = Int(vtmp*Real(larray)) + 1
        iwhich = Min(iwhich,larray)  ! if vtmp.eq.1
CCC        iwhich = ignuin(i,larray)
        itmp = iarray(iwhich)
        iarray(iwhich) = iarray(i)
        iarray(i) = itmp
      End Do
      Return

      End
