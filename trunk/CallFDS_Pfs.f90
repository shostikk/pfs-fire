!*********************************************************
!*****   PFS v4.0 FDS fire model subroutine library ***
!*********************************************************
!
! VTT Technical Research Centre of Finland      
! Date:    20.11.2007
! Version: PFS v4.0 
!
! DISCLAIMER: This source file comes as it is. There is no waranty
!             that it should operate 'correctly'.
!
!
!*********************************************************
!
!  Calling field model FDS 
!
!*********************************************************
!
subroutine CallFDS    (RunDir,                      & ! running directory
                       NrO,     NcO,     Out,       & ! Output array (time, HRR, TC's)
                       DataFile,                    & ! FDS input
                       OutFile,                     & ! FDS output
                       CHID,                        & ! CHID string
                       cFDSexe,                     & ! name of the FDS executable (e.g. fds4.exe)
                       FDSVersion,                  & ! FDS version number
                       MaxRunTime,                  & ! Maximum running time (seconds)
                       RunMode)                       ! Normal, debug, savemode

!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: CallFDS
!DEC$ ATTRIBUTES ALIAS:'CallFDS' :: CallFDS
! Simo ei toimi ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:'CallFDS' :: CallFDS
use dflib
use dfwin
!use dflib
!USE IFPORT
!use dfwin
!USE IFWIN
!
! alla omat kokeilut
!use ifwinty
!use dfwin
!use mt
!use kernel32

! Requires libifcoremd.dll

Implicit None
! Argument definitions

character(250), intent(inout)  :: RunDir
integer(2), intent(inout)      :: NrO, NcO
real(8),intent(out)         :: Out(NrO,NcO)
character(120), intent(inout)  :: DataFile, OutFile
character(120), intent(inout)  :: CHID, cFDSexe
integer(4), intent(in)      :: MaxRunTime, RunMode
real(8), intent(in)         :: FDSVersion

!DEC$ ATTRIBUTES REFERENCE :: RunDir
!DEC$ ATTRIBUTES REFERENCE :: NrO, NcO, Out
!DEC$ ATTRIBUTES REFERENCE :: DataFile, OutFile, CHID, cFDSexe

!
!  If called directly from Excel, use this
!  real(8)        Out(NcO,NrO)

! 
! Local variables
!
logical              ioresultL
integer*2            ioresultI
integer*4            ioresultI4, iochannel, iostat,dbgchannel
! Make these strings longer
character(250)        cDir,cData,cOut,cErr,cCHID,FDSexe
character(250)        HRRFile, TCFile, DelFile
character(250)        HDFile, SPKFile
character(250)        EVACFile
character(1)         DataFilePrelim
integer*4            i, j, N,Nread,NHRR,NTmp, N2
character(1)         HdrTmp
logical(1)              Debugmode, Savemode
logical              HRR_found, FirstMissing
character(10)        db_date, db_time
real(8)              TimeVal, HRR, TmpNum

real(8),  allocatable      ::   InData(:,:)
!
! Process calling variables
!
integer                     res
character                   szArgs*512              ! new process arguments buffer & temp pointer
logical(4)                  fSuccess                ! API return code
type(T_STARTUPINFO)         si                      ! used for CreateProcess
type(T_PROCESS_INFORMATION) pi                      ! used for CreateProcess
type(T_INPUT_RECORD)        ir                      ! used for console input
type(T_KEY_EVENT_RECORD)    ker

integer                     dwResult        ! API return code
integer                     dwCreate        ! new process creation flags
integer                     SIZEOFSTARTUPINFO,SIZEOFSzArgs
integer                     SIZESECURITYATTRIBUTES
logical                     InheritHandle
!!!integer(bool)               InheritHandle

SIZEOFSTARTUPINFO = 68
SIZEOFSzArgs = 512
SIZESECURITYATTRIBUTES = 12
dwCreate = CREATE_NEW_CONSOLE
InheritHandle = .FALSE.

   Debugmode = .FALSE.
   Savemode = .FALSE.
   If (abs(RunMode) .eq. 1) Debugmode = .TRUE.
   If (abs(RunMode) .eq. 2) Savemode = .TRUE.

   !------------------------------------------------------------------
   !  Initialize
   !------------------------------------------------------------------
    
   cDir  =  RunDir(1:index(RunDir,char(00))-1)
   cDir  =  Trim(cDir)
   cData =  DataFile(1:index(DataFile,char(00))-1)
   cData =  Trim(cData)
   cOut  =  OutFile(1:index(OutFile,char(00))-1)
   cOut  =  Trim(cOut)
   cCHID =  CHID(1:index(CHID,char(00))-1)
   cCHID =  Trim(cCHID)
   FDSexe=  cFDSexe(1:index(cFDSexe,char(00))-1)
   FDSexe=  Trim(FDSexe)
      DataFilePrelim = " "
!
   iochannel         = 10
   dbgchannel        = 15

   HRRFile  = CHID(1:index(CHID,char(00))-1)//"_hrr.csv"
   TCFile   = CHID(1:index(CHID,char(00))-1)//"_devc.csv"
   EVACFile = CHID(1:index(CHID,char(00))-1)//"_evac.csv"
   cErr     = CHID(1:index(CHID,char(00))-1)//"_err.out"
!
   HRR_found          = .FALSE.

   !------------------------------------------------------------------
   !  Change to Running directory
   !------------------------------------------------------------------
   ioresultL   = CHANGEDIRQQ (cDir) 
   
   if ( Debugmode ) then
      open  (dbgchannel, file = 'CallFDS.log', position='append',iostat = iostat)
      call  date_and_time(db_date, db_time)
      write (dbgchannel,"")
      write (dbgchannel,'("CallFDS  ", 2A11 )') db_date, db_time
      write (dbgchannel,'( "Data dir   : " A)') Trim(cDir)
      write (dbgchannel,'( "Data file  : " A)') Trim(cData)
      write (dbgchannel,'( "Out file   : " A)') Trim(cOut)
      write (dbgchannel,'( "HRR file   : " A)') Trim(HRRFile)
      write (dbgchannel,'( "DEVC file  : " A)') Trim(TCFile)
      write (dbgchannel,'( "EVAC file  : " A)') Trim(EVACFile)
   endif
   !------------------------------------------------------------------
   !  Execute FDS
   !------------------------------------------------------------------
   if (RunMode .ge. 0) then

      call Zeromemory (LOC(szArgs), SIZEOFszArgs)
      call ZeroMemory (LOC (si), SIZEOFSTARTUPINFO)

      if ( Debugmode ) then
         res =lstrcpy(szArgs, "cmd /c """ //Trim(FDSexe)// DataFilePrelim // Trim(cData) //" 2> "// Trim(cErr) // """ "C)
      else
         res =lstrcpy(szArgs, "cmd /c """ //Trim(FDSexe)// DataFilePrelim // Trim(cData) // """ "C)
      endif
      si%dwFlags = STARTF_USESHOWWINDOW
      si%wShowWindow = SW_MINIMIZE
      if ( Debugmode ) then
         write(dbgchannel,'("szArgs is ",a)') szArgs
      endif
      cDir = Trim(cDir) // char(0)

      fSuccess = .FALSE. ! to enter into while loop
      fSuccess = CreateProcess(null_character,    &  ! image file name
                szArgs,                        &  ! command line (including program name)
                NULL_security_attributes,      &  ! security for process
                NULL_security_attributes,      &  ! security for main thread
                InheritHandle,                 &  ! new process inherits handles?
                dwCreate,                      &  ! creation flags
                NULL,                          &  ! environment
                cDir,                          &  ! current Dir
                si,                            &  ! STARTUPINFO structure
                pi)                               ! PROCESSINFORMATION structure
   
      dwResult = WaitForSingleObject(pi%hProcess, MaxRunTime*1000)

      if (dwResult == WAIT_TIMEOUT ) then
         fSuccess = TerminateProcess(pi%hProcess,1)
      endif
   endif

   !------------------------------------------------------------------
   !  Open HRR file
   !------------------------------------------------------------------
   open(iochannel, file = HRRFile, action = 'read', form='formatted',iostat = iostat)
   if ( iostat .ne. 0 ) goto 900
   !------------------------------------------------------------------
   !  Check for empty HRR file
   !------------------------------------------------------------------
   if ( EOF(iochannel) ) then
      if (Debugmode) then
         write (dbgchannel,'( A )') "HRR file empty"
      endif
      close(iochannel)
   else   
   !------------------------------------------------------------------
   !  Read HRR
   !------------------------------------------------------------------
      HRR_found = .TRUE.
      ! read (iochannel, *) N
      read (iochannel, '(A)') HdrTmp
      read (iochannel, '(A)') HdrTmp
      i = 0
      do while ((.NOT. EOF(iochannel)).and.(i<NrO))
         i = i + 1
         ! read(iochannel,*) TimeVal,HRR,(TmpNum,j = 1,N-1)
         read(iochannel,*) TimeVal,HRR
         Out(i,1) = TimeVal
         Out(i,2) = HRR
      enddo
      close(iochannel)
   endif
   NHRR = i
   Nread = 2
!   Header = "FDS Time, HRR (kW)"
   if ( Debugmode) then
      write (dbgchannel,'( I4, A )') NHRR, " rows read from HRR file."
   endif

   !------------------------------------------------------------------
   !  Open DEVC file
   !------------------------------------------------------------------
   if (NcO .gt. Nread) then
      open(iochannel, file = TCFile, action = 'read', form='formatted',iostat = iostat)
      if ( iostat .ne. 0 ) then
         if ( Debugmode) then
            write (dbgchannel,'( A )') "DEVC file couldn't be opened"
         endif
         close(iochannel)
      else
      !------------------------------------------------------------------
      !  Check for empty DEVC file
      !------------------------------------------------------------------
         if ( EOF(iochannel) ) then
            if (Debugmode) then
               write (dbgchannel,'( A )') "DEVC file empty"
               close(iochannel)
            endif
         else  
         !------------------------------------------------------------------
         !  Read DEVC data
         !------------------------------------------------------------------
            ! read (iochannel, *) N
            N = Max(0,NcO-Nread)
            N2 = min(N,NcO-Nread)
            allocate(InData(NrO+1,N2+1))

            ! Count colons
            i = 1
            Do While(.NOT. EOF(iochannel))
               read (iochannel, fmt='(A)', end=902, err=902, &
                    ADVANCE='NO', iostat=iostat, eor=901) HdrTmp
               If (HdrTmp==',') Then
                  i = i + 1
               End If
            End Do
            ! Now i is the number of columns
901         Continue
            iostat = 0
902         Continue
            if ( Debugmode) then
               If (N2 > i-1) Then
                  N2 = i-1
                  write (dbgchannel,'( A,i4,A )') "DEVC file: Only ",n2," data columns read in."
               End If
            Else
               If (N2 > i-1) Then
                  N2 = i-1
               End If
            endif
            if ( iostat .ne. 0 ) then
               if ( Debugmode) then
                  write (dbgchannel,'( A )') "DEVC file: Read error"
               endif
               i = NrO
            else
               i = 0
            endif

            read (iochannel, '(A)') HdrTmp
            i = 0
            do while ((.NOT. EOF(iochannel)).and.(i<NrO))
               i = i + 1
               read(iochannel,*) (InData(i,j),j = 1,N2+1)
            enddo
            if ( Debugmode) then
               write (dbgchannel,'( I4, A )') i, " rows read from DEVC file."
            endif

            NTmp = i
            do j = 1,N2
            do i = 1,NHRR
               call Interpolate1d(NTmp,InData(:,1),InData(:,j+1),Out(i,1),Out(i,Nread+j))
            enddo
            enddo
            close(iochannel)
            Nread = Nread + N2
         endif
         deallocate(InData)
      endif
   endif

   !------------------------------------------------------------------
   !  Open EVAC file
   !------------------------------------------------------------------
   if (NcO .gt. Nread) then
      open(iochannel, file = EVACFile, action = 'read', form='formatted',iostat = iostat)
      if ( iostat .ne. 0 ) then
         if ( Debugmode) then
            write (dbgchannel,'( A )') "EVAC file couldn't be opened"
         endif
         close(iochannel)
      else
      !------------------------------------------------------------------
      !  Check for empty EVAC file
      !------------------------------------------------------------------
         if ( EOF(iochannel) ) then
            if (Debugmode) then
               write (dbgchannel,'( A )') "EVAC file empty"
               close(iochannel)
            endif
         else  
         !------------------------------------------------------------------
         !  Read EVAC data
         !------------------------------------------------------------------
            read (iochannel, *) N
            allocate(InData(NrO+1,N+1))
            N2 = min(N,NcO-Nread)
            read (iochannel, '(A)') HdrTmp
            read (iochannel, '(A)') HdrTmp
            read (iochannel, '(A)') HdrTmp
            i = 0
            do while ((.NOT. EOF(iochannel)).and.(i<NrO))
               i = i + 1
               read(iochannel,*) (InData(i,j),j = 1,N+1)
            enddo
            NTmp = i
            do j = 1,N2
            do i = 1,NHRR
!               call Interpolate1d(NTmp,InData(:,1),InData(:,j+1),Out(i,1),Out(i,Nread+j))
               Out(i,Nread+j) = InData(i,j+1)
            enddo
            enddo
            close(iochannel)
            Nread = Nread + N2
         endif
         deallocate(InData)
      endif
   endif

   !------------------------------------------------------------------
   !  Delete and Close Files. Don't delete if savemode or negative
   !------------------------------------------------------------------
   if ( (.not. Savemode) .and. (RunMode .ge.0)) then
      ioresultI = DELFILESQQ (OutFile) 
      ioresultI = DELFILESQQ (DataFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_err.out"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.csv"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.sf"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.bf"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.s3d"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.iso"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.sz"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.q"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//".end"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//".smv"
      ioresultI = DELFILESQQ (DelFile)
      DelFile = CHID(1:index(CHID,char(00))-1)//"_*.prt5"
      ioresultI = DELFILESQQ (DelFile)
   elseif (Debugmode) then
      write (dbgchannel,'( A )') "Files not deleted"
   endif
   close(dbgchannel)

   !------------------------------------------------------------------
   !  Output ready
   !------------------------------------------------------------------

   !  If called directly from Excel, use this
   !   Out = transpose(Out)


   !------------------------------------------------------------------
   !  Close debugfile
   !------------------------------------------------------------------

   return

900   continue
   if ( Debugmode) then
      write (dbgchannel,'( A )') "ERROR: HRR file couldn't be opened"
      close (dbgchannel)
   end if
   return

910   continue
   if ( Debugmode) then
      write (dbgchannel,'( A )') "ERROR: HRR and TC file times are different."
      close (dbgchannel)
   end if
   return

920   continue
   if ( Debugmode) then
      write (dbgchannel,'( A )') "ERROR: HRR and HD file times are different."
      close (dbgchannel)
   end if
   return

930   continue
   if ( Debugmode) then
      write (dbgchannel,'( A )') "ERROR: HRR and SPK file times are different."
      close (dbgchannel)
   end if
   return



contains
Subroutine Interpolate1d(nx,x,y,xi,ans)
  Integer(4) nx
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


end
