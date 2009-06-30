!*********************************************************
!*****   PFS2 v2.1 Cfast fire model subroutine library ***
!*********************************************************
!
! VTT Building and Transport                          2003
! VTT Technical Research Centre of Finland
!
! Author:  Simo Hostikka, modified by Timo Korhonen
! Date:    19.4.2004
! Version: PFS2 v2.1
! - number of FDATA points is now determined by FDATA variable in Excel
!   (Cfast 5.0  accepts Fdata that is longer than 20 points) 
! - no one room CFAST any more
! - breaking times windows are calculated using the full model
! - if NTargets = 0, zeros are returned for target columns
! - CallMode = 2 (Savemode) > data and result files are not deleted (except hi)
! 
! DISCLAIMER: This source file comes as it is. There is no waranty
!             that it should operate 'correctly'.
!
!*********************************************************
!
!  Calling zone model CFAST directly (does not use a batch file)
!
!*********************************************************
!
!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: CallCfast
!DEC$ ATTRIBUTES ALIAS:'CallCfast' :: CallCfast
subroutine CallCfast  (cRunDir,                    &
                       cCfastDir,                  & ! Directory containing cfast.exe
                       NrO,     NcO,     Out,      & ! Basic Output array (time, Tup, Tlow, Zi)
                       NRooms,                     & ! Number of rooms
                       NrTimes, NcTimes, Times,    & ! Time parameters for CFAST
                       NrIndx, NcIndx, RoomIndx,   & ! Room numbers of interest
                       DatFile, HistoryFile,       & ! Cfast directory and files
                       ThDbCreate,                 & ! Create thermal data base flag
                       ThDbFile,                   & ! Thermal data base file
                       NTargets,                   & ! Number of targets defined, excluding the floor targets
                       NDetectors,                 & ! Number of detectors inside the building
                       RunMode,                    & ! Normal, debug, savemode
                       MaxRunTime,                 & ! Maximum running time (seconds)
                       BreakingWindows,            & ! Indicator of Breaking windows
                       NrTLim, NcTLim, TLimits,    & ! Detector activation temp, window breaking temp.
                       NrBreak, NcBreak, BrkTime,  & ! Breaking times of windows
                       NrHVO, NcHVO, HVO)            ! HVENT Soffit

use dflib
use dfwin

!
! Argument definitions
!
integer(2)     NrO, NcO
!  If called directly from Excel, use this
!  real(8)        Out(NcO,NrO)
real(8)        Out(NrO,NcO)
integer(2)     NrTimes, NcTimes
real(8)        Times(NrTimes, NcTimes)
integer(2)     NrIndx, NcIndx
real(8)        RoomIndx(NrIndx,NcIndx)
integer(4)     NRooms, NTargets, NDetectors, RunMode, MaxRunTime
character(30)  DatFile, HistoryFile, ThDbFile
character(120) cRunDir,cCfastDir
logical        ThDbCreate
logical        BreakingWindows
integer(2)     NrTLim, NcTLim
real(8)        TLimits(NrTLim, NcTLim) ! DetectorActivationTemperatures, WindowBreaking temperature
integer(2)     NrBreak, NcBreak
real(8)        BrkTime(NrBreak,NcBreak)
integer(2)     NrHVO, NcHVO
real(8)        HVO(NrHVO,NcHVO)

!DEC$ ATTRIBUTES REFERENCE :: NrO, NcO, Out
!DEC$ ATTRIBUTES REFERENCE :: NrTimes, NcTimes, Times
!DEC$ ATTRIBUTES REFERENCE :: cRunDir, cCfastDir,DatFile, HistoryFile, ThDbFile
!DEC$ ATTRIBUTES REFERENCE :: NrIndx, NcIndx, RoomIndx
!DEC$ ATTRIBUTES REFERENCE :: NrHVO, NcHVO, HVO
!DEC$ ATTRIBUTES REFERENCE :: NrTLim, NcTLim, TLimits
!DEC$ ATTRIBUTES REFERENCE :: NrBreak, NcBreak, BrkTime
!DEC$ ATTRIBUTES REFERENCE :: BreakingWindows

! 
! Local variables
!
logical              ioresultL
integer*2            ioresultI
integer*4            ioresultI4, iochannel, iostat, outchannel
character(120)       cTmpDir,CfastExe,ReportssExe
character(30)        ResultFile, ThDbIndxFile, TmpStr, cTemp, cTmpDat
integer*4            i, j, k, ResCol, ii, col
integer*4            ResultRoomStart,ResultTargetStart,ResultDetectorStart
integer*4            NColsPerRoom, NColsPerTarget, NColsForFire, NColsPerDetector
integer*4            TargetStart, DetectorStart
integer*4            nchar, TitleLen, NVars, NTimesMax, NTimes, NO

logical              Debug, WBroken, Savemode
character(10)        db_date, db_time
integer*4            db_statb(12)
integer*4            dbgchannel

real*8               Zi1, Zi2, Tup1, Tup2, TempVal, TempTime
character*121 timodir
character*12 Fmtstr

character,allocatable      ::   InStr(:)
real(8),  allocatable      ::   InData(:,:), TempArray(:,:) 

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
integer                     SIZEOFSTARTUPINFO
integer                     SIZESECURITYATTRIBUTES
  character*512 timochar

SIZEOFSTARTUPINFO = 68
SIZEOFSzArgs = 512
SIZESECURITYATTRIBUTES = 12
dwCreate = CREATE_NEW_CONSOLE

   !------------------------------------------------------------------
   !  Initialize
   !------------------------------------------------------------------
    
   cTmpDir     = ""
   cTmpDir     = cRunDir(1:index(cRunDir,char(00))-1)
   cTmpDir     = Trim(cTmpDir)
   cTmpDat     = DatFile(1:index(DatFile,'.')-1) // 'a.dat'

   CfastExe    = ""
   CfastExe    = cCfastDir(1:index(cCfastDir,char(00))-1)
   if (index(CfastExe,'\',BACK = .TRUE.) .EQ. len_trim(CfastExe)) &
      CfastExe = CfastExe(1:len_trim(CfastExe)-1)
   ReportssExe = Trim(CfastExe)//"\reportss.exe"
   CfastExe    = Trim(CfastExe)//"\cfast.exe"

   iochannel         = 10
   outchannel        = 20
   dbgchannel        = 15
   ResCol            = 0

   NColsPerRoom      = 7 
   NColsPerTarget    = 6
   NColsPerDetector  = 3
   NColsForFire      = 5
   NTimesMax         = int(Times(1,1)) + 1

   ResultRoomStart       = 2
   ResultTargetStart     = 11
   ResultDetectorStart   = 17

   TitleLen       = (NRooms * NColsPerRoom + NColsForFire &
                  + (NTargets + NRooms) * NColsPerTarget  &
                  + NDetectors * NColsPerDetector) * 25

   TargetStart   = 1 + NRooms * NColsPerRoom + NColsForFire + 1
   if (NTargets .gt. 0) then
      DetectorStart  = TargetStart + (NTargets+NRooms) * NColsPerTarget
   else
      DetectorStart  = TargetStart
   end if   
      
   cTemp = ""
   cTemp = DatFile(1:index(DatFile,char(00))-1)
   ResultFile   = cTemp(1:index(cTemp,"."))//"txt"
   ThDbIndxFile = ThDbFile(1:index(ThDbFile,char(00))-1)//'.ndx'

   if (RunMode .eq. 1) then
      Debug = .true.
   else
      Debug = .false.
   end if
   If (RunMode .eq. 2) Then
      Savemode = .true.
      Debug = .true.
   Else
      Savemode = .false.
   End If


   !------------------------------------------------------------------
   !  Delete existing ResultFile
   !------------------------------------------------------------------
   ioresultI = DELFILESQQ (ResultFile) 
   
!   !**** 
!   If ( Savemode ) Then
!      open  (outchannel, file = 'CallCFast_savefile.txt', position='append',iostat = iostat)
!   End If
   !------------------------------------------------------------------
   !  Create logfile
   !------------------------------------------------------------------
   !
   if ( Debug ) then
      open  (dbgchannel, file = 'CallCFast.log', position='append',iostat = iostat)
      ioresultI4 = fstat(dbgchannel,db_statb)
      call  date_and_time(db_date, db_time)
      write (dbgchannel,"")
      write (dbgchannel,'( "CallCfast  ", 2A11 )') db_date, db_time
      write (dbgchannel,'( "CfastExe     ", A)') CfastExe
      write (dbgchannel,'( "Datfile    : ", A)') DatFile(1:index(DatFile,char(00))-1)
      write (dbgchannel,'( "Historyfile: ", A)') HistoryFile(1:index(HistoryFile,char(00))-1)
      write (dbgchannel,'( "NrO: " I)') NrO
      write (dbgchannel,'( "NcO: " I)') NcO
      write (dbgchannel,'( "RoomIndx: " <NcIndx>I)') (int(RoomIndx(1,i)), i = 1,NcIndx)
   end if
   !**** 

   !------------------------------------------------------------------
   !  Execute CFAST 
   !------------------------------------------------------------------
   call zeromemory (LOC(szArgs), SIZEOFszArgs)
   call ZeroMemory (LOC (si), SIZEOFSTARTUPINFO)

   timodir = Trim(cTmpDir)//Char(0)
   
   szArgs =  CfastExe// Trim(cTemp)//Char(0)  
   If (Debug) write (dbgchannel,'("szArgs is ",a)') szArgs
   si%dwFlags = STARTF_USESHOWWINDOW
   si%wShowWindow = SW_MINIMIZE

   fSucess = .FALSE. ! to enter into while loop
   fSuccess = CreateProcess(null_character,    &  ! image file name
                szArgs,                        &  ! command line (including program name)
                NULL_security_attributes,      &  ! security for process
                NULL_security_attributes,      &  ! security for main thread
                .FALSE.,                       &  ! new process inherits handles?
                dwCreate,                      &  ! creation flags
                NULL,                          &  ! environment
                timodir,                      &  ! new current DirName
                si,                            &  ! STARTUPINFO structure
                pi)                               ! PROCESSINFORMATION structure
   
   dwResult = WaitForSingleObject(pi%hProcess, MaxRunTime*1000)

   if (dwResult == WAIT_TIMEOUT ) then
      if ( Debug ) then
      write (dbgchannel,'("Cfast timeout")') 
      end if
      fSuccess = TerminateProcess(pi%hProcess,1)
   else
      if ( Debug ) then
         write (dbgchannel,'("Cfast executed")') 
      end if
   end if

   res = CloseHandle( pi.hThread )
   res = CloseHandle( pi.hProcess )


   !------------------------------------------------------------------
   !  Execute REPORTSS
   !------------------------------------------------------------------
   call zeromemory (LOC(szArgs), SIZEOFszArgs)
   call ZeroMemory (LOC (si), SIZEOFSTARTUPINFO)
   
   szArgs = ReportssExe //" /r:n "// &
        Trim(HistoryFile(1:index(HistoryFile,char(00))-1))//" "//Trim(ResultFile)//Char(0)

   si%dwFlags = STARTF_USESHOWWINDOW
   si%wShowWindow = SW_MINIMIZE

   If (Debug) write (dbgchannel,'("szArgs is ",a)') szArgs


   fSucess = .FALSE. ! to enter into while loop
   fSuccess = CreateProcess(null_character,    &  ! image file name
                szArgs,                        &  ! command line (including program name)
                NULL_security_attributes,      &  ! security for process
                NULL_security_attributes,      &  ! security for main thread
                .FALSE.,                       &  ! new process inherits handles?
                dwCreate,                      &  ! creation flags
                NULL,                          &  ! environment
                timodir,                       &  ! new current DirName
                si,                            &  ! STARTUPINFO structure
                pi)                               ! PROCESSINFORMATION structure

   dwResult = WaitForSingleObject(pi%hProcess, -1)
   ioresultI  = DELFILESQQ(HistoryFile) 
   res = CloseHandle( pi.hThread )
   res = CloseHandle( pi.hProcess )
   if ( Debug ) then
      write (dbgchannel,'("Reportss executed")') 
   end if

   !------------------------------------------------------------------
   !  Open and Read the result file
   !------------------------------------------------------------------
   open(iochannel, file = ResultFile, action = 'read', form='formatted',iostat = iostat)
   if ( iostat .ne. 0 ) goto 900

   !**** 
   if ( Debug ) then
      write (dbgchannel,'( A )') "Result file opened"
   end if
   !**** 

   !------------------------------------------------------------------
   !  Check for empty result file
   !------------------------------------------------------------------
   if ( EOF(iochannel) ) then
      if (Debug) write (dbgchannel,'( A )') "Result file empty"
      if (Debug) close(dbgchannel)
      close(iochannel)
      return
   end if
   
   !------------------------------------------------------------------
   !  Count the number of variables
   !------------------------------------------------------------------
   allocate(InStr(TitleLen))
   InStr = ""
   read(iochannel,'(Q, <TitleLen>A1)') nchar, (InStr(i), i = 1, nchar)
   NVars = count((InStr .eq. ",")) + 1

   if ( Debug ) write (dbgchannel,'( A, I)') "NVars = ", NVars
   
   if ( NVars .lt. 7 ) goto 910
   
   !------------------------------------------------------------------
   !  Allocate and read input matrix
   !------------------------------------------------------------------
   allocate(InData(NTimesMax,NVars))   
   i = 0
   do while (.NOT. EOF(iochannel))
      i = i + 1
      read (iochannel, *) (InData(i,j), j = 1, NVars)
   end do
   NTimes = i
   
   !**** 
   Fmtstr = ''
   If ( NVars .ge. 10 ) Then
      Write(Fmtstr,'(a,i2)') '(',NVars
   Else
      Write(Fmtstr,'(a,i1)') '(',NVars
   End If
   Fmtstr = Trim(Adjustl( Trim(Adjustl(Fmtstr)) // 'e12.6)' ))

   if ( Debug ) write (dbgchannel,'(3A)') "Fmtstr = ", Fmtstr,'***'

!!$   If ( Savemode ) Then
!!$      write(outchannel,*) 'Number of Lines is ',Int(NTimes/30) + 1
!!$      write(outchannel,'(Q, <TitleLen>A1)') nchar, (InStr(i), i = 1, nchar)
!!$      Do i = 1, NTimes, 30
!!$         write (outchannel, fmt=Trim(Fmtstr)) (InData(i,j), j = 1, NVars)
!!$      End Do
!!$      close(outchannel)
!!$   End If

   deallocate(InStr)

   if ( Debug ) write (dbgchannel,'( A, I)') "NTimes = ", NTimes

   !------------------------------------------------------------------
   !  Close and delete files
   !------------------------------------------------------------------
   close(iochannel)

   if ( Debug ) write (dbgchannel,'( A )') "Result file closed"

   if ( .not. Savemode) then
      ioresultI = DELFILESQQ (ResultFile) 
      ioresultI = DELFILESQQ (DatFile) 
      TmpStr = ThDbFile(1:index(ThDbFile,char(00))-1)//'.df'
      ioresultI = DELFILESQQ (TmpStr) 
      ioresultI = DELFILESQQ (ThDbIndxFile) 
      if (Debug) write (dbgchannel,'( A )') "Files deleted"
   else
      if (Debug) write (dbgchannel,'( A )') "Files not deleted"
   end if

   !------------------------------------------------------------------
   !  Parse InData for output
   !------------------------------------------------------------------

   allocate(TempArray(NrO,NcO))
   TempArray = 0.0

   !------------------------------------------------------------------
   ! Loop over result times
   !------------------------------------------------------------------
   
   TempTime = 0.0
   do ii = 1,NrO

   !------------------------------------------------------------------
   !  Time, Tup, Tlow and Zi
   !------------------------------------------------------------------
   ResCol = 1
   TempArray(ii,ResCol) = TempTime

   ResCol = ResultRoomStart
   do i = 1,3
      if ((RoomIndx(1,i).gt.0).and.(RoomIndx(1,i).le.NRooms)) then
         col = (int(RoomIndx(1,i))-1) * NColsPerRoom + 2
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1

         col = (int(RoomIndx(1,i))-1) * NColsPerRoom + 3 
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1

         col = (int(RoomIndx(1,i))-1) * NColsPerRoom + 4
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1
      else
         TempArray(ii,ResCol) = 0.
         ResCol = ResCol + 1
         TempArray(ii,ResCol) = 0.
         ResCol = ResCol + 1
         TempArray(ii,ResCol) = 0.
         ResCol = ResCol + 1
      endif
   end do
   !------------------------------------------------------------------
   !  Target temperature and flux
   !------------------------------------------------------------------
   if ( NTargets .gt. 0 ) then
      !
      ! Target temperature
      !
      ResCol = ResultTargetStart
      k = TargetStart
      do i = 1,NTargets
         col = k
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1
         k = k + NColsPerTarget
      end do
      !
      ! Target flux
      !
      ResCol = ResultTargetStart + 3
      k = TargetStart + 1
      do i = 1,NTargets
         col = k
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1
         k = k + NColsPerTarget
      end do

   Else
      !
      ! No targets, return mdot and RHRmain (two first places for target 1 used)
      !
!      ResCol = ResultTargetStart
!      k = TargetStart - NColsForFire+ 1 ! first fire output is 'main plume flow', then mdot,RHRmain
!      do i = 1,2
!         col = k
!         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
!         TempArray(ii,ResCol) = TempVal
!         ResCol = ResCol + 1
!         k = k + 1
!      end do
   end if

   !------------------------------------------------------------------
   !  Detector temperature, ceiling jet temperature and velocity
   !------------------------------------------------------------------
   if ( NDetectors .gt. 0 ) then
       !**** 
       if ( Debug .and. (ii.eq.1)) then
         write (dbgchannel,'( A, 6F8.2, A)') &
         "Checking detectors. Tact = ", (TLimits(1,i), i = 1,6), " K."
       end if
      !**** 
      !
      ! Detector temperature
      !
      ResCol = ResultDetectorStart
      k = DetectorStart
      do i = 1,NDetectors
         col = k
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1
         k = k + NColsPerDetector
      end do
      !
      ! Ceiling jet temperature
      !
      ResCol = ResultDetectorStart + 6
      k = DetectorStart + 1
      do i = 1,NDetectors
         col = k
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1
         k = k + NColsPerDetector
      end do
      !
      ! Ceiling jet velocity
      !
      ResCol = ResultDetectorStart + 6 + 6
      k = DetectorStart + 2
      do i = 1,NDetectors
         col = k
         call INTERPOL(NTimes,TempTime,InData(1:NTimes,col),InData(1:NTimes,1),TempVal)
         TempArray(ii,ResCol) = TempVal
         ResCol = ResCol + 1
         k = k + NColsPerDetector
      end do
   end if

   TempTime = TempTime + Times(1,1)/dble(NrO-1)
   enddo
   !------------------------------------------------------------------
   !  Check detector activation times
   !------------------------------------------------------------------
   ResCol = NcO
   k = DetectorStart
   do i = 1,NDetectors
      call INTERPOL(NTimes,TLimits(1,i),InData(1:NTimes,1),InData(1:NTimes,k),TempVal)
      ! check for interrupted calculation
      if ((TempVal         .eq. InData(NTimes,1)) .and. &
          (InData(NTimes,1).lt. Times(1,1))) then
          TempVal = Times(1,1)
      endif
      TempArray(i,ResCol) = TempVal
      k = k + NColsPerDetector
      !**** 
      if ( Debug ) then
        write (dbgchannel,'( A, I3, A, F8.2, A)') &
        "Detector ", i, " activation time = ", TempVal, " s."
      end if
     !**** 
   enddo
   do i = NDetectors+1,6
      TempArray(i,ResCol) = Times(1,1)
   enddo

   !------------------------------------------------------------------
   !  Output ready
   !------------------------------------------------------------------
   Out = dble(TempArray)
   !  If called directly from Excel, use this
   !   Out = transpose(TempArray)

   !------------------------------------------------------------------
   !  Check window breakings
   !------------------------------------------------------------------
   if (BreakingWindows) then

      if ( Debug ) write (dbgchannel,'( A, F6.2, A)') "Checking window breakings. Tbreak = ", TLimits(1,7), " C."

      WBroken = .false.
      k = 1
      do while ((k.lt.NTimes) .and. (.not. WBroken))
         k = k + 1
         do i = 1, NRooms
         do j = i + 1, NRooms + 1
            if (BrkTime(i,j) .lt. 0.0) then
               if (i .le. NRooms) then
                  Tup1 = InData(k,(i-1) * NColsPerRoom + 2 )
                  Zi1  = InData(k,(i-1) * NColsPerRoom + 4 )
                  Tup2 = InData(k,(j-1) * NColsPerRoom + 2 )
                  Zi2  = InData(k,(j-1) * NColsPerRoom + 4 )
                  if ( ((Zi1 .lt. HVO(i,j)).and.(Tup1 .ge. TLimits(1,7))) &
                   .or.((Zi2 .lt. HVO(i,j)).and.(Tup2 .ge. TLimits(1,7))) ) then
                     BrkTime(i,j) = InData(k,1)
                     WBroken = .true.
                     !**** 
                     if ( Debug ) then
                        write (dbgchannel,'( A, I3,A,I3, A, F8.1, A)') &
                        "Window from ", j, " to ", i, " opened at ", BrkTime(i,j), " s."
                     end if
                     !**** 
                  end if
               else
                  Tup1 = InData(k,(i-1) * NColsPerRoom + 2 )
                  Zi1  = InData(k,(i-1) * NColsPerRoom + 4 )
                  if ( ((Zi1 .lt. HVO(i,j)).and.(Tup1 .ge. TLimits(1,7))) ) then
                     BrkTime(i,j) = InData(k,1)
                     WBroken = .true.
                     !**** 
                     if ( Debug ) then
                        write (dbgchannel,'( A, I3,A,I3, A, F8.1, A)') &
                        "Window from ", j, " to ", i, " opened at ", BrkTime(i,j), " s."
                     end if
                     !**** 
                  end if
               end if
            end if
         enddo
         enddo
      enddo
      BreakingWindows = WBroken
   Else
      ! delete the one room dat file (it is not deleted before if
      ! windows are not breaking, because then one room calculation
      ! is not called.)
      ioresultI = DELFILESQQ (cTmpDat) 
   end if
   
   deallocate(TempArray)
   deallocate(InData)

   !------------------------------------------------------------------
   !  Close debugfile
   !------------------------------------------------------------------
   if ( Debug ) close(dbgchannel)

   return


900   continue
   if ( Debug) then
      write (dbgchannel,'( A )') "Result file couldn't be opened"
      close (dbgchannel)
   end if
   return


910   continue
   if ( Debug) then
      write (dbgchannel,'( A )') "Error in CFAST results"
      close (dbgchannel)
   end if
   return

contains

      SUBROUTINE INTERPOL(NPAR,X,FTAB,XTAB,FX)
      INTEGER*4 NPAR
      REAL*8 X, FX
      REAL*8 XTAB(NPAR),FTAB(NPAR)

      INTEGER*4 I, J
      REAL*8 EPSILON

      EPSILON=1.D-7

      IF (DABS(X-XTAB(1)).LE.EPSILON) X=XTAB(1)

      IF (X .LE. MINVAL(XTAB)) THEN
         FX = FTAB(1)
!         FX = FTAB(MINLOC(XTAB,DIM=1))
         RETURN
      END IF

      IF (X .GT. MAXVAL(XTAB))THEN
         FX = FTAB(NPAR)
!         FX = FTAB(MAXLOC(XTAB,DIM=1))
         RETURN
      END IF   

      J = 1
      do while (XTAB(J) .LT. X)
         J = J + 1
      end do

      I = J - 1
      X1=XTAB(I)
      X2=XTAB(J)

      F1=FTAB(I)
      F2=FTAB(J)

      DFXIJ=(F2-F1)/(X2-X1)
      FX=DFXIJ*(X-X1)+F1

      RETURN
      END SUBROUTINE INTERPOL

end

!------------------------------------------------------------------
!
!  Write CFAST Dat -file
!
!------------------------------------------------------------------

!
!DEC$ ATTRIBUTES STDCALL,DLLEXPORT        :: WriteCfastData
!DEC$ ATTRIBUTES ALIAS:'WriteCfastData'   :: WriteCfastData

subroutine WriteCfastData (   cRunDir,       &  ! running directory
                              DatFile,       &  ! 1 Cfast datfile
                              HiFile,        &  ! 2 History file
                              ThDbFile,      &  ! 3 Thermal data base
                              NrIndexAr,     &  ! 4 Rows of the sizearray
                              NcIndexAr,     &  ! 5 Columns of the sizearray
                              IndexAr,       &  ! 6 Array of array sizes
                              NrTimes,       &  ! 7 Rows of Times
                              NcTimes,       &  ! 8 Columns of Times
                              Times,         &  ! 9 TIMES keyvector
                              NrTamb,        &  ! 10 Rows of Tamb
                              NcTamb,        &  ! 11 Cols of Tamb
                              Tamb,          &  ! 12 TAMB  keyvector
                              NrRD,          &  ! 13 Rows of RD
                              NcRD,          &  ! 14 Cols of RD
                              RD,            &  ! 15 Room data     
                              NrHVW,         &  ! 16 Rows of HVW
                              NcHVW,         &  ! 17 Cols of HVW
                              HVW,           &  ! 18 HVENT WIDTH
                              NrHVO,         &  ! 19 Rows of HVO
                              NcHVO,         &  ! 20 Cols of HVO
                              HVO,           &  ! 21 HVENT Soffit
                              NrHVI,         &  ! 22 Rows of HVI
                              NcHVI,         &  ! 23 Cols of HVI
                              HVI,           &  ! 24 HVENT Sill
                              NrCV,          &  ! 25 Rows of CV
                              NcCV,          &  ! 26 Cols of CV
                              CV,            &  ! 27 CVENT times
                              NrFdata,       &  ! 28 Rows of Fdata
                              NcFdata,       &  ! 29 Cols of Fdata
                              Fdata,         &  ! 30 Fire time series data
                              NrCeili,       &  ! 31 Rows of Ceili
                              NcCeili,       &  ! 32 Cols of Ceili
                              Ceili,         &  ! 33 Ceiling and wall materials
                              NrChemi,       &  ! 34 Rows of Chemi
                              NcChemi,       &  ! 35 Cols of Chemi
                              Chemi,         &  ! 36 CHEMI keyword
                              NrFpos,        &  ! 37 Rows of Fpos
                              NcFpos,        &  ! 38 Cols of Fpos
                              Fpos,          &  ! 39 FPOS keyword
                              Cjet,          &  ! 40 CJET keyword
                              Species1,      &  ! 41 Species1 
                              Species2,      &  ! 42 FDATA Species2
                              DtCheckXval,   &  ! 43 Run control
                              DtCheckNsteps, &  ! 44 Run control
                              ThDbCreate,    &  ! 45 Flag for creation of thermal data base
                              ThDbNames,     &  ! 46 Thermal data base material names
                              NrThDb,        &  ! 47 Rows for ThDb
                              NcThDb,        &  ! 48 Cols for ThDb
                              ThDb,          &  ! 49 Thermal data base
                              NrTargets,     &  ! 50 Rows for targets
                              NcTargets,     &  ! 51 Cols for targets
                              Targets,       &  ! 52 Target data
                              NrDetect,      &  ! 53 Rows for detect
                              NcDetect,      &  ! 54 Cols for detect
                              Detect,        &  ! 55 Detector data
                              NrFans,        &  ! 56 Rows for fans
                              NcFans,        &  ! 57 Cols for fans
                              Fans)             ! 58 Fan data

use dflib

!DEC$ ATTRIBUTES REFERENCE :: cRunDir,DatFile
!DEC$ ATTRIBUTES REFERENCE :: HiFile
!DEC$ ATTRIBUTES REFERENCE :: ThDbFile
!DEC$ ATTRIBUTES REFERENCE :: NrIndexAr,NcIndexAr,IndexAr
!DEC$ ATTRIBUTES REFERENCE :: NrTimes,NcTimes,Times
!DEC$ ATTRIBUTES REFERENCE :: NrTamb,NcTamb,Tamb
!DEC$ ATTRIBUTES REFERENCE :: NrRD,NcRD,RD
!DEC$ ATTRIBUTES REFERENCE :: NrHVW,NcHVW,HVW
!DEC$ ATTRIBUTES REFERENCE :: NrHVO,NcHVO,HVO
!DEC$ ATTRIBUTES REFERENCE :: NrHVI,NcHVI,HVI
!DEC$ ATTRIBUTES REFERENCE :: NrCV,NcCV,CV
!DEC$ ATTRIBUTES REFERENCE :: NrFdata,NcFdata,Fdata
!DEC$ ATTRIBUTES REFERENCE :: NrCeili,NcCeili,Ceili
!DEC$ ATTRIBUTES REFERENCE :: NrChemi,NcChemi,Chemi
!DEC$ ATTRIBUTES REFERENCE :: NrFpos,NcFpos,Fpos
!DEC$ ATTRIBUTES REFERENCE :: Cjet
!DEC$ ATTRIBUTES REFERENCE :: Species1, Species2
!DEC$ ATTRIBUTES REFERENCE :: ThDbNames
!DEC$ ATTRIBUTES REFERENCE :: NrThDb,NcThDb,ThDb
!DEC$ ATTRIBUTES REFERENCE :: NrTargets,NcTargets,Targets
!DEC$ ATTRIBUTES REFERENCE :: NrDetect,NcDetect,Detect
!DEC$ ATTRIBUTES REFERENCE :: NrFans,NcFans,Fans

implicit none
!
! Arguments
!
character(30)  DatFile, HiFile, ThDbFile
character(120) cRunDir
integer(2)     NrIndexAr, NcIndexAr
integer(2)     IndexAr(NrIndexAr, NrIndexAr)
integer(4)     DtCheckNsteps 
real(8)        DtCheckXval
integer(2)     NrTimes, NcTimes
real(8)        Times(NrTimes,NcTimes)
integer(2)     NrTamb, NcTamb
real(8)        Tamb(NrTamb,NcTamb)
integer(2)     NrRD, NcRD
real(8)        RD(NrRD,NcRD)
integer(2)     NrHVW, NcHVW
real(8)        HVW(NrHVW,NcHVW)
integer(2)     NrHVO, NcHVO
real(8)        HVO(NrHVO,NcHVO)
integer(2)     NrHVI, NcHVI
real(8)        HVI(NrHVI,NcHVI)
integer(2)     NrCV, NcCV
real(8)        CV(NrCV,NcCV)
integer(2)     NrFdata, NcFdata
real(8)        Fdata(NrFdata,NcFdata)
integer(2)     NrCeili, NcCeili
real(8)        Ceili(NrCeili,NcCeili)
integer(2)     NrChemi, NcChemi
real(8)        Chemi(NrChemi,NcChemi)
integer(2)     NrFpos, NcFpos
real(8)        Fpos(NrFpos,NcFpos)
character(10)  Cjet
character(5)   Species1, Species2
logical        ThDbCreate
character(184) ThDbNames
integer(2)     NrThDb, NcThDb
real(8)        ThDb(NrThDb,NcThDb)
integer(2)     NrTargets, NcTargets
real(8)        Targets(NrTargets,NcTargets)
integer(2)     NrDetect, NcDetect
real(8)        Detect(NrDetect,NcDetect)
integer(2)     NrFans, NcFans
real(8)        Fans(NrFans,NcFans)
! 
! Local variables
!
integer(4)                 NRooms, NTargets, NDetectors, NFans, Lfbo, Lfbt
logical                    ioresultL
integer*2                  ioresultI
integer*4                  ioresultI4, iochannel, iostat, thchannel
integer*2                  NThermal
character(128)             CfastDir
character(120)             cTmpDir
character(30)              TmpStr
integer*4                  i, j, k, ii, FtimeLen, NO, MaxTimeSteps
integer*4                  result, lbound, ubound, StrLen
real                       wind
real, allocatable ::       CVFactor(:), CVLocal(:,:)
character*8, allocatable :: ThermalIndex(:)

   cTmpDir = ""
   cTmpDir=cRunDir(1:index(cRunDir,char(00))-1)
   cTmpDir=Trim(cTmpDir)

   wind = 0.0
   NThermal = 100
   MaxTimeSteps = 21
!   NO = min(MaxTimeSteps, NrFdata)
   NO = NrFdata
   FtimeLen = NO-1

   iochannel = 12
   thchannel = 13
!
!  Set integer indexes
!
   NRooms      = IndexAr(1,1)
   NTargets    = IndexAr(1,2)
   NDetectors  = IndexAr(1,3)
   NFans       = IndexAr(1,4)
   Lfbo        = IndexAr(1,5)
   Lfbt        = IndexAr(1,6)
!---------------------------------------------------------------------------------------
!  Change to Cfast drive and directory
!---------------------------------------------------------------------------------------
   ioresultI4  = GETENVQQ ("CFAST_DIR", CfastDir) 
!   ioresultL   = CHANGEDIRQQ (CfastDir) 
   ioresultL   = CHANGEDIRQQ (Trim(cTmpDir)) 

   !
   !---------------------------------------------------------------------------------------
   !  Create Thermal DataBase
   !---------------------------------------------------------------------------------------
   !
   if ( ThDbCreate ) then
!
      NThermal = size(ThDb,1)
      allocate(ThermalIndex(0:NThermal))
      do i = 1,NThermal
         j = (i-1)*8 + 1
         ThermalIndex(i) = ThDbNames(j:j+7)
      end do
      ThermalIndex(0) = 'GYPSUM'
      !---------------------------------------------------------------------------------------
      ! Create thermal index file
      !---------------------------------------------------------------------------------------
!
!  Index file does not need to be created !!
!
      if (.false.) then
      TmpStr = ThDbFile(1:index(ThDbFile,char(00))-1)//'.ndx'
      j = DELFILESQQ (TmpStr) 
      open  (thchannel, file = TmpStr, action = 'write', form='formatted',iostat = iostat)
      do i = 1,NThermal
         write (thchannel, '(A8 " " I1)') ThermalIndex(i), i
      end do
      close (thchannel)      
      endif
      !---------------------------------------------------------------------------------------
      ! Create thermal data base
      !---------------------------------------------------------------------------------------
      TmpStr = ThDbFile(1:index(ThDbFile,char(00))-1)//'.df'
      open  (thchannel, file = TmpStr, action = 'write', form='formatted',iostat = iostat)
      do i = 1,NThermal
         write (thchannel, '(A " " F8.3, F8.1, F8.1, F8.4, F8.3, 7F10.4)') ThermalIndex(i), (ThDb(i,j), j=1,12) 
      end do
      close (thchannel)      
   end if
   !
   !---------------------------------------------------------------------------------------
   !  Create datfile
   !---------------------------------------------------------------------------------------
   !
   open  (iochannel, file = DatFile, action = 'write', form='formatted',iostat = iostat)
!  VERSN
   write (iochannel,'("VERSN    3 " A)') DatFile(1:index(DatFile,char(00))-1)
!  TIMES
   write (iochannel,'("TIMES    " <NcTimes>F11.3 )') Times(1,1), Times(2,1), Times(3,1), Times(4,1), Times(5,1)
!  TAMB
   write (iochannel,'("TAMB     " <NcTamb>F11.2 )') (sngl(Tamb(i,1)), i = 1,NcTamb)
!  EAMB
   write (iochannel,'("EAMB     " <NcTamb>F11.2 )') (sngl(Tamb(i,1)), i = 1,NcTamb)
!  DUMPR
   write (iochannel,'("DUMPR    " A )') HiFile(1:index(HiFile,char(00))-1)
!  THRMF
   TmpStr = ThDbFile(1:index(ThDbFile,char(00))-1)//'.df'
   write (iochannel,'("THRMF    " A )') TmpStr
!  CJET
   write (iochannel,'("CJET     " A )') Cjet(1:index(Cjet,char(00))-1)
!  HI/F
   write (iochannel,'("HI/F   " <NRooms>F7.2 )') (sngl(RD(1,i)), i = 1,NRooms)
!  WIDTH
   write (iochannel,'("WIDTH  " <NRooms>F7.2 )') (sngl(RD(2,i)), i = 1,NRooms)
!  DEPTH
   write (iochannel,'("DEPTH  " <NRooms>F7.2 )') (sngl(RD(3,i)), i = 1,NRooms)
!  HEIGHT
   write (iochannel,'("HEIGH  " <NRooms>F7.2 )') (sngl(RD(4,i)), i = 1,NRooms)
!---------------------------------------------------------------------------------------
!  HVENT
!---------------------------------------------------------------------------------------
   do i = 1, NRooms+1
   do j = 1, NRooms+1
      if ( ( HVW(i,j) .gt. 0.0 ) .and. ( i .ne. j ) ) then
         if ( j .gt. i ) then
            k = 1
         else
            k = 2
         end if
         if ( i .le. NRooms .and. j .le. NRooms ) then
            write (iochannel,'("HVENT " 3I3, 3F8.2 )') min(i,j), max(i,j), k, HVW(i,j), HVO(i,j), HVI(i,j)  
         else 
            write (iochannel,'("HVENT " 3I3, 4F8.2 )') min(i,j), max(i,j), k, HVW(i,j), HVO(i,j), HVI(i,j), wind
         end if
      end if
   end do
   end do
!---------------------------------------------------------------------------------------
!  CVENT
!---------------------------------------------------------------------------------------
   allocate(CVFactor(1:NO))
   allocate(CVLocal(1:NrCV,1:NcCV))
   CVLocal = CV
   !---------------------------------------------------------------------------------------
   ! Loop over the vents
   !---------------------------------------------------------------------------------------

   ! Negative time means that window can be broken by heat

   do i = 1, NRooms+1
   do j = 1, NRooms+1
      if (CVLocal(i,j) .lt. 0.0 ) CVLocal(i,j) = TIMES(1,1) + 1.0
   enddo
   enddo

   do i = 1, NRooms+1
   do j = 1, NRooms+1
      if ( HVW(i,j) .gt. 0.0 ) then
         !
         ! Only first vent can be closed or opened
         !
         if ( j .gt. i ) then
            k = 1
            !
            ! Closing time is set
            !
            if ( CVLocal(j,i) .gt. 0.0 ) then
               do ii = 1,NO
                  !
                  ! First opened, then closed
                  ! 0 0 0 1 1 1 0 0 0 ...
                  ! (closing time greater than opening time)
                  !
                  if (CVLocal(j,i) .gt. CVLocal(i,j)) then  
                     if ( Fdata(ii,1) .ge. CVLocal(i,j) .and. Fdata(ii,1) .lt. CVLocal(j,i) ) then
                        CVFactor(ii) = 1.0
                     else
                        CVFactor(ii) = 0.0 
                     end if
                  !
                  ! First closed then opened
                  ! 1 1  0  0  0  1  1  1 ...
                  ! (opening time greater than closing time) 
                  else        
                     if ( Fdata(ii,1) .ge. CVLocal(i,j) .or. Fdata(ii,1) .lt. CVLocal(j,i) ) then
                        CVFactor(ii) = 1.0
                     else
                        CVFactor(ii) = 0.0
                     end if
                  end if
               end do
               write (iochannel,'("CVENT " 3I3, <NO>F8.2 )') &
                     i, j, k, (CVFactor(ii), ii=1,NO)
            ! 
            ! Closing time is not set
            !
            ! That is: only opening time is set 
            ! 0 0 0  1  1   1 ...
            else  
               do ii = 1,NO
                  if ( Fdata(ii,1) .ge. CVLocal(i,j) ) then
                     CVFactor(ii) = 1.0
                  else
                     CVFactor(ii) = 0.0 
                  end if
               end do
               write (iochannel,'("CVENT " 3I3, <NO>F8.2 )') &
                     i, j, k, (CVFactor(ii), ii=1,NO)
            end if
         !
         ! Possible second vent is always open
         else
            k = 2
            write (iochannel,'("CVENT " 3I3, <NO>F8.2 )') &
                     j, i, k, (1.0 ,ii=1,NO)
         end if
      end if
   end do
   end do

   !---------------------------------------------------------------------------------------
   ! CEILI and WALL
   !---------------------------------------------------------------------------------------
   if ( (any(Ceili .gt. 0.0)) .or. (any(Targets .gt. 0.0))) then
      !---------------------------------------------------------------------------------------
      ! If using existing thermal data base,
      ! then open and read in.
      !---------------------------------------------------------------------------------------
      if (.not. ThDbCreate) then
         allocate(ThermalIndex(0:NThermal))
         TmpStr = ThDbFile(1:index(ThDbFile,char(00))-1)//'.ndx'
         open  (thchannel, file = TmpStr, action = 'read', form='formatted',iostat = iostat)
         i = 0
         do while ( .not. eof(thchannel) ) 
            i = i + 1
            read(thchannel,'(A8, I4)') ThermalIndex(i), j
         end do
         close(thchannel)
!         ThermalIndex(0) = ThermalIndex(1)
         ThermalIndex(0) = 'GYPSUM'
      end if
      !---------------------------------------------------------------------------------------
      ! Write Ceiling materials
      !---------------------------------------------------------------------------------------
      if ( any(Ceili(1,:) .gt. 0.0) ) then
         write (iochannel,'("CEILI " <NRooms>A10 )') &
                     (ThermalIndex(idint(Ceili(1,i))), i = 1,NRooms)
      end if
      !---------------------------------------------------------------------------------------
      ! Write Wall materials
      !---------------------------------------------------------------------------------------
      if ( any(Ceili(2,:) .gt. 0.0) ) then
         write (iochannel,'("WALLS " <NRooms>A10 )') &
                     (ThermalIndex(idint(Ceili(2,i))), i = 1,NRooms)
      end if
   end if      
   !---------------------------------------------------------------------------------------
   !CHEMI
   write (iochannel,'("CHEMI " <NcChemi>F13.2 )') (sngl(Chemi(i,1)), i = 1,NcChemi)
   !LFBO
   write (iochannel,'("LFBO  " I4 )') Lfbo
   !LFBT
   write (iochannel,'("LFBT  " I4 )') Lfbt
   !LFBT
   write (iochannel,'("FPOS  " <NcFpos>F8.2 )') (Fpos(i,1), i = 1,NcFpos)
   !FTIME
   write (iochannel,'("FTIME " <FtimeLen>F8.0 )') (Fdata(i,1), i = 2,NO)
   !FMASS
   write (iochannel,'("FMASS " <NO>F11.6 )') (Fdata(i,2), i = 1,NO)
   !FHIGH
   write (iochannel,'("FHIGH " <NO>F8.3 )') (Fdata(i,3), i = 1,NO)
   !FAREA
   write (iochannel,'("FAREA " <NO>F8.3 )') (Fdata(i,4), i = 1,NO)
   !FQDOT
   write (iochannel,'("FQDOT " <NO>ES11.3E2 )') (Fdata(i,5), i = 1,NO)
   !Species1
   write (iochannel,'( A " " <NO>F8.3 )') Species1(1:index(Species1,char(00))-1),&
                  (Fdata(i,6), i = 1,NO)
   !Species2
   write (iochannel,'( A " " <NO>F8.3 )') Species2(1:index(Species2,char(00))-1),&
                  (Fdata(i,7), i = 1,NO)
   !---------------------------------------------------------------------------------------
   !DTCHECK
   !---------------------------------------------------------------------------------------
   if (DtCheckXval .gt. 0.0) then
      write (iochannel,'("DTCHECK  " ES10.3 " " I6)') sngl(DtCheckXval), DtCheckNsteps
   end if
   !---------------------------------------------------------------------------------------
   !TARGET
   !---------------------------------------------------------------------------------------
   do i = 1,NTargets
      write(iochannel,'("TARGET    " I3, 6F8.2 " " A " IMPLICIT   PDE")') &
                  idnint(Targets(i,1)), &
                  (Targets(i,j), j=2,7), &
                  ThermalIndex(idnint(Targets(i,8)))
   end do
   !---------------------------------------------------------------------------------------
   ! DETECT    Type Compartment Tactiv X Y Z RTI Sprikler SprayDens
   !---------------------------------------------------------------------------------------
   do i = 1,NDetectors
      write(iochannel,'("DETECT    " 2I3, 5F8.2, I3, F8.2)') &
                  idnint(Detect(i,1)), idnint(Detect(i,2)), (Detect(i,j), j=3,7), &
                  idnint(Detect(i,8)), Detect(i,9)
   end do
   !---------------------------------------------------------------------------------------
   ! FANS      1stRoom	Orient Height 2ndRoom Orient Height Area Pmin Pmax FlowRate
   !---------------------------------------------------------------------------------------
   do i = 1,NFans
      ! First node
      if (Fans(i,2).eq.1) then
      write(iochannel,'("MVOPN    " 2I3," ", A," ",  2F8.3)') &
                        idnint(Fans(i,1)), i*2-1, "H", Fans(i,3), Fans(i,7)
      else
      write(iochannel,'("MVOPN    " 2I3, " ", A," ",  2F8.3)') &
                        idnint(Fans(i,1)), i*2-1, "V", Fans(i,3), Fans(i,7)
      endif
      ! Second node
      if (Fans(i,5).eq.1) then
      write(iochannel,'("MVOPN    " 2I3, " ", A," ",  2F8.3)') &
                        idnint(Fans(i,4)), i*2, "H", Fans(i,6), Fans(i,7)
      else
      write(iochannel,'("MVOPN    " 2I3, " ", A," ",  2F8.3)') &
                        idnint(Fans(i,4)), i*2, "V", Fans(i,6), Fans(i,7)
      endif
      ! Fan definition
      write(iochannel,'("MVFAN    " 2I3, 3F8.3)') &
                        i*2-1, i*2, (Fans(i,j), j = 8,10)
   end do
   !---------------------------------------------------------------------------------------
   ! Ready to finish
   !---------------------------------------------------------------------------------------
   deallocate(ThermalIndex)
   close (iochannel)
   !---------------------------------------------------------------------------------------
   ! Finished so return
   !---------------------------------------------------------------------------------------
   return

end
