!*********************************************************
!*****   PFS v4.0 FDS fire model subroutine library  *****
!*********************************************************
!
! VTT Technical Research Centre of Finland      
! Date:    30.06.2009
! Version: PFS v4.0 
!
! DISCLAIMER: This source file comes as it is.
!             There is no warranty that it should
!             operate 'correctly'.
!
!*********************************************************
!
! Calling field model FDS
!
!*********************************************************

subroutine CallFDS    (RunDir,                & ! running directory
                       NrO,     NcO,     Out, & ! Output array (time, HRR, TC's)
                       NcO_HRR,               & ! Number of HRR columns to be read in
                       NcO_DEVC,              & !           DEVC
                       NcO_EVAC,              & !           EVAC
                       DataFile,              & ! FDS input
                       OutFile,               & ! FDS output
                       CHID,                  & ! CHID string
                       cFDSexe,               & ! name of the FDS executable (e.g. fds4.exe)
                       FDSVersion,            & ! FDS version number
                       MaxRunTime,            & ! Maximum running time (seconds)
                       RunMode)                 ! Normal, debug, savemode

!DEC$ ATTRIBUTES STDCALL,DLLEXPORT :: CallFDS
!DEC$ ATTRIBUTES ALIAS:'CallFDS' :: CallFDS

  ! Simo ei toimi ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:'CallFDS' :: CallFDS

  use dflib
  use dfwin

  !use dflib
  !USE IFPORT
  !use dfwin
  !USE IFWIN
  
  ! Alla omat kokeilut
  
  !use ifwinty
  !use dfwin
  !use mt
  !use kernel32

  ! Requires libifcoremd.dll

  implicit none

  integer, parameter :: ik16=selected_int_kind(4)       ! 16 bit integer
  integer, parameter :: ik32=selected_int_kind(9)       ! 32 bit integer
  integer, parameter :: rk64=selected_real_kind(10,40)  ! 64 bit real

  ! Argument definitions

  character(len=250), intent(inout) :: RunDir
  character(len=120), intent(inout) :: DataFile, OutFile
  character(len=120), intent(inout) :: CHID, cFDSexe
  integer(kind=ik16), intent(inout) :: NrO, NcO
  integer(kind=ik16), intent(in)    :: NcO_HRR, NcO_DEVC, NcO_EVAC
  integer(kind=ik32), intent(in)    :: MaxRunTime, RunMode
  real(kind=rk64),    intent(in)    :: FDSVersion
  real(kind=rk64),    intent(out)   :: Out(NrO,NcO)

  !DEC$ ATTRIBUTES REFERENCE :: RunDir
  !DEC$ ATTRIBUTES REFERENCE :: NrO, NcO, Out
  !DEC$ ATTRIBUTES REFERENCE :: DataFile, OutFile, CHID, cFDSexe

  ! If called directly from Excel, use this
  ! real(kind=rk64) :: Out(NcO,NrO)

  ! Local variables

  logical :: ioresultL
  integer(kind=ik16) :: ioresultI
  integer(kind=ik32) :: ioresultI4, iochannel, iostat,dbgchannel

  ! Make these strings longer

  character(len=250)  :: cDir ,cData ,cOut, cErr ,cCHID, FDSexe
  character(len=250)  :: HRRFile, TCFile, EVACFile, DelFile
  character(len=250)  :: HDFile, SPKFile
  character(len=250)  :: input_line
  character(len=10)   :: db_date, db_time
  character(len=1)    :: DataFilePrelim
  character(len=1)    :: HdrTmp

  integer(kind=ik32)  :: i, j, N, Nread, NTmp, N2, NTimesteps, ios, jnew
  integer(kind=ik32)  :: HRR_rows,DEVC_rows,EVAC_rows,Joint_rows
  integer(kind=ik32)  :: HRR_cols,DEVC_cols,EVAC_cols,Joint_cols
  integer(kind=ik32)  :: column

  logical(1)          :: Debugmode,Savemode
  logical             :: InputError
  logical             :: HRRError,DEVCError,EVACError
  logical             :: FirstMissing,UseHRRTime
  
  real(kind=rk64)     :: TimeVal,HRR,TmpNum,Time
  real(kind=rk64)     :: TimeBegin,Timestep,TimeEnd
  real(kind=rk64)     :: HRRTimeBegin,HRRTimeEnd,HRR_dtmin,HRR_dtmax
  real(kind=rk64)     :: DEVCTimeBegin,DEVCTimeEnd,DEVC_dtmin,DEVC_dtmax
  real(kind=rk64)     :: EVACTimeBegin,EVACTimeEnd,EVAC_dtmin,EVAC_dtmax
  
  logical, dimension(3) :: FileReadMask
  
  real(kind=rk64), dimension(3) :: BeginTimes,EndTimes
  real(kind=rk64), allocatable  :: InData(:,:)
  real(kind=rk64), allocatable  :: HRRData(:,:),DEVCData(:,:),EVACData(:,:)

  ! Process calling variables

  integer     :: res
  character   :: szArgs*512                   ! New process arguments buffer & temp pointer
  logical(4)  :: fSuccess                     ! API return code

  type(T_STARTUPINFO)         si              ! Used for CreateProcess
  type(T_PROCESS_INFORMATION) pi              ! Used for CreateProcess
  type(T_INPUT_RECORD)        ir              ! Used for console input
  type(T_KEY_EVENT_RECORD)    ker

  integer :: dwResult                         ! API return code
  integer :: dwCreate                         ! New process creation flags
  integer :: SIZEOFSTARTUPINFO, SIZEOFSzArgs
  integer :: SIZESECURITYATTRIBUTES
  logical :: InheritHandle
  !!!integer(bool) :: InheritHandle

  SIZEOFSTARTUPINFO=68
  SIZEOFSzArgs=512
  SIZESECURITYATTRIBUTES=12

  dwCreate=CREATE_NEW_CONSOLE
  InheritHandle=.false.

  Debugmode=.false.
  Savemode=.false.
  if (abs(RunMode) == 1) Debugmode=.true.
  if (abs(RunMode) == 2) Savemode=.true.

  !------------!
  ! Initialize !
  !------------!
    
  cDir=RunDir(1:index(RunDir,char(00))-1); cDir=Trim(cDir)
  cData=DataFile(1:index(DataFile,char(00))-1); cData=Trim(cData)
  cOut=OutFile(1:index(OutFile,char(00))-1); cOut=Trim(cOut)
  cCHID=CHID(1:index(CHID,char(00))-1); cCHID=Trim(cCHID)
  FDSexe=cFDSexe(1:index(cFDSexe,char(00))-1); FDSexe=Trim(FDSexe)
   
  DataFilePrelim=" "

  iochannel=10   ! Is it always open? Probably!
  dbgchannel=15

  HRRFile=CHID(1:index(CHID,char(00))-1)//"_hrr.csv"
  TCFile=CHID(1:index(CHID,char(00))-1)//"_devc.csv"
  EVACFile=CHID(1:index(CHID,char(00))-1)//"_evac.csv"
  cErr=CHID(1:index(CHID,char(00))-1)//"_err.out"

  Out=0.0 ! This is the output in an error case

  !-----------------------------!
  ! Change to Running directory !
  !-----------------------------!

  ioresultL=CHANGEDIRQQ (cDir) 
  if (Debugmode) then
    open(dbgchannel,file='CallFDS.log',position='append',iostat=iostat)
    call  date_and_time(db_date, db_time)
    write(dbgchannel,'')
    write(dbgchannel,'(''CallFDS  '', 2A11 )') db_date, db_time
    write(dbgchannel,'( ''Data dir   : '' A)') Trim(cDir)
    write(dbgchannel,'( ''Data file  : '' A)') Trim(cData)
    write(dbgchannel,'( ''Out file   : '' A)') Trim(cOut)
    write(dbgchannel,'( ''HRR file   : '' A)') Trim(HRRFile)
    write(dbgchannel,'( ''DEVC file  : '' A)') Trim(TCFile)
    write(dbgchannel,'( ''EVAC file  : '' A)') Trim(EVACFile)
  end if

  !------------------!
  ! Check user input !
  !------------------!

  InputError=.false.
  if (NrO      < 0) InputError=.true.
  if (NcO      < 0) InputError=.true.
  if (NcO_HRR  < 0) InputError=.true.
  if (NcO_DEVC < 0) InputError=.true.
  if (NcO_EVAC < 0) InputError=.true.
  if (NcO /= (NcO_HRR+NcO_DEVC+NcO_EVAC+1)) InputError=.true.

  if (InputError) then
    if (Debugmode) then
      write(dbgchannel,'(a)') 'ERROR: Problems with FDS control values'
      close(dbgchannel)
    end if
    return ! TERMINATE PROGRAM EXECUTION
  end if

  !-------------!
  ! Execute FDS !
  !-------------!

  if (RunMode >= 0) then
    call Zeromemory(LOC(szArgs),SIZEOFszArgs)
    call ZeroMemory(LOC(si),SIZEOFSTARTUPINFO)

    if (Debugmode) then
      res=lstrcpy(szArgs, "cmd /c """//Trim(FDSexe)//DataFilePrelim//Trim(cData)//" 2> "//Trim(cErr)//""" "C)
    else
      res=lstrcpy(szArgs, "cmd /c """//Trim(FDSexe)//DataFilePrelim//Trim(cData)//""" "C)
    end if

    si%dwFlags=STARTF_USESHOWWINDOW
    si%wShowWindow=SW_MINIMIZE
    
    if (Debugmode) then
      write(dbgchannel,'("szArgs is ",a)') szArgs
    end if
    cDir=Trim(cDir)//char(0)

    fSuccess=.false.                             ! To enter into while loop
    fSuccess=CreateProcess(null_character,    &  ! image file name
             szArgs,                          &  ! command line (including program name)
             NULL_security_attributes,        &  ! security for process
             NULL_security_attributes,        &  ! security for main thread
             InheritHandle,                   &  ! new process inherits handles?
             dwCreate,                        &  ! creation flags
             NULL,                            &  ! environment
             cDir,                            &  ! current Dir
             si,                              &  ! STARTUPINFO structure
             pi)                                 ! PROCESSINFORMATION structure
   
    dwResult=WaitForSingleObject(pi%hProcess, MaxRunTime*1000)
    if (dwResult == WAIT_TIMEOUT) then
      fSuccess=TerminateProcess(pi%hProcess,1)
    end if
  end if

  !---------------!
  ! Open HRR file !
  !---------------!

  HRRError=.false.; HRR_rows=0; HRR_cols=0
  open(unit=iochannel,file=HRRFile,status='old',action='read',form='formatted',iostat=ios)
  if (ios /= 0) then
    if (Debugmode) then
      write(dbgchannel,'(a)') 'ERROR: HRR file could not be opened'
    end if
    HRRError=.true.
  else
    
    ! Check for empty HRR file
    if (eof(iochannel)) then
      if (Debugmode) then
        write(dbgchannel,'(a)') 'ERROR: HRR file empty'
      end if
      HRRError=.true.
      close(iochannel)
    else
      
      read(iochannel,*,iostat=ios) HdrTmp ! Skip the comment lines
      read(iochannel,*,iostat=ios) HdrTmp 
      
      ! Count colons in the first data line of the HRR file
      ! N.B. Number of columns = Number of colons + 1
      read(iochannel,'(a)',iostat=ios) input_line
      if (ios /= 0) HRRError=.true.
      
      HRR_cols=0
      do i=1,len_trim(input_line)
        read(input_line(i:i),'(a)',iostat=ios) HdrTmp
        if (ios /= 0) HRRError=.true.
        if (HdrTmp == ',') HRR_cols=HRR_cols+1
      end do
      HRR_cols=HRR_cols+1 

      ! Find out the number of data rows in the HRR file
      rewind(iochannel)
      read (iochannel,'(a)',iostat=ios) HdrTmp
      read (iochannel,'(a)',iostat=ios) HdrTmp

      HRR_rows=0
      do while (.not. eof(iochannel))
        read(iochannel,*,iostat=ios) HdrTmp
        HRR_rows=HRR_rows+1
      end do
      allocate(HRRData(HRR_rows,HRR_cols)); HRRData=0.0
      
      rewind(iochannel)
      read (iochannel,'(a)',iostat=ios) HdrTmp
      read (iochannel,'(a)',iostat=ios) HdrTmp

      do i=1,HRR_rows
        read(iochannel,*,iostat=ios) (HRRData(i,j),j=1,HRR_cols)
        if (ios /= 0) HRRError=.true.
      end do

      close(iochannel)
    end if
  end if

  ! HRR data is now contained in the array HRRData(HRR_rows,2)
  if (Debugmode .and. (.not. HRRError)) then
    write(dbgchannel,'(a,i4,i4)') 'Rows and columns read from the HRR file:  ',HRR_rows,HRR_cols
  end if

  !----------------!
  ! Open DEVC file !
  !----------------!
 
  DEVCError=.false.; DEVC_rows=0; DEVC_cols=0
  open(unit=iochannel,file=TCFile,status='old',action='read',form='formatted',iostat=ios) 
  if (ios /= 0) then
    if (Debugmode) then
      write(dbgchannel,'(a)') 'WARNING: DEVC file could not be opened'
    end if
    DEVCError=.true.
  else

    ! Check for empty DEVC file
    if (eof(iochannel)) then
      if (Debugmode) then
        write(dbgchannel,'(a)') 'WARNING: DEVC file empty'
      end if
      close(iochannel)
      DEVCError=.true.
    else

      read(iochannel,*,iostat=ios) HdrTmp
      read(iochannel,*,iostat=ios) HdrTmp 

      ! Count colons in the first data line of the DEVC file
      read(iochannel,'(a)',iostat=ios) input_line
      if (ios /= 0) DEVCError=.true.
      
      DEVC_cols=0
      do i=1,len_trim(input_line)
        read(input_line(i:i),'(a)',iostat=ios) HdrTmp
        if (ios /= 0) DEVCError=.true.
        if (HdrTmp == ',') DEVC_cols=DEVC_cols+1
      end do
      DEVC_cols=DEVC_cols+1 

      ! Find out the number of data rows in the DEVC file
      rewind(iochannel)
      read(iochannel,*,iostat=ios) HdrTmp
      read(iochannel,*,iostat=ios) HdrTmp 
      
      DEVC_rows=0
      do while (.not. eof(iochannel))
        read(iochannel,*,iostat=ios) HdrTmp
        DEVC_rows=DEVC_rows+1  
      end do
      allocate(DEVCData(DEVC_rows,DEVC_cols)); DEVCData=0.0
      
      rewind(iochannel)
      read(iochannel,'(a)',iostat=ios) HdrTmp
      read(iochannel,'(a)',iostat=ios) HdrTmp 
      
      do i=1,DEVC_rows
        read(iochannel,*,iostat=ios) (DEVCData(i,j),j=1,DEVC_cols)
        if (ios /= 0) DEVCError=.true.
      end do
    
      close(iochannel)
    end if
  end if
   
  ! DEVC data is now contained in the array DEVCData(DEVC_rows,DEVC_cols)
  if (Debugmode .and. (.not. DEVCError)) then
    write(dbgchannel,'(a,i4,i4)') 'Rows and columns read from the DEVC file: ',DEVC_rows,DEVC_cols
  end if

  !----------------!
  ! Open EVAC file !
  !----------------!

  EVACError=.false.; EVAC_rows=0; EVAC_cols=0
  open(unit=iochannel,file=EVACFile,status='old',action='read',form='formatted',iostat=ios)
  if (ios /= 0) then
    if (Debugmode) then
      write(dbgchannel,'(a)') 'WARNING: EVAC file could not be opened'
    end if
    EVACError=.true.
  else

    ! Check for empty EVAC file
    if (eof(iochannel)) then
      if (Debugmode) then
        write(dbgchannel,'(a)') 'WARNING: EVAC file empty'
      end if
      close(iochannel)
      EVACError=.true.
    else

      read(iochannel,*,iostat=ios) HdrTmp
      read(iochannel,*,iostat=ios) HdrTmp 

      ! Count colons in the first data line of the EVAC file
      read(iochannel,'(a)',iostat=ios) input_line
      if (ios /= 0) EVACError=.true.
      
      EVAC_cols=0
      do i=1,len_trim(input_line)
        read(input_line(i:i),'(a)',iostat=ios) HdrTmp
        if (ios /= 0) EVACError=.true.
        if (HdrTmp == ',') EVAC_cols=EVAC_cols+1
      end do
      EVAC_cols=EVAC_cols+1 

      ! Find out the number of data rows in the EVAC file
      rewind(iochannel)
      read(iochannel,*,iostat=ios) HdrTmp
      read(iochannel,*,iostat=ios) HdrTmp 
      
      EVAC_rows=0
      do while (.not. eof(iochannel))
        read(iochannel,*,iostat=ios) HdrTmp
        EVAC_rows=EVAC_rows+1  
      end do
      allocate(EVACData(EVAC_rows,EVAC_cols)); EVACData=0.0
      
      rewind(iochannel)
      read(iochannel,'(a)',iostat=ios) HdrTmp
      read(iochannel,'(a)',iostat=ios) HdrTmp 
      
      do i=1,EVAC_rows
        read(iochannel,*,iostat=ios) (EVACData(i,j),j=1,EVAC_cols)
        if (ios /= 0) EVACError=.true.
      end do
    
      close(iochannel)
    end if
  end if
   
  ! EVAC data is now contained in the array EVACData(EVAC_rows,EVAC_cols)
  if (Debugmode .and. (.not. EVACError)) then
    write(dbgchannel,'(a,i4,i4)') 'Rows and columns read from the EVAC file: ',EVAC_rows,EVAC_cols
  end if

  !--------------------!
  ! Some data analysis !
  !--------------------!

  ! Do we really need this?

  if (.not. HRRError) then
    HRRTimeBegin=minval(HRRData(1:,1))
    HRRTimeEnd=maxval(HRRData(1:,1))
    do i=1,HRR_rows-1
      if (HRRData(i+1,1) < HRRData(i,1)) HRRError=.true.
    end do
    if (Debugmode) then
      if (HRRError) write(dbgchannel,'(a)') 'ERROR: Invalid HRR time value order' 
    end if
  end if

  if (.not. DEVCError) then
    DEVCTimeBegin=minval(DEVCData(1:,1))
    DEVCTimeEnd=maxval(DEVCData(1:,1))
    do i=1,DEVC_rows-1
      if (DEVCData(i+1,1) < DEVCData(i,1)) DEVCError=.true.
    end do
    if (Debugmode) then
      if (DEVCError) write(dbgchannel,'(a)') 'ERROR: Invalid DEVC time value order' 
    end if
  end if

  if (.not. EVACError) then
    EVACTimeBegin=minval(EVACData(1:,1))
    EVACTimeEnd=maxval(EVACData(1:,1))
    do i=1,EVAC_rows-1
      if (EVACData(i+1,1) < EVACData(i,1)) EVACError=.true.
    end do
    if (Debugmode) then
      if (EVACError) write(dbgchannel,'(a)') 'ERROR: Invalid EVAC time value order' 
    end if
  end if
 
  !--------------------------!
  ! Is everything all right? !
  !--------------------------!

  FileReadMask(1)=(.not. HRRError)
  FileReadMask(2)=(.not. DEVCError)
  FileReadMask(3)=(.not. EVACError)
  if (HRRError .and. DEVCError .and. EVACError) then
    if (Debugmode) then
      write(dbgchannel,'(a)') 'ERROR: In HRR-, DEVC- and EVAC data'
      close(dbgchannel)
    end if
    return ! TERMINATE PROGRAM EXECUTION
  end if

  !-------------------------!
  ! Time-related parameters !
  !-------------------------!

  BeginTimes=0.0; EndTimes=0.0
  if (.not. HRRError) then
    BeginTimes(1)=HRRTimeBegin
    EndTimes(1)=HRRTimeEnd
  end if
  if (.not. DEVCError) then
    BeginTimes(2)=DEVCTimeBegin
    EndTimes(2)=DEVCTimeEnd
  end if
  if (.not. EVACError) then
    BeginTimes(3)=EVACTimeBegin
    EndTimes(3)=EVACTimeEnd
  end if

  ! Select the longest time span and set time step length
  TimeBegin=minval(BeginTimes,FileReadMask)
  TimeEnd=maxval(EndTimes,FileReadMask)
  NTimesteps=NrO-1; Timestep=abs(TimeEnd-TimeBegin)/NTimesteps

  if (Timestep < epsilon(Timestep)) then
    if (Debugmode) then
      write(dbgchannel,'(a)') 'ERROR: Too small timestep value'
    end if
    return ! TERMINATE PROGRAM EXECUTION
  end if
 
  !-----------------------------------------!
  ! Fill the output data array Out(NrO,NcO) !
  !-----------------------------------------!

  ! IMPROVEMENT SUGGESTION:
  ! Make this simple

  Out=0.0
  do i=1,Ntimesteps+1
    Time=TimeBegin+(i-1)*Timestep
    Out(i,1)=Time; column=2 

    do j=1,NcO_HRR
      if (.not. HRRError) then
        if (j <= (HRR_cols-1)) then
          if (Time < HRRTimeBegin) then
            Out(i,column)=0.0
          else if (Time > HRRTimeEnd) then
            Out(i,column)=HRRData(HRR_rows,j+1)
          else 
            ! Interpolation of FDS data
            call interpolate(HRR_rows,HRRData(1:,1),(HRRData(1:,j+1)),&
              &Out(i,1),Out(i,column))
          end if
        else
          Out(i,column)=0.0
        end if
      else
        Out(i,column)=0.0
      end if
      column=column+1
    end do

    do j=1,NcO_DEVC
      if (.not. DEVCError) then
        if (j <= (DEVC_cols-1)) then
          if (Time < DEVCTimeBegin) then
            Out(i,column)=0.0
          else if (Time > DEVCTimeEnd) then
            Out(i,column)=DEVCData(DEVC_rows,j+1)
          else 
            ! Interpolation of FDS data
            call interpolate(DEVC_rows,DEVCData(1:,1),(DEVCData(1:,j+1)),&
              &Out(i,1),Out(i,column))
          end if
        else
          Out(i,column)=0.0 
        end if
      else
        Out(i,column)=0.0
      end if
      column=column+1
    end do

    do j=1,NcO_EVAC
      if (.not. EVACError) then
        if (j <= (EVAC_cols-1)) then
          if (Time < EVACTimeBegin) then
            Out(i,column)=0.0
          else if (Time > EVACTimeEnd) then
            Out(i,column)=EVACData(EVAC_rows,j+1)
          else 
            ! Interpolation of FDS data
            call interpolate(EVAC_rows,EVACData(1:,1),(EVACData(1:,j+1)),&
              &Out(i,1),Out(i,column))
          end if
        else
          Out(i,column)=0.0
        end if
      else
        Out(i,column)=0.0
      end if
      column=column+1
    end do 
      
  end do

  !---------------------------------------------------------------!
  ! Delete and Close Files (Don't delete if savemode or negative) !
  !---------------------------------------------------------------!

  if ((.not. Savemode) .and. (RunMode >=0)) then
    ioresultI=DELFILESQQ (OutFile) 
    ioresultI=DELFILESQQ (DataFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_err.out"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.csv"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.sf"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.bf"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.s3d"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.iso"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.sz"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.q"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//".end"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//".smv"
    ioresultI=DELFILESQQ (DelFile)
    DelFile=CHID(1:index(CHID,char(00))-1)//"_*.prt5"
    ioresultI=DELFILESQQ (DelFile)
  else if (Debugmode) then
    write (dbgchannel,'(a)') "Files not deleted"
  end if
  
  !-----------------!
  ! Close debugfile !
  !-----------------!

  close(dbgchannel)
  if (.not. HRRError) deallocate(HRRData,stat=ios)
  if (.not. DEVCError) deallocate(DEVCData,stat=ios)
  if (.not. EVACError) deallocate(EVACData,stat=ios)

  !--------------!
  ! Output ready !
  !--------------!

  !  If called directly from Excel, use this
  !   Out = transpose(Out)

  return

contains

  subroutine interpolate(nrows,t,f,time,ans)

  !--------------------------------------------------------!
  ! This subroutine uses linear interpolation to calculate ! 
  ! measurement data values at desired times.              !
  ! Assumes t_min <= time <= t_max                         !
  !--------------------------------------------------------!

    implicit none

    integer :: i,nrows
    real(kind=rk64) :: time,ans
    real(kind=rk64), dimension(nrows) :: t,f  

    ! IMPROVEMENT SUGGESTION:
    ! The following search should be
    ! made using the bisection method

    i=1
    do while ((t(i) <= time) .and. (i < nrows))
      i=i+1
    end do
    ! The last term in the denominator is to avoid division by zero
    ! The following if statement is an almost unnecessary double check!
    if (i == 1) then
      ans=f(1)
    else if (i == nrows .and. time > t(nrows)) then
      ans=f(nrows)
    else
      ans=f(i-1)+(time-t(i-1))*(f(i)-f(i-1))/(t(i)-t(i-1)+epsilon(time))
    end if

    return 
  end subroutine interpolate

end subroutine CallFDS
