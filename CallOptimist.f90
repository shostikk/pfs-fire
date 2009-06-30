!  OptiMistDLL.f90 
!
!  FUNCTIONS/SUBROUTINES exported from OptiMistDLL.dll:
!	OptiMistDLL      - subroutine 
!
!	OptiMist version 3.0
!	Date: 8.2.2005 Jukka Vaari, VTT Building and Transport


SUBROUTINE OptiMistDLL(NrTime, NcTime, TimeArr, &
                       NrRoom, NcRoom, RoomArr, &
					   NrAmb,  NcAmb,  AmbArr, &
					   NrFire, NcFire, FireArr, &
					   NrMist, NcMist, MistArr, &
					   NrGas,  NcGas,  GasArr, &
					   NrOut,  NcOut,  OutArr, &
					   NrPlot, NcPlot, PlotArr)

!DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: OptiMistDLL
!DEC$ ATTRIBUTES ALIAS:'OptiMistDLL' :: OptiMistDLL

Use dflib
Use dfport
USE COMM
USE DROPS
USE DISCHARGE
USE TEMPS
USE VENT
USE STATS
USE numerical_libraries ! This is for IMSL 

IMPLICIT NONE

Integer(2) NrTime, NcTime
Real(4) TimeArr(NrTime, NcTime)  
Integer(2) NrRoom, NcRoom
Real(4) RoomArr(NrRoom, NcRoom)
Integer(2) NrAmb, NcAmb
Real(4) AmbArr(NrAmb, NcAmb)
Integer(2) NrFire, NcFire
Real(4) FireArr(NrFire, NcFire)
Integer(2) NrMist, NcMist
Real(4) MistArr(NrMist, NcMist)
Integer(2) NrGas, NcGas
Real(4) GasArr(NrGas, NcGas)
Integer(2) NrOut, NcOut
Real(4) OutArr(NrOut, NcOut)
Integer(2) NrPlot, NcPlot
Real(4) PlotArr(NrPlot, NcPlot)

!DEC$ ATTRIBUTES REFERENCE :: NrTime, NcTime, TimeArr
!DEC$ ATTRIBUTES REFERENCE :: NrRoom, NcRoom, RoomArr
!DEC$ ATTRIBUTES REFERENCE :: NrAmb, NcAmb, AmbArr
!DEC$ ATTRIBUTES REFERENCE :: NrFire, NcFire, FireArr
!DEC$ ATTRIBUTES REFERENCE :: NrMist, NcMist, MistArr
!DEC$ ATTRIBUTES REFERENCE :: NrGas, NcGas, GasArr
!DEC$ ATTRIBUTES REFERENCE :: NrOut, NcOut, OutArr
!DEC$ ATTRIBUTES REFERENCE :: NrPlot, NcPlot, PlotArr

!	START parameter input section
!   Default values for testing purposes
!t_start=0.
!dt=0.1
!t_end=200.
!volume=500.
!height=5.
!hvent=2.
!wvent=2.
!airflowrate=0.
!Tgas=20.+273.15
!Tout=Tgas
!Twall=Tgas
!heattrans=10.
!relhum=0.
!relhum_out=0.
!Qdot=6.0e6
!deltaH=44.6e6
!fuelmolmass=0.1
!co2moles=7.
!h2omoles=8.
!limit_AFT=1700.
!preburn=0.	
!waterflowrate=1.0	
!SMD=100.0e-6		
!q=2.0				
!nfract_dist=10		
!nfract_coag=5		 
!coag_flag=.TRUE.	
!Twater=273.15+5.	
!nbottles=0	
!gas_flag=.FALSE.
!bottlevol=0.05	
!bottlepressure=200.		
!X_N2=0.52					
!X_Ar=0.40					
!X_CO2=00.08					
!delay=15.				
!timeconst=30.			
!Tpipe=0.+273.15	

!	Time loop
	
t_start=0.				!	start time, seconds
dt=TimeArr(1, 1)		!	time interval, seconds
t_end=TimeArr(1, 2)		!	end_time, seconds
!	Room
		
volume=RoomArr(1, 1)	! enclosure volume, cubic metres
height=RoomArr(1, 2)	! enclosure height, metres
floorarea=volume/height		! enclosure floor area, square metres
hvent=RoomArr(1, 3)			! height of vertical vent, metres
wvent=RoomArr(1, 4)			! width of vertical vent, metres
wallarea=2.*floorarea+4.*SQRT(floorarea)*height-hvent*wvent
airflowrate=RoomArr(1, 5)	!	m3/s
Twall=RoomArr(1, 6)+273.15	! wall temperature, Kelvins
heattrans=RoomArr(1, 7)		! overall effective heat transfer coefficient, W/(m2K)

!	Ambient conditions

Tgas=AmbArr(1, 1)+273.15		! initial gas temperature inside enclosure, Kelvins
Tout=AmbArr(1, 2)+273.15		! initial gas temperature outside enclosure, Kelvins
relhum=AmbArr(1, 3)				! relative humidity inside enclosure, percents
relhum_out=AmbArr(1, 4)			! relative humidity outside, percents
Yinf=0.01*relhum*EXP(18.3-(3816.44/(Tgas-46.13)))/760.	! water vapor mole fraction
														! don't ask why I call this Yinf rather than h2ofract...
n2fract=0.79*(1.-Yinf)				! nitrogen mole fraction
o2fract=0.21*(1.-Yinf)				! oxygen mole fraction
co2fract=1.-(n2fract+o2fract+Yinf)	! carbon dioxide mole fraction	(NOTE: the formula assumes just N2, O2, CO2 and water vapor)

!	Fire
	
Qdot=1.0e6*FireArr(1, 1)	! RHR of the fire, W
deltaH=1.0e6*FireArr(1, 2)	! heat of combustion, J/kg
fuelmolmass=FireArr(1, 3)	! kg/mol
co2moles=FireArr(1, 4)		! co2 yield in mol/mol 
h2omoles=FireArr(1, 5)		! water yield in mol/mol
o2moles=co2moles+0.5*h2omoles	! oxygen consumption mol/mol 
co2yield=co2moles*0.044/fuelmolmass		! carbon dioxide yield (g/g), for stoichiometric and complete combustion
Limit_AFT=FireArr(1, 6)		! limit adiabatic flame temperature, Kelvins

!Water
	
preburn=MistArr(1, 1)		! seconds
waterflowrate=MistArr(1, 2)	! kg/s
SMD=1.0e-6*MistArr(1, 3)	! Sauter Mean Diameter, metres
q=MistArr(1, 4)				! width parameter for Rosin-Rammler distribution
nfract_dist=10				! number of size groups for mist nozzles
nfract_coag=5				! number of extra size groups for coagulation 
coag_flag=.TRUE.			! TRUE: coagulation enabled; FALSE: coagulation disabled
Twater=MistArr(1, 5)+273.15	! water temperature, Kelvins

!Gas
	
gas_flag=.FALSE.			!	FALSE for no gas discharge, TRUE gas discharge
							!	the gas discharge pressure curve must be specified by waves time_disch and P_disch
nbottles=INT(GasArr(1, 1))	!	number of gas cylinders
bottlevol=GasArr(1, 2)		!	volume of one cylinder, cubic metres
bottlepressure=GasArr(1, 3)	! cylinder pressure, bar
X_N2=GasArr(1, 4)			!	
X_Ar=GasArr(1, 5)			!	
X_CO2=GasArr(1, 6)			!	
delay=GasArr(1, 7)			! discharge delay, seconds
timeconst=GasArr(1, 8)		! discharge time constant, seconds
Tpipe=GasArr(1, 9)+273.15	! Effective gas discharge temperature (assumed constant)
IF (nbottles>0) THEN
	gas_flag=.TRUE.
	CALL MakeDischargeCurve
ENDIF

!END parameter input section

!Process input data

Rgas=8.314			! gas constant, J/(mol K)
pressure=101325.		! pressure, Pascals
totmolconc=pressure/(Rgas*Tout)
n2conc=n2fract*totmolconc	! nitrogen mole concentration (mol/m3)
o2conc=o2fract*totmolconc	! oxygen mole concentration (mol/m3)
co2conc=co2fract*totmolconc	! carbon dioxide mole concentration (mol/m3)
Yconc=Yinf*totmolconc
Yinf_out=0.01*relhum_out*EXP(18.3-(381644./(Tgas-4613.)))/760.	! water vapor mole fraction outside
arconc=0.						! argon mole concentration  
arfract=0.						! argon mole fraction

!Set up data storage arrays

npnts=INT(t_end/dt)
nfract=nfract_dist+nfract_coag
	
ALLOCATE(tim(0:npnts),dconc(0:npnts),wconc(0:npnts), general(0:npnts), &
gastemp(0:npnts),vaporfract(0:npnts),flametemp(0:npnts), &
Ptotal(0:npnts),oxygen(0:npnts),dry_oxygen(0:npnts),co2(0:npnts), &
co2_wet(0:npnts),gas_in(0:npnts),gas_out(0:npnts), ar(0:npnts), &
nitrogen(0:npnts), Sauter(0:npnts), RH(0:npnts))
ALLOCATE(Fractlimit(0:nfract),Volfract(0:nfract), Dropconc(0:nfract), &
Waterconc(0:nfract),evapfract(0:nfract), &
dmass(0:nfract),tmax(0:nfract),merkki(0:nfract),RRdist(0:nfract), &
alloc(nfract*(nfract+1)), coag_coeff(0:nfract))
ALLOCATE(shift(0:nfract,0:nfract))
ALLOCATE(order((nfract/2)*(nfract+1)+1,3))

	
!Calculate droplet distribution and volume fractions

X=SMD*GAMMA(1.-1./q)
i=0
DO WHILE(i<nfract_dist+1)
	IF (i<nfract_dist) THEN
		Volfract(i)=(1./nfract_dist)*REAL(i)
	ELSE
		Volfract(i)=0.99
	ENDIF
	Fractlimit(i)=X*(-LOG(1.-Volfract(i)))**(1./q)	! pisarakoko fraktion ylärajalla
	RRdist(i)=q/(X**q)*Fractlimit(i)**(q-1) &
			 *EXP(-(Fractlimit(i)/X)**q)
	i=i+1
ENDDO

max_size=3000e-6	! suurin mahdollinen pisara
coag_coeff=(max_size/fractlimit(nfract_dist))**(1./nfract_coag)	! vakiokerroin; kertoimet voidaan asettaa myös yksitellen
i=nfract_dist+1
DO WHILE(i<nfract+1)
	fractlimit(i)=coag_coeff(i-nfract_dist)*fractlimit(i-1)	
	i=i+1	
ENDDO
	
CALL BuildShiftTable

watertofloor=0.
wfloor_coag=0.
watertovent=0.
i=0
!shift=0			// jos shift=0, koagulaatiota ei tapahdu
waterconc=0.
dropconc=0.
evapfract=0.
deltaHvent=0.
evapmass=0.

Tflame_ini=Burning()	

aika=t_start
loppu=t_end
Qdot_ini=Qdot
ext_flag=.FALSE.

!Main loop begins

DO while (aika<loppu)

	if (aika<preburn) then
		flowrate=0.
	else
		flowrate=waterflowrate
	endif

!Water discharge and fallout

	CALL InjectAndFall

!Agglomeration of droplets due to kinematic coagulation

	if (coag_flag) then
		CALL Coagulate	
	endif

!Variable RHR for pool fires

!	Qdot=Qdot_ini*(10.*o2fract-1.1)	! VINNOVA-datan korrelaatio 
!1										RHR:n ja happipitoisuuden välillä

!Constant RHR for spray fires

	Qdot=Qdot_ini

!Find new gas temperature

	deltaT=DeltaTemp()
	Tgas=Tgas+deltaT
		
!Compute adiabatic flame temperature

	Tflame=Burning()
		
!Evaluate extinguishment

	if ((.NOT.ext_flag).AND.(Tflame<Limit_AFT)) then
		Qdot_ini=0.1				! Leave a very small flame to be able to track AFT			
		ext_flag=.TRUE.
		OutArr(1, 1)=aika
		OutArr(1, 2)=arfract*100.
		OutArr(1, 3)=n2fract*100.
		OutArr(1, 4)=o2fract*100.
		OutArr(1, 5)=o2fract/(1.-Yinf)*100.
		OutArr(1, 6)=co2fract*100.
		OutArr(1, 7)=Yinf*100.
		satpress=EXP(18.3-(3816.44/(Tgas-46.13)))/760.	! bar
		OutArr(1, 8)=min(100.,100.*Yinf/satpress)		
		OutArr(1, 9)=sum(waterconc)
		OutArr(1, 10)=Tgas-273.15
	endif
		
!Gas discharge

	if (Gas_flag) then
		Call Interpolate1d(1000,time_disch,P_disch,aika,p1)
		Call Interpolate1d(1000,time_disch,P_disch,aika+1,p2)
		gasfeed=nbottles*bottlevol*max((p1-p2),0.)
	else
		gasfeed=0.
	endif
	n2feed=X_N2*gasfeed
	arfeed=X_Ar*gasfeed		
	co2feed=X_CO2*gasfeed		

!Call pressure and vent flow calculation routine 

	CALL VentFlow

!Store data to arrays

	if (i<npnts+1) then
	general(i)=Qdot
	wconc(i)=sum(waterconc)
	tim(i)=aika
	gastemp(i)=Tgas
	vaporfract(i)=Yinf
	flametemp(i)=Tflame							
	Ptotal(i)=pressure
	oxygen(i)=o2fract
	dry_oxygen(i)=o2fract/(1.-Yinf)
	nitrogen(i)=n2fract
	co2(i)=co2fract/(1.-Yinf)
	ar(i)=arfract
	gas_in(i)=gasvol_in/dt
	gas_out(i)=gasvol_out/dt
	endif
!	write(*,*) aika
	i=i+1
	aika=aika+dt		! advance time
!	WRITE (6,*) "Time (s), Tgas (K), Tflame (K), X(O2), X(O2)dry:",
!1		aika,Tgas,Tflame, o2fract,o2fract/(1.-Yinf)	
ENDDO
	

!Calculate relative humidity

i=0
DO WHILE (i<SIZE(tim))
	satpress=EXP(18.3-(3816.44/(Tgas-46.13)))/760.	! bar
	RH(i)=min(100.,100.*vaporfract(i)/satpress)		
	i=i+1
ENDDO

co2_wet=co2*(1.-vaporfract)
gastemp=gastemp-273.15		! gas temperature from Kelvin to Centigrade

!Write datafile

!OPEN(UNIT=5,FILE='Opti.out',STATUS='UNKNOWN')
!i=0
!WRITE (5,'(11A)') ' Aika ','Gastemp ','oxygen ','dry_oxygen ', &
!            'nitrogen ','co2 ','vaporfract ', &
!            'flametemp ','ptotal ','gas_in ','gas_out ', &
!            'Sauter ', 'Wconc '
!DO WHILE (i<SIZE(tim))
!	WRITE (5,'(11(F12.4,1X))') tim(i),gastemp(i),oxygen(i),dry_oxygen(i), &
!	            nitrogen(i),co2(i),vaporfract(i), &
!	            flametemp(i),ptotal(i),gas_in(i),gas_out(i), &
!               Sauter(i),wconc(i)
!	i=i+1
!ENDDO

!Store selected data to PlotArr to be returned to Excel

i=1
dt=t_end/100.
do while (i < 101)
	PlotArr(i, 1) = Real(i) * dt
	i = i + 1
enddo
i=1
do while (i < 101)
	Call Interpolate1d(npnts,tim,gastemp,PlotArr(i, 1),PlotArr(i, 2))
	Call Interpolate1d(npnts,tim,flametemp,PlotArr(i, 1),PlotArr(i, 3))
	Call Interpolate1d(npnts,tim,oxygen,PlotArr(i, 1),PlotArr(i, 4))
	Call Interpolate1d(npnts,tim,Ptotal,PlotArr(i, 1),PlotArr(i, 5))
	i = i + 1
enddo

!WRITE (6,*) 'End OptiMist'

DEALLOCATE(tim,dconc,wconc,general, &
gastemp,vaporfract,flametemp, &
Ptotal,oxygen,dry_oxygen,co2, &
co2_wet,gas_in,gas_out, ar, &
nitrogen, Sauter, RH)
DEALLOCATE(Fractlimit,Volfract,Dropconc, &
Waterconc,evapfract, &
dmass,tmax,merkki,RRdist, &
alloc,coag_coeff)
DEALLOCATE(shift, order)

RETURN
END SUBROUTINE OPTIMISTDLL


!End of main program
