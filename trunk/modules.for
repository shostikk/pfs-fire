

	MODULE COMM

c	Main variable declarations

	REAL floorarea,height,volume
	REAL flowrate,deltap,diam,nozzles,preburn,waterflowrate
	REAL SMD
	REAL aika,dt,loppu,t_start,t_end
	REAL water,floorloss,watertofloor,wfloor_coag,watertovent
	REAL rholiq,rhoair,surften,dynviscliq,dynviscair
	REAL q,X
	REAL largevelo,smallvelo,relvelo,Stk,capteff
	REAL largevol,smallvol,sumvol,newdiam,apu,apu2,apu3
	REAL Tgas,deltaT,cgas,Yinf,Yinf_out,relhum,relhum_out,evapmass
	REAL Ylimit,Tflame_ini,Tflame,Tout
	REAL Twater,Qdot_ini,Qdot,deltaH,n2fract,o2fract,co2fract
	REAL vaporfeed
	REAL co2yield,o2moles,co2moles,h2omoles,fuelmolmass
	REAL n2conc,o2conc,co2conc,Yconc,Rgas,totmolconc
	REAL pressure,hvent,wvent,deltaHvent,gasvol_in,gasvol_out
	REAL bottlevol,n2feed,mass_in,co2feed,o2feed,gasfeed
	REAL arconc,arfract,arfeed,Tpipe
	REAL X_N2,X_O2,X_CO2,X_Ar,airflowrate
	REAL Limit_AFT,Twall,wallarea,heattrans
	REAL max_size
	REAL w_before,w_after,deltaw
	REAL satpress
	REAL timeconst,delay,bottlepressure, p1, p2

	INTEGER npnts, i, j, k, kk, ii, iii, nfract, nbottles
	INTEGER nfract_dist,nfract_coag

	LOGICAL gas_flag, coag_flag, ext_flag

c	set up data storage arrays

	REAL, ALLOCATABLE, DIMENSION(:) :: tim,dconc,wconc,general,
	1 gastemp,vaporfract,flametemp, Ptotal,oxygen,dry_oxygen,co2,
     1 co2_wet,gas_in,gas_out, ar, nitrogen, Sauter, RH,
	1 Fractlimit,Volfract,Dropconc,Waterconc,evapfract,
     1 dmass,tmax,merkki,RRdist, alloc, coag_coeff
	REAL, ALLOCATABLE, DIMENSION(:,:) :: shift
	REAL, ALLOCATABLE, DIMENSION(:,:) :: order

	REAL, DIMENSION(1000) :: time_disch,P_disch

	END MODULE COMM


	MODULE DROPS

	USE COMM

	CONTAINS

	SUBROUTINE InjectAndFall

	rholiq=1000

	j=1

	do while(j<nfract+1)
		if (j<nfract_dist+1) then
			waterconc(j)=waterconc(j)+(1./nfract_dist)
	1					*flowrate*dt/volume	! liters (or kg) per m3
		endif
		apu=min(TermVelo(Fractlimit(j))*dt,height)
		watertofloor=watertofloor+waterconc(j)*floorarea*apu
		if (j>nfract_dist) then
			wfloor_coag=wfloor_coag+waterconc(j)*floorarea*apu
		endif
		waterconc(j)=waterconc(j)*(1.-floorarea*apu/volume)	! gravity settling
		dropconc(j)=waterconc(j)/(rholiq*(1./6.)*3.14159
	1				*Fractlimit(j)**3.)	! no of drops in fraction per m3
		j=j+1
	enddo

	END SUBROUTINE InjectAndFall


	SUBROUTINE BuildShiftTable

	REAL Stk,smallvelo,largevelo,relvelo
	REAL dynviscair,capteff

	rholiq=1000.
	dynviscair=2e-5

	j=nfract
	do while (j>1)
		largevelo=TermVelo(Fractlimit(j))
		k=j-1
		do while (k>0)
			smallvelo=TermVelo(FractLimit(k))
			relvelo=largevelo-smallvelo
			Stk=rholiq*FractLimit(k)**2*relvelo
	1			/(18.*dynviscair*Fractlimit(j))
			capteff=(Stk/(Stk+.25))**2
			shift(j,k)=(3.14159/4.)*Fractlimit(j)**2*relvelo*capteff ! tämä kertaa konsentraatioiden tulo = törmäystaajuus per s per m3
			k=k-1
		enddo
		j=j-1
	enddo

	END SUBROUTINE BuildShiftTable


	SUBROUTINE Coagulate

	REAL apu4
	REAL largevelo,smallvelo,relvelo,Stk,capteff
	REAL largevol,smallvol,sumvol,newdiam
	REAL dropmult
	INTEGER i1, npairs

	rholiq=1000
	dynviscair=2e-5

	npairs=((nfract-1)/2)*nfract

	CALL Randomize
	i1=1
	do while (i1<npairs+1)
		j=order(i1,1)
		k=order(i1,2)
		dropmult=dropconc(j)*dropconc(k)	! konsentraatiot huomioidaan vasta täällä jottei tule kirjanpito-ongelmia
		largevol=(1./6.)*3.14159*Fractlimit(j)**3
		smallvol=(1./6.)*3.14159*Fractlimit(k)**3
		sumvol=largevol+smallvol
		newdiam=(6.*sumvol/3.14159)**(1./3.)
		ii=0
		iii=0
		do while(iii==0)
			if (newdiam<Fractlimit(j+ii)) then	! tsekataan mihin fraktioon uudet pisarat menee (j+ii)
				iii=1
			endif
			ii=ii+1
			if (j+ii>nfract) then
				iii=1
			endif
		enddo
		ii=ii-1
c		print j,k,fractlimit[j],fractlimit[k],newdiam,j+ii
		if (ii==0) then	! jos uusi pisara on samassa fraktiossa kuin suurempi törmääjä (voi olla vain jos j=nfract) 
			apu4=min(shift(j,k)*dropmult*dt,dropconc(k))	! ei saa törmätä olemattomiin pisaroihin
			apu=dropconc(j+ii)+apu4*smallvol/largevol
			apu2=apu
		else
			apu4=min(shift(j,k)*dropmult*dt,dropconc(k))	! ei saa törmätä olemattomiin pisaroihin
c HUOM jos törmäysten määrä ylittää suurempien pisaroiden määrän niin vettä lähtee generoitumaan; generoituva määrä
c on ylimenevien törmäysten määrä kertaa suuremman pisaran tilavuus
			if (shift(j,k)*dropmult*dt>dropconc(j)) then					! täällä huomioitava ettei vettä synnytetä
				apu2=0
				apu=dropconc(j+ii)+dropconc(j)*sumvol
	1					/((1./6.)*3.14159*Fractlimit(j+ii)**3.)
				apu=apu+(apu4-dropconc(j))*smallvol
	1					/((1./6.)*3.14159*Fractlimit(j+ii)**3.)
			else
				apu2=dropconc(j)-shift(j,k)*dropmult*dt						! tässä haarassa suurempia pisaroita on tarpeeksi
				apu=dropconc(j+ii)+apu4*sumvol
	1					/((1./6.)*3.14159*Fractlimit(j+ii)**3)	
			endif
		endif
		apu3=dropconc(k)-apu4
		dropconc(j)=apu2						! huomioidaan myös aika-askel dt sekä
		dropconc(k)=apu3						! massan säilyminen
		dropconc(j+ii)=apu		
		i1=i1+1
	enddo

	j=1
	do while (j<nfract+1)
		waterconc(j)=rholiq*dropconc(j)*((1./6.)*3.14159
	1				*Fractlimit(j)**3)
		j=j+1
	enddo

	END SUBROUTINE Coagulate


	SUBROUTINE Randomize

	INTEGER npairs, i1
	REAL r1

	alloc=0
	order=0
	npairs=((nfract-1)/2)*nfract	! [j][k]-törmäysparien lukumäärä
	i1=1
	do while (i1<npairs+1)
10		CONTINUE
			r1=rnunf()
			j=floor(abs(r1*(nfract-1)))+2	! kokonaisluku [2,nfract]
20			CONTINUE
				r1=rnunf()
				k=floor(abs(r1*(nfract-0)))+1	! kokonaisluku [1,nfract+1]
			IF (k>j-1) GOTO 20
		IF (alloc(nfract*j+k)>0) GOTO 10
		alloc(nfract*j)=1
		alloc(nfract*j+k)=i1
		order(i1,1)=j
		order(i1,2)=k
		i1=i1+1	
	enddo
	
	END SUBROUTINE Randomize


	REAL FUNCTION TermVelo(D)

	REAL D,velo,drag,Re,rhoair
	INTEGER jj,niter
	
	rhoair=1.2
	velo=rholiq*D**2.*9.81/(18.*dynviscair) ! alkuarvaus: drag=24/Re
	jj=0
	niter=7
	do while (jj<niter)
		Re=rhoair*D*velo/dynviscair
		if (Re<=1) then
			drag=24./Re
		endif
		if ((Re>1.) .AND. (Re<400.)) then
			drag=24./Re**.646		! NISTin paperista (Yao, Hung)
		endif
		if (Re>=400.) then
			drag=0.5
		endif
		velo=sqrt(8.*rholiq*D*9.81/(6.*rhoair*drag))
		jj=jj+1
	enddo 

	TermVelo=velo

	END FUNCTION TermVelo


	REAL FUNCTION EvapRate(D)

	REAL D,alpha,lambda,cp
	REAL heattime
	REAL Tamb,Lvap,W,R,Tboil,lgas,cgas
	REAL Ysurf,B,mdot
	REAL condens
	REAL Minf,Msurf

c	lambda=0.6
	rholiq=1000
	cp=4180.

	Tboil=373.
	Lvap=2.4e6	! J/kg
	W=0.018		! kg/mol
	R=8.314		! J/(mol K)
c	lgas=0.02	! W(m K)
c	cgas=2000	! J/(kg K)
	cgas=(o2conc*0.032*0.93+n2conc*0.028*1.045+Yconc*0.018*2.06
	1     +co2conc*0.044*0.95+arconc*0.040*1)	!argonin kerroin hatusta
	cgas=1000.*cgas/(o2conc*0.032+n2conc*0.028+Yconc*0.018+co2conc
	1     *0.044+arconc*0.040)
	lgas=(o2conc*0.031+n2conc*0.030+Yconc*0.023+co2conc*0.020
	1	+arconc*0.020)	!argonin kerroin hatusta
	lgas=lgas/(o2conc+n2conc+Yconc+co2conc+arconc)
c	print cgas,lgas

c	alpha=lambda/(rholiq*cp)
	if (Tgas<Tboil) then
		Ysurf=exp((Lvap*W/R)*(1./Tboil-1./Tgas))
c		B=(Yinf-Ysurf)/(Ysurf-1)
		Minf=Yinf*totmolconc*0.018/(Yconc*0.018+n2conc*0.028
	1	    +o2conc*0.032+co2conc*0.044+arconc*0.040)
		Msurf=Ysurf*totmolconc*0.018/(Yconc*0.018+n2conc*0.028
	1	    +o2conc*0.032+co2conc*0.044+arconc*0.040)
		B=(Minf-Msurf)/(Msurf-1.)
	else
		B=0.9999999
	endif
	if (B<-1.) then
		WRITE (6,*) ' APUVA, Spaldingin siirtoluku mättää - EvapRate'
	endif
	
c	heattime=(1/4)*D**2/alpha
	mdot=(D/2.)*4.*3.14159*lgas/cgas*LOG(1.+B)
c	print "Diameter (microns), evap.rate (kg/s):",diam*1e6,mdot
	
	EvapRate=mdot	
	
	END FUNCTION EvapRate

	END MODULE DROPS


	MODULE DISCHARGE

	USE COMM

	CONTAINS

	SUBROUTINE MakeDischargeCurve
	
	DO i=1,1000
		time_disch(i)=REAL(i)
		if (i < INT(delay)) THEN
		P_disch(i)= 0.
		ELSE
		P_disch(i)=min(bottlepressure,
	1				   bottlepressure*exp(-(REAL(i)-delay)/timeconst))
		ENDIF
	ENDDO

	END SUBROUTINE MakeDischargeCurve

	END MODULE DISCHARGE

	MODULE STATS

	CONTAINS

	Subroutine Interpolate1d(nx,x,y,xi,ans)
	  Integer nx
	  Real(4)    x(nx), y(nx), xi, ans
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
	    ans=y(jl)+(xi-x(jl))*(y(jl+1)-y(jl))/
	1	(x(jl+1)-x(jl)+1.0e-30*(x(2)-x(1)))
	  End If
	End Subroutine Interpolate1d

	END MODULE STATS