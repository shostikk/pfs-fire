	MODULE VENT

c VentFlow laskee massavirran vertikaalisessa suorakaiteen muotoisessa aukossa, kun oletetaan, että
c lämpötila sisällä huoneessa ja ulkopuolella ovat vakioita pystysuunnassa (1-vyöhykemalli)
c ref. SFPE handbook, p. 2-44 eqs. 30,31 yllämainituin yksinkertaistavin oletuksin
c Jos massantuotto kämpässä ylittää aukolle lasketun kriittisen massavirran, kaikki kaasuntuotto oletetaan tulevaksi
c ulos aukosta

	USE COMM

	CONTAINS

	SUBROUTINE VentFlow

	REAL Tvent,gasprod
	REAL htop,hbot,hn
	REAL mdot_room,mdot_in,mdot_out,mdot_crit
	REAL heat_in,heat_out
	REAL co2_capacity,h2o_capacity,n2_capacity,o2_capacity,ar_capacity
	REAL rhoair,netvol_vent
	REAL w_before,w_after
	REAL o2depl,co2prod,h2oprod
	REAL totmolconc,vaporfeed
	REAL rho_out,rho_in,pref_in,pref_out
	REAL h,dh,p_in_h,p_out_h,deltap,pbefore,pafter
	REAL A,K,apu,apu2,apu3,apu4,apu5,ddt,VentTime
	REAL flowcoeff
		
	co2_capacity=37.1/0.044		! lämpökapasiteetit (J/(mol K)) 298 K:ssa
	h2o_capacity=33.6/0.018		! muunnetaan yksikköön J/(kg K)
	n2_capacity=29.1/0.028
	o2_capacity=29.4/0.032
	ar_capacity=20.8/0.040
		
	hbot=0		! oletetaan aukon alaosa lattian tasolle; tällä ei ole väliä 1-vyöhyketilanteessa
	htop=hvent+hbot
	rhoair=1.2


c	Lasketaan kämpän kaasufaasiin syntyvä moolimäärä palamisen ja höyrystymisen vuoksi 
c	palavaksi aineeksi oletetaan heptaani, ja tämä pätkä on kopioitu suoraan Burning-funktiosta
	
	o2depl=(Qdot*dt/deltaH)/fuelmolmass*o2moles		! palon syömä happi mooleina
	co2prod=co2moles/o2moles*o2depl					! tässä on oletettu että polttoaine on heptaani
	h2oprod=h2omoles/o2moles*o2depl


c	lasketaan uusi paine olettamalla että massantuoton aikana aukkovirtausta ei ole (tilavuus pysyy vakiona)
	
c	print "Pressure before VentFlow (Pa):",pressure
	o2conc=o2conc-o2depl/volume
	Yconc=Yconc+h2oprod/volume+(evapmass/0.018)/volume		!	tässä höyrystyminen kasvattaa painetta (tai kondensaatio alentaa)
	co2conc=co2conc+co2prod/volume+(co2feed*dt/0.0224)/volume	! 	palosta ja suuttimista tuleva CO2
	n2conc=n2conc+(n2feed*dt/0.0224)/volume		!	suuttimista tuleva typpi
	arconc=arconc+(arfeed*dt/0.0224)/volume		!	suuttimista tuleva argon
	o2conc=o2conc+0.21*(1.-Yinf_out)*(airflowrate*dt/0.0224)
	1	   *(273./Tout)/volume	!	tuulettimesta tuleva happi
	n2conc=n2conc+0.79*(1.-Yinf_out)*(airflowrate*dt/0.0224)
	1	   *(273./Tout)/volume	!	tuulettimesta tuleva typpi
	Yconc=Yconc+Yinf_out*(airflowrate*dt/0.0224)*(273./Tout)/volume	!	tuulettimesta tuleva vesihöyry
	pressure=Rgas*Tgas*n2conc				! pakko uskoa että tästä tulee oikea yksikkö paineelle !
	pressure=pressure+Rgas*Tgas*o2conc
	pressure=pressure+Rgas*Tgas*Yconc
	pressure=pressure+Rgas*Tgas*co2conc
	pressure=pressure+Rgas*Tgas*arconc
	pbefore=pressure
c	print "Pressure in VentFlow (Pa):",pressure
	totmolconc=o2conc+n2conc+Yconc+co2conc+arconc
	o2fract=o2conc/totmolconc
	n2fract=n2conc/totmolconc
	Yinf=Yconc/totmolconc
	co2fract=co2conc/totmolconc
	arfract=arconc/totmolconc

	rho_in=o2conc*0.032+n2conc*0.028+Yconc*0.018+co2conc*0.044
	1	   +arconc*0.040
	pref_in=pressure
c	pref_in=rho_in*Rgas*Tgas/(rho_in/totmolconc)
c	print "pref_in,rho_in (kg/m3),totmolconc",pref_in,rho_in,totmolconc
	pref_out=101325.
	rho_out=pref_out/(Rgas*Tout)*(0.21*0.032+0.79*0.028)
	
c	Ulostulevan kaasun laskemisessa huomattava että aika-askel on jaettava pienempiin osiin, muutoin kaasuvirtaus tulee
c	hirmuisen paljon yliarvioitua, ja kämppä alkaa 'huohottaa'
	
	VentTime=0.
	ddt=dt/200.		! tämä 'aliaika-askel' määrää ulostulevan kaasumäärän laskentatarkkuuden
	dh=(htop-hbot)/20.	! määrää laskutarkkuuden yleensä ja neutraalikerroksen löytämistarkkuuden erikseen
	gasvol_out=0.		! kuutiometrejä
	gasvol_in=0.
	ii=0
	flowcoeff=0.7
	apu5=wvent*dh*flowcoeff*ddt
	do while (VentTime<dt+ddt)
		apu=0.
		apu2=0.
		h=hbot+dh/2.
c		iii=0
		do while (h<htop)
			p_in_h=pref_in-rho_in*9.81*h
			p_out_h=pref_out-rho_out*9.81*h
			deltap=p_in_h-p_out_h
			if (deltap>0.) then
				apu=apu+apu5*sqrt(2.*deltap/rho_in)
c				iii=iii+1
c				if (iii==1) then
c					print "Neutral plane,pressure:",h,pressure
c				endif
			else
c				print "hihhuu"
				apu2=apu2+apu5*sqrt(2.*(-deltap)/rho_out)
			endif
			h=h+dh
		enddo		! neutraalikerroksen haku puuttuu
		gasvol_out=gasvol_out+apu
		gasvol_in=gasvol_in+apu2
c		ii=ii+1
		apu3=pressure*apu/(Rgas*Tgas)/volume
		apu4=pref_out*apu2/(Rgas*Tgas)/volume
		co2conc=co2conc-co2fract*apu3
		Yconc=Yconc-Yinf*apu3
		Yconc=Yconc+Yinf_out*apu4
		o2conc=o2conc-o2fract*apu3
		o2conc=o2conc+0.21*(1-Yinf_out)*apu4
		n2conc=n2conc-n2fract*apu3
		n2conc=n2conc+0.79*(1-Yinf_out)*apu4
		arconc=arconc-arfract*apu3
		totmolconc=o2conc+n2conc+Yconc+co2conc+arconc
		o2fract=o2conc/totmolconc
		n2fract=n2conc/totmolconc
		Yinf=Yconc/totmolconc
		co2fract=co2conc/totmolconc
		arfract=arconc/totmolconc
		pressure=Rgas*Tgas*totmolconc
		rho_in=o2conc*0.032+n2conc*0.028+Yconc*0.018+co2conc*0.044
	1		  +arconc*0.040
		pref_in=pressure
		VentTime=VentTime+ddt
c		print ii,apu,apu2
	enddo

	pafter=pressure
c	print "Gasflow in / out (m3/s), deltap (Pa)",gasvol_in/dt,gasvol_out/dt,deltap
c	mass_in=(2./3.)*wvent*hvent**(3./2.)*0.7*rho_out*sqrt(2.*9.81)
c	mass_in=mass_in*sqrt((rho_out-rho_in)/rho_out
c	1	    /(1.+(rho_out/rho_in)**(1./3.))**3.)

	w_before=0.
	w_after=0.
	j=1
	w_before=sum(waterconc)
	do while (j<nfract+1)
		waterconc(j)=waterconc(j)*(volume-gasvol_out)/volume	! pisaramäärät lasketaan seuraavalla kutsulla Inject-rutiiniin
		j=j+1
	enddo
	w_after=sum(waterconc)

	watertovent=watertovent+(w_before-w_after)*volume

	heat_out=(Tgas-Tout)*gasvol_out*rho_in*(co2fract*co2_capacity
	1	    +Yinf*h2o_capacity+n2fract*n2_capacity+o2fract*o2_capacity)
c	heat_out=(Tgas-Tout)*4190*gasvol_out*(w_before-w_after)
	
c	heat_out on konvektiivinen lämpövirta ulos mutta sitä ei huomioida lämpöhäviönä kämpän lämpötasapainossa; huomiointi tapahtuu
c	epäsuorasti paineen kautta; jos paine kasvaa, lämpötila kasvaa myös, ja päinvastoin

	heat_in=(Tout-Tgas)*gasvol_in*rho_out
	1	   *(0.79*n2_capacity+0.21*o2_capacity)		! raitista ilmaa raitilta 
													! huomioidaan etumerkki
													! deltaT-funktiota varten (-, jos Tout < Tgas)
	heat_in=(Tout-Tgas)*airflowrate*dt*rho_out
	1	   *(0.79*n2_capacity+0.21*o2_capacity)		
	heat_in=heat_in+(Tpipe-Tgas)*(n2feed*dt/0.0224)*n2_capacity*0.028	!	pulloista tulevan typen lämmitys
	heat_in=heat_in+(Tpipe-Tgas)*(arfeed*dt/0.0224)*ar_capacity*0.040	!	kylmän argonin lämmitys
	heat_in=heat_in+(Tpipe-Tgas)*(co2feed*dt/0.0224)
	1	    *co2_capacity*0.044	!	kylmän hiilidioksidin lämmitys
 
c 	sisääntulevan kaasun lämmittäminen on kämpän kannalta oikea lämpöhäviö
 
	END SUBROUTINE VentFlow

	END MODULE VENT
