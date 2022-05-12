!###########################################################
!## Kwang Soo Kim 
!## Seoul National University 
!## 2017-May-23
!## ARID CONVERSION TO FORTRAN 
!###########################################################
!## 2017-JUN-1 FORTRAN Version has nearly identical results 
!## ########################################################
!## Documentation is needed 
!########################################################### 

Module ard
!
!
Contains
!* subroutine ARID
       Subroutine calARID(CONTROL, YRPLT, LATITUDE, ELEVATION, SRAD, TMAX, TMIN, DEWT,&
     & RAIN, WSPD, DOY, YEAR, ARID, ET0, WAT, DYNAMIC)
!-----------------------------------------------------------------------
       use moduledefs

       IMPLICIT NONE
       SAVE
       REAL LATITUDE, ELEVATION
       REAL SRAD, TMAX, TMIN, DEWT, RAIN, WSPD
       integer DOY, YEAR
       integer YRPLT
       integer DYNAMIC
       REAL ARID

       INTEGER DAYS
       REAL prevWAT
       REAL TMEAN

       REAL AWC !:    Available water capacity
       REAL CSR !:    clear-sky radiation
       REAL CWBD !:   Rate of change in water before drainage
       REAL DDC !:    deep drainage coefficient
       REAL DR !:     drainage
       REAL ea !:     ambient vapor pressure
       REAL es !:     saturation vapor pressure
       REAL ET0 !:    reference ET
       REAL extra !:  extra-terrestrial radiation
       REAL IRDES !:  Inverse relative distance Earth-Sun
       REAL lat !:    latitude in radiationspBplant.f90:
       REAL LWR !:    long-wave radiation
       REAL WUC !:    water uptake coefficient
       REAL NRAD !:   net radiation
       REAL PSC !:    psychrometric constant
       REAL RCN !:    runoff curve number
       REAL RO !:     runoff
       REAL RRAD !:   relative radiation
       REAL SD !:     solar declination
       REAL slope !:  slope of vapor pressure curve
       REAL SSA !:    sun-set hour angle
       REAL SWR !:    short-wave radiation
       REAL TR !:     transpiration
       REAL WAD !:    water after drainage
       REAL WAT !:    water after transpiration
       REAL W_AT !:   water after transpiration (temporary variable)
       REAL WBD !:    water before drainage
       REAL ws2 !:    wind speed at 2 m
       REAL RZD !:    rootzone depth

       REAL ET_ALB !: albedo for ET 
       REAL XHLAI !: LAI for Priestly-Taylor ET function
       REAL PT0 !: Priestly-Taylor ET function
       integer yrdoy
!-----------------------------------------------------------------------

!*
!*# THE R PROGRAM FOR COMPUTING ARID
!*#1. Variables Defined:-
!*#ARID:   Agricultural Reference Index for Drought
!*#AWC:    Available water capacity
!*#CSR:    clear-sky radiation
!*#CWBD:   Rate of change in water before drainage
!*#DDC:    deep drainage coefficient
!*#DR:     drainage
!*#ea:     ambient vapor pressure
!*#es:     saturation vapor pressure
!*#ETo:    reference ET
!*#extra:  extra-terrestrial radiation
!*#IRDES:  Inverse relative distance Earth-Sun
!*#lat:    latitude in radiation
!*#LWR:    long-wave radiation
!*#WUC:    water uptake coefficient
!*#NRAD:   net radiation
!*#PSC:    psychrometric constant
!*#RCN:    runoff curve number
!*#RO:     runoff
!*#RRAD:   relative radiation
!*#SD:     solar declination
!*#slope:  slope of vapor pressure curve
!*#SSA:    sun-set hour angle
!*#SWR:    short-wave radiation
!*#TR:     transpiration
!*#WAD:    water after drainage
!*#WAT:    water after transpiration
!*#W_AT:   water after transpiration (temporary variable)
!*#WBD:    water before drainage
!*#ws2:    wind speed at 2 m
!*#RZD:    rootzone depth

!*#2. Inputs:
!*#This program uses an input dataset with 7 columns
!*#column 1: solar radiation (MJ m2 d-1),
!*#column 2: maximum temperature (癈),
!*#column 3: minimum temperature (癈),
!*#column 4: dewpoint temperature (癈),
!*#column 5: precipitation (mm),
!*#column 6: windspeed at 10 m height (m s-1),
!*#column 7: day-of-year (number), and
!*#column 8: year
!*#Note: Make sure that your dataset has the same columns as above. 
!  Otherwise, change codes accordingly.

!#3. Constants:

!latitude <-28.102                                                # LATITUDE of the weather station (degree)
!elevation <-53.0
!latitude <-soil$Lai                                               # LATITUDE of the weather station (degree)
!elevation <-soil$Elev                                               # ELEVATION of the weather station (m)
     INTEGER LUNIO, ERR, LINC, LNUM, FOUND
     CHARACTER(Len=30) :: FILEIO
     CHARACTER(Len=6) :: ERRKEY
     CHARACTER(Len=6) :: SECTION
     PARAMETER (ERRKEY = 'ARID')

     TYPE (ControlType) CONTROL

     FILEIO = CONTROL % FILEIO
     LUNIO  = CONTROL % LUNIO
     YRDOY  = CONTROL % YRDOY

       !write(*,*) 'in arid', year, doy
       if (DYNAMIC == RUNINIT) then

!#4. Parameters: The following parameter values are default.
            AWC = 0.13
            DDC = 0.55
            RCN = 65
            RZD = 400
            WUC = 0.096

!            AWC = soil$AWC          ##########available water capacity               OK
!            DDC <- soil$DDC          #####deep drainage coefficient                   OK
!            RCN <- soil$RCN          #####runoff curve number                         OK
!            RZD <- soil$RZD          #####rootzone depth
!            WUC <- soil$WUC          #####water uptake coefficient

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! for the moment, arid soil parameters are from cultivar parameter set
! parameter IO will be updated
! read cultivar parameters

        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

        LNUM = 0
        SECTION = '*CULTI'
!       Section *CULTIVARS
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!       Section *CULTIVAR
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          LNUM = LNUM + 1
          READ(LUNIO,'(115X,5F6.0)',IOSTAT=ERR) &
          AWC, RCN, DDC, WUC, RZD
          write(*,*) 'arid io', ' awc =', awc, ' rcn=', rcn, ' ddc=', ddc, &
                      ' wuc=', wuc, ' rzd=', rzd, latitude, elevation
!          write(*,*) 'check latitude position in cul file'

!        LNUM = LNUM + 1

          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

        CLOSE (LUNIO)

       else If (DYNAMIC ==SEASINIT) Then
            ET0 = 0
            WBD = 0
            WAT = 0
            ARID = 0
            prevWAT = 0
      
            write(*,*) 'elevation = ' , elevation, pi 

            lat = latitude*pi/180.
!            PSC = 0.665* 10**-3 *101.3*((293-0.0065*elevation)/293)^5.26
            PSC = 0.665* 1E-3 *101.3*((293-0.0065*elevation)/293)**5.26
            ! #initial value for 'Water after transpiration', 
            ! which is assumed to be AWC (0.13).
            W_AT = RZD*AWC
            ! fill water in soil before season
            prevWAT = W_AT 

       else If (DYNAMIC == RATE) Then
        !    #5.1 First, compute ETo (using the FAO-56 method).
           if(MOD(YEAR, 4) .EQ. 0) then
               days = 366
           else
               days = 365
           endif
           TMEAN = (TMAX + TMIN)/2
           ws2 = WSPD*4.87/log(67.8*10-5.42)
           es  = ((0.6108*exp(17.27*TMAX/(TMAX+237.3)))+&
                  (0.6108*exp(17.27*TMIN/(TMIN+237.3))))/2
           slope = (0.6108*exp(17.27*TMEAN/(TMEAN+237.3))*4098)/&
                   (TMEAN+237.3)**2
           SWR = (1-0.23)*SRAD
           IRDES = 1+0.033*cos(2*pi*DOY/days)
           SD = 0.409*sin(2*pi*DOY/days-1.39)
           SSA = acos(-tan(lat)*tan(SD))
           extra = 24*60*0.082/PI*IRDES*&
                   (SSA*sin(lat)*sin(SD)+cos(lat)*cos(SD)*sin(SSA))
!           CSR = (0.75+2*10^-5*elevation)*extra
           CSR = (0.75+2*1E-5*elevation)*extra
           RRAD = SRAD/CSR
           ea = 0.6108*exp(17.27*DEWT/(DEWT+237.3))
!           write(*,*) 'ssa=' , ssa, ' lwr =', lwr, ' rrad=', rrad, ' lat= ', latitude,' csr=', csr, ' pi=', pi, ' extr=',extra
!           LWR = 4.903*10^-9*((TMAX+273.16)^4+(TMIN+273.16)^4)/2*(0.34-0.14*sqrt(ea))*(1.35*RRAD-0.35)
           LWR = 4.903*1E-9*((TMAX+273.16)**4+(TMIN+273.16)**4)/2*&
                (0.34-0.14*sqrt(ea))*(1.35*RRAD-0.35)
           NRAD = SWR-LWR
!           write(*,*) 'swr=' , swr, ' lwr =', lwr, ' rrad=', rrad, ' srad= ', srad,' csr=', csr, ' elv=', elevation, ' extr=',extra

           ET0 = (0.408*slope*NRAD+&
              PSC*(900/(TMEAN+273))*ws2*(es-ea))/(slope+PSC*(1+0.34*ws2))
           write(*,*) ET0, es, slope, IRDES, SD, SSA
!           ET_ALB = 0.23
!           XHLAI = -99
!           
!           CALL PETPT(ET_ALB, SRAD, TMAX, TMIN, XHLAI, PT0) 
!           ET0 = PT0

           ! #5.2 Then, compute ARID.
           if(rain>0.2*(25400/RCN-254)) then
               RO = (rain-0.2*(25400/RCN-254))**2/(rain+0.8*(25400/RCN-254))
           else
               RO = 0
           endif
           CWBD = rain - RO

       else If (DYNAMIC == INTEGR) Then
!           WBD  = CWBD + W_AT[i-1]
!           if (YRDOY == YRPLT) then 
!               W_AT = RZD*AWC
!           else 
!               W_AT = prevWAT
!           endif
           W_AT = prevWAT
           WBD  = CWBD + W_AT

           if (WBD/RZD > AWC) then
               DR = RZD*DDC*(WBD/RZD - AWC)
           else
               DR = 0
           endif

           WAD = WBD - DR
           TR = min(WUC*RZD*WAD/RZD,ET0)
           WAT = WAD - TR
           ARID = 1-TR/ET0
           prevWAT = WAT
!           write(*,*) 'arid', year, doy, arid, wat, tr, et0, WUC*RZD*WAD/RZD, wad, cwbd, w_at, wbd, dr, rain
!       write(*,*) 'arid', year, doy, et0, wat, tr, arid
       endif

       return
       end subroutine calARID
End Module ard
