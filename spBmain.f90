!########################################################################
!## Kwang Soo Kim 
!## Seoul National University 
!## 2017-APR-23
!########################################################################
!#        SIMPLE B DRIVER 
!########################################################################
!# Documentation needed 
!########################################################################

      SUBROUTINE Simple(CONTROL, ISWITCH, WEATHER, &
       &                YRPLT, MDATE, YREND)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE ModuleData
      USE smplB
      USE ard
      USE smpsw
      
      IMPLICIT NONE
      SAVE
      

      REAL SRAD, TMAX, TMIN, PAR, RAIN, CO2, DEWT, WSPD
      REAL Biomass
      REAL ARID, ET0, WAT
      REAL IRRAMT
      REAL LATITUDE, ELEVATION

      LOGICAL wATER
      INTEGER DOY, DOYP, endsim
      INTEGER FROP, IPRINT
      
      INTEGER YRPLT, YREND, MDATE, YRDOY, YEAR
      INTEGER YRSIM
      INTEGER DYNAMIC
      CHARACTER (Len=1) :: IPLTI

!
!     simple A model soil water test 
      REAL SWFAC1, SWFAC2, LAI

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      Type (WeatherType) WEATHER
      
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      
      year = int(yrdoy / 1000)
      doy = mod(yrdoy, 1000)

      IPLTI   = ISWITCH % IPLTI
!************************************************************************
!************************************************************************
!     INITIALIZATION AND INPUT OF DATA
!************************************************************************
      
      SELECT CASE (DYNAMIC)
      CASE (RUNINIT)
          WATER = .TRUE.
          YRSIM = CONTROL % YRSIM
          FROP  = CONTROL % FROP     

          CALL SWat(CONTROL,                              &
         &    LAI, RAIN, SRAD, TMAX, TMIN, &           
         &    SWFAC1, SWFAC2,                   &             
         &    DYNAMIC)                                        

          CALL calARID(CONTROL, YRPLT, LATITUDE, ELEVATION,&
         &    SRAD, TMAX, TMIN, DEWT, RAIN, WSPD, &           
         &    DOY, YEAR, ARID, ET0, WAT,          &             
         &    DYNAMIC)                                        

          CALL SimpleB(CONTROL, YRPLT, endsim, TMAX, TMIN, & 
         &    SRAD, CO2, ARID, ET0, WAT, &               
         &    Biomass, WATER,                            &           
         &    DYNAMIC)                                        

      CASE (SEASINIT)
          WATER = .TRUE.
!          CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
          CALL GET('MGMT', 'IRRAMT', IRRAMT)
    !     Transfer values from constructed data types into local variables.
          SRAD = WEATHER % SRAD
          TMAX = WEATHER % TMAX
          TMIN = WEATHER % TMIN  
          RAIN = WEATHER % RAIN + IRRAMT
    !      CO2 = WEATHER % CO2
          DEWT = TMIN
          WSPD = 1

          LATITUDE = WEATHER % XLAT 
          ELEVATION = WEATHER % XELEV
    ! kwang soo kim jun 1
    ! default elevation was set to be 100 (no reference)     
          if (ELEVATION .LT. 0) then 
              ELEVATION = 100 
          endif 
          write(*,*) 'latitde & elev ', latitude, elevation


          CALL SWat(CONTROL,                              &
         &    LAI, RAIN, SRAD, TMAX, TMIN, &           
         &    SWFAC1, SWFAC2,                   &             
         &    DYNAMIC)                                        

          CALL calARID(CONTROL, YRPLT, LATITUDE, ELEVATION,&
         &    SRAD, TMAX, TMIN, DEWT, RAIN, WSPD, &           
         &    DOY, YEAR, ARID, ET0, WAT,          &             
         &    DYNAMIC)                                        

          CALL SimpleB(CONTROL, YRPLT, endsim, TMAX, TMIN, & 
         &    SRAD, CO2, ARID, ET0, WAT, &               
         &    Biomass, WATER,                            &           
         &    DYNAMIC)                                        

                                      

!-----------------------------------------------------------------------
!     DAILY TIME LOOP 
!-----------------------------------------------------------------------
        CASE(RATE) 

!         CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
          CALL GET('MGMT', 'IRRAMT', IRRAMT)
    !     Transfer values from constructed data types into local variables.
          SRAD = WEATHER % SRAD 
          TMAX = WEATHER % TMAX
          TMIN = WEATHER % TMIN
          RAIN = WEATHER % RAIN + IRRAMT
          DEWT = TMIN
          WSPD = 1
          
!************************************************************************
!************************************************************************
!     RATE CALCULATIONS
!************************************************************************

          IF (YRDOY .GE. YRPLT) THEN

          CALL SWat(CONTROL,                              &
         &    LAI, RAIN, SRAD, TMAX, TMIN, &           
         &    SWFAC1, SWFAC2,                   &             
         &    DYNAMIC)                                        

          CALL calARID(CONTROL, YRPLT, LATITUDE, ELEVATION,&
         &    SRAD, TMAX, TMIN, DEWT, RAIN, WSPD, &           
         &    DOY, YEAR, ARID, ET0, WAT,          &             
         &    DYNAMIC)                                        

          !write(*,*) YRDOY, RAIN, IRRAMT, RAIN-IRRAMT, ET0, WAT, ARID

          CALL SimpleB(CONTROL, YRPLT, endsim, TMAX, TMIN, & 
         &    SRAD, CO2, ARID, ET0, WAT, &               
         &    Biomass, WATER,                            &           
         &    DYNAMIC)                                        
          ENDIF

!************************************************************************
!************************************************************************
!     INTEGRATION OF STATE VARIABLES
!************************************************************************
       Case(INTEGR)
 
          IF (YRDOY .GE. YRPLT) THEN
          CALL calARID(CONTROL, YRPLT, LATITUDE, ELEVATION,&
         &    SRAD, TMAX, TMIN, DEWT, RAIN, WSPD, &           
         &    DOY, YEAR, ARID, ET0, WAT,          &             
         &    DYNAMIC)                                        

          CALL SimpleB(CONTROL, YRPLT, endsim, TMAX, TMIN, & 
         &    SRAD, CO2, ARID, ET0, WAT, &               
         &    Biomass, WATER,                            &           
         &    DYNAMIC)                                        
          ENDIF


!************************************************************************
!************************************************************************
!     WRITE DAILY OUTPUT
!************************************************************************
       Case(OUTPUT)

        IPRINT = MOD(DOY, FROP)
        IF ((IPRINT .EQ. 0) .OR. (endsim .EQ. 1) .OR. &
     &        (YRDOY .EQ. YRSIM)) THEN

          CALL SWat(CONTROL,                              &
         &    LAI, RAIN, SRAD, TMAX, TMIN, &           
         &    SWFAC1, SWFAC2,                   &             
         &    DYNAMIC)                                        

          IF (YRDOY .GE. YRPLT) THEN

          CALL calARID(CONTROL, YRPLT, LATITUDE, ELEVATION,&
         &    SRAD, TMAX, TMIN, DEWT, RAIN, WSPD, &           
         &    DOY, YEAR, ARID, ET0, WAT,          &             
         &    DYNAMIC)                                        

          CALL SimpleB(CONTROL, YRPLT, endsim, TMAX, TMIN, & 
         &    SRAD, CO2, ARID, ET0, WAT, &               
         &    Biomass, WATER,                            &           
         &    DYNAMIC)                                        
          ENDIF

        ENDIF

       IF (ENDSIM .EQ. 1) YREND = YRDOY
         
!-----------------------------------------------------------------------
!     END OF DAILY TIME LOOP 
!-----------------------------------------------------------------------

!************************************************************************
!************************************************************************
!     CLOSE FILES AND WRITE SUMMARY REPORTS
!************************************************************************
      CASE(SEASEND, ENDRUN)

          CALL SWat(CONTROL,                              &
         &    LAI, RAIN, SRAD, TMAX, TMIN, &           
         &    SWFAC1, SWFAC2,                   &             
         &    DYNAMIC)                                        

          CALL calARID(CONTROL, YRPLT, LATITUDE, ELEVATION,&
         &    SRAD, TMAX, TMIN, DEWT, RAIN, WSPD, &           
         &    DOY, YEAR, ARID, ET0, WAT,          &             
         &    DYNAMIC)                                        
      
          CALL SimpleB(CONTROL, YRPLT, endsim, TMAX, TMIN, & 
         &    SRAD, CO2, ARID, ET0, WAT, &               
         &    Biomass, WATER,                            &           
         &    DYNAMIC)                                        

!-----------------------------------------------------------------------  
      End Select
      Return
      END SUBROUTINE Simple
!#######################################################################
