!########################################################################
!## Kwang Soo Kim
!## Seoul National University
!## 2017-APR-27
!########################################################################
!## DEBUGGING VERSION   2017-May-30
!## difference at the precesion of 0.00005
!########################################################################
!## Documentation is needed
!##
!########################################################################

Module smplB
!
!
Contains
!************************************************************************
!************************************************************************
!*     Subroutine SimpleB_PLANT
!*     This subroutine simulates the growth of the plant using 
!*     a simplified process
!****************************************************************************
!
!*                  LIST OF VARIABLES 
!*     TT        = daily accumulated temperature above tb (degree days)
!*     dBiomass  = total plant dry matter (kg/ha/d)
!*     Biomass   = biomass of a crop on a given day (kg/ha)     
!*     Yield     = yield of a crop on a given day (kg/ha)     
!*     fPAR      = fraction of PAR 
!*     F_Water   = water stress factor  
!*     F_CO2     = adjustment factor of a given CO2 concentration 
!*     F_Heat    = heat streass factor
!*     F_temperature = temperature factor 
!*     HI        = Harvest Index 
!*     dI50B     = change in temperature factor on fPAR
!*     SRAD  = Daily solar radiation (MJ m-2)
!*     TMAX  = Daily maximum temperature (c)
!*     TMIN  = Daily manimum temperature (c)
!*     TMN   = Daily mean temperature (c)
!*     Arid  = daily arid index 
!*     CO2   = CO2 concentration 
!************************************************************************

      SUBROUTINE SimpleB( CONTROL, YRPLT, &
     &    endsim,TMAX,TMIN, SRAD, CO2, Arid,ET0, WAT,&    !Input
     &    Biomass,&                              !Output
     &    WATER, DYNAMIC)                        !Control

!-----------------------------------------------------------------------      
      USE ModuleDefs    !, only : ControlType
      IMPLICIT NONE 
      SAVE 

      REAL TT
      REAL dBiomass
      REAL Yield
      REAL fPAR
      REAL F_Water
      REAL F_CO2
      REAL F_Heat
      REAL F_Temperature 
      REAL dI50A
      REAL dI50B
      REAL Biomass
      REAL TMAX, TMIN, SRAD, TMEAN
      REAL CO2
      REAL HI
      REAL Arid
      REAL ET0
      REAL WAT
      REAL dTT

! parameters
      REAL Tbase
      REAL Tsum
      REAL Topt
      REAL RUE
      REAL HIP
      REAL I50A     ! ##############deteming the leaf growth dynamic, larger values indicate faster LAI development
      REAL I50B     ! ##############determing the time for half of leaves senescenced
      REAL I50maxH   ! #####################The maximum reduction in I50A and I50B due to heat stress
      REAL I50maxW
      REAL maxT     ! ##############threshold temperature for heat stress effects on RUE
      REAL extremeT ! ##############extreme threshold temperature for heat stress effects on RUE
      REAL CO2_RUE  ! ##############change of RUE with per 100ppm increase in CO2
      REAL S_Water
      REAL PCARB
      REAL prevFPAR
      REAL prevdBio
      REAL prevYield
      REAL prevBiom
      REAL PD, PLRS

      REAL fPARX

! control variables
      INTEGER YRDOY, YEAR, DOY, DAS, DAP, YRPLT, YREND, endsim
      INTEGER DYNAMIC
      LOGICAL WATER
      INTEGER WTR ! water paramter from cultivar file
      INTEGER COUNT

     CHARACTER(Len=12) :: OUTG
     LOGICAL FEXIST, FIRST
     Integer NOUTDG, ERRNUM, RUN
     Integer ADAT
     INTEGER MDAT
     INTEGER TIMDIF 
     
     INTEGER LUNIO, ERR, LINC, LNUM, FOUND
     CHARACTER(Len=30) :: FILEIO
     CHARACTER(Len=6) :: ERRKEY
     CHARACTER(Len=6) :: SECTION 
     PARAMETER (ERRKEY = 'SMPLB')

!    Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
     INTEGER, PARAMETER :: SUMNUM = 8
     CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
     REAL, DIMENSION(SUMNUM) :: VALUE


     TYPE (ControlType) CONTROL
     
     FILEIO = CONTROL % FILEIO
     LUNIO  = CONTROL % LUNIO
     YRDOY  = CONTROL % YRDOY
     DAS    = CONTROL % DAS


     DYNAMIC = CONTROL % DYNAMIC 
     RUN     = CONTROL % RUN

!************************************************************************
!************************************************************************
!     INITIALIZATION
!************************************************************************
       If (DYNAMIC == RUNINIT) Then
!************************************************************************

        ADAT = -99
        MDAT = -99

! read species parameters
! maize parameter for testing
        Tbase = 8
        Tsum = 1950
        Topt = 24
        RUE = 1.6
        HI = 0.44
        I50A = 520
        I50B = 1900
        I50maxH = 100
        I50maxW = 5
        maxT = 36
        extremeT = 50
        CO2_RUE = 0.05
        S_Water = 4.0

        fPARX = 0.
!spBplant.f90
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
!          READ(LUNIO,'(37X,20F6.0, 3X, A2, F6.0)',IOSTAT=ERR)
          READ(LUNIO,'(37X,13F6.0, 30X, I6, F6.0)',IOSTAT=ERR) Tbase, Tsum, Topt, RUE, HIP, &
       &       I50A,I50B, I50maxH, I50maxW,maxT, extremeT, CO2_RUE, S_Water, &
!  skip  AWC, RCN, DDC, WUC, RZD
       &       WTR, CO2
!  skip LAT, ELEV
         write(*,*) 'simple io', tbase, tsum, topt, rue, hip, i50a, i50b, wtr, co2, co2_rue, S_water, extremeT
!        LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          if (WTR .eq. 1) then
              WATER = .TRUE.
          else
              WATER = .false.
          endif

        ENDIF


!       Find planting densityLFMAX
        LNUM = 0
        SECTION = '*PLANT'
        REWIND(LUNIO)
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          LNUM = LNUM + 1
          READ(LUNIO,'(3X,I7,8X,F6.0,18X,F6.0)',IOSTAT=ERR) YRPLT, PD, PLRS 
!        LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF   
        
        CLOSE (LUNIO)
        
! prepare output
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)

        !      Initialize daily growth output file
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD', &
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW', &
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF
        
        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)
        
!        WRITE(NOUTDG,11)
        WRITE(NOUTDG,12)
!       Old style output header
!   11   FORMAT('Results of plant growth simulation: ')
!   12   FORMAT(/ &
!     &/,'                Accum', &
!     &/,'       Number    Temp                                    Leaf', &
!     &/,'  Day      of  during   Plant  Canopy    Root   Fruit    Area', &
!     &/,'   of    Leaf  Reprod  Weight  Weight  Weight  weight   Index', &
!     &/,' Year   Nodes    (oC)  (g/m2)  (g/m2)  (g/m2)  (g/m2) (m2/m2)', &
!     &/,' ----  ------  ------  ------  ------  ------  ------  ------')

!       CSM style output header
!   11   FORMAT('*GROWTH ASPECTS OUTPUT FILE')
   12   FORMAT( &
     & '!', &
     &/,'!', &
     &/,'@YEAR DOY   DAS    DAP    TMAX     TT     SRAD      ', &
        'fPAR prevfPAR   BIO prevBIO     YLD  prevYLD   dBIO    FWTR    FCO2    F_HT    FTMP      ', &
        'HI    d50B    d50A       ARID       WAT       ET0')

        ENDSIM = 0



      else if (DYNAMIC == SEASINIT) then
        COUNT = 0
        dTT = 0
        TT = 0
        prevdBio = 0
        Biomass = 1
        fPAR    = 0.001
        dI50A   = I50A
        dI50B   = I50B

        prevfPAR = 0.001
        prevYield = 0
        prevBiom = 0
!************************************************************************
!************************************************************************
!     RATE CALCULATIONS
!************************************************************************
       Else If (DYNAMIC == RATE) Then
!************************************************************************

          TMEAN = (TMAX + TMIN)*0.5
          dTT = max(TMEAN-Tbase,0.)

        ! calculate stress
          call calSTRS(Arid, S_Water, tmax, maxT, extremeT, F_water, F_heat)
          if (Water .EQV. .FALSE.) then
              F_Water = 1
          endif

          ! 10 is used to convert m2 to ha  (g/MJ/m2 ->  kg/ha)
          PCARB = SRAD * 10. * RUE * fPAR *  min(F_Water, F_Heat)
 
          ! calculate daily increase of biomass
          call calGROW(TMEAN, tbase, topt, CO2, CO2_RUE, PCARB, F_Temperature, F_CO2, dBiomass)

          ! calculate fPAR for next day sim
          ! TT + dTT is needed for temp 
          prevFPAR = fPAR
          call calFPAR(TT+dTT, I50A, I50maxW, I50maxH, F_water, F_Heat, fPAR, dI50A, dI50B)
          write(*,*) dTT, TT, fPAR
!************************************************************************
!************************************************************************
!     INTEGRATION
!************************************************************************
       Else If (DYNAMIC == INTEGR) Then 
!************************************************************************
           prevYield = Yield
           prevBiom = Biomass
           if(fPAR < prevFPAR .AND. fPAR <=0.005) then
               write(*,*) "Crop maturity due to senescence!!!!!!!!!!"
               dBiomass = 0
               endsim = 1
           endif
          
           if (TT .LE. Tsum .and. yrdoy .ge. yrplt ) then
               Biomass = Biomass + dBiomass
           else
               dBiomass = 0
               write (*,*) "Crop maturity!!!!!!!!!! dbiomass = zero"
               endsim = 1
           endif

           HI = HIp
           Yield = Biomass * HI
           TT = TT + dTT


!************************************************************************
!************************************************************************
!     OUTPUT
!************************************************************************
       Else If (DYNAMIC == OUTPUT) Then
!************************************************************************
!       Old style output
!        WRITE(NOUTDG,20) DOY,n,int,w,wc,wr,wf,lai
!       CSM style output
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY)+1)
        IF (DAP > DAS) DAP = 0
        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE(NOUTDG,20) YEAR, DOY, DAS, DAP, Tmax, TT, SRAD, &
         fPAR, prevfPAR, Biomass, prevBiom, Yield, prevYield, dBiomass, F_Water, F_CO2, F_Heat, F_Temperature, &
         HI, dI50B, dI50A, ARID, WAT, ET0
   20   FORMAT(1X,I4,1X,I3, 2(1X,I5), 3(1X, F7.2), 7(1X, F13.6), 1X,F8.2, 10(1X,F7.2))


        IF (COUNT .EQ. 23) THEN
          COUNT = 0
!          WRITE(*,30)
!   30     FORMAT(2/)
!          WRITE(*,12)
        ENDIF

!        WRITE(*,20) YEAR, DOY,DAS, Biomass, Yield, dBiomass, F_Water, F_CO2, F_Heat, F_Temperature, HI, dI50B, ARID
        COUNT = COUNT + 1

!************************************************************************
!************************************************************************
      Else If (DYNAMIC == SEASEND) Then
!************************************************************************
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.

!        LABEL(1)  = 'MDAT'; VALUE(1)  = FLOAT(MDAT)
!        LABEL(2)  = 'CWAM'; VALUE(2)  = WC + WF
!        LABEL(3)  = 'HWAM'; VALUE(3)  = WF
!        LABEL(4)  = 'HWAH'; VALUE(4)  = WF
!        LABEL(5)  = 'BWAH'; VALUE(5)  = WC / 10.
!        LABEL(6)  = 'LAIX'; VALUE(6)  = LAIX
!        LABEL(7)  = 'HIAM'; VALUE(7)  = WF / (WC + WF)

        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(ADAT)
        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(MDAT)
        LABEL(3)  = 'CWAM'; VALUE(3)  = Biomass
        LABEL(4)  = 'HWAM'; VALUE(4)  = Yield
        LABEL(5)  = 'HWAH'; VALUE(5)  = -99
        LABEL(6)  = 'BWAH'; VALUE(6)  = -99
        LABEL(7)  = 'LAIX'; VALUE(7)  = -99
        LABEL(8)  = 'HIAM'; VALUE(8)  = HI

!        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
!        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(MDAT)
!        LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
!        LABEL(4)  = 'CWAM'; VALUE(4)  = TOPWT*10.
!        LABEL(5)  = 'HWAM'; VALUE(5)  = HWAM
!        LABEL(6)  = 'HWAH'; VALUE(6)  = HWAH
!        LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
!        LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT     !*1000.
!        LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
!        LABEL(10) = 'H#UM'; VALUE(10) = PSPP
!        LABEL(11) = 'NFXM'; VALUE(11) = WTNFX*10.
!        LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
!        LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
!        LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.
!        LABEL(15) = 'PWAM'; VALUE(15) = PODWT * 10.
!        LABEL(16) = 'LAIX'; VALUE(16) = LAIMX
!        LABEL(17) = 'HIAM'; VALUE(17) = HI
!        LABEL(18) = 'EDAT'; VALUE(18) = FLOAT(YREMRG)

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF



!************************************************************************
!************************************************************************
      Else If (DYNAMIC == ENDRUN) Then
!************************************************************************
       CLOSE(NOUTDG)   

!************************************************************************
!************************************************************************
      ENDIF
!************************************************************************
      RETURN
      END SUBROUTINE SimpleB
!************************************************************************

!************************************************************************
!*     Subroutine calStrs
!*     Calculates water and heat stress factor
!!-----------------------------------------------------------------------
!*     Input:  Arid, S_Water, TMAX, MaxT, extremeT
!*     Output: F_Water, F_Heat
!*************************************************************************
      SUBROUTINE calStrs(Arid, S_Water, TMAX, MaxT, extremeT, F_Water, F_heat)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
      REAL Arid, S_Water
      REAL TMAX, MaxT, extremeT
      REAL F_Water, F_Heat
!-----------------------------------------------------------------------

        ! water stress
        F_Water = max(0.,1-S_Water*Arid)

        ! heat stress
        if (tmax .LE. maxT) then
            F_Heat = 1
        else if (tmax > extremeT) then
            F_Heat = 0
        else
            F_Heat = max(1-(tmax-maxT)/(extremeT-maxT),0.)
        endif

      end subroutine calSTRS


!************************************************************************
!*     Subroutine calFPAR
!*     Calculates water and heat stress factor
!!-----------------------------------------------------------------------
!*     Input:  TT, I50A, I50maxW, I50maxH, F_water, F_Heat
!*     Output: fPAR, dI50A, dI50B
!*************************************************************************
      SUBROUTINE calFPAR(TT, I50A, I50maxW, I50maxH, F_water, F_Heat, fPAR, dI50A, dI50B)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
      REAL TT, I50maxW, I50maxH
      REAL F_water, F_Heat
      REAL fPAR, dI50A, dI50B
      REAL fPAR1, fPAR2
      REAL dI50A1, dI50A2
      REAL dI50B1, dI50B2
      REAL fPAR_Water


!!!!  To be checked
      REAL I50A

!-----------------------------------------------------------------------

      ! calculate daily fPAR
      ! fPAR1 = min(1.,1/(1+exp(-0.01*(TT-I50A))))
      fPAR1 = min(1.,1/(1+exp(-0.01*(TT-dI50A))))
      fPAR2 = min(1.,1/(1+exp(0.01*(TT-dI50B))))

      ! Water stress effects on light interception
      fPAR_Water = 1
      if (F_Water < 0.1) then
          fPAR_Water = 0.9 + F_Water
      endif

      fPAR  = min(fPAR1,fPAR2) * min(fPAR_Water, 1.)

!      write(*,*) dI50A1, dI50A2, dI50B1, dI50B2, fPAR1, fPAR2

      !reduandant
      !dI50A[day+1] = dI50A[day]
      dI50A1 = dI50A + I50maxW*(1-F_water)
      dI50A2 = dI50A + I50maxH*(1-F_Heat)
      dI50A = max(max(0., dI50A1), max(0., dI50A2))

      !dI50B[day+1]<-dI50B[day]
      dI50B1 = dI50B - I50maxW*(1-F_Water)
      dI50B2 = dI50B - I50maxH*(1-F_Heat)
      dI50B  = min(max(0.,dI50B1),max(0.,dI50B2))

      return
      end subroutine calFPAR


!************************************************************************
!*     Subroutine calFPARX
!*     Calculates water and heat stress factor
!!-----------------------------------------------------------------------
!*     Input:  TT, I50A, I50maxW, I50maxH, F_water, F_Heat
!*     Output: fPAR, dI50A, dI50B
!*************************************************************************
      SUBROUTINE calFPARX(TT, F_water, F_Heat, fPAR, dI50A, dI50B)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
      REAL TT, I50maxW, I50maxH
      REAL F_water, F_Heat
      REAL fPAR, dI50A, dI50B
      REAL fPAR1, fPAR2
      REAL dI50A1, dI50A2
      REAL dI50B1, dI50B2
      REAL fPAR_Water


!!!!  To be checked
      REAL I50A

!-----------------------------------------------------------------------

      ! calculate daily fPAR
      ! fPAR1 = min(1.,1/(1+exp(-0.01*(TT-I50A))))
      fPAR1 = min(1.,1/(1+exp(-0.01*(TT-dI50A))))
      fPAR2 = min(1.,1/(1+exp(0.01*(TT-dI50B))))

      ! Water stress effects on light interception
      fPAR_Water = 1
      if (F_Water < 0.1) then
          fPAR_Water = 0.9 + F_Water
      endif

      fPAR  = min(fPAR1,fPAR2) * min(fPAR_Water, 1.)

      return
      end subroutine calFPARX



!************************************************************************
!*     Subroutine calGROW
!*     Calculates daily increase of biomass
!!-----------------------------------------------------------------------
!*     Input:  TMIN, TMAX, tbase, topt, CO2, CO2_RUE, PMAX
!*     Output: F_temperature, F_co2, dBiomass
!*************************************************************************
       SUBROUTINE calGROW(TMEAN, tbase, topt, CO2, CO2_RUE, PCARB,&
      & F_temperature, F_CO2, dBiomass)

!-----------------------------------------------------------------------
       IMPLICIT NONE
       SAVE
       REAL TMEAN, tbase, topt
       REAL CO2, CO2_RUE
       REAL PCARB
       REAL F_Temperature, F_CO2, dBiomass
!-----------------------------------------------------------------------

      ! calculate temperatrure factor
       if(Tmean .GE. Topt) then
          F_Temperature = 1
       else
          F_Temperature = max((Tmean-Tbase)/(Topt-Tbase),0.)
       endif

       if (CO2 .GE. 700) then
          F_CO2 = 1+CO2_RUE*350
       else
          F_CO2 = max((CO2_RUE*CO2*0.01+1-0.01*350*CO2_RUE),1.)
       endif

       dBiomass = PCARB * F_Temperature * F_CO2
      
       return
       end subroutine calGROW

End Module smplB
