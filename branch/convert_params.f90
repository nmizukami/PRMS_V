!***********************************************************************
! Convert PRMS IV parameters to PRMS 5
!***********************************************************************
      SUBROUTINE convert_params()
      USE PRMS_MODULE, ONLY: Process, Dprst_flag, Nhru, Model_mode
      IMPLICIT NONE
! Functions
      EXTERNAL print_module, PRMS_open_module_file, read_error
      INTEGER, EXTERNAL :: declparam, getparam
! Parameters
      REAL, SAVE, ALLOCATABLE :: Soil_rechr_init(:), Soil_moist_init(:), Soil_rechr_max(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_max(:), Ssstor_init(:), Sat_threshold(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_allsnow(:, :), Tmax_allrain(:, :), Dprst_area(:), Dprst_frac(:), Hru_area(:)
      REAL, SAVE, ALLOCATABLE :: Tmax_allrain_offset(:, :), Soil_rechr_init_frac(:)
      REAL, SAVE, ALLOCATABLE :: Soil_moist_init_frac(:), Soil_rechr_max_frac(:), Ssstor_init_frac(:)
!      REAL, SAVE, ALLOCATABLE :: Sro_to_dprst(:)
! Local Variables
      INTEGER :: i, j, dprst_frac_flag, ounit
      CHARACTER(LEN=14), SAVE :: MODNAME
! Save Variables
      CHARACTER(LEN=80), SAVE :: Version_convert_params
!***********************************************************************
      IF ( Process(:4)=='init' ) THEN

        IF ( getparam(MODNAME, 'tmax_allsnow', Nhru*12, 'real', Tmax_allsnow)/=0 ) CALL read_error(2, 'tmax_allsnow')
        IF ( getparam(MODNAME, 'sat_threshold', Nhru, 'real', Sat_threshold)/=0 ) CALL read_error(2, 'sat_threshold')
        IF ( getparam(MODNAME, 'soil_moist_max', Nhru, 'real', Soil_moist_max)/=0 ) CALL read_error(2, 'soil_moist_max')
        IF ( Model_mode(:8)=='CONVERT4' ) THEN
          IF ( getparam(MODNAME, 'tmax_allrain_offset', Nhru*12, 'real', Tmax_allrain_offset)/=0 ) &
     &         CALL read_error(2, 'tmax_allrain_offset')
          IF ( getparam(MODNAME, 'ssstor_init_frac', Nhru, 'real', Ssstor_init_frac)/=0 ) &
     &         CALL read_error(2, 'ssstor_init_frac')
          IF ( getparam(MODNAME, 'soil_moist_init_frac', Nhru, 'real', Soil_moist_init_frac)/=0 ) &
     &         CALL read_error(2, 'soil_moist_init_frac')
          IF ( getparam(MODNAME, 'soil_rechr_init_frac', Nhru, 'real', Soil_rechr_init_frac)/=0 ) &
     &         CALL read_error(2, 'soil_rechr_init_frac')
          IF ( getparam(MODNAME, 'soil_rechr_max_frac', Nhru, 'real', Soil_rechr_max_frac)/=0 ) &
     &         CALL read_error(2, 'soil_rechr_max_frac')
          PRINT *, 'New parameters written to PRMS_4.params'
          CALL PRMS_open_module_file(ounit, 'PRMS_4.params')
          Soil_rechr_init = Soil_rechr_init_frac*Soil_rechr_max
          Soil_rechr_max = Soil_rechr_max_frac*Soil_moist_max
          Soil_moist_init = Soil_moist_init_frac*Soil_moist_max
          Ssstor_init = Ssstor_init_frac*Sat_threshold
          Tmax_allrain = Tmax_allsnow + Tmax_allrain_offset
          IF ( Dprst_flag==1 ) THEN
!            IF ( getparam(MODNAME, 'sro_to_dprst_perv', Nhru, 'real', Sro_to_dprst)/=0 ) CALL read_error(2, 'sro_to_dprst_perv')
!            IF ( getparam(MODNAME, 'dprst_frac', Nhru, 'real', Dprst_frac)/=0 ) CALL read_error(2, 'dprst_frac')
            PRINT *, 'Change parameter names sro_to_dprst_perv to sro_to_dprst and dprst_frac to dprst_frac_hru'
          ENDIF
          WRITE ( ounit, 100 ) 'soil_rechr_init', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Soil_rechr_init(i), i = 1, Nhru )

          WRITE ( ounit, 100 ) 'soil_rechr_max', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Soil_rechr_max(i), i = 1, Nhru )

          WRITE ( ounit, 100 ) 'soil_moist_init', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Soil_moist_init(i), i = 1, Nhru )

          WRITE ( ounit, 110 ) 'ssstor_init', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Ssstor_init(i), i = 1, Nhru )

!          IF ( Dprst_flag==1 ) THEN
!            WRITE ( ounit, 100 ) 'sro_to_dprst_perv', Nhru
!            WRITE ( ounit, '(F9.7)' ) ( Sro_to_dprst(i), i = 1, Nhru )
!            WRITE ( ounit, 100 ) 'dprst_frac', Nhru
!            WRITE ( ounit, '(F9.7)' ) ( Dprst_frac(i), i = 1, Nhru )
!          ENDIF

          WRITE ( ounit, 200 ) 'tmax_allrain_offset', Nhru*12
          DO i = 1, 12
            DO j = 1, Nhru
              WRITE ( ounit, '(F10.7)' ) Tmax_allrain_offset(j, i)
            ENDDO
          ENDDO
        ELSE
          IF ( getparam(MODNAME, 'tmax_allrain', Nhru*12, 'real', Tmax_allrain)/=0 ) CALL read_error(2, 'tmax_allrain')
          IF ( getparam(MODNAME, 'ssstor_init', Nhru, 'real', Ssstor_init)/=0 ) CALL read_error(2, 'ssstor_init')
          IF ( getparam(MODNAME, 'soil_moist_init', Nhru, 'real', Soil_moist_init)/=0 ) CALL read_error(2, 'soil_moist_init')
          IF ( getparam(MODNAME, 'soil_rechr_init', Nhru, 'real', Soil_rechr_init)/=0 ) CALL read_error(2, 'soil_rechr_init')
          IF ( getparam(MODNAME, 'soil_rechr_max', Nhru, 'real', Soil_rechr_max)/=0 ) CALL read_error(2, 'soil_rechr_max')
          PRINT *, 'New parameters written to PRMS_5.params'
          CALL PRMS_open_module_file(ounit, 'PRMS_5.params')
          Soil_rechr_init_frac = 0.0
          Soil_rechr_max_frac = 0.0
          Ssstor_init_frac = 0.0
          DO i = 1, Nhru
            IF ( Soil_rechr_max(i)>0.0 ) Soil_rechr_init_frac(i) = Soil_rechr_init(i)/Soil_rechr_max(i)
            IF ( Soil_moist_max(i)>0.0 ) THEN
              Soil_rechr_max_frac(i) = Soil_rechr_max(i)/Soil_moist_max(i)
              Soil_moist_init_frac(i) = Soil_moist_init(i)/Soil_moist_max(i)
            ENDIF
            IF ( Sat_threshold(i)>0.0 ) Ssstor_init_frac(i) = Ssstor_init(i)/Sat_threshold(i)
          ENDDO
          Tmax_allrain_offset = Tmax_allrain - Tmax_allsnow
          IF ( Dprst_flag==1 ) THEN
            IF ( getparam(MODNAME, 'hru_area', Nhru, 'real', Hru_area)/=0 ) CALL read_error(2, 'hru_area')
!              IF ( getparam(MODNAME, 'sro_to_dprst', Nhru, 'real', Sro_to_dprst)/=0 ) CALL read_error(2, 'sro_to_dprst')
              PRINT *, 'Change parameter name sro_to_dprst to sro_to_dprst_perv'
!              IF ( getparam(MODNAME, 'dprst_frac', Nhru, 'real', Dprst_frac)/=0 ) CALL read_error(2, 'dprst_frac')
            IF ( getparam(MODNAME, 'dprst_area', Nhru, 'real', Dprst_area)/=0 ) CALL read_error(2, 'dprst_area')
            IF ( getparam(MODNAME, 'dprst_frac_hru', Nhru, 'real', Dprst_frac)/=0 ) CALL read_error(2, 'dprst_frac_hru')
            j = 0
            DO i = 1, Nhru
              IF ( Dprst_frac(i)>0.0 ) THEN
                j = 1
                EXIT
              ENDIF
            ENDDO
            IF ( j==1 ) THEN
              PRINT *, 'Change parameter name dprst_frac_hru to dprst_frac'
              PRINT *, 'Using dprst_frac_hru instead of dprst_area'
              dprst_frac_flag = 1
              Dprst_frac = Dprst_area/Hru_area
            ELSE
              dprst_frac_flag = 0
            ENDIF
          ENDIF

          DO i = 1, Nhru
            IF ( Soil_rechr_init_frac(i)>1.0 ) Soil_rechr_init_frac(i) = 1.0
            IF ( Soil_rechr_max_frac(i)>1.0 ) Soil_rechr_max_frac(i) = 1.0
            IF ( Soil_moist_init_frac(i)>1.0 ) Soil_moist_init_frac(i) = 1.0
            IF ( Ssstor_init_frac(i)>1.0 ) Ssstor_init_frac(i) = 1.0

            DO j = 1, 12
              IF ( Tmax_allrain_offset(i,j)<0.0 ) THEN
                PRINT *, 'WARNING, negative tmax_allrain_offset:', Tmax_allrain_offset(i, j), ' set to 0'
                PRINT *, 'allsnow:', Tmax_allsnow(i, j), 'allrain:',  Tmax_allrain(i, j), ' HRU:', i, ' month:', j
                Tmax_allrain_offset(i, j) = 0.0
              ENDIF
            ENDDO
          ENDDO

          WRITE ( ounit, 100 ) 'soil_rechr_init_frac', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Soil_rechr_init_frac(i), i = 1, Nhru )

          WRITE ( ounit, 100 ) 'soil_rechr_max_frac', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Soil_rechr_max_frac(i), i = 1, Nhru )

          WRITE ( ounit, 100 ) 'soil_moist_init_frac', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Soil_moist_init_frac(i), i = 1, Nhru )

          WRITE ( ounit, 110 ) 'ssstor_init_frac', Nhru
          WRITE ( ounit, '(F9.7)' ) ( Ssstor_init_frac(i), i = 1, Nhru )

          IF ( Dprst_flag==1 ) THEN
 !           WRITE ( ounit, 100 ) 'sro_to_dprst_perv', Nhru
 !           WRITE ( ounit, '(F9.7)' ) ( Sro_to_dprst(i), i = 1, Nhru )

            WRITE ( ounit, 100 ) 'dprst_frac', Nhru
            WRITE ( ounit, '(F9.7)' ) ( Dprst_frac(i), i = 1, Nhru )
          ENDIF

          WRITE ( ounit, 200 ) 'tmax_allrain_offset', Nhru*12
          DO i = 1, 12
            DO j = 1, Nhru
              WRITE ( ounit, '(F10.7)' ) Tmax_allrain_offset(j, i)
            ENDDO
          ENDDO
        ENDIF

        CLOSE ( ounit )
 100    FORMAT ('####', /, A, /, '1', /, 'nhru', /, I6, /, '2')
 110    FORMAT ('####', /, A, /, '1', /, 'nssr', /, I6, /, '2')
 200    FORMAT ('####', /, A, /, '2', /, 'nhru', /, 'nmonths', /, I7, /, '2')

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_convert_params = 'convert_params.f90 2017-03-22 14:08:00Z'
        CALL print_module(Version_convert_params, 'Convert PRMS parameters     ', 90)
        MODNAME = 'convert_params'

        ALLOCATE ( Tmax_allsnow(Nhru,12) )
        IF ( declparam(MODNAME, 'tmax_allsnow', 'nhru,nmonths', 'real', &
     &       '32.0', '-10.0', '40.0', &
     &       'Maximum temperature when precipitation is all snow', &
     &       'Maximum air temperature when precipitation is assumed'// &
     &       ' to be snow; if HRU air temperature is less than or equal to this value, precipitation is snow', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_allsnow')

        ALLOCATE ( Sat_threshold(Nhru) )
        IF ( declparam(MODNAME, 'sat_threshold', 'nhru', 'real', &
     &       '999.0', '0.00001', '999.0', &
     &       'Soil saturation threshold, above field-capacity threshold', &
     &       'Water holding capacity of the gravity and preferential-'// &
     &       'flow reservoirs; difference between field capacity and'// &
     &       ' total soil saturation for each HRU', &
     &       'inches')/=0 ) CALL read_error(1, 'sat_threshold')

        ALLOCATE ( Soil_moist_max(Nhru) )
        IF ( declparam(MODNAME, 'soil_moist_max', 'nhru', 'real', &
     &       '2.0', '0.00001', '20.0', &
     &       'Maximum value of water for soil zone', &
     &       'Maximum available water holding capacity of capillary'// &
     &       ' reservoir from land surface to rooting depth of the'// &
     &       ' major vegetation type of each HRU', &
     &       'inches')/=0 ) CALL read_error(1, 'soil_moist_max')

        ALLOCATE ( Tmax_allrain(Nhru,12), Tmax_allrain_offset(Nhru,12) )
        ALLOCATE ( Soil_rechr_max(Nhru), Soil_rechr_init(Nhru), Soil_moist_init(Nhru), Ssstor_init(Nhru) )
        ALLOCATE ( Soil_rechr_init_frac(Nhru), Soil_rechr_max_frac(Nhru), Soil_moist_init_frac(Nhru), Ssstor_init_frac(Nhru) )

        IF ( Model_mode(:8)=='CONVERT4' ) THEN
          IF ( declparam(MODNAME, 'tmax_allrain_offset', 'nhru,nmonths', 'real', &
     &       '1.0', '0.0', '50.0', &
     &       'Precipitation is rain if HRU max temperature >= tmax_allsnow + this value', &
     &       'Monthly (January to December) maximum air temperature'// &
     &       ' when precipitation is assumed to be rain; if HRU air'// &
     &       ' temperature is greater than or equal to tmax_allsnow plus this value, precipitation is rain', &
     &       'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain_offset')

          IF ( declparam(MODNAME, 'soil_rechr_max_frac', 'nhru', 'real', &
     &         '1.0', '0.00001', '1.0', &
     &         'Fraction of capillary reservoir where losses occur as both evaporation and transpiration (soil recharge zone)', &
     &         'Fraction of the capillary reservoir water-holding capacity (soil_moist_max) where losses occur as both'// &
     &         ' evaporation and transpiration (upper zone of capillary reservoir) for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'soil_rechr_max_frac')

          IF ( declparam(MODNAME, 'soil_rechr_init_frac', 'nhru', 'real', &
     &         '0.0', '0.0', '1.0', &
     &         'Initial fraction of available water in the soil recharge zone within the capillary reservoir', &
     &         'Initial fraction of available water in the capillary reservoir where losses occur'// &
     &         ' as both evaporation and transpiration (upper zone of capillary reservoir) for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'soil_rechr_init_frac')

          IF ( declparam(MODNAME, 'soil_moist_init_frac', 'nhru', 'real', &
     &         '0.0', '0.0', '1.0', &
     &         'Initial fraction available water in the capillary reservoir', &
     &         'Initial fraction of available water in the capillary reservoir (fraction of soil_moist_max for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'soil_moist_init_frac')

          IF ( declparam(MODNAME, 'ssstor_init_frac', 'nssr', 'real', &
     &         '0.0', '0.0', '1.0', &
     &         'Initial fraction of available water in the gravity plus preferential-flow reservoirs', &
     &         'Initial fraction of available water in the gravity plus preferential-flow reservoirs'// &
     &         ' (fraction of sat_threshold) for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'ssstor_init_frac')
        ELSE
          IF ( declparam(MODNAME, 'tmax_allrain', 'nhru,nmonths', 'real', &
     &         '38.0', '-8.0', '75.0', &
     &         'Precipitation is rain if HRU max temperature >= this value', &
     &         'Monthly (January to December) maximum air temperature'// &
     &         ' when precipitation is assumed to be rain; if HRU air'// &
     &         ' temperature is greater than or equal to this value, precipitation is rain', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmax_allrain')

          IF ( declparam(MODNAME, 'soil_rechr_max', 'nhru', 'real', &
     &         '1.5', '0.00001', '5.0', &
     &         'Maximum storage for soil recharge zone', &
     &         'Maximum storage for soil recharge zone (upper portion of'// &
     &         ' capillary reservoir where losses occur as both'// &
     &         ' evaporation and transpiration); must be less than or equal to soil_moist_max', &
     &         'inches')/=0 ) CALL read_error(1, 'soil_rechr_max')

          IF ( declparam(MODNAME, 'soil_rechr_init', 'nhru', 'real', &
     &         '1.0', '0.0', '10.0', &
     &         'Initial storage of water for soil recharge zone', &
     &         'Initial storage for soil recharge zone (upper part of'// &
     &         ' capillary reservoir where losses occur as both'// &
     &         ' evaporation and transpiration) for each HRU; must be'// &
     &         ' less than or equal to soil_moist_init', &
     &         'inches')/=0 ) CALL read_error(1, 'soil_rechr_init')

          IF ( declparam(MODNAME, 'soil_moist_init', 'nhru', 'real', &
     &         '3.0', '0.0', '10.0', &
     &         'Initial value of available water in capillary reservoir', &
     &         'Initial value of available water in capillary reservoir for each HRU', &
     &         'inches')/=0 ) CALL read_error(1, 'soil_moist_init')

          IF ( declparam(MODNAME, 'ssstor_init', 'nssr', 'real', &
     &         '0.0', '0.0', '5.0', &
     &         'Initial storage in each GVR and PFR', &
     &         'Initial storage of the gravity and preferential-flow reservoirs for each HRU', &
     &         'inches')/=0 ) CALL read_error(1, 'ssstor_init')

           IF ( Dprst_flag==1 ) THEN
!            ALLOCATE ( Sro_to_dprst(Nhru) )
!            IF ( declparam(MODNAME, 'sro_to_dprst', 'nhru', 'real', &
!     &           '0.2', '0.0', '1.0', &
!     &           'Fraction of pervious surface runoff that flows into surface-depression storage', &
!     &           'Fraction of pervious surface runoff that'// &
!     &           ' flows into surface-depression storage; the remainder'// &
!     &           ' flows to a stream network for each HRU', &
!     &           'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst')
            ALLOCATE ( Dprst_area(Nhru) )
            IF ( declparam(MODNAME, 'dprst_area', 'nhru', 'real', &
     &           '0.0', '0.0', '1.0E9', &
     &           'Aggregate sum of surface-depression storage areas of each HRU', &
     &           'Aggregate sum of surface-depression storage areas of each HRU', &
     &           'acres')/=0 ) CALL read_error(1, 'dprst_area')
            ALLOCATE ( Dprst_frac(Nhru) )
            IF ( declparam(MODNAME, 'dprst_frac_hru', 'nhru', 'real', &
     &           '-1.0', '-1.0', '0.999', &
     &           'Fraction of each HRU area that has surface depressions', &
     &           'Fraction of each HRU area that has surface depressions', &
     &           'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_hru')
            ALLOCATE ( Hru_area(Nhru) )
            IF ( declparam(MODNAME, 'hru_area', 'nhru', 'real', &
     &           '1.0', '0.0001', '1.0E9', &
     &           'HRU area', 'Area of each HRU', &
     &           'acres')/=0 ) CALL read_error(1, 'hru_area')
          ENDIF
        ENDIF

      ENDIF

      END SUBROUTINE convert_params
