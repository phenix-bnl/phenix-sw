c
c     Kinematic filter common block
c     Values are set in evt_filter.f
c
c     The variable start time is also stored in this common block
c
      integer iincl      ! number of included particles
      integer include(20)  ! input list of included (or excluded) GEANT IDs

      real y_min         ! minimum rapidity
      real y_max         ! maximum rapidity
      real pt_min        ! minimum transverse momentum (GeV/c)
      real pt_max        ! maximum transverse momentum (GeV/c)
      real p_min         ! minimum total momentum (GeV/c)
      real p_max         ! maximum total momentum (GeV/c)
      real the_min       ! minimum theta (degrees)
      real the_max       ! maximum theta (degrees)
      real phi_min       ! minimum phi (degrees)
      real phi_max       ! maximum phi (degrees)
      real vrms(3)       ! vertex x,y,z width (vertex spread)
      real xyz0_input(3)   ! input X,Y,Z0 value (mean vertex position)
      real t0cent        ! centroid start time (ns)
      real t0width       ! uniform random time width (ns)

      common /kincut/ y_min, y_max, pt_min, pt_max, the_min, the_max,
     +      phi_min, phi_max, vrms, p_min, p_max, iincl, include,
     +      xyz0_input, t0cent, t0width


