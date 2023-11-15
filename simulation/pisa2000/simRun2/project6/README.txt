     Hadron PWG  requests   Deuteron and Anti-Deuteron Simulations

    * Contact person: Anuj Purwar
    * Event type: Exodus/OSCAR
    * Event quantity: 1 Million events
    * Event vertex: -30 to +30 cm (flat)
    * Event rapidity: -0.6 to +0.6 (flat)
    * Event azimuthal range: East Arm Only (flat)
    * Event pt: 0 to 10 GeV/c, low pt enhancement and flat above 1 GeV/c
    * Magnetic field: On (Sim3D01.root)
    * Projected CPU-hours: Approx. 1000
    * Projected output files size: Not specified
    * Special qualifications: Keep the original Exodus input files


Notes: 1) I made changes in Exodus to add deuterons/anti-deuterons with
          ids 45 and 55 respectively (same as PISA ids) and this
	  change was propagated to PISA by modifying gtran.f and pdgtran.f
	  routines to allow deuterons/anti-deuterons to "pass through".

       2) Hadronic cross sections and interactions need to be implemented
          in Geant/Fluka for deuterons/anti-deuterons. Probable recipe is
	  1.4 times proton cross section as done by STAR.



