****************** cut here eliminating this line *******************
*CMZ :  2.04/00 19/08/93  13.15.55  by  Charles F. Maguire
*-- Author :    Surender Saini   23/04/93
 
      subroutine mumater
c
c    *************************************************************
c    *                                                           *
c    *  MUMATER (vsn 1.00)  muon_arm tracking medium definitions *
c    *                                                           *
c    *  Called by ==> ::  < MUM >                                *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 23/04/93 12.22.10        *
c    *  modified by ::  M. Brooks, 25/04/95, Add material for    *
c    *   honeycomb CSCs                                          *
c    *                                                           *
c    *************************************************************
c
      implicit none
      integer itmat,itmed,isvol,ifield,nwbuf
C
        real fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf(10)
c
c Define some default tracking cuts
c _cuts(9) => cutgam,cutele,cutneu,cuthad,cutmu,
c             cutebrm,cutmbr,cutedr,cutmdr
 
      real*4 tr_hi_cuts(9), tr_lo_cuts(9), tr_med_cuts(9)
      data tr_hi_cuts/4*0.050,0.01,0.050,0.01,2*1.e+4/
      data tr_lo_cuts/5*0.001,0.001,0.01,2*1.e+4/
      data tr_med_cuts/2*0.001,3*0.010,0.010,0.01,2*1.e+4/
 
c Define Shielding concrete
c density = 2.5 gm/cc
c Composition :: O => 52.0% ; Na => 1.5% , Al => 4.0%, Si => 32.5%
c               Ca => 6.0%  ; Fe => 4.0%
c itmat = 441 ,  itmed = 441
 
      real*4  aconcr(6),zconcr(6),wconcr(6),dconcr
      data aconcr/16.00,22.99,26.98,28.09,40.08,55.85/
      data zconcr/ 8.00,11.00,13.00,14.00,20.00,26.00/
      data wconcr/ 0.52,0.015, 0.04,0.325, 0.06, 0.04/
      data dconcr/ 2.50/
 
c Define 75% Isobutane(C4H10) + 25 % Argon mixture
c itmat = 442, itmed = 442
 
      real*4 ac4h10ar,zc4h10ar,dc4h10ar
      real*4 dens_arisb
      data ac4h10ar/53.5/, zc4h10ar/30.0/
      parameter (dens_arisb=53.5/22400)
      data dc4h10ar/dens_arisb/
 
c Define Air ( sensitive medium)
c itmat = 444, itmed = 444
 
      real*4   aair,zair,dair,radair,absair
      data aair /14.610/,zair/7.3/,dair/1.205e-03/,radair/30420.0/,
     +        absair/74688.8/
 
c Define Borated Polyethlene for Neutron shield
c CH2Bn    ; n = 0.1 for 5% borated polyethlene
c itmat = 445, itmed = 445
 
      real*4   ach2b(3),zch2b(3),wch2b(3),dch2b
      data ach2b/1.01,12.010,10.81/
      data zch2b/1.,6.,5./,wch2b/1.,2.,0.1/
      data dch2b/0.95/
c
c N2GASFS  ; Nitrogen gas field sensitive
c itmat = 446, itmed = 446
 
      real*4   an2,zn2,dn2,radn2,absn2
      data an2 /14.01/,zn2/7.0/,dn2/0.00125/,
     +     radn2/30392.0/,absn2/70240.0/
c
c Fiber glass beampipe heat_tape material
c itmat = 450, itmed = 450
 
      real*4   afibg,zfibg,dfibg,radfibg,absfibg
      data afibg/60.08/,zfibg/30./,dfibg/1.0/,
     +      radfibg/11.42/,absfibg/134.0/

caar.....Paul Kirk's new materials
C
      real*4 fe_track_thresholds(9)
      data fe_track_thresholds/4*0.150, 3*0.100e0, 2*1.e+4/
      real*4  pb_track_thresholds(9)
      data pb_track_thresholds/4*0.150, 3*0.100e0, 2*1.e+4/


c
c Define additional materials for the muon arm
 
      nwbuf = 0
c
c
c High cut_off concrete ( itmat = 441, itmed = 441)
 
      call gsmixt(441,'hi_concrete$',aconcr,zconcr,dconcr,6,wconcr)
 
      itmat = 441
      isvol = 0      ! not sensitive
      ifield = 0     ! no magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 0.3   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.5   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = 0.001  ! tracking precision (cm)
      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
      call gstmed(441,'hi_concrete$',itmat,isvol,ifield,fldmax,thmaxf,
     +            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c Special Thresholds for itmed = 441
c
      itmed = 441
      call ugtrcut(itmed,tr_hi_cuts)
 
c Low cut_off concrete ( itmat = 442, itmed = 442)
 
      call gsmixt(442,'lo_concrete$',aconcr,zconcr,dconcr,6,wconcr)
 
      itmat = 442
      isvol = 0      ! not sensitive
      ifield = 0     ! no magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 0.3   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.5   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = .001   ! tracking precision (cm)
      stmin = 0.1    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
 
      call gstmed(442,'lo_concrete$',itmat,isvol,ifield,fldmax,thmaxf,
     +            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
c
c Special Thresholds for itmed = 442
c
      itmed = 442
      call ugtrcut(itmed,tr_lo_cuts)
c
c 75% isobutane + 25% Ar for streamer tubes
c itmat = 443, itmed=443
 
      call gsmate(443,'gas (75%C4H10+25%Ar)$',ac4h10ar,zc4h10ar,
     +             dc4h10ar,1.693e04,2.899e04,ubuf,nwbuf)
c
 
      itmat = 443    ! 75% Isobutane + 25 % Argon
      isvol = 1      ! sensitive
      ifield = 0     ! no magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 0.5   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil = 0.001  ! tracking precision (cm)
      stmin = 0.05   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
 
      call gstmed(443,'streamer_tube_gas$',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c Define sensitive Air Tracking medium for Muon_fake_volumes
c itmat = 444, itmed = 444
 
      itmat = 444
      itmed = 444
 
      call gsmate(itmat,'airfs$',aair,zair,dair,radair,absair,0.,0)
 
 
      isvol  = 1     ! sensitive
      ifield = 1     ! magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 0.5   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.05  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch
 
      call gstmed(444,'AIRFS$',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c 5% Borated Polyethlene for neutron shield
c itmat = 445, itmed = 445
 
      call gsmixt(445,'ch2b$',ach2b,zch2b,dch2b,-2,wch2b)
 
      itmat = 445
      isvol = 0      ! not sensitive
      ifield = 1     ! magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil = 0.001  ! tracking precision (cm)
      stmin = 0.05   ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
 
      call gstmed(445,'Borated_poly$',itmat,isvol,ifield,fldmax,thmaxf,
     +            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c N2GASFS,  Nitrogen field sensitive
c itmat = 446, itmed = 446
 
      itmat = 446
      itmed = 446
 
      call gsmate(itmat,'N2GASFS$',an2,zn2,dn2,radn2,absn2,0.,0)
 
      isvol  = 1     ! sensitive
      ifield = 1     ! magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.02  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch
 
      call gstmed(itmed,'N2GASFS$',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c MUID_iron, Non_magnetic
c itmat = 10, itmed = 447
 
      itmat = 10
      itmed = 447
 
      isvol  = 0     ! non_sensitive
      ifield = 0     ! Non_magnetc_Iron for muid
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil  = 0.1   ! tracking precision (cm)
      stmin  = 2.0   ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch
 
      call gstmed(itmed,'Muid_Iron',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c Lead for PB_shield , Magnetic
c itmat = 448, itmed = 448
 
      itmat = 448
      call gsmate(itmat,'mu_Pb_shield$',207.19,82.0,11.35,
     +              0.56,18.5,ubuf,nwbuf)
      itmed = 448
 
      isvol  = 0     ! non_sensitive
      ifield = 1     ! magnetic Lead
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.02  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch
 
      call gstmed(itmed,'mu_Pb_Shield',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c Cu for copper strips in muid_streamer_tubes : non_magnetic
c itmat = 11, itmed = 449
 
      itmat = 11
      itmed = 449
 
      isvol  = 0     ! non_sensitive
      ifield = 0     ! non magnetic Cu
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.02  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch
 
      call gstmed(itmed,'mu_Cu_strips',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
 
c Beampipe tape_heater :: Glass fiber
c itmat = 450, itmed = 450
 
      itmat = 450
      itmed = 450
      call gsmate(itmat,'heater_fbgl$',afibg,zfibg,dfibg,
     +   radfibg,absfibg,0.,0)
 
      isvol  = 0     ! non_sensitive
      ifield = 0     ! non magnetic Cu
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.02  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch
 
      call gstmed(itmed,'heater_fibgl',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

C  CSC composite material, represents hexcel core covered by
C  copper-covered circuit boards. (sensitive)

      itmat = 451
      itmed = 451
      call gsmate(itmat, 'CSC hardback', 16.89, 8.423, 0.18,
     +  157., 528., 0., 0)

      isvol  = 1     ! sensitive
      ifield = 1     ! magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 0.5   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.05  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(itmed,'CSC_hardback',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

C  CSC composite material, represents hexcel core covered by
C  copper-covered circuit boards. (not sensitive)

      itmat = 451
      itmed = 452

      isvol  = 0     ! not sensitive
      ifield = 1     ! magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 0.5   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.05  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(itmed,'CSC_hardback',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c MUID_tungsten, Non_magnetic
c itmat = 12, itmed = 453

      itmat = 12
      itmed = 453

      isvol  = 0     ! non_sensitive
      ifield = 1     ! magnetic field
      fldmax = 5.0   ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.1   ! tracking precision (cm)
      stmin  = 0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(itmed,'Muid_Tungsten',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c MUID_uranium, Non_magnetic
c itmat = 14, itmed = 454

      itmat = 14
      itmed = 454

      isvol  = 0     ! non_sensitive
      ifield = 1     ! magnetic field
      fldmax = 5.0   ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.1   ! tracking precision (cm)
      stmin  = 0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(itmed,'Muid_Uranium',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c MUID_lead, Non_magnetic
c itmat = 455, itmed = 455

      itmat = 455
      call gsmate(itmat,'muid_lead$',207.19,82.0,11.35,
     +              0.56,18.5,ubuf,nwbuf)
      itmed = 455

      isvol  = 0     ! non_sensitive
      ifield = 1     ! magnetic field
      fldmax = 5.0   ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.1   ! tracking precision (cm)
      stmin  = 0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(itmed,'Muid_Lead',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

c MUID_stainlsteel, Non_magnetic
c itmat = 10, itmed = 456

      itmat = 10
      itmed = 456

      isvol  = 0     ! non_sensitive
      ifield = 1     ! magnetic field
      fldmax = 5.0   ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.1   ! tracking precision (cm)
      stmin  = 0.1   ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(itmed,'Muid_stainlsteel',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

C
C     The following code creates high threshold iron and lead for use
C     in the routines rhic_magnet_install.f.
C
      nwbuf = 0
C
C     Define material in accordance with last sentence on bottom of page
C     202 of GEANT manual!
C  
      call gsmate (1199, 'arc_iron', 55.847e0, 26.0e0, 7.87e0, 1.760e0,
     $     17.10e0, ubuf, nwbuf) 
C
C     Material is now defined.  Create tracking medium. The numerical
C     values that follow are based largely upon Surrender's values for 
C     high threshold iron.
C
      itmat = 1199   ! arc_iron   
      isvol = 0      ! not sensitive
      ifield = 0     ! no magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 2.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = 0.1    ! tracking precision (cm)
      stmin = 2.0    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
C
      call gstmed(1199,'rhic_iron',itmat,isvol,ifield,fldmax,thmaxf,
     +            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)  
C
C     Now set the thresholds for this new iron based in large part upon 
C     Surrender's old values. 
C
C     The values shown below will surely change.  These values are at
C     best guesses inserted here for the purpose of initiating
C     serious calculation. 
C
C
      itmed = 1199
      call ugtrcut(itmed, fe_track_thresholds)
C
C     Now repeat this process for the lead. 
C
      nwbuf = 0
C
C     Define material in accordance with last sentence on bottom of page
C     202 of GEANT manual!
C  
      call gsmate (1198, 'arc_lead', 207.190e0, 82.0e0, 11.350e0, 
     $     0.560e0, 18.50e0, ubuf, nwbuf) 
C
C     Material now defined.  Create tracking medium.
C
      itmat = 1198   ! arc_lead   
      isvol = 0      ! not sensitive
      ifield = 0     ! no magnetic field
      fldmax = 20.0  ! max field
      thmaxf = 2.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 2.0   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.2   ! max fractional energy loss in one step
      epsil = 0.1    ! tracking precision (cm)
      stmin = 2.0    ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.   ! tracking stop switch
C
C     Now define tracking medium.
C
      call gstmed(1198,'rhic_lead',itmat,isvol,ifield,fldmax,thmaxf,
     +            dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
C
C     Set thresholds for lead.  Please read all appropriate statements 
C     above. 
C
C
      itmed = 1198
      call ugtrcut(itmed, pb_track_thresholds)

 
      return
      end

