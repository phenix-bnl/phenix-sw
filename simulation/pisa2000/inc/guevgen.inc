c     $Id: guevgen.inc,v 1.20 2008/02/24 22:16:45 hpereira Exp $

c     Event generator interface variables
c     Global variables

c     KUIP event interface parameters
      integer evt_smooth
      parameter (evt_smooth = 1)
  
c     KUIP event interface common
      integer evt_type ! type of event selected
      real kevt_par (10) ! kuip event parameters
      logical kuip_evt
 
      common /kevgen/ kuip_evt, evt_type, kevt_par
 
c     ev_gen common (buffer for event particle kinematics information)
      integer max_mxtot ! maximum number of particles full event
      parameter (max_mxtot=20000) ! set at 20,000 for now
 
      real pptot (4, max_mxtot), xyz (3), pmc1(10), pmc2(30)
      real xyzmv (3, max_mxtot) ! distinct vertex for each particle
      integer id_parent(max_mxtot) ! parent id number
      integer idtot (max_mxtot)
      integer mxtot, ntot, maxtrk, prntrk, numevt,nrvphi_evt, nrvacc_evt
      integer neutral_evt ! counts number of times SNGL_NEUTRAL is called
      integer nrvjpsi_evt ! counts number of times RV_JPSI is called
      integer nrvchi_evt ! counts number of times RV_CHI is called
      integer ixy_rndm, ixy_change ! for XY randomization
      integer multi_xyz ! 0 ===> only 1 vertex, 1 ===> multiple

c     Additions by K. Barish
      logical logacc ! write out only accepted events	
      logical logaccepted ! particles meet acceptance criteria
      integer acc_part(3) ! filled for accepted particles 

c     Additions for Muon Arm forced accept condition
      integer MuIdWrite ! write out only events in MuID layer # MuIDWrite 
      logical MuIDAccepted ! event had particles in given MuIDWrite layer

c     addition by b.k. nandi
c     "single_acc" : single electron acceptance in the central arm
c     "iran_phi"    : for generating single particle randomly in phi
      integer single_acc, iran_phi

c     addition by c.f. maguire for forced photon conversion parameters
c     and for extra z randomization of oscar events
      real phconvrmin, phconvrmax, phconvposition, zrndmoscar
      integer phconvopt

c     addition by c.f. maguire to disable lorentz broadening of decay massc
      integer lorentzdisable

c     addition by c.f. maguire for random reaction plane angle
      real reactionplaneangle   ! in degrees (0 to 360)
      real pzconst ! polarization constant
      real v2const ! v2 anisotropy constant
      integer stplv2 ! option to set polarization and v2
      integer idpolar ! particle whose decay is polarized in its rest frame
      real rho000 ! first term in computation of rho00
      real rho00 ! spin density matrix for polarization (1/3 is no polarization)
      real alpharestframe ! angle of decay particle w.r.t. direction of parent meson (degrees, 0 to pi)
      real betarestframe ! azimuthal angle of the decay particle in the parent frame (degrees, -pi to pi)

      common /evgen/ ntot, pptot, xyz, idtot, mxtot, pmc1, pmc2,
     &  maxtrk, prntrk, numevt,nrvphi_evt,nrvacc_evt,
     &  ixy_rndm, ixy_change,nrvjpsi_evt,
     &  multi_xyz, xyzmv, id_parent, logacc, logaccepted,
     &  acc_part, nrvchi_evt, single_acc, iran_phi,
     &  neutral_evt, phconvrmin, phconvrmax, phconvopt,
     &  phconvposition, lorentzdisable,
     &  muidwrite, muidaccepted, zrndmoscar,
     &  reactionplaneangle, pzconst, v2const, stplv2,
     &  idpolar, rho000, rho00, alpharestframe,
     &  betarestframe
        
      integer pisaoutputrunnumber     ! identifier for pisa output file
      integer pisainputrunnumber      ! identifier for pisa input file
      integer pisaprojectnumber       ! which simulation project
      integer versionnumber           ! which version number 
      common /evgenid/ pisainputrunnumber, pisaoutputrunnumber,
     &  pisaprojectnumber, versionnumber 
 
      character*40 chevt_name
      common /chevgen/ chevt_name

c     original implementation of muon staged simulations (2004)
      integer nstage
      real zstage
      real pstage
      integer filter

c     additions for using the geant stack for muon staged simulations (2007)
      integer maxstagestack
      parameter(maxstagestack = 9)
      integer nstagestack
      real zstagestack(maxstagestack)
      real pstagestack(maxstagestack)
      integer filterstack(maxstagestack)
      integer nclonestack(maxstagestack)
      integer finalfilter
      integer maxancestor
      parameter (maxancestor = 3000)
      integer storeancestor(maxancestor)
      integer nstoreancestor

      real pkine2(30)   ! need more than the 10 spaces in geant's own pkine
      integer ikine2(30)   ! can use integers too
      common /ppkine/ pkine2, ikine2, nstage, zstage, pstage, filter,
     +  nstagestack, zstagestack, pstagestack,
     +  filterstack, nclonestack, finalfilter,
     +  storeancestor, nstoreancestor
 
