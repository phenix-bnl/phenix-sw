*CMZ :  2.04/00 26/10/94  19.22.34  by  Mike Leitch
*CMZ :  2.03/00 16/08/93  10.33.06  by  Charles F. Maguire
*-- Author :    Surender Saini   12/04/93
 
      subroutine mum(full,nh)
c
c    *************************************************************
c    *                                                           *
c    *  MUM (vsn 1.00)   muon_arm geometry                       *
c    *                                                           *
c    *  Called by ==> ::  < GUGEOM >                             *
c    *  IN   :: full, nh                                         *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 03.04.48        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************
c
c This is the main geometry routine for Muon_arm
c
      real*4 pai, cdtr, crtd
      common/uconst/pai,cdtr,crtd
 
      character*4 full
      integer  nh
 
cc Initialize user constants
c
      pai  = acos(-1.)
      cdtr = pai/180.0
      crtd = 1./cdtr
c
c     call mumater   ! removed to GUGEOM (for general use)
c
c >> 21-MAY-1993 / SSaini
c Nosecone is now called by GUGEOM routine in phnxcore
c
c       Neutron shield, lead shield, piston plug, and beam heat tape
c       are all considered "passive volumes".
c       These are installed (or not installed) as called for in GFFGO.DAT.
c
c**      call nosecone
c <<
c     call mupplug     ! removed to GUGEOM (specify 'PLUG" in GFFGO GEOP line)
c     call muntsh      ! removed to GUGEOM (specify 'NTSH' in GFFGO GEOP line)
c     call mupbsh      ! removed to GUGEOM (specify 'PBSH' in GFFGO GEOP line)
      call mufakv(nh)
      call mutrst(full,nh)
c      call muident(full,nh)
c      call mubpip
c     call beamhtr     ! removed to GUGEOM (specfiy 'BHTP' in GFFGO GEOP line)
      return
      end
