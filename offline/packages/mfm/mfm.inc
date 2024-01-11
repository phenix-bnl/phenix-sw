c $Id: mfm.inc,v 1.1 2007/10/20 10:48:10 hpereira Exp $
c common block used for magnetic field map. 
c They are used both for PISA and offline muon reconstruction
c Author: Hugo Pereira Da Costa

      ! number of phnx passive volumes
      integer*4   phnx_pvol   
      parameter   ( phnx_pvol = 10)
      
      ! passive volume index array
      integer*4   ipvol(phnx_pvol)
      character*4 cpvol(phnx_pvol)
      equivalence (ipvol(1), cpvol(1))
      
      ! options associated to passive volumes
      integer*4   ipvolu_opt(10,phnx_pvol)
      character*4 cpvolu_opt(10,phnx_pvol)
      real*4      rpvolu_opt(10,phnx_pvol)
      equivalence (ipvolu_opt(1,1), cpvolu_opt(1,1), rpvolu_opt(1,1))
      
      common /GUMFM/ipvol,ipvolu_opt
