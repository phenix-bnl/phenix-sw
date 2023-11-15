c $Id: mun_collar.f,v 1.3 2008/05/21 08:22:00 hpereira Exp $
      subroutine mun_collar

C    **************************************************************
C    *                                                            *
C    *  mun_collar - "collar" for the south muon arm.  The collar *
C    *    goes between the backplate of the magnet and the first  *
C    *    muon identifier plane, encircling the beam pipe, and is *
C    *    necessary to keep the occupancy in the south muon       *
C    *    identifier planes to a reasonable level                 *
C    *                                                            *
C    *  Called by ==> ::  gugeom                                  *
C    *  IN   :: none                                              *
C    *  OUT  :: none                                              *
C    *                                                            *
C    *  written  by ::  Melynda Brooks, 12-May-98                 *
C    *  Revised 5/15/98 C.F. Maguire  added Pb skin option
C    *                                                            *
C    **************************************************************

 
      implicit        none

#include "mun_collar.inc"
#include "guphnx.inc"
 
C --------------------------------------------------------------------------

      real par_pgon(3 + 3*maxseg)
      data par_pgon/0.0, 360.0, 2, maxseg*0., maxseg*0., maxseg*0./

      character*50 par_file
      character*4 v_m_name, v_i_name      !volume names for volume and mother
      integer npar, ivolu, i
      real zpos                           !z position in "HALL"

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

      write (6,1)
 1    format(/,'  MUN_COLLAR <I>: install South Muon Arm beam collar',/)

C ---------------------------------------------------------------------------

C  Read in volume parameters from phnx.par file:

      write( *,* ) 'mun_collar -',
     +             ' reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = mun_collar_par, err = 999 )

C  Define "mother" volume, volume name to create, and parameters for 
C  volume: 

      v_m_name = 'HALL'
      v_i_name = 'MUCO'
     
      par_pgon(3) = nseg_collar
      npar = par_pgon(3)*3 + 3
      do i = 1, nseg_collar
        par_pgon(i*3 + 1) = z_collar(i)
        par_pgon(i*3 + 2) = rmin_collar(i)
        par_pgon(i*3 + 3) = rmax_collar(i)
      end do 

      call gsvolu(v_i_name, 'PCON', nmed_collar, par_pgon, npar, ivolu)

      if (color_collar .gt. 0) then
        call gsatt(v_i_name, 'SEEN', 1)
        call gsatt(v_i_name, 'COLO', color_collar)
      else
        call gsatt(v_i_name, 'SEEN', 0)
      end if

      zpos = 0.0
      call gspos(v_i_name, 1, v_m_name, 0.0, 0.0, zpos, 1, 'ONLY')
      if(rmax_lead(1).lt.rmax_collar(1))then
         return
      else

c     put in Pb skin

         write (6,2)
 2       format(/,'  MUN_COLLAR <I>: install Pb skin on collar',/)

         v_i_name = 'MUSK'
     
         par_pgon(3) = nseg_collar
         npar = par_pgon(3)*3 + 3
         do i = 1, nseg_collar
           par_pgon(i*3 + 1) = z_collar(i)
           par_pgon(i*3 + 2) = rmax_collar(i)  ! start at beam collar
           par_pgon(i*3 + 3) = rmax_lead(i)    ! a few cm skin depth for Pb
         end do 

         call gsvolu(v_i_name, 'PCON', nmed_lead, par_pgon, npar, ivolu)

         if (color_lead .gt. 0) then
           call gsatt(v_i_name, 'SEEN', 1)
           call gsatt(v_i_name, 'COLO', color_lead)
         else
           call gsatt(v_i_name, 'SEEN', 0)
         end if
         zpos = 0.0
         call gspos(v_i_name, 1, v_m_name, 0.0, 0.0, zpos, 1, 'ONLY')
      endif
      return

c---------------------------------------------------------------------
  999 continue
      stop 'mun_collar - PISA stop, geometry error.'

      end
