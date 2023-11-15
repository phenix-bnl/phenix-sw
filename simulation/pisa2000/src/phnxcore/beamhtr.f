c $Id: beamhtr.f,v 1.3 2008/05/21 08:22:04 hpereira Exp $
*-- Author :    Surender Saini   17/06/93
 
      subroutine beamhtr

c    *************************************************************
c    *                                                           *
c    *  BEAMHTR (vsn 1.00) Geometry for beampipe heat_tape       *
c    *                                                           *
c    *  Called by ==> ::    <MUM>                                *
c    *  IN   ::   None                                           *
c    *  OUT  ::   None                                           *
c    *                                                           *
c    *  written  by ::  Surender Saini, 17/06/93 15.16.01        *
c    *  modified by ::  JPSullivan,      5/10/93                 *
c    *                  some output to LOUT instead of 6         *
c    *                                                           *
c    *************************************************************

      implicit none
c ------------------------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GUGEOM.
#include "gugeom.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEND.
c ------------------------
 
      integer iflg_bmhtr,nmed_bmhtr,color_bmhtr
      real    rad_bmhtr,thick_bmhtr,zlen_bmhtr,zpos_bmhtr
      namelist/bmhtr_par/iflg_bmhtr,nmed_bmhtr,color_bmhtr,rad_bmhtr,
     +      thick_bmhtr,zpos_bmhtr,zlen_bmhtr
 
      real tube_par(3),zpos
      character*50 par_file
      character*4 v_m_name,v_i_name
 
      integer npar,nmed,ivolu
      
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
 
c --------------------------------------------------------------------
 
      v_m_name = 'HALL'

      write( *,* ) 'beamhtr - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = bmhtr_par, err = 999 )

c Wrap heater element around the beampipe (inside the piston)

      if( iflg_bmhtr .eq. 1 ) then
 
       v_m_name = 'HALL'
       v_i_name = 'BHTR'
       nmed     = nmed_bmhtr
       npar     = 3
 
       tube_par(1) = rad_bmhtr - thick_bmhtr
       tube_par(2) = rad_bmhtr
       tube_par(3) = zlen_bmhtr
 
       call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
       call uvolattr(v_i_name,color_bmhtr)
 
       zpos = zpos_bmhtr + tube_par(3)
       call gspos(v_i_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
 
c Place the tape_heater on the -Z side too
 
       call gspos(v_i_name,2,v_m_name,0.0,0.0,-zpos,1,'ONLY')
 
CTON       write(lunplog, nml = bmhtr_par )
       write(LOUT,'(/2x,
     x       ''<BEAMHTR> : Beampipe heat tape installed'')')
 
      end if
 
      return
 
  999 continue
      stop ' BEAMHTR<E> PISA stop ... PHNX.PAR file error.'
      end
