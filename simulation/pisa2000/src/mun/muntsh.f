c $Id: muntsh.f,v 1.3 2008/05/21 08:22:01 hpereira Exp $
*-- Author :    Surender Saini   12/04/93
 
      subroutine muntsh

c    *************************************************************
c    *                                                           *
c    *  MUNTSH (vsn 1.00)  muon_arm neutron_shield geometry      *
c    *                                                           *
c    *  Called by ==> :: < MUM >                                 *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 01.59.25        *
c    *  modified by ::  JPSullivan, Oct 5, 1993                  *
c    *                  some output to LOUT from GCUNIT          *
c    *                                                           *
c    *************************************************************

c Put neutron shield ( Borated polyethylene ) between magnet & Pb_shield

c Author : S. Saini / 10-MAR-1993
 
c ----------------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEEP,GUGEOM.
#include "gugeom.inc"
*KEND.
c ----------------
 
      character*4 v_m_name,v_c_name
 
      real*4 pai, cdtr,crtd
      common/uconst/pai,cdtr,crtd
 
      real*4 pist_ang,plug_ang,shad_ang,shneuz2,pbcurz2,znose1,
     +   zncref,thncref
      common /ugeom_muon2/ pist_ang,plug_ang,shad_ang,shneuz2,
     +  pbcurz2,znose1,zncref,thncref
 
      real*4 shneut(5),shneuz1
      integer*4 nmed_ntsh,ntshflg,num_ntsh
      namelist /ntsh_par/shneuz1,nmed_ntsh,shneut,ntshflg,num_ntsh
 
      common /mugeom_ntsh/shneuz1,nmed_ntsh,shneut,ntshflg,num_ntsh
 
c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c -----------------------------------------------------------

c Read the geometery file segment

 
      v_m_name = 'HALL'
 
      if(plug_ang .eq. 0.0)plug_ang=pist_ang

c Put Neutron_shield in the muon arm

      write( *,* ) 'muntsh - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = ntsh_par, err = 999 )
 
      if(ntshflg .ne. 0)then
 
       nmed = nmed_ntsh
       npar = 5
       v_c_name = 'MUNS'
       shneuz2 = shneuz1 + 2.*abs(shneut(1))
       if(shneut(1) .lt. 0.0 )then
        shneut(1) = abs(shneut(1))
        shneut(2)=shneuz1*tan(plug_ang*cdtr)
        shneut(3)=shneuz1*tan(shad_ang*cdtr)
        shneut(4)=shneuz2*tan(plug_ang*cdtr)
        shneut(5)=shneuz2*tan(shad_ang*cdtr)
       end if
 
       call gsvolu(v_c_name,'CONE',nmed,shneut,npar,ivolu)
 
       zpos = shneuz1 + shneut(1)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
c position second arm neutron shield if requested
       if(num_ntsh.eq.2) then
         irot=irot+1
         call gsrotm(irot,90.,0.,90.,90.,180.,0.)
         call gspos(v_c_name,2,v_m_name,0.0,0.0,-zpos,irot,'ONLY')
       endif
       call gsatt(v_c_name,'SEEN',1)
 
CTON       write(lunplog,nml=ntsh_par)
       write(LOUT,'(/5x,''NTSH placed at Z='',f10.4,'' cms'')')shneuz1
       write(LOUT,'(2x,''<MUNTSH> : Neutron Shield installed '')')
CTON       call prmater(nmed)
 
      end if
      return
 
  999 continue
      stop 'muntsh - PISA stop, geometry error.'
      end
