c $Id: mupbsh.f,v 1.3 2008/05/21 08:22:00 hpereira Exp $
*-- Author :    Surender Saini   12/04/93
 
      subroutine mupbsh

c    *************************************************************
c    *                                                           *
c    *  MUPBSH (vsn 1.00) muon_arm Lead_shield geometry          *
c    *                                                           *
c    *  Called by ==> :: < MUM >                                 *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 01.46.14        *
c    *  modified by ::  JPSullivan Oct 5, 1993                   *
c    *                  Some output to LOUT from GCUNIT          *
c    *                                                           *
c    *************************************************************

c Put lead curtain into the muon arm

c -----------------------
CTON+CDE,PISALUN.
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEEP,GUGEOM.
#include "gugeom.inc"
*KEND.
c -----------------------
 
      real*4 pai, cdtr,crtd
      common/uconst/pai,cdtr,crtd
 
      real*4 pist_ang,plug_ang,shad_ang,shneuz2,pbcurz2,znose1,
     +   zncref,thncref
      common /ugeom_muon2/ pist_ang,plug_ang,shad_ang,shneuz2,
     +  pbcurz2,znose1,zncref,thncref
 
      real*4 mega_irad1(2),mega_irad2(2),mega_z12(2,2),mega_thick
      common /ugeom_muon1/mega_irad1,mega_irad2,mega_z12,mega_thick
 
 
      character*4 v_m_name,v_c_name
 

c     IBM compiler reports error in initializing varaiables in common block

c     data pbcurz1/120.1/
c     data pbcurt/0.0,12.01,90.502,13.01,98.037/
 
      real*4 pbcurt(5),pbcurz1
      integer*4 ipbshflg,nmed_pbsh,num_phsh
      namelist /pbsh_par/ pbcurt,pbcurz1,nmed_pbsh,ipbshflg,num_pbsh
 
      common /mugeom_pbsh/ pbcurt,pbcurz1,nmed_pbsh,ipbshflg,num_pbsh
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
 
c -------------------------------------------------------------

      
      v_m_name = 'HALL'

c Put Lead curtain into the muon arm

 
      write( *,* ) 'mupbsh - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = pbsh_par, err = 999 )
   
      if(ipbshflg .ne. 0)then
       nmed = nmed_pbsh
       npar = 5
       v_c_name = 'MUPB'
 
       if(pbcurz1 .le. 0.0)pbcurz1 = shneuz2
       pbcurz2 = pbcurz1 + 2.*abs(pbcurt(1))
 
       if(pbcurt(1) .lt. 0.0)then
        pbcurt(1) = abs(pbcurt(1))
        pbcurt(2) = pbcurz1*tan(plug_ang*cdtr)
        pbcurt(3) = pbcurz1*tan(shad_ang*cdtr)
        pbcurt(4) = pbcurz2*tan(plug_ang*cdtr)
        pbcurt(5) = pbcurz2*tan(shad_ang*cdtr)
       end if
 
       call gsvolu(v_c_name,'CONE',nmed,pbcurt,npar,ivolu)
 
       zpos = pbcurz1 + pbcurt(1)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
c position second encap lead shield if requested
       if(num_pbsh.eq.2) then
         irot=irot+1
         call gsrotm(irot,90.,0.,90.,90.,180.,0.)
         call gspos(v_c_name,2,v_m_name,0.0,0.0,-zpos,irot,'ONLY')
       endif
       call gsatt(v_c_name,'SEEN',1)
 
CTON       write(lunplog,nml=pbsh_par)
       write(LOUT,'(/5x,''PBSH placed at Z='',f10.4,'' cms'')')pbcurz1
       write(LOUT,'(2x,''<MUPBSH> : Pb curtain installed '')')
CTON       call prmater(nmed)
 
      end if
      return
 
  999 continue
      stop ' mupbsh - PISA stop, geometry error.'
      end
