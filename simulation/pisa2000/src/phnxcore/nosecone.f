c $Id: nosecone.f,v 1.7 2015/03/05 18:08:22 chiu Exp $
*-- Author :    Surender Saini   12/04/93
 
      subroutine nosecone

c    *************************************************************
c    *                                                           *
c    *  NOSECONE (vsn 1.00) routine for NOSECONE geometry        *
c    *                                                           *
c    *  Called by ==> ::  <GUGEOM>                               *
c    *  IN   ::  none                                            *
c    *  OUT  ::  none                                            *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 01.30.50        *
c    *  modified by ::  JPSullivan Oct 4, 1993                   *
c    *                  some output to LOUT from GCUNIT          *
c    *                                                           *
c    *************************************************************


c THETLSH = lampshade angle + 1
c THNCREF = angle subtended by THETLSH_lampshade at zncref = -32 cm
c           e.g. atan((mega_z12(1)*tan(thetlsh))/(mega_z12(1)-zncref))
c RNCMN1 = 6.0 cm                         --> rmin at Z= ZNCONE1
c RNCMX1 = (zncone1-zncref)*tan(THNCREF)  --> rmax at Z= ZNCONE1
c RNCMN2 = RNCMN1                         --> rmin at Z= ZNCONE2
c RNCMX2 = (zncone2-zncref)*tan(THNCREF)  --> rmax at Z= ZNCONE2
c LINERFLG ... Flag to put Lead liner on the nose cone
c          = 0  ; No nosecone liner
c          = 1  ; Nosecone liner
c THLINER  ... Thickness of the nosecone liner
c
c March 2012, Hubert van Hecke
c I hijacked variable nconeflg. The old meanings still are valid, namely
c       nconeflg = 0    No nosecones
c       nconeflg = 1    Nosecones installed (default z=41 to 60cm)
c New:  nconeflg between 41.0 and 60.0: shave off from the nosecone, and replace
c                                       it with borated polyethylene.
c -----------------
#include "guphnx.inc"
#include "gcunit.inc"
#include "gugeom.inc"
c -----------------
 
      real*4 pai, cdtr,crtd
      common/uconst/pai,cdtr,crtd
 
 
      real*4 pist_ang,plug_ang,shad_ang,shneuz2,pbcurz2,znose1,
     +   zncref,thncref
      common /ugeom_muon2/ pist_ang,plug_ang,shad_ang,shneuz2,
     +  pbcurz2,znose1,zncref,thncref
 
      real*4 mega_irad1(2),mega_irad2(2),mega_z12(2,2),mega_thick
      common /ugeom_muon1/mega_irad1,mega_irad2,mega_z12,mega_thick
 
      character*4 v_m_name,v_c_name
 
      integer*4 nmed_nc,nmed_liner,color_ncone,linerflg
      real nconeflg
      integer*4 num_nc
      integer maxseg, maxseg3
      parameter (maxseg = 100)
      parameter (maxseg3 = 3*maxseg)
      integer nseg_nc(2)
      real*4 pncone(3+3*maxseg)
      real*4 zncone(maxseg,2)
      real*4 rncmn(maxseg,2),rncmx(maxseg,2)
      real*4 thetlsh,thliner
      integer ia,is
 
      data zncone/maxseg*0.,maxseg*0./
      data pncone/0.0,360.0,2,maxseg3*0./
      data thetlsh/38.0/,nmed_nc/17/      ! cfm fix for IBM
      data color_ncone/3/, num_nc/2/
      data linerflg/0/, nmed_liner/18/, thliner/5.0/
 
      real zmin_test

      namelist /ncone_par/ nmed_nc,nseg_nc,zncone,rncmn,rncmx,
     +            thncref,thetlsh,linerflg,nmed_liner,
     +            zncref,thliner,nconeflg,color_ncone,num_nc
 
c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c Put Nose Cone in front of the magnet yoke

      v_m_name = 'HALL'
 
      write( *,* ) 'nosecone - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = ncone_par, err = 999 )

      if (nconeflg.eq.1) nconeflg = 41.0    ! retain old functionality
      zmin_test = nconeflg                  ! divide between poly and brass
      if (nconeflg.lt.41.0) nconeflg = 0.0  ! retain old functionality
      if (nconeflg.ge.60.0) nconeflg = 0.0  ! illegal divide
      if (nconeflg.eq.0) write(LOUT,*)
     &   'Nosecone flag 0 - no nosecones'
      if (nconeflg.eq.46) write(LOUT,*)
     &   'Nosecone flag 1 - two 19-cm brass nosecones'
      if (nconeflg.gt.41.and.nconeflg.lt.60) write (LOUT,*)
     &  'Nosecone flag',nconeflg,': brass/polyethylene mix'
      
      if(nconeflg .ne. 0)then

c     Check the Run2 or Run3 versions of the nosecone
c     The Run3 version is 1 cm thinner for all ZNCONE

         if(RHICRUN.EQ.2.and.ZNCONE(1,1).ne.40.0)then
            write(6,*) ' Inconsistent RHIC Run and ZNCONE(1,1)'
            write(6,*) ' RHICRUN (from SETRHIC command) is set at 2'
            write(6,*) ' ZNCONE(1,1) (from geometry) is not set at 40'
            write(6,*) ' You must change RHICRUN or all ZNCONE'
            stop ' PISA stopping in NOSECONE'
         endif

         if(RHICRUN.GE.3.and.ZNCONE(1,1).ne.41.0)then
            write(6,*) ' Inconsistent RHIC Run and ZNCONE(1,1)'
            write(6,*) ' RHICRUN (from SETRHIC command) is set at 3'
            write(6,*) ' ZNCONE(1,1) (from geometry) is not set at 41'
            write(6,*) ' You must change RHICRUN or all ZNCONE'
            stop ' PISA stopping in NOSECONE'
         endif

       do ia = 1,num_nc         ! loop over 2 nosecones
 
        nmed     = nmed_nc
        npar = 3 * (nseg_nc(ia) + 1)
        do is = 1,nseg_nc(ia)
          if(ia.eq.1) then
            v_c_name = 'NSCN'
            isoff = 3*(is-1)+1
          else
            v_c_name = 'NCSN'
            isoff = 3*(nseg_nc(ia)-is)+1
          endif
          tmp = zncone(is,ia)

          if (tmp.gt.30.and.tmp.lt.zmin_test)
     &          tmp = zmin_test
          pncone(3+isoff) = tmp

          if (tmp.lt.-30.and.tmp.gt.-zmin_test)
     &          tmp = -zmin_test
          pncone(3+isoff) = tmp
           
          pncone(4+isoff) = rncmn(is,ia)
          pncone(5+isoff) = rncmx(is,ia)
        enddo   ! end do is = 1,nseg_nc(ia)
 
        pncone(3) = nseg_nc(ia)
        call gsvolu(v_c_name,'PCON',nmed,pncone,npar,ivolu)
        call gsatt(v_c_name,'SEEN',1)
c***       call uvolattr(v_c_name,color_ncone)
        zpos = 0.
        call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       enddo         ! do ia = 1, num_nc (=2 nosecones)
 
       write(LOUT,
     1  '(''NOSE_cone(s) placed at Z='',2f10.4,'' cms'')')
     2   zncone(1,1),zncone(1,2)
       write(LOUT,'(''nosecone - Nose-Cone ref_angle ='',f10.4)') thncref
       write(LOUT,*) 'nosecone - Nose Cone(s) installed '
       call prmater(nmed)

c Place Nosecone liner

       if (linerflg .eq. 1)then
         nmed     = nmed_liner
         do ia = 1, num_nc
           npar = 3 * (nseg_nc(ia) + 1)
 
           do is = 1,nseg_nc(ia)
             if(ia.eq.1) then
               v_c_name = 'NSLR'
               isoff = 3*(is-1)+1
             else
               v_c_name = 'RLSN'
               isoff = 3*(nseg_nc(ia)-is)+1
             endif
             pncone(3+isoff) = zncone(is,ia)
             pncone(4+isoff) = rncmx(is,ia)
             pncone(5+isoff) = rncmx(is,ia)+thliner
           enddo         ! do is = 1, nseg_nc(ia)
 
           pncone(3) = nseg_nc(ia)
           call gsvolu(v_c_name,'CONE',nmed,pncone,npar,ivolu)
           zpos = 0.
           call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
           call gsatt(v_c_name,'SEEN',1)
 
         enddo         ! do ia = 1, num_nc
 
        write(LOUT,*) 'nosecone - NoseCone_liner(s) installed'
       end if

      if (nconeflg.gt.41.0.and.nconeflg.lt.60) then  ! New: put in polyethylene (=445)
       pncone(1) = 4.00                              ! somewhat arbitrary
       pncone(2) = 53.6                              ! 
       pncone(3) = (zmin_test - 41.0)/2              ! poly thickness
       call gsvolu('POLY','TUBE',445,pncone,3,ivolu)
       call gspos('POLY',1,'HALL',0.,0.0,41.0+pncone(3),irotnull,'ONLY')
       call gspos('POLY',2,'HALL',0.,0.,-41.0-pncone(3),irotnull,'ONLY')
      endif                                          ! if do polyethylene

      end if                                         ! makenosecones yes/no
      return

c---------------------------------------------------- 
  999 continue
      stop 'nosecone - PISA stop ... geometry error.'
      end
