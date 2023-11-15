*CMZ :  2.04/00 05/09/94  09.36.52  by  Mike Leitch
c $Id: mufakv.f,v 1.3 2008/05/21 08:21:59 hpereira Exp $
*-- Author :    Surender Saini   12/04/93
      subroutine mufakv(nh)

c    *************************************************************
c    *                                                           *
c    *  MUFAKV (vsn 1.00) muon_arm fake_volumes geometry         *
c    *                                                           *
c    *  Called by ==> ::  < MUM >                                *
c    *  IN   :: nh                                               *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 02.23.10        *
c    *  modified by ::  JPSullivan,      5/10/93                 *
c    *                  some output to LOUT instead of 6         *
c    *                                                           *
c    *************************************************************

c Put fake_volumes into the muon_arm for debugging leakage etc

c Author: S. Saini / 23-MAR-1993

c ------------------------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GUGEOM.
#include "gugeom.inc"
*KEEP,GCONST.
#include "gconst.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEND.
c ------------------------

      character*4 v_m_name,v_c_name

      real*4 pai, cdtr,crtd
      common/uconst/pai,cdtr,crtd

      real*4 pist_ang,plug_ang,shad_ang,shneuz2,pbcurz2,znose1,
     +   zncref,thncref
      common /ugeom_muon2/ pist_ang,plug_ang,shad_ang,shneuz2,
     +  pbcurz2,znose1,zncref,thncref

      real*4 mega_irad1(2),mega_irad2(2),mega_z12(2,2),mega_thick
      common /ugeom_muon1/mega_irad1,mega_irad2,mega_z12,mega_thick

      real*4 fakvol1(3),fakvol2(5),fakvol3(5),fakvol4(5),fakvol5(5)
      real*4 fakvol6(3)
      real*4 zfake1,zfake2,zfake3,zfake4,zfake5,fak3_thick,
     +         fak4_thick
      integer*4 ifak_flg(6), nmed_fake,color_fake, num_fakv
      namelist /fakv_par/ifak_flg,nmed_fake,fakvol1,fakvol2,
     +   fakvol3,fakvol4,fakvol5,zfake1,zfake2,zfake3,zfake4,zfake5,
     +   fak3_thick,fak4_thick,color_fake,fakvol6,num_fakv

      common /mugeom_fakv/ifak_flg,nmed_fake,fakvol1,fakvol2,
     +   fakvol3,fakvol4,fakvol5,zfake1,zfake2,zfake3,zfake4,zfake5,
     +   fak3_thick,fak4_thick,color_fake,fakvol6,num_fakv


      integer*4 nh,nbitsv(3),nv,idtype,nwpa,nwsa,iset,idet
      character*4  cvolnm(3),cdetnm
      data nv/1/,idtype/7/,nwpa/200/,nwsa/200/,nbitsv/3,3,3/

      character*50 par_file
      integer  ivol_stat, ia
      real tmp

c >> Added 06-MAY-1993/ S. Saini
c Change muon tracking station resolution to 0.001 cms ( 10 microns )

      real origmu(11)/3*1000.0,3*0.0,3*50.,2*0./  ! offset
      real factmu(11)/3*1000.0,1.e7,100.,1.,3*1000.,100.,2000./ !gain

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c ----------------------------------------------

      v_m_name = 'HALL'

      write( *,* ) 'mufakv - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = fakv_par, err = 999 )

      ivol_stat = 0

c Put fake_vol_1 ; in front of the nosecone


      if(ifak_flg(1) .eq. 1)then
       nmed = nmed_fake
       npar = 3
       v_c_name = 'MUF1'

       if(zfake1 .lt. 0.)then
        zfake1 = znose1 - 2.*abs(fakvol1(3))
       end if

       if(fakvol1(1) .lt. 0.)then
        fakvol1(1) = abs(fakvol1(1))
        fakvol1(2) = (zfake1-zncref)*tan(thncref*cdtr)
       end if

       call gsvolu(v_c_name,'TUBE',nmed,fakvol1,npar,ivolu)

       zpos = zfake1 + fakvol1(3)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       call gsatt(v_c_name,'SEEN',1)

       ivol_stat = 1
       write(LOUT,'(/5x,''MUF1 placed at Z='',f10.4,'' cms'')')zfake1
       write(LOUT,'(2x,''<MUFAKV> : Fake_volume_1  installed '')')
       call prmater(nmed)
      end if

c PLace Fake_volume_2 in the muon arm

      if(ifak_flg(2) .eq. 1)then
       nmed = nmed_fake
       npar = 5
       v_c_name = 'MUF2'

        if(zfake2 .le. 0.0)zfake2 = pbcurz2
        zfake22= zfake2 + 2.*abs(fakvol2(1))

       if(fakvol2(1) .lt. 0.)then
        fakvol2(1) = abs(fakvol2(1))
        fakvol2(2) = zfake2*tan(pist_ang*cdtr)
        fakvol2(3) = zfake2*tan(shad_ang*cdtr)
        fakvol2(4) = zfake22*tan(pist_ang*cdtr)
        fakvol2(5) = zfake22*tan(shad_ang*cdtr)
       end if


       call gsvolu(v_c_name,'CONE',nmed,fakvol2,npar,ivolu)

       zpos = zfake2 + fakvol2(1)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       call gsatt(v_c_name,'SEEN',1)

       ivol_stat = 1
       write(LOUT,'(/5x,''MUF2 placed at Z='',f10.4,'' cms'')')zfake2
       write(LOUT,'(2x,''<MUKAKV> : Fake_volume_2  installed '')')
       call prmater(nmed)
      end if

c PLace Fake_volume_3 , covering the inside of lamp_shade

      if(ifak_flg(3) .eq. 1)then

       nmed = nmed_fake
       npar = 5
       v_c_name = 'MUF3'

c**     zfake3 = mega_z12(1,1)
       zfake3 = zfake22

       if(fakvol3(1) .lt. 0.)then
c**       fakvol3(2) = mega_irad1(1)
        fakvol3(2) = zfake3*tan(shad_ang*cdtr)
        fakvol3(3) = fakvol3(2) + fak3_thick
        fakvol3(4) = mega_irad2(1)
        fakvol3(5) = fakvol3(4) + fak3_thick
       end if

       fakvol3(1)=(mega_z12(2,1)-zfake3)/2.

       call gsvolu(v_c_name,'CONE',nmed,fakvol3,npar,ivolu)

       zpos = zfake3 + fakvol3(1)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       call gsatt(v_c_name,'SEEN',1)

       ivol_stat = 1
       write(LOUT,'(/5x,''MUF3 placed at Z='',f10.4,'' cms'')')zfake3
       write(LOUT,'(2x,''<MUFAKV> : Fake_volume_3  installed '')')
       call prmater(nmed)
      end if

c PLace Fake_volume_4 , covering the piston surface

      if(ifak_flg(4) .eq. 1)then

       nmed = nmed_fake
       npar = 5
       v_c_name = 'MUF4'

       zfake4 = zfake22
         zfake42= mega_z12(2,1)-2.*fakvol5(1)
       if(fakvol4(1) .lt. 0.)then
        fakvol4(3) = zfake4*tan(pist_ang*cdtr)
        fakvol4(2) = fakvol4(3) - fak4_thick
        fakvol4(5) = zfake42*tan(pist_ang*cdtr)
        fakvol4(4) = fakvol4(5) - fak4_thick
       end if

       fakvol4(1)=(zfake42-zfake4)/2.

       call gsvolu(v_c_name,'CONE',nmed,fakvol4,npar,ivolu)

       zpos = zfake4 + fakvol4(1)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       call gsatt(v_c_name,'SEEN',1)

       ivol_stat = 1
       write(LOUT,'(/5x,''MUF4 placed at Z='',f10.4,'' cms'')')zfake4
       write(LOUT,'(2x,''<MUFAKV> : Fake_volume_4  installed '')')
       call prmater(nmed)
      end if

c PLace Fake_volume_5 , in front of the Muon identifier

      if(ifak_flg(5) .eq. 1)then
       nmed = nmed_fake
       npar = 5
       v_c_name = 'MUF5'
       if(fakvol5(1) .lt. 0.)then
         fakvol5(1)=abs(fakvol5(1))
         zfake52 = mega_z12(2,1)
         zfake5 = zfake52-2.*fakvol5(1)
         fakvol5(2) = zfake5*tan(pist_ang*cdtr)
         fakvol5(3) = zfake5*tan(shad_ang*cdtr)
         fakvol5(4) = zfake52*tan(pist_ang*cdtr)
         fakvol5(5) = zfake52*tan(shad_ang*cdtr)
       end if

       call gsvolu(v_c_name,'CONE',nmed,fakvol5,npar,ivolu)

       zpos = zfake5 + fakvol5(1)
       call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       call gsatt(v_c_name,'SEEN',1)

       if(num_fakv .eq. 2)then
        v_c_name = 'FUM5'
        tmp = fakvol5(2)
        fakvol5(2) = fakvol5(4)
        fakvol5(4) = tmp
        tmp = fakvol5(3)
        fakvol5(3) = fakvol5(5)
        fakvol5(5) = tmp
        call gsvolu(v_c_name,'CONE',nmed,fakvol5,npar,ivolu)
        call gspos(v_c_name,1,v_m_name,0.0,0.0,-zpos,1,'ONLY')
        call gsatt(v_c_name,'SEEN',1)
       end if

       ivol_stat = 1
       write(LOUT,'(/5x,''MUF5 placed at Z='',f10.4,'' cms'')')zfake5
       write(LOUT,'(2x,''<MUFAKV> : Fake_volume_5  installed '')')
       call prmater(nmed)
      end if

c Put fake_vol_6 ; inside the beam tube to count primaries


      if(ifak_flg(6) .eq. 1) then
        nmed = nmed_fake
        npar = 3
        v_c_name = 'MUF6'
        v_m_name = 'PVAC'

        call gsvolu(v_c_name,'TUBE',nmed,fakvol6,npar,ivolu)
        
        call gspos(v_c_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')
        call gsatt(v_c_name,'SEEN',1)
        
        ivol_stat = 1
        write(LOUT,*) 'mufakv - Fake_volume_6  installed'
        call prmater(nmed)
      endif

CTON       if(ivol_stat .eq. 1)write(lunplog,nml=fakv_par)


c Define sensitive detector parameters

      do ia = 1, num_fakv
       do j = 1, 6
        if(ia .eq. 1 .or. (ia .eq. 2 .and. j .eq. 5))then
         if(ifak_flg(j) .eq. 1)then
          if(ia .eq. 1)then
           write(cdetnm, '(a3, i1)' )'MUF',j
           write(cvolnm(1),'(a3, i1)' )'MUF',j
          else if(ia .eq. 2)then
           write(cdetnm, '(a3, i1)' )'FUM',j
           write(cvolnm(1),'(a3, i1)' )'FUM',j
          end if

          call gsdet('MUM ',cdetnm,nv,cvolnm,nbitsv,idtype,nwpa,nwsa,
     +             iset,idet)

         if(j .eq. 1 .or. j .eq. 2 .or. j .ge. 5)then
           call gsdeth('MUM ',cdetnm,nh,namesh,nbitsh,origmu,factmu)
         else
           call gsdeth('MUM ',cdetnm,nh,namesh,nbitsh,orig,fact)
         end if
         end if
        end if
       end do
      end do

      return

  999 continue
      stop 'mufakv - PISA stop ... PHNX.PAR file error.'
      end
