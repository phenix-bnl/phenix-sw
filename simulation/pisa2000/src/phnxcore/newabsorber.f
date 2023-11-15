*-- Author :    Ralf Seidl 2006/12/19

      subroutine newabsorber
c
c    *************************************************************
c    *                                                           *
c    *  ABSORBER (vsn 1.10) routine for ABSORBER geometry        *
c    *                                                           *
c    *  Called by ==> ::  <GUGEOM>                               *
c    *  IN   ::  none                                            *
c    *  OUT  ::  none                                            *
c    *                                                           *
c    *  written  by ::  Ralf Seidl                               *
c    *                  some output to LOUT from GCUNIT          *
c    *                                                           *
c    *************************************************************
c
c
c THNCMIN = 12 deg    ---> minimal polar angle for absober
c THNCMAX = 37 deg    ---> maximal polar angle
c
c RNCMN1 = (zncone1-zncref-thliner/2)*tan(THNCMIN)  --> rmin at Z= ZNCONE1
c RNCMX1 = (zncone1-zncref-thliner/2)*tan(THNCMAX)  --> rmax at Z= ZNCONE1
c RNCMN2 = (zncone1-zncref + thliner/2)*tan(THNCMIN)  --> rmax at Z= ZNCONE1+thliner
c RNCMX2 = (zncone1-zncref + thliner/2)*tan(THNCMAX)  --> rmax at Z= ZNCONE1
c LINERFLG ... Flag to magnetize the absorber if iron
c          = 0  ; unmagnetized
c          = 1  ; magnetized
c THLINER  ... thickness of absorber
c
c    Jan 2010 HvH: eliminate zero-thickness volume on North side.
c    Oct 2010 RCS: adjust geometry to account for installed absorber

c -----------------
CTON+CDE,PISALUN.
*KEEP,GUPHNX.
      include 'guphnx.inc'
*KEEP,GCUNIT.
c      include 'gcunit.inc'
*KEEP,GUGEOM.
      include 'gugeom.inc'
*KEND.
c    *                                                           *
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

      integer*4 nmat_nc,color_ncone,linerflg
      integer*4 num_nc
      integer maxseg, maxseg3
      parameter (maxseg = 1 )
      integer nseg_nc(2)
      real*4 zncone(2)
      real*4 thliner(2)
      real*4 conepar(7)
      real*4 rncmn(maxseg,2),rncmx(maxseg,2)
      real*4 thncmin(2),thncmax(2)
      real*4 segmin(2),segmax(2)

      real*4 zncone2(2) /120.,-120./ ! z position of inner piece, fixed 
      real*4 thliner2 /5./ ! inner piece thickness, fixed 
      real*4 innerr /33./ ! inner piece inner r, fixed 
      integer ia,is


      data zncone/138.9,-138.9/
      data thncmax/37.0,37.0/
      data thncmin/10.0,12.0/,nmat_nc/14/      ! cfm fix for IBM
      data color_ncone/3/, num_nc/2/
      data linerflg/0/, thliner/10.0,35.0/
      data segmin/0.,0./,segmax/360.,360./

      namelist /abs_par/
     +    num_nc,nmat_nc,zncone,thncmin,
     +    thncmax,thetlsh,linerflg,
     +    zncref,thliner,segmin,segmax,color_ncone


      integer itmat,itmed,isvol,ifield,nwbuf
      real fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf(10)

c -------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c
c Put Nose Cone in front of the magnet yoke
c

      v_m_name = 'HALL'

      write( *,* ) 'newabsorber - reading parameter from phnx.par'
      rewind(itf_lun)
      read( itf_lun, nml = abs_par, err = 999 )

      write(LOUT,'(2x,''<ABS> : geometry file read'')')

c Nose code material definition

      nmed = 2000
      itmat = nmat_nc

      isvol  = 0     ! non_sensitive
      ifield = 0     ! non magnetc
      fldmax = 20.0  ! max field
      thmaxf = 1.0   ! maximum angle due to field (one step) in degrees
      dmaxms = 0.1   ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1   ! max fractional energy loss in one step
      epsil  = 0.001 ! tracking precision (cm)
      stmin  = 0.02  ! min step due to e loss or mulsct. (cm)
      ubuf(1)= 0.    ! tracking stop switch

      call gstmed(nmed,'MuonAbsorber',itmat,isvol,ifield,
     +    fldmax,thmaxf,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)


      write(LOUT,'(5x,''ABS thline(1)  = '',f10.4)')thliner(1)
      write(LOUT,'(5x,''ABS thline(2)  = '',f10.4)')thliner(2)
      write(LOUT,'(5x,''ABS thncmin(1)  = '',f10.4)')thncmin(1)
      write(LOUT,'(5x,''ABS thncmin(2)  = '',f10.4)')thncmin(2)
      write(LOUT,'(5x,''ABS thncmax(1)  = '',f10.4)')thncmax(1)
      write(LOUT,'(5x,''ABS thncmax(2)  = '',f10.4)')thncmax(2)
      write(LOUT,'(5x,''ABS zncone  = '',f10.4)')zncone
      write(LOUT,'(5x,''ABS zncref  = '',f10.4)')zncref

      do ia = 1,num_nc     ! builds north, south, inner and outer

          ! make sure absorber thickness is valid
          if( thliner(ia) .gt. 5 ) then
                 conepar(6)=segmin(ia)
                 conepar(7)=segmax(ia)


              if( ia.eq.1 ) then
                 thliner(ia) = thliner(ia) - 5
                  write( *,* ) 'newabsorber - installing north arm'
                  v_c_name = 'ABSN'
                  conepar(1) = thliner(ia)/2
                  conepar(2) =
     +                (zncone(ia)-zncref)*tan(thncmin(ia)
     +                *3.1415/180)

                  conepar(3) =
     +                (zncone(ia)-zncref)*tan(thncmax(ia)
     +                *3.1415/180)

                  conepar(4) =
     +                (zncone(ia)-zncref+thliner(ia))*tan(thncmin(ia)
     +                * 3.1415/180)

                  conepar(5) =
     +                (zncone(ia)-zncref+thliner(ia))*tan(thncmax(ia)
     +                * 3.1415/180)

                  zpos = zncone(ia)+thliner(ia)/2

              elseif( ia.eq.2 ) then
                 thliner(ia) = thliner(ia) - 5
                  write( *,* ) 'newabsorber - installing south arm'
                  v_c_name = 'ABSS'
                  conepar(1) = thliner(ia)/2

                  conepar(4) =
     +                abs((zncone(ia)-zncref)*tan(thncmin(ia)
     +                *3.1415/180)) +0.1 ! +-0.1 to fix small overlaps 

                  conepar(5) =
     +                abs((zncone(ia)-zncref)*tan(thncmax(ia)
     +                *3.1415/180)) -0.1

                  conepar(2) =
     +                abs((zncone(ia)-zncref-thliner(ia))
     +                * tan(thncmin(ia)
     +                * 3.1415/180)) +0.1

                  conepar(3) =
     +                abs((zncone(ia)-zncref-thliner(ia))
     +                *tan(thncmax(ia)
     +                * 3.1415/180)) -0.1

                  zpos = zncone(ia)-thliner(ia)/2
               endif
              npar = 7

              call gsvolu(v_c_name,'CONS',nmed,conepar,npar,ivolu)
c              call gsvolu(v_c_name,'CONS',942,conepar,npar,ivolu)
              call gsatt(v_c_name,'SEEN',1)
              call gsatt(v_c_name,'COLO',color_ncone)
              call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,0,'ONLY')


              if( ia.eq.1 ) then

                  write( *,* ) 'newabsorber - installing inner north arm'
                  v_c_name = 'ABIN'
                  conepar(1) = thliner2/2
                  conepar(2) = innerr

                  conepar(3) =
     +                (zncone2(ia)-zncref)*tan(thncmax(ia)
     +                *3.1415/180)

                  conepar(4) = innerr

                  conepar(5) =
     +                (zncone2(ia)-zncref+thliner2)*tan(thncmax(ia)
     +                * 3.1415/180)

                  zpos = zncone2(ia)+thliner2/2

              elseif( ia.eq.2 ) then
                
                  write( *,* ) 'newabsorber - installing inner south arm'
                  v_c_name = 'ABIS'
                  conepar(1) = thliner2/2

                  conepar(4) = innerr

                  conepar(5) =
     +                abs((zncone2(ia)-zncref)*tan(thncmax(ia)
     +                *3.1415/180)) -0.1

                  conepar(2) = innerr

                  conepar(3) =
     +                abs((zncone2(ia)-zncref-thliner2)
     +                *tan(thncmax(ia)
     +                * 3.1415/180)) -0.1

                  zpos = zncone2(ia)-thliner2/2

              endif



              write(LOUT,'(5x,''ABS Cone length  = '',f10.4)')conepar(1)
              write(LOUT,'(5x,''ABS Cone r1min  = '',f10.4)')conepar(2)
              write(LOUT,'(5x,''ABS Cone r1max  = '',f10.4)')conepar(3)
              write(LOUT,'(5x,''ABS Cone r2min  = '',f10.4)')conepar(4)
              write(LOUT,'(5x,''ABS Cone r2max  = '',f10.4)')conepar(5)

              npar = 7

              call gsvolu(v_c_name,'CONS',nmed,conepar,npar,ivolu)
c              call gsvolu(v_c_name,'CONS',942,conepar,npar,ivolu)
              call gsatt(v_c_name,'SEEN',1)
              call gsatt(v_c_name,'COLO',color_ncone)
              call gspos(v_c_name,1,v_m_name,0.0,0.0,zpos,0,'ONLY')

          endif

      enddo


      write(LOUT,
     1    '(/5x,''ABS_cone(s) placed at Z='',2f10.4,'' cms'')')
     2    zncone(1),zncone(2)

      write(LOUT,'(5x,''Abs-Cone ref_angle = '',f10.4)')thncmax
      write(LOUT,'(2x,''<ABS> : Absorber Cone(s) installed '')')

      return

  997 continue
      write(6,'(/3x,''Unable to open phnx.par file'')')
      stop ' Cannot find main geometry file'

999   write(6,*) 'newabsorber - Read error in abs_par segment of phnx.par.',
     +    ' The PHNX.PAR file will be re-read to pinpoint the erroneous',
     +    ' line'
      rewind( itf_lun )
      read( itf_lun, nml = abs_par )
      stop 'newabsorber - PISA stop ... PHNX.PAR file error.'
      end
