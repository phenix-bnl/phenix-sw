      subroutine MAGNET

      implicit none

#include "g77trigdef.inc"


c     C.F. Maguire 21-April-1992

c Central and Muon Magnet Yokes (after FAX from Tom Shea, 5/13/92)
c Revision History
c    Revised for Release 2, September 30, 1992
C    SRTonse 26th July 1993: Received the new geometry from S.Saini
C         and put in his code here for the updated muon/piston parts.

C    JPSullivan    Oct 5, 1993 -- put in GCUNIT and use LOUT for output.
c    M. Leitch     Nov 10, 1994   corrections for Muon Arm
c    C.F. Maguire  Feb 27, 1996   revision of central arm steel, yokes
c                                 remove muon cutout taper (not in real magnet)
c                                 refine central yoke intallation based on new
c                                 drawings from Pete Kroon (not RCDs).  There
c                                 is a slight overlap of the DYOKE volume with
c                                 the central magnet volumes (e.g. MAGD, AMGD)
c                  Feb 28, 1996   changed two central coils from polycone to
c                                 simple tube geometry; will have magnetic
c                                 air as default medium for non-installed
c                                 inner coil

c     D. Lee and   Aug 22, 1996   Changes to Muon Arm magnet: teacup, water
c     M. Brooks                   hoses, fins, and move mag_par namelist
c                                 specification to the fpmlink.inc file.
c                                 CFM: We need to split out the central magnet
c                                 code from the muon endcap magnet code and
c                                 not have central magnet parameters (which 
c                                 are now frozen) in the fpmlink.inc file

c     M. Brooks    Sep 10, 1996   Change "polarity" of Z positions for South
c                                 Muon Arm container volume in order to cure
c                                 GEANT tracking problem which caused it to
c                                 ignore volume and produce no hits

c     A. Rose    Sep 12, 31 1996 Added retaining ring, distributed outer 
c                                 magnet coils

c     C.F. Maguire Nov 16, 1996   Corrected placement of outer coil and retain-
c                                 ing ring on the South side
c     HvH Oct 2008: correct placement of outer coil, N and S. Mover outer coil retaining 
c                   ring. Inner magnet coil now made of copper (in phnx.par)
c     HvH Jan 2010: Corrected flowerpot dimensions and placement. This solves an
c                   overlap with ABSS.
c                   Fix large ovelap problems in CM yoke. Remove cyoke_flag.
c                   Fix extrusion of AGEM out of MUA2. 
c                   Fix overlap between BUSN and MUA1
c     HvH Feb 2010: Change MUA1,2 from PGON to PCON to include MFEE (used to protrude)
c     HvH Jan 2012: Fix small ovelap FINS-TSIP 

#include "gcunit.inc"
#include "guphnx.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpmlink.inc"

c    CFM: The central magnet requires actually 16 planes of Z, but
c         this translates into 51 parameters.  This is one more than the maximum
c         which GEANT allows !!
c         So we stop the central magnet at Z = 165 cm (end of 37 deg slope) and
c         add on an "appendix" polycone volume which begins at Z = 165 cm and
c         which corresponds to the 39 degree back slope.

c     main geometry segment

c     central magnet

c     endcap magnet     (after Felix Obenshain FAX of September 30)

      integer iflag,jflag,ip

c**>> 18-APR-1993 / S. Saini

      real cone_par(5)/0.0, 0.0, 0.0, 0.0, 0.0/
      real cone_par2
      real tube_par(3)/0.0, 0.0, 0.0/
      real pgon_par(22)/22*0./
      real pcon2_par(12)/12*0./
      real trap_par(11)
      real buss_par(3)
      real rib_par(11)

      real fin_r,shad_two

      real wtr_r,wtr_x,wtr_y,wtr_phi
c**>> added 18-APR-1993 / S. Saini
c     central core revised Feb. 28, 1996  C. Maguire

       real arm_off(2),pisang
       integer npc,kc, iaa
c<< 

c    added 28-oct-1993 C.F. Maguire for central arm return yoke
c    Jan'10 HvH: added e,f,g for replacement of c
      real*4 cyoke_par(3)        ! horizontal yoke pieces along Z
      real*4 dyoke_par(3)        ! vertical yoke pieces
      real*4 eyoke_par(3)        ! box part of vertical yoke piece Jan'10 HvH
      real*4 fyoke_par(4)        ! trapez. part of vert yoke piece  ''
      real*4 gyoke_par(4)        ! trapez. part of vert yoke piece  ''
      real rmax_1
      real*4 pist_ang,plug_ang,shad_ang,shneuz2,pbcurz2,znose1,
     +   zncref,thncref
      common /ugeom_muon2/ pist_ang,plug_ang,shad_ang,shneuz2,
     +  pbcurz2,znose1,zncref,thncref

      real*4 pai, cdtr, crtd     ! pi, degree-to-radians and vv

c added 10-oct-94, MJL, for central arm coils
c revised 28-Feb-96, CFM to user simpler tube shape
c added Sept-12-96 for retaining ring around coil

      integer retnpar /12/,retrot    !#parameters, rotation
      real zcoil                     !position of outer coils

c**>> added 18-APR-1993 / S. Saini
       common/uconst/pai,cdtr,crtd
c<<
      character*4 v_m_name,v_i_name,v_d_name,v_c_name
      character*4 v_l_name, v_r_name, v_t_name

      integer  npar,nmed,ivolu,izpl,k, zposneg, lpzi, icoil
      integer coilloop

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun



      entry magn_cent

c     Read the geometery file segment

      write( *,* ) 'magnet::magn_cent - ',
     +             'reading mag_par from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = mag_par, err = 999 )

c     do some checking  (should add medium checking?)

      if(npl_cent.gt.ncent_max)then
         write (6,992)npl_cent,ncent_max
992   format(/,2x,'MAG <E>: The requested number of z-planes ',i4,2x,
     1 'exceeds the maximum ',i4,2x,'set',/,11x,'in the MAG.FOR file.',/,11x,
     2 'Check the NPL_CENT value in the PHNX.PAR file, or',/,11x,
     3 'change the NCENT_MAX number in the MAG.FOR subroutine.',/)
         stop ' PISA program stopped because error detected in MAG.FOR'
      endif
      if(npl_cent.lt.2)then
         write (6,993)npl_cent
993   format(/,2x,'MAG <E>: The requested number of z-planes ',i4,2x,
     1   'is less than the minimum (=2)',/,11x,
     2   'required for a PCON in GEANT.',/,11x,
     3   'Check the NPL_CENT value in the PHNX.PAR file.'/)
         stop ' PISA program stop because error detected in MAG.FOR'
      endif
      do izpl=1,npl_cent-1
         if(z_cent(izpl+1).le.z_cent(izpl))then
         write (6,994)izpl,z_cent(izpl),izpl+1,z_cent(izpl+1)
994   format(/,2x,'MAG <E>: The zplane #',i3,' value ',g10.3,2x,
     1   'is greater than or equal to',/,11x,
     2   'the zplane #',i3,' value ',g10.3,/,11x,
     3   'Check the PHNX.PAR file.'/)
         stop ' PISA program stopped because error detected in MAG.FOR'
         endif
      enddo
      do izpl=1,npl_cent
         if(rmax_cent(izpl).lt.rmin_cent(izpl))then
         write (6,995)izpl,rmax_cent(izpl),izpl,rmin_cent(izpl)
995   format(/,2x,'MAG <E>: The rmax #',i3,' value ',g10.3,2x,
     1   'is less than',/,11x,'the rmin #',i3,' value ',g10.3/,11x,
     2   'Check the PHNX.PAR file.'/)
         stop ' PISA program stopped because error detected in MAG.FOR'
         endif
      enddo

c Now put in geometry into proper mother volume

      v_m_name = 'HALL'    ! use HALL as immediate mother
      nmed = nmed_cent           ! should be iron

c  Hardwired fix because GEANT cannot handle NPAR = 51
c      npar = 3*npl_cent+3     ! parameters for polycone PCON

c  Code now assumes that NPL_CENT = 16  (The magnet is already built in 2/96 !)

      npar = 45  ! first 14 zplanes up to Z = 165.0 cm

c     now fill up the invariant polycone parameter array members

      pcon_par(1) = 0.00      ! lower phi limit
      pcon_par(2) = 360.0     ! upper phi limit
      pcon_par(3) = 14        ! number of z-planes

c Now fill up the polycone parameter array for the positive end

      v_i_name = 'MAGD'       ! downstream (+z) half
      do izpl=1,14            ! normal order (using only 14 planes)
        pcon_par(3*izpl+1) = z_cent(izpl) ! z-plane position (positive)
        pcon_par(3*izpl+2) = rmin_cent(izpl) ! rmin at this z
        pcon_par(3*izpl+3) = rmax_cent(izpl) ! rmax at this z
      enddo
      call gsvolu(v_i_name,'PCON',nmed,pcon_par,npar,ivolu)
      if(color_cent.gt.0)then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_cent)
      else

c     invisible

         CALL GSATT(v_i_name,'SEEN',0)
      endif

c     normal position will be at (0.0, 0.0, 0.0), where else?

      call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')

c     Now add in the appendix triangle shape from Z = 165 to Z = 242 cm

      npar = 12  ! last 3 z planes from Z = 165.0 to 242.0 cm

c     now fill up the invariant polycone parameter array members

      pcon_par(1) = 0.00      ! lower phi limit
      pcon_par(2) = 360.0     ! upper phi limit
      pcon_par(3) = 3         ! number of z-planes

c Now fill up the polycone parameter array for the positive end

      v_i_name = 'AMGD'       ! downstream (+z) half
      do izpl=1,3             ! normal order (using last planes 14 to 16)
        pcon_par(3*izpl+1) = z_cent(izpl+13) ! z-plane position (positive)
        pcon_par(3*izpl+2) = rmin_cent(izpl+13) ! rmin at this z
        pcon_par(3*izpl+3) = rmax_cent(izpl+13) ! rmax at this z
      enddo
      call gsvolu(v_i_name,'PCON',nmed,pcon_par,npar,ivolu)
      if(color_cent.gt.0)then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_cent)
      else

c     invisible

         CALL GSATT(v_i_name,'SEEN',0)
      endif

c     normal position will be at (0.0, 0.0, 0.0), where else?

      call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')

c install inner and outer copper coils into central magnet poles
c loop over two coils, inner and outer
c  Revised: 2-28-96, C.F. Maguire, change to tube shape from polycone shape
c                                  for GEANT, 3 parameters instead of 12

c     modified: Sept 21, 1996      made outer coil into series of coils,
c                                  removed loop over inner/outer colis 
c                                  (aar)

          v_d_name = 'M1CO'
        nmed = cencoil_med(1)
        tube_par(1) = cencoil_rin(1)
        tube_par(2) = cencoil_rout(1)
        tube_par(3) = 0.5*(cencoil_z(2,1) - cencoil_z(1,1))

c create the volumes

        call gsvolu(v_d_name,'TUBE',nmed,tube_par,3,ivolu)
        if(cencoil_color(1).gt.0)then
          CALL GSATT(v_d_name,'SEEN',1)
          CALL GSATT(v_d_name,'COLO',cencoil_color(1))
        else

c     invisible

          CALL GSATT(v_d_name,'SEEN',0)
        endif
c locate the volumes inside the yoke volume
c positve Z volume
        v_i_name = 'MAGD'
        call gspos(v_d_name,1,v_i_name,0.0,0.0,
     1    0.5*(cencoil_z(1,1)+cencoil_z(2,1)),1,'ONLY')


c     Now do outer coil

          v_d_name = 'M2CO'
        nmed = cencoil_med(2)
        tube_par(1) = cencoil_rin(2)
        tube_par(2) = cencoil_rout(2)
        tube_par(3) = 0.5*(cencoil_z(2,2) - cencoil_z(1,2))/6.

c create the volumes

        call gsvolu(v_d_name,'TUBE',nmed,tube_par,3,ivolu)
        if(cencoil_color(2).gt.0)then
          CALL GSATT(v_d_name,'SEEN',1)
          CALL GSATT(v_d_name,'COLO',cencoil_color(2))
        else

c     invisible

          CALL GSATT(v_d_name,'SEEN',0)
        endif
c locate the volumes inside the yoke volume
c positve Z volume
        v_i_name = 'MAGD'
        do coilloop=1,6
           zcoil=0.5*(cencoil_z(1,2)+cencoil_z(2,2))+
     1            (coilloop-3)*tube_par(3)*2. -2.4        ! 2.4 used to be 0.1 Oct 08 HvH
           call gspos(v_d_name,coilloop,v_i_name,0.0,0.0,
     1        zcoil,1,'ONLY')
        enddo


c     finished making outer coil


c Now fill up the polycone parameter array for the negative end

      nmed = nmed_cent        ! should be iron
      v_i_name = 'MAGU'       ! upstream (-z) half

c  Hardwired fix because GEANT cannot handle NPAR = 51
c      npar = 3*npl_cent+3     ! parameters for polycone PCON

c  Code now assumes that NPL_CENT = 16  (The magnet is already built in 2/96 !)

      npar = 45  ! first 14 z planes up to Z = 165.0 cm

c     now fill up the invariant polycone parameter array members

      pcon_par(1) = 0.00      ! lower phi limit
      pcon_par(2) = 360.0     ! upper phi limit
      pcon_par(3) = 14        ! number of z-planes
      lpzi = 1          ! reverse index
      do izpl=14,1,-1         ! reverse order
        pcon_par(3*lpzi+1) = -z_cent(izpl)   ! z-plane position (negative)
        pcon_par(3*lpzi+2) = rmin_cent(izpl) ! rmin at this z
        pcon_par(3*lpzi+3) = rmax_cent(izpl) ! rmax at this z
        lpzi = lpzi + 1
      enddo
      call gsvolu(v_i_name,'PCON',nmed,pcon_par,npar,ivolu)
      if(color_cent.gt.0)then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_cent)
      else

c Invisible

         CALL GSATT(v_i_name,'SEEN',0)
      endif

c     normal position will be at (0.0, 0.0, 0.0), where else?

      call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')

c     Now add in the appendix triangle shape from Z = 165 to Z = 242 cm

      npar = 12  ! last 3 zplanes from Z = 165.0 to 242.0 cm


c     now fill up the invariant polycone parameter array members

      pcon_par(1) = 0.00      ! lower phi limit
      pcon_par(2) = 360.0     ! upper phi limit
      pcon_par(3) = 3         ! number of z-planes

c Now fill up the polycone parameter array for the positive end

      v_i_name = 'AMGU'       ! upstream (-z) half
      lpzi = 1                ! reverse index
      do izpl=16,14,-1        ! reverse order
        pcon_par(3*lpzi+1) = -z_cent(izpl)   ! z-plane position (negative)
        pcon_par(3*lpzi+2) = rmin_cent(izpl) ! rmin at this z
        pcon_par(3*lpzi+3) = rmax_cent(izpl) ! rmax at this z
        lpzi = lpzi + 1
      enddo
      call gsvolu(v_i_name,'PCON',nmed,pcon_par,npar,ivolu)
      if(color_cent.gt.0)then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_cent)
      else

c     invisible

         CALL GSATT(v_i_name,'SEEN',0)
      endif

c     normal position will be at (0.0, 0.0, 0.0), where else?

      call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')


c     Revised insertion for central arm return yoke (C.F. Maguire, 2/27/96)
c     This version puts the yoke volumes entirely within the HALL
c     The default yoke parameters are based on the Nov. 1994 RCD 002D and 003B
c     NOTE: The base support of the Magnet Structure is still not installed

c     Upper return yoke volumes

      npar = 3
      nmed = nmed_cent

c    CYOKE is the "horizontal" cross piece from Z = -242 to +242 cm,
c          159.85 full width along X, and 100 cm full height along Y

      cyoke_par(1) = cyoke_dx
      cyoke_par(2) = cyoke_dy
      cyoke_par(3) = cyoke_dz
      v_i_name = 'CRTU'            ! upper part of return yoke
      call gsvolu(v_i_name,'BOX ',nmed,cyoke_par,npar,ivolu)
      call gspos (v_i_name,1,'HALL',0.0,cyoke_yp,0.0,1,'ONLY')

c    DYOKE is the "vertical" piece on the North and South ends; it goes from
c          Z = 135 to 242 cm (North Side), and from 217 to 295 cm vertical;
c          The X full width is the same as the CYOKE.
c          This model is slightly inaccurate since there is a base X width
c          of 180.0 full width extending from Y = 217.0 to 234.0 cm, and
c          the DYOKE volume overlaps with the central steel shape.
c          (but see notes below for a fix - Jan'10 HvH)

      dyoke_par(1) = dyoke_dx
      dyoke_par(2) = dyoke_dy
      dyoke_par(3) = dyoke_dz

c     Jan 2010 HvH: replaced the blocks CRRU, CRRD, CRLU, CRLD, which caused 
c     18-cm volume overlaps with CM 'disks' AMGU and AMGD, with the a 
c     non-overlapping approximation consisting of a box (CRR1) plus three 
c     trapeziods (one CRR2 and 2 CRR3's). All dimensions are derived from
c     the former box dyoke_dx,y,z.
                                        !
      dyoke_arc = 13.5                  ! ~ former overlap
      eyoke_par(1) = dyoke_dx/3.        ! first the central box CRR1,
      eyoke_par(2) = dyoke_dy - dyoke_arc  ! one each N,S, top, bottom
      eyoke_par(3) = dyoke_dz
      call gsvolu('CRR1','BOX ',nmed,eyoke_par,npar,ivolu)
      call gspos('CRR1',1,'HALL',0.0, dyoke_yp, dyoke_zp,1,'ONLY')
      call gspos('CRR1',2,'HALL',0.0, dyoke_yp,-dyoke_zp,1,'ONLY')
      call gspos('CRR1',3,'HALL',0.0,-dyoke_yp, dyoke_zp,1,'ONLY')
      call gspos('CRR1',4,'HALL',0.0,-dyoke_yp,-dyoke_zp,1,'ONLY')
      call gsatt('CRR1','COLO',1)
   
      fyoke_par(1) = dyoke_dx          ! Then the upper (lower) trapeziod CRR2
      fyoke_par(2) = dyoke_dx/3.0      ! One each N,S, top, bottom
      fyoke_par(3) = dyoke_dz          ! 
      fyoke_par(4) = dyoke_arc/2.
      call gsvolu('CRR2','TRD1',nmed,fyoke_par,4,ivolu)
      irot = irot+1                    ! for top blocks:
      call gsrotm(irot,90.,0.,0.,0.,90.,270.)
      call gspos ('CRR2',1,'HALL',0.0,
     &             dyoke_yp+dyoke_dy-dyoke_arc/2.0, dyoke_zp,irot,'ONLY')
      call gspos ('CRR2',2,'HALL',0.0,
     &             dyoke_yp+dyoke_dy-dyoke_arc/2.0,-dyoke_zp,irot,'ONLY')
      irot = irot+1                    ! for bottom blocks
      call gsrotm(irot,90.0, 0.0, 180.0, 0.0, 90.0, 90.0)
      call gspos ('CRR2',3,'HALL',0.0,
     &            -dyoke_yp-dyoke_dy+dyoke_arc/2.0, dyoke_zp,irot,'ONLY')
      call gspos ('CRR2',4,'HALL',0.0,
     &            -dyoke_yp-dyoke_dy+dyoke_arc/2.0,-dyoke_zp,irot,'ONLY')
      call gsatt('CRR2','COLO',1)

      gyoke_par(1) = dyoke_dy         ! Then the horiz. trapezoids
      gyoke_par(2) = dyoke_dy -dyoke_arc ! E,W of the central box CRR1
      gyoke_par(3) = dyoke_dz         ! One pair each N,S, top, bottom
      gyoke_par(4) = dyoke_dx/3.0
      call gsvolu('CRR3','TRD1',nmed,gyoke_par,4,ivolu)
      irot = irot+1                   ! for west:
      call gsrotm(irot,90.,90.,0.,0.,90.,0.)
      call gspos ('CRR3',1,'HALL', -2.0*dyoke_dx/3.0,
     &             dyoke_yp, dyoke_zp,irot,'ONLY')
      call gspos ('CRR3',2,'HALL', -2.0*dyoke_dx/3.0,
     &             dyoke_yp,-dyoke_zp,irot,'ONLY')
      call gspos ('CRR3',3,'HALL', -2.0*dyoke_dx/3.0,
     &            -dyoke_yp, dyoke_zp,irot,'ONLY')
      call gspos ('CRR3',4,'HALL', -2.0*dyoke_dx/3.0,
     &            -dyoke_yp,-dyoke_zp,irot,'ONLY')
      irot = irot+1                   ! for east
      call gsrotm(irot,90.,270.,0.,0.,90.,180.)
      call gspos ('CRR3',5,'HALL', 2.0*dyoke_dx/3.0,
     &             dyoke_yp, dyoke_zp,irot,'ONLY')
      call gspos ('CRR3',6,'HALL', 2.0*dyoke_dx/3.0,
     &             dyoke_yp,-dyoke_zp,irot,'ONLY')
      call gspos ('CRR3',7,'HALL', 2.0*dyoke_dx/3.0,
     &            -dyoke_yp, dyoke_zp,irot,'ONLY')
      call gspos ('CRR3',8,'HALL', 2.0*dyoke_dx/3.0,
     &            -dyoke_yp,-dyoke_zp,irot,'ONLY')
      call gsatt('CRR3','COLO',1)

c     End of replacements Jan 2010 HvH

      write(LOUT,'(/,2x,a,/)')
     &' MAGNET <I>: Upper half of central arm return yoke in HALL'

c    Lower return yoke volume
      v_i_name = 'CRTD'               ! lower part of return yoke
      call gsvolu(v_i_name,'BOX ',nmed,cyoke_par,npar,ivolu)
      call gspos(v_i_name,1,'HALL',0.0,-cyoke_yp,0.0,1,'ONLY')

       write(LOUT,'(/,2x,a,/)')
     1' MAGNET <I>: Lower half of central arm return yoke in HALL'

      if(color_cent.gt.0)then
         CALL GSATT('CRTU','SEEN',1)
         CALL GSATT('CRTU','COLO',color_cent)
         CALL GSATT('CRTD','SEEN',1)
         CALL GSATT('CRTD','COLO',color_cent)
         CALL GSATT('CRR1','SEEN',1)
         CALL GSATT('CRR1','COLO',color_cent)
         CALL GSATT('CRR2','SEEN',1)
         CALL GSATT('CRR2','COLO',color_cent)
         CALL GSATT('CRR3','SEEN',1)
         CALL GSATT('CRR3','COLO',color_cent)
      else
         CALL GSATT('CRTU','SEEN',0)
         CALL GSATT('CRTD','SEEN',0)
         CALL GSATT('CRR1','SEEN',0)
         CALL GSATT('CRR2','SEEN',0)
         CALL GSATT('CRR3','SEEN',0)
      endif  ! check on color for the return yoke

c Added 14-MAY-1993 / S. Saini : To speedup tracking inside the central magnet
c Install a core magnetic_material with large tracking cutoff parameters.

c C.F. Maguire: revised Feb. 28, 1996 because of conflict with inner coil.
c               Central steel core has to be split into two parts, below and
c               above the inner coil which space is empty on Day 1.
c               Adopt a 5 cm skin boundary for the central core volumes.

c     Spet 21, 1996    split outer coil into six units. AAR

      if(iflag_ccore .eq. 1)then
       pcon_par(1) = 0.00      ! lower phi limit
       pcon_par(2) = 360.0     ! upper phi limit
       nmed = nmed_ccore
       do kc = 1,2
        if(kc .eq. 1)then

c    Downstream core

         v_i_name = 'MAGD'            ! downstream (+z) half

c    Lower half

         v_c_name = 'CCR1'
         npar = 3*npc_core(1)+3     ! parameters for polycone PCON
         pcon_par(3) = npc_core(1)  ! number of z-planes
         do izpl=1,npc_core(1)        ! normal order
          pcon_par(3*izpl+1) = zcent_core(izpl) !z-plane +ve
          pcon_par(3*izpl+2) = ccore_rmin(izpl) ! rmin at this z
          pcon_par(3*izpl+3) = ccore_rmax(izpl) ! rmax at this z
         end do
         call gsvolu(v_c_name,'PCON',nmed,pcon_par,npar,ivolu)
         call gsatt(v_c_name,'SEEN',1) 
         call gspos(v_c_name,1,v_i_name,0.0,0.0,0.0,1,'ONLY')

c    Upper half

         v_c_name = 'CCR2'
         npar = 3*npc_core(2)+3     ! parameters for polycone PCON
         pcon_par(3) = npc_core(2)  ! number of z-planes
         lpzi = 1
         do izpl=npc_core(1)+1,npc_core(1)+npc_core(2)   ! normal order
          pcon_par(3*lpzi+1) = zcent_core(izpl) !z-plane +ve
          pcon_par(3*lpzi+2) = ccore_rmin(izpl) ! rmin at this z
          pcon_par(3*lpzi+3) = ccore_rmax(izpl) ! rmax at this z
          lpzi = lpzi + 1
         end do
         call gsvolu(v_c_name,'PCON',nmed,pcon_par,npar,ivolu)
         call gsatt(v_c_name,'SEEN',1) 
         call gspos(v_c_name,1,v_i_name,0.0,0.0,0.0,1,'ONLY')
        else

c      Upstream core

         v_i_name = 'MAGU'       ! upstream (-z) half

c    Lower half

         v_c_name = 'ROC1'
         npar = 3*npc_core(1)+3     ! parameters for polycone PCON
         pcon_par(3) = npc_core(1)  ! number of z-planes
         lpzi = 1          ! reverse index
         do izpl=npc_core(1),1,-1
          pcon_par(3*lpzi+1) =-zcent_core(izpl) !z-plane -ve
          pcon_par(3*lpzi+2) = ccore_rmin(izpl) ! rmin at this z
          pcon_par(3*lpzi+3) = ccore_rmax(izpl) ! rmax at this z
          lpzi = lpzi + 1
         end do
         call gsvolu(v_c_name,'PCON',nmed,pcon_par,npar,ivolu)
         call gsatt(v_c_name,'SEEN',1) 
         call gspos(v_c_name,1,v_i_name,0.0,0.0,0.0,1,'ONLY')

c    Upper half

         v_c_name = 'ROC2'
         npar = 3*npc_core(2)+3     ! parameters for polycone PCON
         pcon_par(3) = npc_core(2)  ! number of z-planes
         lpzi = 1
         do izpl=npc_core(2)+npc_core(1),npc_core(1)+1,-1   ! normal order
          pcon_par(3*lpzi+1) = -zcent_core(izpl) !z-plane +ve
          pcon_par(3*lpzi+2) = ccore_rmin(izpl) ! rmin at this z
          pcon_par(3*lpzi+3) = ccore_rmax(izpl) ! rmax at this z
          lpzi = lpzi + 1
         end do
         call gsvolu(v_c_name,'PCON',nmed,pcon_par,npar,ivolu)
         call gsatt(v_c_name,'SEEN',1) 
         call gspos(v_c_name,1,v_i_name,0.0,0.0,0.0,1,'ONLY')
        end if 
       end do
      end if
c <<
 


c now place the negative Z copies of the central magnet coils

      v_i_name = 'MAGU'

          v_d_name='M1CO'
        call gspos(v_d_name,2,v_i_name,0.0,0.0,
     1    -0.5*(cencoil_z(1,1)+cencoil_z(2,1)),1,'ONLY')


c  Correction: 11/16/96 by CFM,  missing negative sign in Z position

        v_d_name = 'M2CO'
        do coilloop=1,6
           zcoil=-0.5*(cencoil_z(1,2)+cencoil_z(2,2))-
     1          (coilloop-3)*tube_par(3)*2.+ 2.4           ! 2.4 used to be 0.1 Oct 08 HvH
           call gspos(v_d_name,coilloop,v_i_name,0.0,0.0,
     1        zcoil,1,'ONLY')
        enddo

c     retaining ring....AAR

c---*----1----*----2----*----3----*----4----*----5----*----6----*----7--

        irot=irot+1
        retrot=irot

        call gsrotm(retrot,ret_ring_rot(1),ret_ring_rot(2),
     +         ret_ring_rot(3),ret_ring_rot(4),ret_ring_rot(5),
     +         ret_ring_rot(6))

        ret_ring_par( 5) = rmax_cent(2) ! avoid running into MAGD
        ret_ring_par( 8) = rmax_cent(2) ! oct 08 HvH
        ret_ring_par(11) = rmax_cent(2) !
        call gsvolu('RTRI','PCON',retmed,ret_ring_par,retnpar,
     +              ivolu)
        call gsatt('RTRI','SEEN',1)
        call gspos('RTRI',1,'HALL',ret_ring_pos(1),ret_ring_pos(2),
     +        ret_ring_pos(3),0,'ONLY')


c       ret_ring_pos(3)=-1*ret_ring_pos(3)   (removed by CFM)

c    Correction:  11/16/96 by CFM,  fix Z positioN

        call gspos('RTRI',2,'HALL',ret_ring_pos(1),ret_ring_pos(2),
     +       -ret_ring_pos(3),retrot,'ONLY')

        if (ivolu.le.0) then
           write(*,*)'<E>MAGNET.F---Retaining ring setup error'
        else
        endif


c  End of magnet geometry set up


      return
      entry magn_endc

c     Read the geometery file segment


      write( *,* ) 'magnet::magn_endc - ',
     +             'reading mag_par from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = mag_par, err = 999 )

      write( *,* ) 'magnet::magn_endc - ',
     +             'reading mum_par from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = mum_par, err = 999 )
 
c Added S. Saini / 18-APR-1993

      pist_ang = pist_thet
      shad_ang = shad_thet
      pai  = acos(-1.)
      cdtr = pai/180.0
      crtd = 1./cdtr

c    First create muon arm volume, a polygon outside of
c    lampshade down to just inside of the piston
c    DMLee Aug-96
c    Modified to include MPC volume, m.chiu Sep-05

      do zposneg=1,muo_arms       ! positive and negative z
        iaa = 2*mod(zposneg,2) - 1
        if(zposneg.eq.1)then
          v_i_name = 'MUA1'
        else
          v_i_name = 'MUA2'
        endif
        v_m_name = 'HALL'
        npar=16               ! PCON including MFEE
        nmed=ArmMedium        !should be magnetic air
        pgon_par(1) = -22.5
        pgon_par(2) = 360.
cxx     pgon_par(3) = 8.      ! from former PGON octant structure
        pgon_par(3) = 6       ! 6 transitions in Z
        npar = 22             ! 4 + 6*3
        if(zposneg.eq.1)then  ! north arm (2=south)
                              ! planes from S to N:
          pgon_par(4) = StationNominalZpos(1,zposneg) - 4.3 - 12.
          pgon_par(5) = 110.              ! go around the MFEE crate
          pgon_par(6) = pgon_par(5) +40.0

          pgon_par(7) = StationNominalZpos(1,zposneg) - 4.3
          pgon_par(8) = pgon_par(5)
          pgon_par(9) = StationOneFee(3) ! clear the MFEE support plate

          pgon_par(10) = StationNominalZpos(1,zposneg) - 4.3
          pgon_par(11) = 3.9              ! just bigger than the beam pipe
          pgon_par(12) = pgon_par(9)     ! clear the MFEE support plate

          pgon_par(13) = 244.             ! downstream of MPC
          pgon_par(14) = pgon_par(11)     ! = 3.9
          pgon_par(15) = pgon_par(12)+(pgon_par(13)-pgon_par(10))*
     +     tan((shad_thet+3)*cdtr)              + 0.08 ! xxx

          pgon_par(16) = pgon_par(13)     ! = 244.
          pgon_par(17) = 11.0*cosd(22.5) -.1
          pgon_par(18) = pgon_par(15)       + 0.08 ! HvH

          pgon_par(19) = mega_z12(2,zposneg)
          pgon_par(20) = pgon_par(17)
          pgon_par(21) = pgon_par(12)+(pgon_par(19)-pgon_par(10))*
     +     tan((shad_thet+3)*cdtr) +              0.08 ! HvH
          arm_off(1) = iaa*z_roll(zposneg)

        else                       ! south arm --------------------------
                                   ! planes from S to N:
          pgon_par(4) = -mega_z12(2,zposneg)   ! back plane
          pgon_par(5) = 11.0*cosd(22.5) - 0.1
          pgon_par(6) = StationOneFrame(2) + 0.27 +
     &          (-(StationNominalZpos(1,zposneg)-4.0) +
     &            mega_z12(2,zposneg)) * tan((shad_thet+4)*cdtr)

          pgon_par(7) = -244.             ! beam pipe jog
          pgon_par(8) = pgon_par(5)
          pgon_par(9) = StationOneFrame(2) + 0.27 +
     &          (-(StationNominalZpos(1,zposneg)-4.0) -
     &            pgon_par(7) ) * tan((shad_thet+9)*cdtr)

          pgon_par(10) = pgon_par(7)      ! = 244.
          pgon_par(11) = 3.9              ! just bigger than inner beampipe
          pgon_par(12) = pgon_par(9)
                                          ! MFEE clear
          pgon_par(13) = -StationNominalZpos(1,zposneg) + 4.3
          pgon_par(14) = 3.9              ! just bigger than the beam pipe
          pgon_par(15) = StationOneFee(3) ! clear the MFEE support plate

          pgon_par(16) = -StationNominalZpos(1,zposneg) + 4.3
          pgon_par(17) = 122.
          pgon_par(18) = pgon_par(15)     ! clear the MFEE support plate
                                          ! clear FEE crates
          pgon_par(19) = -StationNominalZpos(1,zposneg) + 4.3 + 12.
          pgon_par(20) = 122.             ! go around the MFEE crate
          pgon_par(21) = pgon_par(20) +30.0

          arm_off(2) = iaa*z_roll(zposneg)
        
        endif
        call gsvolu(v_i_name,'PCON',nmed,pgon_par,npar,ivolu)
  
        CALL GSATT(v_i_name,'SEEN',1)

        if(zposneg.eq.1)then
          call gspos(v_i_name,1,v_m_name,0.0,0.0,arm_off(zposneg),1,
     +               'ONLY')
        else
          call gspos(v_i_name,1,v_m_name,0.0,0.0,arm_off(zposneg),1,
     +               'ONLY')
        endif        

        if(zposneg.eq.1)then
          v_m_name = 'MUA1'
        else
          v_m_name = 'MUA2'
        endif
      
        nmed = pist_med   ! iron (mangetic)
        npar = 3*npl_end(zposneg)+3     ! parameters for polycone PCON

c     now fill up the invariant polycone parameter array members

        pcon_par(1) = 0.00      ! lower phi limit
        pcon_par(2) = 360.0     ! upper phi limit
        pcon_par(3) = npl_end(zposneg)     ! number of z-planes

c>>> Added  S. Saini/ 23-MAR-1993
c If rmax_end(1) is negative, calculate PCON parameters from Pist_thet.
 
        rmax_1 = rmax_end(1,zposneg)
 
        if(rmax_1 .lt. 0.0)then
          do k = 1, npl_end(zposneg)
            rmax_end(k,zposneg) = z_end(k,zposneg)*tan(pist_thet*cdtr)
            if(k .eq. 6)rmax_end(k,zposneg)
     1         = rmax_end(k,zposneg)-depth_notch
            if(k .eq. 9)rmax_end(k,zposneg) = rmax_end(k-1,zposneg)
          end do
          rmax_end(5,zposneg) = rmax_end(6,zposneg)
          rmax_end(11,zposneg) = rmax_end(10,zposneg)
        end if
 
        if(rmax_1 .lt. 0.0)then
          coil_orad1(zposneg)= coil_z12(1,zposneg)*tan(pist_thet*cdtr)
          coil_irad1(zposneg)= coil_orad1(zposneg) - coil_thick
          coil_orad2(zposneg)= coil_z12(3,zposneg)*tan(pist_thet*cdtr)
          coil_irad2(zposneg)= coil_orad2(zposneg) - coil_thick
        end if

c<<<<






c Now put in geometry into proper mother volume for the PISTON

        if(zposneg.eq.1)then
          v_i_name = 'PIST'

c Fill up the polycone parameter array for the positive end

 
          do izpl=1,npl_end(1)         ! normal order
            pcon_par(3*izpl+1) = z_end(izpl,1) ! z-plane position (positive)
            pcon_par(3*izpl+2) = rmin_end(izpl,1) ! rmin at this z
            pcon_par(3*izpl+3) = rmax_end(izpl,1) ! rmax at this z
          enddo
 
        else
 
          v_i_name = 'TSIP'
          lpzi = 1          ! reverse index
          do izpl=npl_end(2),1,-1         ! reverse order
            pcon_par(3*lpzi+1) = -z_end(izpl,2)   ! z-plane position (negative)
            pcon_par(3*lpzi+2) = rmin_end(izpl,2) ! rmin at this z
            pcon_par(3*lpzi+3) = rmax_end(izpl,2) ! rmax at this z
            lpzi = lpzi + 1
          enddo
 
        endif
 
        call gsvolu(v_i_name,'PCON',nmed,pcon_par,npar,ivolu)
 
        if(color_endc.gt.0)then
           CALL GSATT(v_i_name,'SEEN',1)
           CALL GSATT(v_i_name,'COLO',color_endc)
        else

c     invisible

           CALL GSATT(v_i_name,'SEEN',0)
        endif
 
        call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,
     +    'ONLY')
 
c>> Added 14-MAY-1993 / S. Saini : To speedup tracking inside the piston
c If iflag_pcore = 1 , then define piston_core

        if(iflag_pcore .eq. 1)then
          cone_par(1) = 0.5*(zpist_core(2,zposneg)
     1    - zpist_core(1,zposneg))
          if(pcore_rad(1,zposneg) .lt. 0.0)then
            pcore_rad(1,zposneg) = abs(pcore_rad(1,zposneg))
            pcore_rad(2,zposneg) = zpist_core(1,zposneg)
     1        *tan(pist_thet*cdtr)-pcdelr(1,zposneg)
            pcore_rad(4,zposneg) = zpist_core(2,zposneg)
     1        *tan(pist_thet*cdtr)-pcdelr(2,zposneg)
          end if
          if(zposneg.eq.1)then
            v_c_name = 'PCOR'
            do npc = 1,4
              cone_par(npc+1) = pcore_rad(npc,1)
            end do
          else
            v_c_name = 'ROCP'
            do npc = 1,2
              cone_par(npc+1) = pcore_rad(npc+2,2)
              cone_par(npc+3) = pcore_rad(npc,2)
            end do
          end if
          nmed = nmed_pcore
          npar = 5
          call gsvolu(v_c_name,'CONE',nmed,cone_par,npar,ivolu)
          call gsatt(v_c_name,'SEEN',1)
 
          if(zposneg.eq.1)then
 
            call gspos(v_c_name,1,v_i_name,0.0,0.0,zpist_core(1,1) +
     +      cone_par(1),1,'ONLY')
          else
           call gspos(v_c_name,1,v_i_name,0.0,0.0,-(zpist_core(1,2) +
     +     cone_par(1)),1,'ONLY')
          endif
          write(LOUT,'(/2x,''<GUGEOM>: Piston_insert installed'')')
        end if      !test on flag_pcore
c <<

C>>> Add fin supports
C>>>>>>>>>>>Added 1-Aug-96 /D.M. Lee

        if(fin_flag.eq.1)then
        
        if(zposneg.eq.1)then
          v_i_name = 'FINN'
          pisang = pist_ang
        else
          v_i_name = 'FINS'
          pisang = pist2_thet
        end if
           
        nmed= fin_med
        npar=11

        trap_par(1) = (fin_z2(zposneg)-fin_z1(zposneg))*.5
        trap_par(3) = 0.0
        trap_par(4) = fin_thick*.5
        trap_par(5) = .5*fin_z1(zposneg)*(tand(corn_ang(zposneg))
     1               -tand(pisang))-0.5
        trap_par(6) = trap_par(5)
        trap_par(7) = 0.0
        trap_par(8) = fin_thick*.5
        trap_par(9) = .5*fin_z2(zposneg)*(tand(corn_ang(zposneg))
     1               -tand(pisang))-0.5
        trap_par(10) = trap_par(9)
        trap_par(11) = 0.0
        trap_par(2)=atand((trap_par(9)-trap_par(5)+fin_z2(zposneg)*
     1  tand(pisang)-fin_z1(zposneg)*tand(pisang))/(trap_par(1)*2.))
        fin_r=tand(trap_par(2))*trap_par(1)+trap_par(5)+
     1       fin_z1(zposneg)*tand(pisang)
        fin_r = fin_r + 0.1           ! fix 1.1mm overlap (FINS-TSIP) Jan 2012 HvH
        call gsvolu(v_i_name,'TRAP',nmed,trap_par,npar,ivolu)
        call GSATT(v_i_name,'SEEN',1)
       
        irot=irot+1

        call gsrotm(irot,90.,247.5,90.,337.5,0.,0.)
        call gspos(v_i_name,1,v_m_name,fin_r*cosd(247.5),
     1   fin_r*sind(247.5),fin_z1(zposneg)+trap_par(1),
     1   irot,'ONLY')

c    add second fin

        irot=irot+1

        call gsrotm(irot,90.,292.5,90.,382.5,0.,0.)
        call gspos(v_i_name,2,v_m_name,fin_r*cosd(292.5),
     1   fin_r*sind(292.5),fin_z1(zposneg)+trap_par(1),
     1   irot,'ONLY')
c       call gspos(v_i_name,2,v_m_name,52.933,-127.791,270.5,
c     1 irot,'ONLY')
        endif           !test on fin_flag    

c	power buss - Aug 96 - DML

        if(buss_flag.eq.1)then
        if(zposneg.eq.1)then
          v_i_name = 'BUSN'
        else
          v_i_name = 'BUSS'
        endif
        
        nmed = buss_med
        npar=3
        buss_par(1)=.5*(abs(buss_z(zposneg))*tand(39.1)
     1              -coil_orad2(zposneg)-20.)
        buss_par(2)= buss_thick
        buss_par(3)= buss_wide
        call gsvolu(v_i_name,'BOX ',nmed,buss_par,npar,ivolu)
        call GSATT(v_i_name,'SEEN',1)
        irot=irot+1

        call gsrotm(irot,90.,247.5,90.,337.5,0.,0.)
        if(zposneg.eq.1)then        ! north   ! vol overlap BUSN-MUA1:
          call gspos(v_i_name,1,v_m_name,     ! changed +20. to +17. Jan'10 HvH
     1     -sind(22.5)*(coil_orad2(zposneg)+buss_par(1)+17.),
     1     -cosd(22.5)*(coil_orad2(zposneg)+buss_par(1)+17.),
     1     buss_z(zposneg),irot,'ONLY')
        else                       ! south
          call gspos(v_i_name,1,v_m_name,
     1     -sind(22.5)*(coil_orad2(zposneg)+buss_par(1)-5.),
     1     -cosd(22.5)*(coil_orad2(zposneg)+buss_par(1)-5.),
     1     buss_z(zposneg),irot,'ONLY')
         endif
     
        if(zposneg.eq.2)then
        irot=irot+1
        call gsrotm(irot,90.,292.5,90.,382.5,0.,0.)
        call gspos(v_i_name,2,v_m_name,
     1   sind(22.5)*(coil_orad2(zposneg)+buss_par(1)-5.),
     1   -cosd(22.5)*(coil_orad2(zposneg)+buss_par(1)-5.),
     1   buss_z(zposneg),irot,'ONLY')


        endif              ! second buss for south arm
        endif              !test on buss_flag     

c    Add Buss flags - Aug 96 - DML

        if(buss_buss_flag.eq.1)then
        if(zposneg.eq.1)then
          v_i_name = 'FLG1'
        else
          v_i_name = 'FLG2'
        endif
        nmed=flag_med
        npar=3

        flag_r=coil_orad2(zposneg)+flag_par(1)
        call gsvolu(v_i_name,'BOX ',nmed,flag_par,npar,ivolu)
        call GSATT(v_i_name,'SEEN',1)
       

          if(zposneg.eq.1)then
        do iflag=1,2 
            irot=irot+1

            flag_phi=(iflag-1)*180.+67.5
            flag_x=cosd(flag_phi)*flag_r
            flag_y=sind(flag_phi)*flag_r
            call gsrotm(irot,90.,flag_phi,90.,
     1      flag_phi+90.,0.,0.)
            call gspos(v_i_name,iflag,v_m_name,
     1      flag_x,flag_y,flag_z(zposneg), irot,'ONLY')
         enddo
          else

c       south arm has 2 flags at each octant boundary

            do jflag = 1,8
              irot=irot+1
              flag_phi=(jflag-1)*45.-22.5
              flag_x=cosd(flag_phi)*flag_r
              flag_y=sind(flag_phi)*flag_r
              call gsrotm(irot,90.,flag_phi,90.,
     1          flag_phi+90.,0.,0.)
              call gspos(v_i_name,jflag,v_m_name,
     1          flag_x,flag_y,flag_z(zposneg), irot,'ONLY')       
            enddo
          endif   !Test on zposneg

        endif     !test on buss_buss_flag

c********add water hoses to south arm
c         estimate hoses with 1 cm of plastic - DMLee Aug-96

        if(wtr_flag.eq.1)then
        if(zposneg.eq.2)then
          v_i_name = 'WATR'
          nmed = wtr_med
          npar=5

          wtr_r = coil_orad2(zposneg)+2*flag_par(1)+ 1.
          call gsvolu(v_i_name,'TUBS',nmed,wtr_par,npar,ivolu)
          call GSATT(v_i_name,'SEEN',1)
          do iflag = 1,8
            irot=irot+1
            wtr_phi = (iflag-1)*45. -22.5
            wtr_x = cosd(wtr_phi)*wtr_r
            wtr_y = sind(wtr_phi)*wtr_r
            call gsrotm(irot,90.,360.+wtr_phi,0.,
     1       360.+wtr_phi+90.,90.,270.+wtr_phi)
            call gspos(v_i_name,iflag,v_m_name,wtr_x,wtr_y,
     1        flag_z(2)-5.,irot,'ONLY')
          enddo                     
        endif      !Test on zposneg
        endif      !test on wtr_flag

c      Add teacup ribs - DMLee Aug-96

        if(rib_flag.eq.1)then
        do ip =1,2
       
          if(zposneg.eq.1)then

            if(ip.eq.1)then
              v_i_name = 'RB1N'
            else
              v_i_name = 'RB2N'
            endif

            pisang=pist_ang
            shad_two = shad_ang
            v_t_name = v_i_name

          else

            if(ip.eq.1)then
              v_i_name = 'RB1S'
            else
              v_i_name = 'RB2S'
            endif

            pisang=pist2_thet
            shad_two = 180.-shad_ang

          endif
       
          nmed=rib_med
          npar=10

          rib_par(1)=-22.5
          rib_par(2)=360.
          rib_par(3)=8.       
          rib_par(4)=2.
          rib_par(5)=rib_z1(zposneg,ip)
          rib_par(6)=rib_z1(zposneg,ip)*tand(pisang) + 10.
          rib_par(7)=rib_z1(zposneg,ip)*tand(shad_two)-1.0
          rib_par(8)=rib_z2(zposneg,ip)
          rib_par(9)=rib_par(6)
          rib_par(10)=rib_z2(zposneg,ip)*tand(shad_two)-1.0

          call gsvolu(v_i_name,'PGON',nmed,rib_par,npar,ivolu)
          call GSATT(v_i_name,'SEEN',1)
          call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'MANY')
       

c      Now subtract a circular volume from rib

          if(zposneg.eq.1)then
            if(ip.eq.1)then
              v_i_name = 'CUT1'
            else
              v_i_name = 'CUT2'
            endif
            pisang=pist_ang
          else
            if(ip.eq.1)then
              v_i_name = 'CUT3'
            else
              v_i_name = 'CUT4'
            endif
            pisang=pist2_thet
          endif

          npar=3
          nmed = cutt_med
          tube_par(1) = rib_z1(zposneg,ip)*tand(pisang) + 10. 
          tube_par(2) = rib_z1(zposneg,ip)*tand(35.5)
          tube_par(3) = (rib_z2(zposneg,ip)-
     1                      rib_z1(zposneg,ip))/2.
          if(zposneg.eq.2)then
          tube_par(2) = -tube_par(2)
          tube_par(3) = -tube_par(3)
          endif
          
          call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
          call GSATT(v_i_name,'SEEN',1)
          call gspos(v_i_name,ip,v_m_name,0.0,0.0,
     1      rib_z1(zposneg,ip)+iaa*tube_par(3),1,'ONLY') 
        enddo        !Loop on ip
        endif        !test on rib_flag

c Now do coil itself

c**>>>  Modified  18-APR-1993 / S. Saini
        if(zposneg.eq.1)then
          v_i_name = 'E1CO'
        else
          v_i_name = 'OC1E'
        endif
        nmed = coil_med   ! copper
        npar = 3
        tube_par(3) = 0.5*(coil_z12(2,zposneg)-coil_z12(1,zposneg))
        tube_par(1) = coil_irad1(zposneg)
        tube_par(2) = coil_orad1(zposneg)
        call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
c <<
        if(color_coil.gt.0)then
          CALL GSATT(v_i_name,'SEEN',1)
          CALL GSATT(v_i_name,'COLO',color_coil)
        else

c     invisible

          CALL GSATT(v_i_name,'SEEN',0)
        endif
 
c**>>>  Modified  18-APR-1993 / S. Saini
        if(zposneg.eq.1)then
          call gspos(v_i_name,1,v_m_name,0.0,0.0,
     1     coil_z12(1,zposneg)+tube_par(3), 1,'ONLY')
        else
          call gspos(v_i_name,1,v_m_name,0.0,0.0,
     1    -(coil_z12(1,zposneg)+tube_par(3)),1,'ONLY')

        endif
c Now put the second coil (new addition)
c Coil 2
 
        if(coil_irad2(zposneg).ne.0.) then
          if(zposneg.eq.1)then
            v_i_name = 'E2CO'
          else
            v_i_name = 'OC2E'
          endif
          nmed = coil_med   ! copper
          npar = 3
          tube_par(3) = 0.5*(coil_z12(4,zposneg)
     1      -coil_z12(3,zposneg))
          tube_par(1) = coil_irad2(zposneg)
          tube_par(2) = coil_orad2(zposneg)
          call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
          if(color_coil.gt.0)then
            CALL GSATT(v_i_name,'SEEN',1)
            CALL GSATT(v_i_name,'COLO',color_coil)
          else

c     invisible

            CALL GSATT(v_i_name,'SEEN',0)
          endif
          if(zposneg.eq.1)then
            call gspos(v_i_name,1,v_m_name,0.0,0.0,
     1        coil_z12(3,zposneg) + tube_par(3), 1,'ONLY')
          else
            call gspos(v_i_name,1,v_m_name,0.0,0.0,
     1        -(coil_z12(3,zposneg) + tube_par(3)), 1,'ONLY')

          endif
        endif !if(coil_irad2(zposneg).ne.0.) then
c <<

c     now put in geometry into proper mother volume for the MEGAPHONE

        if(zposneg.eq.1)then
          v_i_name = 'MEGA'
        else
          v_i_name = 'AGEM'
        endif
        nmed = mega_med   ! iron (magnetic)
        npar = 10
        pgon_par(1) = -22.5
        pgon_par(2) = 360.
        pgon_par(3) = 8.
        pgon_par(4) = 2.
 
c>>> Added S. Saini/23-MAR-1993
 
        if(mega_irad1(zposneg) .lt. 0.0       ! mega_irad1 in phnx.par is -150.711
     1  .or. mega_orad1(zposneg) .lt. 0.0
     1  .or. mega_irad2(zposneg) .lt. 0.0
     1  .or. mega_orad2(zposneg) .lt. 0.0)then
          mega_irad1(zposneg) = mega_z12(1,zposneg)*tan(shad_thet*cdtr)
          mega_orad1(zposneg) = mega_irad1(zposneg) + mega_thick
          mega_irad2(zposneg) = mega_z12(2,zposneg)*tan(shad_thet*cdtr)
          mega_orad2(zposneg) = mega_irad2(zposneg) +  mega_thick
        end if
c<<<
        if(zposneg.eq.1)then
          pgon_par(5) = -(mega_z12(2,1)-mega_z12(1,1))/2.
          pgon_par(6) = mega_irad1(1)
          pgon_par(7) = mega_orad1(1)
          pgon_par(8) = -pgon_par(5)
          pgon_par(9) = mega_irad2(1)
          pgon_par(10) = mega_orad2(1)
        else
          pgon_par(5) = -(mega_z12(2,2)-mega_z12(1,2))/2.
          pgon_par(6) = mega_irad2(2)
          pgon_par(7) = mega_orad2(2)
          pgon_par(8) = -pgon_par(5)
          pgon_par(9) = mega_irad1(2)
          pgon_par(10) = mega_orad1(2)
        endif
        call gsvolu(v_i_name,'PGON',nmed,pgon_par,npar,ivolu)
        if(color_endc.gt.0)then
          CALL GSATT(v_i_name,'SEEN',1)
          CALL GSATT(v_i_name,'COLO',color_endc)
        else

c     invisible

          CALL GSATT(v_i_name,'SEEN',0)
        endif
        if(zposneg.eq.1)then
          call gspos(v_i_name,1,v_m_name,0.0,0.0,mega_z12(1,1) +
     1     pgon_par(8),1,'ONLY')
        else
          call gspos(v_i_name,1,v_m_name,0.0,0.0,-(mega_z12(1,2)
     1     +pgon_par(8)),1,'ONLY')
        endif
      enddo    ! loop over positive and negative Z halves

      write(LOUT,9999)
9999  format(/,2x,'MAGNET <I>: Endcap magnet yoke installed')
      return

c--------------------------------------------------
999   continue
      write(6,1000)
1000  format(/,3x,'Read error in mag_par segment of geometry'/,3x,
     &   'The geometry will be re-read to pinpoint the erroneous',
     &  ' line',/,3x,'****This will cause the program to crash.****',/)
      write( 6, nml = mag_par )
      rewind( itf_lun );
      read( itf_lun, nml = mag_par )
      stop 'magnet - PISA stop because of PHNX.PAR file error.'
      end
