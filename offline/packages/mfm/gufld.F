c $Id: gufld.F,v 1.19 2009/10/01 20:33:11 hpereira Exp $
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                          c
c       SUBROUTINE GUFLD ( XYZ, Field )                                    c
c                                                                          c
c       Author:      Jim Thomas                                            c
c       Address:     L-397                                                 c
c                    Lawrence Livermore National Laboratory                c
c                    Livermore, CA 94550                                   c
c                    (510) 422-3434                                        c
c                    thomas@buddha.llnl.gov                                c
c                                                                          c
c       Subroutines: BFIELD                                                c
c                    GUFLDOLD                                              c
c                                                                          c
c       Description:                                                       c
c                                                                          c
c       Routine to compute the magnetic field at a point XYZ.              c
c       Called by GEANT and PISA.                                          c
c       Calls GUFLDOLD for the old maps (December 1992 and June 1993).     c
c       Calls BFIELD for the new maps (May 25, 1994 and thereafter).       c
c                                                                          c
c       This GEANT user subroutine is new since June, 1994.                c
c       Its main function is to allow users to continue with the old map   c
c       and linear interpolation scheme (N. Smirnov), OR to allow them     c
c       to use the new map and quadratic interpolation scheme (J. Thomas). c
c       The new interpolation scheme checks the steel boundaries to        c
c       ensure that all interpolation points are in the same medium (air   c
c       or steel) as the test point at XYZ.  The new Linear or Quadratic   c
c       interpolation depends on data in a Geant common block.  The        c
c       keywords are:                                                      c
c                                                                          c
c               "old"  => linear with old field map &    (ORDER=0)         c
c                         w/o steel check                                  c
c               "lin"  => linear with new field map      (ORDER=1)         c
c                         with steel check                                 c
c               "quad" => quadratic with new field map   (ORDER=2)         c
c                         with steel check               (default)         c
c                                                                          c
c       There is one global map field for all regions but additional       c
c       high resolution or low resolution sub-regions can be added.        c
c       See the comments in subroutine BFIELD.F, below.                    c
c                                                                          c
c       Please keep PISA specific hooks exclusively in this routine        c
c       so BFIELD.F can be separated off easily in order to provide        c
c       a clean way to access the Bfield maps for HGMC codes.              c
c                                                                          c
c       XYZ     =  (x,y,zz)   in centimeters                               c
c       Field   =  (Bx,By,Bz) in Kilogauss                                 c
c       fscale  =  real number  ==>   scale factor for field               c
c       ORDER   =  0 means old map / linear interpolation w/o steel check  c
c               =  1 means new map / linear interpolation                  c
c               =  2 means new map / quadratic interpolation               c
c       mcode   =  Material Code: 0 for air and coils, 1 for steel         c
c                  Mcode identifies the material at point (x,y,zz).        c
c                                                                          c
c       Fscale, ORDER are derived from information passed into GUFLD       c
c       in the GUPHX common block.  Mcode is derived from information      c
c       passed through the GCMATE common block.                            c
c                                                                          c
c       The scaling feature is convenient but may not be accurate since    c
c       the CM and MM steel are saturated in many regions.  B fields       c
c       generated using less current in the coils thus may not agree       c
c       with a linear scaling of the full strength field map.              c
c                                                                          c
c       Revisions:                                                         c
c              JT   5/25/94 First release                                  c
c           C.F.M   6/1/94  Reworked for backward compatibilty & scaling   c
c         JT & ST   6/2/94  Add steel boundary checking with Mcode         c
c                           information from Geant/PISA                    c
c         JT & AC   9/2/94  Add linear interpolation of NEW map and        c
c                           change keywords from "old","new" to            c
c                           "old", "lin", and "quad"                       c
c           C.F.M 12/18/98  Introduce 3D map switch                        c
c                                                                          c
c         C. F. Maguire: Jan. 12, 2002 Add Run1 3D map (November 2001)     c
c         C. F. Maguire: Feb. 28, 2003 Add Run3 3D map (February 2003)     c
c         C. F. Maguire: Dec. 31, 2003 Remove old Dec. 1998 map support    c
c         C. F. Maguire  Oct. 12, 2004 Add Run4 3D++ map (September 2004)  c
c                        Reversed field code for HBD not yet working       c
c                                                                          c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine    gufld ( xyz, field )
      implicit      none
      real          xyz(3), field(3)

#include "gcmate.inc"
#include "mfm.inc"

#define BFIELD3D03 bfield3d03v2

      integer steel10, steel40, steel45
      real KILOGAUSS
      parameter (steel10 = 10)
      parameter (steel40 = 40)
      parameter (steel45 = 45)
      parameter (KILOGAUSS = 0.001)
      real r, zz, Br, Bz, fscale
      integer order, mcode, sub_order, ifirst /0/
      character*4 volume
      save ifirst, order, sub_order, fscale

c       Z is used by one of the PISA common blocks so we use (x,y,zz) here.
c       Execute this loop only once.
c       The CPVOLU and the RPVOLU are standard PISA volume options in GUPHNX.

      ! one should really avoid doing these string comparison at every
      ! call to gufld, and only do them once, save an integer, and use it for
      ! later comparison.

      if ( ifirst .eq. 0 ) then

        volume = cpvolu_opt(1,1)
        call cltou(volume)
        ifirst = 1

        if( volume.eq.'OLD'.or.volume.eq.'VOLD' ) then

          order = 0
          sub_order = 0

        elseif( volume.eq.'LIN' ) then

          order = 1
          sub_order = 0

        elseif( volume.eq.'QUAD'.or.volume.eq.'2D97' ) then

          ! note: quadratic interpolation here is for 2d
          order = 2
          sub_order = 0

        elseif( volume.eq.'3D99' ) then

          ! note: three dimensional (new december 1998)
          order = 3
          sub_order = 0

        elseif( volume.eq.'2D01'.or.volume.eq.'2DIM' ) then

          ! September 2001 Muon Arm polarity reversal version
          ORDER = 20
          sub_order = 0

          elseif(
     +    volume.eq.'3D01'
     +    .or.volume.eq.'3DIM'
     +    .or.volume.eq.'3D03'
     +    .or.volume.eq.'3D+0'
     +    .or.volume.eq.'3D++'
     +    .or.volume.eq.'3D+-'
     +    .or.volume.eq.'MNAM' ) then

          ! reversed field not yet implemented
          ! 3d real map version (multiple versions possible)
          order = 30
          if( volume .eq. '3D01' ) sub_order = 0
          if( volume .eq. '3DIM' ) sub_order = 1
          if( volume .eq. '3D03' ) sub_order = 1
          if( volume .eq. '3D+0' ) sub_order = 1
          if( volume .eq. '3D++' ) sub_order = 2
          if( volume .eq. '3D+-' ) sub_order = 3
          if( volume .eq. 'MNAM' ) sub_order = 4

        elseif( volume.eq.'2D03' ) then

          ! run3 3d field, averaged over all azimuth
          order = 32
          sub_order = 0

        elseif( volume.eq.'2D04' ) then

          ! 2d magnetic field for the hbd simulations (not yet implemented)
          order = 21
          sub_order = 0

        else

          write(6,10)cpvolu_opt(1,1)
 10       format('gufld - cpvolu_opt(1,1) = ',a4)
          stop ' Invalid magnetic field option'

        endif

        fscale = rpvolu_opt(2,1)
        if ( fscale .eq. 0.0 ) then

          write(6,1)

        elseif ( fscale .ne. 1.00 )then

          write(6,2) fscale

        endif

      endif

      ! Start of Main loop.
      ! If point XYZ is inside steel, set mcode = 1; if in air set mcode = 0
      if (nmat.eq.steel10.or.nmat.eq.steel40.or.nmat.eq.steel45) then
        mcode = 1
      else
        mcode = 0
      endif

      if ( order .le. 0 ) then

        ! using old version
        call gufldold(xyz,field)

      else

        if ( fscale .eq. 0.0 ) then

          ! check on non-zero scale factor
          Field(1) = 0.0
          Field(2) = 0.0
          Field(3) = 0.0

        else

          if(order.eq.3) then

            write(*,*) 'bfield3d99 has been deemed obsolete'
            ! 3 dimensional field map
            ! BNL October 1999
            !call bfield3d99(XYZ, Field)
            ! convert to kilogauss
            !Field(1) = Field(1)*KILOGAUSS
            !Field(2) = Field(2)*KILOGAUSS
            !Field(3) = Field(3)*KILOGAUSS

          elseif(order.eq.30) then

            ! BNL November 2001
            if( sub_order .eq. 0 ) then
              write(*,*) 'bfield3d01 has been deemed obsolete'
              !call bfield3d01(XYZ, Field)

            ! BNL February 2003
            elseif( sub_order .eq. 1 ) then
              call bfield3d03(xyz, field, 0, '', 0)

            ! Outer and inner coils, pos. polarity
            elseif( sub_order .eq. 2 ) then
              call bfield3d03(xyz, field, 1, '', 0)

            ! Outer and inner coils, +- polarity
            elseif( sub_order .eq.3 ) then
              call bfield3d03(xyz, field, 3, '', 0)

            ! variable name
            elseif( sub_order .eq.4 ) then
              call bfield3d03(xyz, field, 2, '', 0)

            else

              ! invalid
              write(*,*) 'gufld - invalid sub_order: ',sub_order

            endif

            ! convert to kilogauss
            Field(1) = Field(1)*KILOGAUSS
            Field(2) = Field(2)*KILOGAUSS
            Field(3) = Field(3)*KILOGAUSS

          else

            ! 2 dimensional field map
            r  = sqrt( XYZ(1)**2 + XYZ(2)**2 )
            zz = XYZ(3)
            call bfield( r, zz, br, bz, mcode, order )
            if ( r .ne. 0.0 ) then

              Field(1) = Br * (XYZ(1)/r) * KILOGAUSS
              Field(2) = Br * (XYZ(2)/r) * KILOGAUSS
            else
              Field(1) = 0.0
              Field(2) = 0.0
            endif

            Field(3) = Bz * KILOGAUSS

          endif  ! check for 3D field

          ! check on scale factor <> 1.0
          if ( fscale .ne. 1.0 ) then
            Field(1) = fscale*Field(1)
            Field(2) = fscale*Field(2)
            Field(3) = fscale*Field(3)
          endif

        endif

      endif

      return

1     format('gufld - You have requested a scale',
     +' factor = 0.0.','. No field is calculated')
2     format('gufld - You have requested a scale',
     +' factor = ',f5.2,'. Normal value is 1.00.')
      end

