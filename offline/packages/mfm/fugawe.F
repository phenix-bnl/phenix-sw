*CMZ :  2.04/00 05/10/92  11.19.33  by  Charles F. Maguire
*-- Author :
*-- Author :
      subroutine fugawe(r,z,iwhere)
      implicit none
c
c     Subroutine to determine which magnetic field map is appropriate
c     Input variables are R and Z (always positive)
c     Output variable is IWHERE
c              IWHERE = 1  Central Region
c              IWHERE = 2  Endcap (fine grid) Region
c              IWHERE = 3  Endcap (wide grid) Region
c              IWHERE = 4  Z larger than ENDW_Z
c
c              IWHERE set negative if R value is too large
c
      real r, z
      integer iwhere
c
c     Initial version (May 5, 1992) has everthing "hardwired"
c     Future versions should have more control from GFFGO.DAT or PHNX.PAR
c
c     Revision May 18, 1992: No "wide grid region" for now
c     The IWHERE parameter useful in case one of the scale factors is 0
c
c     Note for Release 2:
c           This subroutine pre-dates Release 1 when there were two
c           different map files specified, and it was necessary to find
c           out which one applied. It is being retained in case one goes
c           again to a "fine" grid map file near the pole pieces.
c
      real cent_z1         ! Central goes to at least CENT_Z1
      parameter (cent_z1 = 140)  ! Presently CENT_Z1 = 140 cm
      real cent_z2         ! Central always ends by CENT_Z2
      parameter (cent_z2 = 180.0)   ! Presently CENT_Z2 = 180 cm
      real cent_r1         ! Yoke radial distance at CENT_Z1
      parameter (cent_r1 = 113.5)   ! Presently CENT_R1 = 113.5 cm
      real cent_r2         ! Yoke radial distance at CENT_Z2
      parameter (cent_r2 = 150.0)   ! Presently CENT_R2 = 150.0 cm
      real cent_slope         ! Central slopes from Z1 to Z2
      real cent_rmax       ! Central maximum radius
      parameter (cent_rmax = 460.0) ! Presently CENT_RMAX = 460 cm
      real endf_z1         ! Endcap fine starts at ENDF_Z1
      parameter (endf_z1 = 140.0)   ! Same as CENT_Z1 (for now)
      real endf_z2         ! Endcap fine ends at ENDF_Z2
      parameter (endf_z2 = 730.0)   ! Presently ENDF_Z2 = 730 cm
      real endw_z       ! Endcap wide ends at ENDW_Z
      parameter (endw_z = 730.0) ! Presently ENDW_Z = 730 cm
      real end_rmax        ! Endcap fine maximum radius
      parameter (end_rmax = 570.0)  ! Presently END_RMAX = 570 cm
c
c     local variables
c
      real rtest
      integer icall
      data icall /0/
      save icall, cent_slope
      if(icall.eq.0)then
         icall = 1
         cent_slope = (cent_r2 - cent_r1)/(cent_z2 - cent_z1)
      endif
c
c     now do checks as a function of Z first, then R
c
      if(z.le.cent_z1)then
c
c     inside inner central region
c
         if(r.le.cent_rmax)then
            iwhere = 1
         else
            iwhere = -1
         endif    ! check on rmax
         return   ! done with inner central region
      endif
      if(z.gt.cent_z2)then
c
c     either fine or wide grid endcap
c
         if(z.ge.endf_z1.and.z.le.endf_z2)then
c
c     fine grid endcap region
c
            if(r.le.end_rmax)then
               iwhere = 2
            else
               iwhere = -2
            endif       ! check on rmax
            return      ! done with fine grid point
         else
            if(z.le.endw_z)then
               if(r.le.end_rmax)then
                  iwhere = 3
               else
                  iwhere = -3
               endif
               return      ! check on rmax
            endif    ! done with wide grid point
         endif    ! check on fine and wide grid endacp
         if(z.gt.endw_z)then
            IWHERE = 4  ! beyond endcap wide grid region
            return   ! done with large Z grid point
         endif ! check on large Z
      endif ! check on outside the central region
c
c     All that is left is the sloping central/endcap region
c
      if(z.gt.cent_z1.and.z.le.cent_z2)then
         rtest = cent_r1 + cent_slope*z
         if(r.ge.rtest)then
c
c     central map
c
            if(r.le.cent_rmax)then
               iwhere = 1
            else
               iwhere = -1
            endif    ! check on rmax
            return   ! done with central point
         else
c
c     endap (fine grid) map
c
            if(r.le.end_rmax)then
               iwhere = 1
            else
               iwhere = -1
            endif    ! check on rmax
            return   ! cone with endcap point
         endif ! check on central/endcap boundary R value
      endif    ! check on Z within central/endcap boundary
      return   ! this return should never get executed
      end
