      subroutine emc_gustep(trak_mode)

      implicit none

c     *** global variables ***

#include "emcbit.inc"
#include "gconst.inc"
#include "gctmed.inc"
#include "gckine.inc"
#include "gctrak.inc"
#include "geant321/gcking.inc"	
#include "gcsets.inc"
#include "gcvolu.inc"

cMM   contains parameterization

      real pchkov

cMM   contains shift value of carriage

#include "emc_coord.inc"

c     *** local variables ***

      integer   trak_mode           ! 1: particle is entering PbGl
                                    ! 0: particle is in PbGl
      integer   trak_mode_before    ! was a particle entering in step before?
      integer   step_cnt            ! step counter

      integer	i,k,sector
      integer	ihit
      integer   l_mmfirst/1/        ! check for first call

      real	x_loc,y_loc,z_loc   ! location of photon
      real      mod_depth           ! depth in module
      real	p_x,p_y,p_z,p_abs   ! momentum of photon
      real      angle               ! angle w.r.t. module axis
      real	n_phot              ! fraction of detected photons
      real      photcnt             ! detected photons for the whole step
      real      phot_total          ! total photon number

      common /mm_step/ trak_mode_before,step_cnt,phot_total
      save   /mm_step/



      real	hit_arr(14)         ! array for saving hit-relevant data

      integer name4
      character*4 ch_name4
      equivalence (name4, ch_name4)
     
 
      real     thetaclass(90) 
      integer  thetaclasscnt
      data thetaclass /1.7455066E-02,  3.4920771E-02,  5.2407783E-02,
     &                  6.9926813E-02,  8.7488666E-02,  0.1051042,
     &                  0.1227846,      0.1405408,      0.1583844,
     &                  0.1763270,      0.1943803,      0.2125566,
     &                  0.2308682,      0.2493280,      0.2679492,
     &                  0.2867454,      0.3057307,      0.3249197,
     &                  0.3443276,      0.3639702,      0.3838640,
     &                  0.4040263,      0.4244748,      0.4452287,
     &                  0.4663077,      0.4877326,      0.5095255,
     &                  0.5317095,      0.5543091,      0.5773503,
     &                  0.6008607,      0.6248693,      0.6494076,
     &                  0.6745086,      0.7002077,      0.7265425,
     &                  0.7535541,      0.7812856,      0.8097841,
     &                  0.8390997,      0.8692867,      0.9004041,
     &                  0.9325150,      0.9656889,      1.000000,
     &                  1.035530,       1.072369,       1.110613,
     &                  1.150368,       1.191754,       1.234897,
     &                  1.279942,       1.327045,       1.376382,
     &                  1.428148,       1.482561,       1.539865,
     &                  1.600335,       1.664279,       1.732051,
     &                  1.804048,       1.880727,       1.962611,
     &                  2.050304,       2.144507,       2.246037,
     &                  2.355853,       2.475087,       2.605089,
     &                  2.747478,       2.904211,       3.077684,
     &                  3.270853,       3.487416,       3.732052,
     &                  4.010781,       4.331476,       4.704631,
     &                  5.144556,       5.671284,       6.313751,
     &                  7.115370,       8.144348,       9.514368,
     &                  11.43005,       14.30066,      19.08117,
     &                  28.63635,       57.29004,       1E+07/



c     *** program ***

c     check for first call of subroutine

      if (l_mmfirst.eq.1) then
        l_mmfirst = 0

        trak_mode_before = 0
        step_cnt         = 0
        phot_total       = 0.0
      endif



c     get sector-# of active sector

      name4 = names(4)
      if (CH_NAME4.eq.'EC72') then		! test if pbgl sector 7 or
        sector = 7				! sector 8 are the current
      else if (CH_NAME4.eq.'EC82') then 	! volumes
        sector = 8
      else
        write(*,*) 'Wrong sector!'		! only for debugging purposes;
        stop
      endif




c     sum over generated Cherenkov photons

      photcnt = 0.				! (re-)set photon counter

      if (NGPHOT.gt.0) then			! check for produced photons

        do i=1,NGPHOT

c     calculate depth of photon in module
cMM   carriage x-shift correction is done in emc_pbgl_coord

          x_loc = xphot(1,i)
          y_loc = xphot(2,i)
          z_loc = xphot(3,i)

cMM   remember that this is a location transformation
cMM   13.2cm is the lenght of the hut in front of the PbGl

          if (sector.eq.7) then	        	! sector 7 is hit
           call emc_pbgl_coord(x_loc,y_loc,z_loc,sector,0)
           mod_depth = abs(z_loc)-(emc_rpos+emc_dtof)-13.2
          else			        	! sector 8 is hit
           call emc_pbgl_coord(x_loc,y_loc,z_loc,sector,0)
           mod_depth = abs(z_loc)-(emc_rpos+emc_dtof)-13.2
          endif

c     transform momentum components into coordinate system of the
c     current sector to get the proper angle for the photon

          p_x = xphot(4,i)			! get momentum components
          p_y = xphot(5,i)			! of the photon
          p_z = xphot(6,i)
          p_abs = xphot(7,i)*(10**9)		! get total momentum (in eV)

c      here comes a momentum transformation

          call emc_pbgl_coord(p_x,p_y,p_z,sector,1)

          angle=sqrt(p_x*p_x+p_y*p_y)


          if (p_z.ne.0.) then	        	! photon not in the x-y plane

            angle = angle/abs(p_z)		! calculate tan(theta)


c     find appropriate value for angle in thetafield
      
            thetaclasscnt = 0		        ! counter for the range of the
            				        ! angle (steps of 1 degree)
            do k=1,90
              if (angle.lt.thetaclass(k)) then
                thetaclasscnt = thetaclasscnt + 1
                goto 1000
              else
                thetaclasscnt = thetaclasscnt + 1
              endif
            enddo
1000        continue


c     find value for photon energy in efield

            if ((mod_depth.lt.0.).or.(mod_depth.ge.40.)) then
*              print *,'ERROR !!! Data outside the PbGl modules'
*              print *,'Depth = ',mod_depth

               n_phot = 0.0
            else
              if ((p_abs.ge.1.5).and.(p_abs.lt.3.8)) then
                p_abs = (p_abs-1.5)*10.
                mod_depth = mod_depth*2.5       ! evaluate every 4mm

                n_phot = pchkov(int(p_abs)+1,
     &                          thetaclasscnt,int(mod_depth)+1)
              else
c                print *,'==> p_abs outside the right range!'
c                print *,'==> p_abs = ',p_abs

                n_phot = 0.0
              endif
            endif

            photcnt = photcnt + n_phot          ! add detected photons
            n_phot  = 0.0
          endif				        ! end check p_z != 0.0

        enddo				        ! end loop over produced photons

      endif				        ! check for produced photons



c     store information for current step

      if ((IGNEXT.ne.1).and.                    ! 1 -> particle is reaching a boundary
     &    (INWVOL.ne.2).and.                    ! 2 -> track is exiting current volume
     &    (ISTOP.eq.0)) then                    ! 0 -> tracking of particle continues

       if (NGPHOT.gt.0) then                    ! only save if photons were produced

        step_cnt = step_cnt + 1

        phot_total = phot_total + photcnt
        photcnt    = 0.0

        if ((step_cnt.ge.10).or.                ! save after 10 steps
     &     (trak_mode_before.eq.1)) then        ! make sure you get the minimum TOF value

          step_cnt  = 0                         ! reset step counter

          hit_arr(1) = xphot(1,1)               ! x loc. of first photon
          hit_arr(2) = xphot(2,1)               ! y loc. of first photon
          hit_arr(3) = xphot(3,1)               ! z loc. of first photon
          hit_arr(4) = 0.949744E-3*phot_total   ! det. energy
          hit_arr(5) = xphot(8,1)               ! TOF
          do i=6,12
            hit_arr(i) = 0.0
          enddo

          phot_total = 0.0                      ! reset total detected photons

          call gsahit(ISET,IDET,ITRA,NUMBV,hit_arr,ihit)    ! store hit

        endif                                   ! check step counter

       endif                                    ! check for produced photons

      else  ! particle stopped or reaching a boundary

        phot_total = phot_total + photcnt
        photcnt    = 0.0

        step_cnt = 0                             ! reset step counter

        if (phot_total.gt.0.0) then              ! are there photons in the stack?

c       store values in hit array

         hit_arr(1) = xphot(1,1)                 ! x loc. of first photon
         hit_arr(2) = xphot(2,1)                 ! y loc. of first photon
         hit_arr(3) = xphot(3,1)                 ! z loc. of first photon
         hit_arr(4) = 0.949744E-3*phot_total     ! det. energy
         hit_arr(5) = xphot(8,1)                 ! TOF
         do i=6,12
           hit_arr(i) = 0.0
         enddo

         phot_total = 0.0                        ! reset total detected photons

         call gsahit(ISET,IDET,ITRA,NUMBV,hit_arr,ihit)    ! store hit

        endif                                    ! check (phot_total.gt.0.0)

      endif                                     ! check tracking parameters


c     save variables for next step

      if ((NGPHOT.gt.0).or.(trak_mode.eq.1)) then

       trak_mode_before = trak_mode

      endif

      return
      end
