      SUBROUTINE MXPS_GUSTEP
      implicit none

*      Author: Sky D. Rolnick, Febuary 28, 2007
*      Author: Charles F. Maguire, January 12, 2007
*
*      3/26/2011 - JGL - Modified for MXPS geometry


*-----------------------------------------------------------

C     *** global variables ***
                                    

c     Subsytem programmers should be familiar with the contents of this GEANT common blocks

#include "gckine.inc"
#include "gctrak.inc"
#include "gcsets.inc"
#include "gctmed.inc"
#include "gcvolu.inc"
#include "gcflag.inc"
#include "geant321/gcking.inc"
#include "mpcex_ps_data.inc"
#include "guphnx.inc"

      real hit_arr(6) /6*0.0/   ! assumes 6 hit parameters
      integer i,ii,ihit, khit
      integer ix,iy, segm
      integer sen_x, sen_y
      integer kind
      integer tower_id
      integer sensor_id
      integer sen_pos
      real    tofIn, dz

      integer    cell, layer, xy,  ncc, 
     +     xGlob, yGlob, detn

c     Temporary debug variables
c      character NAMATE
c      real A, Z, DENS, RADL, ABSL, UBUF
c      integer NWBUF, imate

c     Begin executable code


C--   Numbering scheme -volume copy numbers (number(2-5))
C--   Description    Value (volume identifier)           
C--   (2) FOCal           1/2      - positive and negative z
C--   (3) Subassembly              - iver+100*ihor
C--   (4) Sampling cells: 1- 7     - S0 (EM0)
C--                       8-14     - S1 (EMC1)
C--                      15-21     - S2 (EMC2)
C--   if cell kind (below) is 1 (strips) then this value spans 
C--   the range of 1-4 only (we have max 4 strip cells in FOCal)     
C--   (5) Silicon:  cell kind + Top/Bottom*10 + cell layer*100 
C--   cell layer in strip cells is used to distinguish between
C--   X and Y strips

      IF(  (isvol.gt.0).and.(iset.gt.0).and.(idet.gt.0) ) THEN

         IF(DESTEP.GT.0.) THEN

            ncc  = number(3)
            detn = det0(iset)+idet
            segm  =  0
            sen_x = 99
            sen_y = 99
            ix    = 99
            iy    = 99
            kind  =  0
            cell  =  0
            layer =  0
            xy    =  0

            if(iset.eq.0.or.idet.eq.0) then
               print *, 'Problem ',ihset,' ',ihdet,' ',iset,' ',idet,
     +                  ' ',idtype,' ',nvname
            endif

C--   FIRST - FIND CALORIMETER SEGMENT
            if(iset.ne.0) then

C--   only one segment in MXPS
               segm = 1               

C--   sampling cell kind
               kind = mod(number(6),10)  

C--   horizontal/vertical
               if(kind.eq.2) then
                  layer  = number(5)
               else 
                  layer = number(5)

                  if(RHICRUN.le.14) then

C--                 different layer configuration for Run-14 MPC-EX south
                    if(ncc==2) then
                      if((layer.eq.2).or.(layer.eq.4).or.
     &                   (layer.eq.6).or.(layer.eq.8)) then 
                        xy    = 1
                      else
                        xy    = 2
                      end if
                    else
                      if((layer.eq.1).or.(layer.eq.3).or.
     &                   (layer.eq.5).or.(layer.eq.7)) then 
                        xy    = 1
                      else
                        xy    = 2
                      end if
                    end if

                  else
                    if((layer.eq.1).or.(layer.eq.3).or.
     &                 (layer.eq.5).or.(layer.eq.7)) then 
                      xy    = 1
                    else
                      xy    = 2
                    end if
                  end if

               end if

               if(idtype.eq.20222) then
                  call mxps_sensor_address(sign(1.,vect(3))*vect(1),
     &                 vect(2), sen_x, sen_y)
                    call mxps_strip_address(
     *                         xy, sign(1.,vect(3))*vect(1),
     *                         vect(2), ix, iy)

C--               bail out if sensor addresses are invalid - 
C--               This should not happen
                  if((sen_x<1).or.(sen_y<1).or.
     &                   (sen_x>6).or.(sen_y>6)) return 

C--               If the strip addresses are invalid then we are
C--               in sensor dead areas - record this in a 
C--               special sensor. 
C--               if((ix.eq.0).or.(iy.eq.0)) return 

                  if((ix.eq.0).or.(iy.eq.0)) then 

                    if(xy.eq.1) then 
                       ix = N_PXL_XY*N_TWR_XY+1; 
                       iy = 1 
                    else
                       ix = 1 
                       iy = N_PXL_XY*N_TWR_XY+1; 
                    end if

                  end if

               else
                  
                  return; 

               end if

            else

               goto 10

            endif
    
*     Energy. deposited in calorimeter

            numbv(1) = (((segm*10+layer)*10+xy)*100+sen_X)*100+sen_Y
            numbv(2) = (ncc*1000+ix)*1000+iy

            hit_arr(1) = IEVENT
            hit_arr(2) = ncc
            hit_arr(3) = numbv(2) ! tower_id
            hit_arr(4) = numbv(1) ! sensor_id  
            hit_arr(5) = tofin  !  time track enters sensor
            hit_arr(6) = destep*1.e+6 ! GeV to KeV   ! energy loss converted to keV

c            print *,'mxps_gustep - gschit called!, idtype = ',idtype

            call gschit(iset,idet,ITRA,NUMBV,hit_arr,1,ihit)

         end if

      ENDIF
 10   CONTINUE


 9000 format(5a10/5i10/2i10/3f10.1,5(e10.1)/8i6/4i5,2A5)
 9001 format(10i10)

      end
