      SUBROUTINE NCC_GUSTEP
      implicit none

*      Author: Sky D. Rolnick, Febuary 28, 2007
*      Author: Charles F. Maguire, January 12, 2007

*        steps in Nosecone calorimeter

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
#include "ncc_data.inc"

c     By initializing hit array it is automatically saved 
c     between repeated calls to ncc_gustep.f (FORTRAN fact)
c     However, there is no need to do that saving in this version of the code

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
      character NAMATE
      real A, Z, DENS, RADL, ABSL, UBUF
      integer NWBUF, imate


      logical firstStoreEMPD /.true./
C      logical firstStoreHDPD /.true./
C      logical firstStorePSSP /.true./
C      logical firstStoreSMSP /.true./

      real         etot
      data etot/0.0/


c     Begin executable code

C-- Decides what tracks need to be further tracked
C      IF(NGKINE.GT.0) THEN
C         DO  I=1,NGKINE
C            ITYPA = GKIN(5,I)
C            IF (ITYPA.NE.4) CALL GSKING(I)
C         END DO
C      ENDIF


C--   Numbering scheme -volume copy numbers (number(2-5))
C--   Description    Value (volume identifier)           
C--   (2) FOCal           1/2      - positive and negative z
C--   (3) Subassembly(1x2 sensors) - iver+100*ihor
C--   (4) Sampling cells: 1- 7     - S0 (EM0)
C--                       8-14     - S1 (EMC1)
C--                      15-21     - S2 (EMC2)
C--   if cell kind (below) is 1 (strips) then this value spans 
C--   the range of 1-4 only (we have max 4 strip cells in FOCal)     
C--   (5) Silicon:  cell kind + Top/Bottom*10 + cell layer*100 
C--   cell layer in strip cells is used to distinguish between
C--   X and Y strips

      IF(isvol.gt.0. .and. iset.gt.0.and.idet.gt.0) THEN
         IF(DESTEP.GT.0.) THEN
            ncc  = number(2)
            detn = det0(iset)+idet
C            print *, 'DETN ' , detn
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
               if(number(3)/10.eq.0) go to 10
               segm = mod(number(3),10)
C--   sampling cell kind
               kind = mod(number(5),10)  
C--   horizontal/vertical
               if(kind.eq.2) then
                  cell  = number(4)
                  layer = cell-(segm-1)*7-1
               else 
                  layer = number(4)
                  xy    = 1
                  if(number(5)/100.gt.6) xy = 2
               end if
                 if(idtype.eq.20222.or.idtype.eq.20221) then
                  call sensor_address(sign(1.,vect(3))*vect(1),
     &                 vect(2), sen_x, sen_y)
                  if(idtype.eq.20221) then
                     call twr_address(0, sign(1.,vect(3))*vect(1),
     +                    vect(2), ix, iy)
C                     print *,layer,vect(3)
                 else
                     call strip_address(xy, sign(1.,vect(3))*vect(1),
     +                    vect(2), ix, iy)
C                     print *,xy,layer,vect(3)
                  end if
               end if

            else
               dz   = abs(vect(3))-Z_NCC
               segm = 0
               if(dz.LT.SEGM_DEPTH(3)) then
                  segm = 3
               else if (dz.lt.SEGM_DEPTH(2)+SEGM_DEPTH(3)) then
                  segm = 2
               else 
                  segm = 1
               end if
            endif

C--   
C--   
*     Energy. deposited in calorimeter

            numbv(1) = (((segm*10+layer)*10+xy)*100+sen_X)*100+sen_Y
            numbv(2) = (ncc*1000+ix)*1000+iy

C            if(kind.eq.1) then
C            print *, 'SEGM = ', segm, '   KIND = ', 
C     +           mod(number(5),10), '   LAYER = ', number(4),' XY', xy,
C     +           '   IDTYPE = ', idtype, '   IX = ', ix, '  IY = ', iy,
C     +           ' ', numbv(1)
C            end if

            hit_arr(1) = IEVENT
            hit_arr(2) = ncc
            hit_arr(3) = numbv(2) ! tower_id
            hit_arr(4) = numbv(1) ! sensor_id  
            hit_arr(5) = tofin  !  time track enters sensor
            hit_arr(6) = destep*1.e+6 ! GeV to KeV   ! energy loss converted to keV

C            if(firstStoreEMPD)then
C               firstStoreEMPD = .false.
C               write(6,1)ihit, (hit_arr(khit),khit=1,6)
C 1             format(/,' First EMPD store, ihit = ', i4,/,
C     1              6(g10.4,2x),/)
C            endif





C            write(*,9000) (names(ii),ii=1,5),
C     &           (number(ii),ii=1,5),
C     &           (numbv(ii),ii=1,2),
C     &           vect,destep,
C     &           iset,idet,idtype,segm,sen_x,sen_y,ix,iy,
C     &           KIND,cell,layer,xy,
C     &           IHSET,IHDET

C     call gsahit(iset,idet,ITRA,NUMBV,hit_arr,ihit)
            call gschit(iset,idet,ITRA,NUMBV,hit_arr,1,ihit)
         end if


      ENDIF
 10   CONTINUE


 9000 format(5a10/5i10/2i10/3f10.1,5(e10.1)/8i6/4i5,2A5)
 9001 format(10i10)

      end
