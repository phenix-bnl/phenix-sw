
C--   PARENT here is the pointer to volume_id

      SUBROUTINE MPCEX_BUILD_PS_CELL(kind, parent)

      integer       kind, parent

#include "gcbank.inc"
#include "gcnum.inc"
#include "gcflag.inc"
#include "mpcex_ps_data.inc"

      integer  il, nr, ivol, iv0, jv0
      real     zlayer, lt, x, y

C--   Only valid for building the minipad layers
      if(kind.ne.1) return 

C--   loop over maximum number of layers in the cell

      zlayer = -cell_depth(kind)/2.

      print *,'<BUILD_CELL> cell_depth/2 = ', cell_depth(kind)/2.       

      do il = 1, maxvol

C--   place copies of GENERIC VOLUMES into preexisting CELL volume
C--   <NOTE> VOLUME COPY NUMBER = Location of parent volume id*100
C--   +layer# inside the parent*10
C--   +copy# in a layer

         if(cell_st(il,kind).ne.0) then

            ivol = cell_st(il,kind)

C--   find the z-size of this layer

            call glook(volume_id(ivol), iq(jvolum+1), nvolum, iv0)
            if(iv0.gt.0) then
               jv0 = lq(jvolum-iv0)
            else
               jv0 = 0
            endif
            lt = 0.
            if(jv0.gt.0) then
               lt = q(jv0+9)*2.
            endif
            zlayer = zlayer + lt/2.

            if(iswit(5).gt.0) 
     &           print *,'<BUILD_CELL> cell ',kind,' layer ',
     &           il,' ',lt, zlayer,' ',volume_id(ivol)

            x = 0.
            y = 0.

            if(ivol.ge.46) then
 
C--   copy numbers are: cell kind + Top/Bottom*10 + cell layer*100 

               call gspos(volume_id(ivol), kind+10+il*100, 
     &              volume_id(parent), x, y, zlayer, 0, 'ONLY')

            else

               call gspos(volume_id(ivol), kind,
     &              volume_id(parent), x, y, zlayer, 0, 'ONLY')

            end if

            if(volume_id(ivol).eq.'STSI') then
               
               depth_to_Si = z_layer
               
            end if

            print *,'<BUILD_CELL> ',kind,volume_id(ivol),x,y,zlayer        

            zlayer = zlayer + lt/2.

         end if
      end do
 999  RETURN
      END




