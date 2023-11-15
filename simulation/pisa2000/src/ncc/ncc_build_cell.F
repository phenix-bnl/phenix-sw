C**********************************************************************  
c     $Id: ncc_build_cell.F,v 1.4 2009/03/11 03:40:16 chiu Exp $
C     author : Edward Kistenev
C     author : Sky D. Rolnick 02/28/07
      
C**********************************************************************
C--   PARENT here is the pointer to volume_id
      SUBROUTINE BUILD_CELL(kind, parent)
      integer       kind, parent

#include "gcbank.inc"
#include "gcnum.inc"
#include "gcflag.inc"
#include "ncc_data.inc"

      integer  il, nr, ivol, iv0, jv0
      real     zlayer, lt, x, y
C--   loop over maximum number of layers in the cell
      zlayer = -cell_depth(kind)/2.
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

C--   Our structure is stackable. Everyting except W and carriers boards is repeated twice (up/down positions) per cell 
            x = 0.
            y = 0.
            if(ivol.ge.46) then 
C--   predicted X position
               x = STDX
C--   an extra tol/2 gap is included vertically to keep stacks separated
C--   distinct volume #  to carry  cell kind identification
               y = -tol/2.-CRDXY/2.- STDY
C--   TODO skip air gaps if present in the stack
C--   copy numbers are: cell kind + Top/Bottom*10 + cell layer*100 
               call gspos(volume_id(ivol), kind+10+il*100, 
     &              volume_id(parent), x, y, zlayer, 0, 'ONLY')
               y = -y
               call gspos(volume_id(ivol), kind+20+il*100, 
     &              volume_id(parent), x, y, zlayer, 0, 'ONLY')
            else
               call gspos(volume_id(ivol), kind,volume_id(parent),
     &              x, y, zlayer, 0, 'ONLY')
            end if
            zlayer = zlayer + lt/2.
         end if
      end do
 999  RETURN
      END




