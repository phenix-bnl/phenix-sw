*CMZ :  2.04/00 05/10/92  11.19.37  by  Charles F. Maguire
*-- Author :
      integer function u_ctoh4(c_string)
      implicit none
c
c Convert a character string into a hollerith format
c
c strings longer than 4 chars will be truncated, shorter ones blank padded
c
      character*(*)  c_string
      character*4 c_temp
      integer     itemp
c
      c_temp = c_string
      call uctoh(c_temp,itemp,4,4)
      u_ctoh4=itemp
      return
      end
