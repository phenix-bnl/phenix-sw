*CMZ :  2.04/00 07/10/94  15.43.31  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   07/10/94
c
c       Check all points in matrix of points to be used in the
c       interpolation to see if they are all in air, or all in steel.
c       Return 0 if all in air, return (ORDER+1)**2 if all in steel
c       and return -1 if matrix is partially in air and partially in steel.
c
        integer    function isitsteel
     >             (jlow,klow,mpoints,npoints,Bpointer,ORDER,Mutable)
        implicit   none
        integer    jlow, klow, mpoints, npoints, Bpointer, ORDER
        integer*2  Mutable(*)
        integer    j, k, kk
 
        isitsteel = 0
        do j = jlow, jlow + ORDER
           do k = klow, klow + ORDER
              if ( jlow.ge.1 .and. jlow+ORDER.le.mpoints .and.
     >             klow.ge.1 .and. klow+ORDER.le.npoints ) then
                 kk = (jlow-1) + (k-1)*mpoints + Bpointer
                 if ( Mutable(kk).gt.0 ) isitsteel = isitsteel + 1
              else
                 isitsteel = -1
                 return
              endif
           enddo
        enddo
 
        return
        end
