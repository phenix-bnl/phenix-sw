*CMZ :  2.04/00 14/10/94  09.16.26  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   07/10/94
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                          c
c        SUBROUTINE CZECHSTEEL                                              c
c    >        (jlow,klow,mpoints,npoints,Bpointer,ORDER,Mutable,Mcode)     c
c                                                                          c
c       Author:      Jim Thomas                                            c
c       Address:     L-397                                                 c
c                    Lawrence Livermore National Laboratory                c
c                    Livermore, CA 94550                                   c
c                    (510) 422-3434                                        c
c                    thomas@buddha.llnl.gov                                c
c                                                                          c
c       Subroutines: ISITSTEEL                                             c
c                                                                          c
c       Description:                                                       c
c                                                                          c
c       Routine to check whether the matrix of points to be used in        c
c       interpolating the B field includes a mixture of steel and air      c
c       values.  (The values in the steel can be MUCH larger than the      c
c       values in the adjacent air.)  If a mixed condition exists,         c
c       this routine searches for a new matrix of points that are all      c
c       in air, if Mcode = 0, or all in steel, if Mcode = 1.               c
c                                                                          c
c       Revisions:       6/1/94   First Release                            c
c                    JT 8/31/94   Allow for Linear Interpolation           c
c                                                                          c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
        subroutine czechsteel
     >        (jlow,klow,mpoints,npoints,Bpointer,ORDER,Mutable,Mcode,
     >         iwflag)
        implicit   none
        integer    jlow, klow, mpoints, npoints, Bpointer, ORDER, Mcode
        integer*2  Mutable(*)
        integer    i, j, k, ii, jj(16), kk(16), isitsteel
 
        integer maxwarn, nwarn, iwflag
        parameter (maxwarn = 10)
        data nwarn /0/
 
        data jj    / 0, 0,-1,-1, 0, 1,-1, 1, 1, 0,-1,-2,-2,-2, 1,-2/
        data kk    / 0,-1, 0,-1, 1, 0, 1,-1, 1,-2,-2,-1, 0, 1,-2,-2/
        save       jj, kk, nwarn
 
        iwflag = 1  ! default as an error
c
c       jj() and kk() are only written for Linear and Quadratic interpolation.
c       They could be expanded for higher order if you wish to work it out.
c
        if (ORDER.ne.1 .and. ORDER.ne.2) then
           write(6,*) ' Checking of Steel Boundaries is only valid if',
     >                ' you are using Linear or Quadratic interpolation'
           write(6,*) ' Boundaries have NOT been checked: ORDER =',ORDER
           return
        endif
 
        do i = 1, (ORDER+2)**2
           j  = jlow + jj(i)
           k  = klow + kk(i)
           ii = isitsteel(j,k,mpoints,npoints,Bpointer,ORDER,Mutable)
           if ( (Mcode.eq.0 .and. ii.eq.0) .or.
     >          (Mcode.eq.1 .and. ii.eq.(ORDER+1)*(ORDER+1)) ) then
              jlow = j
              klow = k
              iwflag = 0
              return
           endif
        enddo
 
        if(nwarn.lt.maxwarn)then
           nwarn = nwarn + 1
           write(6,*) ' CZECHSTEEL <W>:'
           write(6,*) ' Warning: Using mixed air and steel B field',
     >                ' values in this interpolation result.'
           if(nwarn.eq.maxwarn)then
              write(6,*) ' This was the last warning message'
           endif
        endif
        return
        end
