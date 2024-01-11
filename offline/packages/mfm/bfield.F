*CMZ :  2.04/00 20/10/94  23.41.49  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   07/10/94
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                          c
c       SUBROUTINE BFIELD ( r, z, Br, Bz, Mcode, IOrder )                  c
c                                                                          c
c       Author:      Jim Thomas                                            c
c       Address:     L-397                                                 c
c                    Lawrence Livermore National Laboratory                c
c                    Livermore, CA 94550                                   c
c                    (510) 422-3434                                        c
c                    thomas@buddha.llnl.gov                                c
c                                                                          c
c       Subroutines: HUNT                                                  c
c                    POLINT                                                c
c                    CZECHSTEEL                                            c
c                                                                          c
c       Description:                                                       c
c                                                                          c
c       Routine to compute the magnetic field at a point (r,z).  Br and    c
c       Bz are returned in Gauss.  The cylindrically symmetric maps are    c
c       read into arrays and interpolated by a Bi-Quadratic routine taken  c
c       from "Numerical Recipes".  The maps are assumed to list R and Z    c
c       in DESCENDING order.                                               c
c                                                                          c
c       All 2D maps are read into one dimensional arrays and the           c
c       data are retreived by knowing that Fortran stores data in memory   c
c       by going down the columns of the array (ie. incrementing R first   c
c       in B(R,Z)).  The storage arrays are of maximum size B(LIMIT) and   c
c       if you exceed LIMIT, the program will stop and give you an error   c
c       message.                                                           c
c                                                                          c
c       More than one field map can be read; representing different        c
c       regions, for example, or different resolutions.  The variable      c
c       "Iwhere" indicates which map is being used for the interpolation.  c
c       If the point (r,z) falls within the domain of more than one map,   c
c       the map read LAST will take precedence. Thus, a high resolution    c
c       map with limited coverage should be read in after a course map     c
c       that covers the entire region.  Independent maps can be read in    c
c       any order.                                                         c
c                                                                          c
c       "jlow,klow" identify the grid square that contains the (r,z)       c
c       pair from the previous call to BFIELD.F  If these are non-zero,    c
c       the new search will begin starting at element (jlow,klow).         c
c       Otherwise, all tables are searched in the reverse order to         c
c       which they were read.                                              c
c                                                                          c
c       Note: If the point (r,z) is not in any of the tables then the      c
c       B field components are assumed to be zero.                         c
c                                                                          c
c       Mcode    =   Material at point (r,z).  Input by calling routine.   c
c                    0 = air and coils, 1 = steel.  Set to 0 if not used.  c
c       MAPS     =   MAXIMUM number of field maps to be read               c
c       RLIMIT   =   Maximum number of elements in Rtable() array.         c
c       ZLIMIT   =   Ditto for Ztable().                                   c
c       BLIMIT   =   Ditto for Btable().  | Sum of entries for ALL maps |  c
c       ORDER    =   1 => Linear interpolation, 2 => Quadratic interp      c
c       NAMES()  =   List of File names containing B field maps to read.   c
c       Iwhere   =   1 implies Map #1                                      c
c                =   2 implies Map #2  (subset of #1 or independent)       c
c                =   3 implies Map #3  (subset of #2 or independent)       c
c                =   etc.                                                  c
c                                                                          c
c       Revisions:   First Release 5/25/94                                 c
c              JT    6/1/94  Add Mcode information from PISA               c
c                            Change interpolation code to avoid mixing     c
c                            air and steel data points when interpolating. c
c              ST    6/1/94  Make Rstep and Zstep into arrays              c
c              JT    8/28/94 Change number of columns in data file and     c
c                            the corresponding READ statement.             c
c                            Add linear interpolation                      c
c             CFM   10/20/94 Add variable number of maps capability        c
c             CFM   03/12/98 "Save" nmaps variable (Linux caught error)    c
c             CFM   07/08/03 Add Run3 2D field map, Central Arm only       c
c                                                                          c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
        SUBROUTINE    BFIELD( r, z, Br, Bz, Mcode, IOrder )
        implicit      none
 
c
c  CFM: Czechsteel NTUPLE diagnosis insertions
c
 
#include "mfm.inc"
 
      integer iwflag
      integer itrasav
      integer ifirst /0/
      integer nomutable /0/
 
      real          r, z, Br, Bz
      integer       Mcode, nmaps    ! nmaps is new from cfm, 10/20/94
      integer       MAPS, RLIMIT, ZLIMIT, BLIMIT, IORDER, ORDER
      parameter    (MAPS = 3)
      parameter    (RLIMIT = 1000)
      parameter    (ZLIMIT = 1000)
      parameter    (BLIMIT = 50000)
      real          Rtable(RLIMIT,MAPS), Ztable(ZLIMIT,MAPS)
      real          dBr, dBz, Brtable(BLIMIT), Bztable(BLIMIT)
      real          B(10), C(10), Rstep(MAPS), Zstep(MAPS)
      integer*2     Mutable(BLIMIT)
      integer       mpoints(MAPS), npoints(MAPS), Bpointer(MAPS)
      integer       i, j, k,  jlow, klow, Iwhere
      integer       kk
      character*78  string
      logical       Readflag
      character*48  NAMES(MAPS)
      data          NAMES(1) / 'map_both_february_97.dat' /
      character*80  afsname0, afsname1, afsname, afsname2
      data          ReadFlag /.FALSE./
      data          nmaps /0/
            
      data afsname0 
     +/'/afs/rhic/phenix/software/simulation/map_both_february_97.dat'/

      data afsname1 
     +/'/afs/rhic/phenix/software/simulation/map_both_september_01.dat'/

      data afsname2 
     +/'/afs/rhic/phenix/software/simulation/woody-+full-mapMar04.txt'/

      save
 
c
c     Initialization and NTUPLE booking
c
      if(ifirst.eq.0)then
        
        ifirst = 1
        itrasav = 0
        order = iorder
        afsname = afsname0
              
        ! three possiblities of 2D map: February '97, September '01, March '04
        if(order.eq.20.or.order.eq.21)then
          if(order.eq.20)then
            names(1) = 'map_both_september_01.dat'
            afsname = afsname1
          else
            names(1) = 'woody-+full-mapMar04.txt'
            afsname = afsname2
          endif
          order = 2
        endif 
        
        if(order.eq.32)then
          names(1) = 'map_central_july_03.dat'
          order = 2
          nomutable = 1
        endif
        
      endif  ! initialization
            
c        Input B field maps (once).
      if ( .not. ReadFlag ) then
        
        nmaps = ipvolu_opt(3,1)  ! external number of maps
        
        if(nmaps.le.0)then
          nmaps = 1   ! default = 1 map
        endif  ! check on at least one map file
        
        if(nmaps.ge.2)then
          write(6,*)' BFIELD <I>: Number of field maps requested ',
     1    nmaps
          open(unit=15,file='map_extra.txt',err=199,status='old')
          
          do i = 2,nmaps
            read(15,101,err=198,end=197)names(i)
101         format(a48)
          enddo
          
          close(unit=15)
        endif  ! check on number of maps >= 2 for info message
        
        do i = 1, nmaps
          open(unit=15,file=NAMES(i),status='old',err=102)
          goto 105
c
c         Following is good only for nmaps = 1
c
102       continue
          if(i.gt.1)then
            goto 200
          endif
          write(6,103)names(i)
103       format(/,' bfield.f <I> Unable to find map file',
     +    2x,a40, ' in local directory',/,
     +    2x,'Will look in simulation AFS area at RCF',/)
                 open(unit=15,file=afsname,status='old',err=200)
          write(6,104)
104       format(/,' bfield.f <I> Found map file in AFS area',/)
105       continue
                 write(6,100) NAMES(i)
          
                 do j = 1, 10
            read(15,106,err=201) string
            write(6,110) string
          enddo
          
          write(6,*)
          read(15,*,err=202)
          read(15,*,err=203)mpoints(i),npoints(i),Rstep(i),Zstep(i)
          read(15,*,err=204)
          if(mpoints(i).gt.RLIMIT.or.npoints(i).gt.ZLIMIT) goto 210
          if ( i .eq. 1 ) then
            Bpointer(i) = 1
          else
            Bpointer(i) = mpoints(i-1)*npoints(i-1) + Bpointer(i-1)
          endif
          
          do k = 1, npoints(i)
            do j = 1, mpoints(i)
              kk = (j-1) + (k-1)*mpoints(i) + Bpointer(i)
              if ( kk .gt. BLIMIT )then
                write(6,*)' BFIELD <F>: BLIMIT ',blimit,
     1          ' is too small'
                go to 210
              endif  ! check on BLIMIT
              
              if(nomutable.eq.0)then
                read(15,*) Rtable(j,i),Ztable(k,i),Brtable(kk),
     1                     Bztable(kk),Mutable(kk)
              endif
              
              if(nomutable.eq.1)then
                read(15,*) Rtable(j,i),Ztable(k,i),Brtable(kk),
     1          Bztable(kk)
              endif
            enddo
          enddo
          close(unit=15)
        enddo
        ReadFlag = .TRUE.
      endif
      
c
c        MAIN LOOP STARTS HERE: Find which Map applies to this (r,z) point.
c       Extrapolate table 1/2 cell beyond regular boundaries to avoid case
c       where user provides two maps that don't have an overlapping row.
c
      do i = nmaps, 1, -1
        Iw  here = i
        
        if( 
     >    r.le.Rtable(1,i) + 0.55*Rstep(i) .and.
     >    r.ge.Rtable(mpoints(i),i) - 0.55*Rstep(i)  .and.
     >    z.le.Ztable(1,i) + 0.55*Zstep(i)           .and.
     >    z.ge.Ztable(npoints(i),i) - 0.55*Zstep(i)) then
                 call hunt(Rtable(1,i),mpoints(i),r,jlow)
                 call hunt(Ztable(1,i),npoints(i),z,klow)
          go to 25
        endif
        Iwhere = 0
      enddo
 25   continue
      
c
c       Interpolate the tables using bi-linear or bi-quadratic
c       interpolation scheme.  Note that if the (r,z) point doesn't
c       fall within any table then Br(r,z) == 0.0 and Bz(r,z) == 0.0
c
      if ( Iwhere .eq. 0 ) then
        Br = 0.0
        Bz = 0.0
      else
        if(jlow+ORDER.ge.mpoints(Iwhere)) jlow=mpoints(Iwhere)-ORDER
        if(klow+ORDER.ge.npoints(Iwhere)) klow=npoints(Iwhere)-ORDER
        
        call czechsteel(jlow,klow,mpoints(Iwhere),npoints(Iwhere),
     >    Bpointer(Iwhere),ORDER,Mutable,Mcode,
     >    iwflag)
          
        do k = klow, klow + ORDER
          kk = (jlow-1) + (k-1)*mpoints(Iwhere) + Bpointer(Iwhere)
          call polint(Rtable(jlow,Iwhere),Brtable(kk),
     >      ORDER+1,r,B(k-klow+1),dBr)
          call polint(Rtable(jlow,Iwhere),Bztable(kk),
     >      ORDER+1,r,C(k-klow+1),dBz)
        enddo
        
        call polint(Ztable(klow,Iwhere),B,ORDER+1,z,Br,dBr)
        call polint(Ztable(klow,Iwhere),C,ORDER+1,z,Bz,dBz)
        
      endif
      return
      
c
c       Format and Exit codes
c
 100  format(/,1x,'Map file "', A28, '" opened in BFIELD.F',/)
 106  format(A78)
 110  format(1x,A78)
 197  continue
      write(6,*) ' BFIELD <F>: EOF in the  map_extra.txt  file'
      stop
      
 198  continue
      write(6,*) ' BFIELD <F>: Error in the map_extra.txt  file'
      stop
      
 199  continue
      write(6,*) ' BFIELD <F>: Cannot find  map_extra.txt  file'
      stop  ' PISA stopping because file not found'
      
 200  write(6,*) ' Error in opening magnet map files in BFIELD.F'
             stop
            
 201  write(6,*) ' Error 1'
             stop
 202  write(6,*) ' Error 2 in reading magnet map files in BFIELD.F'
             stop
            
 203  write(6,*) ' Error 3 in reading magnet map files in BFIELD.F'
             stop
            
 204  write(6,*) ' Error 4in reading magnet map files in BFIELD.F'
             stop
      
      write(6,*) ' E5',i,j,k,kk,Rtable(j-1,i), Ztable(k-1,i),
     1  Brtable(kk), Bztable(kk),Mutable(kk),Bpointer(i)
             stop
      
 210  write(6,*) ' BFIELD:Exceeded size: check RLIMIT,ZLIMIT,BLIMIT'
      write(6,*)   RLIMIT, ZLIMIT, BLIMIT
      write(6,*) ' Map #',i,mpoints(i), npoints(i), Bpointer(i)
      stop
      END
