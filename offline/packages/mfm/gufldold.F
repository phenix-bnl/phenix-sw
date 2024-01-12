*CMZ :  2.04/00 07/10/94  16.02.05  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   07/10/94
      SUBROUTINE GUFLDOLD ( XYZP, F )
c *******************************************************************
c *                                                                 *
c *   USER ROUTINE TO COMPUTE THE MAGNETIC FIELD F AT POINT XYZP    *
c *                                                                 *
c *******************************************************************
c
c   MAP:
c      Called by GEANT
c      Calls FUGAWE to find out which map field applies
c                   (presently there is only one map field for all regions)
c
c   HISTORY:
c     Original version from Nikolai Smirnov, April 6, 1992
c     Revised C. F. Maguire April 21, 1992 for PISA 1.0
c     Originally had separate maps for Central and Endcap regions
c     Also there were different grid spacings (coarse and fine)
c     Revised May 17, 1992 for single MAP file for both CENTRAL and ENDCAP
c     Retain old logic for two map files in case of future changes
c     Also retain coarse/fine grid option
c     Revised for Release 2 field grid, September 30, by cfm
c     Soren P. Sorensen offers to write continuous interpolation algorithm
c     Revised for single coil map December, 1992
c     Partial clean-up of obsolete logic for PISA 2.02 June 3, 1993
c
c     October, 1994: Subroutine is essentially obsolete because of the
c                    new magnetic field map formats since June, 1994.
c                    Subroutine is being kept for backward compatibility
c                    with old field map formats.
c
 
      implicit none

#include "mfm.inc"
 
c     input variable    xyzp(3)  giving (x,y,z) coordinate
      real   xyzp(3)
      
c     output variable      f(3)  giving bx, by, and bz magnetic field values
      real f(3)
 
c     local variables
      logical nofield, no_efield, no_cfield   ! which regions are active
      logical lcent, lendf, lendw             ! which region of space
      logical logopn                          ! status of field map file
 
      integer nrendf, nzendf, nrcent, nzcent, nrendw, nzendw

c     map file goes from 0 to 730 cm in z with 10 cm spacing
c     map file goes from 0 to 510 cm in r with 10 cm spacing
      parameter ( nrendf=52, nzendf=74 )  ! endcap region
      parameter ( nrcent=52, nzcent=74 )  ! central region

c     retain wide (coarse) grid logic for now
      parameter ( nrendw=1, nzendw=1 ) ! reserved for future changes

      real bfldc(nrcent,nzcent,2), geomprc(nrcent), geompzc(nzcent)
      real bfldef(nrendf,nzendf,2), geompref(nrendf), geompzef(nzendf)
      real buff(4)      ! buffer array for field map read

      integer nr, nz, j1, j2, k1, k2, j, k, ll, iwhere
      real  r, z, ffz, ffr, bfav1, bfav2, bz, br
 
      real cent_fact, end_fact      ! renormalization of field strength
      real rmax_cen /1.0e+30/       ! cut-off point for central field
 
      integer linfst / 0/           ! first pass on field read switch

c     ifile controls whether to use the december 1992 or the june 1993 maps
      integer ifile /0/             ! map file (0 = june 1993, 1= dec 1992)
 
      data nofield/.false./         ! no field set
      data lcent, lendf, lendw /3*.false./
      save linfst, bfldc, geomprc, geompzc, j1, j2, k1, k2
      save bfldef, geompref, geompzef
      save end_fact, cent_fact, rmax_cen
      save nofield, no_efield, no_cfield

      character*4 volume11, volume12

c     begin execution
c     default is zero field, always set when gufld is called
      call vzero( f, 3 )

c     check if beyond cut-off radius or nofield switch is set
      r = sqrt( xyzp(1)*xyzp(1) + xyzp(2)*xyzp(2) )
      if(nofield.or.r.gt.rmax_cen)return

      volume11 = cpvolu_opt(1,1)
      call cltou(volume11)

      volume11 = cpvolu_opt(1,2)
      call cltou(volume12)

c     initialize by reading the map files as requested
      if ( linfst .eq. 0 ) then     ! 0 passes so far
 
c     check if endcap field is requested
         if( volume12.eq.'VOLD'.or.volume12.eq.'OLD' )then
c
c     ENDCAP REQUESTED
c
            no_efield = .false.
            if( volume12.eq.'OLD' )then
               ifile = 0
            else
               ifile = 1
            endif
            end_fact = rpvolu_opt(2,2) ! endcap multiplication factor
            write(6,789)end_fact
789   format('gufldold - Endcap Magnetic Field scale factor = ',
     1           g10.3,11x,'(normal value is 1.00)',//)
         else
c
c     ENDCAP OMITTED
c
            no_efield = .true.
            write(6,888)
888   format('gufldold - No Endcap Magnet Field requested',//)
         endif   ! check on whether endcap has been requested
 
         if( volume11.eq.'VOLD'.or.volume11.eq.'OLD' ) then
c
c     CENTRAL REQUESTED
c
            if( volume11.eq.'OLD' )then
               ifile = 0
            else
               ifile = 1
            endif
            no_cfield = .false.
            cent_fact = rpvolu_opt(2,1)   ! central multiplication factor
            write(6,787)cent_fact
787   format('gufldold - Central Magnetic Field scale factor = ',
     1           g10.3,11x,'(normal value is 1.00)',//)
            if(rpvolu_opt(3,1).ne.0.0)then
               rmax_cen = rpvolu_opt(3,1)
               write(6,'(/,a,f6.1,/)') '  Central field cut-off R = ',
     1                                 rmax_cen
            endif
         else
c
c     CENTRAL OMITTED
c
            no_cfield = .true.
            write(6,886)
886   format('gufldold - No Central Magnet Field requested',//)
         endif   ! check on central field request
 
         if(ifile.eq.1)
     1   write(6,'(/,a,a,/)')' This version of GUFLD uses obsolete',
     2                ' single coil map file map_both_june_93.dat'
         if(ifile.eq.0)
     1   write(6,'(/,a,a,/)')' This version of GUFLD use obsolete',
     2        ' single coil map file map_both_august_94.dat'
 
         nofield = .false.
         if(no_efield.and.no_cfield)then
            write(6,882)
882   format('gufldold - Neither the ENDCAP nor the CENTRAL',
     1        ' magnetic field maps were requested.',/,13x,
     2        'Straight line tracking (infinite momentum',
     3        ' approximation) will occur)',//)
            nofield = .true.
            return
         endif         ! check on no requested field
 
         if(cent_fact.eq.0.0.and.end_fact.eq.0.0)then
            write(6,881)
881   format('gufldold - ',
     1      'Both map table multiplicative factors are zero,',/,2x,
     2      'therefore no magnetic field will be calculated',//)
            nofield = .true.
         endif
         if(nofield)return
 
         logopn = .false.
C
C     Start input central B field map file.
C
         if(no_cfield)go to 100
         nz = nzcent
         nr = nrcent
         J1 = NZ
         J2 = J1 - 1
         K1 = NR
         K2 = NR - 1
c
c     open the map file (only on initial call), file number 15
c     should be a readonly file
c
         if(ifile.eq.1)then
            open(unit=15,file='map_both_june_93.dat',form='formatted',
     1           access='sequential',err=999,status='old')
         else
         open(unit=15,file='map_both_august_94.dat',form='formatted',
     1           access='sequential',err=999,status='old')
         endif
         write(6,897)
897   format('gufldold - Map file successfully opened.')
         logopn = .true.
 
         LINFST = 1           ! Set first pass switch
         DO 17 J = 1, NZ
            DO 16 K = 1, NR
               READ (UNIT=15, FMT=91,END=95,ERR=99)
     1              (BUFF(LL),LL=1,4)
91    FORMAT(4F13.1)
c
c   Take R value only on the first pass (LINFST = 1)
c
               IF(LINFST.EQ.1)GEOMPRC(K) = BUFF(1)
               BFLDC(K,J,1) = BUFF(3)*0.001    ! Convert to kG (for GEANT)
               BFLDC(K,J,2) = BUFF(4)*0.001    ! Convert to kG (for GEANT)
16          CONTINUE
            LINFST = 2                  ! Set second pass switch
            GEOMPZC(J) = BUFF(2)        ! Z values in outer loop
17       CONTINUE
         GO TO 20
 
c
c    Error condition on read of field map
c
95       print *,'gufldold - early input end in magnet field file'
         print *,'gufldold - Inconsistency in map file grid ?'
         stop '  PISA progam stopped during central field read'
 
20       continue   ! branch point of field map read was successful
         close (unit=15)
 
100      continue   ! branch point if central field map read omitted
         if(no_efield)go to 200
c
c     start read of endcap b field file
c
         linfst = 1           ! set first pass switch
         nz = nzendf          ! fine grid here
         nr = nrendf          ! fine grid here
         j1 = nz
         j2 = j1 - 1
         k1 = nr
         k2 = nr - 1
c
c     open the map file (only on initial call), file number 15
c     should be a readonly file
c
         if(ifile.eq.1)then
            open(unit=15,file='map_both_june_93.dat',form='formatted',
     1           access='sequential',err=999,status='old')
         else
         open(unit=15,file='map_both_august_94.dat',form='formatted',
     1           access='sequential',err=999,status='old')
         endif
         if(.not.logopn)write(6,96)
96    format('map file  map_both.dat  successfully opened in',
     1         ' gufld.for')
 
         do 117 j = 1, nz
            do 116 k = 1, nr
               read (unit=15,fmt=91,end=98,err=99) (buff(ll),ll=1,4)
               if(linfst.eq.1)geompref( k) = buff(1) ! radial coordinate
               bfldef(k,j,1) = buff(3)*0.001  ! gauss to kg (for geant)
               bfldef(k,j,2) = buff(4)*0.001  ! gauss to kg (for geant)
116         continue
            linfst = 2                  ! second pass switch
            geompzef(j) = buff(2)       ! z coordinate (fine)
117      continue
         go to 200
 
98       print *,'gufldold - early input end in magnet field file'
         print *,'gufldold - inconsistency in map file grid ?'
         stop '  pisa progam stopped in endcap field read'
 
200      continue
c
c     successful read of endcap (fine grid) map
c
         close (unit=15)
      endif      ! check on initial call  (linfst = 0)
 
c
c     now calculate the field by linear interpolation
c
      r = sqrt( xyzp(1)*xyzp(1) + xyzp(2)*xyzp(2) )
      z = abs( xyzp(3))
c
c     now find which magnetic map applies
c
      iwhere = 0
      call fugawe(r,z,iwhere)
      go to (300,400,500,600) , iabs(iwhere)
 
      print *,'gufldold - illegal iwhere value = ',iwhere
      write(6,299)linfst,r,z,xyzp
299   format(3x,'linfst ',i3,'  r = ',e12.5,3x,'z = ',e13.5,
     1       'xyzp ',3e14.5)
      write(6,*) 'gufldold - the fugawe subroutine did not work.'
      return    ! field was set to 0.0 above
 
300   continue
c
c     in central region
c
      lcent = .true.
      lendf = .false.
      lendw = .false.
      if(iwhere.lt.0)return
      nz = nzcent
      nr = nrcent
c
c     check, may be we are still into the same geom. cell (central field)
c     should add a logical check here as well (cfm)
c
      if(lcent.and.z.ge.geompzc(j1).and.z.lt.geompzc(j2)) go to 30
c
c     let find the geom. cell in z- direction
c
      do 27 j= 2, nz
        j1 = j
        if ( z .ge. geompzc(j) ) go to 28
27    continue
28    j2 = j1 - 1
30    continue   ! branch point for being in the same z cell as last call
c
c     may be we are still into the same geom. cell in r- direction
c
      if(lcent.and.r.ge.geomprc(k1).and.r.lt.geomprc(k2)) go to 40
c
c     let find the geom cell in r- direction
c
      do 37 k = 2, nr
         k1 = k
         if ( r .ge. geomprc(k) ) go to 38
37    continue
38    k2 = k1 - 1
40    continue   ! branch point for being in the same r cell as last call
 
 
      ffz = ( z - geompzc(j2)) / ( geompzc(j1) - geompzc(j2) )
      ffr = ( r - geomprc(k2)) / ( geomprc(k1) - geomprc(k2) )
      bfav1 = (bfldc(k1,j1,2) - bfldc(k1,j2,2))*ffz + bfldc(k1,j2,2)
      bfav2 = (bfldc(k2,j1,2) - bfldc(k2,j2,2))*ffz + bfldc(k2,j2,2)
      bz = (bfav1 - bfav2)*ffr + bfav2
      bfav1 = (bfldc(k1,j1,1) - bfldc(k1,j2,1))*ffr + bfldc(k1,j2,1)
      bfav2 = (bfldc(k2,j1,1) - bfldc(k2,j2,1))*ffr + bfldc(k2,j2,1)
      br = (bfav1 - bfav2)*ffz + bfav2
c
c     now resolve the br field into bx and by components
c
      if(r.ne.0.0)then
c
c     negative z side has opposite sign radial component (central field)
c
         if(xyzp(3).lt.0.0)br = -br
         f(1) = cent_fact*br*(xyzp(1)/r)
         f(2) = cent_fact*br*(xyzp(2)/r)
      else
c
c     what are we doing along the beam axis?  set radial field to 0 anyway.
c
         f(1) = 0.0
         f(2) = 0.0
      endif
      f(3) = cent_fact*bz
      return         ! finished with point in central field
 
400   continue
c
c     branch here if in fine grid endcap region
c
      lcent = .false.
      lendf = .true.
      lendw = .false.
      if(iwhere.lt.0)return
      nz = nzendf
      nr = nrendf
c
c     check, may be we are still into the same geom. cell
c
      if(lendf.and.z.ge.geompzef(j1).and.z.lt.geompzef(j2)) go to 430
c
c     let find the geom. cell in z- direction
c
      do 427 j= 2, nz
         j1 = j
         if ( z .ge. geompzef(j) ) go to 428
427   continue
428   j2 = j1 - 1       ! orig. j2 = j1 +1 (corrected by cfm)
430   continue   ! branch point for being in the same z cell as last call
c
c     may be we are still into the same geom. cell in r- direction
c
      if(lendf.and.r.ge.geompref(k1).and.r.lt.geompref(k2)) goto 450
c
c     let find the geom cell in r- direction
c
      do 437 k = 2, nr
         k1 = k
         if ( r .ge. geompref(k) ) go to 438
437   continue
438   k2 = k1 - 1       ! orig. k2 = k1 +1 (corrected by cfm)
450   continue   ! branch point for being in the same r ceall as last call
 
      ffz = ( z - geompzef(j2)) / ( geompzef(j1) - geompzef(j2) )
      ffr = ( r - geompref(k2)) / ( geompref(k1) - geompref(k2) )
      bfav1 = (bfldef(k1,j1,2) - bfldef(k1,j2,2))*ffz +
     1         bfldef(k1,j2,2)
      bfav2 = (bfldef(k2,j1,2) - bfldef(k2,j2,2))*ffz +
     1         bfldef(k2,j2,2)
      bz = (bfav1 - bfav2)*ffr + bfav2
      bfav1 = (bfldef(k1,j1,1) - bfldef(k1,j2,1))*ffr +
     1         bfldef(k1,j2,1)
      bfav2 = (bfldef(k2,j1,1) - bfldef(k2,j2,1))*ffr +
     1         bfldef(k2,j2,1)
      br = (bfav1 - bfav2)*ffz + bfav2
c
c     now split br into bx and by components
c
      if(r.ne.0.0)then
         f(1) = end_fact*br*(xyzp(1)/r)
         f(2) = end_fact*br*(xyzp(2)/r)
      else
c
c     what are we doing along the beam axis?  set radial field to 0 anyway.
c
         f(1) = 0.0
         f(2) = 0.0
      endif
      f(3) = end_fact*bz
      return
 
500   continue
      lcent = .false.
      lendf = .false.
      lendw = .true.
      if(iwhere.lt.0)return
      return      ! no wide grid region logic yet; default field is 0
 
600   continue
      if(iwhere.lt.0)return
      return      ! now in muon id region (should not be magnetic)
 
99     print *, 'gufldold - read error for mgn field input file '
       stop  '  pisa program stopped with error detected in gufld.for'
999    continue
       write(6,998)
998    format('gufldold - unable to open the common magnetic',
     1   ' map file')
       stop 'gufldold - check if map file exists'
       end
