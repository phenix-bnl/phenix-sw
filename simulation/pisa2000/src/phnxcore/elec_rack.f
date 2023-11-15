	subroutine elec_rack ()

c     Original Authors: Andrew Rose, Phool Chand
c     Creation date:    March 9, 1996

c     Purpose is to install electronics racks in various x,y,z with
c     different dimensions.

c     NOTE: In PISA (and PHENIX) North is +Z axis   Up vertical is +Y axis
c                                ===> West is +X axis for right hand system

c     Calling Map
c        Called by GUGEOM when 'CARR' is included on GEOP line of pisa.kumac
c        Called by CARRIAGE
c        Calls standard GEANT volume and positioning routines: GSVOLU, GSPOS,
c              GSATT

c     Variables:
c        num....int....number of boxes to place (argument)  (gone, 3/21)
c        flenme.char...file name of rack data file (argument)(gone,3/21)

c     Revision History
c        Author     Date       Comment
c        AAR,PC	     3/18      made more general, fleshed out routine
c        AAR         3/21      began transition from rack.dat to phnx.par



c=====================End Header Description===============================



c     Global variables

#include "gcunit.inc"
#include "guphnx.inc"
#include "gugeom.inc"

c     Local variables


c       declare volumes assositaed with a given quadrant (ne,nw,se,sw)
c       and magnet sections
      character tempname*4
      real thick /1.0/
      real NWA(6,10) /60*0./
      real NEA(6,10) /60*0./
      real SWA(6,10) /60*0./
      real SEA(6,10) /60*0./
      real MAG(6,12) /72*0./
      character*4 NWCH(20) /20*'####'/,
     +            NECH(20) /20*'####'/,
     +            SWCH(20) /20*'####'/,
     +            SECH(20) /20*'####'/,
     +            MACH(20) /20*'####'/
      logical nw,ne,sw,se,ma,run
      real sha(3)           !all boxes



      integer n_med/40/, i /1/,j /1/,k /1/       ! medium number, counter index


      namelist /rack_par/ NE,SE,SW,NW,MA,
     + NECH,NWCH,SWCH,SECH,MACH,
     + NEA,NWA,SWA,SEA,MAG, thick

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

      write( *,* ) 'elec_rack - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun , nml = rack_par, err = 999 )

      if (ne) then
c---*----1----*----2----*----3----*----4----*----5----*----6----7--
       i=1
       do while(NECH(i).ne.'####')
	sha(1)=nea(4,i)
	sha(2)=nea(5,i)
	sha(3)=nea(6,i)
        call gsvolu(NECH(i),'BOX ',n_med,sha,3,ivolu)
        call gspos(NECH(i),1,'HALL',nea(1,i),nea(2,i),nea(3,i),0,
     +             'ONLY')
        call gsatt(NECH(i),'SEEN',1)

        tempname='AR' // char(48+k/10)
     +                // char(48+k-10*(k/10))
	sha(1)=sha(1)-thick
	sha(2)=sha(2)-thick
	sha(3)=sha(3)-thick
        call gsvolu(tempname,'BOX ',n_med,sha,3,ivolu)
        call gspos(tempname,1,NECH(i),0.,0.,0.,0,'ONLY')
        k=k+1
        i=i+1
       end do
       i=1
      else
      endif

      if (nw) then
c---*----1----*----2----*----3----*----4----*----5----*----6----7--
       i=1
       do while(NWCH(i).ne.'####')
	sha(1)=nwa(4,i)
	sha(2)=nwa(5,i)
	sha(3)=nwa(6,i)
        call gsvolu(NWCH(i),'BOX ',n_med,sha,3,ivolu)
        call gspos(NWCH(i),1,'HALL',nwa(1,i),nwa(2,i),nwa(3,i),0,
     +             'ONLY')
        call gsatt(NWCH(i),'SEEN',1)
        tempname='AR' // char(48+k/10)
     +                // char(48+k-10*(k/10))
	sha(1)=sha(1)-thick
	sha(2)=sha(2)-thick
	sha(3)=sha(3)-thick
        call gsvolu(tempname,'BOX ',n_med,sha,3,ivolu)
        call gspos(tempname,1,NWCH(i),0.,0.,0.,0,'ONLY')
        k=k+1
        i=i+1
       end do
       i=1
      else
      endif



      if (sw) then
       do while(SWCH(i).ne.'####')
	sha(1)=swa(4,i)
	sha(2)=swa(5,i)
	sha(3)=swa(6,i)
        call gsvolu(SWCH(i),'BOX ',n_med,sha,3,ivolu)
        call gspos(SWCH(i),1,'HALL',swa(1,i),swa(2,i),swa(3,i),0,
     +             'ONLY')
        call gsatt(SWCH(i),'SEEN',1)
        tempname='AR' // char(48+k/10)
     +                // char(48+k-10*(k/10))
	sha(1)=sha(1)-thick
	sha(2)=sha(2)-thick
	sha(3)=sha(3)-thick
        call gsvolu(tempname,'BOX ',n_med,sha,3,ivolu)
        call gspos(tempname,1,SWCH(i),0.,0.,0.,0,'ONLY')
        k=k+1
        i=i+1
       end do
       i=1
      else
      endif

      if (se) then
c---*----1----*----2----*----3----*----4----*----5----*----6----7--
       i=1
       do while(SECH(i).ne.'####')
	sha(1)=sea(4,i)
	sha(2)=sea(5,i)
	sha(3)=sea(6,i)
        call gsvolu(SECH(i),'BOX ',n_med,sha,3,ivolu)
        call gspos(SECH(i),1,'HALL',sea(1,i),sea(2,i),sea(3,i),0,
     +             'ONLY')
        call gsatt(SECH(i),'SEEN',1)
        tempname='AR' // char(48+k/10)
     +                // char(48+k-10*(k/10))
	sha(1)=sha(1)-thick
	sha(2)=sha(2)-thick
	sha(3)=sha(3)-thick
        call gsvolu(tempname,'BOX ',n_med,sha,3,ivolu)
        call gspos(tempname,1,SECH(i),0.,0.,0.,0,'ONLY')
        k=k+1
        i=i+1
       end do
       i=1
      else
      endif

      if (ma) then
       i=1
       do while(MACH(i).ne.'####')
	sha(1)=mag(4,i)
	sha(2)=mag(5,i)
	sha(3)=mag(6,i)
        call gsvolu(MACH(i),'BOX ',n_med,sha,3,ivolu)
        call gspos(MACH(i),1,'HALL',mag(1,i),mag(2,i),mag(3,i),0,
     +             'ONLY')
        call gsatt(MACH(i),'SEEN',1)
        tempname='AR' // char(48+k/10)
     +                // char(48+k-10*(k/10))
	sha(1)=sha(1)-thick
	sha(2)=sha(2)-thick
	sha(3)=sha(3)-thick
        call gsvolu(tempname,'BOX ',n_med,sha,3,ivolu)
        call gspos(tempname,1,MACH(i),0.,0.,0.,0,'ONLY')
        k=k+1
        i=i+1
       end do
       i=1
      else
      endif


      return


c---------------------------------------------------------------------
999   continue
      write(6,1000)
1000  format(/,3x,'Read error in rack_par segment of phnx.par'/,3x,
     1  '  Namelist mis-match in rack_par segment of phnx.par ?',//,3x,
     2  'The PHNX.PAR file will be re-read to pinpoint the erroneous',
     3  ' line',/,3x,'****This will cause the program to crash.****',//)
      write(6,nml=rack_par)

      rewind( itf_lun )
      read( itf_lun, nml = rack_par )
      stop ' CARRIAGE.F <E> PISA stop because of PHNX.PAR file error.'

	end
