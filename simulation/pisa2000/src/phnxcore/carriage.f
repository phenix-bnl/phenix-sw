      subroutine carriage
      implicit none

c     Original Authors: Andrew Rose, Phool Chand, and Charles F. Maguire
c     Creation date:    February 29, 1996

c     Purpose is to install the central arm detectors' carriage structure as
c     parameterized in the phnx.par file
c     NOTE: In PISA (and PHENIX) North is +Z axis   Up vertical is +Y axis
c                                ===> West is +X axis for right hand system

c     Calling Map
c        Called by GUGEOM when 'CARR' is included on GEOP line of pisa.kumac
c        Calls standard GEANT volume and positioning routines: GSVOLU, GSPOS,
c              GSATT

c     Revision History
c        Author     Date       Comment


c=====================End Header Description===============================


c     Global variables

#include "gcunit.inc"
#include "guphnx.inc"
#include "gugeom.inc"

c     Local variables

      integer ivolu       !vol#
      real carpar(5),crupar(11), capart(5)
      real pi      ! shape parameters for GEANT call

c     parameters on namelist  (lengths in cm, angles in degrees)

      real rmin  /510./    ! inner radius
      real rmax  /590./    ! outer radius
      real dz    /20.0/    ! half-length in Z
      real zpos  /280./    ! position (North side)
      real phi1  /-37.5/   ! bottom azimuth
      real phi2  /60.0/    ! top azimuth
      integer n_med /46/   ! tracking medium (low threshold, non-magnetic)
      real thick /4.5/     ! wall thickness

      real uxpos,uypos,uzpos, the1, ph1, alph1, alph2
      real udz /25.625/
      real udx1 /43.4/
      real udx2 /43.4/
      real udy1 /38.35/
      real udy2 /11.455/

      real cbasep(3),cbdx,cbdy,cbdz   !values for the arc base
      real cbx,cby,cbz

      real crplate(3)			!values for plate, derive from others

      namelist /car_par/ rmin,rmax,dz,phi1,phi2,n_med,zpos,
     +   udz,udx1,udx2,udy1,udy2,uxpos,uypos,uzpos,the1, 
     +   ph1, alph1, alph2,cbdx,cbdy,cbdz,cbx,cby,cbz, thick
      
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

c     Begin execution
      pi=4*atan(1.)

c     Get geometry

      write( *,* ) 'carriage - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = car_par, err = 999 )

c     parameters for base plate, derive width from z position and width of arms,
c	derive dx from dx of arm support, define standard thickness
      crplate(3)=(cbdz+cbz+3.683)	!3. is the offset in drawing
      crplate(2)=21.59			!Y from drawing
      crplate(1)=cbdx

c     parameters for carriage arc
      carpar(1)=rmin
      carpar(2)=rmax
      carpar(3)=dz
      carpar(4)=phi1
      carpar(5)=phi2
      capart(1)=rmin+thick
      capart(2)=rmax-thick
      capart(3)=dz-thick
      capart(4)=phi1
      capart(5)=phi2


cthese are the values for the triangular piece extending towards the magnet.

      crupar(1)=udz
      crupar(2)=THE1       !Theta
      crupar(3)=PH1
      crupar(4)=udy1
      crupar(5)=udx1
      crupar(6)=udx2
      crupar(7)=-1*(pi/2-atan2(2*udy1,udx1))*360/(2*pi)
      crupar(8)=udy2
      crupar(9)=udx1
      crupar(10)=udx2
      crupar(11)=-1*(pi/2-atan2(2*udy1,udx1))*360/(2*pi)


cthese are the values for the box at the base of the carriage.


      cbasep(1)=cbdx
      cbasep(2)=cbdy
      cbasep(3)=cbdz

c	Define side base support (plate)

      call gsvolu('CRSB','BOX ',n_med,crplate,3,ivolu)

      crplate(3)=(cbdz+cbz+3.683)-thick	!set thickness with air volume
      crplate(2)=21.59-thick			!Y from drawing
      crplate(1)=cbdx-thick
      call gsvolu('AIR1','BOX ',15,crplate,3,ivol)
      call gspos('AIR1',1,'CRSB',0.,0.,0.,0,'ONLY')

      call gspos('CRSB',1,'HALL',cbx,cby-cbdy-crplate(2),
     +    0.0,0,'ONLY')
      call gspos('CRSB',2,'HALL',-cbx,cby-cbdy-crplate(2),
     +    0.0,0,'ONLY')

c      Define first side of carriage arc (WEST since +x is on WEST side)

      call gsvolu('CAR1','TUBS',n_med,carpar,5,ivolu)
      call gsvolu('AIR2','TUBS',15,capart,5,ivolu)
      call gspos('AIR2',1,'CAR1',0.,0.,0.,0,'ONLY')
      call gspos('CAR1',1,'EMCL',0.0,0.0,zpos,0,'ONLY')
      call gsatt('CAR1','SEEN',1)
      call gspos('CAR1',2,'EMCL',0.0,0.0,-zpos,0,'ONLY')


c'triangle' west then east
      call gsvolu('CR11','TRAP',n_med,crupar,11,ivolu)
      call gspos('CR11',1,'HALL',uxpos,uypos,uzpos,0,'ONLY')
      call gspos('CR11',2,'HALL',uxpos,uypos,-uzpos,0,'ONLY')
      call gsatt('CR11','SEEN',1)

      crupar(7)=-1*(pi/2-atan2(2*udy1,-udx1))*360/(2*pi)
      crupar(11)=crupar(7)
      call gsvolu('CR12','TRAP',n_med,crupar,11,ivolu)
      call gspos('CR12',1,'HALL',-uxpos,uypos,uzpos,0,'ONLY')
      call gspos('CR12',2,'HALL',-uxpos,uypos,-uzpos,0,'ONLY')
      call gsatt('CR12','SEEN',1)
	

cbase of arm support  ('Arm Base')...may interesct
c first and second are west side, 3&4 are east
      call gsvolu('CRAB','BOX ',n_med,cbasep,3,ivolu)

      cbasep(1)=cbasep(1)-thick
      cbasep(2)=cbasep(2)-thick
      cbasep(3)=cbasep(3)-thick

      call gsvolu('AIR1','BOX ',15,cbasep,3,ivol)
      call gspos('AIR1',1,'CRAB',0.,0.,0.,0,'ONLY')


      call gspos('CRAB',1,'HALL',cbx,cby,cbz,0,'MANY')
      call gspos('CRAB',2,'HALL',cbx,cby,-cbz,0,'MANY')
      call gspos('CRAB',3,'HALL',-cbx,cby,cbz,0,'MANY')
      call gspos('CRAB',4,'HALL',-cbx,cby,-cbz,0,'MANY')
      call gsatt('CRAB','SEEN',1)



c      Define second side  (EAST since -x is on EAST side)

      carpar(4)= 180.0 - phi2  ! assumes phi2 is positive ( +60.0 deg default)
      carpar(5)= 180.0 - phi1  ! assumes phi1 is negative ( -37.5 deg default)

      call gsvolu('CAR2','TUBS',n_med,carpar,5,ivolu)
      call gsvolu('AIR3','TUBS',15,capart,5,ivolu)
      call gspos('AIR3',1,'CAR2',0.,0.,0.,0,'ONLY')
      call gspos('CAR2',1,'EMCL',0.0,0.0,zpos,0,'ONLY')
      call gsatt('CAR2','SEEN',1)

      call gspos('CAR2',2,'EMCL',0.0,0.0,-zpos,0,'ONLY')
      call gsatt('CAR2','SEEN',1)

      write(6,1)
 1    format(/,'  CARRIAGE <I>: Carriage volume installed',/)


c	call rack install routine

	call elec_rack()

c     return control to GUGEOM

      return


c     Error handling

999   continue
      write(6,1000)
1000  format(/,3x,'Read error in car_par segment of phnx.par'/,3x,
     1  '  Namelist mis-match in car_par segment of phnx.par ?',//,3x,
     2  'The PHNX.PAR file will be re-read to pinpoint the erroneous',
     3  ' line',/,3x,'****This will cause the program to crash.****',//)

      rewind( itf_lun )
      read( itf_lun, nml = car_par )
      stop ' CARRIAGE.F <E> PISA stop because of PHNX.PAR file error.'

      end
