C===========================================================
C
C    File name: hbd_support.f
C    Maxim Naglis, Ilia Ravinovich,  December 2009
C 
C    HBD suport structure
c
c  Jan 2012 removed uni1, see note below
C
C===========================================================
      subroutine hbd_support
      implicit none
#include "gcunit.inc"
      real sup_par(3), sup_pos(3)
      integer ivol, icall
      integer i, j
      character*1 armch(2)
      character*1 attch(2)
      character*4 volname
      Data armch /'0','1'/  !EAST - 0; WEST - 1
      Data attch /'B','T'/ !TOP - 1, BOTTOM - 0
      namelist /hbd_sup_par/ sup_par, sup_pos
      integer itf_lun             !  geometry description logical unit 
      common /interface/itf_lun
      data icall/0/
      integer nmat
C===========================================================
      if(icall.eq.0) then
         icall = 1
      else
         write (LOUT,*)' HBD support was already called before - return'
         return         ! HBD support called twice ?
      endif

      write(LOUT,*) 'HBD support - reading parameters from the par file'
      rewind(itf_lun)
      read( itf_lun, nml = hbd_sup_par, err = 999 )
C===========================================================
C 
C Below are 4 I-beams for HBD support which run parallel to the beam pipe.
C The dimensions and positions are taken from the drawing
C 'HBD Installation Assembly' provide by Rich Ruggiero.
C The top one is made out of FR4, the bottom one our from Al
C Each I-beam below is "constructed" out of 3 boxes
C
C===========================================================
      call hbd_trk_media('CF4')
      do i = 0, 1   !arm
         do j = 0, 1            !attitude
            volname='I1'//attch(j+1)//armch(i+1)
            write( *,* ) volname

            if(j.eq.0) then
               nmat = 9
            else
               nmat = 1504
            endif

            sup_par(1) = 3.0
            sup_par(2) = 0.4
            sup_par(3) = 64.8
            CALL GSVOLU(volname,'BOX ',nmat,sup_par,3,IVOL)
            
            sup_pos(1) = -50.8+i*2*50.8
            sup_pos(2) = -86.2+j*2*86.2
            sup_pos(3) = 0.0
            CALL GSPOS(volname,1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
            CALL GSATT(volname,'COLO',4)
            CALL GSATT(volname,'SEEN',1)

            volname='I2'//attch(j+1)//armch(i+1)

            sup_par(1) = 0.3
            sup_par(2) = 3.0
            sup_par(3) = 64.8
            CALL GSVOLU(volname,'BOX ',nmat,sup_par,3,IVOL)
            sup_pos(1) = -50.8+i*2*50.8
            sup_pos(2) = -(86.2+0.4+3.0)+
     & j*2*(86.2+0.4+3.0)
            sup_pos(3) = 0.0 
            CALL GSPOS(volname,1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
            CALL GSATT(volname,'COLO',4)
            CALL GSATT(volname,'SEEN',1)

            volname='I3'//attch(j+1)//armch(i+1)

            sup_par(1) = 3.0
            sup_par(2) = 0.4
            sup_par(3) = 64.8
            CALL GSVOLU(volname,'BOX ',nmat,sup_par,3,IVOL)
            sup_pos(1) = -50.8+i*2*50.8
            sup_pos(2) = -(86.2+0.4+3.0+3.0+0.4)+
     & j*2*(86.2+0.4+3.0+3.0+0.4)
            sup_pos(3) = 0.0
            CALL GSPOS(volname,1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
            CALL GSATT(volname,'COLO',4)
            CALL GSATT(volname,'SEEN',1)

         enddo
      enddo
C===========================================================
C 
C Below are 8 profiles ("extrusions") which run perpendicular to the beam pipe:
C 4 are on the top and 4 are on the bottom.
C The dimensions and positions are taken from the drawing
C 'HBD Installation Assembly' provide by Rich Ruggiero.
C All of them are made out of Al.
C
C===========================================================
      nmat = 9
C===========================================================
      sup_par(1) = 63.9
      sup_par(2) = 0.075
      sup_par(3) = 1.91
C===========================================================
      CALL GSVOLU('EXT1','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = 85.5
      sup_pos(3) = 52.1
      CALL GSPOS('EXT1',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT1','COLO',4)
      CALL GSATT('EXT1','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT2','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = 85.5
      sup_pos(3) = 28.0
      CALL GSPOS('EXT2',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT2','COLO',4)
      CALL GSATT('EXT2','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT3','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = 85.5
      sup_pos(3) = -28.0
      CALL GSPOS('EXT3',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT3','COLO',4)
      CALL GSATT('EXT3','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT4','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = 85.5
      sup_pos(3) = -52.1
      CALL GSPOS('EXT4',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT4','COLO',4)
      CALL GSATT('EXT4','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT5','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = -85.5
      sup_pos(3) = 52.1
      CALL GSPOS('EXT5',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT5','COLO',4)
      CALL GSATT('EXT5','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT6','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = -85.5
      sup_pos(3) = 28.0
      CALL GSPOS('EXT6',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT6','COLO',4)
      CALL GSATT('EXT6','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT7','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = -85.5
      sup_pos(3) = -28.0
      CALL GSPOS('EXT7',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT7','COLO',4)
      CALL GSATT('EXT7','SEEN',1)
C===========================================================
      CALL GSVOLU('EXT8','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = -85.5
      sup_pos(3) = -52.1
      CALL GSPOS('EXT8',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('EXT8','COLO',4)
      CALL GSATT('EXT8','SEEN',1)
C===========================================================
C 
C Below is unistrut (FR4) which was added by Rob Pisani for
C the gas lines as a strain relief on the top. There is a short one 
C at the bottom also.
C
C===========================================================
      nmat = 1504
C===========================================================
      sup_par(1) = 63.9
      sup_par(2) = 0.80
      sup_par(3) = 2.01
C===========================================================
*      CALL GSVOLU('UNI1','BOX ',nmat,sup_par,3,IVOL)   ! This bar caused a minor volume conflict
*      sup_pos(1) = 0.0                                 ! with I1T0 and I1T1. Remove for now. Note that
*      sup_pos(2) = 85.5                                ! the top HBD support structure is not re-used 
*      sup_pos(3) = 0.0                                 ! for the F/VTX, but I leave it for now
*      CALL GSPOS('UNI1',1,'HALL',                      ! Jan 2012 HvH
*     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
*      CALL GSATT('UNI1','COLO',4)
*      CALL GSATT('UNI1','SEEN',1)
C===========================================================
C 
C Below is a complicated structure for the HBD legs.
C The dimensions and positions are taken from the drawing
C 'HBD Installation Assembly' provide by Rich Ruggiero.
C
C===========================================================
C
C First comes double flange linear bearing made out of Al.
C It is a commercial product, 80/20 PT 6826.
C
C===========================================================
      nmat = 9
C===========================================================
      sup_par(1) = 15.2
      sup_par(2) = 1.75
      sup_par(3) = 0.64
C===========================================================
      CALL GSVOLU('BE01','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 85.5
      sup_pos(3) = 30.5
      CALL GSPOS('BE01',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE01','COLO',4)
      CALL GSATT('BE01','SEEN',1)
C===========================================================
      CALL GSVOLU('BE02','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 85.5
      sup_pos(3) = 25.5
      CALL GSPOS('BE02',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE02','COLO',4)
      CALL GSATT('BE02','SEEN',1)
C===========================================================
      CALL GSVOLU('BE03','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 85.5
      sup_pos(3) = -30.5
      CALL GSPOS('BE03',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE03','COLO',4)
      CALL GSATT('BE03','SEEN',1)
C===========================================================
      CALL GSVOLU('BE04','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 85.5
      sup_pos(3) = -25.5
      CALL GSPOS('BE04',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE04','COLO',4)
      CALL GSATT('BE04','SEEN',1)
C===========================================================
      CALL GSVOLU('BE05','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -85.5
      sup_pos(3) = 30.5
      CALL GSPOS('BE05',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE05','COLO',4)
      CALL GSATT('BE05','SEEN',1)
C===========================================================
      CALL GSVOLU('BE06','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -85.5
      sup_pos(3) = 25.5
      CALL GSPOS('BE06',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE06','COLO',4)
      CALL GSATT('BE06','SEEN',1)
C===========================================================
      CALL GSVOLU('BE07','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -85.5
      sup_pos(3) = -30.5
      CALL GSPOS('BE07',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE07','COLO',4)
      CALL GSATT('BE07','SEEN',1)
C===========================================================
      CALL GSVOLU('BE08','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -85.5
      sup_pos(3) = -25.5
      CALL GSPOS('BE08',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE08','COLO',4)
      CALL GSATT('BE08','SEEN',1)
C===========================================================
      CALL GSVOLU('BE09','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 85.5
      sup_pos(3) = 30.5
      CALL GSPOS('BE09',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE09','COLO',4)
      CALL GSATT('BE09','SEEN',1)
C===========================================================
      CALL GSVOLU('BE10','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 85.5
      sup_pos(3) = 25.5
      CALL GSPOS('BE10',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE10','COLO',4)
      CALL GSATT('BE10','SEEN',1)
C===========================================================
      CALL GSVOLU('BE11','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 85.5
      sup_pos(3) = -30.5
      CALL GSPOS('BE11',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE11','COLO',4)
      CALL GSATT('BE11','SEEN',1)
C===========================================================
      CALL GSVOLU('BE12','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 85.5
      sup_pos(3) = -25.5
      CALL GSPOS('BE12',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE12','COLO',4)
      CALL GSATT('BE12','SEEN',1)
C===========================================================
      CALL GSVOLU('BE13','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -85.5
      sup_pos(3) = 30.5
      CALL GSPOS('BE13',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE13','COLO',4)
      CALL GSATT('BE13','SEEN',1)
C===========================================================
      CALL GSVOLU('BE14','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -85.5
      sup_pos(3) = 25.5
      CALL GSPOS('BE14',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE14','COLO',4)
      CALL GSATT('BE14','SEEN',1)
C===========================================================
      CALL GSVOLU('BE15','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -85.5
      sup_pos(3) = -30.5
      CALL GSPOS('BE15',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE15','COLO',4)
      CALL GSATT('BE15','SEEN',1)
C===========================================================
      CALL GSVOLU('BE16','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -85.5
      sup_pos(3) = -25.5
      CALL GSPOS('BE16',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE16','COLO',4)
      CALL GSATT('BE16','SEEN',1)
C===========================================================
      sup_par(1) = 15.2
      sup_par(2) = 0.64
      sup_par(3) = 5.08
C===========================================================
      CALL GSVOLU('BE17','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 84.8
      sup_pos(3) = 28.0
      CALL GSPOS('BE17',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE17','COLO',4)
      CALL GSATT('BE17','SEEN',1)
C===========================================================
      CALL GSVOLU('BE18','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 84.8
      sup_pos(3) = -28.0
      CALL GSPOS('BE18',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE18','COLO',4)
      CALL GSATT('BE18','SEEN',1)
C===========================================================
      CALL GSVOLU('BE19','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -84.8
      sup_pos(3) = 28.0
      CALL GSPOS('BE19',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE19','COLO',4)
      CALL GSATT('BE19','SEEN',1)
C===========================================================
      CALL GSVOLU('BE20','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -84.8
      sup_pos(3) = -28.0
      CALL GSPOS('BE20',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE20','COLO',4)
      CALL GSATT('BE20','SEEN',1)
C===========================================================
      CALL GSVOLU('BE21','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 84.8
      sup_pos(3) = 28.0
      CALL GSPOS('BE21',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE21','COLO',4)
      CALL GSATT('BE21','SEEN',1)
C===========================================================
      CALL GSVOLU('BE22','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 84.8
      sup_pos(3) = -28.0
      CALL GSPOS('BE22',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE22','COLO',4)
      CALL GSATT('BE22','SEEN',1)
C===========================================================
      CALL GSVOLU('BE23','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -84.8
      sup_pos(3) = 28.0
      CALL GSPOS('BE23',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE23','COLO',4)
      CALL GSATT('BE23','SEEN',1)
C===========================================================
      CALL GSVOLU('BE24','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -84.8
      sup_pos(3) = -28.0
      CALL GSPOS('BE24',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BE24','COLO',4)
      CALL GSATT('BE24','SEEN',1)
C===========================================================
C
C Next comes mounting plate made out of Al.
C
C===========================================================
      nmat = 9
C===========================================================
      sup_par(1) = 15.2
      sup_par(2) = 0.48
      sup_par(3) = 5.08
C===========================================================
      CALL GSVOLU('PL01','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 83.7
      sup_pos(3) = 28.0
      CALL GSPOS('PL01',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL01','COLO',4)
      CALL GSATT('PL01','SEEN',1)
C===========================================================
      CALL GSVOLU('PL02','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 83.7
      sup_pos(3) = -28.0
      CALL GSPOS('PL02',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL02','COLO',4)
      CALL GSATT('PL02','SEEN',1)
C===========================================================
      CALL GSVOLU('PL03','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -83.7
      sup_pos(3) = 28.0
      CALL GSPOS('PL03',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL03','COLO',4)
      CALL GSATT('PL03','SEEN',1)
C===========================================================
      CALL GSVOLU('PL04','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -83.7
      sup_pos(3) = -28.0
      CALL GSPOS('PL04',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL04','COLO',4)
      CALL GSATT('PL04','SEEN',1)
C===========================================================
      CALL GSVOLU('PL05','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 83.7
      sup_pos(3) = 28.0
      CALL GSPOS('PL05',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL05','COLO',4)
      CALL GSATT('PL05','SEEN',1)
C===========================================================
      CALL GSVOLU('PL06','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 83.7
      sup_pos(3) = -28.0
      CALL GSPOS('PL06',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL06','COLO',4)
      CALL GSATT('PL06','SEEN',1)
C===========================================================
      CALL GSVOLU('PL07','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -83.7
      sup_pos(3) = 28.0
      CALL GSPOS('PL07',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL07','COLO',4)
      CALL GSATT('PL07','SEEN',1)
C===========================================================
      CALL GSVOLU('PL08','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -83.7
      sup_pos(3) = -28.0
      CALL GSPOS('PL08',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL08','COLO',4)
      CALL GSATT('PL08','SEEN',1)
C===========================================================
C
C Next comes an adjustment bar made out of Al.
C
C===========================================================
      nmat = 9
C===========================================================
      sup_par(1) = 10.2
      sup_par(2) = 0.64
      sup_par(3) = 0.64
C===========================================================
      CALL GSVOLU('BA01','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 82.6
      sup_pos(3) = 32.4
      CALL GSPOS('BA01',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA01','COLO',4)
      CALL GSATT('BA01','SEEN',1)
C===========================================================
      CALL GSVOLU('BA02','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 82.6
      sup_pos(3) = 23.6
      CALL GSPOS('BA02',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA02','COLO',4)
      CALL GSATT('BA02','SEEN',1)
C===========================================================
      CALL GSVOLU('BA03','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 82.6
      sup_pos(3) = -32.4
      CALL GSPOS('BA03',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA03','COLO',4)
      CALL GSATT('BA03','SEEN',1)
C===========================================================
      CALL GSVOLU('BA04','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 82.6
      sup_pos(3) = -23.6
      CALL GSPOS('BA04',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA04','COLO',4)
      CALL GSATT('BA04','SEEN',1)
C===========================================================
      CALL GSVOLU('BA05','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -82.6
      sup_pos(3) = 32.4
      CALL GSPOS('BA05',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA05','COLO',4)
      CALL GSATT('BA05','SEEN',1)
C===========================================================
      CALL GSVOLU('BA06','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -82.6
      sup_pos(3) = 23.6
      CALL GSPOS('BA06',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA06','COLO',4)
      CALL GSATT('BA06','SEEN',1)
C===========================================================
      CALL GSVOLU('BA07','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -82.6
      sup_pos(3) = -32.4
      CALL GSPOS('BA07',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA07','COLO',4)
      CALL GSATT('BA07','SEEN',1)
C===========================================================
      CALL GSVOLU('BA08','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -82.6
      sup_pos(3) = -23.6
      CALL GSPOS('BA08',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA08','COLO',4)
      CALL GSATT('BA08','SEEN',1)
C===========================================================
      CALL GSVOLU('BA09','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 82.6
      sup_pos(3) = 32.4
      CALL GSPOS('BA09',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA09','COLO',4)
      CALL GSATT('BA09','SEEN',1)
C===========================================================
      CALL GSVOLU('BA10','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 82.6
      sup_pos(3) = 23.6
      CALL GSPOS('BA10',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA10','COLO',4)
      CALL GSATT('BA10','SEEN',1)
C===========================================================
      CALL GSVOLU('BA11','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 82.6
      sup_pos(3) = -32.4
      CALL GSPOS('BA11',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA11','COLO',4)
      CALL GSATT('BA11','SEEN',1)
C===========================================================
      CALL GSVOLU('BA12','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 82.6
      sup_pos(3) = -23.6
      CALL GSPOS('BA12',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA12','COLO',4)
      CALL GSATT('BA12','SEEN',1)
C===========================================================
      CALL GSVOLU('BA13','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -82.6
      sup_pos(3) = 32.4
      CALL GSPOS('BA13',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA13','COLO',4)
      CALL GSATT('BA13','SEEN',1)
C===========================================================
      CALL GSVOLU('BA14','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -82.6
      sup_pos(3) = 23.6
      CALL GSPOS('BA14',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA14','COLO',4)
      CALL GSATT('BA14','SEEN',1)
C===========================================================
      CALL GSVOLU('BA15','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -82.6
      sup_pos(3) = -32.4
      CALL GSPOS('BA15',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA15','COLO',4)
      CALL GSATT('BA15','SEEN',1)
C===========================================================
      CALL GSVOLU('BA16','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -82.6
      sup_pos(3) = -23.6
      CALL GSPOS('BA16',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('BA16','COLO',4)
      CALL GSATT('BA16','SEEN',1)
C===========================================================
C
C Next comes a bottom plate made out of FR4.
C
C===========================================================
      nmat = 1504
C===========================================================
      sup_par(1) = 13.4
      sup_par(2) = 0.64
      sup_par(3) = 3.18
C===========================================================
      CALL GSVOLU('PL09','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 82.6
      sup_pos(3) = 28.0
      CALL GSPOS('PL09',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL09','COLO',4)
      CALL GSATT('PL09','SEEN',1)
C===========================================================
      CALL GSVOLU('PL10','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = 82.6
      sup_pos(3) = -28.0
      CALL GSPOS('PL10',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL10','COLO',4)
      CALL GSATT('PL10','SEEN',1)
C===========================================================
      CALL GSVOLU('PL11','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -82.6
      sup_pos(3) = 28.0
      CALL GSPOS('PL11',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL11','COLO',4)
      CALL GSATT('PL11','SEEN',1)
C===========================================================
      CALL GSVOLU('PL12','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 16.5
      sup_pos(2) = -82.6
      sup_pos(3) = -28.0
      CALL GSPOS('PL12',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL12','COLO',4)
      CALL GSATT('PL12','SEEN',1)
C===========================================================
      CALL GSVOLU('PL13','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 82.6
      sup_pos(3) = 28.0
      CALL GSPOS('PL13',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL13','COLO',4)
      CALL GSATT('PL13','SEEN',1)
C===========================================================
      CALL GSVOLU('PL14','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = 82.6
      sup_pos(3) = -28.0
      CALL GSPOS('PL14',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL14','COLO',4)
      CALL GSATT('PL14','SEEN',1)
C===========================================================
      CALL GSVOLU('PL15','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -82.6
      sup_pos(3) = 28.0
      CALL GSPOS('PL15',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL15','COLO',4)
      CALL GSATT('PL15','SEEN',1)
C===========================================================
      CALL GSVOLU('PL16','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -16.5
      sup_pos(2) = -82.6
      sup_pos(3) = -28.0
      CALL GSPOS('PL16',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('PL16','COLO',4)
      CALL GSATT('PL16','SEEN',1)
C===========================================================
C
C Now let's put HV cables. What I do here I just put a box with an equivalent
C copper material.
C
C===========================================================
      nmat = 11
C===========================================================
      sup_par(1) = 1.91
      sup_par(2) = 0.005
      sup_par(3) = 64.8
C===========================================================
      CALL GSVOLU('HVC1','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 50.8
      sup_pos(2) = 93.8
      sup_pos(3) = 0.0
      CALL GSPOS('HVC1',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('HVC1','COLO',4)
      CALL GSATT('HVC1','SEEN',1)
C===========================================================
      CALL GSVOLU('HVC2','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = -50.8
      sup_pos(2) = 93.8
      sup_pos(3) = 0.0
      CALL GSPOS('HVC2',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('HVC2','COLO',4)
      CALL GSATT('HVC2','SEEN',1)
C===========================================================
      sup_par(1) = 63.9
      sup_par(2) = 0.0085
      sup_par(3) = 1.91
C===========================================================
      CALL GSVOLU('HVC3','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = 93.8
      sup_pos(3) = 52.1
      CALL GSPOS('HVC3',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('HVC3','COLO',4)
      CALL GSATT('HVC3','SEEN',1)
C===========================================================
      CALL GSVOLU('HVC4','BOX ',nmat,sup_par,3,IVOL)
      sup_pos(1) = 0.0
      sup_pos(2) = 93.8
      sup_pos(3) = -52.1
      CALL GSPOS('HVC4',1,'HALL',
     & sup_pos(1), sup_pos(2), sup_pos(3),0,'ONLY')
      CALL GSATT('HVC4','COLO',4)
      CALL GSATT('HVC4','SEEN',1)
C===========================================================
      write(LOUT,*) 'HBD support - installed'
      return
C===========================================================
 999  STOP 'HBD support - Read error in support segment of the par file'
      end
