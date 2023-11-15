c $Id: dcgeom96.f,v 1.6 2008/05/21 08:21:58 hpereira Exp $

      subroutine dcgeom96(nmdcch,jflbnk)


c*****************************************************************
c**** This version if for using within Interactive GEANT !!!!
c*****************************************************************

c *********************************************************************

C This routine defines the PISA drift chamber geometry.
c The geometry contains separate volumes for each cell and
c for each plane in each cell.  All planes are identical.
c There are 80 cells per arm and 40 planes per cell.
c The volume numbering follows the PHENIX standard convention.
c Planes #1-12 and 31-42 are X planes.
c Planes #13-16 and 33-36 are U planes.
c Planes #17-20 and 37-40 are V planes.

c Revision History:

c     05/05/00    Baublis           Correct strut position, mylar thickness,
c                                   U&V volumes twist signs, wires radii and more
c     02/15/97    Maguire           Add date keys to lfi_para bank (DC and PC1)
c     09/10/97    Baublis           New design of U&V wire supports and other
c     10/10/96    Maguire           ZEBRA problem with DCGP array; new offset
c     10/1/96     Mitchell          General clean-up.  Parameter expansion.
c     8/13/96     Mitchell          Added X, UV wire supports on ends in z
c     12/13/95    J.T. Mitchell     Rewritten for final DC design
c     7/19/94     N. Smirnoff       Original

c *********************************************************************

      IMPLICIT NONE
#include "g77trigdef.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      integer*4 nmdcch,jflbnk
      character*10 chform
      logical logdbg
      integer iod,iqindex

c ********************* INTERNAL VARIABLES for Sensitive Volumes **********

      integer i,ii,j,k,kk,l,ll
      integer icell,iplane,iwire
      integer ikey,igroup,jplane
      real test1,test2,pdiff,uvang,dext1,dext2,rtemp
      real par(11),par2(12)
      integer ierror,itest

      real dphicellw,dphicelle,dphigusw,dphiguse
      real rcenter,cellphi,deltaphi,r0
      real cellphi1,cellphi2
      real xcenter,ycenter,zcenter
      real phi2,r1,r2,ri,dyi
      real phii,xi,yi,zi,twist
      real rc,deltas,deltax,deltay,phiuv,thetauv
      integer il
      real plength,pl,yl,xl,deltap
      real dr_adj

      real z1_tan, z2_tan,tan_thet



c ********************* Rotation Matrix number arrays *******************

      integer irttr(160)
      integer irotwininw(22),irotwinine(22)
      integer irotwinoutw(10),irotwinoute(10)
      integer irotgusw(23),irotguse(23),irotend(4)
      integer irotslote(161),irotslotw(161)
      integer irotsuppe(160),irotsuppw(160)
      integer irotslote_uv(161),irotslotw_uv(161)
      integer irotsuppe_uv(160),irotsuppw_uv(160)

c ********************* INTERNAL VARIABLES for Support Volumes **********

      integer igus,igus2,iwin
      real dcsthick,dcsthick2   ! support structure thickness
      real gusr,gusphi,gusphi2   ! for gusset definition
      real gusx,gusy,gusz
      real z_slot,dx_shift,gusphi_0,uv_phi_adj
      integer idcbe,idcbs
      real endz,endr,endphi
      real endx,endy,endx0,endy0
      real deltar,maxht   ! for UV support definition
      real tan_thet1z
      real dphiwinin, dphiwinout
      real dphistrut,phistrut_w,phistrut_e

c ****************** Material definition variables *********************

      real amylar(3),zmylar(3),wmylar(3)   ! for mylar
      real achgas(3),zchgas(3),wchgas(3)   ! for DC gas
      real rcer,acer(2),zcer(2),wcer(2)   ! for ceramic

      data amylar /12.01,1.01,15.99/
      data zmylar /6.0,1.0,8.0/
      data wmylar /5.0,4.0,2.0/
      data achgas /39.95,12.01,1.01/
      data zchgas /18.0,6.0,1.0/
      data wchgas /0.5,0.125,0.375/
      data rcer /3.94/
      data acer /27.0,16.0/
      data zcer /13.0,8.0/
      data wcer /2.0,3.0/

c **************** Magnetic field definition variables ****************

      real fieldm,tmaxfd,dmaxms,deemax,epsil,stmin
      integer ifield,ifld

c ****************** User input parameters *****************************
c See the WWW documentation for complete explanations of each variable.

      real dc_suppzlength
      real dc_inradius
      real dc_outradius
      real dc_phibotw
      real dc_phitopw
      real dc_phibote
      real dc_phitope

      integer dc_ncells
      real dc_rplane(40)
      real dc_planethick
      real dc_uvangle

      integer dc_ti_switch
      real dc_winthickin
      real dc_winthickout
      real dc_supptiside
      real dc_suppalside
      real dc_suppzthick
      real dc_supptibase
      real dc_suppalbase

      integer dc_nwinin
      integer dc_nwinout
      real dc_suppwinz
      real dc_suppwinx

      integer dc_ngusset
      real dc_x1baserad
      real dc_x2baserad
      real dc_x1basez
      real dc_x2basez
      real dc_x1slotthick
      real dc_x2slotthick
      real dc_x1slotz
      real dc_x2slotz
      real dc_x1suppthick
      real dc_x2suppthick
      real dc_x1suppz
      real dc_x2suppz
      real dc_x1rextent
      real dc_x2rextent

      real dc_u1rextent
      real dc_u1baserad
      real dc_u1basez
      real dc_u1slotthick
      real dc_u1slotz
      real dc_u1suppthick
      real dc_u1suppz

      real dc_v1rextent
      real dc_v1baserad
      real dc_v1basez  
      real dc_v1slotthick
      real dc_v1slotz
      real dc_v1suppthick
      real dc_v1suppz

      real dc_u2rextent
      real dc_u2baserad
      real dc_u2basez
      real dc_u2slotthick
      real dc_u2slotz
      real dc_u2suppthick
      real dc_u2suppz

      real dc_v2rextent
      real dc_v2baserad
      real dc_v2basez  
      real dc_v2slotthick
      real dc_v2slotz
      real dc_v2suppthick
      real dc_v2suppz

      real thet_slotu1
      real thet_slotv1
      real thet_slotu2
      real thet_slotv2

      real dc_cfibinrad
      real dc_cfiboutrad 
      real dc_cfibposrad 
      real dc_cfibdist2c 
 


      namelist /dc_it96_par/ dc_suppzlength, dc_inradius, dc_outradius,
     +     dc_phibotw, dc_phitopw, dc_phibote, dc_phitope, dc_ncells,
     +     dc_rplane, dc_planethick, dc_ti_switch, 
     +     dc_winthickin, dc_winthickout, dc_supptiside, 
     +     dc_suppalside, dc_suppzthick,
     +     dc_supptibase, dc_suppalbase, dc_ngusset, 
     +     dc_nwinin,dc_nwinout,dc_suppwinz,dc_suppwinx,
     +     dc_x1baserad, dc_x2baserad, dc_x1basez, dc_x2basez, 
     +     dc_x1slotthick, 
     +     dc_x2slotthick, dc_x1slotz, dc_x2slotz, dc_x1suppthick,
     +     dc_x2suppthick, dc_x1suppz, dc_x2suppz, dc_x1rextent, 
     +     dc_x2rextent,
     +     dc_u1rextent,dc_u1baserad,dc_u1basez,dc_u1slotthick,
     +     dc_u1slotz,dc_u1suppthick,dc_u1suppz,dc_v1rextent,
     +     dc_v1baserad,dc_v1basez,dc_v1slotthick,dc_v1slotz,
     +     dc_v1suppthick,dc_v1suppz,dc_u2rextent,dc_u2baserad,
     +     dc_u2basez,dc_u2slotthick,dc_u2slotz,dc_u2suppthick,
     +     dc_u2suppz,dc_v2rextent,dc_v2baserad,dc_v2basez,
     +     dc_v2slotthick,dc_v2slotz,dc_v2suppthick,dc_v2suppz,
     +     thet_slotu1,thet_slotv1,thet_slotu2,thet_slotv2,
     +     dc_cfibinrad,dc_cfiboutrad
c!!! +     ,dc_cfibposrad,dc_cfibdist2c

c ****************** User input default values *****************************
c See the WWW documentation for complete explanations of each variable.
c These are the default values implemented in phnx.par.

c      data dc_suppzlength /180.0/
c      data dc_inradius /202.0/
c      data dc_outradius /246.0/
c      data dc_phibotw /-33.75/
c      data dc_phitopw /56.25/
c      data dc_phibote /213.75/
c      data dc_phitope /123.75/
c      data dc_ncells /80/
c      data dc_rplane /204.55,205.15,205.75,206.35,206.95,207.55,
c     +                208.15,208.75,209.35,209.95,210.55,211.15,
c     +                214.85,215.45,216.05,216.65,
c     +                220.35,220.95,221.55,222.15,
c     +                225.85,226.45,227.05,227.65,228.25,228.85,
c     +                229.45,230.05,230.65,231.25,231.85,232.45,
c     +                236.15,236.75,237.35,237.95,
c     +                241.65,242.25,242.85,243.45/

c      data dc_planethick /0.599/
c      data dc_ti_switch /1/
c      data dc_winthick /0.004/
c      data dc_supptiside /1.0/
c      data dc_suppalside /1.5/
c      data dc_suppzthick /30.0/
c      data dc_supptibase /1.5/
c      data dc_suppalbase /2.5/
c      data dc_ngusset /23/

c      data dc_nwinin /22/
c      data dc_nwinout /10/
c      data dc_suppwinz /13./
c      data dc_suppwinx /10./

c      data dc_x1baserad /1.57/
c      data dc_x2baserad /1.57/
c      data dc_x1basez /0.768/
c      data dc_x2basez /0.768/
c      data dc_x1slotthick /0.80/
c      data dc_x2slotthick /0.80/
c      data dc_x1slotz /2.33/
c      data dc_x2slotz /2.33/
c      data dc_x1suppthick /0.80/
c      data dc_x2suppthick /0.80/
c      data dc_x1suppz /4.0/
c      data dc_x2suppz /4.0/
c      data dc_x1rextent /1.2/
c      data dc_x2rextent /1.2/

c      dc_u1rextent =    1.75
c      dc_u1baserad =    1.2
c      dc_u1basez =      1.25   
c      dc_u1slotthick =  1.07
c      dc_u1slotz =      2.25   
c      dc_u1suppthick =  0.475
c      dc_u1suppz =      4.0

c      dc_v1rextent =    1.75
c      dc_v1baserad =    1.2
c      dc_v1basez =      1.3    
c      dc_v1slotthick =  1.07
c      dc_v1slotz =      2.2    
c      dc_v1suppthick =  0.475
c      dc_v1suppz =      4.0

c      dc_u2rextent =    1.75
c      dc_u2baserad =    1.2
c      dc_u2basez =      1.25   
c      dc_u2slotthick =  1.07
c      dc_u2slotz =      2.25   
c      dc_u2suppthick =  0.475
c      dc_u2suppz =      4.0

c      dc_v2rextent =    1.75
c      dc_v2baserad =    1.2
c      dc_v2basez =      1.3    
c      dc_v2slotthick =  1.07
c      dc_v2slotz =      2.2    
c      dc_v2suppthick =  0.475
c      dc_v2suppz =      4.0

c      thet_slotu1 =  5.42
c      thet_slotv1 = -thet_slotu1 
c      thet_slotu2 =  6.0
c      thet_slotv2 = -thet_slotu2

c      data dc_cfibinrad /2.54/
c      data dc_cfiboutrad /4.013/

c --- Strut location radius and displacement from the DC centre line

       data dc_cfibposrad /226.1/  ! strut location radious
       data dc_cfibdist2c /2.3/    ! distance between strut and DC center line

c ********************* Define sensitive volume names *********************

      character*4 arm1name,arm2name
      character*2 parm1name,parm2name
      character*2 name40(40)
      character*4 namec,namep
      character*1 name20(20),namee,namew,name8(8)

      data arm1name /'DCAW'/
      data arm2name /'DCAE'/
      data namee /'F'/
      data namew /'D'/
      data parm1name /'DP'/
      data parm2name /'FP'/
      data name40 /'01','02','03','04','05','06','07','08','09','10',
     +             '11','12','13','14','15','16','17','18','19','20',
     +             '21','22','23','24','25','26','27','28','29','30',
     +             '31','32','33','34','35','36','37','38','39','40'/
      data name8 /'1','2','3','4','5','6','7','8'/
      data name20 /'1','2','3','4','5','6','7','8','9','A','B','C',
     +     'D','E','F','G','H','I','J','K'/

       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

c ********************* Executable Statements ***************************

      write(6,*) 'dcgeom96 - execution starting (new version)'


c ********************* Read in the input parameters *******************

      write( *,* ) 'dcgeom96 - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = dc_it96_par, err = 666 )
      goto 42
      
 666  write(6,*) 'dcgeom96 - Error reading geometry. Exiting.'
      return
 42   continue

c ********************* Global calculated variables ********************


c dphicell is the width in phi of each cell.
      dphicellw = (dc_phitopw-dc_phibotw)/float(dc_ncells)
      dphicelle = (dc_phibote-dc_phitope)/float(dc_ncells)

c dphigus is the spacing in phi between each support gusset.
      dphigusw = (dc_phitopw-dc_phibotw)/float(dc_ngusset-3)
      dphiguse = (dc_phibote-dc_phitope)/float(dc_ngusset-3)

c dphiwinin is the spacing in phi between each inner support windows 
c               the same for both arms
      dphiwinin = (dc_phitopw-dc_phibotw+dphigusw)/float(dc_nwinin-1)

c dphiwinout is the spacing in phi between each outter support windows 
c               the same for both arms
      dphiwinout = (dc_phitopw-dc_phibotw+dphigusw)/float(dc_nwinout-1)

c dphistrut is the difference in phi between strut and DC center line
      dphistrut = (dc_cfibdist2c/dc_cfibposrad)*raddeg

c phistrut_w is the phi angle of the supporting strut in West arm
c        cellphi = dc_phibotw+dphicellw/4.+(icell-1)*dphicellw
      phistrut_w = dc_phibotw+dphicellw/4.+(39+0.5)*dphicellw
     *             -dphistrut

c phistrut_e is the phi angle of the supporting strut in East arm
c        cellphi = dc_phitope+3.*dphicelle/4.+(icell-1)*dphicelle
      phistrut_e = dc_phitope+3.*dphicelle/4.+(39+0.5)*dphicelle
     *             +dphistrut

c ********************* Checks on the input parameters ******************

c     Consistency checks on the global size of both arms
      if (dc_inradius.gt.dc_outradius) then
         write(6,*) 'dcgeom96 - dc_inradius > dc_outradius'
         write(6,*) 'inradius = ',dc_inradius,'outradius = ',
     +        dc_outradius
         return
      endif
      if (dc_phibotw.gt.dc_phitopw) then
         write(6,*) 'dcgeom96 - dc_phibotw > dc_phitopw'
         write(6,*) 'phibotw = ',dc_phibotw,'phitopw = ',
     +        dc_phitopw
         return
      endif
      if (dc_phitope.gt.dc_phibote) then
         write(6,*) 'dcgeom96 - dc_phitope > dc_phibote'
         write(6,*) 'phitope = ',dc_phitope,'phibote = ',
     +        dc_phibote
         return
      endif

c     Make sure that the number of cells is divisible by four
c     in order for them to be divided into keystones.
      test1 = float(dc_ncells)/4.0
      test2 = float(int(dc_ncells/4))
      if (test1.ne.test2) then
         write(6,*) 'dcgeom96 - dc_ncells not divisible by 4 '
         write(6,*) 'ncells = ',dc_ncells
         return
      endif

c     perform checks on the radii of each plane and the thickness of the
c     sensitive volumes.
      do iplane = 2,40
         if (dc_rplane(iplane).lt.dc_rplane(iplane-1)) then
            write(6,*) 'dcgeom96 - dc_rplane values out of order'
            write(6,*) 'rplane(',iplane-1,') = ',dc_rplane(iplane-1)
            write(6,*) 'rplane(',iplane,') = ',dc_rplane(iplane)
            return
         endif
         pdiff = dc_rplane(iplane)-dc_rplane(iplane-1)
         if (pdiff.lt.dc_planethick) then
            write(6,*) 'dcgeom96 - plane thickness too large'
            write(6,*) 'rplane(',iplane-1,') = ',dc_rplane(iplane-1)
            write(6,*) 'rplane(',iplane,') = ',dc_rplane(iplane)
            write(6,*) 'planethick = ',dc_planethick
            write(6,*) 'separation = ',pdiff
            return
         endif
      enddo

      if (dc_ti_switch.ne.0) dc_ti_switch=1   ! make binary


c     Check for symmetry in the size of the two arms.  Only a warning.
      if (dphicellw.ne.dphicelle) then
         write(6,*) 'DCGEOM-W: west and east arms phi asymmetric'
      endif

c     Consistency check on the C fiber support paramters
      if (dc_cfibinrad.gt.dc_cfiboutrad) then
         write(6,*) 'dcgeom96 - dc_cfibinrad > dc_cfiboutrad'
         write(6,*) 'cfibinrad = ',dc_cfibinrad
         write(6,*) 'cfiboutrad = ',dc_cfiboutrad
         return
      endif

c     Check to make sure the bases of the wire supports don't run
c     into each other.
      dext1 = dc_inradius
      dext2 = dc_rplane(1) - dc_x1rextent
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - X1 support conflicts with inner window'
         write(6,*) 'x1rextent = ',dc_x1rextent
         write(6,*) 'rplane(1) = ',dc_rplane(1)
         write(6,*) 'inradius = ',dc_inradius
         return
      endif
      dext1 = dc_rplane(12)+dc_x1rextent
      rtemp = dc_rplane(13)/cosd(dc_uvangle)   ! z=0 adjustment
      dext2 = rtemp-dc_u1rextent
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - X1 support conflicts with U1 support'
         write(6,*) 'x1rextent = ',dc_x1rextent
         write(6,*) 'u1rextent = ',dc_u1rextent
         write(6,*) 'rplane(12) = ',dc_rplane(12)
         write(6,*) 'rplane(13) = ',rtemp
         return
      endif
      dext1 = dc_rplane(16)+dc_u1rextent
      dext2 = dc_rplane(17)-dc_v1rextent
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - U1 support conflicts with V1 support'
         write(6,*) 'x1rextent = ',dc_u1rextent
         write(6,*) 'u1rextent = ',dc_v1rextent
         write(6,*) 'rplane(16) = ',dc_rplane(16)
         write(6,*) 'rplane(17) = ',dc_rplane(17)
         return
      endif
      rtemp = dc_rplane(20)/cosd(dc_uvangle)   ! z=0 adjustment
      dext1 = rtemp+dc_v1rextent
      dext2 = dc_rplane(21)-dc_x2rextent
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - V1 support conflicts with X2 support'
         write(6,*) 'v1rextent = ',dc_v1rextent
         write(6,*) 'x2rextent = ',dc_x2rextent
         write(6,*) 'rplane(20) = ',rtemp
         write(6,*) 'rplane(21) = ',dc_rplane(21)
         return
      endif
      dext1 = dc_rplane(32)+dc_x2rextent
      rtemp = dc_rplane(33)/cosd(dc_uvangle)   ! z=0 adjustment
      dext2 = rtemp-dc_u2rextent
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - X2 support conflicts with U2 support'
         write(6,*) 'x2rextent = ',dc_x2rextent
         write(6,*) 'u2rextent = ',dc_u2rextent
         write(6,*) 'rplane(32) = ',dc_rplane(32)
         write(6,*) 'rplane(33) = ',rtemp
         return
      endif
      dext1 = dc_rplane(36)+dc_u2rextent
      dext2 = dc_rplane(37)-dc_v2rextent
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - U2 support conflicts with V2 support'
         write(6,*) 'u2rextent = ',dc_u2rextent
         write(6,*) 'v2rextent = ',dc_v2rextent
         write(6,*) 'rplane(36) = ',dc_rplane(36)
         write(6,*) 'rplane(37) = ',dc_rplane(37)
         return
      endif
      rtemp = dc_rplane(40)/cosd(dc_uvangle)
      dext1 = rtemp+dc_v2rextent
      dext2 = dc_outradius
      if (dext2.lt.dext1) then
         write(6,*) 'dcgeom96 - U2 support conflicts with outer window'
         write(6,*) 'u2rextent = ',dc_u2rextent
         write(6,*) 'outradius = ',dc_outradius
         write(6,*) 'rplane(40) = ',rtemp
         return
      endif

c     Check to make sure rotation matrix bounds are not exceeded
      if (dc_ncells.gt.80) then
         write(6,*) 'dcgeom96 - ncells>80.  Increase rotation'
         write(6,*) 'matrix sizes.  ncells = ',dc_ncells
         return
      endif
      if (dc_ngusset.gt.23) then
         write(6,*) 'dcgeom96 - ngusset>23.  Increase rotation'
         write(6,*) 'matrix sizes. ngusset = ',dc_ngusset
         return
      endif


c ********************* Define rotation matrices ************************

c Arm 1 (west) sensitive volume rotations.  Store in irttr.
      do icell=1,dc_ncells   ! loop over cells
         cellphi = dc_phibotw+dphicellw/4.+(icell-1)*dphicellw
         irot = irot + 1
         irttr(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irttr(icell),90.0,270.0+cellphi,90.0,
     +        phi2,0.0,360.0)
      enddo

c Arm 2 (east) sensitive volume rotations. Store in irttr.
      do icell=1,dc_ncells   ! loop over cells
         cellphi = dc_phitope+3.*dphicelle/4.+(icell-1)*dphicelle
         irot = irot + 1
         irttr(icell+dc_ncells) = irot
         phi2 = cellphi
         call gsrotm(irttr(icell+dc_ncells),90.0,cellphi-90.0,90.0,
     +        phi2,0.0,360.0)
      enddo

c Rotation matrices for DC support gussets - arm 1. Store in irotgusw.

      do icell=1,dc_ngusset
         cellphi = dc_phibotw+(icell-2)*dphigusw
         irot = irot + 1
         irotgusw(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotgusw(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

      irotgusw(1) = irotgusw(2)  ! end gussets are paralell to previous ones
      irotgusw(dc_ngusset) = irotgusw(dc_ngusset-1)

c Rotation matrices for DC support gussets - arm 2. Store in irotguse.

      do icell=1,dc_ngusset
         cellphi = dc_phitope+(icell-2)*dphiguse
         irot = irot + 1
         irotguse(icell) = irot
         phi2 = cellphi
         call gsrotm(irotguse(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

      irotguse(1) = irotguse(2)  ! end gussets are paralell to previous ones
      irotguse(dc_ngusset) = irotguse(dc_ngusset-1)


c Rot. matrices for C frame inner support windows - arm 1.Store in irotwininw.

      do icell=1,dc_nwinin
         cellphi = dc_phibotw - dphigusw/2. +(icell-1)*dphiwinin
         irot = irot + 1
         irotwininw(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotwininw(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo


c Rot. matrices for C frame outer support windows - arm 1.Store in irotwininw.

      do icell=1,dc_nwinout
         cellphi = dc_phibotw - dphigusw/2. +(icell-1)*dphiwinout
         irot = irot + 1
         irotwinoutw(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotwinoutw(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo


c Rot. matrices for C frame inner support windows - arm 2.Store in irotwinine.

      do icell=1,dc_nwinin
         cellphi = dc_phitope - dphiguse/2. +(icell-1)*dphiwinin
         irot = irot + 1
         irotwinine(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotwinine(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo


c Rot. matrices for C frame outer support windows - arm2.Store in irotwinoute.

      do icell=1,dc_nwinout
         cellphi = dc_phitope - dphiguse/2. +(icell-1)*dphiwinout
         irot = irot + 1
         irotwinoute(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotwinoute(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

c Rotation matrices for support end structures - arm 1. Store in irotend.
      cellphi = dc_phibotw ! -(dphigusw/2.0)
      irot = irot + 1
      irotend(1) = irot
      if (cellphi.ge.0.0) phi2 = cellphi
      if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
      call gsrotm(irotend(1),90.0,phi2,90.0,
     +     90.0+phi2,0.0,360.0)
      cellphi = dc_phitopw  ! +(dphigusw/2.0)
      irot = irot + 1
      irotend(2) = irot
      if (cellphi.ge.0.0) phi2 = cellphi
      if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
      call gsrotm(irotend(2),90.0,phi2,90.0,
     +     90.0+phi2,0.0,360.0)

c Rotation matrices for support end structures - arm 2. Into irotend.

      cellphi = dc_phitope ! -(dphiguse/2.0)
      irot = irot + 1
      irotend(3) = irot
      phi2 = 90.0+cellphi
      call gsrotm(irotend(3),90.0,phi2,90.0,
     +     90.0+phi2,0.0,360.0)
      cellphi = dc_phibote ! +(dphiguse/2.0)
      irot = irot + 1
      irotend(4) = irot
      phi2 = 90.0+cellphi
      call gsrotm(irotend(4),90.0,phi2,90.0,
     +     90.0+phi2,0.0,360.0)

c Rotation matrices for X1&X2 Al wire support slots - arm 1. In irotslotw.
      deltaphi = dphicellw/2.0
      do icell=1,2*dc_ncells
         cellphi = dc_phibotw+(icell-1)*deltaphi
         irot = irot + 1
         irotslotw(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotslotw(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

c Rotation matrices for X1&X2 Al wire support slots - arm 2. In irotslote.
      deltaphi = dphicelle/2.0
      do icell=1,2*dc_ncells
         cellphi = dc_phitope+(icell-1)*deltaphi
         irot = irot + 1
         irotslote(icell) = irot
         phi2 = cellphi
         call gsrotm(irotslote(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

c Rotation matrices for X1&X2 Al wire support G10 cards - arm 1. In irotsuppw.
      deltaphi = dphicellw/2.0
      do icell=1,2*dc_ncells-1
         cellphi = dc_phibotw+deltaphi/2.0+(icell-1)*deltaphi
         irot = irot + 1
         irotsuppw(icell) = irot
         if (cellphi.ge.0.0) phi2 = cellphi
         if (cellphi.lt.0.0) phi2 = 360.0 + cellphi
         call gsrotm(irotsuppw(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

c Rotation matrices for X1&X2 Al wire support G10 cards - arm 2. In irotsuppe.
      deltaphi = dphicelle/2.0
      do icell=1,2*dc_ncells-1
         cellphi = dc_phitope+deltaphi/2.0+(icell-1)*deltaphi
         irot = irot + 1
         irotsuppe(icell) = irot
         phi2 = cellphi
         call gsrotm(irotsuppe(icell),90.0,phi2,90.0,
     +        90.0+phi2,0.0,360.0)
      enddo

*   Rotation matrices for U&V Al wire support slots - arm 1. In irotslotw_uv.

      do icell=1,2*dc_ncells
         cellphi = dc_phibotw+(icell-1)*deltaphi   !  SLOT
         irot = irot + 1
         irotslotw_uv(icell) = irot
         call gsrotm(irotslotw_uv(icell),   90., cellphi-90.,
     +               0., 0.,             90., cellphi     )      
      enddo

c Rotation matrices for U&V Al wire support slots - arm 2. In irotslote_uv.
      deltaphi = dphicelle/2.0
      do icell=1,2*dc_ncells
         cellphi = dc_phitope+(icell-1)*deltaphi
         irot = irot + 1
         irotslote_uv(icell) = irot
         call gsrotm(irotslote_uv(icell),   90., cellphi-90.,
     +               0., 0.,             90., cellphi     )      
      enddo

c Rotation matrices for U&V Al wire support G10 cards - arm 1. In irotsuppw_uv.
      deltaphi = dphicellw/2.0
      do icell=1,2*dc_ncells-1
         cellphi = dc_phibotw+deltaphi/2.0+(icell-1)*deltaphi
         irot = irot + 1
         irotsuppw_uv(icell) = irot
         call gsrotm(irotsuppw_uv(icell),   90., cellphi-90.,
     +               0., 0.,             90., cellphi     )      
      enddo

c Rotation matrices for U&V Al wire support G10 cards - arm 2. In irotsuppe_uv.
      deltaphi = dphicelle/2.0
      do icell=1,2*dc_ncells-1
         cellphi = dc_phitope+deltaphi/2.0+(icell-1)*deltaphi
         irot = irot + 1
         irotsuppe_uv(icell) = irot
         call gsrotm(irotsuppe_uv(icell),   90., cellphi-90.,
     +               0., 0.,             90., cellphi     )      
      enddo


c define user materials **************************

      call gsmate(401,'Titanium$',47.88,22.0,4.54,3.56,27.1,0,0)
      call gsmate(402,'C-Epoxy$',12.2,6.13,1.68,25.0,67.0,0,0)
      call gsmate(403,'G10$',18.14,9.065,1.7,19.4,56.7,0,0)
      call gsmixt(463,' MYLAR$',amylar,zmylar,1.39,-3,wmylar)
      call gsmixt(466,'DC GAS$',achgas,zchgas,0.0010394,3,wchgas)


c *********************** define user tracking media parameters ***********

      fieldm = 4.0    ! change from 20 kg
      ifield = 1
      ifld = 0
      tmaxfd = 1.
      dmaxms = 0.5
      deemax = 0.2
      epsil = 0.01
      stmin = 0.01

      call gstmed(420,'C-Epoxy$',402, 0, ifield,
     +     fieldm,tmaxfd,dmaxms,deemax, epsil, stmin, 0, 0)
      call gstmed(465,' GAS IN DR CH 1     $', 466,  0,  ifield,
     +     fieldm,tmaxfd,dmaxms,deemax, epsil, stmin, 0, 0)
      call gstmed(480,' ACT GAS IN DR CH 1     $', 466,  1,  ifield,
     +     fieldm,tmaxfd,dmaxms,deemax, epsil, stmin, 0, 0)

      call gstmed(413,' Titanium for DC frame $',401,0,ifield,
     +     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,0,0)
      call gstmed(414,' Aluminum for DC frame $',9,0,ifield,
     +     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,0,0)
      call gstmed(416,' G10 for wire supports $',403,0,ifield,
     +     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,0,0)

      call gstmed(410,'Air $',15,0,ifield,
     +     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,0,0)
      call gstmed(415,'DC Window $',463,0,ifield,
     +     fieldm,tmaxfd,dmaxms,deemax,epsil,stmin,0,0)


c ********************* DC SUPPORT VOLUME DEFINITION ********************

c Mylar windows stretched over the inner and outer radius of each arm.
c The names for the arm 1 windows will be DWIN and DWOT.
c The names for the arm 2 windows will be EWIN and EWOT.

      par(1) = dc_inradius - dc_winthickin   ! mylar thickness defined
      par(2) = dc_inradius
      
      par(3)= dc_suppzlength/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DWIN','TUBS',415,par,5,ierror)
      call gsatt('DWIN','SEEN',1)
      call gsatt('DWIN','COLO',5)

      par(1) = dc_inradius - dc_winthickin
      par(2) = dc_inradius
      
      par(3) = dc_suppzlength/2.0
      par(4) = dc_phitope
      par(5) = dc_phibote
      call gsvolu('FWIN','TUBS',415,par,5,ierror)
      call gsatt('FWIN','SEEN',1)
      call gsatt('FWIN','COLO',5)

      par(1) = dc_outradius
      par(2) = dc_outradius + dc_winthickout
      par(3) = dc_suppzlength/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DWOT','TUBS',415,par,5,ierror)
      call gsatt('DWOT','SEEN',1)
      call gsatt('DWOT','COLO',5)

      par(1) = dc_outradius
      par(2) = dc_outradius + dc_winthickout
      par(3) = dc_suppzlength/2.0
      par(4) = dc_phitope
      par(5) = dc_phibote
      call gsvolu('FWOT','TUBS',415,par,5,ierror)
      call gsatt('FWOT','SEEN',1)
      call gsatt('FWOT','COLO',5)
      
c Primary DC support structure located at the ends of the DC in z.
c Use the ti_switch variable to choose whether these are Ti or Al.
c Defined as C sections using 3 radial sections of material on each
c end of the DC.  For arm 1, the supports will be named
c DSPO (outer radial support), DSPS (side support), and DSPI (inner
c radial support). For arm 2, replace the 'D' with an 'F'.

c set thickness of support structure elements
      if (dc_ti_switch.eq.1) then
         dcsthick = dc_supptiside
         dcsthick2 = dc_supptibase
      else
         dcsthick = dc_suppalside
         dcsthick2 = dc_suppalbase
      endif

c *** definition of windows to be inserted into inner
c     radial support of C-frame

      par(1) = dc_inradius
      par(2) = dc_inradius + dcsthick
      par(3) = dc_suppwinz/2.0
      par(4) = -(dc_suppwinx/dc_inradius)*raddeg/2.
      par(5) = +(dc_suppwinx/dc_inradius)*raddeg/2.
      if (dc_ti_switch.eq.1) then
         call gsvolu('IWIN','TUBS',410,par,5,ierror)
      else
         call gsvolu('IWIN','TUBS',410,par,5,ierror)
      endif
      call gsatt('IWIN','SEEN',1)
      call gsatt('IWIN','COLO',5)


c *** definition of windows to be inserted into outer
c     radial support of C-frame

      par(1) = dc_outradius - dcsthick
      par(2) = dc_outradius
      par(3) = dc_suppwinz/2.0
      par(4) = -(dc_suppwinx/dc_outradius)*raddeg/2.
      par(5) = +(dc_suppwinx/dc_outradius)*raddeg/2.
      if (dc_ti_switch.eq.1) then
         call gsvolu('OWIN','TUBS',410,par,5,ierror)
      else
         call gsvolu('OWIN','TUBS',410,par,5,ierror)
      endif
      call gsatt('OWIN','SEEN',1)
      call gsatt('OWIN','COLO',5)


c *** definition of inner, outer and side supports of C-frame section

      par(1) = dc_inradius
      par(2) = dc_inradius + dcsthick
      par(3) = dc_suppzthick/2.0
      par(4) = dc_phibotw-dphigusw
      par(5) = dc_phitopw+dphigusw
      if (dc_ti_switch.eq.1) then
         call gsvolu('DSPI','TUBS',413,par,5,ierror)
      else
         call gsvolu('DSPI','TUBS',414,par,5,ierror)
      endif
      call gsatt('DSPI','SEEN',1)
      call gsatt('DSPI','COLO',5)

      par(1) = dc_outradius - dcsthick
      par(2) = dc_outradius
      par(3) = dc_suppzthick/2.0
      par(4) = dc_phibotw-dphigusw
      par(5) = dc_phitopw+dphigusw
      if (dc_ti_switch.eq.1) then
         call gsvolu('DSPO','TUBS',413,par,5,ierror)
      else
         call gsvolu('DSPO','TUBS',414,par,5,ierror)
      endif
      call gsatt('DSPO','SEEN',1)
      call gsatt('DSPO','COLO',5)

      par(1) = dc_inradius + dcsthick
      par(2) = dc_outradius - dcsthick
      par(3) = dcsthick2/2.0
      par(4) = dc_phibotw-dphigusw
      par(5) = dc_phitopw+dphigusw
      if (dc_ti_switch.eq.1) then
         call gsvolu('DSPS','TUBS',413,par,5,ierror)
      else
         call gsvolu('DSPS','TUBS',414,par,5,ierror)
      endif
      call gsatt('DSPS','SEEN',1)
      call gsatt('DSPS','COLO',5)

      par(1) = dc_inradius
      par(2) = dc_inradius + dcsthick
      par(3) = dc_suppzthick/2.0
      par(4) = dc_phitope-dphiguse
      par(5) = dc_phibote+dphiguse
      if (dc_ti_switch.eq.1) then
         call gsvolu('FSPI','TUBS',413,par,5,ierror)
      else
         call gsvolu('FSPI','TUBS',414,par,5,ierror)
      endif
      call gsatt('FSPI','SEEN',1)
      call gsatt('FSPI','COLO',5)

      par(1) = dc_outradius - dcsthick
      par(2) = dc_outradius
      par(3) = dc_suppzthick/2.0
      par(4) = dc_phitope-dphiguse
      par(5) = dc_phibote+dphiguse
      if (dc_ti_switch.eq.1) then
         call gsvolu('FSPO','TUBS',413,par,5,ierror)
      else
         call gsvolu('FSPO','TUBS',414,par,5,ierror)
      endif
      call gsatt('FSPO','SEEN',1)
      call gsatt('FSPO','COLO',5)

      par(1) = dc_inradius + dcsthick
      par(2) = dc_outradius - dcsthick
      par(3) = dcsthick2/2.0
      par(4) = dc_phitope-dphiguse
      par(5) = dc_phibote+dphiguse
      if (dc_ti_switch.eq.1) then
         call gsvolu('FSPS','TUBS',413,par,5,ierror)
      else
         call gsvolu('FSPS','TUBS',414,par,5,ierror)
      endif
      call gsatt('FSPS','SEEN',1)
      call gsatt('FSPS','COLO',5)

c The primary DC support is a C-section that needs support internally
c using gussets.  Below are the definitions for the gussets.  It will
c be portrayed by a single volume named DGUS.

      par(1) = (dc_outradius-dc_inradius-2.0*dcsthick)/2.0
      par(2) = dcsthick/2.0
      par(3) = (dc_suppzthick-dcsthick2)/2.0
      if (dc_ti_switch.eq.1) then
         call gsvolu('DGUS','BOX ',413,par,3,ierror)
      else
         call gsvolu('DGUS','BOX ',414,par,3,ierror)
      endif
      call gsatt('DGUS','SEEN',1)
      call gsatt('DGUS','COLO',5)

c Define the endcaps of the DC support structures.  These are hollow
c boxes stretching from the negative z structure to the positive z
c structure.  They are centered midway between the first two gussets
c on that end.  They span one gusset-spacing in phi.
c The names of the box volumes will be DCBE (top/bottom) and DCBS (side).
c They will be placed in the mother volume DEND filled with air.

      par(1) = (dc_outradius-dc_inradius)/2.0
      par(2) = (dc_inradius*dphigusw*0.0175)/2.0
      par(3) = dc_suppzlength/2.0
      call gsvolu('DEND','BOX ',410,par,3,ierror)
      call gsatt('DEND','SEEN',1)
      call gsatt('DEND','COLO',5)
 
      par(1) = dcsthick/2.0
      par(2) = ((dc_inradius*dphigusw*0.0175)-
     +     (2.0*dcsthick))/2.0
      par(3) = dc_suppzlength/2.0
      if (dc_ti_switch.eq.1) then
         call gsvolu('DCBE','BOX ',413,par,3,ierror)
      else
         call gsvolu('DCBE','BOX ',414,par,3,ierror)
      endif
      call gsatt('DCBE','SEEN',1)
      call gsatt('DCBE','COLO',5)

      par(1) = (dc_outradius-dc_inradius)/2.0
      par(2) = dcsthick/2.0
      par(3) = dc_suppzlength/2.0
      if (dc_ti_switch.eq.1) then
         call gsvolu('DCBS','BOX ',413,par,3,ierror)
      else
         call gsvolu('DCBS','BOX ',414,par,3,ierror)
      endif
      call gsatt('DCBS','SEEN',1)
      call gsatt('DCBS','COLO',5)

c Define the wire support volumes.  

c *** Definition of the X1 wire support.
c First, define the radial base that attaches to the drift chamber frame.
c Their names are DIX1 and DOX1.

      par(1) = dc_rplane(1)-dc_x1rextent
      par(2) = dc_rplane(1)-dc_x1rextent+dc_x1baserad
      par(3) = dc_x1basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DIX1','TUBS',414,par,5,ierror)
      call gsatt('DIX1','SEEN',1)
      call gsatt('DIX1','COLO',3)
      par(4) = dc_phitope
      par(5) = dc_phibote
      call gsvolu('FIX1','TUBS',414,par,5,ierror)
      call gsatt('FIX1','SEEN',1)
      call gsatt('FIX1','COLO',3)

      par(1) = dc_rplane(12)+dc_x1rextent-dc_x1baserad
      par(2) = dc_rplane(12)+dc_x1rextent

      par(3) = dc_x1basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DOX1','TUBS',414,par,5,ierror)
      call gsatt('DOX1','SEEN',1)
      call gsatt('DOX1','COLO',3)
      par(4) = dc_phitope
      par(5) = dc_phibote
      call gsvolu('FOX1','TUBS',414,par,5,ierror)
      call gsatt('FOX1','SEEN',1)
      call gsatt('FOX1','COLO',3)

c Define the Al support structures that define the slots for the
c wire support G10 cards.  The name is DSX1.

      par(1) = ((dc_rplane(12)+dc_x1rextent)-
     +     (dc_rplane(1)-dc_x1rextent))/2.0
      par(3) = dc_x1slotz/2.0
      par(2) = dc_x1slotthick/2.0
      call gsvolu('DSX1','BOX ',414,par,3,ierror)
      call gsatt('DSX1','SEEN',1)
      call gsatt('DSX1','COLO',3)

c Define the G10 wire support structures.  These go into the Al
c support slots and are the pieces that the wires are physically
c attached to for spacing.  One G10 card goes into each slot.
c The name is DGX1.

      par(1) = ((dc_rplane(12)+dc_x1rextent)-
     +     (dc_rplane(1)-dc_x1rextent))/2.0
      par(3) = dc_x1suppz/2.0
      par(2) = dc_x1suppthick/2.0
      call gsvolu('DGX1','BOX ',416,par,3,ierror)
      call gsatt('DGX1','SEEN',1)
      call gsatt('DGX1','COLO',3)

c *** Definition of the X2 wire support.

c First, define the radial base that attaches to the drift chamber frame.
c Their names are DIX2 and DOX2.

      par(1) = dc_rplane(21)-dc_x2rextent
      par(2) = dc_rplane(21)-dc_x2rextent+dc_x2baserad
      par(3) = dc_x2basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DIX2','TUBS',414,par,5,ierror)
      call gsatt('DIX2','SEEN',1)
      call gsatt('DIX2','COLO',3)
      par(4) = dc_phitope
      par(5) = dc_phibote
      call gsvolu('FIX2','TUBS',414,par,5,ierror)
      call gsatt('FIX2','SEEN',1)
      call gsatt('FIX2','COLO',3)

      par(1) = dc_rplane(32)+dc_x2rextent-dc_x2baserad
      par(2) = dc_rplane(32)+dc_x2rextent

      par(3) = dc_x2basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DOX2','TUBS',414,par,5,ierror)
      call gsatt('DOX2','SEEN',1)
      call gsatt('DOX2','COLO',3)
      par(4) = dc_phitope
      par(5) = dc_phibote
      call gsvolu('FOX2','TUBS',414,par,5,ierror)
      call gsatt('FOX2','SEEN',1)
      call gsatt('FOX2','COLO',3)

c Define the Al support structures that define the slots for the
c wire support G10 cards.  The name is DSX2.

      par(1) = ((dc_rplane(32)+dc_x2rextent)-
     +     (dc_rplane(21)-dc_x2rextent))/2.0
      par(3) = dc_x2slotz/2.0
      par(2) = dc_x2slotthick/2.0
      call gsvolu('DSX2','BOX ',414,par,3,ierror)
      call gsatt('DSX2','SEEN',1)
      call gsatt('DSX2','COLO',3)

c Define the G10 wire support structures.  These go into the Al
c support slots and are the pieces that the wires are physically
c attached to for spacing.  One G10 card goes into each slot.
c The name is DGX2.

      par(1) = ((dc_rplane(32)+dc_x2rextent)-
     +     (dc_rplane(21)-dc_x2rextent))/2.0
      par(3) = dc_x2suppz/2.0
      par(2) = dc_x2suppthick/2.0
      call gsvolu('DGX2','BOX ',416,par,3,ierror)
      call gsatt('DGX2','SEEN',1)
      call gsatt('DGX2','COLO',3)

c***********************************************************************
c *** Definition of the U1 wire support.
c***********************************************************************

c First, define the radial base that attaches to the drift chamber frame.
c Their names are DIU1 and DOU1.


      par(1) = dc_rplane(13)-dc_u1rextent  
      par(2) = dc_rplane(13)-dc_u1rextent+dc_u1baserad
      par(3) = dc_u1basez/2.0
      par(4) = dc_phibotw                    ! inner base for West
      par(5) = dc_phitopw
      call gsvolu('DIU1','TUBS',414,par,5,ierror)
      call gsatt('DIU1','SEEN',1)
      call gsatt('DIU1','COLO',3)
      par(4) = dc_phitope                    ! inner base for East
      par(5) = dc_phibote
      call gsvolu('FIU1','TUBS',414,par,5,ierror)
      call gsatt('FIU1','SEEN',1)
      call gsatt('FIU1','COLO',3)

      par(1) = dc_rplane(16)+dc_u1rextent-dc_u1baserad
      par(2) = dc_rplane(16)+dc_u1rextent    ! outer base for West
      
      par(3) = dc_u1basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DOU1','TUBS',414,par,5,ierror)
      call gsatt('DOU1','SEEN',1)
      call gsatt('DOU1','COLO',3)
      par(4) = dc_phitope                    ! outer base for East
      par(5) = dc_phibote
      call gsvolu('FOU1','TUBS',414,par,5,ierror)
      call gsatt('FOU1','SEEN',1)
      call gsatt('FOU1','COLO',3)

c Define the Al support structures that define the slots for the
c wire support G10 cards.  The name is DSU1.

      par(1) = dc_u1slotthick/2.0
      par(2) = dc_u1slotz/2.0
      par(3) = ((dc_rplane(16)+dc_u1rextent)-
     +         (dc_rplane(13)-dc_u1rextent))/2.0
      par(4) = thet_slotu1 
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DSU1','PARA',414,par,6,ierror)
      call gsatt('DSU1','SEEN',1)
      call gsatt('DSU1','COLO',3)

c Define the G10 wire support structures.  These go into the Al
c support slots and are the pieces that the wires are physically
c attached to for spacing.  One G10 card goes into each slot.
c The name is DGU1.

      par(1) = dc_u1suppthick/2.0      
      par(2) = dc_u1suppz/2.0
      par(3) = ((dc_rplane(16)+dc_u1rextent)-
     +     (dc_rplane(13)-dc_u1rextent))/2.0
      par(4) = thet_slotu1
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DGU1','PARA',416,par,6,ierror)
      call gsatt('DGU1','SEEN',1)
      call gsatt('DGU1','COLO',3)


c************************************************************************
c *** Definition of the V1 wire support.
c************************************************************************

c First, define the radial base that attaches to the drift chamber frame.
c Their names are DIV1 and DOV1.

      par(1) = dc_rplane(17)-dc_v1rextent  
      par(2) = par(1)+dc_v1baserad
      par(3) = dc_v1basez/2.0
      par(4) = dc_phibotw                    ! inner base for West
      par(5) = dc_phitopw
      call gsvolu('DIV1','TUBS',414,par,5,ierror)
      call gsatt('DIV1','SEEN',1)
      call gsatt('DIV1','COLO',3)
      par(4) = dc_phitope                    ! inner base for East
      par(5) = dc_phibote
      call gsvolu('FIV1','TUBS',414,par,5,ierror)
      call gsatt('FIV1','SEEN',1)
      call gsatt('FIV1','COLO',3)

      par(1) = dc_rplane(20)+dc_v1rextent-dc_v1baserad
      par(2) = dc_rplane(20)+dc_v1rextent    ! outer base for West
      
      par(3) = dc_v1basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DOV1','TUBS',414,par,5,ierror)
      call gsatt('DOV1','SEEN',1)
      call gsatt('DOV1','COLO',3)
      par(4) = dc_phitope                    ! outer base for East
      par(5) = dc_phibote
      call gsvolu('FOV1','TUBS',414,par,5,ierror)
      call gsatt('FOV1','SEEN',1)
      call gsatt('FOV1','COLO',3)

c Define the Al support structures that define the slots for the
c wire support G10 cards.  The name is DSV1.

      par(1) = dc_v1slotthick/2.0
      par(2) = dc_v1slotz/2.0
      par(3) = ((dc_rplane(20)+dc_v1rextent)-
     +         (dc_rplane(17)-dc_v1rextent))/2.0
      par(4) = thet_slotv1
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DSV1','PARA',414,par,6,ierror)
      call gsatt('DSV1','SEEN',1)
      call gsatt('DSV1','COLO',3)

c Define the G10 wire support structures.  These go into the Al
c support slots and are the pieces that the wires are physically
c attached to for spacing.  One G10 card goes into each slot.
c The name is DGV1.

      par(1) = dc_v1suppthick/2.0      
      par(2) = dc_v1suppz/2.0
      par(3) = ((dc_rplane(20)+dc_v1rextent)-
     +     (dc_rplane(17)-dc_v1rextent))/2.0
      par(4) = thet_slotv1
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DGV1','PARA',416,par,6,ierror)
      call gsatt('DGV1','SEEN',1)
      call gsatt('DGV1','COLO',3)

c***********************************************************************
c *** Definition of the U2 wire support.
c***********************************************************************

c First, define the radial base that attaches to the drift chamber frame.
c Their names are DIU2 and DOU2.

      par(1) = dc_rplane(33)-dc_u2rextent  
      par(2) = par(1)+dc_u2baserad
      par(3) = dc_u2basez/2.0
      par(4) = dc_phibotw                    ! inner base for West
      par(5) = dc_phitopw
      call gsvolu('DIU2','TUBS',414,par,5,ierror)
      call gsatt('DIU2','SEEN',1)
      call gsatt('DIU2','COLO',3)
      par(4) = dc_phitope                    ! inner base for East
      par(5) = dc_phibote
      call gsvolu('FIU2','TUBS',414,par,5,ierror)
      call gsatt('FIU2','SEEN',1)
      call gsatt('FIU2','COLO',3)

      par(1) = dc_rplane(36)+dc_u2rextent    ! outer base for West
      par(2) = par(1)-dc_u2baserad

      par(1) = dc_rplane(36)+dc_u2rextent-dc_u2baserad
      par(2) = dc_rplane(36)+dc_u2rextent    ! outer base for West
      
      par(3) = dc_u2basez/2.0
      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DOU2','TUBS',414,par,5,ierror)
      call gsatt('DOU2','SEEN',1)
      call gsatt('DOU2','COLO',3)
      par(4) = dc_phitope                    ! outer base for East
      par(5) = dc_phibote
      call gsvolu('FOU2','TUBS',414,par,5,ierror)
      call gsatt('FOU2','SEEN',1)
      call gsatt('FOU2','COLO',3)

c Define the Al support structures that define the slots for the
c wire support G10 cards.  The name is DSU2.

      par(1) = dc_u2slotthick/2.0
      par(2) = dc_u2slotz/2.0
      par(3) = ((dc_rplane(36)+dc_u2rextent)-
     +         (dc_rplane(33)-dc_u2rextent))/2.0
      par(4) = thet_slotu2
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DSU2','PARA',414,par,6,ierror)
      call gsatt('DSU2','SEEN',1)
      call gsatt('DSU2','COLO',3)

c Define the G10 wire support structures.  These go into the Al
c support slots and are the pieces that the wires are physically
c attached to for spacing.  One G10 card goes into each slot.
c The name is DGU2.

      par(1) = dc_u2suppthick/2.0      
      par(2) = dc_u2suppz/2.0
      par(3) = ((dc_rplane(36)+dc_u2rextent)-
     +     (dc_rplane(33)-dc_u2rextent))/2.0
      par(4) = thet_slotu2
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DGU2','PARA',416,par,6,ierror)
      call gsatt('DGU2','SEEN',1)
      call gsatt('DGU2','COLO',3)


c************************************************************************
c *** Definition of the V2 wire support.
c************************************************************************

c First, define the radial base that attaches to the drift chamber frame.
c Their names are DIV2 and DOV2.

      par(1) = dc_rplane(37)-dc_v2rextent  
      par(2) = par(1)+dc_v2baserad
      par(3) = dc_v2basez/2.0
      par(4) = dc_phibotw                    ! inner base for West
      par(5) = dc_phitopw
      call gsvolu('DIV2','TUBS',414,par,5,ierror)
      call gsatt('DIV2','SEEN',1)
      call gsatt('DIV2','COLO',3)
      par(4) = dc_phitope                    ! inner base for East
      par(5) = dc_phibote
      call gsvolu('FIV2','TUBS',414,par,5,ierror)
      call gsatt('FIV2','SEEN',1)
      call gsatt('FIV2','COLO',3)

      par(1) = dc_rplane(40)+dc_v2rextent-dc_v2baserad
      par(2) = dc_rplane(40)+dc_v2rextent    ! outer base for West
      
      par(3) = dc_v2basez/2.0

      par(4) = dc_phibotw
      par(5) = dc_phitopw
      call gsvolu('DOV2','TUBS',414,par,5,ierror)
      call gsatt('DOV2','SEEN',1)
      call gsatt('DOV2','COLO',3)
      par(4) = dc_phitope                    ! outer base for East
      par(5) = dc_phibote
      call gsvolu('FOV2','TUBS',414,par,5,ierror)
      call gsatt('FOV2','SEEN',1)
      call gsatt('FOV2','COLO',3)

c Define the Al support structures that define the slots for the
c wire support G10 cards.  The name is DSV2.

      par(1) = dc_v2slotthick/2.0
      par(2) = dc_v2slotz/2.0
      par(3) = ((dc_rplane(40)+dc_v2rextent)-
     +         (dc_rplane(37)-dc_v2rextent))/2.0
      par(4) = thet_slotv2
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DSV2','PARA',414,par,6,ierror)
      call gsatt('DSV2','SEEN',1)
      call gsatt('DSV2','COLO',3)

c Define the G10 wire support structures.  These go into the Al
c support slots and are the pieces that the wires are physically
c attached to for spacing.  One G10 card goes into each slot.
c The name is DGX1.

      par(1) = dc_v2suppthick/2.0      
      par(2) = dc_v2suppz/2.0
      par(3) = ((dc_rplane(40)+dc_v2rextent)-
     +     (dc_rplane(37)-dc_v2rextent))/2.0
      par(4) = thet_slotv2
      par(5) = 0.
      par(6) = 0.
      call gsvolu('DGV2','PARA',416,par,6,ierror)
      call gsatt('DGV2','SEEN',1)
      call gsatt('DGV2','COLO',3)


c********************************************************************
c Define the central support strut traversing the X2 section of the
c drift chamber in the z-direction.  This puppy is epoxy (C fiber).
c********************************************************************

      par(1) = dc_cfibinrad/2.0
      par(2) = dc_cfiboutrad/2.0
      maxht = dc_x2basez+dc_x2suppz
      par(3) = (dc_suppzlength-2.0*maxht)/2.0
      call gsvolu('DSTR','TUBE',420,par,3,ierror)
      call gsatt('DSTR','SEEN',1)
      call gsatt('DSTR','COLO',5)

c ********************* SENSITIVE VOLUME DEFINITION *********************

c ************************* Define general cell geometries **************

c Define the general cell geometries. Each is defined as conical sections
c with identical dimensions.  The names are DCAW for the west arm and
c DCAE for the east arm.  All sensitive volumes will be placed within
c these volumes, which contain insensitive drift chamber gas.

c Arm 1 west (concentric section - 1 volume)

      par(1) = dc_suppzlength/2.0
      par(2) = dc_inradius
      par(3) = dc_outradius
      par(4) = dc_inradius
      par(5) = dc_outradius
      par(6) = dc_phibotw
      par(7) = dc_phitopw
      namec = arm1name
      call gsvolu(namec,'CONS',465,par,7,ierror)
      call gsatt(namec,'SEEN',1)
      call gsatt(namec,'COLO',5)

c Arm 2 east (concentric section - 1 volume)

      par(1) = dc_suppzlength/2.0
      par(2) = dc_inradius
      par(3) = dc_outradius
      par(4) = dc_inradius
      par(5) = dc_outradius
      par(6) = dc_phitope
      par(7) = dc_phibote
      namec = arm2name
      call gsvolu(namec,'CONS',465,par,7,ierror)
      call gsatt(namec,'SEEN',1)
      call gsatt(namec,'COLO',5)


c ************* Define the plane geometries *****************************

c Define the plane geometries.  These are trapezoids to be placed within
c each of the cell volumes.  Same dimensions fixed for each arm.
c The names for each sensitive volume are divided into planes, with cell
c formed from copies of the planar volumes.  The arm 1 sensitive volume
c names are called DPXX where XX is the plane number ranging from 1-40.
c The arm 2 names are FPXX.

c Note that there are only 76 UV cells versus 80 X cells.  The UV cell 
c numbering starts at 2 and goes to 78.
c The X2 cells number 41 and 42 are missing in both arms to make room for
c the support strut (they are included in the definition, but not the
c positioning).

c The volumes will be positioned within the following loops:
c A loop over keystones.  There are 20 keystones per arm, each containing
c 4 cells.  A loop over wire groups.  There are 8 wire groups per arm.
c Odd numbered wire groups will contain the X1 and UV1 sections.  Even
c numbered wire groups will contain the X2 and UV2 sections.  Finally,
c a loop over planes within a wire group, from plane 1 to plane 20.  Here,
c plane numbers 1-12 are X sections, planes 13-16 are U sections, and
c planes 17-20 are V sections, independent of wire group.

c start with arm 1 (west)
      do igroup = 1,2   ! loop over wire groups in a cell
         do iplane=1,20         ! loop over planes in a group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            r2 = dc_rplane(jplane)+(dc_planethick/2.0)
            r1 = dc_rplane(jplane)-(dc_planethick/2.0)
            if (iplane.le.12) then    ! this is an X section
               if (igroup.eq.1) maxht = dc_x1basez+dc_x1suppz
               if (igroup.eq.2) maxht = dc_x2basez+dc_x2suppz
               par(1) = (dc_suppzlength-2.0*maxht)/2.0
               par(2) = 0.0     ! theta
               par(3) = 0.0     ! phi
               par(7) = 0.0     ! alp1
               par(4) = 0.5*(r2-r1)*cosd(dphicellw/2.0) ! H1
               par(8) = par(4)  ! H2
               par(5) = r1*sind(dphicellw/2.0) ! BL1
               par(9) = par(5)  ! BL2
               par(6) = r2*sind(dphicellw/2.0) ! TL1
               par(10) = par(6) ! TL2
               namep = parm1name//name40(jplane)
               call gsvolu(namep,'TRAP',480,par,11,ierror)
               call gsatt(namep,'SEEN',1)
               call gsatt(namep,'COLO',3)

            elseif (iplane.ge.13.and.iplane.le.16) then   ! U section

               rc = dc_rplane(jplane)
               deltax = rc*sind(2.0*dphicellw)
               phiuv = 0.0
               twist = 2.0*dphicellw
      tan_thet = sign(deltax,thet_slotu1)/(dc_suppzlength/2.0)  ! tangens of ideal THETA
               if (igroup.eq.1) then
      z1_tan  = dc_suppzlength/2. - dc_u1basez 
      z2_tan  = z1_tan - dc_u1slotz
      tan_thet= (z1_tan*tan_thet - dc_u1slotz*tand(thet_slotu1))/z2_tan 
      thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
      maxht   = dc_u1basez+dc_u1suppz
               endif
               if (igroup.eq.2) then
      z1_tan  = dc_suppzlength/2. - dc_u1basez 
      z2_tan  = z1_tan - dc_u2slotz
      tan_thet= (z1_tan*tan_thet - dc_u2slotz*tand(thet_slotu2))/z2_tan 
      thetauv = atand(tan_thet)   ! THETA after correction to slot inclination
      maxht   = dc_u2basez+dc_u2suppz
               endif

               par2(1) = dc_suppzlength/2.0 - maxht ! DZ
               par2(2) = thetauv ! THET
               par2(3) = phiuv  ! PHI
               par2(4) = twist  ! TWIST
               par2(5) = 0.5*(r2-r1)*cosd(dphicellw/2.0) ! H1
               par2(6) = r1*sind(dphicellw/2.0) ! BL1
               par2(7) = r2*sind(dphicellw/2.0) ! TL1
               par2(8) = 0.0    ! ALP1
               par2(9) = par2(5) ! H2
               par2(10) = par2(6) ! BL2
               par2(11) = par2(7) ! TL2
               par2(12) = 0.0   ! ALP2
               namep = parm1name//name40(jplane)
               call gsvolu(namep,'GTRA',480,par2,12,ierror)
               call gsatt(namep,'SEEN',1)
               call gsatt(namep,'COLO',2)

            elseif (iplane.ge.17) then ! V section

               rc = dc_rplane(jplane)
               deltax = rc*sind(2.0*dphicellw)
               phiuv = 0.0
               twist = -2.0*dphicellw
       tan_thet = sign(deltax,thet_slotv1)/(dc_suppzlength/2.0)  ! tangens of ideal THETA
               if (igroup.eq.1) then
       z1_tan  = dc_suppzlength/2. - dc_v1basez 
       z2_tan  = z1_tan - dc_v1slotz
       tan_thet= (z1_tan*tan_thet - dc_v1slotz*tand(thet_slotv1))/z2_tan 
       thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
       maxht   = dc_v1basez+dc_v1suppz
               endif
               if (igroup.eq.2) then
       z1_tan  = dc_suppzlength/2. - dc_v2basez 
       z2_tan  = z1_tan - dc_v2slotz
       tan_thet= (z1_tan*tan_thet - dc_v2slotz*tand(thet_slotv2))/z2_tan 
       thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
       maxht   = dc_v2basez+dc_v2suppz
               endif

               par2(1) = dc_suppzlength/2.0 - maxht
               par2(2) = thetauv ! THET
               par2(3) = phiuv  ! PHI
               par2(4) = twist  ! TWIST
               par2(5) = 0.5*(r2-r1)*cosd(dphicellw/2.0) ! H1
               par2(6) = r1*sind(dphicellw/2.0) ! BL1
               par2(7) = r2*sind(dphicellw/2.0) ! TL1
               par2(8) = 0.0    ! ALP1
               par2(9) = par2(5) ! H2
               par2(10) = par2(6) ! BL2
               par2(11) = par2(7) ! TL2
               par2(12) = 0.0   ! ALP2
               namep = parm1name//name40(jplane)
               call gsvolu(namep,'GTRA',480,par2,12,ierror)
               call gsatt(namep,'SEEN',1)
               call gsatt(namep,'COLO',2)
            endif               ! iplane
         enddo                  ! iplane=1,20
      enddo                     ! igroup=1,2

c arm 2 (east)
      do igroup = 1,2           ! loop over wire groups in a cell
         do iplane=1,20         ! loop over planes in a group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            r2 = dc_rplane(jplane)+(dc_planethick/2.0)
            r1 = dc_rplane(jplane)-(dc_planethick/2.0)
            if (iplane.le.12) then    ! this is an X section
               if (igroup.eq.1) maxht = dc_x1basez+dc_x1suppz
               if (igroup.eq.2) maxht = dc_u2basez+dc_x2suppz
               par(1) = dc_suppzlength/2.0 - maxht
               par(2) = 0.0     ! theta
               par(3) = 0.0     ! phi
               par(7) = 0.0     ! alp1
               par(4) = 0.5*(r2-r1)*cosd(dphicelle/2.0) ! H1
               par(8) = par(4)  ! H2
               par(5) = r1*sind(dphicelle/2.0) ! BL1
               par(9) = par(5)  ! BL2
               par(6) = r2*sind(dphicelle/2.0) ! TL1
               par(10) = par(6) ! TL2
               namep = parm2name//name40(jplane)
               call gsvolu(namep,'TRAP',480,par,11,ierror)
               call gsatt(namep,'SEEN',1)
               call gsatt(namep,'COLO',3)

            elseif (iplane.ge.13.and.iplane.le.16) then   ! U section

               rc = dc_rplane(jplane)
               deltax = rc*sind(2.0*dphicellw)
               phiuv = 0.0
               twist = 2.0*dphicellw
       tan_thet = sign(deltax,thet_slotu1)/(dc_suppzlength/2.0)  ! tangens of ideal THETA
               if (igroup.eq.1) then
       z1_tan  = dc_suppzlength/2. - dc_u1basez 
       z2_tan  = z1_tan - dc_u1slotz
       tan_thet= (z1_tan*tan_thet - dc_u1slotz*tand(thet_slotu1))/z2_tan 
       thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
       maxht   = dc_u1basez+dc_u1suppz
               endif
               if (igroup.eq.2) then
       z1_tan  = dc_suppzlength/2. - dc_u2basez 
       z2_tan  = z1_tan - dc_u2slotz
       tan_thet= (z1_tan*tan_thet - dc_u2slotz*tand(thet_slotu2))/z2_tan 
       thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
       maxht   = dc_u2basez+dc_u2suppz
               endif

               par2(1) = dc_suppzlength/2.0 - maxht
               par2(2) = thetauv ! THET
               par2(3) = phiuv  ! PHI
               par2(4) = twist  ! TWIST
               par2(5) = 0.5*(r2-r1)*cosd(dphicelle/2.0) ! H1
               par2(6) = r1*sind(dphicelle/2.0) ! BL1
               par2(7) = r2*sind(dphicelle/2.0) ! TL1
               par2(8) = 0.0    ! ALP1
               par2(9) = par2(5) ! H2
               par2(10) = par2(6) ! BL2
               par2(11) = par2(7) ! TL2
               par2(12) = 0.0   ! ALP2
               namep = parm2name//name40(jplane)
               call gsvolu(namep,'GTRA',480,par2,12,ierror)
               call gsatt(namep,'SEEN',1)
               call gsatt(namep,'COLO',2)

            elseif (iplane.ge.17) then ! V section

               rc = dc_rplane(jplane)
               deltax = rc*sind(2.0*dphicellw)
               phiuv = 0.0
               twist = -2.0*dphicellw
       tan_thet = sign(deltax,thet_slotv1)/(dc_suppzlength/2.0)  ! tangens of ideal THETA
               if (igroup.eq.1) then
       z1_tan  = dc_suppzlength/2. - dc_v1basez 
       z2_tan  = z1_tan - dc_v1slotz
       tan_thet= (z1_tan*tan_thet - dc_v1slotz*tand(thet_slotv1))/z2_tan 
       thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
       maxht   = dc_v1basez+dc_v1suppz
               endif
               if (igroup.eq.2) then
       z1_tan  = dc_suppzlength/2. - dc_v2basez 
       z2_tan  = z1_tan - dc_v2slotz
       tan_thet= (z1_tan*tan_thet - dc_v2slotz*tand(thet_slotv2))/z2_tan 
       thetauv = atand(tan_thet)  ! THETA after correction to slot inclination
       maxht   = dc_v2basez+dc_v2suppz
               endif

               par2(1) = dc_suppzlength/2.0 - maxht
               par2(2) = thetauv ! THET
               par2(3) = phiuv  ! PHI
               par2(4) = twist  ! TWIST
               par2(5) = 0.5*(r2-r1)*cosd(dphicelle/2.0) ! H1
               par2(6) = r1*sind(dphicelle/2.0) ! BL1
               par2(7) = r2*sind(dphicelle/2.0) ! TL1
               par2(8) = 0.0    ! ALP1
               par2(9) = par2(5) ! H2
               par2(10) = par2(6) ! BL2
               par2(11) = par2(7) ! TL2
               par2(12) = 0.0   ! ALP2
               namep = parm2name//name40(jplane)
               call gsvolu(namep,'GTRA',480,par2,12,ierror)
               call gsatt(namep,'SEEN',1)
               call gsatt(namep,'COLO',2)
            endif               ! iplane
         enddo                  ! iplane=1,20
      enddo                     ! igroup=1,2


c ********************** Position plane geometries ********************

c Position the plane geometries within the arm geometries
c There are no positioned the following planes (wires):
c for WEST arm:
c Cell number 40 of the X2 section is keystone 10, group 8, wires 1-12.
c Cell number 41 of the X2 section is keystone 11, group 2, wires 2,4,6,8.

c for EAST arm:
c Cell number 40 of the X2 section is keystone 10, group 8, wires 2,4,6,8..
c Cell number 41 of the X2 section is keystone 11, group 2, wires 1-12.



c arm 1 (west)
      do ikey = 1,dc_ncells/4   ! loop over keystones
         do igroup = 1,8   ! loop over wire groups
            do iplane=1,20      ! loop over planes
               jplane = iplane
               icell = 4*(ikey-1)+(int((igroup+1)/2))
               phii = dc_phibotw +dphicellw/4.+ (icell-1)*dphicellw
               if (igroup.eq.2.or.igroup.eq.4.or.igroup.eq.6.
     +              or.igroup.eq.8) jplane = iplane + 20

               ri = dc_rplane(jplane)*cosd(dphicellw/2.0)
               zi = 0.0

               if (iplane.le.12) then   ! X plane
                  xi = ri/sqrt(1+tand(phii)*tand(phii))
                  yi = sqrt(ri*ri-xi*xi)
                  if (phii.lt.0.0) yi = -yi
                  namep = parm1name//name40(jplane)
                  namec = arm1name
                  itest = 1
           if (ikey.eq.10.and.igroup.eq.8.and.
     +         iplane.le.12) itest = 0                   ! strut is here
           if (ikey.eq.11.and.igroup.eq.2.and.(iplane.eq.2.or.
     + iplane.eq.4.or.iplane.eq.6.or.iplane.eq.8)) itest = 0 ! look to strut
                  if (itest.eq.1)
     +              call gspos(namep,icell,namec,xi,yi,zi,
     +                   irttr(icell),'ONLY')
                  endif
               if (iplane.ge.13) then   ! U or V plane
                  if (icell.ge.3.and.icell.le.78) then
                     dr_adj = 2.*ri*sind(dphicellw)**2
                     r0 = ri - dr_adj   ! R adjustment at Z = 0
                     xi = r0/sqrt(1+tand(phii)*tand(phii))
                     yi = sqrt(r0*r0-xi*xi)
                     if (phii.lt.0.0) yi = -yi
                     namep = parm1name//name40(jplane)
                     namec = arm1name
                     call gspos(namep,icell,namec,xi,yi,zi,
     +                    irttr(icell),'ONLY')
                  endif
               endif
            enddo               ! iplane=1,20
         enddo                  ! igroup=1,8
      enddo                     ! ikey=1,20

c arm 2 (east)
      do ikey = 1,dc_ncells/4   ! loop over keystones
         do igroup = 1,8   ! loop over wire groups
            do iplane=1,20      ! loop over planes
               jplane = iplane
               icell = 4*(ikey-1)+(int((igroup+1)/2))
               phii = dc_phitope +3.*dphicellw/4.+(icell-1)*dphicelle
               if (igroup.eq.2.or.igroup.eq.4.or.igroup.eq.6.
     +              or.igroup.eq.8) jplane = iplane + 20

               ri = dc_rplane(jplane)*cosd(dphicelle/2.0)
               zi = 0.0

               if (iplane.le.12) then   ! X section
                  xi = -ri/sqrt(1+tand(phii)*tand(phii))
                  yi = sqrt(ri*ri-xi*xi)
                  if (phii.gt.180.0) yi = -yi
                  namep = parm2name//name40(jplane)
                  namec = arm2name
                  itest = 1
           if (ikey.eq.10.and.igroup.eq.8.and.(iplane.eq.2.or.
     +  iplane.eq.4.or.iplane.eq.6.or.iplane.eq.8)) itest = 0 !look to strut
           if (ikey.eq.11.and.igroup.eq.2.and.
     +         iplane.le.12) itest = 0                   ! strut is here
                  if (itest.eq.1)
     +                 call gspos(namep,icell,namec,xi,yi,zi,
     +                 irttr(icell+80),'ONLY')
                  endif
               if (iplane.ge.13) then   ! U or V plane
                  if (icell.ge.3.and.icell.le.78) then
                     dr_adj = 2.*ri*sind(dphicellw)**2
                     r0 = ri - dr_adj   ! R adjustment at Z = 0
                     zi = 0.0
                     xi = -r0/sqrt(1+tand(phii)*tand(phii))
                     yi = sqrt(r0*r0-xi*xi)
                     if (phii.gt.180.0) yi = -yi
                     namep = parm2name//name40(jplane)
                     namec = arm2name
                     call gspos(namep,icell,namec,xi,yi,zi,
     +                    irttr(icell+80),'ONLY')
                  endif
               endif
            enddo               ! iplane=1,20
         enddo                  ! igroup=1,8
      enddo                     ! ikey=1,20


c********************************************************************
c Position the support strut in the X2 section, cell 40 in WEST Arm
c and cell 41 in EAST Arm
*********************************************************************

      endz = 0.0
      endx = dc_cfibposrad/sqrt(1+tand(phistrut_w)*tand(phistrut_w))
      endy = sqrt(dc_cfibposrad*dc_cfibposrad-endx*endx)
      call gspos('DSTR',1,arm1name,endx,endy,endz,0,'ONLY')

      endx = -dc_cfibposrad/sqrt(1+tand(phistrut_e)*tand(phistrut_e))
      endy = sqrt(dc_cfibposrad*dc_cfibposrad-endx*endx)
      call gspos('DSTR',2,arm2name,endx,endy,endz,0,'ONLY')


c ******************** Position the cell geometries ***********************

      namec = arm1name
      call gspos(namec,1,wDCH,0.0,0.0,0.0,0,'ONLY')
      namec = arm2name
      call gspos(namec,1,eDCH,0.0,0.0,0.0,0,'ONLY')

c ******************** Position the support geometries *******************

c Position mylar entrance and exit radial windows

      call gspos('DWOT',1,wDCH,0.0,0.0,0.0,0,'ONLY')
      call gspos('FWOT',1,eDCH,0.0,0.0,0.0,0,'ONLY')
      call gspos('DWIN',1,wDCH,0.0,0.0,0.0,0,'ONLY')
      call gspos('FWIN',1,eDCH,0.0,0.0,0.0,0,'ONLY')

c Position windows into inner and outter supports of C-frame

      do iwin=1,dc_nwinin  
      call gspos('IWIN',iwin,'DSPI',0.,0.,0.,irotwininw(iwin),'ONLY')
      call gspos('IWIN',iwin,'FSPI',0.,0.,0.,irotwinine(iwin),'ONLY')
      enddo
      do iwin=1,dc_nwinout  
      call gspos('OWIN',iwin,'DSPO',0.,0.,0.,irotwinoutw(iwin),'ONLY')
      call gspos('OWIN',iwin,'FSPO',0.,0.,0.,irotwinoute(iwin),'ONLY')
      enddo

c Position main Ti (or Al) supports at ends in z
      call gspos('DSPO',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)+(dc_suppzthick/2.0),0,'ONLY')
      call gspos('DSPI',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)+(dc_suppzthick/2.0),0,'ONLY')
      call gspos('DSPS',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)+(dcsthick2/2.0),0,'ONLY')
      call gspos('DSPO',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)-(dc_suppzthick/2.0),0,'ONLY')
      call gspos('DSPI',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)-(dc_suppzthick/2.0),0,'ONLY')
      call gspos('DSPS',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)-(dcsthick2/2.0),0,'ONLY')
      call gspos('FSPO',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)+(dc_suppzthick/2.0),0,'ONLY')
      call gspos('FSPI',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)+(dc_suppzthick/2.0),0,'ONLY')
      call gspos('FSPS',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)+(dcsthick2/2.0),0,'ONLY')
      call gspos('FSPO',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)-(dc_suppzthick/2.0),0,'ONLY')
      call gspos('FSPI',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)-(dc_suppzthick/2.0),0,'ONLY')
      call gspos('FSPS',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)-(dcsthick2/2.0),0,'ONLY')

c Position the gussets within the C-section of the main support

      igus2 = 0
      do igus=1,dc_ngusset   ! west arm gussets
         igus2 = igus2+1
         gusz = (dc_suppzlength/2.0)+(dc_suppzthick/2.0)+(dcsthick2/2.0)
         gusr = (dc_inradius+dc_outradius)/2.0
         gusphi = dc_phibotw+(igus-2)*dphigusw
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.lt.0.0) gusy = -gusy
         call gspos('DGUS',igus2,wDCH,gusx,gusy,gusz,
     +        irotgusw(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DGUS',igus2,wDCH,gusx,gusy,-gusz,
     +        irotgusw(igus),'ONLY')
      enddo

      do igus=1,dc_ngusset   ! east arm gussets
         igus2 = igus2+1
         gusz = (dc_suppzlength/2.0)+(dc_suppzthick/2.0)+(dcsthick2/2.0)
         gusr = (dc_inradius+dc_outradius)/2.0
         gusphi = dc_phitope+(igus-2)*dphiguse
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.gt.180.0) gusy = -gusy
         call gspos('DGUS',igus2,eDCH,gusx,gusy,gusz,
     +        irotguse(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DGUS',igus2,eDCH,gusx,gusy,-gusz,
     +        irotguse(igus),'ONLY')
      enddo

c Position the end box plates connecting the two DC supports in each arm.

c First position the daughter volumes in the mother DEND volume.
      endz = 0.0
      endy = 0.0
      endx = (dc_outradius-dc_inradius)/2.0-dcsthick/2.0
      call gspos('DCBE',1,'DEND',endx,endy,endz,0,'ONLY')
      call gspos('DCBE',2,'DEND',-endx,endy,endz,0,'ONLY')
      endx = 0.0
      endy = (dc_inradius*dphigusw*0.0175)/2.0-(dcsthick/2.0)
      call gspos('DCBS',1,'DEND',endx,endy,endz,0,'ONLY')
      call gspos('DCBS',2,'DEND',endx,-endy,endz,0,'ONLY')

c Now position the DEND volume within INTR.  4 placements needed.

      endz = 0.0
      endr = (dc_inradius+dc_outradius)/2.0
      endphi = dc_phibotw-(dphigusw/2.0)   ! bottom of arm 1
      endx0 = endr/sqrt(1+tand(endphi)*tand(endphi))
      endy0 = sqrt(endr*endr-endx0*endx0)
      if (endphi.lt.0.0) endy0 = -endy0
      endy = endy0
      endx = endx0
      call gspos('DEND',1,wDCH,endx,endy,endz,
     +        irotend(1),'ONLY')
      endphi = dc_phitopw+(dphigusw/2.0)   ! top of arm 1
      endx0 = endr/sqrt(1+tand(endphi)*tand(endphi))
      endy0 = sqrt(endr*endr-endx0*endx0)
      if (endphi.lt.0.0) endy0 = -endy0
      endy = endy0
      endx = endx0
      call gspos('DEND',2,wDCH,endx,endy,endz,
     +        irotend(2),'ONLY')

      endphi = dc_phibote+(dphiguse/2.0)   ! bottom of east arm
      endx0 = -endr/sqrt(1+tand(endphi)*tand(endphi))
      endy0 = sqrt(endr*endr-endx0*endx0)
      if (endphi.gt.180.0) endy0 = -endy0
      endy = endy0
      endx = endx0
      call gspos('DEND',3,eDCH,endx,endy,endz,
     +        irotend(3),'ONLY')
      endphi = dc_phitope-(dphiguse/2.0)   ! top of east arm
      endx0 = -endr/sqrt(1+tand(endphi)*tand(endphi))
      endy0 = sqrt(endr*endr-endx0*endx0)
      if (endphi.gt.180.0) endy0 = -endy0
      endy = endy0
      endx = endx0
      call gspos('DEND',4,eDCH,endx,endy,endz,
     +        irotend(4),'ONLY')

c***********************************************
c Position the radial bases of wire supports 
c***********************************************

c  Positioning the X1 radial base of wire support

      call gspos('DIX1',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x1basez/2.0),0,'ONLY')
      call gspos('DOX1',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x1basez/2.0),0,'ONLY')
      call gspos('DIX1',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x1basez/2.0),0,'ONLY')
      call gspos('DOX1',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x1basez/2.0),0,'ONLY')
      call gspos('FIX1',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x1basez/2.0),0,'ONLY')
      call gspos('FOX1',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x1basez/2.0),0,'ONLY')
      call gspos('FIX1',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x1basez/2.0),0,'ONLY')
      call gspos('FOX1',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x1basez/2.0),0,'ONLY')

c  Positioning the U1&V1 radial base of wire support 

      call gspos('DIU1',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u1basez/2.0),0,'ONLY')
      call gspos('DOU1',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u1basez/2.0),0,'ONLY')
      call gspos('DIU1',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u1basez/2.0),0,'ONLY')
      call gspos('DOU1',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u1basez/2.0),0,'ONLY')
      call gspos('FIU1',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u1basez/2.0),0,'ONLY')
      call gspos('FOU1',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u1basez/2.0),0,'ONLY')
      call gspos('FIU1',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u1basez/2.0),0,'ONLY')
      call gspos('FOU1',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u1basez/2.0),0,'ONLY')
     
      call gspos('DIV1',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v1basez/2.0),0,'ONLY')
      call gspos('DOV1',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v1basez/2.0),0,'ONLY')
      call gspos('DIV1',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v1basez/2.0),0,'ONLY')
      call gspos('DOV1',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v1basez/2.0),0,'ONLY')
      call gspos('FIV1',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v1basez/2.0),0,'ONLY')
      call gspos('FOV1',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v1basez/2.0),0,'ONLY')
      call gspos('FIV1',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v1basez/2.0),0,'ONLY')
      call gspos('FOV1',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v1basez/2.0),0,'ONLY')

c  Positioning the X2  radial base of wire support 

      call gspos('DIX2',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x2basez/2.0),0,'ONLY')
      call gspos('DOX2',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x2basez/2.0),0,'ONLY')
      call gspos('DIX2',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x2basez/2.0),0,'ONLY')
      call gspos('DOX2',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x2basez/2.0),0,'ONLY')
      call gspos('FIX2',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x2basez/2.0),0,'ONLY')
      call gspos('FOX2',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_x2basez/2.0),0,'ONLY')
      call gspos('FIX2',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x2basez/2.0),0,'ONLY')
      call gspos('FOX2',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_x2basez/2.0),0,'ONLY')


c  Positioning the U2&V2 radial base of wire support 

      call gspos('DIU2',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u2basez/2.0),0,'ONLY')
      call gspos('DOU2',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u2basez/2.0),0,'ONLY')
      call gspos('DIU2',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u2basez/2.0),0,'ONLY')
      call gspos('DOU2',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u2basez/2.0),0,'ONLY')
      call gspos('FIU2',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u2basez/2.0),0,'ONLY')
      call gspos('FOU2',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_u2basez/2.0),0,'ONLY')
      call gspos('FIU2',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u2basez/2.0),0,'ONLY')
      call gspos('FOU2',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_u2basez/2.0),0,'ONLY')
     
      call gspos('DIV2',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v2basez/2.0),0,'ONLY')
      call gspos('DOV2',1,wDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v2basez/2.0),0,'ONLY')
      call gspos('DIV2',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v2basez/2.0),0,'ONLY')
      call gspos('DOV2',2,wDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v2basez/2.0),0,'ONLY')
      call gspos('FIV2',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v2basez/2.0),0,'ONLY')
      call gspos('FOV2',1,eDCH,0.0,0.0,
     +     (dc_suppzlength/2.0)-(dc_v2basez/2.0),0,'ONLY')
      call gspos('FIV2',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v2basez/2.0),0,'ONLY')
      call gspos('FOV2',2,eDCH,0.0,0.0,
     +     -(dc_suppzlength/2.0)+(dc_v2basez/2.0),0,'ONLY')

c************************************************************************
c Position the wire support Al slots and G10 cards for X1 and X2 sections
c************************************************************************

c *** X1 section

c *** position Al slots

      igus2 = 0
      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells
         gusz = (dc_suppzlength/2.0)-dc_x1basez-(dc_x1slotz/2.0)
         gusr = (dc_rplane(12)+dc_rplane(1))/2.0
         gusphi = dc_phibotw+(igus-1)*deltaphi
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSX1',igus2,wDCH,gusx,gusy,gusz,   !  WEST
     +        irotslotw(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DSX1',igus2,wDCH,gusx,gusy,-gusz,
     +        irotslotw(igus),'ONLY')
      enddo
      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells
         gusz = (dc_suppzlength/2.0)-dc_x1basez-(dc_x1slotz/2.0)
         gusr = (dc_rplane(12)+dc_rplane(1))/2.0
         gusphi = dc_phitope+(igus-1)*deltaphi
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSX1',igus2,eDCH,gusx,gusy,gusz,      ! EAST
     +        irotslote(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DSX1',igus2,eDCH,gusx,gusy,-gusz,
     +        irotslote(igus),'ONLY')
      enddo

c Position G10 cards.

      igus2 = 0
      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_x1basez-(dc_x1suppz/2.0)
         gusr = (dc_rplane(12)+dc_rplane(1))/2.0
         gusphi = dc_phibotw+deltaphi/2.+(igus-1)*deltaphi
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGX1',igus2,wDCH,gusx,gusy,gusz,
     +        irotsuppw(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DGX1',igus2,wDCH,gusx,gusy,-gusz,
     +        irotsuppw(igus),'ONLY')
      enddo
      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_x1basez-(dc_x1suppz/2.0)
         gusr = (dc_rplane(12)+dc_rplane(1))/2.0
         gusphi = dc_phitope+deltaphi/2.+(igus-1)*deltaphi
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGX1',igus2,eDCH,gusx,gusy,gusz,
     +        irotsuppe(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DGX1',igus2,eDCH,gusx,gusy,-gusz,
     +        irotsuppe(igus),'ONLY')
      enddo

c *** X2 section       

c *** position Al slots

      igus2 = 0
      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells
         gusz = (dc_suppzlength/2.0)-dc_x2basez-(dc_x2slotz/2.0)
         gusr = (dc_rplane(32)+dc_rplane(21))/2.0
         gusphi = dc_phibotw+(igus-1)*deltaphi
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSX2',igus2,wDCH,gusx,gusy,gusz,
     +        irotslotw(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DSX2',igus2,wDCH,gusx,gusy,-gusz,
     +        irotslotw(igus),'ONLY')
      enddo
      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells
         gusz = (dc_suppzlength/2.0)-dc_x2basez-(dc_x2slotz/2.0)
         gusr = (dc_rplane(32)+dc_rplane(21))/2.0
         gusphi = dc_phitope+(igus-1)*deltaphi
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSX2',igus2,eDCH,gusx,gusy,gusz,
     +        irotslote(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DSX2',igus2,eDCH,gusx,gusy,-gusz,
     +        irotslote(igus),'ONLY')
      enddo

c Position G10 cards.

      igus2 = 0
      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_x2basez-(dc_x2suppz/2.0)
         gusr = (dc_rplane(32)+dc_rplane(21))/2.0
         gusphi = dc_phibotw+deltaphi/2.+(igus-1)*deltaphi
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGX2',igus2,wDCH,gusx,gusy,gusz,
     +        irotsuppw(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DGX2',igus2,wDCH,gusx,gusy,-gusz,
     +        irotsuppw(igus),'ONLY')
      enddo
      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_x2basez-(dc_x2suppz/2.0)
         gusr = (dc_rplane(32)+dc_rplane(21))/2.0
         gusphi = dc_phitope+deltaphi/2.+(igus-1)*deltaphi
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(gusr*gusr-gusx*gusx)
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGX2',igus2,eDCH,gusx,gusy,gusz,
     +        irotsuppe(igus),'ONLY')
         igus2 = igus2+1
         call gspos('DGX2',igus2,eDCH,gusx,gusy,-gusz,
     +        irotsuppe(igus),'ONLY')
      enddo


c************************************************************************
c Position the wire support Al slots and G10 cards for U1&V1 and U2&V2 sections
c************************************************************************

c Position Al slots.

c *** U1 section  
  
      z_slot     = dc_u1slotz/2.0+dc_u1basez
      dx_shift   = z_slot*tand(thet_slotu1)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_u1basez-(dc_u1slotz/2.0)
         gusr = (dc_rplane(16)+dc_rplane(13))/2.0
         gusphi_0 = dc_phibotw+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg    
         gusphi = gusphi_0 + uv_phi_adj      ! correction of inclination
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU1',igus2,wDCH,gusx,gusy,gusz,   ! North
     +        irotslotw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj      ! correction of inclination
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU1',igus2,wDCH,gusx,gusy,-gusz,  ! South
     +        irotslotw_uv(igus),'ONLY')
      enddo

c *** EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_u1basez-(dc_u1slotz/2.0)
         gusr = (dc_rplane(16)+dc_rplane(13))/2.0
         gusphi_0 = dc_phitope+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU1',igus2,eDCH,gusx,gusy,gusz,
     +        irotslote_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU1',igus2,eDCH,gusx,gusy,-gusz,
     +        irotslote_uv(igus),'ONLY')
      enddo

c *** V1 section

      z_slot     = dc_v1slotz/2.0+dc_v1basez
      dx_shift   = z_slot*tand(thet_slotv1)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_v1basez-(dc_v1slotz/2.0)
         gusr = (dc_rplane(20)+dc_rplane(17))/2.0
         gusphi_0 = dc_phibotw+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV1',igus2,wDCH,gusx,gusy,gusz,
     +        irotslotw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV1',igus2,wDCH,gusx,gusy,-gusz,
     +        irotslotw_uv(igus),'ONLY')
      enddo

c ***  EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_v1basez-(dc_v1slotz/2.0)
         gusr = (dc_rplane(20)+dc_rplane(17))/2.0
         gusphi_0 = dc_phitope+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV1',igus2,eDCH,gusx,gusy,gusz,
     +        irotslote_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV1',igus2,eDCH,gusx,gusy,-gusz,
     +        irotslote_uv(igus),'ONLY')
      enddo

c *** Position the wire support G10 cards.

c *** U1 section
      z_slot     = dc_u1suppz/2.0+dc_u1basez
      dx_shift   = z_slot*tand(thet_slotu1)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_u1basez-(dc_u1suppz/2.0)
         gusr = (dc_rplane(16)+dc_rplane(13))/2.0
         gusphi_0 = dc_phibotw+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU1',igus2,wDCH,gusx,gusy,gusz,
     +        irotsuppw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU1',igus2,wDCH,gusx,gusy,-gusz,
     +        irotsuppw_uv(igus),'ONLY')
      enddo

c *** EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_u1basez-(dc_u1suppz/2.0)
         gusr = (dc_rplane(16)+dc_rplane(13))/2.0
         gusphi_0 = dc_phitope+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU1',igus2,eDCH,gusx,gusy,gusz,
     +        irotsuppe_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU1',igus2,eDCH,gusx,gusy,-gusz,
     +        irotsuppe_uv(igus),'ONLY')
      enddo

c  *** V1 section

      z_slot     = dc_v1suppz/2.0+dc_v1basez
      dx_shift   = z_slot*tand(thet_slotv1)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_v1basez-(dc_v1suppz/2.0)
         gusr = (dc_rplane(20)+dc_rplane(17))/2.0
         gusphi_0 = dc_phibotw+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV1',igus2,wDCH,gusx,gusy,gusz,
     +        irotsuppw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV1',igus2,wDCH,gusx,gusy,-gusz,
     +        irotsuppw_uv(igus),'ONLY')
      enddo

c ***   EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_v1basez-(dc_v1suppz/2.0)
         gusr = (dc_rplane(20)+dc_rplane(17))/2.0
         gusphi_0 = dc_phitope+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV1',igus2,eDCH,gusx,gusy,gusz,
     +        irotsuppe_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV1',igus2,eDCH,gusx,gusy,-gusz,
     +        irotsuppe_uv(igus),'ONLY')
      enddo

c *** Position Al slots. 
  
c *** U2 section  

      z_slot     = dc_u2slotz/2.0+dc_u2basez
      dx_shift   = z_slot*tand(thet_slotu2)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_u2basez-(dc_u2slotz/2.0)
         gusr = (dc_rplane(36)+dc_rplane(33))/2.0
         gusphi_0 = dc_phibotw+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg    
         gusphi = gusphi_0 + uv_phi_adj      ! correction of inclination
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU2',igus2,wDCH,gusx,gusy,gusz,   ! North
     +        irotslotw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj      ! correction of inclination
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU2',igus2,wDCH,gusx,gusy,-gusz,  ! South
     +        irotslotw_uv(igus),'ONLY')
      enddo

c *** EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_u2basez-(dc_u2slotz/2.0)
         gusr = (dc_rplane(36)+dc_rplane(33))/2.0
         gusphi_0 = dc_phitope+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU2',igus2,eDCH,gusx,gusy,gusz,
     +        irotslote_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSU2',igus2,eDCH,gusx,gusy,-gusz,
     +        irotslote_uv(igus),'ONLY')
      enddo

c *** V2 section

      z_slot     = dc_v2slotz/2.0+dc_v2basez
      dx_shift   = z_slot*tand(thet_slotv2)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_v2basez-(dc_v2slotz/2.0)
         gusr = (dc_rplane(40)+dc_rplane(37))/2.0
         gusphi_0 = dc_phibotw+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV2',igus2,wDCH,gusx,gusy,gusz,
     +        irotslotw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV2',igus2,wDCH,gusx,gusy,-gusz,
     +        irotslotw_uv(igus),'ONLY')
      enddo

c ***  EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells 
         gusz = (dc_suppzlength/2.0)-dc_v2basez-(dc_v2slotz/2.0)
         gusr = (dc_rplane(40)+dc_rplane(37))/2.0
         gusphi_0 = dc_phitope+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV2',igus2,eDCH,gusx,gusy,gusz,
     +        irotslote_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DSV2',igus2,eDCH,gusx,gusy,-gusz,
     +        irotslote_uv(igus),'ONLY')
      enddo

c *** Position G10 cards.

c *** U2 section

      z_slot     = dc_u2suppz/2.0+dc_u2basez
      dx_shift   = z_slot*tand(thet_slotu2)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_u2basez-(dc_u2suppz/2.0)
         gusr = (dc_rplane(36)+dc_rplane(33))/2.0
         gusphi_0 = dc_phibotw+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU2',igus2,wDCH,gusx,gusy,gusz,
     +        irotsuppw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU2',igus2,wDCH,gusx,gusy,-gusz,
     +        irotsuppw_uv(igus),'ONLY')
      enddo

c *** EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_u2basez-(dc_u2suppz/2.0)
         gusr = (dc_rplane(36)+dc_rplane(33))/2.0
         gusphi_0 = dc_phitope+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU2',igus2,eDCH,gusx,gusy,gusz,
     +        irotsuppe_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGU2',igus2,eDCH,gusx,gusy,-gusz,
     +        irotsuppe_uv(igus),'ONLY')
      enddo

c  *** V2 section

      z_slot     = dc_v2suppz/2.0+dc_v2basez
      dx_shift   = z_slot*tand(thet_slotv2)
      igus2 = 0

c ***  WEST, North (+Z) and South (-Z)

      deltaphi = dphicellw/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_v2basez-(dc_v2suppz/2.0)
         gusr = (dc_rplane(40)+dc_rplane(37))/2.0
         gusphi_0 = dc_phibotw+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV2',igus2,wDCH,gusx,gusy,gusz,
     +        irotsuppw_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.lt.0.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV2',igus2,wDCH,gusx,gusy,-gusz,
     +        irotsuppw_uv(igus),'ONLY')
      enddo

c ***   EAST, North (+Z) and South (-Z)

      deltaphi = dphicelle/2.0
      do igus=1,2*dc_ncells-1
         gusz = (dc_suppzlength/2.0)-dc_v2basez-(dc_v2suppz/2.0)
         gusr = (dc_rplane(40)+dc_rplane(37))/2.0
         gusphi_0 = dc_phitope+deltaphi/2.0+(igus-1)*deltaphi
         uv_phi_adj = dx_shift/gusr
         uv_phi_adj = uv_phi_adj*raddeg 
         gusphi = gusphi_0 + uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV2',igus2,eDCH,gusx,gusy,gusz,
     +        irotsuppe_uv(igus),'ONLY')
         gusphi = gusphi_0 - uv_phi_adj 
         gusx = -gusr/sqrt(1+tand(gusphi)*tand(gusphi))
         gusy = sqrt(abs(gusr*gusr-gusx*gusx))
         if (gusphi.gt.180.0) gusy = -gusy
         igus2 = igus2+1
         call gspos('DGV2',igus2,eDCH,gusx,gusy,-gusz,
     +        irotsuppe_uv(igus),'ONLY')
      enddo


c ZEBRA Parameter Bank Creation:
c This is not currently used by this DC geometry.  However, in order
c to preserve the operation of the PC geometry, this is included here.

      jflbnk = 0
      chform = '7I -F'    ! 7 int, rest floating point
      call mzform('PARA',chform,iod)   ! book characteristics
      lfi_para = 0              ! safety, reset after MZBOOK
      call mzbook(ixdiv_fr, lfi_para, lfi_para, 1,
     +     'PARA', 0, 0,
     +     10, iod, 0)         ! Reduce to 10, Feb. 15, 1998
      jflbnk = 1
      iqf(lfi_para + 2) = 19980215  ! key in the date here for DC (STAF)

c Now create the parameter bank for the drift chamber input parameters.
c It is named dcgp for DC Geometry Parameters.

      chform = '5I -F'   ! 5 integers, rest real
      call mzform('DCGP',chform,iod)   ! book em, Danno
      lfi_dcgp = 0                     ! safety (reset after MZBOOK)
      call mzbook(ixdiv_fr, lfi_dcgp, lfi_dcgp, 1,
     +     'DCGP', 0, 0, 110, iod, 0)   ! 110 input parameters

      iqindex = lfi_dcgp               ! CFM: change up by one unit

      iqf(iqindex+1) = dc_ncells
      iqf(iqindex+2) = dc_ngusset
      iqf(iqindex+3) = dc_ti_switch
      iqf(iqindex+4) = dc_nwinin
      iqf(iqindex+5) = dc_nwinout

      qf(iqindex+6) = dc_suppzlength
      qf(iqindex+7) = dc_inradius
      qf(iqindex+8) = dc_outradius
      qf(iqindex+9) = dc_phibotw
      qf(iqindex+10) = dc_phitopw
      qf(iqindex+11) = dc_phibote
      qf(iqindex+12) = dc_phitope

      do iplane=1,40
         qf(iqindex+12+iplane) = dc_rplane(iplane)
      enddo

      qf(iqindex+53) = dc_planethick
      qf(iqindex+54) = dc_winthickin
      qf(iqindex+55) = dc_winthickout
      qf(iqindex+56) = dc_supptiside
      qf(iqindex+57) = dc_suppalside
      qf(iqindex+58) = dc_suppzthick
      qf(iqindex+59) = dc_supptibase
      qf(iqindex+60) = dc_suppalbase
      qf(iqindex+61) = dc_suppwinz
      qf(iqindex+62) = dc_suppwinx

      qf(iqindex+63) = dc_x1baserad
      qf(iqindex+64) = dc_x2baserad
      qf(iqindex+65) = dc_x1basez
      qf(iqindex+66) = dc_x2basez
      qf(iqindex+67) = dc_x1slotthick
      qf(iqindex+68) = dc_x2slotthick
      qf(iqindex+69) = dc_x1slotz
      qf(iqindex+70) = dc_x2slotz
      qf(iqindex+71) = dc_x1suppthick
      qf(iqindex+72) = dc_x2suppthick
      qf(iqindex+73) = dc_x1suppz
      qf(iqindex+74) = dc_x2suppz
      qf(iqindex+75) = dc_x1rextent
      qf(iqindex+76) = dc_x2rextent

      qf(iqindex+77) = dc_u1rextent
      qf(iqindex+78) = dc_v1rextent
      qf(iqindex+79) = dc_u2rextent
      qf(iqindex+80) = dc_v2rextent

      qf(iqindex+81) = dc_u1basez
      qf(iqindex+82) = dc_v1basez
      qf(iqindex+83) = dc_u2basez
      qf(iqindex+84) = dc_v2basez

      qf(iqindex+85) = dc_u1baserad
      qf(iqindex+86) = dc_v1baserad
      qf(iqindex+87) = dc_u2baserad
      qf(iqindex+88) = dc_v2baserad

      qf(iqindex+89) = dc_u1slotthick
      qf(iqindex+90) = dc_v1slotthick
      qf(iqindex+91) = dc_u2slotthick
      qf(iqindex+92) = dc_v1slotthick

      qf(iqindex+93) = dc_u1slotz
      qf(iqindex+94) = dc_v1slotz
      qf(iqindex+95) = dc_u2slotz
      qf(iqindex+96) = dc_v2slotz

      qf(iqindex+97) =  dc_u1suppz
      qf(iqindex+98) =  dc_v1suppz
      qf(iqindex+99) =  dc_u2suppz
      qf(iqindex+100) = dc_v2suppz

      qf(iqindex+101) = dc_u1suppthick
      qf(iqindex+102) = dc_v1suppthick
      qf(iqindex+103) = dc_u2suppthick
      qf(iqindex+104) = dc_v2suppthick

      qf(iqindex+105) =  thet_slotu1
      qf(iqindex+106) =  thet_slotv1
      qf(iqindex+107) =  thet_slotu2
      qf(iqindex+108) =  thet_slotv2

      qf(iqindex+109) = dc_cfibinrad
      qf(iqindex+110) = dc_cfiboutrad

c     wrap it up

      write(6,*) 'dcgeom96 - execution completed successfully.'


      return
      end
