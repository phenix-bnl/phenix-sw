c $Id: rhic_magnet_install.f,v 1.3 2008/05/21 08:22:13 hpereira Exp $
      subroutine rhic_magnet_install

      implicit none

#include "g77trigdef.inc"


C     This routine was written for the purpose of installing magnets 
C     DX, D0, Q1 - Q5, and D5 into PISA/PISORP.  The source of the 
C     information concerning the dimensions of the magnets was the 
C     RHIC Design Manual as released in October, 1994.  In addition some 
C     dimensions were provided by Jack Sondericker.  

C     The locations of the magnets are at best preliminary.  The 
C     locations of the geometrical centers of the magnets were derived
C     from so-called "strawman" diagrams that are undergoing development
C     even now. 


C     Conventions

C     All dimensions included below are expressed in centimeters.  All 
C     lengths are half-lengths in order to conform to the conventions 
C     of GEANT.  All angles are in degrees.  


C     The path to common block gugeompar shown in the line below must
C     be changed before use in other environments.

#include "gugeom.inc"


c     Additions made by C.F. Maguire during initial CVS implementation

     


C     Declare variables that are not associated with any particular
C     magnet here.

      real*4 angle1, angle2

      integer*4 irot1, irot2, irot3, irot4, irot5, irot6, irot7
      integer*4 irot8, irot9, irot10, irot11, irot12, irot13, irot14 
      integer*4 nmed, ivolu

C     Declare variables used in description of floor here.

      real*4 x_coord_floor, y_coord_floor, z_coord_floor
      real*4 floor_par (3)

      parameter ( x_coord_floor = 2.40e+02 )
      parameter ( y_coord_floor = -1.9558e+02 )
      parameter ( z_coord_floor = 1.0550e+04 )

C     Declare variables to be used in description of walls here.

      real*4 x_coord_wall, y_coord_wall, z_coord_wall
      real*4 wall_par (3), x_coord_wall_2, z_coord_wall_2
      real*4 x_coord_wall_3, z_coord_wall_3
      real*4 x_coord_wall_4, z_coord_wall_4
      real*4 x_coord_wall_5, z_coord_wall_5
      real*4 x_coord_wall_6, z_coord_wall_6
      real*4 x_coord_wall_7, z_coord_wall_7
      real*4 x_coord_wall_8, z_coord_wall_8
      real*4 x_coord_wall_9, z_coord_wall_9
      real*4 x_coord_wall_10, z_coord_wall_10


      parameter ( y_coord_wall = 1.524e+01 )

C     Declare variables used in description of ceiling here. 

      real*4 x_coord_ceiling, y_coord_ceiling, z_coord_ceiling
      real*4 ceiling_par (3)

C     Note that we must not cover the experimental area with a ceiling!

      parameter ( x_coord_ceiling = 2.40e+02 )
      parameter ( y_coord_ceiling = 2.667e+02 )
      parameter ( z_coord_ceiling = 1.0550e+04 )

C     Declare variables used in description of DX here.  Data 
C     pertaining to DX were taken from pages 58-60 of the design 
C     manual. Recall that DX is common to both arcs.

      real*4 dx_length, dx_iron_ir, dx_iron_or, dx_collar_ir
      real*4 dx_collar_or, dx_x_coord, dx_y_coord, dx_z_coord
      real*4 dx_tube_ir, dx_tube_or, dx_par (3)
      real*4 dx_coil_ir, dx_coil_or

      parameter (dx_iron_ir = 1.3335e+01, dx_iron_or = 3.11e+01)
      parameter (dx_collar_ir = 1.0235e+01, dx_collar_or = 1.3335e+01)
      parameter (dx_coil_ir = 9.0e0, dx_coil_or = 1.0235e+01)
      parameter (dx_tube_ir = 7.981e0, dx_tube_or = 8.70e0)
      parameter (dx_x_coord = 0.0e0, dx_y_coord = 0.0e0)
      parameter (dx_z_coord = 1.165e+03, dx_length = 1.85e+02)

C     Declare variables that are used in the description of D0 here.
C     Data pertaining to D0 were taken from pages 55 - 57 of the design
C     manual.  Additional information pertaining to the dimensions of
C     beam tube was provided by Jack Sondericker at BNL.  There seems to
C     be no agreement as to the magnetic length of D0.  The RHIC design
C     manual quotes 3.6 meters, but Jack Sondericker quotes 4.4 meters.

C     The D0 magnets are actually slightly curved, but we will approximate
C     them as being straight in this program. 

      real*4 d0_length, d0_iron_ir, d0_iron_or, d0_collar_ir
      real*4 d0_collar_or, d0_x_coord, d0_y_coord, d0_z_coord
      real*4 d0_tube_ir, d0_tube_or, d0_par (3)
      real*4 d0_coil_ir, d0_coil_or
      real*4 theta_d0_deg, dist_dx_d0

      parameter (d0_iron_ir = 6.970e0, d0_iron_or = 1.55e+01)
      parameter (d0_collar_ir = 5.970e0, d0_collar_or = 6.970e0)
      parameter (d0_coil_ir = 5.0e0, d0_coil_or = 5.970e0)
      parameter (d0_tube_ir = 4.458e0, d0_tube_or = 4.760e0)
      parameter (theta_d0_deg = 1.08064e0, dist_dx_d0 = 1.06516e+03)
      parameter (d0_length = 2.20e+02, d0_y_coord = 0.0e0)

C     Declare variables that are used in the description of Q1, Q2, 
C     and Q3 here.  These quads are identical except for their length.
C     Variables that are common to all three quadrupoles are denoted by
C     the prefix "qla", which stands for "large aperture quadrupole".
C     The lengths of the quads are denoted by q1_length, q2_length, and
C     q3_length.   The relevant pages in the RHIC design manual are 
C     3ii, 3iii, and 48 - 54. 

C     It is important to note that magnet Q2 is really a corrector plus
C     a quadrupole and that Q3 is really a corrector plus a quadrupole
C     plus another quadrupole.  The parameters q1_length, q2_length, and
C     q3_length include the lengths of the associated corrector or 
C     correctors.

C     The inner diameter of the iron in these magnets varies between 
C     17.4 and 18.4 cm.  I have approximated the inner diameter by a 
C     constant value of 17.9 cm.  The spacer, which separates the coil 
C     from the yoke, also varies in thickness, and I have approximated
C     the true thickness by a single value of 1.25 cm. 

      real*4 q1_length, q2_length, q3_length
      real*4 qla_iron_ir, qla_iron_or, qla_collar_ir
      real*4 qla_collar_or, qla_theta_deg
      real*4 q1_x_coord, q1_y_coord, q1_z_coord
      real*4 q2_x_coord, q2_y_coord, q2_z_coord
      real*4 q3_x_coord, q3_y_coord, q3_z_coord
      real*4 qla_tube_ir, qla_tube_or, qla_par (3)
      real*4 qla_coil_ir, qla_coil_or
      real*4 d0_q1_dist, q1_q2_dist, q2_q3_dist

      parameter (qla_iron_ir = 8.950e0, qla_iron_or = 1.7525e+01)
      parameter (qla_collar_ir = 7.700e0, qla_collar_or = 8.950e0)
      parameter (qla_coil_ir = 6.50e0, qla_coil_or = 7.700e0)
      parameter (qla_tube_ir = 5.636e0, qla_tube_or = 6.033e0)
      parameter (q1_length = 9.5e+01, q2_length = 2.20e+02)
      parameter (q3_length = 2.00e+02, qla_theta_deg = 0.210536)
      parameter (d0_q1_dist = 3.779e+02, q1_q2_dist = 4.092e+02)
      parameter (q2_q3_dist = 4.607e+02, q1_y_coord = 0.0e0)
      parameter (q2_y_coord = 0.0e0, q3_y_coord = 0.0e0)

C     Declare variables that are used to describe Q4 and Q5 here. 
C     Q4 and Q5 are quadrupoles of standard aperture.  These magnets 
C     are described on pages 27 - 36 of the RHIC design manual.  Note
C     that the dimensions of the yoke are the same as those of the 
C     arc dipoles.  Numerical values for these parameters are shown on
C     page 20 of the design manual. 

C     Note also that both Q4 and Q5 are combinations of quadrupole, 
C     corrector, and trimmer.  The lengths shown below include the lengths
C     of all components. 

      real*4 q4_length, q5_length
      real*4 qsa_iron_ir, qsa_iron_or, qsa_collar_ir
      real*4 qsa_collar_or, qsa_theta_deg
      real*4 q4_x_coord, q4_y_coord, q4_z_coord
      real*4 q5_x_coord, q5_y_coord, q5_z_coord
      real*4 qsa_tube_ir, qsa_tube_or, qsa_par (3)
      real*4 qsa_coil_ir, qsa_coil_or
      real*4 q3_q4_dist, q4_q5_dist
      real*4 qsa_gap_ir, qsa_gap_or

      parameter (qsa_iron_ir = 5.97e0, qsa_iron_or = 1.3335e+01)
      parameter (qsa_collar_ir = 5.00e0, qsa_collar_or = 5.97e0)
      parameter (qsa_coil_ir = 3.995e0, qsa_coil_or = 5.00e0)
      parameter (qsa_gap_ir = 3.650, qsa_gap_or = 3.995e0)
      parameter (qsa_tube_ir = 3.455e0, qsa_tube_or = 3.650e0)
      parameter (q4_length = 2.050e+02, q5_length = 1.70e+02)
      parameter ( qsa_theta_deg = 0.210536e0)
      parameter (q3_q4_dist = 4.04317e+03, q4_q5_dist = 7.480e+02)
      parameter ( q4_y_coord = 0.0e0,q5_y_coord = 0.0e0 )


C     Declare variables that will be used to describe D5 below.
C     Note that there are differences between the D5 in the inner arc
C     and D5 in the outer arc.  For example the magnetic length of D5I 
C     is 6.92 m and the magnetic length of D5O is 8.71 m. 

C     Numerical values for the parameters for the parameters declared
C     below were extracted from pages 14 - 24 of the RHIC design manual. 

      real*4 d5_inner_length, d5_outer_length, d5_theta_deg
      real*4 d5_iron_ir, d5_iron_or, d5_collar_ir, d5_collar_or
      real*4 d5_out_x_coord, d5_out_y_coord, d5_out_z_coord
      real*4 d5_in_x_coord, d5_in_y_coord, d5_in_z_coord
      real*4 d5_tube_ir, d5_tube_or, d5_par (3)
      real*4 d5_coil_ir, d5_coil_or
      real*4 q5_d5_inner_dist, q5_d5_outer_dist, d5_gap_ir, d5_gap_or
      real*4 save_this_sine, save_this_cos

      parameter (d5_iron_ir = 5.97e0, d5_iron_or = 1.3335e+01)
      parameter (d5_collar_ir = 5.00e0, d5_collar_or = 5.97e0)
      parameter (d5_coil_ir = 3.995e0, d5_coil_or = 5.00e0)
      parameter (d5_gap_ir = 3.650, d5_gap_or = 3.995e0)
      parameter (d5_tube_ir = 3.455e0, d5_tube_or = 3.650e0)
      parameter (d5_theta_deg = 0.210536e0)
      parameter (d5_inner_length = 3.46e+02)
      parameter (d5_outer_length = 4.355e+02)
      parameter (q5_d5_inner_dist = 7.3980e+02)
      parameter (q5_d5_outer_dist = 7.4122e+02)
      parameter ( d5_out_y_coord = 0.0e0, d5_in_y_coord = 0.0e0)

C     Declare arrays for description of Q6 below.  

      real*4 q6_in_x_coord, q6_in_y_coord, q6_in_z_coord
      real*4 q6_out_x_coord, q6_out_y_coord, q6_out_z_coord
      real*4 q6_length, d5_q6_in_dist, d5_q6_out_dist
      real*4 d5_d6_theta_deg

      data d5_q6_in_dist / 7.3980e+02/
      data d5_q6_out_dist / 7.4122e+02/
      parameter ( q6_length = 1.70e+02, d5_d6_theta_deg = 1.84431 )
      parameter (q6_in_y_coord = 0.0e0, q6_out_y_coord = 0.0e0)

C     Declare arrays for description of D6 below. 

      real*4 d6_in_x_coord, d6_in_y_coord, d6_in_z_coord
      real*4 d6_out_x_coord, d6_out_y_coord, d6_out_z_coord
      real*4 d6_length, d5_d6_in_dist, d5_d6_out_dist, d6_par(3)

      data d5_d6_in_dist / 1.80496e+03 / 
      data d5_d6_out_dist / 1.80693e+03 /
      parameter ( d6_length = 1.475e+02 )
      parameter (d6_in_y_coord = 0.0e0, d6_out_y_coord = 0.0e0)

C     Declare arrays for description of Q7 below.

      real*4 q7_in_x_coord, q7_in_y_coord, q7_in_z_coord
      real*4 q7_out_x_coord, q7_out_y_coord, q7_out_z_coord
      real*4 q7_length, d6_q7_in_dist, d6_q7_out_dist
      real*4 d6_d8_theta_deg

      data d6_q7_in_dist /3.66166e+02/
      data d6_q7_out_dist / 3.66979e+02/
      parameter ( q7_length = 1.25e+02 )
      parameter (q7_in_y_coord = 0.0e0, q7_out_y_coord = 0.0e0)
      parameter (d6_d8_theta_deg = 2.54106e0)

C     Declare arrays for Q8 below. 

      real*4 q8_in_x_coord, q8_in_y_coord, q8_in_z_coord
      real*4 q8_out_x_coord, q8_out_y_coord, q8_out_z_coord
      real*4 q8_length, d6_q8_in_dist, d6_q8_out_dist

      data d6_q8_in_dist / 1.91847e+03/
      data d6_q8_out_dist / 1.91928e+03/
      parameter ( q8_length = 1.30e+02 )
      parameter (q8_in_y_coord = 0.0e0, q8_out_y_coord = 0.0e0)

C     Declare arrays for D8 below.

      real*4 d8_in_x_coord, d8_in_y_coord, d8_in_z_coord
      real*4 d8_out_x_coord, d8_out_y_coord, d8_out_z_coord
      real*4 d8_length, d6_d8_in_dist, d6_d8_out_dist, d8_par(3)

      data d6_d8_in_dist /2.61824e+03/
      data d6_d8_out_dist /2.62054e+03/
      parameter ( d8_length = 4.725e+02 )
      parameter (d8_in_y_coord = 0.0e0, d8_out_y_coord = 0.0e0)

C     Declare arrays for Q9 below.  

      real*4 q9_in_x_coord, q9_in_y_coord, q9_in_z_coord
      real*4 q9_out_x_coord, q9_out_y_coord, q9_out_z_coord
      real*4 q9_length, d8_q9_in_dist, d8_q9_out_dist
      real*4 d8_d9_theta_deg

      data d8_q9_in_dist / 7.3966e+02/
      data d8_q9_out_dist / 7.4141e+02/
      parameter ( q9_length = 1.70e+02 )
      parameter (q9_in_y_coord = 0.0e0, q9_out_y_coord = 0.0e0)
      parameter (d8_d9_theta_deg = 4.77124)

C     Declare arrays for D9 below. 

      real*4 d9_in_x_coord, d9_in_y_coord, d9_in_z_coord
      real*4 d9_out_x_coord, d9_out_y_coord, d9_out_z_coord
      real*4 d9_length, d8_d9_in_dist, d8_d9_out_dist, d9_par(3)

      data d8_d9_in_dist / 1.80482e+03 / 
      data d8_d9_out_dist / 1.80712e+03 /
      parameter ( d9_length = 1.475e+02 )
      parameter (d9_in_y_coord = 0.0e0, d9_out_y_coord = 0.0e0)

c     Addition for the use of the PISA geometry namelist control file phnx.par
c     Parameters for RHIC_MAGNET_INSTALL will be in the rhic_mag segment
c     Default is no beam line absorber

      integer iflag_lead_abs /0/  ! 0 is no shield; otherwise there is a shield
      real*4  lead_abs_par(3) /12.0, 400., 1.0/   ! defaults from Andrew
      real*4  lead_abs_pos(3) /0.0, 0.0, 3.75778e+03/ ! original values
      integer nmed_g10 /24/  ! tracking medium number for G10 
      integer nmed_dx /1199/   ! tracking medium for DX magnet
      integer nmed_cu /15/   ! tracking medium for Copper
      integer nmed_lead_abs /1198/   ! lead absorber tracking medium

c     Added for the Lead Muon Arm shield.....AAR  1/14/96
c     Put into namelist set ... CFM 3/1/96

      namelist /rhic_mag/ nmed_g10, nmed_dx, nmed_cu, 
     $                    lead_abs_par, lead_abs_pos, nmed_lead_abs,
     $                    iflag_lead_abs

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun


c     Begin execution


      write( *,* ) 'rhic_magnet_install - ',
     +             'reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = rhic_mag, err = 999 )


C     Install floor.

      nmed = 442
      floor_par (1) = 6.50e+02
      floor_par (2) = 4.318e+01 
      floor_par (3) = 9.450e+03
      call gsvolu ('FLOR', 'BOX ', nmed, floor_par, 3, ivolu)
      call gsatt ('FLOR', 'COLO', 7)
      call gspos ('FLOR', 1, 'HALL', x_coord_floor, y_coord_floor,
     $     z_coord_floor, 0, 'ONLY')
      call gspos ('FLOR', 2, 'HALL', x_coord_floor, y_coord_floor,
     $     -z_coord_floor, 0, 'ONLY')

C     Install wall, section 1.

      x_coord_wall = 3.0480e+02
      z_coord_wall = 5.60e+03  
      wall_par (1) = 5.334e+01 
      wall_par (2) = 1.67e+02 
      wall_par (3) = 4.40e+03
      call gsvolu ('WAL1', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL1', 'COLO', 1)
      call gspos ('WAL1', 1, 'HALL', x_coord_wall, y_coord_wall,
     $     z_coord_wall, 0, 'ONLY')
      call gspos ('WAL1', 2, 'HALL', - x_coord_wall, y_coord_wall,
     $     z_coord_wall, 0, 'ONLY')
      call gspos ('WAL1', 3, 'HALL', - x_coord_wall, y_coord_wall,
     $     -z_coord_wall, 0, 'ONLY')
      call gspos ('WAL1', 4, 'HALL',  x_coord_wall, y_coord_wall,
     $     -z_coord_wall, 0, 'ONLY')

C     Install wall, section 2. 

      x_coord_wall_2 = 3.5480e+02  
      z_coord_wall_2 = 1.10e+04
      wall_par (3) = 0.999e+03
      call gsvolu ('WAL2', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL2', 'COLO', 1)
      call gspos ('WAL2', 1, 'HALL', x_coord_wall_2, y_coord_wall,
     $     z_coord_wall_2, 0, 'ONLY')
      call gspos ('WAL2', 2, 'HALL', x_coord_wall_2, y_coord_wall,
     $     -z_coord_wall_2, 0, 'ONLY')
      x_coord_wall_2 = -2.5480e+02
      call gspos ('WAL2', 3, 'HALL', x_coord_wall_2, y_coord_wall,
     $     z_coord_wall_2, 0, 'ONLY')
      call gspos ('WAL2', 4, 'HALL',  x_coord_wall_2, y_coord_wall,
     $     -z_coord_wall_2, 0, 'ONLY')

C     Install wall, section 3.

      x_coord_wall_3 = 4.0480e+02  
      z_coord_wall_3 = 1.25e+04
      wall_par (3) = 0.499e+03
      call gsvolu ('WAL3', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL3', 'COLO', 1)
      call gspos ('WAL3', 1, 'HALL', x_coord_wall_3, y_coord_wall,
     $     z_coord_wall_3, 0, 'ONLY')
      call gspos ('WAL3', 2, 'HALL', x_coord_wall_3, y_coord_wall,
     $     -z_coord_wall_3, 0, 'ONLY')
      x_coord_wall_3 = -2.0480e+02
      call gspos ('WAL3', 3, 'HALL', x_coord_wall_3, y_coord_wall,
     $     z_coord_wall_3, 0, 'ONLY')
      call gspos ('WAL3', 4, 'HALL',  x_coord_wall_3, y_coord_wall,
     $     -z_coord_wall_3, 0, 'ONLY')

C     Install wall, section 4.

      x_coord_wall_4 = 4.5480e+02  
      z_coord_wall_4 = 1.35e+04
      call gsvolu ('WAL4', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL4', 'COLO', 1)
      call gspos ('WAL4', 1, 'HALL', x_coord_wall_4, y_coord_wall,
     $     z_coord_wall_4, 0, 'ONLY')
      call gspos ('WAL4', 2, 'HALL', x_coord_wall_4, y_coord_wall,
     $     -z_coord_wall_4, 0, 'ONLY')
      x_coord_wall_4 = -1.5480e+02
      call gspos ('WAL4', 3, 'HALL', x_coord_wall_4, y_coord_wall,
     $     z_coord_wall_4, 0, 'ONLY')
      call gspos ('WAL4', 4, 'HALL',  x_coord_wall_4, y_coord_wall,
     $     -z_coord_wall_4, 0, 'ONLY')

C     Install wall, section 5.

      x_coord_wall_5 = 5.0480e+02  
      z_coord_wall_5 = 1.45e+04
      call gsvolu ('WAL5', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL5', 'COLO', 1)
      call gspos ('WAL5', 1, 'HALL', x_coord_wall_5, y_coord_wall,
     $     z_coord_wall_5, 0, 'ONLY')
      call gspos ('WAL5', 2, 'HALL', x_coord_wall_5, y_coord_wall,
     $     -z_coord_wall_5, 0, 'ONLY')
      x_coord_wall_5 = -1.0480e+02
      call gspos ('WAL5', 3, 'HALL', x_coord_wall_5, y_coord_wall,
     $     z_coord_wall_5, 0, 'ONLY')
      call gspos ('WAL5', 4, 'HALL',  x_coord_wall_5, y_coord_wall,
     $     -z_coord_wall_5, 0, 'ONLY')

C     Install wall, section 6.

      x_coord_wall_6 = 5.5480e+02  
      z_coord_wall_6 = 1.55e+04
      call gsvolu ('WAL6', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL6', 'COLO', 1)
      call gspos ('WAL6', 1, 'HALL', x_coord_wall_6, y_coord_wall,
     $     z_coord_wall_6, 0, 'ONLY')
      call gspos ('WAL6', 2, 'HALL', x_coord_wall_6, y_coord_wall,
     $     -z_coord_wall_6, 0, 'ONLY')
      x_coord_wall_6 = -5.480e+01
      call gspos ('WAL6', 3, 'HALL', x_coord_wall_6, y_coord_wall,
     $     z_coord_wall_6, 0, 'ONLY')
      call gspos ('WAL6', 4, 'HALL',  x_coord_wall_6, y_coord_wall,
     $     -z_coord_wall_6, 0, 'ONLY')

C     Install wall, section 7.

      x_coord_wall_7 = 6.0480e+02  
      z_coord_wall_7 = 1.65e+04
      call gsvolu ('WAL7', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL7', 'COLO', 1)
      call gspos ('WAL7', 1, 'HALL', x_coord_wall_7, y_coord_wall,
     $     z_coord_wall_7, 0, 'ONLY')
      call gspos ('WAL7', 2, 'HALL', x_coord_wall_7, y_coord_wall,
     $     -z_coord_wall_7, 0, 'ONLY')
      x_coord_wall_7 = -0.480e+01
      call gspos ('WAL7', 3, 'HALL', x_coord_wall_7, y_coord_wall,
     $     z_coord_wall_7, 0, 'ONLY')
      call gspos ('WAL7', 4, 'HALL',  x_coord_wall_7, y_coord_wall,
     $     -z_coord_wall_7, 0, 'ONLY')

C     Install wall, section 8.

      x_coord_wall_8 = 6.5480e+02  
      z_coord_wall_8 = 1.75e+04
      call gsvolu ('WAL8', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL8', 'COLO', 1)
      call gspos ('WAL8', 1, 'HALL', x_coord_wall_8, y_coord_wall,
     $     z_coord_wall_8, 0, 'ONLY')
      call gspos ('WAL8', 2, 'HALL', x_coord_wall_8, y_coord_wall,
     $     -z_coord_wall_8, 0, 'ONLY')
      x_coord_wall_8 = 4.520e+01
      call gspos ('WAL8', 3, 'HALL', x_coord_wall_8, y_coord_wall,
     $     z_coord_wall_8, 0, 'ONLY')
      call gspos ('WAL8', 4, 'HALL',  x_coord_wall_8, y_coord_wall,
     $     -z_coord_wall_8, 0, 'ONLY')

C     Install wall, section 9.

      x_coord_wall_9 = 7.0480e+02  
      z_coord_wall_9 = 1.85e+04
      call gsvolu ('WAL9', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WAL9', 'COLO', 1)
      call gspos ('WAL9', 1, 'HALL', x_coord_wall_9, y_coord_wall,
     $     z_coord_wall_9, 0, 'ONLY')
      call gspos ('WAL9', 2, 'HALL', x_coord_wall_9, y_coord_wall,
     $     -z_coord_wall_9, 0, 'ONLY')
      x_coord_wall_9 = 9.520e+01
      call gspos ('WAL9', 3, 'HALL', x_coord_wall_9, y_coord_wall,
     $     z_coord_wall_9, 0, 'ONLY')
      call gspos ('WAL9', 4, 'HALL',  x_coord_wall_9, y_coord_wall,
     $     -z_coord_wall_9, 0, 'ONLY')

C     Install wall, section 10.

      x_coord_wall_10 = 7.5480e+02  
      z_coord_wall_10 = 1.95e+04
      call gsvolu ('WALA', 'BOX ', nmed, wall_par, 3, ivolu)
      call gsatt ('WALA', 'COLO', 1)
      call gspos ('WALA', 1, 'HALL', x_coord_wall_10, y_coord_wall,
     $     z_coord_wall_10, 0, 'ONLY')
      call gspos ('WALA', 2, 'HALL', x_coord_wall_10, y_coord_wall,
     $     -z_coord_wall_10, 0, 'ONLY')
      x_coord_wall_10 = 1.4520e+02
      call gspos ('WALA', 3, 'HALL', x_coord_wall_10, y_coord_wall,
     $     z_coord_wall_10, 0, 'ONLY')
      call gspos ('WALA', 4, 'HALL',  x_coord_wall_10, y_coord_wall,
     $     -z_coord_wall_10, 0, 'ONLY')


C     Begin installation of ceiling.

      ceiling_par (1) = 6.50e+02 
      ceiling_par (2) = 8.382e+01 
      ceiling_par (3) = 9.45e+03
      call gsvolu ('CEIL', 'BOX ', nmed, ceiling_par, 3, ivolu)
      call gspos ('CEIL', 1, 'HALL', x_coord_ceiling, y_coord_ceiling,
     $     z_coord_ceiling, 0, 'ONLY')
      call gspos ('CEIL', 2, 'HALL', x_coord_ceiling, y_coord_ceiling,
     $     - z_coord_ceiling, 0, 'ONLY')

C     Begin installation of magnets.  Start with DX.

      dx_par(1) = dx_iron_ir
      dx_par(2) = dx_iron_or
      dx_par(3) = dx_length
      nmed = nmed_dx
      call gsvolu ('DXFE', 'TUBE', nmed, dx_par, 3, ivolu)
      call gsatt ('DXFE', 'COLO', 6)
      call gspos ('DXFE', 1, 'HALL', dx_x_coord, dx_y_coord,
     $     dx_z_coord, 0, 'ONLY')

C     Unlike all other magnets, the collar of DX is made of stainless
C     steel.  Eventually we must define stainless steel, but for the
C     moment we will regard the collar as being material 900. 

      dx_par(1) = dx_collar_ir
      dx_par(2) = dx_collar_or
      nmed = 900
      call gsvolu ('DXCL', 'TUBE', nmed, dx_par, 3, ivolu)
      call gsatt ('DXCL', 'COLO', 6)

C     Dont clutter up the graphics output by displaying anything but
C     the iron.

      call gsatt ('DXCL', 'SEEN', 0)
      call gspos ('DXCL', 1, 'DXFE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     The coil consists of superconducting Niobium Titanium wire embedded
C     in a Copper matrix.  Because the Cu constitutes approximately 64%
C     of the coil, we will temporarily regard the coil as being Cu. 

      dx_par(1) = dx_coil_ir
      dx_par(2) = dx_coil_or
      nmed = nmed_cu
      call gsvolu ('DXCO', 'TUBE', nmed, dx_par, 3, ivolu)
      call gsatt ('DXCO', 'COLO', 6)
      call gsatt ('DXCO', 'SEEN', 0)
      call gspos ('DXCO', 1, 'DXCL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     The geometry of the beam tube is complicated.  In fact there isnt 
C     even a single tube in DX.  The engineers thought it a bad idea 
C     for a tube at 4 Kelvin to be exposed to room temperature, so they 
C     buried the tube in a second tube and stuffed the space between the
C     two tubes with superinsulation.   According to Jack Sondericker,
C     the dimensions of the two tubes are as follows:
C     OD of outer tube = 17.40 cm       OD of inner tube = 14.29 cm
C     wall thickness   = 0.559 cm        wall thickness = 0.160 cm
C     We will use just one tube and set the thickness of the tube to
C     the sum of the thicknesses shown above.

      dx_par(1) = dx_tube_ir
      dx_par(2) = dx_tube_or
      nmed = 900
      call gsvolu ('DXTU', 'TUBE', nmed, dx_par, 3, ivolu)
      call gsatt ('DXTU', 'COLO', 6)
      call gsatt ('DXTU', 'SEEN', 0)
      call gspos ('DXTU', 1, 'DXCO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      dx_par(1) = 0.0e0
      dx_par(2) = dx_tube_ir
      nmed = 16
      call gsvolu ('DXVA', 'TUBE', nmed, dx_par, 3, ivolu)
      call gsatt ('DXTU', 'SEEN', 0)
      call gspos ('DXVA', 1, 'DXTU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')


C     Because DX is common to both arcs on the same side of the  
C     interaction point, it is only necessary that we create a second
C     copy on the other side of the interaction point.

      call gspos ('DXFE',2, 'HALL', dx_x_coord, dx_y_coord, 
     $     -dx_z_coord, 0, 'ONLY') 

C     First, install the iron of D0.  Note that the coordinates of the
C     geometrical center are calculated with respect to the coordinates 
C     of the geometrical center of DX.

C     Note that the functions sind and cosd are non-standard functions
C     that may not be defined on other machines. 

      d0_x_coord = -  dist_dx_d0 * sind (theta_d0_deg)
      d0_z_coord = dx_z_coord + dist_dx_d0 * cosd (theta_d0_deg)


      d0_par(1) = d0_iron_ir
      d0_par(2) = d0_iron_or
      d0_par(3) = d0_length
      nmed = nmed_dx
      call gsvolu ('D0FE', 'TUBE', nmed, d0_par, 3, ivolu)
      call gsatt ('D0FE', 'COLO', 6)

C     The coordinate system of D0 is rotated with respect to the 
C     master coordinate sytem, so define the appropriate rotation matrix.
C     Note that the following rotation describes the magnet in the
C     outside arc.

      angle1 = 90.0e0 - theta_d0_deg
      irot = irot + 1
      irot1 = irot
      call gsrotm (irot1, angle1, 0.0e0, 90.0e0, 90.0e0, theta_d0_deg,
     $     180.0e0)
      call gspos ('D0FE', 1, 'HALL', d0_x_coord, d0_y_coord,
     $     d0_z_coord, irot1, 'ONLY')

C     The collar of D0 is made from RX630 (Phenolic) molding.  We 
C     regard phenoic as being G-10 at the moment.

      d0_par(1) = d0_collar_ir
      d0_par(2) = d0_collar_or
      nmed = nmed_g10
      call gsvolu ('D0CL', 'TUBE', nmed, d0_par, 3, ivolu)
      call gsatt ('D0CL', 'COLO', 6)
      call gsatt ('D0CL', 'SEEN', 0)
      call gspos ('D0CL', 1, 'D0FE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil to D0.  

      d0_par(1) = d0_coil_ir
      d0_par(2) = d0_coil_or
      nmed = nmed_cu
      call gsvolu ('D0CO', 'TUBE', nmed, d0_par, 3, ivolu)
      call gsatt ('D0CO', 'COLO', 6)
      call gsatt ('D0CO', 'SEEN', 0)
      call gspos ('D0CO', 1, 'D0CL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube.  The dimensions of the tube were 
C     provided by Jack Sondericker, and there seems to be some 
C     inconsistency between these numbers and the RHIC design manual. 

      d0_par(1) = d0_tube_ir
      d0_par(2) = d0_tube_or
      nmed = 900
      call gsvolu ('D0TU', 'TUBE', nmed, d0_par, 3, ivolu)
      call gsatt ('D0TU', 'COLO', 6)
      call gsatt ('D0TU', 'SEEN', 0)
      call gspos ('D0TU', 1, 'D0CO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d0_par(1) = 0.0e0
      d0_par(2) = d0_tube_ir
      nmed = 16
      call gsvolu ('D0VA', 'TUBE', nmed, d0_par, 3, ivolu)
      call gsatt ('D0TU', 'SEEN', 0)
      call gspos ('D0VA', 1, 'D0TU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     There are two D0 magnets on each side of the interaction point, 
C     making a total of four magnets in all.  Note the necessity for 
C     additional rotation matrices.  

C     First, install the second magnet on the same side of the interaction
C     point.  

      angle1 = 90.0e0 + theta_d0_deg
      irot = irot + 1
      irot2 = irot
      call gsrotm (irot2, angle1, 0.0e0, 90.0e0, 90.0e0, theta_d0_deg,
     $     0.0e0)
      call gspos ('D0FE',2, 'HALL', -d0_x_coord, d0_y_coord, 
     $     d0_z_coord, irot2, 'ONLY')  

C     Now install the two magnets on the other side of the interaction
C     point.

      angle1 = 90.0e0 + theta_d0_deg
      angle2 = 180.0e0 - theta_d0_deg
      irot = irot + 1
      irot3 = irot
      call gsrotm (irot3, angle1, 180.0, 90.0, 90.0, angle2, 0.0e0)
      call gspos ('D0FE',3, 'HALL', -d0_x_coord, d0_y_coord, 
     $     -d0_z_coord, irot3, 'ONLY') 
      angle1 = 90.0e0 - theta_d0_deg
      angle2 = 180.0 - theta_d0_deg
      irot = irot + 1
      irot4 = irot
      call gsrotm (irot4, angle1, 180.0, 90.0, 90.0, angle2, 180.0)
      call gspos ('D0FE',4, 'HALL', d0_x_coord, d0_y_coord, 
     $     -d0_z_coord, irot4, 'ONLY') 

C     Install Q1.  First, calculate the coordinates of the geometrical
C     center of the magnet. 

      q1_x_coord = d0_x_coord - d0_q1_dist * sind (qla_theta_deg)
      q1_z_coord = d0_z_coord + d0_q1_dist * cosd (qla_theta_deg)
      qla_par(1) = qla_iron_ir
      qla_par(2) = qla_iron_or
      qla_par(3) = q1_length
      nmed = nmed_dx
      call gsvolu ('Q1FE', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q1FE', 'COLO', 6)
      angle1 = 90.0e0 - qla_theta_deg
      angle2 = qla_theta_deg
      irot = irot + 1 
      irot5 = irot
      call gsrotm (irot5, angle1, 0.0e0, 90.0e0, 90.0e0, angle2,
     $     180.0e0)
      call gspos ('Q1FE', 1, 'HALL', q1_x_coord, q1_y_coord,
     $     q1_z_coord, irot5, 'ONLY')

C     The collar of the quads is made from RX630 molding.  We regard 
C     RX630 as being G-10 at the moment.

      qla_par(1) = qla_collar_ir
      qla_par(2) = qla_collar_or
      nmed = nmed_g10
      call gsvolu ('Q1CL', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q1CL', 'COLO', 6)
      call gsatt ('Q1CL', 'SEEN', 0)
      call gspos ('Q1CL', 1, 'Q1FE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil to Q1.  

      qla_par(1) = qla_coil_ir
      qla_par(2) = qla_coil_or
      nmed = nmed_cu
      call gsvolu ('Q1CO', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q1CO', 'COLO', 6)
      call gsatt ('Q1CO', 'SEEN', 0)
      call gspos ('Q1CO', 1, 'Q1CL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube.  The dimensions of the tube were 
C     provided by Jack Sondericker, and there seems to be a slight
C     inconsistency between these numbers and the RHIC design manual. 

      qla_par(1) = qla_tube_ir
      qla_par(2) = qla_tube_or
      nmed = 900
      call gsvolu ('Q1TU', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q1TU', 'COLO', 6)
      call gsatt ('Q1TU', 'SEEN', 0)
      call gspos ('Q1TU', 1, 'Q1CO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qla_par(1) = 0.0e0
      qla_par(2) = qla_tube_ir
      nmed = 16
      call gsvolu ('Q1VA', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q1TU', 'SEEN', 0)
      call gspos ('Q1VA', 1, 'Q1TU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     There are two Q1 magnets on each side of the interaction point 
C     making a total of four magnets in all.  Note the necessity for 
C     rotation matrices.  

      angle1 = 90.0e0 +  qla_theta_deg
      angle2 = qla_theta_deg
      irot = irot + 1
      irot6 = irot
      call gsrotm ( irot6, angle1, 0.0e0, 90.0e0, 90.0e0, 
     $     angle2, 0.0e0)
      call gspos ('Q1FE',2, 'HALL', -q1_x_coord, q1_y_coord, 
     $     q1_z_coord, irot6, 'ONLY')
      angle1 = 90.0e0 + qla_theta_deg
      angle2 = 180.0e0 - qla_theta_deg
      irot = irot + 1
      irot7 = irot
      call gsrotm (irot7, angle1, 180.0e0, 90.0e0, 90.0e0, angle2,
     $     0.0e0)
      call gspos ('Q1FE',3, 'HALL', -q1_x_coord, q1_y_coord, 
     $     -q1_z_coord, irot7, 'ONLY') 
      angle1 = 90.0e0 - qla_theta_deg

C     In principle the following line shouldnt be necessary, but it 
C     certainly seems to be necessary, for whatever reason.

      angle2 = 180.0e0 - qla_theta_deg
      irot = irot + 1
      irot8 = irot
      call gsrotm (irot8, angle1, 180.0e0, 90.0e0, 90.0e0, angle2,
     $     180.0e0)
      call gspos ('Q1FE',4, 'HALL', q1_x_coord, q1_y_coord, 
     $     -q1_z_coord, irot8, 'ONLY')

C     Install Q2.  First, calculate the coordinates of the geometrical
C     center of the magnet. 

      q2_x_coord = q1_x_coord - q1_q2_dist * sind (qla_theta_deg)
      q2_z_coord = q1_z_coord + q1_q2_dist * cosd (qla_theta_deg)
      qla_par(1) = qla_iron_ir
      qla_par(2) = qla_iron_or
      qla_par(3) = q2_length
      nmed = nmed_dx
      call gsvolu ('Q2FE', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q2FE', 'COLO', 6)
      call gspos ('Q2FE', 1, 'HALL', q2_x_coord, q2_y_coord,
     $     q2_z_coord, irot5, 'ONLY')

C     Install the collar to Q2.

      qla_par(1) = qla_collar_ir
      qla_par(2) = qla_collar_or
      nmed = nmed_g10
      call gsvolu ('Q2CL', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q2CL', 'COLO', 6)
      call gsatt ('Q2CL', 'SEEN', 0)
      call gspos ('Q2CL', 1, 'Q2FE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil to Q2.  

      qla_par(1) = qla_coil_ir
      qla_par(2) = qla_coil_or
      nmed = nmed_cu
      call gsvolu ('Q2CO', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q2CO', 'COLO', 6)
      call gsatt ('Q2CO', 'SEEN', 0)
      call gspos ('Q2CO', 1, 'Q2CL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube. 

      qla_par(1) = qla_tube_ir
      qla_par(2) = qla_tube_or
      nmed = 900
      call gsvolu ('Q2TU', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q2TU', 'COLO', 6)
      call gsatt ('Q2TU', 'SEEN', 0)
      call gspos ('Q2TU', 1, 'Q2CO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qla_par(1) = 0.0e0
      qla_par(2) = qla_tube_ir
      nmed = 16
      call gsvolu ('Q2VA', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q2TU', 'SEEN', 0)
      call gspos ('Q2VA', 1, 'Q2TU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the other Q2 magnets.

      call gspos ('Q2FE',2, 'HALL', -q2_x_coord, q2_y_coord, 
     $     q2_z_coord, irot6, 'ONLY')
      call gspos ('Q2FE',3, 'HALL', -q2_x_coord, q2_y_coord, 
     $     -q2_z_coord, irot7, 'ONLY') 
      call gspos ('Q2FE',4, 'HALL', q2_x_coord, q2_y_coord, 
     $     -q2_z_coord, irot8, 'ONLY') 

C     Install Q3.  First, calculate the coordinates of the geometrical
C     center of the magnet. 

      q3_x_coord = q2_x_coord - q2_q3_dist * sind (qla_theta_deg)
      q3_z_coord = q2_z_coord + q2_q3_dist * cosd (qla_theta_deg)
      qla_par(1) = qla_iron_ir
      qla_par(2) = qla_iron_or
      qla_par(3) = q3_length
      nmed = nmed_dx
      call gsvolu ('Q3FE', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q3FE', 'COLO', 6)
      call gspos ('Q3FE', 1, 'HALL', q3_x_coord, q3_y_coord,
     $     q3_z_coord, irot5, 'ONLY')

C     Install the collar inside Q3.

      qla_par(1) = qla_collar_ir
      qla_par(2) = qla_collar_or
      nmed = nmed_g10
      call gsvolu ('Q3CL', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q3CL', 'COLO', 6)
      call gsatt ('Q3CL', 'SEEN', 0)
      call gspos ('Q3CL', 1, 'Q3FE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside Q3.  

      qla_par(1) = qla_coil_ir
      qla_par(2) = qla_coil_or
      nmed = nmed_cu
      call gsvolu ('Q3CO', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q3CO', 'COLO', 6)
      call gsatt ('Q3CO', 'SEEN', 0)
      call gspos ('Q3CO', 1, 'Q3CL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside Q3. 

      qla_par(1) = qla_tube_ir
      qla_par(2) = qla_tube_or
      nmed = 900
      call gsvolu ('Q3TU', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q3TU', 'COLO', 6)
      call gsatt ('Q3TU', 'SEEN', 0)
      call gspos ('Q3TU', 1, 'Q3CO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qla_par(1) = 0.0e0
      qla_par(2) = qla_tube_ir
      nmed = 16
      call gsvolu ('Q3VA', 'TUBE', nmed, qla_par, 3, ivolu)
      call gsatt ('Q3TU', 'SEEN', 0)
      call gspos ('Q3VA', 1, 'Q3TU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the other Q3 magnets.

      call gspos ('Q3FE',2, 'HALL', -q3_x_coord, q3_y_coord, 
     $     q3_z_coord, irot6, 'ONLY')
      call gspos ('Q3FE',3, 'HALL', -q3_x_coord, q3_y_coord, 
     $     -q3_z_coord, irot7, 'ONLY') 
      call gspos ('Q3FE',4, 'HALL', q3_x_coord, q3_y_coord, 
     $     -q3_z_coord, irot8, 'ONLY') 


C     The code that follows was inserted in order to study the effect of
C     an absorber upon produced particles that originate in region 3.
C     The coordinates of the center of the slab are chosen to coincide
C     with the position of the valve that separates region 2 from region
C     3. The end of this special insertion is marked with a comment line
C     that contains the special characters ******

c     nmed = 41
c     lead_abs_par (1) = 12.0e0
c     lead_abs_par (2) = 4.00e+02
c     lead_abs_par (3) = 1.0e0

c     Code above for setting parameters is now controlled via namelist

      if(iflag_lead_abs.ne.0)then
         write(6,1)nmed_lead_abs,lead_abs_par,lead_abs_pos
 1       format(//,'  RHIC_MAGNET_INSTALL <I>: beam line absorber',
     $          ' installed',/,3x,' Tracking medium = ',i4,/,3x,
     $          ' Tube dimensions (cm) = ',3f6.1,/,3x,
     $          ' Tube position (cm) = ',3e14.5,/)
         nmed = nmed_lead_abs
         call gsvolu ('ABPB', 'TUBE', nmed, lead_abs_par, 3, ivolu)
         call gsatt ('ABPB', 'SEEN', 1)

c        call gspos ('ABPB', 1, 'HALL', 0.0e0, 0.0e0, 3.75778e+03, 0, 
c     $        'ONLY')

c     Lead absorber position now determined from the phnx.par file

         call gspos ('ABPB', 1, 'HALL', lead_abs_pos(1),
     $               lead_abs_pos(2), lead_abs_pos(3), 0, 'ONLY')
      else
         write(6,2)
 2       format(/,' RHIC_MAGNET_INSTALL <I>: NO beam line absorber',/)
      endif  ! check on installing the lead absorber shield

c     Install Q4.  First, calculate the coordinates of the geometrical
C     center of the magnet. 

      q4_x_coord = q3_x_coord - q3_q4_dist * sind (qsa_theta_deg)
      q4_z_coord = q3_z_coord + q3_q4_dist * cosd (qsa_theta_deg)
      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q4_length
      nmed = nmed_dx
      call gsvolu ('Q4FE', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q4FE', 'COLO', 6)
      call gspos ('Q4FE', 1, 'HALL', q4_x_coord, q4_y_coord,
     $     q4_z_coord, irot5, 'ONLY')

C     Install the collar inside Q4.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q4CL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q4CL', 'COLO', 6)
      call gsatt ('Q4CL', 'SEEN', 0)
      call gspos ('Q4CL', 1, 'Q4FE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside Q4.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q4CO', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q4CO', 'COLO', 6)
      call gsatt ('Q4CO', 'SEEN', 0)
      call gspos ('Q4CO', 1, 'Q4CL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q4GA', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q4GA', 'COLO', 6)
      call gsatt ('Q4GA', 'SEEN', 0)
      call gspos ('Q4GA', 1, 'Q4CO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside Q4. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q4TU', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q4TU', 'COLO', 6)
      call gsatt ('Q4TU', 'SEEN', 0)
      call gspos ('Q4TU', 1, 'Q4GA', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q4VA', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q4TU', 'SEEN', 0)
      call gspos ('Q4VA', 1, 'Q4TU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the other Q4 magnets.

      call gspos ('Q4FE',2, 'HALL', -q4_x_coord, q4_y_coord, 
     $     q4_z_coord, irot6, 'ONLY')
      call gspos ('Q4FE',3, 'HALL', -q4_x_coord, q4_y_coord, 
     $     -q4_z_coord, irot7, 'ONLY') 
      call gspos ('Q4FE',4, 'HALL', q4_x_coord, q4_y_coord, 
     $     -q4_z_coord, irot8, 'ONLY') 

C     Install Q5.  First, calculate the coordinates of the geometrical
C     center of the magnet. 

      q5_x_coord = q4_x_coord - q4_q5_dist * sind (qsa_theta_deg)
      q5_z_coord = q4_z_coord + q4_q5_dist * cosd (qsa_theta_deg)
      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q5_length
      nmed = nmed_dx
      call gsvolu ('Q5FE', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q5FE', 'COLO', 6)
      call gspos ('Q5FE', 1, 'HALL', q5_x_coord, q5_y_coord,
     $     q5_z_coord, irot5, 'ONLY')

C     Install the collar inside Q5.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q5CL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q5CL', 'COLO', 6)
      call gsatt ('Q5CL', 'SEEN', 0)
      call gspos ('Q5CL', 1, 'Q5FE', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside Q5.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q5CO', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q5CO', 'COLO', 6)
      call gsatt ('Q5CO', 'SEEN', 0)
      call gspos ('Q5CO', 1, 'Q5CL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q5GA', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q5GA', 'COLO', 6)
      call gsatt ('Q5GA', 'SEEN', 0)
      call gspos ('Q5GA', 1, 'Q5CO', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside Q5. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q5TU', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q5TU', 'COLO', 6)
      call gsatt ('Q5TU', 'SEEN', 0)
      call gspos ('Q5TU', 1, 'Q5GA', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q5VA', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q5TU', 'SEEN', 0)
      call gspos ('Q5VA', 1, 'Q5TU', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the other Q5 magnets.

      call gspos ('Q5FE',2, 'HALL', -q5_x_coord, q5_y_coord, 
     $     q5_z_coord, irot6, 'ONLY')
      call gspos ('Q5FE',3, 'HALL', -q5_x_coord, q5_y_coord, 
     $     -q5_z_coord, irot7, 'ONLY') 
      call gspos ('Q5FE',4, 'HALL', q5_x_coord, q5_y_coord, 
     $     -q5_z_coord, irot8, 'ONLY') 

C     Install D5.  First, calculate the coordinates of the geometrical
C     centers of the two magnets.  First the outer arc. 

      save_this_sine = sind (d5_theta_deg)
      save_this_cos  = cosd (d5_theta_deg)
      d5_out_x_coord = q5_x_coord - q5_d5_outer_dist * 
     $     save_this_sine
      d5_out_z_coord = q5_z_coord + q5_d5_outer_dist * 
     $     save_this_cos

C     Now calculate the coordinates of the magnet in the inner arc. 

      d5_in_x_coord = - q5_x_coord + q5_d5_inner_dist * 
     $     save_this_sine
      d5_in_z_coord = q5_z_coord + q5_d5_inner_dist * 
     $     save_this_cos

C    Now install the magnet in the outer arc.

      d5_par(1) = d5_iron_ir
      d5_par(2) = d5_iron_or
      d5_par(3) = d5_outer_length
      nmed = nmed_dx
      call gsvolu ('D5OF', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5OF', 'COLO', 6)
      call gspos ('D5OF', 1, 'HALL', d5_out_x_coord, d5_out_y_coord,
     $     d5_out_z_coord, irot5, 'ONLY')

C     Install the collar inside D5.

      d5_par(1) = d5_collar_ir
      d5_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D5OC', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5OC', 'COLO', 6)
      call gsatt ('D5OC', 'SEEN', 0)
      call gspos ('D5OC', 1, 'D5OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside outer D5.  

      d5_par(1) = d5_coil_ir
      d5_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D5OL', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5OL', 'COLO', 6)
      call gsatt ('D5OL', 'SEEN', 0)
      call gspos ('D5OL', 1, 'D5OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the outer
C     magnet.

      d5_par(1) = d5_gap_ir
      d5_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D5OG', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5OG', 'COLO', 6)
      call gsatt ('D5OG', 'SEEN', 0)
      call gspos ('D5OG', 1, 'D5OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer D5. 

      d5_par(1) = d5_tube_ir
      d5_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D5OT', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5OT', 'COLO', 6)
      call gsatt ('D5OT', 'SEEN', 0)
      call gspos ('D5OT', 1, 'D5OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d5_par(1) = 0.0e0
      d5_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D5OV', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5OV', 'SEEN', 0)
      call gspos ('D5OV', 1, 'D5OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the inner D5 magnet.

      d5_par(1) = d5_iron_ir
      d5_par(2) = d5_iron_or
      d5_par(3) = d5_inner_length
      nmed = nmed_dx
      call gsvolu ('D5IF', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5IF', 'COLO', 6)
      call gspos ('D5IF', 1, 'HALL', d5_in_x_coord, d5_in_y_coord,
     $     d5_in_z_coord, irot6, 'ONLY')

C     Install the collar inside the inner D5.

      d5_par(1) = d5_collar_ir
      d5_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D5IC', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5IC', 'COLO', 6)
      call gsatt ('D5IC', 'SEEN', 0)
      call gspos ('D5IC', 1, 'D5IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside inner D5.  

      d5_par(1) = d5_coil_ir
      d5_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D5IL', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5IL', 'COLO', 6)
      call gsatt ('D5IL', 'SEEN', 0)
      call gspos ('D5IL', 1, 'D5IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the inner
C     D5 magnet.

      d5_par(1) = d5_gap_ir
      d5_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D5IG', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5IG', 'COLO', 6)
      call gsatt ('D5IG', 'SEEN', 0)
      call gspos ('D5IG', 1, 'D5IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner D5. 

      d5_par(1) = d5_tube_ir
      d5_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D5IT', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5IT', 'COLO', 6)
      call gsatt ('D5IT', 'SEEN', 0)
      call gspos ('D5IT', 1, 'D5IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d5_par(1) = 0.0e0
      d5_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D5IV', 'TUBE', nmed, d5_par, 3, ivolu)
      call gsatt ('D5IV', 'SEEN', 0)
      call gspos ('D5IV', 1, 'D5IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the corresponding magnets on the other side of the origin.

      call gspos ('D5IF',2, 'HALL', d5_in_x_coord, d5_in_y_coord, 
     $     -d5_in_z_coord, irot5, 'ONLY') 
      call gspos ('D5OF',2, 'HALL', d5_out_x_coord, d5_out_y_coord, 
     $     -d5_out_z_coord, irot6, 'ONLY') 

C     For purposes of alignment we will now change the values of the
C     variables d5_in_x_coord, d5_in_z_coord, d5_out_x_coord, and 
C     d5_out_z_coord.  We want to point the following magnets towards
C     the downstream end of D5, not towards its center.  At the same
C     time we do this, though, we must subtract the half lengths of the
C     magnets from d5_q6_in_dist, etc., in order to keep the distances
C     between magnets constant. 

      d5_in_x_coord = d5_in_x_coord + d5_inner_length * save_this_sine    
      d5_in_z_coord = d5_in_z_coord + d5_inner_length * save_this_cos
      d5_out_x_coord = d5_out_x_coord - d5_outer_length * 
     $     save_this_sine    
      d5_out_z_coord = d5_out_z_coord + d5_outer_length *
     $     save_this_cos
      d5_q6_in_dist = d5_q6_in_dist - d5_inner_length
      d5_q6_out_dist = d5_q6_out_dist - d5_outer_length
      d5_d6_in_dist = d5_d6_in_dist - d5_inner_length
      d5_d6_out_dist = d5_d6_out_dist - d5_outer_length

C     Install Q6 in the inner and outer arcs.  First, calculate the
C     coordinates of the centers. 

      save_this_sine = sind (d5_d6_theta_deg)
      save_this_cos = cosd (d5_d6_theta_deg)
      q6_in_x_coord = d5_in_x_coord + d5_q6_in_dist * 
     $     save_this_sine
      q6_in_z_coord = d5_in_z_coord + d5_q6_in_dist * 
     $     save_this_cos
      q6_out_x_coord = d5_out_x_coord + d5_q6_out_dist * 
     $     save_this_sine
      q6_out_z_coord = d5_out_z_coord + d5_q6_out_dist * 
     $     save_this_cos

C     Now install Q6 in the inner arc. Note that we need a new rotation 
C     matrix. 

      angle1 = 90.0e0 +  d5_d6_theta_deg
      angle2 = d5_d6_theta_deg
      irot = irot + 1
      irot9 = irot
      call gsrotm ( irot9, angle1, 0.0e0, 90.0e0, 90.0e0, 
     $     angle2, 0.0e0)
      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q6_length
      nmed = nmed_dx
      call gsvolu ('Q6IF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6IF', 'COLO', 6)
      call gspos ('Q6IF', 1, 'HALL', q6_in_x_coord, q6_in_y_coord,
     $     q6_in_z_coord, irot9, 'ONLY')

C     Install the collar inside the inner Q6..

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q6IC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6IC', 'COLO', 6)
      call gsatt ('Q6IC', 'SEEN', 0)
      call gspos ('Q6IC', 1, 'Q6IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside the inner Q6.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q6IL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6IL', 'COLO', 6)
      call gsatt ('Q6IL', 'SEEN', 0)
      call gspos ('Q6IL', 1, 'Q6IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q6IG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6IG', 'COLO', 6)
      call gsatt ('Q6IG', 'SEEN', 0)
      call gspos ('Q6IG', 1, 'Q6IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner Q6. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q6IT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6IT', 'COLO', 6)
      call gsatt ('Q6IT', 'SEEN', 0)
      call gspos ('Q6IT', 1, 'Q6IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q6IV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6IV', 'SEEN', 0)
      call gspos ('Q6IV', 1, 'Q6IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the outer Q6.

      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q6_length
      nmed = nmed_dx
      call gsvolu ('Q6OF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6OF', 'COLO', 6)
      call gspos ('Q6OF', 1, 'HALL', q6_out_x_coord, q6_out_y_coord,
     $     q6_out_z_coord, irot9, 'ONLY')

C     Install the collar inside the outer Q6.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q6OC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6OC', 'COLO', 6)
      call gsatt ('Q6OC', 'SEEN', 0)
      call gspos ('Q6OC', 1, 'Q6OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now Onstall the coil inside the outer Q6.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q6OL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6OL', 'COLO', 6)
      call gsatt ('Q6OL', 'SEEN', 0)
      call gspos ('Q6OL', 1, 'Q6OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q6OG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6OG', 'COLO', 6)
      call gsatt ('Q6OG', 'SEEN', 0)
      call gspos ('Q6OG', 1, 'Q6OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer Q6. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q6OT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6OT', 'COLO', 6)
      call gsatt ('Q6OT', 'SEEN', 0)
      call gspos ('Q6OT', 1, 'Q6OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q6OV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q6OV', 'SEEN', 0)
      call gspos ('Q6OV', 1, 'Q6OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the magnets on the other side of the beam line. 
C     We need a new rotation matrix. 

      angle1 = 90.0e0 - d5_d6_theta_deg
      angle2 = d5_d6_theta_deg
      irot = irot + 1 
      irot10 = irot
      call gsrotm (irot10, angle1, 0.0e0, 90.0e0, 90.0e0, angle2,
     $     180.0e0)
      call gspos ('Q6IF',2, 'HALL', q6_in_x_coord, q6_in_y_coord, 
     $     -q6_in_z_coord, irot10, 'ONLY') 
      call gspos ('Q6OF',2, 'HALL', q6_out_x_coord, q6_out_y_coord, 
     $     -q6_out_z_coord, irot10, 'ONLY') 

C     Begin installation of D6.  first, calculate the coordinates of the
C     centers of the magnets.

      d6_out_x_coord = d5_out_x_coord + d5_d6_out_dist * 
     $     save_this_sine
      d6_out_z_coord = d5_out_z_coord + d5_d6_out_dist * 
     $     save_this_cos

C     Now calculate the coordinates of the magnet in the inner arc. 

      d6_in_x_coord = d5_in_x_coord + d5_d6_in_dist * 
     $     save_this_sine
      d6_in_z_coord = d5_in_z_coord + d5_d6_in_dist * 
     $     save_this_cos

C     Now install the magnet in the outer arc.

      d6_par(1) = d5_iron_ir
      d6_par(2) = d5_iron_or
      d6_par(3) = d6_length
      nmed = nmed_dx
      call gsvolu ('D6OF', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6OF', 'COLO', 6)
      call gspos ('D6OF', 1, 'HALL', d6_out_x_coord, d6_out_y_coord,
     $     d6_out_z_coord, irot9, 'ONLY')

C     Install the collar inside the outer D6.

      d6_par(1) = d5_collar_ir
      d6_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D6OC', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6OC', 'COLO', 6)
      call gsatt ('D6OC', 'SEEN', 0)
      call gspos ('D6OC', 1, 'D6OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside outer D6.  

      d6_par(1) = d5_coil_ir
      d6_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D6OL', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6OL', 'COLO', 6)
      call gsatt ('D6OL', 'SEEN', 0)
      call gspos ('D6OL', 1, 'D6OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the outer
C     magnet.

      d6_par(1) = d5_gap_ir
      d6_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D6OG', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6OG', 'COLO', 6)
      call gsatt ('D6OG', 'SEEN', 0)
      call gspos ('D6OG', 1, 'D6OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer D6. 

      d6_par(1) = d5_tube_ir
      d6_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D6OT', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6OT', 'COLO', 6)
      call gsatt ('D6OT', 'SEEN', 0)
      call gspos ('D6OT', 1, 'D6OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d6_par(1) = 0.0e0
      d6_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D6OV', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6OV', 'SEEN', 0)
      call gspos ('D6OV', 1, 'D6OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the inner D6 magnets.

      d6_par(1) = d5_iron_ir
      d6_par(2) = d5_iron_or
      d6_par(3) = d6_length
      nmed = nmed_dx
      call gsvolu ('D6IF', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6IF', 'COLO', 6)
      call gspos ('D6IF', 1, 'HALL', d6_in_x_coord, d6_in_y_coord,
     $     d6_in_z_coord, irot9, 'ONLY')

C     Install the collar inside the inner D6.

      d6_par(1) = d5_collar_ir
      d6_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D6IC', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6IC', 'COLO', 6)
      call gsatt ('D6IC', 'SEEN', 0)
      call gspos ('D6IC', 1, 'D6IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside inner D6.  

      d6_par(1) = d5_coil_ir
      d6_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D6IL', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6IL', 'COLO', 6)
      call gsatt ('D6IL', 'SEEN', 0)
      call gspos ('D6IL', 1, 'D6IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the inner
C     D6 magnet.

      d6_par(1) = d5_gap_ir
      d6_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D6IG', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6IG', 'COLO', 6)
      call gsatt ('D6IG', 'SEEN', 0)
      call gspos ('D6IG', 1, 'D6IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner D6. 

      d6_par(1) = d5_tube_ir
      d6_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D6IT', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6IT', 'COLO', 6)
      call gsatt ('D6IT', 'SEEN', 0)
      call gspos ('D6IT', 1, 'D6IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d6_par(1) = 0.0e0
      d6_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D6IV', 'TUBE', nmed, d6_par, 3, ivolu)
      call gsatt ('D6IV', 'SEEN', 0)
      call gspos ('D6IV', 1, 'D6IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the corresponding magnets on the other side of the origin.

      call gspos ('D6IF',2, 'HALL', d6_in_x_coord, d6_in_y_coord, 
     $     -d6_in_z_coord, irot10, 'ONLY') 
      call gspos ('D6OF',2, 'HALL', d6_out_x_coord, d6_out_y_coord, 
     $     -d6_out_z_coord, irot10, 'ONLY') 

C     End installation of D6.  Begin installation of Q7


C     For purposes of alignment we will now change the values of the
C     variables d6_in_x_coord, d6_in_z_coord, d6_out_x_coord, and 
C     d6_out_z_coord.  We want to point the following magnets towards
C     the downstream end of D6, not towards its center.  At the same
C     time we do this, though, we must subtract the half lengths of the
C     magnets from d6_q7_in_dist, etc., in order to keep the distances
C     between magnets constant. 

      d6_in_x_coord = d6_in_x_coord + d6_length * save_this_sine    
      d6_in_z_coord = d6_in_z_coord + d6_length * save_this_cos
      d6_out_x_coord = d6_out_x_coord + d6_length * save_this_sine    
      d6_out_z_coord = d6_out_z_coord + d6_length * save_this_cos
      d6_q7_in_dist = d6_q7_in_dist - d6_length
      d6_q7_out_dist = d6_q7_out_dist - d6_length
      d6_q8_in_dist = d6_q8_in_dist - d6_length
      d6_q8_out_dist = d6_q8_out_dist - d6_length
      d6_d8_in_dist = d6_d8_in_dist - d6_length
      d6_d8_out_dist = d6_d8_out_dist - d6_length

C     Now install Q7 in the inner arc.  First define the new rotation
C     matrices. 

      angle1 = 90.0e0 + d6_d8_theta_deg
      angle2 = d6_d8_theta_deg
      irot = irot + 1 
      irot11 = irot
      call gsrotm (irot11, angle1, 0.0e0, 90.0e0, 90.0e0, angle2,
     $     0.0e0)
      angle1 = 90.0e0 - d6_d8_theta_deg
      angle2 = d6_d8_theta_deg
      irot = irot + 1 
      irot12 = irot
      call gsrotm (irot12, angle1, 0.0e0, 90.0e0, 90.0e0, angle2,
     $     180.0e0)
      save_this_sine = sind (d6_d8_theta_deg)
      save_this_cos  = cosd (d6_d8_theta_deg)

      q7_in_x_coord = d6_in_x_coord + d6_q7_in_dist * 
     $     save_this_sine
      q7_in_z_coord = d6_in_z_coord + d6_q7_in_dist * 
     $     save_this_cos
      q7_out_x_coord = d6_out_x_coord + d6_q7_out_dist * 
     $     save_this_sine
      q7_out_z_coord = d6_out_z_coord + d6_q7_out_dist * 
     $     save_this_cos

      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q7_length
      nmed = nmed_dx
      call gsvolu ('Q7IF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7IF', 'COLO', 6)
      call gspos ('Q7IF', 1, 'HALL', q7_in_x_coord, q7_in_y_coord,
     $     q7_in_z_coord, irot11, 'ONLY')

C     Install the collar inside the inner Q7.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q7IC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7IC', 'COLO', 6)
      call gsatt ('Q7IC', 'SEEN', 0)
      call gspos ('Q7IC', 1, 'Q7IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside the inner Q7.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q7IL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7IL', 'COLO', 6)
      call gsatt ('Q7IL', 'SEEN', 0)
      call gspos ('Q7IL', 1, 'Q7IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q7IG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7IG', 'COLO', 6)
      call gsatt ('Q7IG', 'SEEN', 0)
      call gspos ('Q7IG', 1, 'Q7IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner Q7. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q7IT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7IT', 'COLO', 6)
      call gsatt ('Q7IT', 'SEEN', 0)
      call gspos ('Q7IT', 1, 'Q7IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q7IV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7IV', 'SEEN', 0)
      call gspos ('Q7IV', 1, 'Q7IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the outer Q7.

      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q7_length
      nmed = nmed_dx
      call gsvolu ('Q7OF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7OF', 'COLO', 6)
      call gspos ('Q7OF', 1, 'HALL', q7_out_x_coord, q7_out_y_coord,
     $     q7_out_z_coord, irot11, 'ONLY')

C     Install the collar inside the outer Q7.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q7OC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7OC', 'COLO', 6)
      call gsatt ('Q7OC', 'SEEN', 0)
      call gspos ('Q7OC', 1, 'Q7OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now Onstall the coil inside the outer Q7.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q7OL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7OL', 'COLO', 6)
      call gsatt ('Q7OL', 'SEEN', 0)
      call gspos ('Q7OL', 1, 'Q7OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q7OG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7OG', 'COLO', 6)
      call gsatt ('Q7OG', 'SEEN', 0)
      call gspos ('Q7OG', 1, 'Q7OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer Q7. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q7OT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7OT', 'COLO', 6)
      call gsatt ('Q7OT', 'SEEN', 0)
      call gspos ('Q7OT', 1, 'Q7OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q7OV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q7OV', 'SEEN', 0)
      call gspos ('Q7OV', 1, 'Q7OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the magnets on the other side of the origin.

      call gspos ('Q7IF',2, 'HALL', q7_in_x_coord, q7_in_y_coord, 
     $     -q7_in_z_coord, irot12, 'ONLY') 
      call gspos ('Q7OF',2, 'HALL', q7_out_x_coord, q7_out_y_coord, 
     $     -q7_out_z_coord, irot12, 'ONLY') 

C     End installation of Q7.

C     Begin installation of Q8.   

      q8_in_x_coord = d6_in_x_coord + d6_q8_in_dist * 
     $     sind(d6_d8_theta_deg)
      q8_in_z_coord = d6_in_z_coord + d6_q8_in_dist * 
     $     cosd(d6_d8_theta_deg)
      q8_out_x_coord = d6_out_x_coord + d6_q8_out_dist * 
     $     sind(d6_d8_theta_deg)
      q8_out_z_coord = d6_out_z_coord + d6_q8_out_dist * 
     $     cosd(d6_d8_theta_deg)

C     Now install Q8 in the inner arc. 


      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q8_length
      nmed = nmed_dx
      call gsvolu ('Q8IF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8IF', 'COLO', 6)
      call gspos ('Q8IF', 1, 'HALL', q8_in_x_coord, q8_in_y_coord,
     $     q8_in_z_coord, irot11, 'ONLY')

C     Install the collar inside the inner Q8.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q8IC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8IC', 'COLO', 6)
      call gsatt ('Q8IC', 'SEEN', 0)
      call gspos ('Q8IC', 1, 'Q8IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside the inner Q8.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q8IL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8IL', 'COLO', 6)
      call gsatt ('Q8IL', 'SEEN', 0)
      call gspos ('Q8IL', 1, 'Q8IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q8IG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8IG', 'COLO', 6)
      call gsatt ('Q8IG', 'SEEN', 0)
      call gspos ('Q8IG', 1, 'Q8IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner Q8. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q8IT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8IT', 'COLO', 6)
      call gsatt ('Q8IT', 'SEEN', 0)
      call gspos ('Q8IT', 1, 'Q8IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q8IV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8IV', 'SEEN', 0)
      call gspos ('Q8IV', 1, 'Q8IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the outer Q8.

      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q8_length
      nmed = nmed_dx
      call gsvolu ('Q8OF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8OF', 'COLO', 6)
      call gspos ('Q8OF', 1, 'HALL', q8_out_x_coord, q8_out_y_coord,
     $     q8_out_z_coord, irot11, 'ONLY')

C     Install the collar inside the outer Q8.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q8OC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8OC', 'COLO', 6)
      call gsatt ('Q8OC', 'SEEN', 0)
      call gspos ('Q8OC', 1, 'Q8OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now Onstall the coil inside the outer Q8.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q8OL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8OL', 'COLO', 6)
      call gsatt ('Q8OL', 'SEEN', 0)
      call gspos ('Q8OL', 1, 'Q8OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q8OG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8OG', 'COLO', 6)
      call gsatt ('Q8OG', 'SEEN', 0)
      call gspos ('Q8OG', 1, 'Q8OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer Q8. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q8OT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8OT', 'COLO', 6)
      call gsatt ('Q8OT', 'SEEN', 0)
      call gspos ('Q8OT', 1, 'Q8OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q8OV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q8OV', 'SEEN', 0)
      call gspos ('Q8OV', 1, 'Q8OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the magnets on the other side of the origin.

      call gspos ('Q8IF',2, 'HALL', q8_in_x_coord, q8_in_y_coord, 
     $     -q8_in_z_coord, irot12, 'ONLY') 
      call gspos ('Q8OF',2, 'HALL', q8_out_x_coord, q8_out_y_coord, 
     $     -q8_out_z_coord, irot12, 'ONLY') 

C     End installation of Q8.

C     Begin installation of D8.


      d8_out_x_coord = d6_out_x_coord + d6_d8_out_dist * 
     $     save_this_sine
      d8_out_z_coord = d6_out_z_coord + d6_d8_out_dist * 
     $     save_this_cos

C     Now calculate the coordinates of the magnet in the inner arc. 

      d8_in_x_coord = d6_in_x_coord + d6_d8_in_dist * 
     $     save_this_sine
      d8_in_z_coord = d6_in_z_coord + d6_d8_in_dist * 
     $     save_this_cos

C     Now install the magnet in the outer arc.

      d8_par(1) = d5_iron_ir
      d8_par(2) = d5_iron_or
      d8_par(3) = d8_length
      nmed = nmed_dx
      call gsvolu ('D8OF', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8OF', 'COLO', 6)
      call gspos ('D8OF', 1, 'HALL', d8_out_x_coord, d8_out_y_coord,
     $     d8_out_z_coord, irot11, 'ONLY')

C     Install the collar inside the outer D8.

      d8_par(1) = d5_collar_ir
      d8_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D8OC', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8OC', 'COLO', 6)
      call gsatt ('D8OC', 'SEEN', 0)
      call gspos ('D8OC', 1, 'D8OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside outer D8.  

      d8_par(1) = d5_coil_ir
      d8_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D8OL', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8OL', 'COLO', 6)
      call gsatt ('D8OL', 'SEEN', 0)
      call gspos ('D8OL', 1, 'D8OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the outer
C     magnet.

      d8_par(1) = d5_gap_ir
      d8_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D8OG', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8OG', 'COLO', 6)
      call gsatt ('D8OG', 'SEEN', 0)
      call gspos ('D8OG', 1, 'D8OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer D8. 

      d8_par(1) = d5_tube_ir
      d8_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D8OT', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8OT', 'COLO', 6)
      call gsatt ('D8OT', 'SEEN', 0)
      call gspos ('D8OT', 1, 'D8OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d8_par(1) = 0.0e0
      d8_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D8OV', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8OV', 'SEEN', 0)
      call gspos ('D8OV', 1, 'D8OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the inner D8 magnets.

      d8_par(1) = d5_iron_ir
      d8_par(2) = d5_iron_or
      d8_par(3) = d8_length
      nmed = nmed_dx
      call gsvolu ('D8IF', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8IF', 'COLO', 6)
      call gspos ('D8IF', 1, 'HALL', d8_in_x_coord, d8_in_y_coord,
     $     d8_in_z_coord, irot11, 'ONLY')

C     Install the collar inside the inner D8.

      d8_par(1) = d5_collar_ir
      d8_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D8IC', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8IC', 'COLO', 6)
      call gsatt ('D8IC', 'SEEN', 0)
      call gspos ('D8IC', 1, 'D8IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside inner D8.  

      d8_par(1) = d5_coil_ir
      d8_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D8IL', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8IL', 'COLO', 6)
      call gsatt ('D8IL', 'SEEN', 0)
      call gspos ('D8IL', 1, 'D8IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the inner
C     D8 magnet.

      d8_par(1) = d5_gap_ir
      d8_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D8IG', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8IG', 'COLO', 6)
      call gsatt ('D8IG', 'SEEN', 0)
      call gspos ('D8IG', 1, 'D8IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner D8. 

      d8_par(1) = d5_tube_ir
      d8_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D8IT', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8IT', 'COLO', 6)
      call gsatt ('D8IT', 'SEEN', 0)
      call gspos ('D8IT', 1, 'D8IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d8_par(1) = 0.0e0
      d8_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D8IV', 'TUBE', nmed, d8_par, 3, ivolu)
      call gsatt ('D8IV', 'SEEN', 0)
      call gspos ('D8IV', 1, 'D8IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the corresponding magnets on the other side of the origin.

      call gspos ('D8IF',2, 'HALL', d8_in_x_coord, d8_in_y_coord, 
     $     -d8_in_z_coord, irot12, 'ONLY') 
      call gspos ('D8OF',2, 'HALL', d8_out_x_coord, d8_out_y_coord, 
     $     -d8_out_z_coord, irot12, 'ONLY') 

C     End installation of D8. 


C     For purposes of alignment we will now change the values of the
C     variables d8_in_x_coord, d8_in_z_coord, d8_out_x_coord, and 
C     d8_out_z_coord.  We want to point the following magnets towards
C     the downstream end of D8, not towards its center.  At the same
C     time we do this, though, we must subtract the half lengths of the
C     magnets from d8_q9_in_dist, etc., in order to keep the distances
C     between magnets constant. 

      d8_in_x_coord = d8_in_x_coord + d8_length * save_this_sine    
      d8_in_z_coord = d8_in_z_coord + d8_length * save_this_cos
      d8_out_x_coord = d8_out_x_coord + d8_length * save_this_sine    
      d8_out_z_coord = d8_out_z_coord + d8_length * save_this_cos
      d8_q9_in_dist = d8_q9_in_dist - d8_length
      d8_q9_out_dist = d8_q9_out_dist - d8_length
      d8_d9_in_dist = d8_d9_in_dist - d8_length
      d8_d9_out_dist = d8_d9_out_dist - d8_length

C     Define required rotation matrices. 

      angle1 = 90.0e0 + d8_d9_theta_deg
      angle2 = d8_d9_theta_deg
      irot = irot + 1 
      irot13 = irot
      call gsrotm (irot13, angle1, 0.0e0, 90.0e0, 90.0e0, angle2,
     $     0.0e0)
      angle1 = 90.0e0 - d8_d9_theta_deg
      angle2 = d8_d9_theta_deg
      irot = irot + 1 
      irot14 = irot
      call gsrotm (irot14, angle1, 0.0e0, 90.0e0, 90.0e0, angle2,
     $     180.0e0)
      save_this_sine = sind (d8_d9_theta_deg)
      save_this_cos  = cosd (d8_d9_theta_deg)

C     Begin installation of Q9.

      q9_in_x_coord = d8_in_x_coord + d8_q9_in_dist * 
     $     save_this_sine
      q9_in_z_coord = d8_in_z_coord + d8_q9_in_dist * 
     $     save_this_cos
      q9_out_x_coord = d8_out_x_coord + d8_q9_out_dist * 
     $     save_this_sine
      q9_out_z_coord = d8_out_z_coord + d8_q9_out_dist * 
     $     save_this_cos

C     Now install Q9 in the inner arc. 


      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q9_length
      nmed = nmed_dx
      call gsvolu ('Q9IF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9IF', 'COLO', 6)
      call gspos ('Q9IF', 1, 'HALL', q9_in_x_coord, q9_in_y_coord,
     $     q9_in_z_coord, irot13, 'ONLY')

C     Install the collar inside the inner Q9.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q9IC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9IC', 'COLO', 6)
      call gsatt ('Q9IC', 'SEEN', 0)
      call gspos ('Q9IC', 1, 'Q9IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside the inner Q9.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q9IL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9IL', 'COLO', 6)
      call gsatt ('Q9IL', 'SEEN', 0)
      call gspos ('Q9IL', 1, 'Q9IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q9IG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9IG', 'COLO', 6)
      call gsatt ('Q9IG', 'SEEN', 0)
      call gspos ('Q9IG', 1, 'Q9IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner Q9. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q9IT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9IT', 'COLO', 6)
      call gsatt ('Q9IT', 'SEEN', 0)
      call gspos ('Q9IT', 1, 'Q9IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q9IV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9IV', 'SEEN', 0)
      call gspos ('Q9IV', 1, 'Q9IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the outer Q9.

      qsa_par(1) = qsa_iron_ir
      qsa_par(2) = qsa_iron_or
      qsa_par(3) = q9_length
      nmed = nmed_dx
      call gsvolu ('Q9OF', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9OF', 'COLO', 6)
      call gspos ('Q9OF', 1, 'HALL', q9_out_x_coord, q9_out_y_coord,
     $     q9_out_z_coord, irot13, 'ONLY')

C     Install the collar inside the outer Q9.

      qsa_par(1) = qsa_collar_ir
      qsa_par(2) = qsa_collar_or
      nmed = nmed_g10
      call gsvolu ('Q9OC', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9OC', 'COLO', 6)
      call gsatt ('Q9OC', 'SEEN', 0)
      call gspos ('Q9OC', 1, 'Q9OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now Onstall the coil inside the outer Q9.  

      qsa_par(1) = qsa_coil_ir
      qsa_par(2) = qsa_coil_or
      nmed = nmed_cu
      call gsvolu ('Q9OL', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9OL', 'COLO', 6)
      call gsatt ('Q9OL', 'SEEN', 0)
      call gspos ('Q9OL', 1, 'Q9OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil.

      qsa_par(1) = qsa_gap_ir
      qsa_par(2) = qsa_gap_or
      nmed = 16
      call gsvolu ('Q9OG', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9OG', 'COLO', 6)
      call gsatt ('Q9OG', 'SEEN', 0)
      call gspos ('Q9OG', 1, 'Q9OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer Q9. 

      qsa_par(1) = qsa_tube_ir
      qsa_par(2) = qsa_tube_or
      nmed = 900
      call gsvolu ('Q9OT', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9OT', 'COLO', 6)
      call gsatt ('Q9OT', 'SEEN', 0)
      call gspos ('Q9OT', 1, 'Q9OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      qsa_par(1) = 0.0e0
      qsa_par(2) = qsa_tube_ir
      nmed = 16
      call gsvolu ('Q9OV', 'TUBE', nmed, qsa_par, 3, ivolu)
      call gsatt ('Q9OV', 'SEEN', 0)
      call gspos ('Q9OV', 1, 'Q9OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the magnets on the other side of the origin.

      call gspos ('Q9IF',2, 'HALL', q9_in_x_coord, q9_in_y_coord, 
     $     -q9_in_z_coord, irot14, 'ONLY') 
      call gspos ('Q9OF',2, 'HALL', q9_out_x_coord, q9_out_y_coord, 
     $     -q9_out_z_coord, irot14, 'ONLY') 

C     End installation of Q9.  Begin installation of D9.

      d9_out_x_coord = d8_out_x_coord + d8_d9_out_dist * 
     $     save_this_sine
      d9_out_z_coord = d8_out_z_coord + d8_d9_out_dist * 
     $     save_this_cos

C     Now calculate the coordinates of the magnet in the inner arc. 

      d9_in_x_coord = d8_in_x_coord + d8_d9_in_dist * 
     $     save_this_sine
      d9_in_z_coord = d8_in_z_coord + d8_d9_in_dist * 
     $     save_this_cos

C     Now install the magnet in the outer arc.

      d9_par(1) = d5_iron_ir
      d9_par(2) = d5_iron_or
      d9_par(3) = d9_length
      nmed = nmed_dx
      call gsvolu ('D9OF', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9OF', 'COLO', 6)
      call gspos ('D9OF', 1, 'HALL', d9_out_x_coord, d9_out_y_coord,
     $     d9_out_z_coord, irot13, 'ONLY')

C     Install the collar inside the outer D9.

      d9_par(1) = d5_collar_ir
      d9_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D9OC', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9OC', 'COLO', 6)
      call gsatt ('D9OC', 'SEEN', 0)
      call gspos ('D9OC', 1, 'D9OF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside outer D9.  

      d9_par(1) = d5_coil_ir
      d9_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D9OL', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9OL', 'COLO', 6)
      call gsatt ('D9OL', 'SEEN', 0)
      call gspos ('D9OL', 1, 'D9OC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the outer
C     magnet.

      d9_par(1) = d5_gap_ir
      d9_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D9OG', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9OG', 'COLO', 6)
      call gsatt ('D9OG', 'SEEN', 0)
      call gspos ('D9OG', 1, 'D9OL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the outer D9. 

      d9_par(1) = d5_tube_ir
      d9_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D9OT', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9OT', 'COLO', 6)
      call gsatt ('D9OT', 'SEEN', 0)
      call gspos ('D9OT', 1, 'D9OG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d9_par(1) = 0.0e0
      d9_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D9OV', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9OV', 'SEEN', 0)
      call gspos ('D9OV', 1, 'D9OT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the inner D9 magnets.

      d9_par(1) = d5_iron_ir
      d9_par(2) = d5_iron_or
      d9_par(3) = d9_length
      nmed = nmed_dx
      call gsvolu ('D9IF', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9IF', 'COLO', 6)
      call gspos ('D9IF', 1, 'HALL', d9_in_x_coord, d9_in_y_coord,
     $     d9_in_z_coord, irot13, 'ONLY')

C     Install the collar inside the inner D9.

      d9_par(1) = d5_collar_ir
      d9_par(2) = d5_collar_or
      nmed = nmed_g10
      call gsvolu ('D9IC', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9IC', 'COLO', 6)
      call gsatt ('D9IC', 'SEEN', 0)
      call gspos ('D9IC', 1, 'D9IF', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the coil inside inner D9.  

      d9_par(1) = d5_coil_ir
      d9_par(2) = d5_coil_or
      nmed = nmed_cu
      call gsvolu ('D9IL', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9IL', 'COLO', 6)
      call gsatt ('D9IL', 'SEEN', 0)
      call gspos ('D9IL', 1, 'D9IC', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the gap between the beam tube and the coil in the inner
C     D9 magnet.

      d9_par(1) = d5_gap_ir
      d9_par(2) = d5_gap_or
      nmed = 16
      call gsvolu ('D9IG', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9IG', 'COLO', 6)
      call gsatt ('D9IG', 'SEEN', 0)
      call gspos ('D9IG', 1, 'D9IL', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Now install the beam tube inside the inner D9. 

      d9_par(1) = d5_tube_ir
      d9_par(2) = d5_tube_or
      nmed = 900
      call gsvolu ('D9IT', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9IT', 'COLO', 6)
      call gsatt ('D9IT', 'SEEN', 0)
      call gspos ('D9IT', 1, 'D9IG', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the vacuum inside the tube.

      d9_par(1) = 0.0e0
      d9_par(2) = d5_tube_ir
      nmed = 16
      call gsvolu ('D9IV', 'TUBE', nmed, d9_par, 3, ivolu)
      call gsatt ('D9IV', 'SEEN', 0)
      call gspos ('D9IV', 1, 'D9IT', 0.0e0, 0.0e0, 0.0e0, 0, 'ONLY')

C     Install the corresponding magnets on the other side of the origin.

      call gspos ('D9IF',2, 'HALL', d9_in_x_coord, d9_in_y_coord, 
     $     -d9_in_z_coord, irot14, 'ONLY') 
      call gspos ('D9OF',2, 'HALL', d9_out_x_coord, d9_out_y_coord, 
     $     -d9_out_z_coord, irot14, 'ONLY') 
      return

c     Error branch points associated with phnx.par namelist file

 999  continue
      stop 'rhic_magnet_install - Error in rhic_mag in geometry'

C     Miscellaneous notes of interest

C     I have not installed the vacuum in pipes that run between
C     the magnets because the installation is tedious and the effect 
C     is small.  At some later date this deficiency should be remedied. 

C     Neither the stainless steel containment vessel nor the cryostat 
C     have been installed by this program.  The installation of the 
C     containment vessels will be trivial, but I have no drawings of the
C     cryostats at yet.  

C     The color code for graphics output is located on page 369
C     of the GEANT manual in the section entitled "XINT002".

C     The allowed shapes in GEANT are specified on page 101 of the
C     GEANT manual in the section entitled "GEOM050". 

C     The routine "gsvolu" is discussed on page 110 of the GEANT 
C     manual in the section entitled "GEOM100". 

C     The routine "gspos" is discussed on page 111 of the GEANT 
C     manual in the section entitled "GEOM100". 

C     The definition of rotation matrices is discussed on page 124
C     of the GEANT manual in the section entitled "GEOM200".  The 
C     name of the relevant subroutine is "gsrotm". 

C     Particle definitions within the context of GEANT are found on
C     pages 53 - 55 of the GEANT manual in the section entitled 
C     "CONS300".

C     Material definitions are found on page 39 of the GEANT manual in
C     the section entitled "CONS100".  The table presented therein is
C     woefully small, and a larger table may be found in Appendix B, 
C     section 2 of the PISA/PISORP users manual. 

     
      end
