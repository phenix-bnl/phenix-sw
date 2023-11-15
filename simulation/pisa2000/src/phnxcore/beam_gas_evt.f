      subroutine beam_gas_evt(ievstat)
      implicit none

#include "g77trigdef.inc"

c     Beam gas event generator

c     Original version from Paul Kirk (LSU)


c     Calling variable

      integer ievstat


c     Common block definition

#include "guevgen.inc"

C     Declare parameters that control dimensions of arrays below.

      integer*4 no_theta_bins, no_bins_ptot, no_bins_pz 
      integer*4 no_cos_theta_bins, no_bins_pions, no_bins_nucleons
      integer*4 no_bins_kaons, no_bins_gamma, no_bins_leptons
      integer*4 no_bins_ptot_nucleon

      parameter (no_theta_bins = 36 )
      parameter (no_bins_ptot = 150, no_bins_pz = 150)
      parameter (no_cos_theta_bins = 40, no_bins_pions = 100)
      parameter (no_bins_nucleons = 50, no_bins_kaons = 20)
      parameter (no_bins_gamma = 20, no_bins_leptons = 10)
      parameter (no_bins_ptot_nucleon = 160)

C     Declare other integers below

      integer*4 ievent, igeant, nwb, itrtyp
      integer*4 i_cnt_particles (200), i, j
      integer*4 lun_hydrogen, lun_helium, lun_nitrogen
      integer*4 lun_output, no_open_files, lun_input
      integer*4 no_bins_z_plot, option_number
      integer*4 number_hydrogen_int, number_helium_int
      integer*4 number_nitrogen_int
      integer*4 interaction_region, index, k
      parameter (lun_hydrogen = 1151, lun_helium = 1152)
      parameter (lun_nitrogen  = 1153, lun_output = 1150)
      parameter ( no_bins_z_plot = 100)


c     CFM I/O additions

      integer istat             ! error return from KUOPEN
      integer lun_fnames        ! file for file names
      parameter (lun_fnames = 1199)
      character*80 evtname(3)   ! names of the event files (H, He, and N)
      character*80 outname      ! name for output file

C     Declare real variables below.

      real*4 ion_mass, ion_charge, ion_lifetime
      real*4 trig_factor_1, trig_factor_2
      real*4 trig_factor_3, trig_factor_4, trig_factor_5
      real*4 theta_low, theta_high, ub(1) 
      real*4 upper_lim_ptot, upper_lim_pz, lower_lim_ptot 
      real*4 lower_lim_pz, bin_width_ptot, bin_width_pz
      real*4 no_pi_plus (no_bins_pions)
      real*4 no_pi_minus (no_bins_pions)
      real*4 no_pi_naught (no_bins_pions), no_neutrons(no_bins_nucleons)
      real*4 no_protons (no_bins_nucleons)
      real*4 no_gamma (no_bins_gamma) 
      real*4 no_electron (no_bins_leptons), no_positron(no_bins_leptons)
      real*4 no_kaon_plus (no_bins_kaons) 
      real*4 no_kaon_minus (no_bins_kaons), no_kaon_0 (no_bins_kaons)
      real*4 no_pi_plus_theta (no_theta_bins), no_pi_plus_phi (36)
      real*4 no_pi_plus_cos_theta (no_cos_theta_bins) 
      real*4 no_pi_plus_ptot (no_bins_ptot)
      real*4 no_pi_plus_pz (no_bins_pz)
      real*4 no_pi_minus_theta (no_theta_bins), no_pi_minus_phi (36)
      real*4 no_pi_minus_cos_theta (no_cos_theta_bins)
      real*4 no_pi_minus_ptot(no_bins_ptot)
      real*4 no_pi_minus_pz (no_bins_pz)
      real*4 no_pi_naught_theta (no_theta_bins), no_pi_naught_phi (36)
      real*4 no_pi_naught_cos_theta (no_cos_theta_bins)
      real*4 no_pi_naught_ptot(no_bins_ptot)
      real*4 no_pi_naught_pz (no_bins_pz)
      real*4 no_ka_plus_theta (no_theta_bins), no_ka_plus_phi (36)
      real*4 no_ka_plus_cos_theta (no_cos_theta_bins)
      real*4 no_ka_plus_ptot (no_bins_ptot)
      real*4 no_ka_plus_pz (no_bins_pz)
      real*4 no_ka_minus_theta (no_theta_bins), no_ka_minus_phi (36)
      real*4 no_ka_minus_cos_theta (no_cos_theta_bins)
      real*4 no_ka_minus_ptot(no_bins_ptot)
      real*4 no_ka_minus_pz (no_bins_pz)
      real*4 no_ka_naught_theta (no_theta_bins), no_ka_naught_phi (36)
      real*4 no_ka_naught_cos_theta (no_cos_theta_bins)
      real*4 no_ka_naught_ptot(no_bins_ptot)
      real*4 no_ka_naught_pz (no_bins_pz)
      real*4 no_protons_theta (no_theta_bins), no_protons_phi (36)
      real*4 no_protons_cos_theta (no_cos_theta_bins)
      real*4 no_protons_ptot (no_bins_ptot_nucleon)
      real*4 no_protons_pz (no_bins_ptot_nucleon)
      real*4 no_neutrons_theta (no_theta_bins), no_neutrons_phi (36)
      real*4 no_neutrons_cos_theta (no_cos_theta_bins)
      real*4 no_neutrons_ptot (no_bins_ptot_nucleon)
      real*4 no_neutrons_pz (no_bins_ptot_nucleon)
      real*4 no_gammas_theta (no_theta_bins), no_gammas_phi (36)
      real*4 no_gammas_cos_theta (no_cos_theta_bins)
      real*4 no_gammas_ptot (no_bins_ptot)
      real*4 no_gammas_pz (no_bins_pz)
      real*4 no_electrons_theta (no_theta_bins), no_electrons_phi (36)
      real*4 no_electrons_cos_theta (no_cos_theta_bins)
      real*4 no_electrons_ptot (no_bins_ptot)
      real*4 no_electrons_pz (no_bins_pz)
      real*4 no_positrons_theta (no_theta_bins), no_positrons_phi (36)
      real*4 no_positrons_cos_theta (no_cos_theta_bins)
      real*4 no_positrons_ptot (no_bins_ptot)
      real*4 no_positrons_pz (no_bins_pz)

c     Following "number" variables were originally reals

      integer number_positive_pions, number_negative_pions
      integer number_neutral_pions, number_protons, number_neutrons
      integer number_positive_kaons, number_negative_kaons
      integer number_neutral_kaons, number_gammas
      integer number_positrons, number_electrons
      real*4 probability_hydrogen, probability_helium
      real*4 probability_nitrogen, total_probability
      real*4 rel_prob_hydrogen, rel_prob_helium, rel_prob_nitrogen
      real*4 hydrogen_thickness(4), helium_thickness(4)
      real*4 nitrogen_thickness(4), length_region(4)
      real*4 total_hydrogen_thickness, total_helium_thickness
      real*4 total_nitrogen_thickness, total_length_hydrogen
      real*4 total_length_helium, total_length_nitrogen
      real*4 random_number, distance, cuts(4), sum, numerator
      real*4 denominator, least_z_region (4), intercept_line_1
      real*4 slope_line_1, intercept_line_2, slope_line_2
      real*4 intercept_line_3, slope_line_3
      real*4 intercept_line_4, slope_line_4
      real*4 intercept_line_5, slope_line_5
      real*4 lower_lim_z_plot, upper_lim_z_plot, bin_width_z_plot
      real*4 z_plot_hydrogen(100), z_plot_helium (100)
      real*4 z_plot_nitrogen (100)
      real*4 change_sign_x, change_sign_z, z_in_meters
      real*4 x_coordinate_interaction, z_coordinate_interaction
      real*4 px, py, pz
      real*4 azimuthal_angle, zenith_angle, cosine_of_angle
      real*4 temp, temp01, total_momentum, temp_x, temp_z
      real*4 sigma_au_hydrogen, sigma_au_helium, sigma_au_nitrogen
      real*4 test_value, delta_z, rndm, theta_bin_width
      real*4 total_length
      real*4 x_coordinate_valve_1, z_coordinate_valve_1
      real*4 x_coordinate_valve_2, z_coordinate_valve_2
      real*4 x_coordinate_valve_3, z_coordinate_valve_3
      parameter (x_coordinate_valve_1 = - 1.443e+01)
      parameter (x_coordinate_valve_2 = - 2.57025e+01)
      parameter (x_coordinate_valve_3 = - 3.84831e+01)
      parameter (z_coordinate_valve_1 = 1.930e+03)
      parameter (z_coordinate_valve_2 = 3.75778e+03)
      parameter (z_coordinate_valve_3 = 7.23592e+03)

      real*4 length_r4_segment_1, length_r4_segment_2
      real*4 length_r4_segment_3, length_r4_segment_4
      parameter (length_r4_segment_1 = 2.20971e+03)
      parameter (length_r4_segment_2 = 1.51893e+03)
      parameter (length_r4_segment_3 = 2.94554e+03)
      parameter (length_r4_segment_4 = 1.48212e+03)

      real*4 x_coord_d0, z_coord_d0
      real*4 x_coord_d5_end, z_coord_d5_end
      real*4 x_coord_d6_end, z_coord_d6_end
      real*4 x_coord_d8_end, z_coord_d8_end
      parameter (x_coord_d0 = -2.00885e+01)
      parameter (z_coord_d0 = 2.22998e+03)
      parameter (x_coord_d5_end = -4.66028e+01)
      parameter (z_coord_d5_end = 9.44562e+03)
      parameter (x_coord_d6_end = 2.28202e0)
      parameter (z_coord_d6_end = 1.09638e+04)
      parameter (x_coord_d8_end = 1.32873e+02)
      parameter (z_coord_d8_end = 1.39064e+04)

      logical*4 region_1_segment_1, region_1_segment_2
      logical*4 region_2_segment_1, region_2_segment_2
      logical*4 region_3, universal_vertex
      logical*4 region_4_segment_1, region_4_segment_2
      logical*4 region_4_segment_3, region_4_segment_4

      real*4 angle_zz_r1_s1, angle_zz_r1_s2 
      real*4 angle_zz_r2_s1, angle_zz_r2_s2 
      real*4 angle_zz_r3, keep_this_angle
      real*4 angle_zz_r4_s1, angle_zz_r4_s2 
      real*4 angle_zz_r4_s3, angle_zz_r4_s4
      parameter (angle_zz_r1_s1 = 1.80e+02)
      parameter (angle_zz_r1_s2 = 1.78919e+02)
      parameter (angle_zz_r2_s1 = 1.78919e+02)
      parameter (angle_zz_r2_s2 = 1.79789e+02)
      parameter (angle_zz_r3 = 1.79789e+02)
      parameter (angle_zz_r4_s1 = 1.79789e+02)
      parameter (angle_zz_r4_s2 = 1.81844e+02)
      parameter (angle_zz_r4_s3 = 1.82541e+02)
      parameter (angle_zz_r4_s4 = 1.84771e+02)

      real*4  p2_temp, p3_temp, p4_temp

      parameter (theta_low = 0.0e0, theta_high = 1.80e+02)
      parameter (upper_lim_ptot = 1.50e+02, upper_lim_pz = 1.50e+02)
      parameter (lower_lim_ptot = 0.0e0, lower_lim_pz = 0.0e0)
      parameter (bin_width_ptot =  1.0e0, bin_width_pz = 1.0e0)
      parameter (theta_bin_width = 5.0e0) 
      parameter (lower_lim_z_plot = 0.0e0, upper_lim_z_plot = 2.0e+02)
      parameter (bin_width_z_plot = 2.0e0)
      parameter (intercept_line_1 = 2.19756e+01)
      parameter (intercept_line_2 = -1.18943e+01)
      parameter (intercept_line_3 = -3.50756e+02)
      parameter (intercept_line_4 = -4.84280e+02)
      parameter (intercept_line_5 = -1.02785e+03)

C     The slopes shown below are appropriate for the outer arc only!

      parameter (slope_line_1 = -1.88630e-02)
      parameter (slope_line_2 = -3.67456e-03)
      parameter (slope_line_3 = 3.22004e-02)
      parameter (slope_line_4 = 4.43790e-02)
      parameter (slope_line_5 = 8.34669e-02)
      parameter (change_sign_x = 1.0e0, change_sign_z = 1.0e0)
      parameter (sigma_au_hydrogen = 2.10e-24)
      parameter (sigma_au_helium = 2.48e-24)
      parameter (sigma_au_nitrogen = 3.06e-24)
      logical*4 first_access, first_enabled_region
      logical*4 hydrogen_file, helium_file, nitrogen_file
      logical*4 enable_region(4), true_distributions 
      logical*4 hydrogen_interaction, helium_interaction
      logical*4 nitrogen_interaction, read_input

C     Declare character variables below. 

      CHARACTER*9  LBL
      CHARACTER*40 CAR2
      CHARACTER*20  CAR3



C     ********************** BEGIN DATA STATEMENTS
c     ***********************

      data first_access /.true./ 
      data helium_thickness / 0.0e0, 2.1e+09, 0.0e0, 9.4e+09/
      data hydrogen_thickness / 6.2e+10, 8.0e+10, 1.1e+11, 3.6e+11/
      data nitrogen_thickness / 4.7e+09, 0.0e0, 8.4e+09, 0.0e0/
      data least_z_region / 0.0e0, 1.930e+03, 3.75778e+03,7.23592e+03/
      data enable_region / .false.,  .false., .true., .false./
      data universal_vertex /.true./
      data length_region /1.93014e+03, 1.82781e+03, 3.47816e+03,
     $     8.1563e+03/
      data nwb /1/, read_input /.true./

C     **************** END DATA STATEMENTS ***********************



C     *****  BEGIN DEFINITIONS OF LOCALLY CREATED VARIABLES *********

C     LBL, CAR2, and CAR3 are also arrays of characters that serve no
C     purpose other than reading in ascii text from the first line of
C     each event.

C     First_access is a logical variable that is set equal to .true. in
c     a data statement.  Upon first call to this routine the file that
C     contains the hijet file is set opened, the header to the file is
c     read in, and the value of first_access itself is set equal to 
C     .false.


C     IEVENT is the number of the events in the HIJET file. The files to
c     which I currently have access contains 100 events. 


C     NPT is the number of particles that were produced in the HIJET 
C     event under examination. 

C     *********  END DEFINITIONS OF LOCALLY CREATED VARIABLES  *********

      

C     *************  DESCRIPTION OF THE HIJET FILES ******************

C     The HIJET files to which I currently have access comprise 1500 
C     interactions between Au of full energy and hydrogen atoms. 
C     The files are located in /disk3/kirk/cvs/pisa/bin.  The names of the
C     file to which I currently have access is auhebgas.evt.
C     These files were produced by RON LONGACRE at BNL. 

C     *************** END DESCRIPTION OF HIJET FILE *****************

      if (first_access)then
         first_access = .false.
         trig_factor_1 = sqrt (1.0e0 + slope_line_1 **2)
         trig_factor_2 = sqrt (1.0e0 + slope_line_2 **2)
         trig_factor_3 = sqrt (1.0e0 + slope_line_3 **2)
         trig_factor_4 = sqrt (1.0e0 + slope_line_4 **2)
         trig_factor_5 = sqrt (1.0e0 + slope_line_5 **2)

C     ************************************************************
C     ************************************************************
C     $NOTE$

C     On February 22 the following code was modified in order that 
C     each interaction occur at one and only one position.

C     Note that multi_xyz, xyz(3), and xyzmv(3, mxtot) are stored in
C     common block "evgen" in guevgen.inc.

C     ************************************************************
C     ************************************************************
         multi_xyz = 0

C     Place vertex for all interactions in the middle of D5 as a 
C     mechanism for initiating calculations.

         xyz(1) = -4.50025e+01
         xyz(2) = 0.0e0
         xyz(3) = +9.010125e+03
         z_in_meters = abs(xyz(3)) / 1.0e+02
         keep_this_angle = angle_zz_r4_s1


C     ************************************************************
C     ************************************************************
C     $NOTE$

C     This routine has been modified to handle Ron Longacre's new file.
C     The new file is written in an entirely different format from the 
C     old file, and the new files are both larger and more accurate 
C     than the old files.  Consequently the old files are now obsolete. 
C     As of this date, Feb. 22, 1996, the ONLY new file in our possession
C     was created on the assumption that the target is hydrogen. We
C     therefore hardwire the logical variable hydrogen_file = .true. 
C     because the other files cannot be read at this time.  Eventually 
C     this restriction will be lifted.


C     ************************************************************
C     ************************************************************

         hydrogen_file = .true.
         helium_file = .false.
         nitrogen_file = .false.
         true_distributions = .false.
         read_input = .false.
         if (read_input) then
            if(kevt_par(1).gt.0.5)then
               hydrogen_file = .true.
            else
               hydrogen_file = .false.
            endif
            if(kevt_par(2).gt.0.5)then
               helium_file = .true.
            else
               helium_file = .false.
            endif
            if(kevt_par(3).gt.0.5)then
               nitrogen_file = .true.
            else
               nitrogen_file = .false.
            endif
            if(kevt_par(4).gt.0.5)then
               true_distributions = .true.
            else
               true_distributions = .false.
            endif
         endif

c     now check the file names

         call kuopen(lun_fnames, 'beam_gas_names.txt', 'old',
     +        istat)
         if(istat.eq.0)then

c     file exists, so read the non-default names

            read(lun_fnames,*)evtname(1)
            read(lun_fnames,*)evtname(2)
            read(lun_fnames,*)evtname(3)
            read(lun_fnames,*)outname
            close(lun_fnames)
         else

c     file does not exist, so use default names

            evtname(1) = '/disk3/kirk/cvs/pisa/bin/aupbgas.evt'

C     ************************************************************
C     ************************************************************
C     $NOTE$

C     Do not permit user to open files that do not exist.

C     ************************************************************
C     ************************************************************

C     evtname(2) = '/disk1/kirk/auhebgas.evt'
C     evtname(3) = '/disk1/kirk/aunbgas.evt'
            outname = 
     +           '/disk3/kirk/OUTPUT/output_file.text'
         endif 


C     Open the file for output.

         open (unit=lun_output,
     $        file=outname,
     $        status='new')
         

C     Open the desired files for input and count them.  This programs  
C     accommodates from one to three files of HIJET data. 

         no_open_files = 0
         if (hydrogen_file) then
            open (unit=lun_hydrogen,
     $           file=evtname(1),status='old')
            no_open_files = no_open_files + 1
         endif
         if (helium_file) then
            open (unit=lun_helium,
     $           file=evtname(2),status='old')
            no_open_files = no_open_files + 1
         endif
         if (nitrogen_file) then
            open (unit=lun_nitrogen,
     $           file=evtname(3),status='old')
            no_open_files = no_open_files + 1
         endif

C     Determine which element, or combination of elements, the user 
C     has selected as targets.  There are seven possible combinations 
C     of elements, and we now set the value of the variable
c     "option_number"
C     according to the combination selected by the user.  The key is
C     shown below.  

C     Hydrogen          Helium           Nitrogen    option number

C     Y                N                  N             1
C     N                Y                  N             2
C     N                N                  Y             3
C     Y                Y                  N             4
C     Y                N                  Y             5
C     N                Y                  Y             6
C     Y                Y                  Y             7

         
         option_number = 0
         if (no_open_files .eq. 1) then
            if (hydrogen_file) then
               option_number = 1
               go to 20
            elseif (helium_file) then
               option_number = 2
               go to 20
            else
               option_number = 3
               go to 20
            endif
         endif
         if (no_open_files .eq. 2) then
            if (hydrogen_file .and. helium_file) then
               option_number = 4
               go to 20
            elseif (hydrogen_file .and. nitrogen_file) then
               option_number = 5
               go to 20
            else 
               option_number = 6
               go to 20
            endif
         endif
         if (no_open_files .eq. 3)then
            option_number = 7
            go to  20
         endif

C     There is no way for the user to reach this point unless he failed 
C     to open at least one file.

         write (lun_output,600)
         goto 17
 20      continue 

C     Ascertain which sections of the beampipe the user wants to 
C     activate, and calculate the total thicknesses of the target
c     elements
C     if the users has selected to use the actual profiles.

C     ************************************************************
C     ************************************************************
C     $NOTE$

C     The following block of code is not to be executed if 
C     universal_vertex = .true. because user must specify the unique 
C     vertex under such circumstances.

C     ************************************************************
C     ************************************************************
         if ( .not. universal_vertex ) then
            total_length = 0.0e0
            total_length_hydrogen = 0.0e0
            total_length_helium = 0.0e0
            total_length_nitrogen = 0.0e0
            total_hydrogen_thickness = 0.0e0
            total_helium_thickness = 0.0e0
            total_nitrogen_thickness = 0.0e0
            do 21 i = 1,4
               if (enable_region(i)) then
                  total_length = total_length + length_region(i)
                  if (hydrogen_file) then
                     total_length_hydrogen = total_length_hydrogen +
     $                    length_region(i)
                     if (true_distributions)then
                        total_hydrogen_thickness = total_hydrogen_thickn
     $                       ess + hydrogen_thickness(i)
                     endif
                  endif
                  if (helium_file) then
                     if (true_distributions) then
                        total_helium_thickness = total_helium_thickness  
     $                       + helium_thickness(i)
                        if (i .eq. 2 .or. i .eq. 4) then
                           total_length_helium = total_length_helium +
     $                          length_region(i)
                        endif
                     else
                        total_length_helium = total_length_helium + 
     $                       length_region (i)
                     endif
                  endif 
                  if (nitrogen_file) then
                     if (true_distributions) then
                        total_nitrogen_thickness = total_nitrogen_thickn
     $                       ess + nitrogen_thickness(i)
                        if (i .eq. 1 .or. i .eq. 3) then
                           total_length_nitrogen = total_length_nitrogen 
     $                          + length_region(i)
                        endif 
                     else 
                        total_length_nitrogen = total_length_nitrogen + 
     $                       length_region (i)
                     endif 
                  endif 
               endif 
 21         continue 

C     Check for possible blunders on the part of the user that would
C     create havoc later on. 

            if (total_length .eq. 0.0e0)then
               write (lun_output,601)
               goto 17
            endif 
            if (helium_file .and. true_distributions) then
               if (.not. enable_region (2) .and. .not. enable_region(4))
     $              then
                  write (lun_output, 602)
                  goto 17
               endif
            endif
            if (nitrogen_file .and. true_distributions) then
               if (.not. enable_region (1) .and. .not. enable_region(3))
     $              then
                  write (lun_output, 603)
                  goto 17
               endif
            endif
         endif

C     Now calculate the probabilities for interactions with the various 
C     targets.

         probability_hydrogen = 0.0e0
         probability_helium = 0.0e0
         probability_nitrogen = 0.0e0
         if (hydrogen_file) then
            if (true_distributions) then
               probability_hydrogen = total_hydrogen_thickness *
     $              sigma_au_hydrogen
            else 
               probability_hydrogen = sigma_au_hydrogen
            endif 
         endif 
         if (helium_file) then
            if (true_distributions) then
               probability_helium = total_helium_thickness *
     $              sigma_au_helium
            else 
               probability_helium = sigma_au_helium
            endif
         endif 
         if (nitrogen_file) then
            if (true_distributions) then
               probability_nitrogen = total_nitrogen_thickness *
     $              sigma_au_nitrogen
            else 
               probability_nitrogen = sigma_au_nitrogen
            endif 
         endif 
         total_probability = probability_hydrogen + 
     $        probability_helium + probability_nitrogen

C     Calculate the relative probabilities for interactions with
C     the chosen targets.

         rel_prob_hydrogen = 0.0e0
         rel_prob_helium = 0.0e0
         rel_prob_nitrogen = 0.0e0
         if (hydrogen_file) then
            rel_prob_hydrogen = probability_hydrogen / 
     $           total_probability
         endif
         if (helium_file) then
            rel_prob_helium = probability_helium / 
     $           total_probability
         endif
         if (nitrogen_file) then
            rel_prob_nitrogen = probability_nitrogen / 
     $           total_probability
         endif

C     Clear arrays and variables.

         do 200 i = 1, no_theta_bins
            no_pi_plus_theta(i) = 0.0e0
            no_pi_minus_theta(i) = 0.0e0
            no_pi_naught_theta(i) = 0.0e0
            no_ka_plus_theta(i) = 0.0e0
            no_ka_minus_theta(i) = 0.0e0
            no_ka_naught_theta(i)= 0.0e0
            no_protons_theta(i) = 0.0e0
            no_neutrons_theta(i) = 0.0e0
            no_gammas_theta(i) = 0.0e0
            no_electrons_theta(i) = 0.0e0
            no_positrons_theta(i) = 0.0e0
 200     continue 
         do 2011 i = 1, no_bins_pz
            no_pi_plus_pz(i) = 0.0e0
            no_pi_minus_pz(i) = 0.0e0
            no_pi_naught_pz(i) = 0.0e0
            no_ka_plus_pz (i) = 0.0e0
            no_ka_minus_pz(i) = 0.0e0
            no_ka_naught_pz(i) = 0.0e0
            no_gammas_pz(i) = 0.0e0
            no_electrons_pz(i) = 0.0e0
            no_positrons_pz(i) = 0.0e0
 2011    continue 
         do 2012 i = 1, no_bins_ptot
            no_pi_plus_ptot(i) = 0.0e0
            no_pi_minus_ptot(i) = 0.0e0
            no_pi_naught_ptot(i) = 0.0e0
            no_ka_plus_ptot(i) = 0.0e0
            no_ka_minus_ptot(i) = 0.0e0
            no_ka_naught_ptot(i) = 0.0e0
            no_gammas_ptot(i) = 0.0e0
            no_electrons_ptot(i) = 0.0e0
            no_positrons_ptot(i) = 0.0e0
 2012    continue 
         do 2222 i = 1, no_bins_ptot_nucleon
            no_protons_ptot(i) = 0.0e0
            no_neutrons_ptot(i) = 0.0e0
            no_protons_pz(i) = 0.0e0
            no_neutrons_pz(i) = 0.0e0
 2222    continue 
         do 2013 i = 1, 36
            no_pi_plus_phi(i) = 0.0e0
            no_pi_minus_phi(i) = 0.0e0
            no_pi_naught_phi(i) = 0.0e0
            no_ka_plus_phi(i) = 0.0e0
            no_ka_minus_phi(i) = 0.0e0
            no_ka_naught_phi(i) = 0.0e0
            no_protons_phi(i) = 0.0e0
            no_neutrons_phi(i) = 0.0e0
            no_gammas_phi(i) = 0.0e0
            no_electrons_phi(i) = 0.0e0
            no_positrons_phi(i) = 0.0e0
 2013    continue 
         do 2014 i = 1, no_cos_theta_bins
            no_pi_plus_cos_theta(i) = 0.0e0
            no_pi_minus_cos_theta(i) = 0.0e0
            no_pi_naught_cos_theta(i) = 0.0e0
            no_ka_plus_cos_theta(i) = 0.0e0
            no_ka_minus_cos_theta(i) = 0.0e0
            no_ka_naught_cos_theta(i) = 0.0e0
            no_protons_cos_theta(i) = 0.0e0
            no_neutrons_cos_theta(i) = 0.0e0
            no_gammas_cos_theta(i) = 0.0e0
            no_electrons_cos_theta(i) = 0.0e0
            no_positrons_cos_theta(i) = 0.0e0
 2014    continue 
         do 205 i = 1, no_bins_pions
            no_pi_plus(i) = 0.0e0
            no_pi_minus(i) = 0.0e0
            no_pi_naught(i) = 0.0e0
 205     continue 
         do 206 i = 1, no_bins_nucleons
            no_neutrons(i) = 0.0e0
            no_protons(i) = 0.0e0
 206     continue 
         do 207 i = 1, no_bins_kaons
            no_kaon_plus(i) = 0.0e0
            no_kaon_minus(i) = 0.0e0
            no_kaon_0 (i) = 0.0e0
 207     continue 
         do 208 i = 1, no_bins_gamma
            no_gamma(i) = 0.0e0
 208     continue 
         do 209 i = 1, no_bins_leptons
            no_electron(i) = 0.0e0
            no_positron(i) = 0.0e0
 209     continue 
         number_hydrogen_int = 0
         number_helium_int = 0
         number_nitrogen_int = 0
         do 2223 i = 1, 100
            z_plot_hydrogen (i) = 0.0e0
            z_plot_helium (i) = 0.0e0
            z_plot_nitrogen (i) = 0.0e0
 2223    continue 
      endif

C     This is the end of the statements that are executed only upon
C     first call to this routine.  Begin the code that executed upon 
C     every call to the routine. 


C     Some variables must be cleared at the beginning of each new event.

      number_positive_pions = 0.0e0
      number_negative_pions = 0.0e0
      number_neutral_pions = 0.0e0
      number_protons = 0.0e0
      number_neutrons = 0.0e0
      number_positive_kaons = 0.0e0
      number_negative_kaons = 0.0e0
      number_neutral_kaons = 0.0e0
      number_gammas = 0.0e0
      number_positrons = 0.0e0
      number_electrons = 0.0e0
      region_1_segment_1 = .false.
      region_1_segment_2 = .false.
      region_2_segment_1 = .false.
      region_2_segment_2 = .false.
      region_3 = .false.
      region_4_segment_1 = .false.
      region_4_segment_2 = .false.
      region_4_segment_3 = .false.
      region_4_segment_4 = .false.

      ievstat = -1

C     Determine the identity of the target.

      hydrogen_interaction = .false.
      helium_interaction = .false.
      nitrogen_interaction = .false.
C     random_number = r_lcran(0)
      random_number = rndm()
      if (no_open_files .eq. 1) then
         if(option_number .eq. 1) then
            hydrogen_interaction = .true. 
            goto 25
         elseif (option_number .eq. 2) then
            helium_interaction = .true. 
            goto 25
         else 
            nitrogen_interaction = .true. 
            goto 25
         endif 
      endif
      if (no_open_files .eq. 2) then
         if (option_number .eq. 4)then
            if (random_number .le. rel_prob_hydrogen) then
               hydrogen_interaction = .true.
               goto 25
            else
               helium_interaction = .true. 
               goto 25
            endif 
         elseif (option_number .eq. 5) then
            if (random_number .le. rel_prob_hydrogen) then
               hydrogen_interaction = .true. 
               goto 25
            else
               nitrogen_interaction = .true. 
               goto 25
            endif 
         else 
            if (random_number .le. rel_prob_helium) then
               helium_interaction = .true. 
               goto 25
            else
               nitrogen_interaction = .true.
               goto 25
            endif 
         endif 
      endif
      if (no_open_files .eq. 3) then
         if (random_number .le. rel_prob_hydrogen) then
            hydrogen_interaction = .true.
            goto 25
         elseif (random_number .le. (rel_prob_hydrogen +
     $           rel_prob_helium)) then
            helium_interaction = .true. 
            goto 25
         else 
            nitrogen_interaction = .true. 
            goto 25
         endif 
      endif 

C     In principle the following statement should never be executed.

      write (lun_output, 604)
      goto 17
 25   continue 
C     ********************************************************
C     ********************************************************
C     $NOTE$

C     If universal_vertex = .true.,  then omit the following block of 
C     code because the origin of the interaction must be uniquely 
C     specified by the user.


C     ********************************************************
C     ********************************************************
      if (.not. universal_vertex ) then

C     Locate the distance between the origin and the position of the 
C     interaction.  In this context "distance" means distance as
c     measured
C     along enabled regions only!

         do 30 i = 1, 4
            cuts(i) = -1.0e0
 30      continue 
         sum = 0.0e0
         numerator = 0.0e0
         denominator = 0.0e0
         if (hydrogen_interaction) then
            denominator = total_hydrogen_thickness
            goto 29
         elseif (helium_interaction) then
            denominator = total_helium_thickness
            goto 29
         else 
            denominator = total_nitrogen_thickness
         endif 
 29      continue 
C     random_number = r_lcran(0)
         random_number = rndm()
         if (hydrogen_interaction) then
            if (true_distributions) then
               do 310 i = 1,4 
                  if (enable_region(i)) then
                     numerator = numerator + hydrogen_thickness(i)
                     cuts(i) = numerator / denominator
                  endif 
 310           continue 
               first_enabled_region = .true. 
               do 311 i = 1,4

C     Note that the following inequality can never be satisfied if
C     region i is not enabled. 

                  if (random_number .le. cuts(i)) then
                     interaction_region = i
                     if (first_enabled_region) then
                        delta_z = (random_number / cuts(i)) * 
     $                       length_region(i)
                     else
                        temp = -1.0e0
                        do 7613 k = 1, i - 1
                           if (cuts(k) .gt. temp) then
                              temp = cuts(k)
                           endif 
 7613                   continue 
                        temp01 = cuts(i) - temp
                        delta_z = ((random_number - temp) / temp01) * 
     $                       length_region (i)
                     endif 
                     goto 28
                  elseif (first_enabled_region)then
                     if (cuts(i) .gt. 0.0e0) then
                        first_enabled_region = .false. 
                     endif 
                  endif 
 311           continue 
            else

C     Arrive here if user wants uniform densities.

               distance = random_number * total_length_hydrogen
               do 312 i = 1,4
                  if (enable_region(i)) then
                     test_value  = distance - length_region (i)
                     if (test_value .gt. 0.0e0) then
                        distance = distance - length_region (i)
                     else
                        delta_z = distance
                        interaction_region = i
                        goto 28
                     endif 
                  endif 
 312           continue 
            endif 
         endif 
         if (helium_interaction) then
            if (true_distributions) then
               do 31 i = 1,4
                  if (enable_region(i)) then
                     numerator = numerator + helium_thickness(i)
                     cuts(i) = numerator / denominator
                  endif 
 31            continue 
               first_enabled_region = .true. 
               do 32 i = 2,4,2

C     Note that the following inequality can never be satisfied if
C     region i is not enabled. 

                  if (random_number .le. cuts(i)) then
                     interaction_region = i
                     if (first_enabled_region) then
                        delta_z = (random_number / cuts(i)) * 
     $                       length_region(i)
                     else 
                        temp = -1.0e0
                        do 7614 k = 1, i - 1
                           if (cuts(k) .gt. temp) then
                              temp = cuts(k)
                           endif 
 7614                   continue 
                        temp01 = cuts(i) - temp
                        delta_z = ((random_number - temp) / temp01) * 
     $                       length_region (i)
                     endif 
                     goto 28
                  elseif (first_enabled_region)then
                     if (cuts(i) .gt. 0.0e0) then
                        first_enabled_region = .false. 
                     endif 
                  endif 
 32            continue 
            else

C     Arrive here if user wants uniform densities.

               distance = random_number * total_length_helium
               do 33 i = 1,4
                  if (enable_region(i)) then
                     test_value  = distance - length_region (i)
                     if (test_value .gt. 0.0e0) then
                        distance = distance - length_region (i)
                     else
                        delta_z = distance
                        interaction_region = i
                        goto 28
                     endif 
                  endif 
 33            continue 
            endif 
         endif 
         if (nitrogen_interaction) then
            if (true_distributions) then
               do 319 i = 1,4
                  if (enable_region(i)) then
                     numerator = numerator + nitrogen_thickness(i)
                     cuts(i) = numerator / denominator
                  endif 
 319           continue 
               first_enabled_region = .true. 
               do 321 i = 1,3,2

C     Note that the following inequality can never be satisfied if
C     region i is not enabled. 

                  if (random_number .le. cuts(i)) then
                     interaction_region = i
                     if (first_enabled_region) then
                        delta_z = (random_number / cuts(i)) * 
     $                       length_region(i)
                     else 
                        temp = -1.0e0
                        do 7615 k = 1, i - 1
                           if (cuts(k) .gt. temp) then
                              temp = cuts(k)
                           endif 
 7615                   continue 
                        temp01 = cuts(i) - temp
                        delta_z = ((random_number - temp)/ temp01) * 
     $                       length_region (i)
                     endif 
                     goto 28
                  elseif (first_enabled_region)then
                     if (cuts(i) .gt. 0.0e0) then
                        first_enabled_region = .false. 
                     endif 
                  endif 
 321           continue 
            else

C     Arrive here if user wants uniform densities.

               distance = random_number * total_length_nitrogen
               do 333 i = 1,4
                  if (enable_region(i)) then
                     test_value  = distance - length_region (i)
                     if (test_value .gt. 0.0e0) then
                        distance = distance - length_region (i)
                     else
                        delta_z = distance
                        interaction_region = i
                        goto 28
                     endif 
                  endif 
 333           continue 
            endif 
         endif 

C     In principle the following statement should never be executed.

         write (lun_output, 605)
         goto 17
 28      continue 

C     At this point we know the identity of the target, the region in
c     which the interaction occurred, and the z coordinate as 
C     measured along the z-axis from the end of the region that is 
C     closest to the origin.  Now generate authentic x and z 
C     coordinates.  The quantity delta_z that is returned by the code
C     above is equal to delta_z only in the small angle approximation.

         if (interaction_region .eq. 1) then
            if (delta_z .le. 1.16501e+03) then
               x_coordinate_interaction = 0.0e0
               z_coordinate_interaction = delta_z
               region_1_segment_1 = .true.
               keep_this_angle = angle_zz_r1_s1
            else
               temp = delta_z - 1.16501e+03
               temp_z = temp / trig_factor_1
               x_coordinate_interaction = slope_line_1 * temp_z
               z_coordinate_interaction = temp_z + 1.16501e+03
               region_1_segment_2 = .true. 
               keep_this_angle = angle_zz_r1_s2
            endif
         elseif (interaction_region .eq. 2 ) then
            if (delta_z .le. 3.00e+02) then
               temp_z = delta_z / trig_factor_1
               temp_x = slope_line_1 * temp_z
               x_coordinate_interaction = x_coordinate_valve_1 
     $              + temp_x
               z_coordinate_interaction = z_coordinate_valve_1
     $              + temp_z
               region_2_segment_1 = .true.
               keep_this_angle = angle_zz_r2_s1
            else
               delta_z = delta_z - 3.00e+02
               temp_z = delta_z / trig_factor_2
               temp_x = slope_line_2 * temp_z
               x_coordinate_interaction = x_coord_d0 
     $              + temp_x
               z_coordinate_interaction = z_coord_d0
     $              + temp_z
               region_2_segment_2 = .true.
               keep_this_angle = angle_zz_r2_s2
            endif
         elseif (interaction_region .eq. 3) then
            region_3 = .true.
            keep_this_angle = angle_zz_r3
            temp_z = delta_z /trig_factor_2
            temp_x = slope_line_2 * temp_z
            x_coordinate_interaction = x_coordinate_valve_2 
     $           + temp_x
            z_coordinate_interaction = z_coordinate_valve_2
     $           + temp_z
         else
            if (delta_z .le. length_r4_segment_1) then
               region_4_segment_1 = .true.
               keep_this_angle = angle_zz_r4_s1
               temp_z = delta_z /trig_factor_2
               temp_x = slope_line_2 * temp_z
               x_coordinate_interaction = x_coordinate_valve_3
     $              + temp_x
               z_coordinate_interaction = z_coordinate_valve_3 
     $              + temp_z
            else
               delta_z = delta_z - length_r4_segment_1
            endif
            if (delta_z .le. length_r4_segment_2) then
               region_4_segment_2 = .true.
               keep_this_angle = angle_zz_r4_s2
               temp_z = delta_z /trig_factor_3
               temp_x = slope_line_3 * temp_z
               x_coordinate_interaction = x_coord_d5_end + temp_x
               z_coordinate_interaction = z_coord_d5_end + temp_z
            else
               delta_z = delta_z - length_r4_segment_2
            endif
            if (delta_z .le. length_r4_segment_3) then
               region_4_segment_3 = .true.
               keep_this_angle = angle_zz_r4_s3
               temp_z = delta_z /trig_factor_4
               temp_x = slope_line_4 * temp_z
               x_coordinate_interaction = x_coord_d6_end + temp_x
               z_coordinate_interaction = z_coord_d6_end + temp_z
            else
               region_4_segment_4 = .true.
               keep_this_angle = angle_zz_r4_s4
               delta_z = delta_z - length_r4_segment_3
               temp_z = delta_z /trig_factor_4
               temp_x = slope_line_4 * temp_z
               x_coordinate_interaction = x_coord_d8_end + temp_x
               z_coordinate_interaction = z_coord_d8_end + temp_z
            endif
         endif

C     ****************************************************************
C     ************************* OLD VERSION OF CODE SHOWN BELOW *****
C     z_coordinate_interaction = least_z_region (interaction_region)
C     $     + delta_z
C     if (interaction_region .eq. 1) then
C     if (z_coordinate_interaction .le. 1.16501e+03)then
C     x_coordinate_interaction = 0.0e0
C     else
C     x_coordinate_interaction = intercept_line_1 + 
C     $           slope_line_1 * z_coordinate_interaction
C     endif
C     elseif (interaction_region .eq. 2)then
C     if (z_coordinate_interaction .le. 2.22998e+03) then
C     x_coordinate_interaction = intercept_line_1 + 
C     $           slope_line_1 * z_coordinate_interaction
C     else 
C     x_coordinate_interaction = intercept_line_2 + 
C     $           slope_line_2 * z_coordinate_interaction
C     endif 
C     else 
C     x_coordinate_interaction = intercept_line_2 + 
C     $        slope_line_2 * z_coordinate_interaction 
C     endif 

C     *************  OLD VERSION OF CODE SHOWN ABOVE ****************
C     ***************************************************************

C     Note that the preceding code distributes the interactions over 
C     positive z and negative x.  Negative x corresponds to the outside 
C     arc.  These signs are appropriate for bombarding our muon arm with
C     background but inappropriate if the objective is to examine
c     particles
C     that pass through the second muon arm, the central volume of
c     PHENIX, 
C     and enter our muon arm with approximately the same slope as
c     particles
C     that originate at the origin.  Permit change of sign here. 

         x_coordinate_interaction = change_sign_x  * 
     $        x_coordinate_interaction
         z_coordinate_interaction = change_sign_z  * 
     $        z_coordinate_interaction

C     Load the xyz "prebuffer" here.

         xyz(1) = x_coordinate_interaction
         xyz(2) = 0.0e0
         xyz(3) = z_coordinate_interaction
      endif

C     Store the location of the interactions.

      if (hydrogen_interaction) then
         z_in_meters = abs(z_coordinate_interaction)/ 1.0e+02
         call serve(z_in_meters, lower_lim_z_plot, upper_lim_z_plot,
     $        bin_width_z_plot, index)
         if (index .ge. 1 .and. index. le. no_bins_z_plot) then
            z_plot_hydrogen (index) = z_plot_hydrogen(index) + 1.0e0
         endif 
      elseif (helium_interaction) then
         z_in_meters = abs(z_coordinate_interaction)/ 1.0e+02
         call serve(z_in_meters, lower_lim_z_plot, upper_lim_z_plot,
     $        bin_width_z_plot, index)
         if (index .ge. 1 .and. index. le. no_bins_z_plot) then
            z_plot_helium (index) = z_plot_helium(index) + 1.0e0
         endif 
      else 
         z_in_meters = abs(z_coordinate_interaction)/ 1.0e+02
         call serve(z_in_meters, lower_lim_z_plot, upper_lim_z_plot,
     $        bin_width_z_plot, index)
         if (index .ge. 1 .and. index. le. no_bins_z_plot) then
            z_plot_nitrogen (index) = z_plot_nitrogen(index) + 1.0e0
         endif 
      endif 

C     Read in the event number and the number of  particles produced
C     in the event.

      if (hydrogen_interaction) then
         lun_input = lun_hydrogen
         number_hydrogen_int = number_hydrogen_int + 1
         goto 26
      elseif (helium_interaction)then
         lun_input = lun_helium
         number_helium_int = number_helium_int + 1
         goto 26
      else 
         lun_input = lun_nitrogen
         number_nitrogen_int = number_nitrogen_int + 1
         goto 26
      endif 
 26   continue 
      numevt = numevt + 1
      read(lun_input, 1001, end = 17) mxtot, lbl, ievent, car2 
      write (lun_output, 1003) ievent, mxtot

C     Begin loop over produced particles in this event.

      DO 1 I=1, mxtot
         read(lun_input, 1002) idtot(i), p2_temp, p3_temp,
     $        p4_temp

C     We must specify the angle between the direction of motion and the
C     positive z axis of the PHENIX coordinate system.  these angles are
C     stored in the variables angle_zz_r4_s1, etc. 

         pptot(2, I) = p4_temp * sind(keep_this_angle) 
     $        + p2_temp * cosd(keep_this_angle)
         pptot(4, I) = p4_temp * cosd(keep_this_angle)
     $        - p2_temp * sind(keep_this_angle)
         pptot (3, I) = p3_temp

C     Transfer subscripted variables to non-subscripted variables.

         px = pptot (2, I)
         py = pptot (3, I)
         pz = pptot (4, I)
         total_momentum = sqrt (px * px + py * py + pz * pz)
         igeant = idtot (i)
         call GFPART (igeant, car3, itrtyp, ion_mass,
     $        ion_charge, ion_lifetime, ub, nwb) 
         pptot (1, I) = sqrt (total_momentum**2 + ion_mass**2)
         

C     These particles are their own parents.

         id_parent(i) = idtot(i)
         azimuthal_angle = atan2d ( py, px )
         cosine_of_angle = pz / total_momentum
         zenith_angle = acosd (cosine_of_angle)

C     Count the number of particles of each type.

         if (igeant .ge. 1 .and. igeant .le. 200) then
            i_cnt_particles (igeant) = 
     $           i_cnt_particles (igeant) + 1
         endif

C     Leave some space for counting the number of particles.


C     Increment distributions for positive pions.

         if ( igeant .eq. 8 ) then
            number_positive_pions = number_positive_pions + 1
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_pi_plus_theta (index) = no_pi_plus_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0, 
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_pi_plus_phi (index) = no_pi_plus_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_pi_plus_cos_theta (index) = 
     $              no_pi_plus_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_pi_plus_ptot (index) = no_pi_plus_ptot (index) + 1.0e0
            endif
            temp = abs (pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_pi_plus_pz (index) = no_pi_plus_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for negative pions.

         if ( igeant .eq. 9 ) then
            number_negative_pions = number_negative_pions + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_pi_minus_theta (index) = no_pi_minus_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0, 
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_pi_minus_phi (index) = no_pi_minus_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_pi_minus_cos_theta (index) = 
     $              no_pi_minus_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_pi_minus_ptot (index) = no_pi_minus_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs (pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_pi_minus_pz (index) = no_pi_minus_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for neutral pions. 

         if ( igeant .eq. 7 ) then
            number_neutral_pions = number_neutral_pions + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_pi_naught_theta (index) = no_pi_naught_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_pi_naught_phi (index) = no_pi_naught_phi (index) 
     $              + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_pi_naught_cos_theta (index) = 
     $              no_pi_naught_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_pi_naught_ptot (index) = no_pi_naught_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs(pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_pi_naught_pz (index) = no_pi_naught_pz (index) 
     $              + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for positive kaons.

         if ( igeant .eq. 11 ) then
            number_positive_kaons = number_positive_kaons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_ka_plus_theta (index) = no_ka_plus_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_ka_plus_phi (index) = no_ka_plus_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_ka_plus_cos_theta (index) = 
     $              no_ka_plus_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_ka_plus_ptot (index) = no_ka_plus_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs(pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_ka_plus_pz (index) = no_ka_plus_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for negative kaons. 

         if ( igeant .eq. 12 ) then
            number_negative_kaons = number_negative_kaons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_ka_minus_theta (index) = no_ka_minus_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_ka_minus_phi (index) = no_ka_minus_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_ka_minus_cos_theta (index) = 
     $              no_ka_minus_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_ka_minus_ptot (index) = no_ka_minus_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs(pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_ka_minus_pz (index) = no_ka_minus_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for neutral kaons.

         if ( igeant .eq. 10 .or. igeant .eq. 16 ) then
            number_neutral_kaons = number_neutral_kaons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_ka_naught_theta (index) = no_ka_naught_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_ka_naught_phi (index) = no_ka_naught_phi (index) 
     $              + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_ka_naught_cos_theta (index) = 
     $              no_ka_naught_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_ka_naught_ptot (index) = no_ka_naught_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs (pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_ka_naught_pz (index) = no_ka_naught_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for protons.

         if ( igeant .eq. 14 ) then
            number_protons = number_protons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_protons_theta (index) = no_protons_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_protons_phi (index) = no_protons_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_protons_cos_theta (index) = 
     $              no_protons_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot_nucleon)then
               no_protons_ptot (index) = no_protons_ptot (index) + 1.0e0
            endif
            temp = abs(pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot_nucleon)then
               no_protons_pz (index) = no_protons_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for neutrons.

         if ( igeant .eq. 13 ) then
            number_neutrons = number_neutrons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_neutrons_theta (index) = no_neutrons_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_neutrons_phi (index) = no_neutrons_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_neutrons_cos_theta (index) = 
     $              no_neutrons_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot_nucleon)then
               no_neutrons_ptot (index) = no_neutrons_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs (pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot_nucleon)then
               no_neutrons_pz (index) = no_neutrons_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for gammas. 

         if ( igeant .eq. 1 ) then
            number_gammas = number_gammas + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_gammas_theta (index) = no_gammas_theta (index) + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_gammas_phi (index) = no_gammas_phi (index) + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_gammas_cos_theta (index) = 
     $              no_gammas_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_gammas_ptot (index) = no_gammas_ptot (index) + 1.0e0
            endif
            temp = abs (pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_gammas_pz (index) = no_gammas_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for positrons.

         if ( igeant .eq. 2 ) then
            number_positrons = number_positrons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_positrons_theta (index) = no_positrons_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_positrons_phi (index) = no_positrons_phi (index) 
     $              + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_positrons_cos_theta (index) = 
     $              no_positrons_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_positrons_ptot (index) = no_positrons_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs(pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_positrons_pz (index) = no_positrons_pz (index) + 1.0e0
            endif
            go to 1
         endif

C     Increment distributions for electrons.

         if ( igeant .eq. 3 ) then
            number_electrons = number_electrons + 1.0e0
            call serve (zenith_angle, theta_low, theta_high, 
     $           theta_bin_width, index)
            if (index .ge. 1 .and. index .le. no_theta_bins) then
               no_electrons_theta (index) = no_electrons_theta (index) 
     $              + 1.0e0
            endif
            call serve (azimuthal_angle, -180.0e0, 180.0e0, 10.0e0,
     $           index)
            if (index .ge. 1 .and. index .le. 36) then
               no_electrons_phi (index) = no_electrons_phi (index) 
     $              + 1.0e0
            endif
            call serve (cosine_of_angle, -1.0e0, +1.0e0, 5.0e-02, 
     $           index)
            if (index .ge. 1 .and. index .le. no_cos_theta_bins) then
               no_electrons_cos_theta (index) = 
     $              no_electrons_cos_theta(index) + 1.0e0
            endif
            call serve (total_momentum, lower_lim_ptot, upper_lim_ptot, 
     $           bin_width_ptot, index)
            if ( index .ge. 1 .and. index .le. no_bins_ptot) then
               no_electrons_ptot (index) = no_electrons_ptot (index) 
     $              + 1.0e0
            endif
            temp = abs (pz)
            call serve (temp, lower_lim_pz, upper_lim_pz,
     $           bin_width_pz, index)
            if ( index .ge. 1 .and. index .le. no_bins_pz) then
               no_electrons_pz (index) = no_electrons_pz (index) + 1.0e0
            endif
         endif 
 1    continue 

C     An entire event has now been processed.  Before proceeding to
C     the next event, increment the numbers distributions.

      temp = float (number_positive_pions)
      call serve (temp,0.0, 99.0, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_pions) then
         no_pi_plus (index) = no_pi_plus (index) + 1.0e0
      endif
      temp = float (number_negative_pions)
      call serve (temp,0.0, 99.0, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_pions) then
         no_pi_minus (index) = no_pi_minus (index) + 1.0e0
      endif
      temp = float (number_neutral_pions)
      call serve (temp,0.0, 99.0, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_pions) then
         no_pi_naught (index) = no_pi_naught (index) + 1.0e0
      endif
      temp01 = float(no_bins_kaons - 1)
      temp = float (number_neutral_kaons)
      call serve (temp,0.0, temp01, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_kaons) then
         no_kaon_0 (index) = no_kaon_0 (index) + 1.0e0
      endif
      temp = float (number_positive_kaons)
      call serve (temp,0.0, temp01, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_kaons) then
         no_kaon_plus (index) = no_kaon_plus (index) + 1.0e0
      endif
      temp = float (number_negative_kaons)
      call serve (temp,0.0, temp01, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_kaons) then
         no_kaon_minus (index) = no_kaon_minus (index) + 1.0e0
      endif
C     temp = float (number_protons)
C     call serve (temp, 55.0, 104.0, 1.0, index)
C     if (index .ge. 1 .and. index .le. no_bins_nucleons) then
C     no_protons (index) = no_protons (index) + 1.0e0
C     endif
C     temp = float (number_neutrons)
C     call serve (temp, 105.0, 154.0, 1.0, index)
C     if (index .ge. 1 .and. index .le. no_bins_nucleons) then
C     no_neutrons (index) = no_neutrons (index) + 1.0e0
C     endif
      temp = float (number_gammas)
      temp01 = float (no_bins_gamma - 1)
      call serve (temp,0.0, temp01, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_gamma) then
         no_gamma (index) = no_gamma (index) + 1.0e0
      endif
      temp01 = float (no_bins_leptons - 1)
      temp = float (number_electrons)
      call serve (temp,0.0, temp01, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_leptons) then
         no_electron (index) = no_electron (index) + 1.0e0
      endif
      temp = float (number_positrons)
      call serve (temp,0.0, temp01, 1.0, index)
      if (index .ge. 1 .and. index .le. no_bins_leptons) then
         no_positron (index) = no_positron (index) + 1.0e0
      endif

C     Set ievstat = -1 to signal a successful event.

      ievstat = -1
      return
 17   continue 

C     Arrive here if end of file was encountered or all  events read
c     in.


C     Set the value of ievstat to +2 in order to signal the end of file.

      ievstat = 2
      write (lun_output,80) (j, i_cnt_particles(j),j = 1,200)
      write (lun_output,607) hydrogen_file, helium_file, nitrogen_file,
     $     true_distributions
      write (lun_output,608) enable_region
      write (lun_output, 609) probability_hydrogen, probability_helium,
     $     probability_nitrogen
      write (lun_output, 610) rel_prob_hydrogen, rel_prob_helium, 
     $     rel_prob_nitrogen
      write (lun_output, 611) total_hydrogen_thickness, 
     $     total_helium_thickness, total_nitrogen_thickness
      write (lun_output, 612) total_length_hydrogen, 
     $     total_length_helium, total_length_nitrogen
      write (lun_output,606) number_hydrogen_int, number_helium_int,
     $     number_nitrogen_int


C     Close the open hijet files.

      if (hydrogen_file) then
         close (unit=lun_hydrogen, status='keep')
      elseif (helium_file) then
         close (unit=lun_helium, status='keep')
      elseif (nitrogen_file) then
         close (unit= lun_nitrogen,status = 'keep')
      endif 

C     Because of concern over too many open logical units, re-use the
C     logical unit number for hydrogen since that file has been closed.


C     Write out the distributions in z for each of the arrays
C     z_plot_hydrogen, z_plot_helium, and z_plot_nitrogen.

      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/z_plot_hydrogen',
     $     status='new')
      write (lun_hydrogen,*)z_plot_hydrogen
      close (unit=lun_hydrogen, status = 'keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/z_plot_helium',
     $     status='new')
      write (lun_hydrogen,*)z_plot_helium
      close (unit=lun_hydrogen, status = 'keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/z_plot_nitrogen',
     $     status='new')
      write (lun_hydrogen,*)z_plot_nitrogen
      close (unit=lun_hydrogen, status = 'keep')

C     Write out distributions with respect to total momentum. 

      do 72 i = 1, no_bins_ptot
         no_pi_plus_ptot (i) = no_pi_plus_ptot (i)/ bin_width_ptot
         no_pi_minus_ptot (i) = no_pi_minus_ptot (i)/ bin_width_ptot
         no_pi_naught_ptot (i) = no_pi_naught_ptot (i)/ bin_width_ptot
         no_ka_plus_ptot (i) = no_ka_plus_ptot (i)/ bin_width_ptot
         no_ka_minus_ptot (i) = no_ka_minus_ptot (i)/ bin_width_ptot
         no_ka_naught_ptot (i) = no_ka_naught_ptot (i)/ bin_width_ptot
C     no_protons_ptot (i) = no_protons_ptot (i)/ bin_width_ptot
C     no_neutrons_ptot (i) = no_neutrons_ptot (i)/ bin_width_ptot
         no_gammas_ptot (i) = no_gammas_ptot (i)/ bin_width_ptot
         no_electrons_ptot (i) = no_electrons_ptot (i)/ bin_width_ptot
         no_positrons_ptot (i) = no_positrons_ptot (i)/ bin_width_ptot
 72   continue 
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_plus_dptot',
     $     status='new')
      write (lun_hydrogen,*) no_pi_plus_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_minus_dptot',
     $     status='new')
      write (lun_hydrogen,*) no_pi_minus_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_naught_dptot', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_naught_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_plus_dptot',
     $     status='new')
      write (lun_hydrogen,*) no_ka_plus_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_minus_dptot', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_minus_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_naught_dptot',
     $     status='new')
      write (lun_hydrogen,*) no_ka_naught_ptot
      close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dproton_dptot', 
C     $     status='new')
C     write (lun_hydrogen,*) no_protons_ptot
C     close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dneutron_dptot',
C     $     status='new')
C     write (lun_hydrogen,*) no_neutrons_ptot
C     close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dgamma_dptot', 
     $     status='new')
      write (lun_hydrogen,*) no_gammas_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_delectron_dptot', 
     $     status='new')
      write (lun_hydrogen,*) no_electrons_ptot
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpositron_dptot',
     $     status='new')
      write (lun_hydrogen,*) no_positrons_ptot
      close (unit=lun_hydrogen, status='keep')

C     write out distributions with respect to pz.

      do 201 i = 1, 150
         no_pi_plus_pz (i) = no_pi_plus_pz (i)/ bin_width_pz
         no_pi_minus_pz (i) = no_pi_minus_pz (i)/ bin_width_pz
         no_pi_naught_pz (i) = no_pi_naught_pz (i)/ bin_width_pz
         no_ka_plus_pz (i) = no_ka_plus_pz (i)/ bin_width_pz
         no_ka_minus_pz (i) = no_ka_minus_pz (i)/ bin_width_pz
         no_ka_naught_pz (i) = no_ka_naught_pz (i)/ bin_width_pz
C     no_protons_pz (i) = no_protons_pz (i)/ bin_width_pz
C     no_neutrons_pz (i) = no_neutrons_pz (i)/ bin_width_pz
         no_gammas_pz (i) = no_gammas_pz (i)/ bin_width_pz
         no_electrons_pz (i) = no_electrons_pz (i)/ bin_width_pz
         no_positrons_pz (i) = no_positrons_pz (i)/ bin_width_pz
 201  continue     
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_plus_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_plus_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_minus_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_minus_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_naught_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_naught_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_plus_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_plus_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_minus_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_minus_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_naught_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_naught_pz
      close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dproton_dpz', 
C     $     status='new')
C     write (lun_hydrogen,*) no_protons_pz
C     close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dneutron_dpz', 
C     $     status='new')
C     write (lun_hydrogen,*) no_neutrons_pz
C     close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dgamma_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_gammas_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_delectron_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_electrons_pz
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpositron_dpz', 
     $     status='new')
      write (lun_hydrogen,*) no_positrons_pz
      close (unit=lun_hydrogen, status='keep')

C     Write out distributions with repect to azimuthal angle.

      do 202 i = 1, 36
         no_pi_plus_phi (i) = no_pi_plus_phi (i)/ 1.0e+01
         no_pi_minus_phi (i) = no_pi_minus_phi (i)/ 1.0e+01
         no_pi_naught_phi (i) = no_pi_naught_phi (i)/ 1.0e+01
         no_ka_plus_phi (i) = no_ka_plus_phi (i)/ 1.0e+01
         no_ka_minus_phi (i) = no_ka_minus_phi (i)/ 1.0e+01
         no_ka_naught_phi (i) = no_ka_naught_phi (i)/ 1.0e+01
C     no_protons_phi (i) = no_protons_phi (i)/ 1.0e+01
C     no_neutrons_phi (i) = no_neutrons_phi (i)/ 1.0e+01
         no_gammas_phi (i) = no_gammas_phi (i)/ 1.0e+01
         no_electrons_phi (i) = no_electrons_phi (i)/ 1.0e+01
         no_positrons_phi (i) = no_positrons_phi (i)/ 1.0e+01
 202  continue      
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_plus_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_plus_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_minus_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_minus_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_naught_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_naught_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_plus_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_plus_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_minus_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_minus_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_naught_dphi',
     $     status='new')
      write (lun_hydrogen,*) no_ka_naught_phi
      close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dproton_dphi', 
C     $     status='new')
C     write (lun_hydrogen,*) no_protons_phi
C     close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dneutron_dphi', 
C     $     status='new')
C     write (lun_hydrogen,*) no_neutrons_phi
C     close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dgamma_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_gammas_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_delectron_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_electrons_phi
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpositron_dphi', 
     $     status='new')
      write (lun_hydrogen,*) no_positrons_phi
      close (unit=lun_hydrogen, status='keep')

C     Write out distributions with respect to zenith angle. 

      do 203 i = 1, 36
         no_pi_plus_theta (i) = no_pi_plus_theta (i)/ 5.0e0
         no_pi_minus_theta (i) = no_pi_minus_theta (i)/ 5.0e0
         no_pi_naught_theta (i) = no_pi_naught_theta (i)/ 5.0e0
         no_ka_plus_theta (i) = no_ka_plus_theta (i)/ 5.0e0
         no_ka_minus_theta (i) = no_ka_minus_theta (i)/ 5.0e0
         no_ka_naught_theta (i) = no_ka_naught_theta (i)/ 5.0e0
C     no_protons_theta (i) = no_protons_theta (i)/ 5.0e0
C     no_neutrons_theta (i) = no_neutrons_theta (i)/ 5.0e0
         no_gammas_theta (i) = no_gammas_theta (i)/ 5.0e0
         no_electrons_theta (i) = no_electrons_theta (i)/ 5.0e0
         no_positrons_theta (i) = no_positrons_theta (i)/ 5.0e0
 203  continue       
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_plus_dtheta',
     $     status='new')
      write (lun_hydrogen,*) no_pi_plus_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_minus_dtheta', 
     $     status='new')
      write (lun_hydrogen,*) no_pi_minus_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_naught_dtheta',
     $     status='new')
      write (lun_hydrogen,*) no_pi_naught_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_plus_dtheta', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_plus_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_minus_dtheta', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_minus_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_naught_dtheta', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_naught_theta
      close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dproton_dtheta', 
C     $     status='new')
C     write (lun_hydrogen,*) no_protons_theta
C     close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dneutron_dtheta', 
C     $     status='new')
C     write (lun_hydrogen,*) no_neutrons_theta
C     close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dgamma_dtheta', 
     $     status='new')
      write (lun_hydrogen,*) no_gammas_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_delectron_dtheta',
     $     status='new')
      write (lun_hydrogen,*) no_electrons_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpositron_dtheta',
     $     status='new')
      write (lun_hydrogen,*) no_positrons_theta

C     Write out distributions with respect to cosine theta.

      do 204 i = 1, no_cos_theta_bins
         no_pi_plus_cos_theta (i) = no_pi_plus_cos_theta (i)/ 5.0e-02
         no_pi_minus_cos_theta (i) = no_pi_minus_cos_theta (i)/ 5.0e-02
         no_pi_naught_cos_theta (i) = no_pi_naught_cos_theta (i)
     $        / 5.0e-02
         no_ka_plus_cos_theta (i) = no_ka_plus_cos_theta (i)/ 5.0e-02
         no_ka_minus_cos_theta (i) = no_ka_minus_cos_theta (i)/ 5.0e-02
         no_ka_naught_cos_theta (i) = no_ka_naught_cos_theta (i)
     $        / 5.0e-02
C     no_protons_cos_theta (i) = no_protons_cos_theta (i)/ 5.0e-02
C     no_neutrons_cos_theta (i) = no_neutrons_cos_theta (i)/ 5.0e-02
         no_gammas_cos_theta (i) = no_gammas_cos_theta (i)/ 5.0e-02
         no_electrons_cos_theta (i) = no_electrons_cos_theta (i)
     $        / 5.0e-02
         no_positrons_cos_theta (i) = no_positrons_cos_theta (i)
     $        / 5.0e-02
 204  continue     
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_plus_dcos_theta',
     $     status='new')
      write (lun_hydrogen,*) no_pi_plus_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_minus_dcos_theta',
     $     status='new')
      write (lun_hydrogen,*) no_pi_minus_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpi_naught_dcos_theta' 
     $     ,status='new')
      write (lun_hydrogen,*) no_pi_naught_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_plus_dcos_theta', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_plus_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_minus_dcos_theta', 
     $     status='new')
      write (lun_hydrogen,*) no_ka_minus_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dka_naught_dcos_theta' 
     $     ,status='new')
      write (lun_hydrogen,*) no_ka_naught_cos_theta
      close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dproton_dcos_theta', 
C     $     status='new')
C     write (lun_hydrogen,*) no_protons_cos_theta
C     close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/ordinate_dneutron_dcos_theta', 
C     $     status='new')
C     write (lun_hydrogen,*) no_neutrons_cos_theta
C     close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dgamma_dcos_theta', 
     $     status='new')
      write (lun_hydrogen,*) no_gammas_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_delectron_dcos_theta', 
     $     status='new')
      write (lun_hydrogen,*) no_electrons_cos_theta
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/ordinate_dpositron_dcos_theta', 
     $     status='new')
      write (lun_hydrogen,*) no_positrons_cos_theta
      close (unit=lun_hydrogen, status='keep')

C     Now write out the numbers distributions

      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_positive_pions',
     $     status='new')
      write (lun_hydrogen,*) no_pi_plus
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_negative_pions',
     $     status='new')
      write (lun_hydrogen,*) no_pi_minus
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_neutral_pions',
     $     status='new')
      write (lun_hydrogen,*) no_pi_naught
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_positive_kaons',
     $     status='new')
      write (lun_hydrogen,*) no_kaon_plus
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_negative_kaons',
     $     status='new')
      write (lun_hydrogen,*) no_kaon_minus
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_neutral_kaons',
     $     status='new')
      write (lun_hydrogen,*) no_kaon_0
      close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/number_protons',
C     $     status='new')
C     write (lun_hydrogen,*) no_protons
C     close (unit=lun_hydrogen, status='keep')
C     open (unit=lun_hydrogen,
C     $     file='/disk1/kirk/OUTPUT/number_neutrons',
C     $     status='new')
C     write (lun_hydrogen,*) no_neutrons
C     close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_gammas',
     $     status='new')
      write (lun_hydrogen,*) no_gamma
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_electrons',
     $     status='new')
      write (lun_hydrogen,*) no_electron
      close (unit=lun_hydrogen, status='keep')
      open (unit=lun_hydrogen,
     $     file='/disk1/kirk/OUTPUT/number_positrons',
     $     status='new')
      write (lun_hydrogen,*) no_positron
      close (unit=lun_hydrogen, status='keep')
      close (unit=lun_output, status='keep')
      return
 80   format (' ',' ', 'geant number = ', i5, 2x, 'number observations =
     $      ',i5)
 600  format (' ', 'Execution ceased because no file was opened.') 
 601  format (' ', 'Execution ceased because no region of the beamline 
     $     was activated.')
 602  format (' ', 'Execution ceased because helium file was opened, tr
     $     ue distributions were specified, but neither region of cold 
     $     bore was enabled.')
 603  format (' ', 'Execution ceased because nitrogen file was opened, t
     $     rue distributions were specified, but neither region of warm 
     $     bore was enabled.')
 604  format (' ', 'Execution ceased because the program could not ident
     $     ify the target.')
 605  format (' ', 'Execution ceased because of problem in determining t
     $     he z coordinate of the interaction.')
 606  format (' ','Number of interactions with hydrogen = ', I6,/,
     $     ' ', 'Number of interactions with helium = ', I6,/,
     $     ' ', 'Number of interactions with nitrogen = ', I6//)
 607  format ('0','hydrogen file = ', 1l4, 2x, 'helium file = ', 1l4,
     $     2x, 'nitrogen file = ', 1l4, 2x, 'true_distributions = ', 1l4
     $     //)
 608  format ('0', 'enable regions = ', 4l4)
 609  format ('0',T35,'***** ABSOLUTE PROBABILITIES *****'/
     $     ' ', 'hydrogen = ', 1e12.5, 2x, 'helium = ', 1e12.5, 2x,
     $     'nitrogen = ', 1e12.5)
 610  format ('0', T35,'***** RELATIVE PROBABILITIES *****'/
     $     ' ', 'hydrogen = ', 1f7.5, 2x, 'helium = ', 1f7.5, 2x, 
     $     'nitrogen = ', 1f7.5)
 611  format ('0', 'total thickness of hydrogen = ', 1e12.5, 2x, 
     $     'per cm**2'/' ', 'total thickness of helium = ', 1e12.5, 2x,
     $     'per cm**2'/' ', 'total thickness of nitrogen = ', 1e12.5, 2x
     $     ,'per cm**2')
 612  format ('0', 'total length of hydrogen = ', 1e12.5, 2x, 
     $     'cm'/' ', 'total length of helium = ', 1e12.5, 2x,
     $     'cm'/' ', 'total length of nitrogen = ', 1e12.5, 2x,
     $     'cm')
 1001 format(I7, a9, I4, a40)
 1002 format(17x, i3, 2x, 1g12.5, 1x, 1g12.5, 1x, 1g12.5)
 1003 format (' ', 'Event number ', 2x, i5, 2x, 'containing ', 2x, i5,
     $     2x, ' trajectories was readin from the hijet file.')
 1101 format(a)
 1102 format(' ', a)
      end
