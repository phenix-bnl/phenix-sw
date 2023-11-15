      subroutine au_venus_evt (ievstat)
      implicit none

c     Original author: Paul Kirk (LSU)
c     Creation date: February 12, 1997

c     Revision History:
c        Date      Name            Comments
c     03/23/97     C.F. Maguire    Minor fixes, general I/O correction

c     This is the routine for reading in the file of good pions that 
c     was extracted from the VENUS file provided by Ron Longacre.  

      integer ievstat

c*******************************************************************
c*******************************************************************

c     WARNING: THE SPECIFIC FORMAT OF THE INCLUDE STATEMENT SHOWN BELOW
c     IS SPECIFIC FOR SUN MACHINES. MODIFICATION MAY BE REQUIRED.

#include "guevgen.inc"

c********************************************************************
c********************************************************************

c     Declare integer arrays below

      integer lun_input, lun_output, ievent_last, array_index
      integer itrtyp, lun_control, ipart, igeant, impact_param
      integer ievent, num_pi_plus_event, num_pi_minus_event
      integer num_pi_plus_tot, num_pi_minus_tot

      integer kimmortal, kstrong

c     Additions for GSPART call on IBM

      integer nubuf, nubuf_out
      parameter (nubuf = 1)
      real ubuf(nubuf)

c     Declare real arrays below

      real pion_mass, pion_lifetime, long_time
      real amass, charge, tlife, ptot, theta, px, py, pz

c     Declare logical arrays below.

      logical first_access, immortal_pion, strong_pion

c     Declare character arrays below.

      character*80 input_name, output_name, control_name
      character*20 pi_plus_label, pi_minus_label, chnpar


c     Declare parameters below.

      parameter (lun_input= 1150, lun_output = 1151)
      parameter (pion_mass= 0.1395700e0, lun_control = 1152)
      parameter (long_time = 1.0e+30, pion_lifetime = 2.603e-08)
      parameter (pi_plus_label = 'pion+', pi_minus_label = 'pion-')


c     Declare data below.

      data first_access /.true./, ievent_last /1/, array_index/0/



c     ************************* REQUIRED INPUT *******************

c     Two files are required for successful operation of this routine.
c     The VENUS file is identified through the specification of the
c     character string "input_name", as shown several lines below. 
c     This character string must be replaced for operation on any
c     machine other than "east" and "west" at LLNL.  The file is read in
c     under the control of format 80.

c     The second of the two required files is a control file that
c     must contain values for the two logical variables "immortal_pion"
c     and "strong_pion".  Set the logical variable "immortal_pion" 
c     equal to .true. in order to set the lifetime of the pion equal to 
c     infinity.  Set the value of "immortal_pion" equal to .false. if 
c     the lifetime of the pion should be left unchanged.  Similarly, in 
c     order to examine the response of the apparatus to non-interacting 
c     pions set the value of "strong_pion" equal to .false.  

c     *********************** END REQUIRED INPUT *********************

c     **************************** OUTPUT ****************************

c     The output produced from within this file will be written to 
c     file "output_name" on logical unit "lun_output".  "Output_name" is
c     specified below and must be changed for operation anywhere other 
c     than on "east" or "west" at LLNL.

c     *************************** END OUTPUT *************************

c     Some tasks need be done only once.  Do them upon first access.

      if (first_access) then
         first_access = .false.

c     All interactions within this program originate at the origin of 
c     coordinate system.  Establish the vertex upon first access.

         multi_xyz = 0
         xyz(1) = 0.0e0
         xyz(2) = 0.0e0
         xyz(3) = 0.0e0
         ubuf(1) = 0

c     Specify name of input file and open it.

c        input_name = '/u3/andrew/pisa/bin/good_pions'

         input_name = 'good_pions.dat'
         open (unit=lun_input, file=input_name, status = 'old')

c     Specify the name of the output file and open it.

c        output_name='/u3/andrew/OUTPUT/output_file.text'

         output_name = 'au_venus.out'
         open (unit=lun_output, file=output_name, status='unknown')

c     Specify the name of the control file, open it, and read contents.

c        control_name='/u3/andrew/pisa/bin/control_venus.text'

         control_name = 'control_venus.input' 
         open (unit = lun_control, file=control_name, status='old')
         read (lun_control, 81) kimmortal, kstrong
         if(kimmortal.eq.1)then
            immortal_pion = .true.
         else
            immortal_pion = .false.
         endif
         if(kstrong.eq.1)then
            strong_pion = .true.
         else
            strong_pion = .false.
         endif
         close (unit = lun_control, status = 'keep')
         write (lun_output, 60) immortal_pion, strong_pion

         if (immortal_pion .and. .not. strong_pion) then 

c     Arrive here if user wants non-interacting pions of infinite
c     lifetime.

            itrtyp = 5
            call gspart (8, pi_plus_label, itrtyp, pion_mass, +1.0e0,
     $      long_time, ubuf, nubuf)
            call gspart (9, pi_minus_label, itrtyp, pion_mass, -1.0e0,
     $      long_time, ubuf, nubuf)

         elseif (immortal_pion .and. strong_pion) then

c     Arrive here if user wants strongly interacting pions of 
c     infinite lifetime.

            itrtyp = 4
            call gspart (8, pi_plus_label, itrtyp, pion_mass, +1.0e0,
     $      long_time, ubuf, nubuf)
            call gspart (9, pi_minus_label, itrtyp, pion_mass, -1.0e0,
     $      long_time, ubuf, nubuf)

         elseif (.not. immortal_pion .and. .not. strong_pion) then

c     Arrive here if user wants non-interacting pions of standard 
c     lifetime. 

            itrtyp = 5
            call gspart (8, pi_plus_label, itrtyp, pion_mass, +1.0e0,
     $      pion_lifetime, ubuf, nubuf)
            call gspart (9, pi_minus_label, itrtyp, pion_mass, -1.0e0,
     $      pion_lifetime, ubuf, nubuf)

         endif

c     Note that no call to gspart is necessary if the user wants
c     strongly interacting pions of standard lifetime. 

c     Now print out the numbers that are stored in the data structure
c     JPART.

         ipart = 8
         call gfpart (ipart, chnpar, itrtyp, amass, charge, tlife,
     $   ubuf, nubuf_out)
         write (lun_output, 61)
         write (lun_output, *) ipart, chnpar, itrtyp, amass, charge,
     $   tlife
         ipart = 9
         call gfpart (ipart, chnpar, itrtyp, amass, charge, tlife,
     $   ubuf, nubuf_out)
         write (lun_output, *) ipart, chnpar, itrtyp, amass, charge,
     $   tlife

         num_pi_plus_tot = 0
         num_pi_minus_tot = 0
         chevt_name = 'Au_Venus_Evt'
      endif


c     This ends the code that is executed only once. 


c     Some tasks must be performed upon the first call in each event.
c     The reseting of array_index and mxtot are among these tasks.

      array_index = 0
      mxtot = 0
      num_pi_plus_event = 0
      num_pi_minus_event = 0
      numevt = numevt + 1
 2    continue 
      read (lun_input, 80, end= 17) igeant, impact_param, ievent,  
     $ptot, theta, px, py, pz 

c     Determine whether or not a new event has been encountered.
c     The logic here is a bit contorted.  The file "good_pions" was
c     extracted from the raw VENUS file by eliminating all particles
c     other than pions.  Consequently I do not really know how many
c     pions are in each event, and mxtot is unknown.  We must therefore
c     calculate mxtot as we load arrays.  We test for the occurrence of
c     a new event by comparing the event number with the number of the
c     preceding event. 

      if (ievent .eq. ievent_last) then
         array_index = array_index + 1
         if (array_index .lt. 20000) then
            if (igeant .eq. 8) then
               num_pi_plus_event = num_pi_plus_event + 1
               num_pi_plus_tot = num_pi_plus_tot + 1
            else
               num_pi_minus_event = num_pi_minus_event + 1
               num_pi_minus_tot = num_pi_minus_tot + 1
            endif 

c     MXTOT contains the number of pions in this event. 

            mxtot = mxtot + 1
            pptot(2, array_index) = px
            pptot(3, array_index) = py
            pptot(4, array_index) = pz

c     Note that the particle is ASSUMED to be a pion.  When the program
c     is modified to accomodate kaons, the following line must be 
c     modified.

            pptot(1, array_index) = sqrt (ptot * ptot 
     $      + pion_mass * pion_mass)

c     These particles are their own parents. 

            idtot (array_index) = igeant
         else
            write (lun_output, 62) ievent
            stop
         endif 

c     Arrive here if the array pptot has been loaded using a valid
c     value of array_index.  Read in another pion. 

         go to 2
      else 

c     Arrive here if a new event has been encountered.
c     Print out a message to the user announcing successful loading of
c     preceding event. 

         write (lun_output,63 ) ievent_last, num_pi_plus_event, 
     $   num_pi_minus_event, num_pi_plus_tot, num_pi_minus_tot
         ievent_last = ievent

c     Note that the first trajectory in the new event will be lost.

         ievstat = -1
         return
      endif
 17   continue 


c     The logic here is pretty slick.  In the absence of some sort of
c     trick, the data in event number 200 would be lost by setting 
c     ievstat = 2.  Therefore I have created a fictitious event, 
c     number 201, that contains only two trajectories, and appended this
c     event to the end of the input file.  Now when the end of file is
c     encountered, no real data are lost, only the phony data in 
c     event 201. 

c     Arrive here when end of file has been encountered.  Set ievstat=2
c     in order to signal the end of file.  

      ievstat = 2
      write (lun_output, 64)
      close (unit=lun_input,status = 'keep')
      close (unit=lun_output,status = 'keep')
      stop
 60   format (' ', 'immortal_pion = ', 1l4, 3x, 'strong_pion = ', 1l4)
 61   format (' ', t25,'FOLLOWING OUTPUT FROM GFPART')
 62   format (' ', 'TOO MANY PIONS IN EVENT ', I5, 
     $' OF GOOD PION FILE')
 63   format (' ', 'event number ', i5, ' from good_pion file was succes
     $sfully loaded.'/' ', 'event contained ', i8, ' pi pluses and ', 
     $i8,' pi minuses, bringing total to ', i8, ' and ', i8, ' ,respecti
     $vely.'///)
 64   format (' ', 'End of file encountered. That is all for now, Paul')
 80   format ( i4, 2x, 1f10.5, 2x, i3, 2x, 1e14.7, 2x, 1f10.5/
     $3(1e14.7, 2x))
 81   format (2I3)
      end
