c $Id: photon_converter.f,v 1.10 2008/05/21 08:22:11 hpereira Exp $
      subroutine photon_converter
      implicit none

c     Original author: Charles F. Maguire (from first version by Takashi Hachiya)
c     Creation date: February 16, 2002

c     T. Hachiya Feb, 20, 2002, Add more converter of overlap region at bottom. 


c     Purpose: Install the photon converter, first used in Run2

c     Method: The geometry parameters for the photon converter are contained
c             in its own conv_par segment in the phnx.par file.  The inner radius
c             of the converter is taken numerically from a sum of MVD parameters.
c             The converter volume is installed in the VERT subvolume if there is an MVD.
c             The converter volume is installed in the HALL main volume if there is no MVD.

c     Called by: The photon_converter subroutine is called by gugeom if there is
c                a CONV option on the GEOP passive volumes control line of pisa.kumac .

c     Calls: usual GEANT volume subroutines

c     Qualificiations: Be careful to match the MVD geometry information


c     F. Kajihara, Jun, 10, 2005, Add Run4 configuration (conv_ver==3) 


#include "guphnx.inc"
#include "gugeom.inc"

c     local variables

      integer nmed
      integer npar
      integer ivolu
      real par(3), par_ovlap(5)

      integer nmat
      integer isvol
      integer ifield
      integer nwbuf /1/

      integer conv_ver /3/ ! converter ID : 1 for run2, 2 for run3 d-Au, 3 for run4 Au-Au

      real fieldm
      real tmaxfd
      real dmaxms
      real deemax
      real epsil
      real stmin
      real ubuf(1)

      real pi_conv /3.14159265358979323846/
      real thickness_conv /0.0254/     ! thickness in cm
      real inner_radius_conv /28.991/  ! from MVD:   R2_VER_ENC + DR_VER_ENC1 + DR_VER_ENC2
      integer med_conv /110/           ! tracking medium is defined in this routine
      real dz_conv /30.48/             ! length in cm
      real bottom_overlap_conv /6.985/ ! overlap length at bottom of photon converter in cm
      real in_rad_ovlap, half_ovlap_the_rad, half_ovlap_the
c run3 converter angular range
c from 270.0 + ( 79.063 / 2.0 ) - 360.0 = -50.4685
c to 270.0 - ( 79.063 / 2.0 ) = 230.4685
c      real conv_range(2) / -50.4685, 230.4685 /
c run4 converter angular range
c from 270.0 + ( 0 / 2.0 ) - 360.0 = -90.00
c to   270.0 - ( 0 / 2.0 ) = 270.0
      real conv_range(2) / -90.00, 270.0 /
    

      namelist /conv_par/inner_radius_conv, thickness_conv, med_conv,
     +                   dz_conv, bottom_overlap_conv,
     +                   conv_ver, conv_range


C     Tracking media # 110 -- Additional material for photon convertion
C     (brass)  by T.Hachiya   brass (Cu(70%)+Zn(29.88)+Fe(0.05%)+Pb(0.07%))

      real anbrass(4) / 63.546, 65.39, 55.845, 207.2 / !Cu+Zn+Fe+Pb
      real znbrass(4) / 29.00,  30.00, 26.00,  82.00 / !Cu+Zn+Fe+Pb
      real wnbrass(4) / 0.7,  0.2988, 0.0005, 0.0007 / !Cu+Zn+Fe+Pb
      real dennbrass / 8.5 /            ! (g/cm3)
 
c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun


c     begin execution

c     Read the geometery file segment


      write( *,* ) 'photon_converter - ',
     +             'reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = conv_par, err = 999 )


C     Tracking media # 110 -- Additional material for photon convertion
C     (brass)  by T.Hachiya   brass (Cu+Zn)

      call gsmixt(110,'Additional Convertor$', anbrass , znbrass,
     +            dennbrass, 4, wnbrass)

      nmat    = 110   ! Additional Converter number
      isvol   = 0     ! Not sensitive
      ifield  = 1     ! Magnetic field
      fieldm  = 5.0   ! max field 5 Tesla
      tmaxfd  = 0.3   ! maximum angle due to field (one step) in degrees
      dmaxms  = 0.5   ! max disp. due to mulsct. in one step (cm)
      deemax  = 0.2   ! max fractional energy loss in one step
      epsil   = 0.01  ! tracking precision (cm)
      stmin  = 0.01  ! min step due to e loss or mulsct. (cm)
      ubuf(1) = 0.    ! tracking stop switch

      call gstmed(110,'Additional Converter$', nmat, isvol, ifield,
     +            fieldm, tmaxfd, dmaxms, deemax, epsil, stmin, 
     +            ubuf, nwbuf)

      if ( 1 .eq. conv_ver ) then ! run2 photon converter

c    calculate the degree of overlap region for additional material

        in_rad_ovlap = inner_radius_conv + thickness_conv     
        half_ovlap_the_rad=0.5*( bottom_overlap_conv / in_rad_ovlap )
        half_ovlap_the = half_ovlap_the_rad*180.0/pi_conv
ccc      write(6,*) 'Theta of Overlap of conv in ', half_ovlap_the


c     Additional conversion material for photon measurement
c     original version by T. Hachiya

        nmed = med_conv
        par(1) = inner_radius_conv
        par(2) = par(1) + thickness_conv
        par(3) = dz_conv
        call gsvolu( 'CONV', 'TUBE', NMED, PAR, 3, IVOLU )
        call gsatt( 'CONV', 'SEEN', 1 )
        call gsatt( 'CONV', 'COLO', 2 )

        par_ovlap(1) = par(2)
        par_ovlap(2) = par_ovlap(1) + thickness_conv
        par_ovlap(3) = dz_conv
        par_ovlap(4) = 270.0 - half_ovlap_the
        par_ovlap(5) = 270.0 + half_ovlap_the
        call gsvolu( 'CNOL', 'TUBS', NMED, PAR_OVLAP, 5, IVOLU )
        call gsatt( 'CNOL', 'SEEN', 1 )
        call gsatt( 'CNOL', 'COLO', 4 )

        if(ivolu_opt(1,1).ne.0)then
           call gspos( 'CONV', 1, 'VERT',0.,0.,0.,IROTNULL,'ONLY')
        else
           call gspos( 'CONV', 1, 'HALL',0.,0.,0.,IROTNULL,'ONLY')
        endif

        call gspos( 'CNOL', 1, 'CONV',0.,0.,0.,IROTNULL,'ONLY')

        write(6,99) inner_radius_conv, bottom_overlap_conv
 99     format(//,'  The photon converter has been installed',/,
     +          7X,'Radius  : ',f7.4, 'cm',/,
     +          7X,'Overlap : ',f7.4, 'cm',//)
      else if ( 2 .eq. conv_ver ) then ! run3 d-Au photon converter
c by Tsugu.TABARU
c Run 3 d-Au converter
c I do not put glue tapes that holds the converter and the protective mylar
c bands.

c Here, I use par_ovlap(5) for the converter used in run3 d-Au run.
c The converter did not have an overlap region, but we need five parameters
c to specify the shape, so I cannot use par(3).
        nmed = med_conv
        par_ovlap(1) = inner_radius_conv
        par_ovlap(2) = par_ovlap(1) + thickness_conv
        par_ovlap(3) = dz_conv
c 15+3/4"=40.005 cm.  1.37991 radian to 28.991 cm radius.  So, 79.063 deg.
        par_ovlap(4) = conv_range(1)
        par_ovlap(5) = conv_range(2)
        call gsvolu( 'CONV', 'TUBS', nmed, par_ovlap, 5, ivolu )
        call gsatt( 'CONV', 'SEEN', 1 )
        call gsatt( 'CONV', 'COLO', 2 )
        if(ivolu_opt(1,1).ne.0)then
           call gspos( 'CONV', 1, 'VERT',0.,0.,0.,IROTNULL,'ONLY')
        else
           call gspos( 'CONV', 1, 'HALL',0.,0.,0.,IROTNULL,'ONLY')
        endif
        write(6,98) par_ovlap(1), par_ovlap(2), par_ovlap(3),
     1              par_ovlap(4), par_ovlap(5)
 98     format(//,'  The photon converter has been installed',/,
     1          7X,'Radius  : ',f7.4,'--',f7.4,'cm',/,
     2          7X,'Length/2 : ',f7.4,'cm',/,
     3          7X,'angle : ',f8.3,'--',f8.3,'deg',//)
c Three mylar bands protecting MVD.
        par(1) = inner_radius_conv + thickness_conv
c The thickness of mylar bands is 10 mil = 0.0254 cm.
        par(2) = par(1) + 0.0254
c The length is 3" = 7.62 cm
        par(3) = 7.62 / 2
c myler is defined in mat_mixt_med.f with the id 820.
        call gsvolu( 'CFML', 'TUBE', 820, par, 3, ivolu )
        call gsatt( 'CFML', 'SEEN', 1 )
        call gsatt( 'CFML', 'COLO', 5 )
        call gspos( 'CFML', 1, 'CONV',0.,0.,0.,IROTNULL,'ONLY')
c The space among mylers are unknown, actually.
c Here, I place them at the place, which I read from the scetch.
        call gspos( 'CFML', 2, 'CONV',0.,0.,
     1              dz_conv / 2.0,IROTNULL,'ONLY')
        call gspos( 'CFML', 3, 'CONV',0.,0.,
     1              -dz_conv / 2.0,IROTNULL,'ONLY')
      else if ( 3 .eq. conv_ver ) then ! run4 Au-Au photon converter
cccccccccccccccccccccccccccccccccccccccccccccccccc
c by F. Kajihara 
c Run4 Au+Au photon-converter simulation
c The converter did not have an overlap region. 
cccccccccccccccccccccccccccccccccccccccccccccccccc
        nmed = med_conv
        par_ovlap(1) = inner_radius_conv
        par_ovlap(2) = par_ovlap(1) + thickness_conv
        par_ovlap(3) = dz_conv
        par_ovlap(4) = -90.00 
        par_ovlap(5) =  270.0 
        call gsvolu( 'CONV', 'TUBS', nmed, par_ovlap, 5, ivolu )
        call gsatt( 'CONV', 'SEEN', 1 )
        call gsatt( 'CONV', 'COLO', 2 )

        if(ivolu_opt(1,1).ne.0)then
           call gspos( 'CONV', 1, 'VERT',0.,0.,0.,IROTNULL,'ONLY')
        else
           call gspos( 'CONV', 1, 'HALL',0.,0.,0.,IROTNULL,'ONLY')
        endif

        call gspos( 'CNOL', 1, 'CONV',0.,0.,0.,IROTNULL,'ONLY')

        write(6,97) par_ovlap(1), par_ovlap(2), par_ovlap(3),
     1              par_ovlap(4), par_ovlap(5)
 97     format(//,'  The photon converter has been installed',/,
     1          7X,'Radius  : ',f7.4,'--',f7.4,'cm',/,
     2          7X,'Length/2 : ',f7.4,'cm',/,
     3          7X,'angle : ',f8.3,'--',f8.3,'deg',//)
      endif ! End of Run4 Au+Au 
      return


c     Error conditions

 999  continue
      stop 'photon_converter - error reading phnx.par file'
      end
