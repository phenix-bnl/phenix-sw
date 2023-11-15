c $Id: mupplug.f,v 1.3 2008/05/21 08:22:00 hpereira Exp $
*-- Author :    Surender Saini   12/04/93

      subroutine mupplug

c    **************************************************************
c    *                                                            *
c    *  MUPPLUG (vsn 1.00) muon_arm piston_plug geometry          *
c    *                                                            *
c    *  Called by ==> ::  < MUM >                                 *
c    *  IN   :: none                                              *
c    *  OUT  :: none                                              *
c    *                                                            *
c    *  written  by ::  Surender Saini, 12/04/93 02.08.20         *
c    *  modified by ::  - JPSullivan Oct 5, 1993                  *
c    *                    Some output to LOUT from GCUNIT         *
c    *                  - MLBrooks, 30-Jan-97                     *
c    *                    Allow plugs in two arms to have         *
c    *                    different geometry                      *
c    *                  - Reinhard Stotzer 23-Jan-1998            *
c    *                    Added copper mounting plates and a      *
c    *                    lead wrap for the piston plug           *
c    **************************************************************

c Author :   09-NOV-1992 / S. Saini


c New Piston_plug

c ----------------------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEEP,GUGEOM.
#include "gugeom.inc"
*KEND.
c ----------------------

      real*4 pai,cdtr,crtd
      common/uconst/pai,cdtr,crtd


* Parameters and variables used to define the piston plugs:

      integer   nplug_max     ! maximum z-planes of piston plug
      parameter (nplug_max=5)

      integer iplugflg   ! flag to turn the plug on (=1) or off (=0)
      integer num_plug   ! number of plugs (=1: plug in the north arm
*                          only; =2: plugs in the north and south arm
      integer npl_plug   ! number of z-planes used to define the shape
*                          of the plug (has to be less than nplug_max)
      integer nmed_plug  ! tracking medium of the plug
      integer color_plug ! color of the plug in GEANT
*     The first value or set of values of the following four variables
*     refer to the north arm and the second set to the south arm:
      real z_plug(nplug_max,2)    ! z position (cm) of planes used to
*                                   define the piston shape
      real rmin_plug(nplug_max,2) ! inner radius (cm) at position z_plug
      real rmax_plug(nplug_max,2) ! outer radius (cm) at position z_plug
      real plug_thet(2)           ! outer angle (theta in degrees) of
*                                   piston (used only if the first
*                                   rmax_plug element is negative)

      real pcon_par(3*nplug_max+3)


* Parameters and variables used to define the copper mounting plates
* for the piston plugs (a GEANT polycone shape is used even though
* a cone would have been sufficient; this was done so that possible
* design changes could be incorporated more easily):

      integer   nmount_max     ! maximum z-planes of mounting plates
      parameter (nmount_max=2)

      integer imountflg   ! flag to turn the mounts on (=1) or off (=0)
      integer num_mount   ! number of mounts (=1: mount in the north arm
*                           only; =2: mounts in the north and south arm
      integer npl_mount   ! number of z-planes used to define the shape
*                           of the mount (has to be less than nmount_max)
      integer nmed_mount  ! tracking medium of the mounts
      integer color_mount ! color of the mounts in GEANT
*     The first value or set of values of the following four variables
*     refer to the north arm and the second set to the south arm:
      real z_mount(nmount_max,2)    ! z position (cm) of planes used to
*                                     define the mounting plates
      real rmin_mount(nmount_max,2) ! inner radius (cm) at pos. z_mount
      real rmax_mount(nmount_max,2) ! outer radius (cm) at pos. z_mount

      real pcon2_par(3*nmount_max+3)

* Parameters and variables used to define the lead wrap and extension
* of the piston plugs (a GEANT polycone shape):

      integer   iwrap_max     ! maximum z-planes of lead wrap
      parameter (iwrap_max=7)

      integer iwrapflg   ! flag to turn the wrap on (=1) or off (=0)
      integer num_wrap   ! number of wraps (=1: wrap in the north arm
*                          only; =2: wraps in the north and south arm
      integer npl_wrap   ! number of z-planes used to define the shape
*                          of the wrap (has to be less than iwrap_max)
      integer nmed_wrap  ! tracking medium of the wrap
      integer color_wrap ! color of the wrap in GEANT
*     The first value or set of values of the following four variables
*     refer to the north arm and the second set to the south arm:
      real z_wrap(iwrap_max,2)    ! z position (cm) of planes used to
*                                   define the wrap shape
      real rmin_wrap(iwrap_max,2) ! inner radius (cm) at pos. z_wrap
      real rmax_wrap(iwrap_max,2) ! outer radius (cm) at pos. z_wrap
      real wrap_thetmin(2)        ! inner angle (theta in degrees)
      real wrap_thetmax(2)        ! outer angle (theta in degrees)
      real wrap_dr(2)             ! radial thickness of the lead wrap
      real wrap_dz(2)             ! length of wrap in z past the copper
*                                   piston plug

      real pcon3_par(3*iwrap_max+3)


* Other parameters and variables:

      namelist /plug_par/ iplugflg, num_plug, npl_plug, nmed_plug,
     &                    color_plug, z_plug, rmin_plug, rmax_plug,
     &                    plug_thet,
     &                    imountflg, num_mount, npl_mount, nmed_mount,
     &                    color_mount, z_mount, rmin_mount, rmax_mount,
     &                    iwrapflg, nmed_wrap, color_wrap,
     &                    wrap_thetmin, wrap_thetmax, wrap_dr, wrap_dz

      character*4 v_m_name ! name of the mother volume
      character*4 v_i_name ! name of the daughter volume

      integer  npar, nmed, ivolu, izpl
      integer  iplug, imount, iwrap  ! dummy count variables
      real     rmax_1

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c ------------------------------------------------------------

c Read the geometery file segment


      write( *,* ) 'mupplug - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = plug_par, err = 999 )


* Loop for defining and installing the piston plug:

      if(iplugflg .ne. 0)then

        if(npl_plug .gt. nplug_max)then
         write (6,10)npl_plug,nplug_max
   10    format(/'PLUG<E>: The requested number of z-planes ',i4,2x,
     &   'exceeds the maximum ',i4,2x,'set',/,11x,'in the MUPPLUG',
     &   'routine.',/)
         stop ' PISA program stopped --> error detected in MUPPLUG'
        endif
        if(npl_plug .lt. 2)then
          write (6,15)npl_plug
   15    format(/'PLUG<E>: The requested number of z-planes ',i4,2x,
     &   'is less than the minimum (=2)',/,11x,
     &   'required for a PCON in GEANT.',/,11x,
     &   'Check the NPL_PLUG value in the geometry.'/)
         stop ' PISA program stopped --> error detected in MUPPLUG'
        endif

        do iplug = 1, num_plug
          do izpl = 1,npl_plug-1
           if(z_plug(izpl+1,iplug) .lt. z_plug(izpl,iplug))then
             write (6,20)izpl,z_plug(izpl,iplug),izpl+1,
     &        z_plug(izpl+1,iplug)
   20        format(/,'PLUG<E>: The zplane #',i3,' value ',g10.3,2x,
     &       'is greater than or equal to',/,11x,
     &       'the zplane #',i3,' value ',g10.3,/,11x,
     &       'Check the geometry.'//)
             stop ' PISA program stopped-->error detected in MUPPLUG'
           endif
          enddo
        end do

        do iplug = 1, num_plug
          rmax_1 = rmax_plug(1,iplug)
          do izpl=1,npl_plug
            if(rmax_1 .lt. 0.0)then
             rmax_plug(izpl,iplug) = z_plug(izpl,iplug)*
     +       tan(plug_thet(iplug)*cdtr)
            end if
            if(rmax_plug(izpl,iplug) .lt. rmin_plug(izpl,iplug))then
              write (6,25)izpl,rmax_plug(izpl,iplug),izpl,
     &         rmin_plug(izpl,iplug)
   25          format(/,'PLUG<E>: The rmax #',i3,' value ',g10.3,2x,
     &        'is less than',/,11x,'the rmin #',i3,' value ',g10.3/,11x,
     &        'Check the geometry.'//)
              stop ' PISA program stopped->error detected in MUPPLUG'
            endif
          enddo
        end do

c Reduce outer radius of the pistion plug if lead wrap is requested:
        if (iwrapflg .ne. 0) then
          do iplug = 1, num_plug
            do izpl=1,npl_plug
              rmax_plug(izpl,iplug)= rmax_plug(izpl,iplug)-
     &                               wrap_dr(iplug)- 0.01
            enddo
          end do
        endif

c now put in geometry into proper mother volume

        do  iplug = 1, num_plug

          v_m_name = 'HALL'    ! use HALL as immediate mother
          nmed = nmed_plug     ! should be Cu
          npar = 3*npl_plug+3

c now fill up the invariant polycone parameter array members

          pcon_par(1) = 0.00      ! lower phi limit
          pcon_par(2) = 360.0     ! upper phi limit
          pcon_par(3) = npl_plug     ! number of z-planes

c now fill up the polycone parameter array

          IF (IPLUG .EQ. 1) THEN
            v_i_name = 'MUPP'
          ELSE
            v_i_name = 'MUPQ'
          END IF

          do izpl=1,npl_plug
           pcon_par(3*izpl+1) = z_plug(izpl,iplug) ! z-plane position (positive)
           pcon_par(3*izpl+2) = rmin_plug(izpl,iplug) ! rmin at this z
           pcon_par(3*izpl+3) = rmax_plug(izpl,iplug) ! rmax at this z
          enddo
          call gsvolu(v_i_name,'PCON',nmed,pcon_par,npar,ivolu)
          if(color_plug.gt.0)then
            CALL GSATT(v_i_name,'SEEN',1)
            CALL GSATT(v_i_name,'COLO',color_plug)
          else

c invisible

            CALL GSATT(v_i_name,'SEEN',0)
          endif

c position it in the mother volume

          if(iplug.eq.1) then
            call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')
c position second arm plug is requested
          else if(iplug.eq.2) then
            irot=irot+1
            call gsrotm(irot,90.,0.,90.,90.,180.,0.)
            call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,irot,'ONLY')
          endif


          write(LOUT,'(/5x,''Plug placed at Z='',f10.4,'' cms'')')
     +     z_plug(1,iplug)
          write(LOUT,'(5x,''Plug angle = '',f10.4)')plug_thet
          write(LOUT,'(''<UGEOM> : Piston_plug installed '')')
          call prmater(nmed)

        end do    ! loop over num_plug
      end if      ! test on iplugflg


* Loop for defining and installing the mounting plates:

      if(imountflg .ne. 0)then

        if(npl_mount .gt. nmount_max)then
         write (6,110)npl_mount,nmount_max
  110    format(/'PLUG<E>: The requested number of z-planes ',i4,2x,
     &   'exceeds the maximum ',i4,'set',/,11x,'in the MUPPLUG',
     &   'routine.',/)
         stop ' PISA program stopped --> error detected in MUPPLUG'
        endif
        if(npl_mount .lt. 2)then
          write (6,115)npl_mount
  115    format(/'PLUG<E>: The requested number of z-planes ',i4,2x,
     &   'is less than the minimum (=2)',/,11x,
     &   'required for a PCON in GEANT.',/,11x,
     &   'Check the NPL_MOUNT value in the geometry.'/)
         stop ' PISA program stopped --> error detected in MUPPLUG'
        endif

        do imount = 1, num_mount
          do izpl = 1,npl_mount-1
           if(z_mount(izpl+1,imount) .le. z_mount(izpl,imount))then
             write (6,120)izpl,z_mount(izpl,imount),izpl+1,
     &        z_mount(izpl+1,imount)
  120        format(/,2x,'PLUG<E>: The zplane #',i3,' value ',g10.3,2x,
     &       'is greater than or equal to',/,11x,
     &       'the zplane #',i3,' value ',g10.3,/,11x,
     &       'Check the geometry.'//)
             stop ' PISA program stopped-->error detected in MUPPLUG'
           endif
          enddo
        end do

        do imount = 1, num_mount
          do izpl=1,npl_mount
            if(rmax_mount(izpl,imount) .lt. rmin_mount(izpl,imount))then
              write (6,125)izpl,rmax_mount(izpl,imount),izpl,
     &         rmin_mount(izpl,imount)
  125          format(/,2x,'PLUG<E>: The rmax #',i3,' value ',g10.3,2x,
     &        'is less than',/,11x,'the rmin #',i3,' value ',g10.3/,11x,
     &        'Check the geometry.'//)
              stop ' PISA program stopped->error detected in MUPPLUG'
            endif
          enddo
        end do

c now put in geometry into proper mother volume

        do  imount = 1, num_mount

          v_m_name = 'HALL'    ! use HALL as immediate mother
          nmed = nmed_mount    ! should be Cu
          npar = 3*npl_mount+3

c now fill up the invariant polycone parameter array members

          pcon2_par(1) = 0.00       ! lower phi limit
          pcon2_par(2) = 360.0      ! upper phi limit
          pcon2_par(3) = npl_mount  ! number of z-planes

c now fill up the polycone parameter array

          IF (IMOUNT .EQ. 1) THEN
            v_i_name = 'MUMN'  ! mounting plate in the north arm
          ELSE
            v_i_name = 'MUMS'  ! mounting plate in the south arm
          END IF

          do izpl=1,npl_mount
           pcon2_par(3*izpl+1) = z_mount(izpl,imount) ! z-plane pos. (positive)
           pcon2_par(3*izpl+2) = rmin_mount(izpl,imount) ! rmin at this z
           pcon2_par(3*izpl+3) = rmax_mount(izpl,imount) ! rmax at this z
          enddo
          call gsvolu(v_i_name,'PCON',nmed,pcon2_par,npar,ivolu)
          if(color_mount.gt.0)then
            CALL GSATT(v_i_name,'SEEN',1)
            CALL GSATT(v_i_name,'COLO',color_mount)
          else

c invisible

            CALL GSATT(v_i_name,'SEEN',0)
          endif

c position mount in the mother volume:
          if(imount.eq.1) then
            call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')
c position the south arm mount if requested:
          else if(imount.eq.2) then
            irot=irot+1
            call gsrotm(irot,90.,0.,90.,90.,180.,0.)
            call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,irot,'ONLY')
          endif


          write(LOUT,'(/5x,''Plug mount placed at Z='',f10.4,'' cm'')')
     +     z_mount(1,imount)
          write(LOUT,'(2x,''<UGEOM> : Piston-plug mount installed '')')
          call prmater(nmed)

        end do    ! loop over num_mount
      end if      ! test on imountflg



* Loop for defining and installing the lead wrap and extension:

      if (iwrapflg.ne.0) then

        num_wrap = num_plug ! if a wrap is requested it will be
*                             installed in each piston plug that was
*                             requested (north/south)
        npl_wrap = npl_plug + 2

        if(npl_wrap .gt. iwrap_max)then
         write (6,210)npl_wrap,iwrap_max
  210    format(/2x,'PLUG<E>: The requested number of z-planes ',i4,2x,
     &   'exceeds the maximum ',i4,2x,'set',/,11x,'in the MUPPLUG',
     &   'routine.',/)
         stop ' PISA program stopped --> error detected in MUPPLUG'
        endif
        if(npl_wrap .lt. 2)then
          write (6,215)npl_wrap
  215    format(/2x,'PLUG<E>: The requested number of z-planes ',i4,2x,
     &   'is less than the minimum (=2)',/,11x,
     &   'required for a PCON in GEANT.',/,11x,
     &   'Check the NPL_WRAP value in the geometry.'/)
         stop ' PISA program stopped --> error detected in MUPPLUG'
        endif

* Calculate z, rmin and rmax for the wrap:
        do iwrap = 1, num_wrap ! in the code num_wrap=num_plug
          do izpl=1,(npl_plug-1) ! use npl_plug here, not npl_wrap
            z_wrap(izpl,iwrap)= z_plug(izpl,iwrap)
            rmin_wrap(izpl,iwrap)= rmax_plug(izpl,iwrap) + 0.01
            rmax_wrap(izpl,iwrap)= rmin_wrap(izpl,iwrap) +
     &                             wrap_dr(iwrap)
          enddo
          z_wrap(npl_plug,iwrap)= z_plug(npl_plug,iwrap) + 0.01
          rmax_wrap(npl_plug,iwrap)= z_wrap(npl_plug,iwrap) *
     &                               tan(wrap_thetmax(iwrap)*cdtr)
          rmin_wrap(npl_plug,iwrap)= rmax_wrap(npl_plug,iwrap) -
     &                               wrap_dr(iwrap)
          do izpl=(npl_plug+1),npl_wrap
            if (izpl.eq.npl_plug+1) then
              z_wrap(izpl,iwrap)= z_wrap(npl_plug,iwrap) + 0.01
            else
              z_wrap(izpl,iwrap)= z_plug(npl_plug,iwrap) +
     &                            wrap_dz(iwrap)
            endif
            rmin_wrap(izpl,iwrap)= z_wrap(izpl,iwrap) *
     &                             tan(wrap_thetmin(iwrap)*cdtr)
            rmax_wrap(izpl,iwrap)= z_wrap(izpl,iwrap) *
     &                              tan(wrap_thetmax(iwrap)*cdtr)
          enddo
        end do


        do iwrap = 1, num_wrap
          do izpl = 1,npl_wrap-1
           if(z_wrap(izpl+1,iwrap) .le. z_wrap(izpl,iwrap))then
             write (6,220)izpl,z_wrap(izpl,iwrap),izpl+1,
     &        z_wrap(izpl+1,iwrap)
  220        format(/,'mupplug - The zplane #',i3,' value ',g10.3,2x,
     &       'is greater than or equal to',/,11x,
     &       'the zplane #',i3,' value ',g10.3,/,11x,
     &       'Check the geometry.'//)
             stop ' PISA program stopped-->error detected in MUPPLUG'
           endif
          enddo
        end do

        do iwrap = 1, num_wrap
          do izpl=1,npl_wrap
            if(rmax_wrap(izpl,iwrap) .lt. rmin_wrap(izpl,iwrap))then
              write (6,225)izpl,rmax_wrap(izpl,iwrap),izpl,
     &         rmin_wrap(izpl,iwrap)
  225          format(/,'mupplug - The rmax #',i3,' value ',g10.3,2x,
     &        'is less than',/,11x,'the rmin #',i3,' value ',g10.3/,11x,
     &        'Check geometry file.'//)
              stop ' PISA program stopped->error detected in MUPPLUG'
            endif
          enddo
        end do


c now put in geometry into proper mother volume

        do  iwrap = 1, num_wrap

          v_m_name = 'HALL'    ! use HALL as immediate mother
          nmed = nmed_wrap    ! should be lead
          npar = 3*npl_wrap+3

c now fill up the invariant polycone parameter array members

          pcon3_par(1) = 0.00       ! lower phi limit
          pcon3_par(2) = 360.0      ! upper phi limit
          pcon3_par(3) = npl_wrap  ! number of z-planes

c now fill up the polycone parameter array

          IF (IWRAP .EQ. 1) THEN
            v_i_name = 'MUWN'  ! lead wrap in the north arm
          ELSE
            v_i_name = 'MUWS'  ! lead in the south arm
          END IF

          do izpl=1,npl_wrap
           pcon3_par(3*izpl+1) = z_wrap(izpl,iwrap) ! z-plane pos. (positive)
           pcon3_par(3*izpl+2) = rmin_wrap(izpl,iwrap) ! rmin at this z
           pcon3_par(3*izpl+3) = rmax_wrap(izpl,iwrap) ! rmax at this z
          enddo
          call gsvolu(v_i_name,'PCON',nmed,pcon3_par,npar,ivolu)
          if(color_wrap.gt.0)then
            CALL GSATT(v_i_name,'SEEN',1)
            CALL GSATT(v_i_name,'COLO',color_wrap)
          else

c invisible

            CALL GSATT(v_i_name,'SEEN',0)
          endif

c position wrap in the mother volume:
          if(iwrap.eq.1) then
            call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,1,'ONLY')
c position the south arm wrap if requested:
          else if(iwrap.eq.2) then
            irot=irot+1
            call gsrotm(irot,90.,0.,90.,90.,180.,0.)
            call gspos(v_i_name,1,v_m_name,0.0,0.0,0.0,irot,'ONLY')
          endif



          write(LOUT,'(''mupplug - Plug wrap placed at Z='',f10.4)')
     +     z_wrap(1,iwrap)
          write(LOUT,'(''mupplug - Plug-wrap angle = '',f10.4)') wrap_thetmax
          write(LOUT, * ) 'mupplug - Piston-plug wrap installed'
          call prmater(nmed)

        end do    ! loop over num_wrap
      end if      ! test on iwrapflg





      return

c -----------------------
  999 continue
      close(unit=15)
      stop ' mupplug - PISA stop, geometry error'
      end
