*CMZ :  2.04/00 05/10/92  11.19.35  by  Charles F. Maguire
*-- Author :
*-- Author :
      subroutine g_ini_paw
      implicit none
c
c PAW initialization for interactive GEANT
c CFM: Originally a GSI routine
c
      call g_ini_zeb     ! zebra init
      call g_def_flag       ! Declare/init flags
c
c Declare commands (KUIP)
c
      call u_kdef_util    ! utility commands
      call u_kdef_dst       ! DST commands
c
      call e_kdef
      call e_kdef_user    ! user commands
c
      return
      end
