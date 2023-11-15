*-- Author :
C
C    FGLINK.INC for the GEANT ZEBRA banks
C
C
      common   /fglink/
     1  lfg_link,       ! structural links
     1  lfg_kine,
     1  lfg_vertx,
     1  lfg_part,
     1  lfg_spare,
c
     1  lfg_lref,       ! reference links
     1  lfg_last
C
      integer*4   lfg_link,lfg_lref,lfg_last,
     1   lfg_kine,      ! GEANT kine banks
     1   lfg_vertx,     ! GEANT vertex banks
     1   lfg_part,      ! GEANT particle banks
     1   lfg_spare(10)     ! GEANT particle banks
c
