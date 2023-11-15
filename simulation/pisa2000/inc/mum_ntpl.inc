*-- Author :    Charles F. Maguire   01/10/92
C
C     Special include file for the MUM user routine
C
c       ntuple 1000 for the muon detector tracking
        integer           np1000
        parameter       ( np1000 = 15 )
        character*8     ch1000( np1000 )
     &        / 'EVENT_NO',     'TRACK_NO',     'PLANE   ',
     &          'pid     ',     'tof     ',     'dele    ',
     &          'X       ',     'Y       ',     'Z       ',
     &          'PX      ',     'PY      ',     'PZ      ',
     &          'PVX     ',     'PVY     ',     'PVZ     '/
        real              evt1000( np1000 )
        integer           np1010
        parameter       ( np1010 = 13 )
        character*8     ch1010( np1010 )
     1        / 'EVENT_NO',     'TRACK_NO',     'PARTICLE',
     1          'VERTEX_X',     'VERTEX_Y',     'VERTEX_Z',
     1          'PX      ',     'PY      ',     'PZ      ',
     1          'MASS    ',     'CHARGE  ',     'PARENT  ',
     &          'PAR MASS' /
        real              evt1010( np1010 )
C
C  -----End of MUM NTUPLE INCLUDE-----
