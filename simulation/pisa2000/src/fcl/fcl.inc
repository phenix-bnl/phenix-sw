      INTEGER N_SUBVOLUMES
      PARAMETER (N_SUBVOLUMES=90)
      CHARACTER*4 NAME_SUBVOLUMES(N_SUBVOLUMES)
     &     /'FC01', 'FC02', 'FC03', 'FC04', 'FC05',
     &     'FC06', 'FC07', 'FC08', 'FC09', 'FC10',
     &     'FC11', 'FC12', 'FC13', 'FC14', 'FC15',
     &     'FC16', 'FC17', 'FC18', 'FC19', 'FC20',
     &     'FC21', 'FC22', 'FC23', 'FC24', 'FC25',
     &     'FC26', 'FC27', 'FC28', 'FC29', 'FC30',
     &     'FC31', 'FC32', 'FC33', 'FC34', 'FC35',
     &     'FC36', 'FC37', 'FC38', 'FC39', 'FC40',
     &     'FC41', 'FC42', 'FC43', 'FC44', 'FC45',
     &     'FC46', 'FC47', 'FC48', 'FC49', 'FC50',
     &     'FC51', 'FC52', 'FC53', 'FC54', 'FC55',
     &     'FC56', 'FC57', 'FC58', 'FC59', 'FC60',
     &     'FC61', 'FC62', 'FC63', 'FC64', 'FC65',
     &     'FC66', 'FC67', 'FC68', 'FC69', 'FC70',
     &     'FC71', 'FC72', 'FC73', 'FC74', 'FC75',
     &     'FC76', 'FC77', 'FC78', 'FC79', 'FC80',
     &     'FC81', 'FC82', 'FC83', 'FC84', 'FC85',
     &     'FC86', 'FC87', 'FC88', 'FC89', 'FC90'/

      real ModuleXYHalfWidth /5.0/
      real ModuleZHalfWidth /58.5/

      integer Sides      /1/
      integer Rows       /10/
      integer Columns    /9/

      real ZFront       /1800/
      real XFront       /20/
      real YFront       /0/
      real ZHalfLength  /100/
      real Angle  /2.3649/
      real ModuleSpacing /10.16/
      real SideWallWidth /.3175/ ! 1/8" Al
      real BottomWallWidth /5.08/ ! 2" Steel
      real TopWallWidth /1.905/ ! 3/4" Al
      real FrontWallWidth /1.27/ ! 1/2" Al

      real fclOut(N_SUBVOLUMES)
