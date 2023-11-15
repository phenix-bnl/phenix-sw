C
C --- mun_collar.inc -------------------------------------------------
C
C     Parameters required to create south muon arm "collar" which
C     goes between the backplate of the muon magnet and the first
C     muon identifier plane, encircling the beam pipe.
C     
 
      integer maxseg                      !Parameters for creating volume
      parameter (maxseg=5)

      integer nseg_collar /2/  !Number of z-segments defining volume
      integer nmed_collar /46/ !Medium to use for collar
      integer color_collar /2/ !Collar of collar
      integer color_lead /3/   !Lead skin color
      integer nmed_lead /99/   !Lead with same thresholds as #46 iron
      real z_collar(maxseg)    !z positions where rmin and rmax are defined
      real rmin_collar(maxseg) !rmin of collar at z position
      real rmax_collar(maxseg) !rmax of collar at z position
      real rmax_lead(maxseg)   !rmax of lead skin
     
        namelist /mun_collar_par/
     +    nseg_collar, nmed_collar, color_collar, z_collar, 
     +    rmin_collar, rmax_collar, nmed_lead, rmax_lead, color_lead
 
