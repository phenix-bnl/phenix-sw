c    $Id: init_vertexOffset.f,v 1.3 2008/05/21 08:22:10 hpereira Exp $

c     file: init_vertexOffset.f
c     Original author: Hugo Pereira
c     $Date: 2008/05/21 08:22:10 $
c     $Revision: 1.3 $

      subroutine init_vertexoffset()

      ! event.par variable definition
#include "kincut.inc"

      ! first makes sure initialization is done only once
      logical first /.true./
      save first
      
      ! namelist to read in event.par parameters
      namelist /epar/ y_min, y_max, pt_min, pt_max, the_min, the_max,
     +  phi_min, phi_max, vrms, p_min, p_max, iincl, include,
     +  xyz0_input,north_south, the_min2, the_max2, nskip_evt,
     +  t0cent, t0width, b_min, b_max, ptfactor, pzfactor,
     +  p0topchfactor,  k0tokchfactor,  pitokaonfactor
     

      ! make sure that initialization is done only once  
      if( .not. first ) then
        return
      endif
      
      ! switch flag to tell initialization was done
      first = .false.
      
      ! open event.par file
      open(
     +  unit=17,file='event.par',status='old',
     +  form='formatted',err=793)
      goto 794
      
      ! error opening file  
793   continue
      stop 'init_vertexOffset - unable to open event.par'
            
794   continue
            
      ! parse file using epar namespace to store the variables
      read(17,nml=epar,err=795,end=795)
      close(unit=17)
      go to 796
            
      ! error reading file
795   continue
      stop 'init_vertexOffset - error reading  event.par '
            
796   continue
      
      ! printout vertex parameters
      write( lout, 797 ) xyz0_input, vrms

797   format(
     +    'init_vertexOffset - xyz0_input = ',3f6.2,' cm',/, 
     +    'init_vertexOffset - vrms = ',3f6.2,' cm')
          
      ! initialize vertexOffset class
      call intialize_vertex_offset( xyz0_input, vrms );
       
      return
      end
      
