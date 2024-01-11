#ifndef MVDGEOPARAMETER
#define MVDGEOPARAMETER

/** simple c++ version of STAF table dMvdGeo.
  */
class MvdGeoParameter {
   public :
  MvdGeoParameter() {}
  virtual ~MvdGeoParameter(){}
      /** initialize using hardwired values in case STAF table dMvdGeo
        * is not found or is empty
        */
      void Init();  
      void Show();
   
   public :
      /// Tilt angle of plena [degrees]
      float ang_ver_plena;       
      /// Thickness of "skin" layers [inside/outside] 
      float dr_ver_enc1;		
      /// Thickness of "core" layer of enclosure shell
      float dr_ver_enc2;	
      /// Thickness of walls in strut tube
      float dr_ver_strut;
      /// Box with X, Y, Z
      float dvisl[3];
      /// Box with X, Y, Z
      float dvoel[3];
      /// Box with X, Y, Z
      float dvosl[3];		
      /// Trapezoid with DX1, DX2, DY, DZ
      float dvroh[4];		
      /// Box with X, Y, Z
      float dvwr1[3];	
      /// Box with X, Y, Z
      float dvwr2[3];		
      /// Half-length of cylindrical enclosure shell
      float dz_ver_enc; 
      /// Half-thickness of walls on enclosure's endplate.
      float dz_ver_endpl;
      /// Half-thickness of barrel mounting plate
      float dz_ver_mopl;	
      /// Half-thickness of MVD pad motherboards
      float dz_ver_pmb;
      /// For VISL_2 VOSL_2
      float nph_segs;		
      /// Read-out interval
      float pitch;		
      /// Inner radius of cylindrical enclosure shell
      float r1_ver_enc;	
      /// Inner radius of pad silicon
      float r1_ver_pad;
      /// Outer radius of cylindrical enclosure shell
      float r2_ver_enc;	
      /// Outer radius of pad silicon
      float r2_ver_pad;	
      /// Outer radius of mounting plate
      float r_ver_mopl;	
      /// Distance from beamline to plenum's closest edge
      float r_ver_plena;
      /// Other radius of MVD pad motherboards
      float r_ver_pmb;		
      /// Inner radius of enclosure shell strut
      float r_ver_strut;
      /// For VISL_1 VOSL_1
      float sv1ph;		
      /// Spacing between MCM's
      float ver_el_space;	
      /// Thickness of plenum walls [full thickness]
      float ver_plnm_thwall;
      /// Thickness of bus cable [full, not half thicknes]
      float ver_th_bus_cable;
      /// Thickness of cables from Si to MCM [cm]
      float ver_th_cabl;		
      /// Width of bus cable [full, not half width]
      float ver_w_bus_cable;
      /// Width of bus cable [full, not half width]
      float ver_z_center;
      /// Radius for VISL
      float vislr;		
      /// Radius for VOSL
      float voslr;		
      /// Width of the ribs
      float wdvroh;	
      /// Z position of barrel mounting plate
      float z_ver_mopl;	
      /// Z position of pad detector
      float z_ver_pad;	
      /// Z of MVD pad motherboards
      float z_ver_pmb;	
      /// "Bus" cables -- 2-Feb-1996
      short med_ver_bus_cable;	
      /// Si to MCM cables -- 2-Feb-1996
      short med_ver_cabl;	
      /// Enclosure skin [Al]
      short med_ver_enc1;		
      /// Enclosure core [rohacell]
      short med_ver_enc2;	
      /// Enclosure endplate [Al]
      short med_ver_endpl;	
      /// Helium as default filling material
      short med_ver_he;		
      /// Silicon insensitive
      short med_ver_ins;		
      /// Mcm material -- 2-Feb-1996
      short med_ver_mcm;		
      /// Medium for barrel mounting plate
      short med_ver_mopl;	
      /// Mvd pad motherboard [G10]
      short med_ver_pmb;
      /// Rohacell support structure
      short med_ver_roh;	
      /// Silicon sensitive
      short med_ver_sen;		
      /// Enclusure strut [carbon]
      short med_ver_strut;	
      /// Number of holes on the side
      short nhvroh;		
      /// Number of strips in the wafer
      short nstrip;		
      /// Number of wafers missing in "hole"
      short nwhole;
      /// Number of wafers for perpendicular strips
      short nwperp;			
};

#endif
