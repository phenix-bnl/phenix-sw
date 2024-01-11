// $$Id: TFvtxMILLEPEDE.h,v 1.8 2015/09/09 01:50:17 jinhuang Exp $$

/*!
 \file  TFvtxMILLEPEDE.h
 \brief   OOP warpper for MILLEPEDE and a generalized least squares fit for residual evaluation. Many code moved from \ref FvtxGlobalAlign by Z.Y. You
 \author  Jin Huang
 \version $Revision: 1.8 $
 \date  $Date: 2015/09/09 01:50:17 $
 */

#ifndef TFVTXMILLEPEDE_H_
#define TFVTXMILLEPEDE_H_

#ifndef __CINT__
#include <boost/array.hpp>
#endif

#include <MUTOO.h>
#include <MUIOO.h>
#include <string>
#include <cstdarg>
#include <cstdio>
#include <map>

#include <TObject.h>

#include <MUTOO.h>
#include <FVTXOO.h>
#include <FVTXGEOM.h>

#include <MILLEPEDE.h>

class TTree;
class TFvtxMPTrack;
class TForwardMPTrack;
class TFvtxMPNode;

/*!
 \class   TFvtxMILLEPEDE
 \brief   OOP warpper for MILLEPEDE and a generalized least squares fit for residual evaluation.
 */
class TFvtxMILLEPEDE
{

  //////////////////////////////////////////////////////////////
  //!@name constructor, destructor and service functions
  //@{
public:

  TFvtxMILLEPEDE();
  virtual
  ~TFvtxMILLEPEDE();

  //! return module name
  virtual const char *
  Name() const
  {
    return "TFvtxMILLEPEDE";
  }

  //! return formated string
  static std::string
  format(const char* format, ...);

  //@}

  //!@name output tree marameters
  //@{
  enum
  {
    BUFFER_SIZE = 32000
  };
  enum
  {
    AUTO_SAVE = 16000
  };
  //@}

  //////////////////////////////////////////////////////////////
  //!@name OOP rapper of millepede operations
  //@{
protected:

  //! initialize minimization parameters
  bool
  init_parameters(bool dumpMille);

  //! initialize minimization
  bool
  init_minimize();

  //! end minimization
  void
  end_minimize();

  //! feed TFvtxMPTrack to millipede
  void
  MILLEPEDE_Fit(TFvtxMPTrack * track, int verbosity = 0);

  //! feed TForwardMPTrack to millipede
  void
  MILLEPEDE_Fit(TForwardMPTrack * track, int verbosity = 0);

  //@}

  //////////////////////////////////////////////////////////////
  //!@name flags
  //@{

public:

  enum Flag
  {

    //! azimtuhal modulation constraint for wedges
    USE_AZIMUTHAL_MOD_CONSTRAINTS = 1 << 2,

    ALIGN_FVTX_STATION = 1 << 3,
    ALIGN_FVTX_WEDGE = 1 << 4,
    ALIGN_FVTX = ALIGN_FVTX_STATION + ALIGN_FVTX_WEDGE,
    ALIGN_W = 1 << 5,
    ALIGN_Z = 1 << 6,
    ALIGN_PHI = 1 << 7,
    ALIGN_PSIX = 1 << 8,
    ALIGN_PSIY = 1 << 9,

    //! constraint wedges
    USE_CONSTRAINTS = 1 << 10,

    ITERATE = 1 << 11,

    //! fit track only in the W-Z space
    TRACK_1D_Fit = 1 << 12,

    //! whether to constrain the location of each FVTX cage
    USE_CONSTRAINTS_CAGE_POS = 1 << 13,

    //! use a single z shift for entire cage
    USE_CONSTRAINTS_CAGE_Z = 1 << 1,

    NONE = 0
  };

  enum Flag_MuArm
  {
    USE_THETA_CUT_MU_ARM = 1 << 2,
    USE_THETA_CUT_CLOSE_TRACK_MU_ARM = 1 << 3,
    ALIGN_MUID = 1 << 4,
    ALIGN_MUTR = 1 << 5,
    ALIGN_MU_ARM = ALIGN_MUID + ALIGN_MUTR,
    ALIGN_W_MU_ARM = 1 << 6,
    ALIGN_Z_MU_ARM = 1 << 7,
    ALIGN_PHI_MU_ARM = 1 << 8,
    USE_CONSTRAINTS_MU_ARM = 1 << 9,
    USE_CONSTRAINTS_MU_ARM_OneStation = 1 << 10,
    USE_CONSTRAINTS_MU_ARM_WithInStation = 1 << 11,
    USE_CONSTRAINTS_MU_ARM_LastStation = 1 << 12,
    USE_CONSTRAINTS_MUID_WithInPanel = 1 << 13,

    //! do not constraint global position for all MuTr stations
    USE_CONSTRAINTS_MUTR_ZeroStation = 1 << 14,
    //! do not constraint global position for all MuTr stations
    USE_CONSTRAINTS_MUID_ZeroStation = 1 << 15,

    NONE_MUTR = 0
  };

public:
  virtual void
  set_flags(const unsigned long int& value)
  {
    _flags = value;
  }

  virtual void
  set_flag(const Flag& flag, const bool& value)
  {
    if (value)
      {
        _flags |= flag;
      }
    else
      {
        _flags &= (~flag);
      }
  }

  virtual bool
  get_flag(const Flag& flag) const
  {
    return _flags & flag;
  }

  virtual void
  set_flags_mu_arm(const unsigned long int& value)
  {
    _flags_mu_arm = value;
  }

  virtual void
  set_flag(const Flag_MuArm& flag, const bool& value)
  {
    if (value)
      {
        _flags_mu_arm |= flag;
      }
    else
      {
        _flags_mu_arm &= (~flag);
      }
  }

  virtual bool
  get_flag(const Flag_MuArm& flag) const
  {
    return _flags_mu_arm & flag;
  }

//  //! fvtx station alignment flag
//  void
//  set_align_fvtx_station(bool value)
//  {
//    set_flag(ALIGN_FVTX_STATION, value);
//  }
//
//  //! fvtx wedge alignment flag
//  void
//  set_align_fvtx_wedge(bool value)
//  {
//    set_flag(ALIGN_FVTX_WEDGE, value);
//  }
//
//  //! w alignment flag
//  void
//  set_align_w(bool value)
//  {
//    set_flag(ALIGN_W, value);
//  }
//
//  //! z alignment flag
//  void
//  set_align_z(bool value)
//  {
//    set_flag(ALIGN_Z, value);
//  }
//
//  //! phi alignment flag
//  void
//  set_align_phi(bool value)
//  {
//    set_flag(ALIGN_PHI, value);
//  }
//
//  //! psix alignment flag
//  void
//  set_align_psix(bool value)
//  {
//    set_flag(ALIGN_PSIX, value);
//  }
//
//  //! psiy alignment flag
//  void
//  set_align_psiy(bool value)
//  {
//    set_flag(ALIGN_PSIY, value);
//  }
//
//  //! constraint flag
//  void
//  set_constraint(bool value)
//  {
//    set_flag(USE_CONSTRAINTS, value);
//  }
//
//  //! iterations
//  void
//  set_iterate(const bool& value)
//  {
//    set_flag(ITERATE, value);
//  }

protected:
  //! configuration flags
  int _flags;

  //! configuration flags
  int _flags_mu_arm;

  //@}

  //////////////////////////////////////////////////////////////
  //!@name export misalignment to TTree and text files
  //@{
public:

  //! output text filename of misalignment
  void
  set_output_filename(const char* file)
  {
    if (file)
      _output_misalignment = file;
  }

  //! dump the fixed parameters to ostream out
  void
  print_fixed_parameters(std::ostream &out = std::cout) const;

  // ! output file for the alignment parameter output
  void
  export_fvtx_misalignment_to_tree(std::ostream &out_cp = std::cout);

  //! dump the results of the minimisation to ostream out
  void
  export_mutr_parameters_to_tree(std::ostream &out = std::cout);

  //! dump the results of the minimisation to ostream out
  void
  export_muid_parameters_to_tree(std::ostream &out = std::cout);

  //! make a backup copy of an existing file
  static std::string
  make_backup(const std::string& filename);

  //! dump all alignment parameters to a given file
  bool
  print_to_file(const char* filename);

protected:

  void
  reset_misalignment_variables(void);

  void
  build_misalignment_tree(void);

  // ! output file for the alignment parameter output
  void
  export_misalignment_to_text();

private:

  //! Set an output text file for the alignment parameter output
  const char* _output_misalignment;

  //! millepede output tree
  TTree* _alignment_tree;

  //! track arm
  int _arm;

  //! track octant
  int _octant;

  //! track cage
  int _cage;

  //! track station
  int _station;

  //! track sector
  int _sector;

  //! gap location
  int _gap;

  //! half octant location
  int _half;

  //! cathode location
  int _cathode;

  //! strip location
  int _strip;

  //! detector name following MILLEPEDE::enu_detector_id
  int _detector_id;

  //! plane location
  int _plane;

  //! panel location
  int _panel;

  //! orientation location
  int _orientation;

  //! detector index
  int _detector_index;

  //! number of tracks in the detector
  int _nb_tracks;

  //! strip angle
  double _angle;

  //! millepede x correction
  double _delta_x_millepede;

  //! millepede y correction
  double _delta_y_millepede;

  // desalignment tree parameters
  //! millepede z correction
  double _delta_z_millepede;

  //! millepede w correction
  double _delta_w_millepede;

  //! millepede phi correction
  double _delta_phi_millepede;

  //! millepede psix correction
  double _delta_psix_millepede;

  //! millepede psiy correction
  double _delta_psiy_millepede;

  //! error on millepede x correction
  double _error_x;

  //! error on millepede y correction
  double _error_y;

  //! error on millepede z correction
  double _error_z;

  //! error on millepede w correction
  double _error_w;

  //! error on millepede phi correction
  double _error_phi;

  //! error on millepede psix correction
  double _error_psix;

  //! error on millepede psiy correction
  double _error_psiy;

  //! input x misalignment
  double _delta_x;

  //! input y misalignment
  double _delta_y;

  //! input z misalignment
  double _delta_z;

  //! input w misalignment
  double _delta_w;

  //! input phi misalignment
  double _delta_phi;

  //! input psix misalignment
  double _delta_psix;

  //! input psiy misalignment
  double _delta_psiy;

  //@}

  //////////////////////////////////////////////////////////////
  //!@name FVTX/MuTr Service function
  //@{

public:

  //! get half angle
  static double
  get_half_angle(int arm, int cage, int station, int sector, int half,
      int strip);

  //! get sign of w_absolute relative to the r direction
  static int
  get_w_sign(int arm, int cage, int station, int sector, int half);

  //! check MuTr w sign definitions within half octant
  static bool
  check_w_sign_mutr_halfocts();

  //! get sign correction for w direction of MuTr
  static int
  get_w_sign_cor_mutr(unsigned short arm, unsigned short station,
      unsigned short octant, unsigned short half_octant, unsigned short gap,
      unsigned short cathode, unsigned short strip);

  //! get corrected half angle which consistent with that for strip0
  static double
  get_half_angle_mutr(unsigned short arm, unsigned short station,
      unsigned short octant, unsigned short half_octant, unsigned short gap,
      unsigned short cathode, unsigned short strip);

  //@}

  //////////////////////////////////////////////////////////////
  //!@name indexing parameters for millepede 1-D parameter array
  //@{

public:
  //! calculate detector unique index for a given arm, cage, station tuple
  int
  get_index_station(int arm, int cage, int station) const;

  //! calculate detector unique index for a given arm, cage, station, sector, half column tuple
  int
  get_index_half(int arm, int cage, int station, int sector, int half) const;

  enum enu_subindex_station
  {
    IDX_STA_X = 0, // x
    IDX_STA_Y = 5, // y
    IDX_STA_Z = 1, // z
    IDX_STA_PHI = 2, // PHI
    IDX_STA_PSIX = 3, // PSIX
    IDX_STA_PSIY = 4 // PSIY
  };

  enum enu_subindex_half
  {

    IDX_HALF_W = 0, // w
    IDX_HALF_Z = 1, // z
    IDX_HALF_PHI = 2 // phi
  };

  //! total number of fvtx stations  (16)
  static const int _nb_fvtx_station = FVTXOO::MAX_ARM * FVTXOO::MAX_CAGE
      * FVTXOO::MAX_STATION;

  //! total number of fvtx wedges (in fact half_wedge as floating unit, 768)
  static const int _nb_fvtx_wedge = FVTXOO::MAX_ARM * FVTXOO::MAX_CAGE
      * FVTXOO::MAX_STATION * FVTXOO::MAX_SECTOR * FVTXOO::MAX_COLUMN;

  //! total number of mutr detectors
  static const int _nb_mutr_det = MUTOO::NumberOfArms * MUTOO::NumberOfStations
      * MUTOO::NumberOfOctants * MUTOO::NumberOfHalfOctants
      * MUTOO::NumberOfGaps * MUTOO::NumberOfCathodePlanes;

  //! total number of muid detectors
  static const int _nb_muid_det = MUIOO::MAX_ARM * MUIOO::MAX_PLANE
      * MUIOO::MAX_PANEL * MUIOO::MAX_ORIENTATION;

  //! offset to add to muid unique ids to avoid overlap with mutr
  static const int _muid_offset = MUTOO::NumberOfArms * MUTOO::NumberOfStations
      * MUTOO::NumberOfOctants * MUTOO::NumberOfHalfOctants
      * MUTOO::NumberOfGaps * MUTOO::NumberOfCathodePlanes;

  //! total number of detectors (16*2+768 = 800)
  int
  get_nb_det_fvtx() const
  {
    // one (wedge) det has 3(NPARPLAN) parameters, station * 2 because it has 6 parameters
    //  static const int _nb_det = _nb_fvtx_station * 2 + _nb_fvtx_wedge;
    return get_flag(ALIGN_FVTX) ? (_nb_fvtx_station * 2 + _nb_fvtx_wedge) : 0;
  }

  int
  get_nb_det_mu_arm() const
  {
    int _nb_det = 0;

    if (get_flag(ALIGN_MUTR))
      _nb_det += _nb_mutr_det;
    if (get_flag(ALIGN_MUID))
      _nb_det += _nb_muid_det;

    return _nb_det;
  }

  int
  get_nb_det() const
  {
    return get_nb_det_fvtx() + get_nb_det_mu_arm();
  }

  //! check if several detectors have same index [debug]
  void
  check_detector_index(void) const;

  int
  get_n_track_para_fvtx() const
  {
    return
        get_flag(ALIGN_FVTX) ?
            (get_flag(TRACK_1D_Fit) ? 2 : MILLEPEDE::NPARTRK) : 0;
  }

  int
  get_n_track_para_mu_arm() const
  {
    return
        get_flag(ALIGN_MU_ARM) ?
            MILLEPEDE::NPARTRK * 2/*Single track + matching*/: 0;
  }

  int
  get_n_track_para() const
  {
//    return
//        (get_n_track_para_fvtx() < get_n_track_para_mu_arm()) ?
//            get_n_track_para_mu_arm() : get_n_track_para_fvtx();

    return _n_track_para;
  }

  void
  set_n_track_para(int p)
  {
    _n_track_para = p;
  }

  int
  get_index_offset_fvtx() const
  {
    return 0;
  }

  int
  get_index_offset_mutr() const
  {
    return get_nb_det_fvtx() ;
  }

  int
  get_index_offset_muid() const
  {
    return get_nb_det_fvtx() + _muid_offset;
  }

  //@}

  //////////////////////////////////////////////////////////////
  //!@name fixing and constraining, common features
  //@{

  //! bit values for the alignment parameters
  enum ParameterBit
  {
    PAR_W = 1 << 0,
    PAR_Z = 1 << 1,
    PAR_PHI = 1 << 2,
    PAR_PSIX = 1 << 3,
    PAR_PSIY = 1 << 4,
    ALL = PAR_Z | PAR_W | PAR_PHI | PAR_PSIX | PAR_PSIY
  };

  //! pass registered fixed detectors to millepede
  void
  register_fixed_detectors(void);
  //@}

  //////////////////////////////////////////////////////////////
  //!@name fixing and constraining a substructure of FVTX
  //@{

public:
  //! fix all fvtx stations
  void
  fix_fvtx_all_stations(void);

  //! fix fvtx stations
  void
  fix_fvtx_stations(void);

  //! constrain fvtx stations
  void
  constrain_fvtx_stations(void);

  //! constrain fvtx Z to be the same with in one cage
  void
  constrain_fvtx_cage_z(void);

  //! constrain fvtx Z do not have a shear distortion with in one cage
  void
  constrain_fvtx_cage_z_shear(void);

  //! fix all fvtx wedges
  void
  fix_fvtx_all_wedges(const ParameterBit fix_flag = ALL);

  //! fix fvtx 2 station wedges
  void
  fix_fvtx_2station_wedges(void);

  //! fix fvtx station
  void
  fix_fvtx_station(int arm, int cage, int station, unsigned int flags);

  //! fix fvtx wedge
  void
  fix_fvtx_wedge(int arm, int cage, int station, int sector,
      unsigned int flags);

protected:

  //! fix parameters for fvtx for a given detector arm, cage, station
  void
  fix_parameter_fvtx_station(int arm, int cage, int station, int parameter_bit);

  //! fix parameters for fvtx for a given detector arm, cage,  station, sector
  void
  fix_parameter_fvtx_wedge(int arm, int cage, int station, int sector,
      int parameter_bit);

  // constraint the 2 columns (half wedges) of each sector to move at the same time
  bool
  constraint_halfs(int arm, int cage, int station, int sector);

  // constraint the 2 columns (half wedges) of each sector to move at the same time
  bool
  constraint_azimuthal_modulation(int arm, int station);

#ifndef __CINT__

protected:

  //! fvtx station id
  class FvtxStationId
  {
  public:

    //! constructor
    FvtxStationId(int arm = 0, int cage = 0, int station = 0) :
        _arm(arm), _cage(cage), _station(station)
    {
    }

    //! equal to operator
    bool
    operator ==(const FvtxStationId& id) const
    {
      return _arm == id._arm && _cage == id._cage && _station == id._station;
    }

    //! less than operator
    bool
    operator <(const FvtxStationId& id) const
    {
      if (_arm != id._arm)
        return _arm < id._arm;
      else if (_cage != id._cage)
        return _cage < id._cage;
      else if (_station != id._station)
        return _station < id._station;
      else
        return false;
    }

    int _arm;
    int _cage;
    int _station;

    //! map fvtx station id to fixed alignment flags
    typedef std::map<FvtxStationId, unsigned int> Map;

  };

  //! fvtx wedge id
  class FvtxWedgeId
  {
  public:

    //! constructor
    FvtxWedgeId(int arm = 0, int cage = 0, int station = 0, int sector = 0) :
        _arm(arm), _cage(cage), _station(station), _sector(sector)
    {
    }

    //! equal to operator
    bool
    operator ==(const FvtxWedgeId& id) const
    {
      return _arm == id._arm && _cage == id._cage && _station == id._station
          && _sector == id._sector;
    }

    //! less than operator
    bool
    operator <(const FvtxWedgeId& id) const
    {
      if (_arm != id._arm)
        return _arm < id._arm;
      else if (_cage != id._cage)
        return _cage < id._cage;
      else if (_station != id._station)
        return _station < id._station;
      else if (_sector != id._sector)
        return _sector < id._sector;
      else
        return false;
    }

    int _arm;
    int _cage;
    int _station;
    int _sector;

    //! map fvtx wedge id to fixed alignment flags
    typedef std::map<FvtxWedgeId, unsigned int> Map;

  };

  //! map fvtx station detector id to fixed parameter flags
  FvtxStationId::Map _fixed_fvtx_stations;

  //! map fvtx wedge detector id to fixed parameter flags
  FvtxWedgeId::Map _fixed_fvtx_wedges;

#endif

  //@}

  //////////////////////////////////////////////////////////////
  //!@name fixing and constraining a substructure of Muon Arm
  //@{

#ifndef __CINT__

  //! mutr detector id
  class MutrDetId
  {
  public:

    //! constructor
    MutrDetId(int arm = 0, int station = 0, int gap = 0, int cathode = 0) :
        _arm(arm), _station(station), _gap(gap), _cathode(cathode)
    {
    }

    //! equal to operator
    bool
    operator ==(const MutrDetId& id) const
    {
      return _arm == id._arm && _station == id._station && _gap == id._gap
          && _cathode == id._cathode;
    }

    //! less than operator
    bool
    operator <(const MutrDetId& id) const
    {
      if (_arm != id._arm)
        return _arm < id._arm;
      else if (_station != id._station)
        return _station < id._station;
      else if (_gap != id._gap)
        return _gap < id._gap;
      else if (_cathode != id._cathode)
        return _cathode < id._cathode;
      else
        return false;
    }

    int _arm;
    int _station;
    int _gap;
    int _cathode;

    //! map mutr detector id to fixed alignment flags
    typedef std::map<MutrDetId, unsigned int> Map;

  };

  //! muid detector id
  class MuidDetId
  {
  public:

    //! constructor
    MuidDetId(int arm = 0, int plane = 0, int orientation = 0) :
        _arm(arm), _plane(plane), _orientation(orientation)
    {
    }

    //! equal to operator
    bool
    operator ==(const MuidDetId& id) const
    {
      return _arm == id._arm && _plane == id._plane
          && _orientation == id._orientation;
    }

    //! less than operator
    bool
    operator <(const MuidDetId& id) const
    {
      if (_arm != id._arm)
        return _arm < id._arm;
      else if (_plane != id._plane)
        return _plane < id._plane;
      else if (_orientation != id._orientation)
        return _orientation < id._orientation;
      else
        return false;
    }

    int _arm;
    int _plane;
    int _orientation;

    //! map muid detector id to fixed parameter flags
    typedef std::map<MuidDetId, unsigned int> Map;

  };

//! map mutr detector id to fixed parameter flags
  MutrDetId::Map _fixed_mutr_detectors;

//! map muid detector id to fixed parameter flags
  MuidDetId::Map _fixed_muid_detectors;

#endif

public:
  //! fix mutr cathode
  void
  fix_mutr_cathode(int arm, int station, int gap, int cathode,
      unsigned int flags);

  //! is this a fixed mutr cathode?
  bool
  is_fixed_mutr_cathode(int arm, int station, int gap, int cathode,
      unsigned int flags);

  //! fix mutr gap
  void
  fix_mutr_gap(int arm, int station, int gap, unsigned int flags)
  {
    fix_mutr_cathode(arm, station, gap, 0, flags);
    fix_mutr_cathode(arm, station, gap, 1, flags);
  }

  //! fix muid planes
  void
  fix_muid_plane(int arm, int plane, int orientation, unsigned int flags);

  //! is this a fixed muid plane?
  bool
  is_fixed_muid_plane(int arm, int plane, int orientation, unsigned int flags) const;

  //! fix muid plane
  void
  fix_muid_plane(int arm, int plane, unsigned int flags)
  {
    fix_muid_plane(arm, plane, 0, flags);
    fix_muid_plane(arm, plane, 1, flags);
  }

  //! fix all mutr cathodes
  void
  fix_mutr_all(void);

  //! fix default mutr gaps (station0 gap 1 and station1 gap0)
  void
  fix_mutr_2gaps(void);

  //! fix mutr station0 and station1 entirely
  void
  fix_mutr_2stations(void);

  //! fix default muid planes (plane 0 and plane 1)
  void
  fix_muid_2planes(void);

  //! calculate detector unique index for a given arm,station,gap and cathode tuple
  int
  get_index_cathode(int arm, int station, int gap, int cathode) const;

  //! calculate detector unique index for a given arm,station,octant, half octant, gap, cathode tuple
  int
  get_index_half_octant(int arm, int station, int octant, int half_octant,
      int gap, int cathode) const;

  /*! \brief
   calculate detector unique index for a given arm,plane,panel,orientation tuple
   adds an offset so that it does not overlap with mutr unique ids
   */
  int
  get_index_panel(int arm, int plane, int panel, int orientation) const;

  //! offset is added to avoid overlaping of MuID over MuTr
  int
  get_index_orientation(int arm, int plane, int orientation) const;

#ifndef __CINT__

  //! calculate detector unique index for a given cathode locator
  int
  get_index_half_octant(const MUTOO::cathode_locator& location) const
  {
    return

    get_index_half_octant(location.get<0>(), location.get<1>(),
        location.get<2>(), location.get<3>(), location.get<4>(),
        location.get<5>());
  }

  //! calculate detector unique index for a given cathode locator
  int
  get_index_cathode(const MUTOO::cathode_locator& location) const
  {
    return get_index_cathode(location.get<0>(), location.get<1>(),
        location.get<4>(), location.get<5>());
  }

  /*! \brief
   calculate detector unique index for a given muid orientation locator
   adds an offset so that it does not overlap with mutr unique ids
   */
  int
  get_index_panel(const MUIOO::panel_orient_locator& location) const
  {
    return get_index_panel(location.get<0>(), location.get<1>(),
        location.get<2>(), location.get<3>());
  }

  //! offset is added to avoid overlaping of MuID over MuTr
  int
  get_index_orientation(const MUIOO::panel_orient_locator& location) const
  {
    return get_index_orientation(location.get<0>(), location.get<1>(),
        location.get<3>());
  }

#endif

  //! fix parameters for MuTr for a given detector arm station octant cathode
  void
  fix_parameter_mutr(int arm, int station, int gap, int cathode,
      int parameter_bit);

  //! fix parameters for MuTr for a given detector arm station octant cathode
  void
  fix_parameter_mutr(int arm, int station, int octant, int half_octant, int gap,
      int cathode, int parameter_bit);

  //! fix parameters for MuID for a given detector arm plane orientation
  void
  fix_parameter_muid(int arm, int plane, int orientation, int parameter_bit);

  //! constraint the 2 half MuTr octants of each octant to rotate at the same time
  bool
  constraint_mutr_half_octant_rotation(int arm, int station, int gap, int cath, int octant);

  //! constraint the 2 half MuTr octants of each octant to move at the same time
  bool
  constraint_mutr_half_octant_translation(int arm, int station, int gap, int cath, int octant);

  //! constraint rotation with in a octant on one station
  bool
  constraint_mutr_station(int arm, int station);

  //! constraint quardrants in MuTr station 0
  bool
  constraint_quards_station0(int arm, int gap, int cath);

  bool
  constraint_muid_orientations(int arm, int plane, int panel) const;
  //@}

  //////////////////////////////////////////////////////////////
  //!@name MILLEPEDE interface parameters
  //@{

#ifndef __CINT__

protected:

  //! vector of global derivatives
  boost::array<float, MILLEPEDE::NGLB> _dergb;

  static const int MAX_NPARTRK = MILLEPEDE::NPARTRK * 2;

  //! vector of local derivatives
  boost::array<float, MAX_NPARTRK> _derlc;

  //! vector of parameters
  boost::array<float, MILLEPEDE::NGLB> _par;

  //! number of tracks used for each detector
  boost::array<int, MILLEPEDE::NPLAN> _n_tracks;

  //! vector of parameters
  boost::array<int, MILLEPEDE::NGLB> _n_filled_par;
#endif

  //@}

  //////////////////////////////////////////////////////////////
  //!@name low level ops to millepede
  //@{
public:
  //! scratch filename needed for iteration
  void
  set_scratch_filename(const char* filename)
  {
    _scratch_filename = filename;
  }

  void
  set_n_std_dev(const int c)
  {
    _n_std_dev = c;
  }

  int
  get_n_std_dev()
  {
    return _n_std_dev;
  }

  void
  set_sigma_rescaling(double c)
  {
    _sigma_rescaling = c;
  }

  double
  get_sigma_rescaling()
  {
    return _sigma_rescaling;
  }

protected:

  //! scratch filename needed for iterations
  const char* _scratch_filename;

  //! controls the track quality cut during matrix inversion iterations
  int _n_std_dev;

  int _n_track_para;

  //! true when InitParameters was called successfuly
  bool _par_init;

  double _sigma_rescaling;

  //@}

  //////////////////////////////////////////////////////////////
  //!@name wedge status of whether be included in optimization
  //@{

public:
  // utilities to exclude a wedge if needed

  typedef enum
  {
    DET_ENABLE = 0, // include a wedge in the analysis
    DET_EXCLUDE // exclude a wedge from the analysis
  } enu_detector_status;

  void
  set_wedge_status(int arm, int cage, int station, int sector,
      enu_detector_status status);

  enu_detector_status
  get_wedge_status(int arm, int cage, int station, int sector, int half) const;

  // for sector = 0 - 47 = cage * 2 + sector
  void
  set_wedge_status(int arm, int station, int sector, enu_detector_status status)
  {
    const int cage = sector / FVTXGEOM::NumberOfSectors;
    const int sec = sector % FVTXGEOM::NumberOfSectors;
    set_wedge_status(arm, cage, station, sec, status);
  }

protected:

#ifndef __CINT__

  boost::array<enu_detector_status, MILLEPEDE::NPLAN> _detector_status;

#endif

  //@}
};

#endif /* TFVTXMILLEPEDE_H_ */
