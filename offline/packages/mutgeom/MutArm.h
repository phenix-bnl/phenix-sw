#ifndef MutArm_h
#define MutArm_h

// $Id: MutArm.h,v 1.49 2010/08/25 00:54:01 hpereira Exp $

/*!
  \file MutArm.h
  \brief Describes an Arm of the muon tracker system.
  \author Douglas Fields, Nicki Bruner, Hugo Pereira
  \version $Revision: 1.49 $
  \date $Date: 2010/08/25 00:54:01 $
*/

#include <iosfwd>
#include <stdexcept>
#include <string>
#include <set>
#include <vector>

#include "PHTimeStamp.h"
#include "MutGeomObject.h"
#include "MUTGEOM.h"

// forward declarations ...
class MutPISAPara;
class MutStation;
class MutStrip;
class MutWire;

//! Describes an Arm of the muon tracker system
class MutArm : public MutGeomObject
{

  public:

  //! constructor.
  MutArm(MUTGEOM::ArmNumber ArmNum);

  //! constructor from database.
  MutArm(MUTGEOM::ArmNumber ArmNum, PHTimeStamp &time_stamp);

  //! Real constructor for pisa response/reco.
  MutArm(MUTGEOM::ArmNumber ArmNum, MutPISAPara *PISAPara);

  //! Copy constructor.
  MutArm(const MutArm& rhs)
  { throw std::logic_error( "MutArm::MutArm - use of copy constructor is forbidden" ); }

  //! Destructor.
  virtual ~MutArm();

  //! Arm identifier
  MUTGEOM::ArmNumber getArm() const
  {return _arm;}

  //! the timeStamp used for accessing data in the database.
  const PHTimeStamp& getArmTimeStamp() const
  {return _db_time;}

  //! write the DCM channel for every strip to a file
  void dumpDCMChannels( const char *outFile="a_s_map.dat" );

  //! retrieves noseCone minimum z
  const double& getNoseConeZmin() const
  {return noseConeZmin;}

  //! retrieves nosecone maximum z
  const double& getNoseConeZmax() const
  {return noseConeZmax;}

  //! Refresh a station in the Arm.
  void RefreshStation(const MUTGEOM::StationNumber& StationNum);

  //! Update octant survey data in the DB. data is stored in file in mm and corrected for survey target height.
  bool updateOctantSurvey(
    PHTimeStamp &time_start,
    PHTimeStamp &time_stop,
    PdbBankID bank_id,
    const char *descrip, const char *file);

  //! Store global alignment corrections in the DB.
  bool updateGlobalAligConsts(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file);

  //! Store internal alignment corrections in the DB.
  bool updateInternalAligConsts(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file);

  //! print DCM channel map to file
  void printDCMChannelMap( std::string tag = "" ) const;

  //! update DCM channel map
  /*! the station argument is used to set which station is to be updated. -1 means all stations */
  void updateDCMChannelMap( PHTimeStamp&, PHTimeStamp&, std::string description, int station = -1 );

  /*!
    correct alignment of octants between the stations and the
    stations globally
  */
  void fetchGlobalAligConsts(const char *file);

  //! correct alignment of octants between the stations and the stations globally
  /*! \param output_file if non 0, fetched constants will be written there */
  void fetchGlobalAligConsts(const int& run_number, PdbBankID bank_id, const char* output_file = 0);

  //! correct alignment of octants between the stations and the stations globally
  /*! \param output_file if non 0, fetched constants will be written there */
  void fetchGlobalAligConsts(PHTimeStamp &time_stamp, PdbBankID bank_id, const char* output_file = 0);

  //! correct internal chamber alignment - file read method
  void fetchInternalAligConsts(const char *file);

  //! correct internal chamber alignment - file read method
  /*! \param output_file if non 0, fetched constants will be written there */
  void fetchInternalAligConsts(const int& run_number, PdbBankID bank_id, const char* output_file = 0);

  //! correct internal chamber alignment - file read method
  /*! \param output_file if non 0, fetched constants will be written there */
  void fetchInternalAligConsts(PHTimeStamp &time_stamp, PdbBankID bank_id, const char* output_file = 0);

  //! landau parameters (from file)
  /*!
  these are used in the simulations to decide the charge deposited in the chamber
  by a particle. They are adjusted on a year by year basis to match real-data cluster
  charge distributions
  */
  void fetchLandauParameters( const char* file );

  //! landau parameters
  /*!
  these are used in the simulations to decide the charge deposited in the chamber
  by a particle. They are adjusted on a year by year basis to match real-data cluster
  charge distributions
  */
  void fetchLandauParameters( int run_number );

  //! Translate position of Arm and its Stations.
  void translate(const PHPoint &translation);

  //! Rotate the position and orientation of the arm and its elements.
  void rotate(float angle, char axisLabel);

  //! Get dead channels from a file.
  void fetchDeadChannels(const char *file);

  //! Get dead channels from the database.
  void fetchDeadChannels(PHTimeStamp &time_stamp, PdbBankID bank_id);

  //! Insert dead channels from file into the database.
  bool updateDeadChannels(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file);

  //! Damaged strips need to be re-weighted in the fits.
  void fetchAttenuatedChannels(const char *file);

  //! Damaged strips need to be re-weighted in the fits.
  void fetchAttenuatedChannels(PHTimeStamp &time_stamp, PdbBankID bank_id);

  //! Damaged strips need to be re-weighted in the fits.
  bool updateAttenuatedChannels(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file);

  //! disable dead HV channels which can be 1 or more anode cards
  /*! return the number of (unique) dead HV channels read from file */
  unsigned int fetchDisabledWires(const char *file);

  //! disable dead HV channels which can be 1 or more anode cards
  /*!
    return the number of (unique) disabled HV channels
    \param output_file if non 0, will write disabled to file
  */
  unsigned int fetchDisabledWires(const int& run_number, PdbBankID bank_id, const char* output_file = 0);

  //! disable dead HV channels which can be 1 or more anode cards
  /*!
    return the number of (unique) disabled HV channels
    \param output_file if non 0, will write disabled to file
  */
  unsigned int fetchDisabledWires(PHTimeStamp &time_stamp, PdbBankID bank_id, const char* output_file = 0 );

  //! Update disabled wires from file in the DB.
  bool updateDisabledWires( const int& run_number, PdbBankID bank_id, const char *descrip, const char *file );

  //! Update disabled wires from file in the DB.
  bool updateDisabledWires(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file );

  //! reset list of disabled dead HV channels
  void resetDisabledWires();

  //! disable individual anode wires
  /*! return the number of disabled HV wires */
  unsigned int fetchDisconnectedWires(const char *file);

  //! disable individual anode wires
  /*! return the number of disabled HV wires */
  unsigned int fetchDisconnectedWires(const int& run_number, PdbBankID bank_id);

  //! disable individual anode wires
  /*! return the number of disabled HV wires */
  unsigned int fetchDisconnectedWires(PHTimeStamp &time_stamp, PdbBankID bank_id);

  //! disable individual anode wires
  bool updateDisconnectedWires(PHTimeStamp &time_start, PHTimeStamp &time_stop, PdbBankID bank_id, const char *descrip, const char *file);

  //! Output actual geometry info so that it may be used in simulation.
  bool updatePisaGeom(const char* PisaOutput = "PISAMutGeom.dat");

  /*!
    For Monte Carlo reconstruction, return strip channel and distance
    of hit from strip center given the simulated track position.
    "PisaCoordinate" is the (x,y,z) position of the hit in the plane.
    "stripIP" is the impact parameter of the hit to the midpoint of
    the closest strip.
    "pStrip" is a pointer to the strip object closest to the pisa hit.
  */
  int convertPisaHitToChamber(PHPoint PisaCoordinate, MutWire *prtWire[1], double &wireIP, MutStrip *ptrStrip[2], double stripIP[2]);

  //! Return the anode card of the wire nearest to hitCoordinate.
  int getAnode(PHPoint hitCoordinate, int &station, int &octant, int &gap);

  //! verbosity
  void set_verbosity( const MUTGEOM::Verbosity& verbosity )
  { _verbosity = verbosity; }

  //! Pointers to this arm's stations
  std::vector<MutStation*> f_pMutStations;

  //! Pointer to the parameter object from pisa
  MutPISAPara *PISAPar;

  /*!
    If a timeStamp is given in the constructor, then DB read is the default
    for creation of all subelements.
  */
  bool readFromDB;

  //! nose cone
  double noseConeZmin;

  //! nose cone
  double noseConeZmax;

  //! basic classe to store internal alignment constants
  class InternalAligConst
  {

    public:

    // constructor
    InternalAligConst( void ):
      arm(0), station(0), octant(0), half(0), gap(0), plane(0),
      ds(0), dr(0), dz(0),
      pitch(0), roll(0), yaw(0),
      dExpansion(0)
    {}

    //!@name location
    //@{
    int arm;
    int station;
    int octant;
    int half;
    int gap;
    int plane;
    //@}

    //

    //!@name translation
    //@{

    //! perp to radial
    double ds;

    //! radial
    double dr;

    //! along the beam
    double dz;
    //@}

    //!@name body-centered rotations
    //@{
    double pitch;
    double roll;
    double yaw;
    //@}

    //! expansion
    double dExpansion;

  };

  // list of internal aligmnent parameters
  typedef std::vector<InternalAligConst> InternalAligConstList;

  //! basic classe to store internal alignment constants
  class GlobalAligConst
  {

    public:

    // constructor
    GlobalAligConst( void ):
      arm(0), station(0), octant(0),
      ds(0), dr(0), dz(0),
      pitch(0), roll(0), yaw(0),
      dExpansion(0)
    {}

    //!@name location
    //@{
    int arm;
    int station;
    int octant;
    //@}

    //!@name translation
    //@{

    //! perp to radial
    double ds;

    //! radial
    double dr;

    //! along the beam
    double dz;
    //@}

    //!@name body-centered rotations
    //@{
    double pitch;
    double roll;
    double yaw;
    //@}

    //! expansion
    double dExpansion;

  };

  // list of global aligmnent parameters
  typedef std::vector<GlobalAligConst> GlobalAligConstList;

  // used to find alignment parameters matching given detector
  class SameDetectorFTor
  {

    public:

    //! constructor
    SameDetectorFTor( const InternalAligConst& alig ):
      arm( alig.arm ),
      station( alig.station ),
      octant( alig.octant ),
      half( alig.half ),
      gap( alig.gap ),
      plane( alig.plane )
    {}

    //! constructor
    SameDetectorFTor( const GlobalAligConst& alig ):
      arm( alig.arm ),
      station( alig.station ),
      octant( alig.octant ),
      half( -1 ),
      gap( -1 ),
      plane( -1 )
    {}

    //! equal to operator
    bool operator() (const InternalAligConst& alig ) const
    {
      return
        arm == alig.arm &&
        station == alig.station &&
        octant == alig.octant &&
        half == alig.half &&
        gap == alig.gap &&
        plane == alig.plane;
    }

    //! equal to operator
    bool operator() (const GlobalAligConst& alig ) const
    {
      return
        arm == alig.arm &&
        station == alig.station &&
        octant == alig.octant;
    }

    private:
    int arm;
    int station;
    int octant;
    int half;
    int gap;
    int plane;

  };

  //! get list of internal alignment parameters
  /*! the list is filled when reading geometry from either file or DB */
  const InternalAligConstList& internalAligConsts( void ) const
  { return _internalAligConsts; }

  //! get list of global alignment parameters
  /*! the list is filled when reading geometry from either file or DB */
  const GlobalAligConstList& globalAligConst( void ) const
  { return _globalAligConsts; }

  //! arm number
  MUTGEOM::ArmNumber _arm;

  //! database timestamp
  PHTimeStamp _db_time;

  //! verbosity
  MUTGEOM::Verbosity _verbosity;

  //! Add stations to the Arm.
  void AddStations();

  //! landau parameters container
  class LandauParameters
  {

    public:

    //! constructor
    LandauParameters( void ):
      _arm(-1),
      _station(0),
      _octant(0),
      _gap(0),
      _offset(0),
      _scale(0)
    {}

    //! constructor
    LandauParameters( int arm, int station, int octant, int gap, double offset, double scale ):
      _arm(arm),
      _station(station),
      _octant(octant),
      _gap( gap ),
      _offset( offset ),
      _scale( scale )
    {}

    int _arm;
    int _station;
    int _octant;
    int _gap;
    double _offset;
    double _scale;

  };

  // load from stream
  friend std::istream& operator >> (std::istream&, LandauParameters& );

  //! streamer
  friend std::ostream& operator << ( std::ostream&, const LandauParameters& );

  //! array of landau parameters
  typedef std::vector<LandauParameters> LandauParametersArray;

  //!@name hard-coded sets of landau parameters
  //@{

  void fetchLandauParameters_default( void );
  void fetchLandauParameters_run8( void );
  void fetchLandauParameters_run9( void );

  //! pass set of landau parameters to the relevant TMutGap
  void loadLandauParameters( const LandauParametersArray& );

  //@}

  //! disable anodes
  void disable_anode_cards( const int& sta, const int& oct, const int& gap, const int& anodeCard);

  //! disable anodes
  void disable_anodes( const std::set<std::string>& );

  //! update geometry from InternalAligConst
  void setGlobalAligConst( const GlobalAligConst& );

  //! streamer
  friend std::istream& operator >> ( std::istream&, GlobalAligConst& );

  //! streamer
  friend std::ostream& operator << ( std::ostream&, const GlobalAligConst& );

  //! update geometry from InternalAligConst
  void setInternalAligConst( const InternalAligConst& );

  //! list of internal alignment parameters
  InternalAligConstList _internalAligConsts;

  //! list of internal alignment parameters
  GlobalAligConstList _globalAligConsts;

  //! streamer
  friend std::istream& operator >> ( std::istream&, InternalAligConst& );

  //! streamer
  friend std::ostream& operator << ( std::ostream&, const InternalAligConst& );

};

#endif






