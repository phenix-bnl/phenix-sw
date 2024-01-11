
using namespace std;



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


void update_mutr_alignment()
{

  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  PdbBankID bankID = 0;
  
  TString source_data_file  = "mut.internalAligConsts.dat";

  typedef std::vector<InternalAligConst> InternalAligConstList;
  InternalAligConstList _internalAligConsts;

  
  fstream f_src(source_data_file, ios_base::in);


  static const int bufsize = 256;
  char linebuf[bufsize];

  while(f_src.getline(linebuf,bufsize,'\n'))
  {

    // skip empty or commented lines
    if( !strlen( linebuf ) ) continue;
    if( strncmp( linebuf, "//", 2 ) == 0 ) continue;

    istringstream stringbuf(linebuf);

    // dump to alignement constant
    InternalAligConst alig;
    stringbuf >> alig;

  }
  
  f_src.close;
}

