// $Id: MuidEffic.h,v 1.21 2009/05/12 02:20:28 shoji Exp $
#ifndef __MUIDEFFIC_H__
#define __MUIDEFFIC_H__

/*!
  \file MuidEffic.h
  \ingroup supermodules 
  \brief reads MC and reconstructed muid maps, fills efficiency ntuples
  \author S. Kelly
  \version $Revision: 1.21 $
  \date $Date: 2009/05/12 02:20:28 $
*/

#include <SubsysReco.h>

// Forward declerations
class PHCompositeNode;
class TNtuple;

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

/*!
  \class MuidEffic
  \ingroup supermodules 
  \brief reads MC and reconstructed muid maps, fills efficiency ntuples
*/
class MuidEffic: public SubsysReco
{
 public:
  
  //! constructor
  MuidEffic( const char* name= "MUIDEFFIC", const char* file = 0 );
  
  //! destructor
  virtual ~MuidEffic() {}
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! init method (begin of process)
  int Init(PHCompositeNode *topNode);
  
  //! end of process method
  int End(PHCompositeNode *topNode);
  
  //! root output filename
  void set_filename( const char* file )
  { if( file ) _filename = file; }
  
  //! option to run with muid only (for cosmics)
  void set_muid_only( const bool& b )
  { _muid_only = b; }
  
 protected:
  
  //! initialize module ntuples
  bool initialize_ntuple();
  
  //! return unique index from gap and orientation
  int get_plane_index( const int& gap, const int& orientation ) const;
  
#ifndef __CINT__
  //! module timer
  PHTimeServer::timer _timer;
#endif
  
  //! efficiency ntuple
  TNtuple* _ntuple;
  
  //! output ntuple file name
  std::string _filename;
  
  //! flags
  bool _is_sim;
  bool _muid_only;
  
  // constants
  enum { kHoriz = 0,
         kVert = 1,
         nOrients = 2,
	 nGaps = 5 };
  
};

#endif /* __MUIDEFFIC_H__ */
