// $Id: MWGRecal.h,v 1.1 2009/07/04 18:33:51 hpereira Exp $

#ifndef MWGRecal_h
#define MWGRecal_h

/*!
  \file    MWGRecal.h
  \brief   muon nanoDST recalibrator. Redo the extrapolation from station1 to vertex
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:33:51 $
*/

#include <SubsysReco.h>
#include <PHMuoTracksOut.h>

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

class PHCompositeNode;

//! muon nanoDST creation, using new framework input nodes
class MWGRecal: public SubsysReco
{
  public:
 
  //! constructor
  MWGRecal( const char* name = "MWGRECAL" ); 
  
  //! destructor
  virtual ~MWGRecal()
  {}

  //! init method
  int Init(PHCompositeNode *top_node);
  
  //! run initialization method
  int InitRun(PHCompositeNode *top_node);
  
  //! event loop
  int process_event(PHCompositeNode *top_node);
  
  //! vertex error
  /*! default is 0.5, which corresponds to Au+Au */
  void set_z_vertex_error( const double& value )
  { _z_vertex_error = value; }
  
  //! End
  int End(PHCompositeNode *top_node);
  
  //! pattern recognition flags
  enum Flag
  {
    NONE = 0,
      
    //! input file is MC 
    /*! load external vertex from PISA header node rather that PHGlobal */
    MC = (1<<0),
    
    //! redo extrapolation from MuTR station 1 to vertex
    EXTRAPOLATE_TRACKS = (1<<1),
    
    //! re-fit dimuon vertices
    FIT_DIMUON_VERTEX = (1<<2)

  };
    
  //! flags
  void set_flags( const unsigned int& value )
  { _flags = value; }
  
  //! flags
  void set_flag( const Flag& flag, const bool& value )
  {
    if( value ) _flags |= flag;
    else _flags &= (~flag);
  }
    
  //! flags
  bool get_flag( const Flag& flag ) const
  { return _flags & flag; }

  protected:
  
  //! load external vertex (from simulation "header" node or PHGlobal)
  bool load_vertex( PHCompositeNode* node );
  
  //! extrapolate tracks from MuTR station1 to vertex
  /*! 
  this is needed because the wrong magnetic field and material map was used
  when producing run7 nanoDSTs, so that the extrapolation done during the production
  is not correct 
  */
  bool extrapolate_track( PHMuoTracksOut *, const int& );

  //! refit dimuon vertex, based on the re-extrapolated track
  bool fit_dimuon_vertex( PHMuoTracksOut *, const int& );
   
  //! vertex
  double _z_vertex;
  
  //! vertex error
  double _z_vertex_error;
  
  //! flags
  unsigned int _flags;
  
  #ifndef __CINT__
  //! Timer
  PHTimeServer::timer _timer;
  #endif
  
};

#endif



