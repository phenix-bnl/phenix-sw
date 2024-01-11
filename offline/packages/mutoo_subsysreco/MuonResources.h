// $Id: MuonResources.h,v 1.3 2007/12/10 12:59:51 hpereira Exp $
#ifndef __MuonResources_H__
#define __MuonResources_H__

//////////////////////////////////////////////////////////////
/*! 
  \file MuonResources.h
  \brief muon code resources evaluation
  \author  Hugo Pereira
  \version $Revision: 1.3 $
  \date $Date: 2007/12/10 12:59:51 $
*/
//////////////////////////////////////////////////////////////

#include <list>
#include <string>
#include <SubsysReco.h>
#include <PHResources.h>

class TTree;
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

//////////////////////////////////////////////////////////////
/*! 
  \class MuonResources
  \brief muon code resources evaluation
*/
//////////////////////////////////////////////////////////////

class MuonResources: public SubsysReco
{
  public:
  
  //! constructor
  MuonResources( 
    const char *name = "MUONRESOURCES", 
    const char* filename= "resources.root"
    );

  //! destructor
  virtual ~MuonResources() 
  {}
  
  //! direct initialization
  void initialize( void );
  
  //! unique initialisation
  int Init(PHCompositeNode *topNode);
  
  //! event processing
  int process_event(PHCompositeNode *topNode);
  
  //! full end
  int End(PHCompositeNode *topNode);
  
  //! supermodule name
  const char *Name() const
  {return ThisName.c_str(); }
  
  //! event sampling
  void set_event_sampling( const int& value )
  { _event_sampling = value; }
  
  //! defines mutoo evaluation filename
  void set_file_name( const char* file ) 
  { if( file ) _file_name = file; }
  
  //! add map to keep track of multiplicity
  void add_map( const std::string& name );
    
  protected:
  
  //! initialization flag
  /*! 
    it is used to allow initialization 
    either manually (in the root macro) 
    or via Fun4All
  */
  bool _initialized;
  
  //! sampling to run top command 
  /*! default value correspond to once every 100 events */
  int _event_sampling;
       
  //! resources
  PHResources _resources;
  
  #ifndef __CINT__
  //! module timer.
  PHTimeServer::timer _timer;
  #endif
  
  //! evaluation file
  std::string _file_name;
    
  //! resources tree
  TTree *_resources_tree;               
 
  //! local event number
  int _event;
  
  //! job start time
  int _start_time;
  
  //! event time
  int _time;
   
  #ifndef __CINT__
     
  //! map multiplicity management
  class MapMultiplicity
  {
    public:
    
    //! constructor
    MapMultiplicity( const std::string& name ):
      _name( name ),
      _current(0),
      _previous(0),
      _diff(0)
      {}
    
    //! update method
    bool update( PHCompositeNode* top_node );
      
    //! map name
    std::string _name;
    
    //! current size
    int _current;
    
    //! previous size
    int _previous;
    
    //! difference
    int _diff;
  };
  
  //! list of map
  typedef std::list<MapMultiplicity> MapMultiplicity_list;
  
  //! maps
  MapMultiplicity_list _maps;
  
  #endif
  
};

#endif
