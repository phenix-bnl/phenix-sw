// $Id: MuonSubsysReco.h,v 1.9 2009/06/02 21:32:05 hpereira Exp $

#ifndef _MuonSubsysReco_h_
#define _MuonSubsysReco_h_

/*!
  \file MuonSubsysReco.h
  \ingroup supermodules 
  \brief muon implementation of fun4all MuonSubsysReco
  \author  H. Pereira Da Costa
  \version $Revision: 1.9 $
  \date $Date: 2009/06/02 21:32:05 $
*/

#include <string>
#include <SubsysReco.h>
#include <map>
#include <set>

class PHCompositeNode;

//! muon implementation of fun4all MuonSubsysReco
/*! 
  assign an index to each muon type MuonSubsysReco to keep track of which module comes first and which comes last.
  This is needed to ensure that the handling of the maps is done safely: that 
  1/ the muon PHMaps are reseted only once, in the first module called
  2/ written to the TClonesArray only once, in the last module 
*/
class MuonSubsysReco: public SubsysReco
{
  public:

  //! constructor
  MuonSubsysReco( const char* );
  
  //! destructor
  virtual ~MuonSubsysReco( void );
  
  //! Init method
  /*! 
  it is used to update the "row" index of the module in the list of registered modules, to decide which module
  is supposed to clear the maps and which is supposed to write them 
  */
  int Init(PHCompositeNode *);
  
  //! event reset
  /*! 
  it clears the contents of the PHMapManager at every events, for the last registerred module.
  warning. This base class method _must_ be called by all classes that inherit from these.
  */
  virtual int ResetEvent(PHCompositeNode *topNode);
  
  //! write raw map
  static void print_rows( void );
  
  protected:
  
  //! write all Muon maps, provided the module is the last one.
  /*! 
    Writting is actually done only when called from the first registered module.
    It is supposed to be called at the end of each process_event
    method for modules deriving from MuonSubsysReco
  */
  virtual void write_maps_if_needed( void );

  //! load vertex, provided the module is the first one
  /*!
    loading of the vertex is actually done only when called from the first registered module.
    It is supposed to be colled at the beginning of each process_event 
    method for modules deriving from MuonSubsysReco
  */
  virtual void load_vertex_if_needed( PHCompositeNode* top_node );
  
  private:
      
  //! clear all Muon maps, provided the module is the first one.
  /*! 
    Clearing is actually done only when called from the first registered module.
    It is supposed to be called at the beginning of each process_event
    method for modules deriving from MuonSubsysReco
  */
  virtual void clear_maps_if_needed( void );
  
  //! returns true if this module is first in the list
  bool is_first()  const
  { return _row == _min_row; }
  
  //! returns true if this module is last in the list
  bool is_last() const
  { return _row == _max_row; }
  
  //! returns new row
  static unsigned int _get_row( const std::string& module_name );
  
  //! update min and max rows from set
  static void _update_min_max( void );
  
  //! true if row was initialized, via the Init method
  bool _initialized;
  
  //! this module row in the list
  unsigned int _row;
  
  //!@name static row storage
  //@{
  
  //! static list of recorded rows
  static std::set< unsigned int > _rows;
  
  //! map rows and module names
  static std::map< unsigned int, std::string> _row_map;
  
  //! minimum registered row 
  static unsigned int _min_row;
  
  //! maximum registered row
  static unsigned int _max_row;
    
  //! PHMapManager status
  enum Status
  {
    
    /*! 
      set for each event every time clear_maps is called 
      for the first module
    */
    CLEARED,
        
    /*! 
      set for each event every time write_maps is called 
      for the first module
    */				
    WRITTEN
  };
  
  //! PHMapManager status.
  static Status _map_status;
  
  //@}

};
#endif
