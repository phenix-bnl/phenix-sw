/*!
  \file FvtxAlign.cxx 
  \ingroup supermodules
  \brief fvtx align module
  \author Zhengyun You
  \version $Revision: 1.1 $
  \date $Date: 2011/01/05 20:18:53 $
*/

#include <FvtxAlign.h>

#include <PHTFileServer.h>
#include <TNtuple.h>

#include <FVTXOO.h>
#include <FvtxGeom.h>

//___________________________________________________________________
FvtxAlign::FvtxAlign( const char* name, const char* file ) : 
  SubsysReco( name ),
  _filename( file ? file:"fvtx_align_ntuples.root" ),
  _timer( PHTimeServer::get()->insert_new( name ) )
{}

//___________________________________________________________________
int FvtxAlign::InitRun(PHCompositeNode *topNode)
{
  PHTFileServer::get().open( _filename,"RECREATE");

  return 0;
}

//___________________________________________________________________
int FvtxAlign::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();  

/*  write_align_ntuple(top_node);

  static ulong_t auto_save=0;
  if(auto_save++%10 == 0) {
     _align_vars2->autosave();
     phtfileserver::get().flush( _filename );
  }
*/

  _timer.get()->stop();

  return 1;
}

//___________________________________________________________________
int FvtxAlign::End(PHCompositeNode* top_node) 
{
  _timer.get()->print_stat();
//  phtfileserver::get().write( _filename );
  return 0;
}

//___________________________________________________________________
void FvtxAlign::write_align_ntuple(PHCompositeNode* top_node)
{

    std::cout << "started write_align_ntuple" <<std::endl;

}

