// $Id: Fun4AllPisaInputManager.C,v 1.21 2018/10/12 13:34:41 phnxbld Exp $

#include <Fun4AllServer.h>
#include <Fun4AllPisaInputManager.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllSyncManager.h>
#include <getClass.h>
#include <recoConsts.h>

#include <dcghitWrapper.h>
#include <fkinWrapper.h>
#include <primaryWrapper.h>
#include <headerWrapper.h>
#include <pythiaWrapper.h>

#include <bbcghitWrapper.h>
#include <dcghitWrapper.h>
#include <pcghitWrapper.h>

#include <PISAEventHeader.h>

#include <KinGetGEA.h>
#include <BbcGetGEA.h>
#include <DchGetGEA.h>
#include <PadGetGEA.h>

#include <RunToTime.hh>

#include <frog/FROG.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIOManager.h>
#include <PHTypedNodeIterator.h>
#include <PHTimeStamp.h>

#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

//_____________________________________________________________________
Fun4AllPisaInputManager::Fun4AllPisaInputManager(
  const string &name,
  const string &nodename,
  int verbose) :
  Fun4AllInputManager(name,"DST"),
  _is_open( false ),
  _events_total( 0 ),
  _events_thisfile( 0 )
{
  verbosity=verbose;  
  return ;
}

//_____________________________________________________
Fun4AllPisaInputManager::~Fun4AllPisaInputManager()
{
  cout << "Fun4AllPisaInputManager::~Fun4AllPisaInputManager" << endl;
  Reset();
  return ;
}

//_____________________________________________________
void Fun4AllPisaInputManager::Reset()
{

  cout << "Fun4AllPisaInputManager::Reset." << endl;

  // clear hits
  _pisa_run.PrintCounters();
  _pisa_run.HitsClear();
  _pisa_run.Reset();
  
   cout << "Fun4AllPisaInputManager::Reset - done." << endl;
 
}

//____________________________________________________________________
int Fun4AllPisaInputManager::fileopen(const string &filein)
{

  // get run number from recoConst
  recoConsts* rc( recoConsts::instance() );
  int run_number( rc->get_IntFlag("RUNNUMBER") );
    
  // guess timestamp from run number if valid
  if( run_number != 0 )
  {
    
    RunToTime *runTime = RunToTime::instance();
    PHTimeStamp *time_stamp_ptr = runTime->getBeginTime( abs(run_number) );
    if ( time_stamp_ptr )
    {
      
      cout << "Fun4AllPisaInputManager::fileopen - run number: " << run_number << endl;
      cout << "Fun4AllPisaInputManager::fileopen - time stamp: " << *time_stamp_ptr << endl;
      rc->set_TimeStamp( *time_stamp_ptr );
      delete time_stamp_ptr;

    } else { 
      
      cout << "Fun4AllPisaInputManager::fileopen - no valid timestamp found for run number: " << run_number << endl; 
      exit(1);
    
    }
    
  } else { 
    cout 
      << "Fun4AllPisaInputManager::fileopen - run number not set. Add" << endl
      << "Fun4AllPisaInputManager::fileopen - recoConsts::instance()->set_IntFlag(\"RUNNUMBER\",run_number); " << endl
      << "Fun4AllPisaInputManager::fileopen - to your macro."
      << endl;
    exit(1);
  }
 
  // check sync manager
  if (! mySyncManager)
  {
    cout << "Things have changed, please register the input manager "
      << this->Name()
      << " with the Fun4AllServer before opening a file" << endl;
    exit(1);
  }
  
  mySyncManager->CurrentRun(run_number);
  cout << "Fun4AllPisaInputManager::fileopen - run_number: " << run_number << endl;

  // check if not already opened
  if ( _is_open )
  {
    cout << "Fun4AllPisaInputManager::fileopen - current file is still opened: " << filein << endl;
    cout << "Fun4AllPisaInputManager::fileopen - call fileclose first." << endl;
    return -1;
  }
  
  // store new filename
  filename = filein;
  cout << "Fun4AllPisaInputManager::fileopen - opening file " << filein << endl;

  // use frog to get true filename
  string fname( FROG().location(filein.c_str()) );

  // reset pisa interface
  _pisa_run.Reset();

  // try add file and check
  if ( !_pisa_run.AddFile( fname ) ) return -1;

  _is_open = true;
  _events_thisfile = 0;

  // make sure needed nodes are present
  CreateNodeTree( Fun4AllServer::instance()->getNode(InputNode.c_str() ) );

  return 0;

}

//___________________________________________________________________________________
int Fun4AllPisaInputManager::pisaMergeFileOpen(const string &filename)
{

  // check that at least one file is opened
  if( !_is_open )
  {
    cout << "Fun4AllPisaInputManager::pisaMergeFileOpen - no file open. " << endl;
    cout << "Fun4AllPisaInputManager::pisaMergeFileOpen - use Fun4AllPisaInputManager::fileopen first." << endl;
    return -1;
  }
  
  // try open new TFile
  string fname( FROG().location(filename.c_str()) );
  
  // try add file and check
  if( !_pisa_run.AddFile( fname ) ) return -1;
  
  return 0;

}

//_____________________________________________________________________
int Fun4AllPisaInputManager::PushBackEvents(const int nevt)
{

  if (  _events_thisfile > nevt) 
  {
    
    // reset this file and total number of events
    _events_thisfile -= nevt;
  
  } else {
    
    cout << "Fun4AllPisaInputManager::PushBackEvents - nevt: " << nevt << " is larger than _events_thisfile: " << _events_thisfile << endl;
    cout << "Fun4AllPisaInputManager::PushBackEvents - _events_thisfile is set to 0. " << endl;
    _events_thisfile = 0;
    
  }
  
  return 0;
  
}

//_________________________________________________________________________________
//#define NEWONLY
int Fun4AllPisaInputManager::run(const int nevents)
{

  
  
  // process as many events as passed in argument
  for( int processed = 0; processed < ((nevents > 0 ) ? nevents:1); processed++ )
  {
    
    // clear hits from previous events
    _pisa_run.HitsClear();

    // check if there is one file opened
    if( !_is_open && (OpenNextFile() != 0 ) )
    {
      cout << "Fun4AllPisaInputManager::run - no file opened." << endl;
      return -1;
    }

    // check event number is valid (with respect to number of available files)
    if( _events_thisfile >= _pisa_run.GetMaxEvents() )
    {
    
      // current file is completed. Close it, try open next file
      fileclose();
      if( OpenNextFile() != 0 ) 
      {
        cout << "Fun4AllPisaInputManager::run - all events processed." << endl;
        return -1;
      }
      
    }
    
    // initialization/printouts at first event of new file
    if( !_events_thisfile ) 
    {
      
      // propagate verbosity to pisaRun
      _pisa_run.set_verbosity( verbosity );
      
      cout << "Fun4AllPisaInputManager::run - _n_files = " <<  _pisa_run.GetNInputs() << endl;
      cout << "Fun4AllPisaInputManager::run - _max_events_thisfile = " << _pisa_run.GetMaxEvents() << endl;
      
    }
            
    // try run event
    if( !_pisa_run.GetOneEvent( _events_thisfile ) ) return -1;
    
    // fill staff tables
    PHCompositeNode *top_node = Fun4AllServer::instance()->topNode();

    // set pisa header event number
    /* 
    note: in case of several files opened in a raw, it is unclear whether 
    one should set this to the current event id in current file
    or to the total number of processed events. Right now, ther former is used.
    */
    PISAEventHeader *pisaEventHeader = PISAEventHeader::GetEventHeader();
    pisaEventHeader->SetEvent( _events_thisfile );
    
    // fill comonly used staff tables
    BbcGetGEA( top_node );    
    DchGetGEA( top_node );
    PadGetGEA( top_node );
    KinGetGEA( top_node );

    // increment total number of processed events
    _events_thisfile++;
    _events_total++;
    
  }
  
  return 0;

}

//__________________________________________________________________
int Fun4AllPisaInputManager::fileclose()
{
  
  // check if there is some file opened
  if (!_is_open)
  {
    cout << PHWHERE << "No Input file open" << endl;
    return -1;
  }

  // reset all structures
  Reset();
  
  /* 
    set open flag to false, to prevent running
    on non existing files
  */
  _is_open = false;
  
  return 0;
  
}

//_______________________________________________________________________
int Fun4AllPisaInputManager::CreateNodeTree(PHCompositeNode *input_node)
{

  PHCompositeNode *top_node = Fun4AllServer::instance()->topNode();
  if(!top_node)
  {
    cout << "\n Fun4AllPisaInputManager::CreateNodeTree - cannot find top_node node\n";
    return -1;
  }

  PHNodeIterator top_iter(top_node);

  // DST node
  PHCompositeNode *dst_node = static_cast<PHCompositeNode*>( top_iter.findFirst("PHCompositeNode", "DST") );
  if(!dst_node)
  {
    cerr << "Fun4AllPISAInputManager::CreateNodeTree - DST node not found " << endl;
    return -1;
  }

  // eva node
  PHNodeIterator dst_iter(top_node);
  PHCompositeNode *eva_node = static_cast<PHCompositeNode*>( dst_iter.findFirst( "PHCompositeNode", "EVA" ) );
  if( !eva_node )
  {
    cerr << "Fun4AllPISAInputManager::CreateNodeTree - creating EVA node " << endl;
    eva_node = new PHCompositeNode("EVA");
    dst_node->addNode(eva_node);
  }

  // gea node
  PHCompositeNode *gea_node = static_cast<PHCompositeNode*>( dst_iter.findFirst( "PHCompositeNode", "GEA" ) );
  if( !gea_node )
  {
    cerr << "Fun4AllPISAInputManager::CreateNodeTree - creating GEA node " << endl;
    gea_node = new PHCompositeNode("GEA");
    dst_node->addNode(gea_node);
  }

  // sim PRDF node
  PHCompositeNode *simPrdf = dynamic_cast<PHCompositeNode*> (top_iter.findFirst("PHCompositeNode", "SIMPRDF"));
  if (!simPrdf)
  {
    cerr << "Fun4AllPISAInputManager::CreateNodeTree - creating SIMPRDF node " << endl;
    simPrdf = new PHCompositeNode("SIMPRDF");
    top_node->addNode(simPrdf);
  }

  fkinWrapper* fkin = findNode::getClass<fkinWrapper>(eva_node,"fkin");
  if( !fkin )
  {
    fkin = new fkinWrapper("fkin",30000);
    PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkin,"fkin");
    eva_node->addNode(fkinNode);
  }
  
  primaryWrapper* primary =  findNode::getClass<primaryWrapper>(eva_node,"primary");
  if( !primary )
  {
    primary = new primaryWrapper("primary",30000);
    PHIODataNode<PHTable>* primaryNode = new PHIODataNode<PHTable>(primary,"primary");
    eva_node->addNode(primaryNode);
  }
  
  pythiaWrapper* pythia = findNode::getClass<pythiaWrapper>(eva_node,"pythia");
  if( !pythia ) 
  {
    pythia = new pythiaWrapper("pythia",1);
    PHIODataNode<PHTable>* pythiaNode = new PHIODataNode<PHTable>(pythia,"pythia");
    eva_node->addNode(pythiaNode);
  }
  
  headerWrapper* header = findNode::getClass<headerWrapper>(input_node,"header");
  if(!header)
  {
    header = new headerWrapper("header",1);
    PHIODataNode<PHTable>* headerNode = new PHIODataNode<PHTable>(header,"header");
    eva_node->addNode(headerNode);
  }

  // BBC staff table
  bbcghitWrapper* bbcghit = findNode::getClass<bbcghitWrapper>(gea_node,"bbcghit");
  if( !bbcghit )
  {
    bbcghit = new bbcghitWrapper("bbcghit", 1000);
    PHIODataNode<PHTable>* bbcghitNode = new PHIODataNode<PHTable>(bbcghit, "bbcghit");
    gea_node->addNode(bbcghitNode);
  }
  
  // max array size for dc node
  dcghitWrapper* dcghit = findNode::getClass<dcghitWrapper>(input_node, "dcghit");
  if (!dcghit)
  {
    dcghit = new dcghitWrapper("dcghit", 60000);
    PHIODataNode<PHTable>* dcghitNode =
    new PHIODataNode<PHTable>(dcghit, "dcghit");
    gea_node->addNode(dcghitNode);
  }

  for (int i = 1;i <= 3;i++)
  {
    ostringstream NodeName;
    NodeName.str("");
    NodeName << "pc" << i << "ghit";
    pcghitWrapper *pcghit = findNode::getClass<pcghitWrapper>(input_node, NodeName.str().c_str());
    if (!pcghit)
    {
      pcghit = new pcghitWrapper(NodeName.str().c_str(), 1500);
      PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(pcghit, NodeName.str().c_str());
      gea_node->addNode(NewNode);
    }
  }

  return 0;
}

//________________________________________________
int Fun4AllPisaInputManager::OpenNextFile()
{
  
  while( !filelist.empty() )
  {
    
    string filename( filelist.front() );
    filelist.pop_front();

    if( fileopen( filename ) == 0 ) return 0;
              
  }
  
  // could not open any file
  return -1;
  
}
