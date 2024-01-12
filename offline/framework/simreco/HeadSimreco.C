#include <HeadSimreco.h>
#include <RunHeader.h>
#include <EventHeader.h>
#include <EventTypes.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>


#include <PISAEventHeader.h>

#include <PHCompositeNode.h>

#include <cmath>

using namespace std;

//__________________________________________________________
// return 0, -1 or +1 depending on integer value
template < typename T >
int get_sign( const T& value ) 
{
  if( !value ) return 0;
  return (value > 0) ? 1:-1;
}

//__________________________________________________________
HeadSimreco::HeadSimreco(const string &name): HeadReco(name)
{
  eventsequence = 0;
  return ;
}

int
HeadSimreco::Init(PHCompositeNode *topNode)
{
  HeadReco::Init(topNode);
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader)
    {
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("RUNNUMBER"))
    {
      int runnumber = rc->get_IntFlag("RUNNUMBER");
      runheader->set_RunNumber(runnumber);
    }
    }
  return EVENT_OK;
}

//_____________________________________________________________________
int HeadSimreco::InitRun(PHCompositeNode *topNode)
{
  HeadReco::InitRun(topNode);
  PISAEventHeader *pisaEventHeader = PISAEventHeader::GetEventHeader();
  float mapScaleFactor = 1;
  if (pisaEventHeader)
  {
    mapScaleFactor = pisaEventHeader->GetMapFScale();
  } else {
    cout << "HeadSimreco::InitRun - No pisaEventHeader found, setting mapScaleFactor to " << mapScaleFactor << endl;
  }
    
  RunHeader *rh = findNode::getClass<RunHeader>(topNode, "RunHeader");
  cout << "HeadSimreco::InitRun - mapScaleFactor: " << mapScaleFactor << endl;
  
  // this is done to avoid double inversion of magnetic field sign.
  // meaning: if mapScale is negative, and central magnet current is negative, one would actually 
  // keep the sign of the currents unchanged.
  if( get_sign( rh->get_currentCentral() ) == get_sign( mapScaleFactor ) )
  { mapScaleFactor = fabs(mapScaleFactor); }
  
  rh->set_currentCentral( int(rh->get_currentCentral()*mapScaleFactor));
  rh->set_currentInner( int(rh->get_currentInner()*mapScaleFactor ));
  rh->set_currentNorth( int(rh->get_currentNorth()*mapScaleFactor ));
  rh->set_currentSouth( int(rh->get_currentSouth()*mapScaleFactor ));
    
  // some dump
  if (verbosity)
  {
    cout << "HeadSimreco::InitRun - south arm magnet current = " << rh->get_currentSouth() << endl;
    cout << "HeadSimreco::InitRun - north arm magnet current = " << rh->get_currentNorth() << endl;
    cout << "HeadSimreco::InitRun - central magnet current = " << rh->get_currentCentral() << endl;
    cout << "HeadSimreco::InitRun - inner magnet current = " << rh->get_currentInner() << endl;      
  }
  
  startrunticks = rh->get_TimeStart();
  return EVENT_OK;
}

int
HeadSimreco::process_event(PHCompositeNode *topNode)
{
  eventsequence++;
  EventHeader* eventheader = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (eventheader)
    {
      eventheader->set_EvtSequence(eventsequence);
      eventheader->set_EvtType(DATAEVENT);
      eventheader->set_TimeStamp(startrunticks);
      if (verbosity)
        {
          eventheader->identify();
        }
    }
  return EVENT_OK;
}

