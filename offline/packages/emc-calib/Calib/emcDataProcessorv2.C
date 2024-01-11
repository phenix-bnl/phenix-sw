#include "emcDataProcessorv2.h"

#include "emcFEMList.h"
#include "emcPacketProcessorv1.h"
#include "emcRawDataProcessorv2.h"
#include "emcCalibrationDataHelper.h"
#include "emcDCProcessorv2.h"

#include "packet.h"
#include "Event.h"
#include "EmcIndexer.h"
#include <cassert>
#include "emcTowerContainer.h"

ClassImp(emcDataProcessorv2)

//_____________________________________________________________________________
emcDataProcessorv2::emcDataProcessorv2()
{
  fPacketProcessor = getPacketProcessor();
  fRawDataProcessor = 0;
  fDCProcessor = 0;
  fCalibrationDataHelper = 0;
  fRunNumber = 0;
  fTimeStamp = 0;
}

//_____________________________________________________________________________
emcDataProcessorv2::emcDataProcessorv2(int runnumber, 
				       const PHTimeStamp& ts, 
				       bool initall,
				       emcManageable::EStorage dataSource,
				       const char* sectors)
{
  fPacketProcessor = getPacketProcessor();
  fRawDataProcessor = 0;
  fDCProcessor = 0;
  fRunNumber = runnumber;
  fTimeStamp = new PHTimeStamp(ts);
  fCalibrationDataHelper = new emcCalibrationDataHelper
    (fRunNumber,*fTimeStamp,initall, dataSource,sectors);
  emcFEMList f(sectors);

  for ( int i = 0; i < 172; ++i ) 
    {
      if ( f.hasFEM(i) )
	{
	  fFemList.insert(i);
	}
    }
}

//_____________________________________________________________________________
emcDataProcessorv2::~emcDataProcessorv2()
{
  delete fPacketProcessor;
  delete fRawDataProcessor;
  delete fDCProcessor;
  delete fCalibrationDataHelper;
  delete fTimeStamp;
}

//_____________________________________________________________________________
bool
emcDataProcessorv2::calibrate(emcTowerContainer* pbsc, 
			      emcTowerContainer* pbgl, 
			      time_t incrementalTime)
{
  if ( !fDCProcessor ) 
    {
      if ( fRunNumber > 0 ) 
	{
	  if ( !fCalibrationDataHelper ) 
	    {
	      if ( fTimeStamp ) 
		{  
		  // preferred method. Specify the timestamp
		  fCalibrationDataHelper = 
		    new emcCalibrationDataHelper(fRunNumber,*fTimeStamp,true);
		}
	      else
		{
		  // Otherwise, this guy will try to figure
		  // out by himself what's the timestamp, from
		  // the runnumber. This might fail... and then
		  // timestamp will be = "now".
		  fCalibrationDataHelper = 
		    new emcCalibrationDataHelper(fRunNumber,true);
		}
	    }

	  fDCProcessor = getDCProcessor(fCalibrationDataHelper);
	}
      else
	{
	  std::cerr << "<E> emcDataProcessorv2::calibrate : runnumber ("
		    << fRunNumber << " is invalid !" << std::endl;
	  return false;
	}
    }

  return fDCProcessor->calibrate(pbsc,pbgl,incrementalTime);
}



//_____________________________________________________________________________
bool
emcDataProcessorv2::decode(const Event& event, 
			   emcTowerContainer* pbsc, 
			   emcTowerContainer* pbgl)
{
  // This ugly const_cast is there just because some methods
  // in Event are not const, but still we'd like our decode method
  // to advertise that it will not change the event, so Event is const.
  Event& ncevent = const_cast<Event&>(event);

  if ( fRunNumber <= 0 ) 
    {
      setRunNumber(ncevent.getRunNumber());
      assert(fRunNumber>0);
    }

  if (pbsc)
    {
      pbsc->Reset();
    }

  if (pbgl)
    {
      pbgl->Reset();
    }

  // FIXME: what about reference FEMs (for online only).
  std::set<int>::const_iterator it;

  for ( it = fFemList.begin(); it != fFemList.end(); ++it ) 
    {    
      int ifem = *it;

      int packetID = 8001+ifem;

      Packet* packet = ncevent.getPacket(packetID);
      
      if ( packet ) 
	{
	  if ( pbsc && EmcIndexer::isPbScFEM(ifem) )
	    {
	      bool ok = fPacketProcessor->process(*packet,*pbsc);
	      if (!ok)
		{
		  std::cerr << "Could not process packet " << packetID 
			    << " in event " 
			    << std::endl;
		  ncevent.identify(std::cerr);
		}
	    }
	  else if ( pbgl && EmcIndexer::isPbGlFEM(ifem ) )
	    {
	      bool ok = fPacketProcessor->process(*packet,*pbgl);
	      if (!ok)
		{
		  std::cerr << "Could not process packet " << packetID 
			    << " in event " 
			    << std::endl;
		  ncevent.identify(std::cerr);
		}
	    }
	}      
      delete packet;
    }

  return true;
}

//_____________________________________________________________________________
emcDCProcessor*
emcDataProcessorv2::getDCProcessor(emcCalibrationDataHelper* cdh) const
{
  return new emcDCProcessorv2(cdh);
}

//_____________________________________________________________________________
emcCalibrationDataHelper*
emcDataProcessorv2::getCalibrationDataHelper() const
{
  return fCalibrationDataHelper;
}

//_____________________________________________________________________________
emcPacketProcessor*
emcDataProcessorv2::getPacketProcessor() const
{
  return new emcPacketProcessorv1;
}

//_____________________________________________________________________________
emcRawDataProcessor*
emcDataProcessorv2::getRawDataProcessor(emcCalibrationDataHelper* cdh) const
{
  return new emcRawDataProcessorv2(cdh);
}

//_____________________________________________________________________________
void
emcDataProcessorv2::identify(std::ostream& os) const
{
  os << "emcDataProcessorv2::identify" << std::endl;
}

//_____________________________________________________________________________
int
emcDataProcessorv2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcDataProcessorv2::Reset()
{  
}

//_____________________________________________________________________________
bool
emcDataProcessorv2::toADCandTDC(emcTowerContainer* pbsc, 
				emcTowerContainer* pbgl, 
				const emcBadModules& bad)
{
  if ( !fRawDataProcessor ) 
    {
      if ( fRunNumber > 0 ) 
	{
	  if ( !fCalibrationDataHelper ) 
	    {
	      if ( fTimeStamp )
		{
		  fCalibrationDataHelper = 
		    new emcCalibrationDataHelper(fRunNumber,*fTimeStamp,true);
		}
	      else
		{
		  fCalibrationDataHelper = 
		    new emcCalibrationDataHelper(fRunNumber,true);
		}
	    }

	  fRawDataProcessor = getRawDataProcessor(fCalibrationDataHelper);
	}
      else
	{
	  std::cerr << "<E> emcDataProcessorv2::toADCandTDC : runnumber ("
		    << fRunNumber << " is invalid !" << std::endl;
	  return false;
	}
    }

  return fRawDataProcessor->toADCandTDC(pbsc,pbgl,bad);
}

