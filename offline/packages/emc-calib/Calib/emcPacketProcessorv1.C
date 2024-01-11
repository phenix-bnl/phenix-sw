#include "emcPacketProcessorv1.h"
#include "packet.h"
#include "packet_emc.h"
#include "emcTowerContent.h"
#include "emcTowerContainer.h"
#include "emcDataError.h"

ClassImp(emcPacketProcessorv1)

namespace {

  void checkData(emcTowerContainer& towers, int imin, int imax)
  {
    for ( int i = imin; i  <= imax; ++i ) 
      {
	emcTowerContent* tower = towers.getTower(i);
	
	int dataerror = 0;
	
	// Checks the adc/tdc values.
	
	int hgpost,lgpost,hgpre,lgpre,tac;
	
	hgpost = tower->HGPost();
	hgpre  = tower->HGPre();
	lgpost = tower->LGPost();
	lgpre  = tower->LGPre();
	tac    = tower->TAC();
	
	if ( hgpre <= emcDataError::HG_MIN() || 
	     hgpre>=emcDataError::HG_MAX() )
	  {
	    dataerror |= emcDataError::HG_PRE_OUT();
	  }
	if ( hgpost <= emcDataError::HG_MIN() || 
	     hgpost>=emcDataError::HG_MAX() )
	  {
	    dataerror |= emcDataError::HG_POST_OUT();
	  }
	if ( lgpre <= emcDataError::LG_MIN() || 
	     lgpre>=emcDataError::LG_MAX() )
	  {
	    dataerror |= emcDataError::LG_PRE_OUT();
	  }
	if ( lgpost <= emcDataError::LG_MIN() || 
	     lgpost>=emcDataError::LG_MAX() )
	  {
	    dataerror |= emcDataError::LG_POST_OUT();
	  }
	if ( tac <= emcDataError::TAC_MIN() || 
	     tac>=emcDataError::TAC_MAX() ) 
	  {
	    dataerror |= emcDataError::TAC_OUT();
	  }
	
	tower->SetDataError(dataerror);
      }
  }
}

using std::cout;
using std::cerr;
using std::endl;

//_____________________________________________________________________________
emcPacketProcessorv1::emcPacketProcessorv1()
{
  fDataArray = new int*[6];
  for ( size_t i = 0 ; i < 6 ; i++) 
    {
      fDataArray[i] = new int[144];
    }
}

//_____________________________________________________________________________
emcPacketProcessorv1::~emcPacketProcessorv1()
{
  for ( size_t i = 0 ; i < 6 ; i++) 
    {
      delete[] fDataArray[i];
    }
  delete[] fDataArray;
}

//_____________________________________________________________________________
void
emcPacketProcessorv1::identify(std::ostream& os) const
{
  os << "emcPacketProcessorv1::identify" << std::endl;
}

//_____________________________________________________________________________
int
emcPacketProcessorv1::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
bool
emcPacketProcessorv1::process(const Packet& packet, emcTowerContainer& towers)
{ 
  // The Packet get methods are not const... so we use a hugly
  // const_cast here to allow our process method signature to be correct 
  // (i.e. we really want to advertise that the packet will not
  // be changed by us).

  Packet& ncpacket = const_cast<Packet&>(packet);

  Packet_emc* p = dynamic_cast<Packet_emc*>(&ncpacket);
  if (!p) 
    {  
      cerr << "<E> " << __FILE__ << "  " << __LINE__
	   << " Cannot convert Packet to Packet_emc : ";
      ncpacket.identify(cerr);       
      return false;
    }

  int n = p->filliList6x144(fDataArray,0, 0, -4096);

  int ntowers = towers.size();

  int imin = ntowers;

  for ( int i = 0 ; i < n ; i++ ) 
    {
      emcTowerContent* t = towers.addTower(ntowers);
      
      t->SetID(p->getIdentifier()-8001,fDataArray[5][i]);

      t->SetRaw(4095 - fDataArray[1][i],
		4095 - fDataArray[3][i],
		4095 - fDataArray[2][i],
		4095 - fDataArray[4][i],
		4095 - fDataArray[0][i],
		p->iValue(1,"AMU"), // pre
		p->iValue(2,"AMU"), // post
		p->iValue(0,"AMU"), // tac
		p->iValue(0,"BCLK") // beamclock
		);

      ++ntowers;
  }

  int imax = ntowers-1;

  checkData(towers,imin,imax);

  return true;
}

//_____________________________________________________________________________
void
emcPacketProcessorv1::Reset()
{
  
}
