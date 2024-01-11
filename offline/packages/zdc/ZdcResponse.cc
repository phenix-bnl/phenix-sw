#include <iostream>
#include "gsl/gsl_const.h"
#include "PHObject.h"
#include "PHIODataNode.h"
#include "ZdcDCM.h"
#include "ZdcOut.h"
#include "ZdcResponse.h"

using namespace std;

static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;

static PHDWORD raw[32];

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<ZdcOut>   ZdcOutNode_t;
typedef PHIODataNode<ZdcDCM>   ZdcDCMNode_t;
typedef PHIODataNode<ZdcRaw>   ZdcRawNode_t;

ZdcResponse::ZdcResponse()
{
  zdcgea = 0;
  zdcraw = 0;
  phrawdatanode = 0;
}

void ZdcResponse::Clear()
{
  // Reset ZDC Geant Data  
  if (zdcgea) zdcgea->Reset();
}

PHBoolean ZdcResponse::PutEndProduct(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  // get ZdcGeaHits object if it exists
  if (!zdcgea)
    {
      PHTypedNodeIterator<ZdcGeaHits> zdcgeaiter(topNode);
      PHIODataNode<ZdcGeaHits> *ZdcGeaNode = zdcgeaiter.find("ZdcGeaHits");
      if (ZdcGeaNode)
        {
          zdcgea = ZdcGeaNode->getData();
        }
      if (!zdcgea)
        {
          cout << PHWHERE << "Unable to get ZdcGeaHits object" << endl;
          return False;
        }
    }

  // get prdf output object if it exists
  if (!phrawdatanode)
    {
      PHCompositeNode *prdfNode =
          static_cast<PHCompositeNode*>( iter.findFirst("PHCompositeNode","PRDF") );
      if (!prdfNode)
        {
          cout << "ZdcResponse: Unable to get prdfNode, is it missing?" << endl;
          return False;
        }
      else
        {
cout << "found prdfnode" << endl;
        }

      phrawdatanode = new PHRawDataNode(0,"zdcPRDF000",0,0,0,0);
      prdfNode->addNode( phrawdatanode );
    }

  // get ZdcRaw object if it exists
  if (!zdcraw)
    {
      PHTypedNodeIterator<ZdcRaw> zdcrawiter(topNode);
      PHIODataNode<ZdcRaw> *ZdcRawNode = zdcrawiter.find("ZdcRaw");
      if (ZdcRawNode)
        {
          zdcraw = ZdcRawNode->getData();
        }
      if (!zdcraw)
        {
          cout << PHWHERE << "Unable to get ZdcRaw object" << endl;
          return False;
        }
    }

  calculate();

  return True;
}

int ZdcResponse::calculate()
{
  double esum[2] = { 0., 0. };
  double tofsum[2] = { 0., 0. };

  //-*** get calibrated information (energy, time);
  unsigned int nhits = zdcgea->get_nhits();
  cout << "nhits " << nhits << endl;
  for (unsigned int ihit=0; ihit<nhits; ihit++)
    {
      ZdcPISAHit *zdchit = zdcgea->get_ZdcPISAHits( ihit );

      Float_t dele = zdchit->GetDele();
      Float_t tof = zdchit->GetTof();
      Int_t north_south = zdchit->GetDirection();

      esum[north_south] += dele;
      tofsum[north_south] += tof;
    }
  cout << "nesum = " << esum[Zdc::North]
       << " sesum = " << esum[Zdc::South] << endl;
  cout << "ntofsum = " << tofsum[Zdc::North]
       << " stofsum = " << tofsum[Zdc::South] << endl;

  for (int i=0; i<32; i++) raw[i] = 0;

  raw[0] = 0xf0000;	// flag word
  raw[1] = 0xe0018;	// module address
  raw[2] = 0x60004;	// event number
  raw[3] = 0x6001f;	// beam clock counter
  raw[4] = 0x6000d;	// detector id

  raw[5] = int((tofsum[Zdc::South]/nhits) * 1e6 - 60000) & 0xffff;
  raw[6] = int((tofsum[Zdc::South]/nhits) * 1e6 - 60000) & 0xffff;
  raw[7] = int(esum[Zdc::South]) & 0xffff;

  raw[17] = int((tofsum[Zdc::North]/nhits) * 1e6 - 60000) & 0xffff;
  raw[18] = int((tofsum[Zdc::North]/nhits) * 1e6 - 60000) & 0xffff;
  raw[19] = int(esum[Zdc::North]) & 0xffff;

  zdcraw->Clear();

  PHDWORD *rawptr = &raw[5];
  for (int ich=0; ich<8; ich++)
    {
      zdcraw->AddZdcRawHit( rawptr[2], rawptr[0], rawptr[1], ich );

      *rawptr++ |= (0xc0000 | ich<<20);
      *rawptr++ |= (0xb0000 | ich<<20);
      *rawptr++ |= (0xa0000 | ich<<20);
    }

  raw[29] = 0x10080000;
  raw[30] = 0x10080000;
  raw[31] = 0x10070000;
  phrawdatanode->setData( raw );
  phrawdatanode->setLength( 32 );
  phrawdatanode->setID( 13001 );
  phrawdatanode->setWordLength( 4 );
  phrawdatanode->setHitFormat( 1013 );

  // to get the module number, use y = z - 1868 - imod*22.5
  return 0;
}

