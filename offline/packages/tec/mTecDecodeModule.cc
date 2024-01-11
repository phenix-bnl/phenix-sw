#include "phool.h"
#include "PHCompositeNode.h"
#include "Event.h"
#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"

#include "mTecDecodeModule.h"
#include "dTecFemDataWrapper.h"
#include "TecOutV1.hh"
#include "TecTrack.hh"
#include "TecHit.hh"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<dTecFemDataWrapper> dTecFemDataNode_t;
typedef PHIODataNode<TecOutV1> TecOutNode_t;

mTecDecodeModule::mTecDecodeModule(){
  Verbose=0;
  Write2File=0;
  UseOldFormat=0;
  UseObjy=1;
  FadcCut=2;
}

//===============================================================================

mTecDecodeModule::~mTecDecodeModule(){}

//===============================================================================

PHBoolean mTecDecodeModule::event(PHCompositeNode *topNode) {
  cerr << "mTecDecodeModule ERROR: " << endl
       << "  event(PHCompositeNode*) method is obsolete." << endl
       << "  Use event(PHCompositeNode*, TecAddressObject*) method instead." 
       << endl;  
  return False;
}

//===========================================================================

PHBoolean mTecDecodeModule::event(PHCompositeNode *topNode, 
                                  TecAddressObject* TAO) {

// Get the Tec packets from the Event object and transfer the
// data to the dTecFemData table.

static int CallNumber = 0;

if(Verbose>0) cout << "mTecDecodeModule: Started...   " << CallNumber << endl;

  PHNodeIterator iter(topNode);
  PHCompositeNode* tecNode;
  PHNode *n1;

// Find the node with the Event object; it is called "PRDF".
  n1 = iter.findFirst("PHDataNode", "PRDF");
  if (!n1) {
    return False;
  }

// Extract the Event object from the node.
  EventNode_t* node = static_cast<EventNode_t*>(n1);
  Event* event = node->getData();
  if (!event) {
    return False;
  }

// Get pointer to dTecFemData table

  dTecFemDataNode_t* FDN = static_cast<dTecFemDataNode_t*>(iter.findFirst("PHIODataNode", "dTecFemData"));
  if (!FDN) {
    cerr << "mTecDecodeModule -> ERROR: dTecFemData table not found." << endl;
    return False;
  }
  dTecFemDataWrapper* dTecFemData = FDN->getData();

//--------------------------------------------------------------------------

// Try to find TecOut in the node tree

  PHTypedNodeIterator<TecOutV1> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;

  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
    if(Verbose>0)
      cout << "mTecDecodeModule: Found TecOutV1." << endl;
  }
  else {		// TecOut Does not exist. Add it to node tree.
    if(Verbose>0)
      cout << "mTecDecodeModule: Adding TecOutV1 to node tree..." << endl;

    tecNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TEC"));
    if (!tecNode) {
      cerr << "mTecDecodeModule ERROR: TEC node does not exist." << endl;
      return False;
    }

// Add TecOut to tecNode

      tecout = new TecOutV1();
      PHIODataNode<PHObject>* TecOutNodeNew = new 
        PHIODataNode<PHObject>(tecout,"TecOutV1","PHObject");
      tecNode->addNode(TecOutNodeNew);
      if(Verbose>0) 
             cout << "mTecDecodeModule: TecOutV1 added to tecNode." << endl;

  }
// Set run number
  int runnumber = event->getRunNumber();
  tecout->setRunNumber(runnumber);

//---------------------------------------------------------------------------

  Packet* p;
  int nok = 0;
  static const int id_base     = TECBASEPACKETID;
  static const int max_packets = TECMAXPACKETS;
  static int evt_save = -1;
  int DCM[2000];
  int sum, nw;
  int id,ik,ij;
  int mycrate,myslot,mypsadd1,mypsadd2,mypsadd;
  int amp,amplitude[80];
  int *allSamples = 0, nwout;
  int module,scheme,slot,crate,psadd1,psadd2,evt=0,parity;
  int datawds,DetID; 
  float xyz[3] = {0., 0., 0.};
  float charge = 0.;
  int trackid = -1;
  int wire = -1,index = -1;

  int npackets = 0;

//--------- Read TEC packets -------------------------------------

// Loop over packets in the event (maximum 800)

  for (int k = 0; k < max_packets; k++) {
    id = id_base + k;


    if ( (p = event->getPacket(id)) != 0) {

    if(Verbose>1) {
      cout << "---------------------------------------------------------" << endl;     
      p->identify();
    }
    npackets++;

    // get crate and slot from packet ID
      mycrate  = TAO->getCrate(id);
      myslot   = TAO->getSlot(id);
      mypsadd1 = TAO->getPsadd1(id);
      mypsadd2 = TAO->getPsadd2(id);

      p->fillIntArray( DCM,              // address of buffer
		       2000,             // size of buffer
		       &nw,              // number of words read
		       "DATA");          //  includes header, data
                                         //  and trailer

      module = p->iValue(0,"MODULE");
      scheme   = p->getHitFormat();
      slot =   ((module & 0x0000001F)     );
      crate =  ((module & 0x000003E0) >> 5);
      psadd1 = ((module & 0x00003C00) >> 10);
      psadd2 = ((module & 0x0003C000) >> 14);
      evt = p->iValue(0,"EVTNR"); 		// event number
	parity = p->iValue(0,"PARITY");		
	datawds   = p->getDataLength(); // header, data and trailer
				        // datawds must be equal nw
	DetID     = (DCM[0] & 0xfffff);

        int myparity = 0;
        for(int j=0; j<nw-2; j++) {myparity = myparity ^ (DCM[j] & 0xfffff); }

	if(Verbose>0) {
	if (DetID != 0x4000) cerr << "mTecDecodeModule ERROR: " <<
                                     "Wrong detector ID = " << DetID << endl;
	if((parity & 0xfffff) != myparity ) cerr << "mTecDecodeModule ERROR: " <<
	     "Parity Error: " << (parity & 0xfffff) << " " << myparity << endl;
	if(crate!=mycrate || slot!=myslot) cerr << "mTecDecodeModule ERROR: " <<
             "Hardware address mismatch: " << crate << " " << mycrate << " "
                                           << slot << " " << myslot << endl;
        if((evt_save >=0) && (evt != evt_save+1)) { 
          cerr << "mTecDecodeModule ERROR: " << "Event sequence error: " 
               << evt << " " << evt_save+1 << endl; 
        } 
	if(Verbose>1) {
        cout << "myPacketID: " << id << " " << mycrate << " " << myslot 
        << " " << mypsadd1 << " " << mypsadd2 << endl;
        cout << "  PacketID: " << id << " " << crate << " " << slot 
        << " " << psadd1 << " " << psadd2 << " " << hex << module << dec << " " 
        << nw << " " << datawds << " " << hex << DetID << " " 
        << (parity & 0xfffff) << " " << myparity << dec << " " << scheme 
        << " " << evt << endl;
  }
} // end Verbose>0

// loop over channels of this FEM and fill dTecFemData table
// fill mTecFemData only if hardware indices make sense

        if(mycrate>=0 && myslot>=0) {

	  allSamples = p->getIntArray(&nwout);
	  if (nwout != TECMAXCHANNEL * TECMAXTIMEBIN)
	    {
	      cout << PHWHERE 
		   << "Major problem!"
		   << endl;
	      delete [] allSamples;
	      return False;
	    }
        for(ij=0; ij<TECMAXCHANNEL; ij++) {

          sum = 0;
          for(ik=0; ik<TECMAXTIMEBIN; ik++) {
	    amplitude[ik] = allSamples[TECMAXCHANNEL * ij + ik];
             if(amplitude[ik]>FadcCut) sum += amplitude[ik];
          }

              if(sum>0) {

                  dTecFemData->SetRowCount(nok+1);
                  dTecFemData->set_id(nok,nok);
                  dTecFemData->set_crate(nok,mycrate);
                  dTecFemData->set_slot(nok,myslot);
                  if((ij<32) && ((mypsadd1!=15) || (mypsadd1!=-1))) {
                    dTecFemData->set_psadd(nok,mypsadd1);
                    mypsadd=mypsadd1;
                  }
                  else {
                     if((ij>=32) && ((mypsadd2!=15) || (mypsadd1!=-1))) {
                       dTecFemData->set_psadd(nok,mypsadd2);
                       mypsadd=mypsadd2;
                     }
                     else    // this should never happen 
                     {
                       mypsadd=-1;
                       dTecFemData->set_psadd(nok,mypsadd);
                       cerr << "mTecDecodeModule ERROR: Wrong psadd: " << nok << " " 
		            << id << " " << mycrate << " " << myslot << " "
                            << mypsadd1 << " " << mypsadd2 << endl;
                     }
                  }
                  dTecFemData->set_ichan(nok,ij);

                  PHBoolean status0 = TAO->setHard(mycrate,myslot,mypsadd,ij);
                  if(status0) {
      		    int isect=TAO->getSector();
      		    int iside=TAO->getSide();
      		    int iplane=TAO->getPlane();
      		    wire=TAO->getWire();
                    index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+iside;
                  }
		  else {
                      wire=-1;
                      index=-1;
                    if(Verbose>0) {
                      cerr << "mTecDecode Hard/Soft Map ERROR: " << mycrate
                           << " " << myslot << " " << mypsadd << " " << ij
                           << " " << id << endl;
                    } 
                  }

                    for(ik=0; ik<TECMAXTIMEBIN; ik++) {
                      amp = amplitude[ik];
                      if(amp>FadcCut) {dTecFemData->set_amplitude(ik,nok,amp);}
                        else { dTecFemData->set_amplitude(ik,nok,0); }
		      if(amp>FadcCut && status0) { 
			tecout->AddTecHit(index,wire,ik,amp,
					  charge,xyz,trackid);
                      }
                    }

                  nok++;

              } // sum>0
        } // ij - loop over 64 channels 
	delete [] allSamples;
        } // mycrate & myslot OK
        else {
          if(Verbose>0) cerr << "mTecDecode ERROR: "
                             << "Bad crate or slot number for packetID = " 
                             << id << " " << mycrate << " " << crate 
                             << " " << myslot << " " << slot << endl;
        }

      delete p;   // event->getPacket creates a packet object on the
                  // heap, so delete it here!

    } // if packet exists
  } // end k loop over packets

  evt_save = evt;

  if(Verbose > 0) {
    cout << "mTecDecodeModule: Finished event # " << CallNumber << " " << evt << endl;
    cout << "mTecDecodeModule: Number of entries in dTecFemData table = " 
         << dTecFemData->RowCount() << endl;
    cout << "mTecDecodeModule: Number of Packets = " 
         << npackets << endl;
  }

  CallNumber++;

  return True;
}

