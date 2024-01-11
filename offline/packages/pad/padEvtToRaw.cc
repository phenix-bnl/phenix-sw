// Class: padEvtToRaw (implementation)
//
// Created by: David Silvermyr
//
// Description:
// This class reads fired channel information from an Event (the packets
// in an event) and fills the raw data tables.

#include <PHIODataNode.h>
#include <PadAddressObject.hh>
#include <PadCalibrationObject.hh>
#include <recoConsts.h> 
#include <padEvtToRaw.hh>
#include <dPadRawWrapper.h>
#include <Event.h>
#include <getClass.h>

#include <iostream>

using namespace std;

// Constructor for padEvtToRaw
padEvtToRaw::padEvtToRaw()
{
  addressObj = new PadAddressObject;
  PadCalObj  = new PadCalibrationObject;
}

// Destructor for padEvtToRaw
padEvtToRaw::~padEvtToRaw()
{
  delete addressObj; 
  delete PadCalObj;
}

int
padEvtToRaw::FetchCalDataFromPdbCal(PHTimeStamp &TS)
{
  
  PadCalObj->setTimeStamp(TS);
  //fetches # of hot channels
  PadCalObj->Fetch();
  return 0;
}

// Event method for padEvtToRaw
int
padEvtToRaw::event(PHCompositeNode* topNode,
                   Event *evt,
                   Event *evt2,
                   short evtreal,
                   short evt2real)
{
 
 
  unsigned int nentries[3]; // number of entries in the tables
  memset(nentries, 0, sizeof(nentries));
  unsigned int j;

  // Find the input tables
  dPadRawWrapper *dPc1Raw = findNode::getClass<dPadRawWrapper>(topNode, "dPc1Raw");

  if (!dPc1Raw)
    {
      cout << "padEvtToRaw::event ERROR: dPc1Raw not found.\n";
      exit(1);
    }

  dPadRawWrapper *dPc2Raw = findNode::getClass<dPadRawWrapper>(topNode, "dPc2Raw");
  if (!dPc2Raw)
    {
      cout << "padEvtToRaw::event ERROR: dPc2Raw not found.\n";
      exit(1);
    }

  dPadRawWrapper *dPc3Raw = findNode::getClass<dPadRawWrapper>(topNode, "dPc3Raw");
  if (!dPc3Raw)
    {
      cout << "padEvtToRaw::event ERROR: dPc3Raw not found.\n";
      exit(1);
    }


  // ok, let's now loop over all PC packets in the event(s), see what we find
  // and fill the tables
  short ipacketid;
  short pc, arm, side, sector, padz, padx;
  short padcolumn, padrow, realk;


  // set row counts
  dPc1Raw->SetRowCount(nentries[0]);
  dPc2Raw->SetRowCount(nentries[1]);
  dPc3Raw->SetRowCount(nentries[2]);

  static short occupied[2160];
  short nRaw1Pc1 = 0, nRaw1Pc2 = 0, nRaw1Pc3 = 0;
  short nRaw2Pc1 = 0, nRaw2Pc2 = 0, nRaw2Pc3 = 0;
  short nOverlapPc1 = 0, nOverlapPc2 = 0, nOverlapPc3 = 0;

  // and now: let's store the fired channels, evt first, then evt2


  for (ipacketid = 4001; ipacketid <= 4096; ipacketid++)
    {

      memset(occupied, 0, sizeof(occupied));
      Packet *p = evt->getPacket(ipacketid);


      if (p)
        {
	    //# of pad hits from hot rocs = (# of hot TGL*16) + 1500 (baseline)
	    unsigned int npadlimit = (PadCalObj->GetNumberofPadHits(ipacketid))*16+1500;
	    unsigned int hits = p->iValue(0, "PADSUM");
	    // cout << PHWHERE << "packet =  " << ipacketid
	    //	   << "   npadlimit =  " << npadlimit << endl;
	    
          if (hits > npadlimit)
            {
              cout << PHWHERE << "Too many hits in packet " << ipacketid
		   << ": " << hits << endl;

              delete p;
              return -1;
            }
          pc = addressObj->getDet(ipacketid);
          arm = addressObj->getArm(ipacketid);
          if ( (pc > 0) && (evtreal != 1) )
            {
              arm = 1 - arm; // switch arm for PC2/3 for simulation PRDF
            }
          side = addressObj->getSide(ipacketid);
          sector = addressObj->getSector(ipacketid);

          int k = 0; // pad index
          int ival = 0; // 0<=>off, 1<=>on
          int iword = 0; // word index
          int ix = 0; // bits in the word

          int nw = 0; // number of words
          int padsOn[108] = {0};
          p->fillIntArray(padsOn, 108, &nw);

          for (iword = 0; iword < 108; iword++)
            { // along z
              for (ix = 0; ix < 20; ix++)
                { // bits in word; across wires
                  k = (108 - iword) * 20 - ix - 1;
                  ival = ( (padsOn[iword] >> ix) & 0x1 );
                  if (ival == 1)
                    { // pad is on

                      if (evtreal == 1)
                        { // only channel swap for PC2 in real data
                          // PC2 correction
                          if (pc == 1)
                            {
                              // switching 2:11 and 2:13 for PC2 (holes 19 and 44)
                              padcolumn = (k % 240) / 20; // 0 to 11
                              padrow = k % 4; // 0 to 3
                              if ( (padcolumn == 6) && (padrow == 2) )
                                {
                                  realk = k + 18;
                                }
                              else if ( (padcolumn == 7) && (padrow == 0) )
                                {
                                  realk = k - 18;
                                }
                              else
                                {
                                  realk = k;
                                }
                            }
                          else
                            {
                              realk = k;
                            }
                        }
                      else
                        {
                          realk = k;
                        }

		      if (realk > 2159) {
			cout << PHWHERE << " ERROR: overflow realk = "  << realk << endl;
			return -1;
		      }

                      padz = addressObj->getPadz(realk);
                      padx = addressObj->getPadx(ipacketid, realk);

                      occupied[realk]++; // we want to make sure that we are not double counting

                      // ok. let's store this guy..
                      if (pc == 0)
                        {
                          j = 0;
                          if (nentries[j] < dPc1Raw->MaxRowCount())
                            {
                              dPc1Raw->set_arm(nentries[j], arm);
                              dPc1Raw->set_side(nentries[j], side);
                              dPc1Raw->set_sector(nentries[j], sector);
                              dPc1Raw->set_padz(nentries[j], padz);
                              dPc1Raw->set_padx(nentries[j], padx);
                              dPc1Raw->set_padtype(nentries[j], 0);
                              dPc1Raw->set_id(nentries[j], nentries[j]); // tmp.: don't know what to put here..
                              nentries[j]++;
                              nRaw1Pc1++;
                            }
                          else
                            {
                              cout << PHWHERE << "Number of entries exceeds dPc1Raw table size "
				   << endl;
                              exit(1);
                            }

                        }
                      else if (pc == 1)
                        {
                          j = 1;
                          if (nentries[j] < dPc2Raw->MaxRowCount())
                            {
                              dPc2Raw->set_arm(nentries[j], arm);
                              dPc2Raw->set_side(nentries[j], side);
                              dPc2Raw->set_sector(nentries[j], sector);
                              dPc2Raw->set_padz(nentries[j], padz);
                              dPc2Raw->set_padx(nentries[j], padx);
                              dPc2Raw->set_padtype(nentries[j], 0);
                              dPc2Raw->set_id(nentries[j], nentries[j]); // tmp.: don't know what to put here..
                              nentries[j]++;
                              nRaw1Pc2++;
                            }
                          else
                            {
                              cout << PHWHERE << "Number of entries exceeds dPc2Raw table size"
				   << endl;
                              exit(1);
                            }
                        }
                      else if (pc == 2)
                        {
                          j = 2;
                          if (nentries[j] < dPc3Raw->MaxRowCount())
                            {
                              dPc3Raw->set_arm(nentries[j], arm);
                              dPc3Raw->set_side(nentries[j], side);
                              dPc3Raw->set_sector(nentries[j], sector);
                              dPc3Raw->set_padz(nentries[j], padz);
                              dPc3Raw->set_padx(nentries[j], padx);
                              dPc3Raw->set_padtype(nentries[j], 0);
                              dPc3Raw->set_id(nentries[j], nentries[j]); // tmp.: don't know what to put here..
                              nentries[j]++;
                              nRaw1Pc3++;
                            }
                          else
                            {
                              cout << PHWHERE << "Number of entries exceeds dPc3Raw table size"
				   << endl;
                              exit(1);
                            }
                        }
                    }
                }
            }

          delete p;
        } // p
      if (evt2)
        {
          Packet *p = evt2->getPacket(ipacketid);

          if (p)
            {
              pc = addressObj->getDet(ipacketid);
              arm = addressObj->getArm(ipacketid);
              if ( (pc > 0) && (evt2real != 1) )
                {
                  arm = 1 - arm; // switch arm for PC2/3 for simulation PRDF
                }
              side = addressObj->getSide(ipacketid);
              sector = addressObj->getSector(ipacketid);

              for (int k = 0; k < 2160; k++)
                {
                  if (p->iValue(k) == 1)
                    { // pad is on

                      if (evt2real == 1)
                        { // only channel swap for PC2 in real data
                          // PC2 correction
                          if (pc == 1)
                            {
                              // switching 2:11 and 2:13 for PC2 (holes 19 and 44)
                              padcolumn = (k % 240) / 20; // 0 to 11
                              padrow = k % 4; // 0 to 3
                              if ( (padcolumn == 6) && (padrow == 2) )
                                {
                                  realk = k + 18;
                                }
                              else if ( (padcolumn == 7) && (padrow == 0) )
                                {
                                  realk = k - 18;
                                }
                              else
                                {
                                  realk = k;
                                }
                            }
                          else
                            {
                              realk = k;
                            }
                        }
                      else
                        {
                          realk = k;
                        }

		      if (realk > 2159) {
			cout << PHWHERE << " ERROR: overflow realk = "  << realk << endl;
			return -1;
		      }

                      padz = addressObj->getPadz(realk);
                      padx = addressObj->getPadx(ipacketid, realk);

                      if (occupied[realk] == 0)
                        { // we want to make sure that we are not double counting
                          // ok. let's store this guy..
                          if (pc == 0)
                            {
                              j = 0;
                              dPc1Raw->set_arm(nentries[j], arm);
                              dPc1Raw->set_side(nentries[j], side);
                              dPc1Raw->set_sector(nentries[j], sector);
                              dPc1Raw->set_padz(nentries[j], padz);
                              dPc1Raw->set_padx(nentries[j], padx);
                              dPc1Raw->set_padtype(nentries[j], 0);
                              dPc1Raw->set_id(nentries[j], nentries[j]); // tmp.: don't know what to put here..
                              nentries[j]++;
                              nRaw2Pc1++;
                            }
                          else if (pc == 1)
                            {
                              j = 1;
                              dPc2Raw->set_arm(nentries[j], arm);
                              dPc2Raw->set_side(nentries[j], side);
                              dPc2Raw->set_sector(nentries[j], sector);
                              dPc2Raw->set_padz(nentries[j], padz);
                              dPc2Raw->set_padx(nentries[j], padx);
                              dPc2Raw->set_padtype(nentries[j], 0);
                              dPc2Raw->set_id(nentries[j], nentries[j]); // tmp.: don't know what to put here..
                              nentries[j]++;
                              nRaw2Pc2++;
                            }
                          else if (pc == 2)
                            {
                              j = 2;
                              dPc3Raw->set_arm(nentries[j], arm);
                              dPc3Raw->set_side(nentries[j], side);
                              dPc3Raw->set_sector(nentries[j], sector);
                              dPc3Raw->set_padz(nentries[j], padz);
                              dPc3Raw->set_padx(nentries[j], padx);
                              dPc3Raw->set_padtype(nentries[j], 0);
                              dPc3Raw->set_id(nentries[j], nentries[j]); // tmp.: don't know what to put here..
                              nentries[j]++;
                              nRaw2Pc3++;
                            }
                        } // not previously occupied (from first PRDF)
                      else
                        { // overlapping..
                          if (pc == 0)
                            {
                              nOverlapPc1++;
                            }
                          else if (pc == 1)
                            {
                              nOverlapPc2++;
                            }
                          else if (pc == 2)
                            {
                              nOverlapPc3++;
                            }
                        }
                    }
                }

              delete p;
            } // p
        } // evt2
    } // end of loop over packetid

  // final count
  dPc1Raw->SetRowCount(nentries[0]);
  dPc2Raw->SetRowCount(nentries[1]);
  dPc3Raw->SetRowCount(nentries[2]);

  return 0;
}  // end method padEvtToRaw::event
