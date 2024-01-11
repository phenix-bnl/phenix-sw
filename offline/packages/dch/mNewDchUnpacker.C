#include <mNewDchUnpacker.hh>
#include <PHDchAddressObject.h>
#include <dDchNibbleGhitWrapper.h>
#include <dDchRawWrapper.h>
#include <dDchGhitRawWrapper.h>
#include <DchRawTable.hh>
#include <DchRawOutv1.hh>
#include <Event.h>
#include <getClass.h>
#include <iostream>
#include <cstring>

using namespace std;

mNewDchUnpacker::mNewDchUnpacker()
{
  // initialize finder array (only used for sims)
  memset(finder, -1, sizeof(finder));
}

int
mNewDchUnpacker::event(PHCompositeNode *topNode,
                       Event *evt,
                       const short startpacket,
                       const short endpacket)
{
  //------------------------------------------------------------------------
  // DESCRIPTION: 1)This routine unpacks the DCM table into the RAW table.
  //              The detector hardware address
  //              (arm,side,keystone,pair) is deduced from the data, although
  //              this information is also contained in the DCM table packetID.
  //             2)If the NibbleGhit table is not empty, then it is used to
  //             fill the GhitRaw table and thus restoring the information
  //             needed for the evaluator.
  //-------------------------------------------------------------------------

  PHDchAddressObject* dchAddressObject = findNode::getClass<PHDchAddressObject>(topNode, "DchDAO");

// used only for sims - node is only looked up if MCFlag is on
  dDchGhitRawWrapper *dchghit = 0; 

  DchRawTable *rawTable = findNode::getClass<DchRawTable>(topNode, "DchRawTablev1");
  if (!rawTable)
    {
      cout << "DchRawTablev1 not found " << endl;
      return -1;
    }

  //----------------------------------------------------------

  int rawcounter = -1;
  short arm, side, keystone, pair, channel, TimeSlice, mask;
  short harm, hside, hcell, hplane;
  short Nibble, edge, time, value, width;
  unsigned long mod;
  unsigned long int DCMword;
  int geantID, rawID;

  // locally used variables

  short Parm = -1;   // previous raw data read, use to identify
  short Pside = -1;   // trailing edge of same hit
  short Pkeystone = -1;
  short Ppair = -1;
  short Pchannel = -1;
  short Pedge = -1;
  short ltime = -10;
  short ttime = -10;
  int ltmpindex = 0;

  short flagMC = dchAddressObject->getFlagMC();
  int tmpindex;
  static int rawdat[1000];
  int number_of_words;
  int my_parity, fem_parity;

  //-----------------------------------------------------------------------------------
  // if Monte-Carlo data use the relational tables
  //-----------------------------------------------------------------------------------
  if (flagMC)
    {
      //--------------------------------------------------------------------------------------
      // Run through the data and fill in the finder array with the GEANT ID for each nibble.
      //--------------------------------------------------------------------------------------

      // Get the MC objects from the node tree
      dDchNibbleGhitWrapper *nibbleghit =
        findNode::getClass<dDchNibbleGhitWrapper>(topNode, "dDchNibbleGhit");
      dchghit =
        findNode::getClass<dDchGhitRawWrapper>(topNode, "dDchGhitRaw");
      dchghit->SetRowCount(0);


      // initialize finder array
      memset(finder, -1, sizeof(finder));
      for (unsigned int i = 0; i < nibbleghit->RowCount(); i++)
        {
          arm = nibbleghit->get_arm(i);
          side = nibbleghit->get_side(i);
          keystone = nibbleghit->get_key(i);
          pair = nibbleghit->get_pair(i);
          channel = nibbleghit->get_channel(i);
          Nibble = nibbleghit->get_Nibble(i);

          if (arm < 0 || arm >= numberOfArms)
            {
              cerr << "dDchNibbleGhit table corrupt: arm value is over the limits = " << arm << endl;
              continue;
            }
          if (side < 0 || side >= numberOfSides)
            {
              cerr << "dDchNibbleGhit table corrupt: side value is over the limits = " << side << endl;
              continue;
            }
          if (keystone < 0 || keystone >= numberOfKeyStones)
            {
              cerr << "dDchNibbleGhit table corrupt: keystone value is over the limits = " << keystone << endl;
              continue;
            }
          if (pair < 0 || pair >= numberOfPairs)
            {
              cerr << "dDchNibbleGhit table corrupt: pair value is over the limits = " << pair << endl;
              continue;
            }
          if (channel < 0 || channel >= numberOfChannels)
            {
              cerr << "dDchNibbleGhit table corrupt: channel value is off the limits = " << channel << endl;
              continue;
            }
          if (Nibble < 0 || Nibble >= numberOfNibbles)
            {
              cerr << "dDchNibbleGhit table corrupt: nibble value is off the limits = " << Nibble << endl;
              continue;
            }

          tmpindex = Nibble + numberOfNibbles * (channel + numberOfChannels * (pair +
									       numberOfPairs * (keystone + numberOfKeyStones * (side + numberOfSides * arm))));

          finder[tmpindex] = nibbleghit->get_ghitid(i);   //  Fill ghitid into finder array
        }
    }  // end if flagMC

  for (short ipacketid = 3000 + startpacket; ipacketid <= 3000 + endpacket; ipacketid++)       // Loop over DCH packets
    {
      Packet *p = evt->getPacket(ipacketid);

      if (p)                                                        // If packet exist, decode it
        {
          short dlength = p->getDataLength();

          if (dlength > 1000)
            {
              cerr << "Dch packet " << ipacketid << " length is too high " << dlength << " - packet is corrupt !!!" << endl;
            }
          else
            {
              p->fillIntArray(rawdat, 1000, &number_of_words, "DATA");  // rawdat is the packet data

              dchAddressObject->decodePacketID(ipacketid);       // get hard addresses from packetID

              arm = dchAddressObject->getArm()->getValue();
              side = dchAddressObject->getSide()->getValue();
              keystone = dchAddressObject->getKeystone()->getValue();
              pair = dchAddressObject->getPair()->getValue();

              if (rawdat[0]&0x80000000)                           //  There is a header information
                {
                  short tarm = rawdat[0] & 0x01;	              // Decode arm
                  mod = rawdat[2] & 0x000FFFFF;
                  short tpair = mod & 0x0f;                    // Decode pair
                  mod >>= 4;
                  short tkeystone = mod & 0xff;                     // Decode keystone
                  mod >>= 8;
                  short tside = mod & 0xf;                         // Decode pair

                  // boundary check goes here

                  if (tarm < 0 || tarm >= numberOfArms)
                    {
                      cout << "Dch Packet " << ipacketid << " corrupt: arm value is over the limits = " << tarm << endl;
                      delete p;
                      continue;
                    }
                  if (tside < 0 || tside >= numberOfSides)
                    {
                      cout << "Dch Packet " << ipacketid << " corrupt: side value is over the limits = " << tside << endl;
                      delete p;
                      continue;
                    }
                  if (tkeystone < 0 || tkeystone >= numberOfKeyStones)
                    {
                      cout << "Dch Packet " << ipacketid << " corrupt: keystone value is over the limits = " << tkeystone << endl;
                      delete p;
                      continue;
                    }
                  if (tpair < 0 || tpair >= numberOfPairs)
                    {
                      cout << "Dch Packet " << ipacketid << " corrupt: pair value is over the limits = " << tpair << endl;
                      delete p;
                      continue;
                    }

                  // match hard <-> soft addresses
                  if (arm != tarm)
                    {
                      arm = tarm;
                    }
                  if (side != tside)
                    {
                      side = tside;
                    }
                  if (keystone != tkeystone)
                    {
                      keystone = tkeystone;
                    }

                  // This error is critical as it might have been
                  // caused by slipped packet. Disregard data.
                  if (pair != tpair)
                    {
                      delete p;
                      continue;
                    }

                  // Event integrity checks goes here
		  if (p->iValue(0, "CHECKPARITY") < 0)
		    {
		      my_parity = 0;
		      for (int j = 0; j < (number_of_words - 3); j++ )
			{
			  my_parity = my_parity ^ ( rawdat[j] & 0xfffff );
			}
		      fem_parity = (rawdat[dlength - 2] & 0xfffff);

		      if (!flagMC && fem_parity != my_parity)
			{
			   cout << "Event " << evt->getEvtSequence()
				<< ": Parity error in DCH packet " << ipacketid
			        << "   FEM parity = " << hex << fem_parity 
                                << " recalc parity  = " << my_parity << dec << endl;
			}
		    }
                  //////////////////////////////////////////////////////////////////////////////////////
                }

              for (short x = 0; x < dlength; x++)
                {
                  DCMword = rawdat[x];
                  if (DCMword&0x80000000)
                    continue;                // header or trailer word - skip it
                  if (DCMword&0x000FFFFF)
                    {  //  In case of non-zerosupressed data get rid of all zero words

                      channel = (DCMword >> 24) & 0x7f;    // Decode channel

                      if (channel < 0 || channel >= numberOfChannels)
                        {
                          cerr << "Dch Packet " << ipacketid << " corrupt: Channel value is over the limits = " << channel << endl;
                          continue;
                        }

                      // Are we in new channel ?
                      // Set edge to initial value
                      if ((Parm != arm) || (Pside != side) || (Pkeystone != keystone) || (Ppair != pair) || (Pchannel != channel))
                        {
                          Pedge = -1;
                          ltime = -10;
                          ttime = -10;
                        }

                      dchAddressObject->setHard(arm, side, keystone, pair, channel);

                      harm = dchAddressObject->getArm()->getValue();
                      hside = dchAddressObject->getSide()->getValue();
                      hcell = dchAddressObject->getCell()->getValue();
                      hplane = dchAddressObject->getPlane()->getValue();

                      TimeSlice = (DCMword >> 20) & 0xf;

                      for (mask = 0;mask < 4;mask++)
                        {	   // Each Timeslice has a 20-bit data word.
                          Nibble = TimeSlice * 4 + mask;         // Thiso data word actually  contains four 5-bit
                          if (Nibble < 0 || Nibble >= numberOfNibbles)
                            {
                              cerr << "Dch Packet " << ipacketid << " corrupt: Nibble value is over the limits = " << Nibble << endl;
                              continue;
                            }

                          value = (DCMword >> ((mask * 5) % 20)) & 0x1f;

                          // +++++++++++++++++++   leading edge found ++++++++++++++++++++++++++++++ //

                          if ( value&0x10 )
                            {
                              // just store the information
                              // it will be filled if the next edge is trailing and the width is appropriate.
                              edge = 0;
                              time = (Nibble * 16) + (value & 0xf);

                              Parm = arm;                          // memorize coordinates
                              Pside = side;                         // used for check if
                              Pkeystone = keystone; 			// trailing edge is from
                              Ppair = pair;                         // same hit
                              Pchannel = channel;
                              Pedge = edge;
                              ltime = time;                               // save time of the leading edge

                              // Nibbles can be different for both edges - so store this index also
                              ltmpindex = Nibble + numberOfNibbles * (channel + numberOfChannels * (pair +
												    numberOfPairs * (keystone + numberOfKeyStones * (side + numberOfSides * arm))));
                            }

                          // +++++++++++++++++++   trailing edge found ++++++++++++++++++++++++++++++ //

                          if ((value&0x18) == 0x08)
                            {                // edge fill RAW and relational tables

                              if (Pedge == 0)
                                {       // Normal case - previouse edge was leading

                                  ttime = (Nibble * 16) + (value & 0x7) * 2;
                                  width = ttime - ltime;

                                  if (width > 20)
                                    {

                                      // save leading edge
                                      rawcounter++;
                                      DchRawOutv1 *rawOut = new DchRawOutv1();

                                      rawOut->setId(rawcounter);
                                      rawOut->setGlobal(dchAddressObject->getGlobalIndex()->getValue());
                                      rawOut->setArm(harm);
                                      rawOut->setSide(hside);
                                      rawOut->setCell(hcell);
                                      rawOut->setPlane(hplane);
                                      rawOut->setEdge(0);
                                      rawOut->setTime(ltime);

                                      geantID = finder[ltmpindex];
                                      rawID = rawcounter - 1;

                                      if (geantID >= 0)
                                        {
                                          unsigned int index = dchghit->RowCount();
                                          dchghit->set_ghitid(index, geantID);
                                          dchghit->set_rawid(index, rawID);
                                          index++;
                                          dchghit->SetRowCount(index);
                                        }
                                      rawTable->AddRaw(rawOut);
				      delete rawOut;
                                      // save trailing edge
                                      rawcounter++;
                                      rawOut = new DchRawOutv1();

                                      rawOut->setId(rawcounter);
                                      rawOut->setGlobal(dchAddressObject->getGlobalIndex()->getValue());
                                      rawOut->setArm(harm);
                                      rawOut->setSide(hside);
                                      rawOut->setCell(hcell);
                                      rawOut->setPlane(hplane);
                                      rawOut->setEdge(1);
                                      rawOut->setTime(ttime);

                                      tmpindex = Nibble + numberOfNibbles * (channel + numberOfChannels * (pair + numberOfPairs * (keystone + numberOfKeyStones * (side + numberOfSides * arm))));

                                      geantID = finder[tmpindex];
                                      rawID = rawcounter - 1;

                                      if (geantID >= 0)
                                        {
                                          unsigned int index = dchghit->RowCount();
                                          dchghit->set_ghitid(index, geantID);
                                          dchghit->set_rawid(index, rawID);
                                          index++;
                                          dchghit->SetRowCount(index);
                                        }
                                      rawTable->AddRaw(rawOut);
				      delete rawOut;
                                    }
                                }

                              // 	      if ( Pedge ==  1 )  { /* Abnormal case of two trailing edges in a row */   }
                              //            if ( Pedge == -1 )  { /* Abnormal case of first trailing edges in a row */ }

                              Parm = arm;                          // memorize channel information for the next cycle
                              Pside = side;
                              Pkeystone = keystone;
                              Ppair = pair;
                              Pchannel = channel;
                              Pedge = 1;

                            }
                        }
                    }
                }
            }
          delete p;
        }
    }

  int iret = fillWrapperTables(topNode);

  return iret;
}

int
mNewDchUnpacker::fillWrapperTables(PHCompositeNode *topNode)
{
  dDchRawWrapper *rawWrapper = findNode::getClass<dDchRawWrapper>(topNode, "dDchRaw");
  DchRawTable *rawTable = findNode::getClass<DchRawTable>(topNode, "DchRawTablev1");
  if (rawTable->Entries() > (int) rawWrapper->MaxRowCount())
    {
      cout << PHWHERE << " Number of Dch Raw hits " << rawTable->Entries()
	   << " exceeds number of rows in staf table: " << rawWrapper->MaxRowCount()
	   << " aborting Event" << endl;
      return -2;
    }
  for (int i = 0; i < rawTable->Entries(); i++)
    {
      DchRawOutv1* raw = rawTable->getRaw(i);
      rawWrapper->set_id(i, raw->getId());
      rawWrapper->set_global(i, raw->getGlobal());
      rawWrapper->set_arm(i, raw->getArm());
      rawWrapper->set_plane(i, raw->getPlane());
      rawWrapper->set_cell(i, raw->getCell());
      rawWrapper->set_side(i, raw->getSide());
      rawWrapper->set_edge(i, raw->getEdge());
      rawWrapper->set_time(i, raw->getTime());

    }
  rawWrapper->SetRowCount(rawTable->Entries());
  return 0;
}

