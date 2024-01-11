// Implementation of class file: PHDchAddressObject.h                       
//                                                                
// Created by: Federica Ceretto at Wed Feb 17 15:21:39 1999                                      
//                                                                
// Description: The global index is calculate as looping in the order over all CELLS, PLANES,ARMS and SIDES  

#include <PHDchAddressObject.h> 
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <DchSubs_DCM.h>

#include <iostream>

using namespace std;

PHDchAddressObject::PHDchAddressObject()
{
  flagMC = 1;

  arm = new PdbIndex(0, 1, 0, "ARM");
  side = new PdbIndex(0, 1, 0, "SIDE");
  key = new PdbIndex(0, 19, 0, "KEYSTONE");
  plane = new PdbIndex(0, 39, 0, "PLANE");
  cell = new PdbIndex(0, 79, 0, "CELL");
  pair = new PdbIndex(0, 1, 0, "PAIR");
  board = new PdbIndex(0, 79, 0, "BOARD");
  bchan = new PdbIndex(1, 40, 1, "BCHAN");
  channel = new PdbIndex(0, 79, 0, "CHANNEL");

  int tmpMaxIndex = (arm->getMax() + 1) * (side->getMax() + 1) * (plane->getMax() + 1) * (cell->getMax() + 1) - 1;


  index = new PdbIndex(0, tmpMaxIndex, 0, "GLOBAL");


  DetectorIndex.append(arm);
  DetectorIndex.append(side);
  DetectorIndex.append(key);
  DetectorIndex.append(plane);
  DetectorIndex.append(cell);
  DetectorIndex.append(pair);
  DetectorIndex.append(board);
  DetectorIndex.append(bchan);
  DetectorIndex.append(channel);

}

PHDchAddressObject::PHDchAddressObject(short flag)
{
  flagMC = flag;
}


PHDchAddressObject::~PHDchAddressObject()
{  
  DetectorIndex.clearAndDestroy();
  if (index) 
    {
      delete index;
    }
  commit();
}

void PHDchAddressObject::setSoft(int iarm, int iside, int iplane, int icell)
{
  //----------------------------------------
  // set in one go all the Software Indices
  //----------------------------------------
  arm->setValue(iarm);  
  side->setValue(iside);
  plane->setValue(iplane);
  cell->setValue(icell);

  //----------------------------------------
  // Upgrade the Hardware indices
  //----------------------------------------

    fromSoftToHard();
}

void 
PHDchAddressObject::decodePacketID(int packetId)
{
  //----------------------------------------
  // set in one go all the Software Indices
  //----------------------------------------

  // The subsystem ID as defined by J.Nagle at
  // http://www.nevis.columbia.edu/~nagle/PHENIX/Data_Formats/PHENIX_Formats/header_words_pub.htm

  // Encoding relation:
  // packetId=Dch_ID_DCM*1000+80*arm+40*side+2*keystone+pair+1;
  int p = packetId - Dch_ID_DCM * 1000 + 1;
  int harm =   p / 80;
  int hside = (p - 80 * harm) / 40;
  int hkey =  (p - 80 * harm - 40 * hside) / 2;
  int hpair =  p - 80 * harm - 40 * hside - 2 * hkey;

  arm->setValue(harm);
  side->setValue(hside);
  key->setValue(hkey);
  pair->setValue(hpair);

}

void PHDchAddressObject::setHard(int iarm, int iside, int ikey,   int ipair, int ichannel)
{

  //----------------------------------------
  // set in one go all the Hardware Indices
  //----------------------------------------
  arm->setValue(iarm);
  side->setValue(iside);
  key->setValue(ikey);
  pair->setValue(ipair);
  channel->setValue(ichannel);

  //----------------------------------------
  // Upgrade the Software indices
  //----------------------------------------
  fromHardToSoft();

}



PHBoolean PHDchAddressObject::setGlobalIndex(PdbIndex* ind)
{
  //------------------------------------------------------------------------
  // setting only the software indices: arm, cell, plane but not the side
  //------------------------------------------------------------------------

  int ii = ind->getValue();
  return setGlobalIndex(ii);
}


PHBoolean PHDchAddressObject::setGlobalIndex(int ind)
{
  //------------------------------------------------------------------------
  // setting only the software indices: arm, cell, plane but not the side
  //------------------------------------------------------------------------

  int tmpValue;
  cell->setValue((ind%cell->getNumberOf()));
  
  tmpValue = ind/cell->getNumberOf();
  plane->setValue(tmpValue%plane->getNumberOf());

  tmpValue = tmpValue/plane->getNumberOf();
  arm->setValue(tmpValue%arm->getNumberOf());

  tmpValue = tmpValue/arm->getNumberOf();
  side->setValue(tmpValue%side->getNumberOf());

  key->setValue(cell->getValue()/4);
  return True;
}

PdbIndex*
PHDchAddressObject::getDetectorIndex(int ind , PdbIndex* global)
{
  // From global index to local one --> the software indices : only
  // arm,plane,cell,side can be calculated
  setGlobalIndex(global);
  switch (ind)
    {
    case ARM:
      return arm;
    case PLANE:
      return plane;
    case CELL:
      return cell;
    case SIDE:
      return side;
    default:
      cout << PHWHERE << "Can not be calculated!" << endl;
      return 0;
    }
}

PHBoolean
PHDchAddressObject::setDetectorIndex(int ind, PdbIndex* local)
{
  // Setting a particular index to a certain value/range: Since the
  // function does not update all the index together, for setting a
  // value is strongly suggested to use functions which set in one go
  // all the index together This function is instead useful ONLY to
  // set the range of one index

  switch (ind)
    {
    case ARM:
      arm = local;
      break;
    case PLANE:
      plane = local;
      break;
    case CELL:
      cell = local;
      break;
    case KEYSTONE:
      key = local;
      break;
    case SIDE:
      side = local;
      break;
    case PAIR:
      pair = local;
      break;
    case BOARD:
      board = local;
      break;
    case BCHAN:
      bchan = local;
      break;
    case CHANNEL:
      channel = local;
      break;
    default:
      cout << PHWHERE << "Invalid index!" << endl;
      return False;
    }

  return True;
}

void PHDchAddressObject::initialize()
{
  // This function will be substitute later when the access to the
  // database is ready !  The values will be read from the database
  DetectorIndex.clearAndDestroy();


  if (index) delete index;

  arm = new PdbIndex(0, 1, 0, "ARM");
  side = new PdbIndex(0, 1, 0, "SIDE");
  key = new PdbIndex(0, 19, 0, "KEYSTONE");
  plane = new PdbIndex(0, 39, 0, "PLANE");
  cell = new PdbIndex(0, 79, 0, "CELL");
  pair = new PdbIndex(0, 1, 0, "PAIR");
  board = new PdbIndex(0, 79, 0, "BOARD");
  bchan = new PdbIndex(1, 40, 1, "BCHAN");
  channel = new PdbIndex(0, 79, 0, "CHANNEL");

  int tmpMaxIndex = (arm->getMax() + 1) * (side->getMax() + 1) * (plane->getMax() + 1) * (cell->getMax() + 1) - 1;

  
  index = new PdbIndex(0, tmpMaxIndex, 0, "GLOBAL");

  DetectorIndex.append(arm);
  DetectorIndex.append(side);
  DetectorIndex.append(key);
  DetectorIndex.append(plane);
  DetectorIndex.append(cell);
  DetectorIndex.append(pair);
  DetectorIndex.append(board);
  DetectorIndex.append(bchan);
  DetectorIndex.append(channel);

}

void PHDchAddressObject::fromSoftToHard()
{
  // How to get from Software indices to Hardware ones
  int maxPlane = plane->getMax();
  bchan->setValue(maxPlane + 1 - plane->getValue());
  if ( ((arm->getValue() == (int) WEST) && (side->getValue() == (int) SOUTH)) ||     // In these regions the boards
       ((arm->getValue() == (int) EAST) && (side->getValue() == (int) NORTH)) )    // number smoothly
    {
      board->setValue(cell->getValue()); // for x1 and x2
      if ((plane->getValue() >= numberOfXPlanes && plane->getValue() < halfNumberOfPlanes) ||
          (plane->getValue() >= halfNumberOfPlanes + numberOfXPlanes && plane->getValue() < numberOfPlanes))
        {
          board->setValue(cell ->getValue() - 2); // for u1 and u2
        }
      if ((plane->getValue() >= numberOfXPlanes + numberOfUorVPlanes && plane->getValue() < halfNumberOfPlanes ) ||
          (plane->getValue() >= numberOfPlanes - numberOfUorVPlanes))
        {
          board->setValue(cell ->getValue() + 2);  // for v1 and v2
        }
      key->setValue(board->getValue() / 4);
      pair->setValue((board->getValue() - 4*key->getValue()) / 2);
      channel->setValue((maxPlane - plane->getValue())*2 + board->getValue() - (4*key->getValue() + 2*pair->getValue()));
    }
  else
    {
      board->setValue(cell->getValue()); // for x1 and x2
      if ((plane->getValue() >= numberOfXPlanes && plane->getValue() < halfNumberOfPlanes) ||
          (plane->getValue() >= halfNumberOfPlanes + numberOfXPlanes && plane->getValue() < numberOfPlanes))
        {
          board->setValue((cell->getValue() + 2)); // for u1 and u2
        }
      if ((plane->getValue() >= numberOfXPlanes + numberOfUorVPlanes && plane->getValue() < halfNumberOfPlanes ) ||
          (plane->getValue() >= numberOfPlanes - numberOfUorVPlanes))
        {
          board->setValue(cell->getValue() - 2);  // for v1 and v2
        }
      key->setValue(board->getValue() / 4);
      pair->setValue(1 - (board->getValue() - 4*key->getValue()) / 2);
      channel->setValue((maxPlane - plane->getValue())*2 + (1 - (board->getValue() - (4*key->getValue() + 2*(1 - pair->getValue())))));
    }

  //---------------------------
  //  Calculate the global index
  //---------------------------
  index->setValue(cell->getValue()
                  + plane->getValue()*(cell->getNumberOf())
                  + arm->getValue()*(plane->getNumberOf())*(cell->getNumberOf())
                  + side->getValue()*(arm->getNumberOf())*(plane->getNumberOf())*(cell->getNumberOf()));
}

void PHDchAddressObject::fromHardToSoft()
{
  // How to get from Hardware indices to software ones
  int maxPlane = plane->getMax();
  plane->setValue(maxPlane - channel->getValue() / 2);
  int iplane = plane->getValue();
  bchan->setValue(maxPlane + 1 - plane->getValue());   // count 1-40 and in the opposite direction

  if ( ((arm->getValue() == (int) WEST) && (side->getValue() == (int) SOUTH)) ||     // In these regions the boards
       ((arm->getValue() == (int) EAST) && (side->getValue() == (int) NORTH)) )    // number smoothly
    {

      board->setValue(key->getValue()*4 + pair->getValue()*2 + channel->getValue() % 2);
      cell->setValue(board->getValue());   // for x1 and x2
      if ((iplane >= numberOfXPlanes && iplane < halfNumberOfPlanes) ||
          (iplane >= halfNumberOfPlanes + numberOfXPlanes && iplane < numberOfPlanes))
        {
          cell->setValue(board->getValue() + 2); // for u1 and u2
        }
      if ((iplane >= numberOfXPlanes + numberOfUorVPlanes && iplane < halfNumberOfPlanes ) ||
          (iplane >= numberOfPlanes - numberOfUorVPlanes))
        {
          cell->setValue(board->getValue() - 2); // for v1 and v2
        }
    }
  else
    {

      board->setValue(key->getValue()*4 + (3 - pair->getValue()*2 - channel->getValue() % 2));
      cell->setValue(board->getValue()); // for x1 and x2
      if ((iplane >= numberOfXPlanes && iplane < halfNumberOfPlanes) ||
          (iplane >= halfNumberOfPlanes + numberOfXPlanes && iplane < numberOfPlanes))
        {
          cell->setValue(board->getValue() - 2); // for u1 and u2
        }
      if ((iplane >= numberOfXPlanes + numberOfUorVPlanes && iplane < halfNumberOfPlanes ) ||
          (iplane >= numberOfPlanes - numberOfUorVPlanes))
        {
          cell->setValue(board->getValue() + 2);  // for v1 and v2

        }
    }

  //---------------------------
  //  Calculate the global index
  //---------------------------



  index->setValue(cell->getValue()
                  + plane->getValue()*(cell->getNumberOf())
                  + arm->getValue()*(plane->getNumberOf())*(cell->getNumberOf())
                  + side->getValue()*(arm->getNumberOf())*(plane->getNumberOf())*(cell->getNumberOf()));
}

PHBoolean PHDchAddressObject::update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                                     const char *calibname, PdbBankID bankID, const char *descrip )
{
  if (committed == 1)
    {
      if (!application->startUpdate())
        {
          PHMessage("PHDchAddressObject", PHError, "Aborting ... Database not writable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }

  addressBank = bankManager->createBank("PdbIndexBank", bankID, descrip, Tstart, Tstop, calibname);
  int length = getLength();
  addressBank->setLength(length);
  addressBank->print();
  start = Tstart;
  stop = Tstop;


  PdbIndex *pdbindex;
  for (size_t i = 0; i < addressBank->getLength(); i++)
    {
      pdbindex = (PdbIndex*) & addressBank->getEntry(i);
      *pdbindex = *(DetectorIndex[i]);
    }
  return True;
}

PHBoolean 
PHDchAddressObject::updateValidityTimeForLastBank(PHTimeStamp& Tstart, 
						  PHTimeStamp& Tstop,
						  PHTimeStamp& Tsearch, 
						  const char *calibname, 
						  PdbBankID bankID, 
						  int force)
{

  if (!fetch(Tsearch, calibname, bankID))
    return False;
  cout << PHWHERE
       << "start Time: " << start 
       << " stop Time: " << stop << endl;
  if (stop == PHTimeStamp::PHFarFuture || force)
    {
      addressBank->setEndValTime(Tstop);
      addressBank->setStartValTime(Tstart);
      stop = Tstop;
      cout << PHWHERE 
	   << "Final:: start Time: " << start 
	   << " stop Time: " << stop << endl;
    }
  else
    {
      PHMessage("PHDchAddress::updateValidityTimeForLastBank",
		PHWarning, "Tstop not  in Far Future");
    }
  return True;
}

PHBoolean 
PHDchAddressObject::fetch(PHTimeStamp &Tsearch,
			  const char *name,
			  PdbBankID bankID )
{
  if (committed == 1)
    {
      if (!application->startRead())
        {
          PHMessage("PHDchAddressObject", PHError, 
		    "Aborting ... Database not readable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }

  addressBank = bankManager->fetchBank("PdbIndexBank", bankID, name, Tsearch);
  if (!addressBank)
    {
      return False;
    }
  DetectorIndex.clearAndDestroy();
  // addressBank->print();
  start = addressBank->getStartValTime();
  stop = addressBank->getEndValTime();

  PdbIndex *thisIndex;
  PdbIndex *pdbindex;

  for (unsigned int i = 0; i < addressBank->getLength(); i++)
    {
      pdbindex = (PdbIndex*) & addressBank->getEntry(i);
      //  pdbindex->print();
      thisIndex = new PdbIndex();
      *thisIndex = *pdbindex;
      DetectorIndex.append(thisIndex);
    }

  arm = PHAddressObject::getDetectorIndex(0);
  side = PHAddressObject::getDetectorIndex(1);
  key = PHAddressObject::getDetectorIndex(2);
  plane = PHAddressObject::getDetectorIndex(3);
  cell = PHAddressObject::getDetectorIndex(4);
  pair = PHAddressObject::getDetectorIndex(5);
  board = PHAddressObject::getDetectorIndex(6);
  bchan = PHAddressObject::getDetectorIndex(7);
  channel = PHAddressObject::getDetectorIndex(8);

  int tmpMaxIndex = (arm->getMax() + 1) * (side->getMax() + 1) * (plane->getMax() + 1) * (cell->getMax() + 1) - 1 ;
  if (index) 
    {
      delete index;
    }
  index = new PdbIndex(0, tmpMaxIndex, 0, "GLOBAL");
  delete addressBank;
  addressBank = 0;
  return True;

}



