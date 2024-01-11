#include <PHObject.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbErtDecode.hh>
#include <PdbCalBank.hh>

#include <getClass.h>

#include <EMCalRichDecode.h>
#include <dErtFemDataWrapper.h>

#include <cstdlib>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>

using namespace std;

typedef PHIODataNode<dErtFemDataWrapper> dErtFemDataNode_t;

EMCalRichDecode::EMCalRichDecode(const int runnumber)
{
  unsigned int ishift = 1;
  for (int i = 0; i < 16; i++)
    {
      bitMask[i] = ishift;
      ishift <<= 1;
    }


  //.. SM bit position

  for (int i = 0; i < NSM; i++)
    {
      SMBitPosPBSC[i] = -1;
      SMBitPosPBGL[i] = -1;
      SMBitPosRICH[i] = -1;
    }

  //..  read ErtDecode DB ...

  PdbBankManager *bankManager = PdbBankManager::instance();

  PdbBankID bankID(0);
  const char *calibname = "calib.ErtBLT.map";

  PdbErtDecode *ertdbdecode;

  PdbCalBank *ertBank = bankManager->fetchBank("PdbErtDecodeBank", bankID, calibname, runnumber);

  if (!ertBank)
    {
      cout << "failed to get calib.ErtBLT.map bank from DB" << endl;
      exit(1);
    }
  ertBank->printHeader();

  ertBank->print();
  ertdbdecode = (PdbErtDecode*) & (ertBank->getEntry(0));

  //... 4x4A ..
  for (int k = 0; k < 2; k++)
    {
      for (int j = 0; j < 4; j++)
        {
          word4x4ACoord[j][k][0] = ertdbdecode->get_word4x4ACoord_ROC(j, k);
          word4x4ACoord[j][k][1] = ertdbdecode->get_word4x4ACoord_WORD(j, k);
        }
    }
  //... 4x4B ..
  for (int k = 0; k < 2; k++)
    {
      for (int j = 0; j < 4; j++)
        {
          word4x4BCoord[j][k][0] = ertdbdecode->get_word4x4BCoord_ROC(j, k);
          word4x4BCoord[j][k][1] = ertdbdecode->get_word4x4BCoord_WORD(j, k);
        }
    }
  //... 4x4C ..
  for (int k = 0; k < 2; k++)
    {
      for (int j = 0; j < 4; j++)
        {
          word4x4CCoord[j][k][0] = ertdbdecode->get_word4x4CCoord_ROC(j, k);
          word4x4CCoord[j][k][1] = ertdbdecode->get_word4x4CCoord_WORD(j, k);
        }
    }
  //..  2x2 ....
  for (int k = 0; k < 2; k++)
    {
      for (int j = 0; j < 4; j++)
        {
          word2x2Coord[j][k][0] = ertdbdecode->get_word2x2Coord_ROC(j, k);
          word2x2Coord[j][k][1] = ertdbdecode->get_word2x2Coord_WORD(j, k);
        }
    }
  //..  RICH ....
  for (int k = 0; k < 2; k++)
    {
      for (int j = 0; j < 4; j++)
        {
          word4x5CoordRICH[j][k][0] = ertdbdecode->get_word4x5Coord_ROC(j, k);
          word4x5CoordRICH[j][k][1] = ertdbdecode->get_word4x5Coord_WORD(j, k);
        }
    }
  for (int i = 0; i < 16; i++)
    {
      SMCoordModeAPBGL[i] = ertdbdecode->get_SMCoordModeAPBGL(i);
    }
  for (int i = 0; i < 16; i++)
    {
      SMCoordModeBPBGL[i] = ertdbdecode->get_SMCoordModeBPBGL(i);
    }
  for (int i = 0; i < 16; i++)
    {
      SMCoordModeAPBSC[i] = ertdbdecode->get_SMCoordModeAPBSC(i);
    }
  for (int i = 0; i < 16; i++)
    {
      SMCoordModeBPBSC[i] = ertdbdecode->get_SMCoordModeBPBSC(i);
    }
  for (int i = 0; i < 16; i++)
    {
      SMCoordModeARICH[i] = ertdbdecode->get_SMCoordModeARICH(i);
    }
  for (int i = 0; i < 16; i++)
    {
      SMCoordModeBRICH[i] = ertdbdecode->get_SMCoordModeBRICH(i);
    }
  delete ertBank;
  for (int i = 0; i < NBIT; i++)
    {
      if (SMCoordModeAPBSC[i] != -1)
        {
          SMBitPosPBSC[SMCoordModeAPBSC[i]] = i;
        }
      SMBitPosPBGL[SMCoordModeAPBGL[i]] = i;
      SMBitPosRICH[SMCoordModeARICH[i]] = i;

      if (SMCoordModeBPBSC[i] != -1)
        {
          SMBitPosPBSC[SMCoordModeBPBSC[i]] = i;
        }
      SMBitPosPBGL[SMCoordModeBPBGL[i]] = i;
      SMBitPosRICH[SMCoordModeBRICH[i]] = i;
    }
}


//________________________________
void EMCalRichDecode::Reset()
{
  memset(Bit4x4AHalfSectEMC, 0, sizeof(Bit4x4AHalfSectEMC));
  memset(Bit4x4BHalfSectEMC, 0, sizeof(Bit4x4BHalfSectEMC));
  memset(Bit4x4CHalfSectEMC, 0, sizeof(Bit4x4CHalfSectEMC));
  memset(Bit2x2HalfSectEMC, 0, sizeof(Bit2x2HalfSectEMC));
  memset(Bit4x5HalfSectRICH, 0, sizeof(Bit4x5HalfSectRICH));

  memset(Bit4x4ASMEMC, 0, sizeof(Bit4x4ASMEMC));
  memset(Bit4x4BSMEMC, 0, sizeof(Bit4x4BSMEMC));
  memset(Bit4x4CSMEMC, 0, sizeof(Bit4x4CSMEMC));
  memset(Bit2x2SMEMC, 0, sizeof(Bit2x2SMEMC));

  memset(Bit4x5SMRICH, 0, sizeof(Bit4x5SMRICH));

  BitPi0Low = 0, BitPi0High = 0, BitPHI = 0;
  BitSingleElectron = 0, BitChargedHadron = 0;

  arm4x4A = -1, sector4x4A = -1, side4x4A = -1;
  arm4x4B = -1, sector4x4B = -1, side4x4B = -1;
  arm4x4C = -1, sector4x4C = -1, side4x4C = -1;
  arm2x2 = -1,  sector2x2 = -1,  side2x2 = -1;
  earm = -1,    esector = -1,    eside = -1;
  phiarm1 = -1, phisector1 = -1, phiside1 = -1;
  phiarm2 = -1, phisector2 = -1, phiside2 = -1;
  memset(PacketData, 0, sizeof(PacketData));
}
//________________________________
void EMCalRichDecode::SetPacketData(int packetID, int roc, int word, long value)
{

  int arm;
  if (packetID == EASTPACKETID)
    {
      arm  = 1;
    }
  else if (packetID == WESTPACKETID)
    {
      arm  = 0;
    }
  else
    {
      std::cout << "**  unknown packets " << endl;
      return;
    }

  PacketData[arm][roc][word] = value;
}

//________________________________
void EMCalRichDecode::Calculate()
{
  int *emcsmcoord, *richsmcoord;

  for (int arm = 0; arm < NARM; arm++)
    {
      for (int sector = 0; sector < NSECTOR; sector++)
        {
          for (int side = 0; side < NSIDE; side++)
            {

              if (arm == 1)
                {
                  if (sector == 0 || sector == 1)  //__PBGL
                    {
                      emcsmcoord = side ? SMCoordModeBPBGL : SMCoordModeAPBGL;
                    }
                  else
                    {
                      emcsmcoord = side ? SMCoordModeBPBSC : SMCoordModeAPBSC;
                    }
                  richsmcoord = side ? SMCoordModeBRICH : SMCoordModeARICH;
                }
              else
                {
                  emcsmcoord = side ? SMCoordModeAPBSC : SMCoordModeBPBSC;
                  richsmcoord = side ? SMCoordModeARICH : SMCoordModeBRICH;
                }

              int roc1 = word4x4ACoord[sector][side][0];
              int word1 = word4x4ACoord[sector][side][1];
              int roc2 = word4x4BCoord[sector][side][0];
              int word2 = word4x4BCoord[sector][side][1];
              int roc3 = word4x4CCoord[sector][side][0];
              int word3 = word4x4CCoord[sector][side][1];
              int roc4 = word2x2Coord[sector][side][0];
              int word4 = word2x2Coord[sector][side][1];
              int roc5 = word4x5CoordRICH[sector][side][0];
              int word5 = word4x5CoordRICH[sector][side][1];

              for (int bit = 0; bit < 16; bit++)
                {
                  //...  4x4A ...
                  FillRawBit(arm, sector, side,  roc1, word1, bit, emcsmcoord, 0);

                  //...  4x4B ...
                  FillRawBit(arm, sector, side,  roc2, word2, bit, emcsmcoord, 1);

                  //...  4x4C ...
                  FillRawBit(arm, sector, side,  roc3, word3, bit, emcsmcoord, 2);

                  //...  2x2 ...
                  FillRawBit(arm,  sector, side, roc4, word4, bit, emcsmcoord, 3);

                  //...  4x5 RICH ...
                  FillRawBit(arm,  sector, side, roc5, word5, bit, richsmcoord, 4);
                }
            }
        }
    }
  FillPHITriggerBit();
  FillElectronTriggerBit();
}

//___________________________________
void EMCalRichDecode::FillRawBit(int arm, int sector, int side, int roc, int word, int bit, int* smcoord, int tileType)
{
  if ((PacketData[arm][roc][word] & bitMask[bit]) == bitMask[bit])
    {

      int sm = smcoord[bit];
      if (sm == -1)
        {
          return;  //__ non-existing SM
        }

      if (tileType == 0)   //__ 4x4A __
        {

          Bit4x4AHalfSectEMC[arm][sector][side] = 1;
          Bit4x4ASMEMC[arm][sector][sm] = 1;

          BitPi0Low = 1;  //.. fill the trigger bits by the way

          arm4x4A =  arm;
          sector4x4A = sector;
          side4x4A = side;

        }
      else if (tileType == 1)   //__ 4x4B __
        {

          Bit4x4BHalfSectEMC[arm][sector][side] = 1;
          Bit4x4BSMEMC[arm][sector][sm] = 1;

          BitPi0High = 1;  //.. fill the trigger bits by the way

          arm4x4B = arm;
          sector4x4B = sector;
          side4x4B = side;

        }
      else if (tileType == 2)   //__ 4x4C __
        {

          Bit4x4CHalfSectEMC[arm][sector][side] = 1;
          Bit4x4CSMEMC[arm][sector][sm] = 1;

          arm4x4C = arm;
          sector4x4C = sector;
          side4x4C = side;

        }
      else if (tileType == 3)   //__ 2x2 __
        {

          Bit2x2HalfSectEMC[arm][sector][side] = 1;
          Bit2x2SMEMC[arm][sector][sm] = 1;

          BitChargedHadron = 1;  //.. fill the trigger bits by the way

          arm2x2 = arm;
          sector2x2 = sector;
          side2x2 = side;

        }
      else if (tileType == 4)   //__  RICH 4x5 __
        {

          Bit4x5HalfSectRICH[arm][sector][side] = 1;
          Bit4x5SMRICH[arm][sector][sm] = 1;

        }
    }
}

//_________________________________________
void  EMCalRichDecode::FillPHITriggerBit()
{
  int ncount = 0;
  for (int arm = 0; arm < NARM; arm++)
    {
      for (int sector = 0; sector < NSECTOR; sector++)
        {
          for (int side = 0; side < NSIDE; side++)
            {
              if (Bit4x5HalfSectRICH[arm][sector][side] == 1)
                {

                  if (!ncount)
                    {
                      ncount++;
                    }
                  if (ncount == 1 && arm != phiarm1 && phiarm1 != -1)
                    {
                      ncount++;
                    }
                  if (ncount == 1)
                    {
                      phiarm1 = arm;
                      phisector1 = sector;
                      phiside1 = side;
                    }
                  if (ncount == 2)
                    {
                      phiarm2 = arm;
                      phisector2 = sector;
                      phiside2 = side;

                      BitPHI = 1;

                      return;
                    }
                }
            }
        }
    }

  //---------------------------------------------
  // if ncount==1 in this event, the following
  // 3 variable are not zero but you want them 0
  //  since the trigger is not fired.
  //
  phiarm1 = -1;
  phisector1 = -1;
  phiside1 = -1;
}
//___________________________________
void  EMCalRichDecode::FillElectronTriggerBit()
{ // half sector trigger tile

  for (int arm = 0; arm < NARM; arm++)
    {
      for (int sector = 0; sector < NSECTOR; sector++)
        {
          for (int side = 0; side < NSIDE; side++)
            {
              if (Bit2x2HalfSectEMC[arm][sector][side] == 1 &&
                  Bit4x5HalfSectRICH[arm][sector][side] == 1)
                {

                  BitSingleElectron = 1;

                  earm = arm;
                  esector = sector;
                  eside = side;

                  break;
                }
            }
        }
    }
}
//________________________________
bool  EMCalRichDecode::GetRawBitPi0Low(int& arm, int& sector, int& side)
{
  if (arm4x4A != -1)
    {
      arm = arm4x4A;
      sector =  sector4x4A;
      side = side4x4A;

      return true;

    }
  return false;
}
//________________________________
bool  EMCalRichDecode::GetRawBitPi0High(int& arm, int& sector, int& side)
{
  if (arm4x4B != -1)
    {
      arm = arm4x4B;
      sector =  sector4x4B;
      side = side4x4B;

      return true;

    }
  return false;
}

//________________________________
bool  EMCalRichDecode::GetRawBitPHI(int& arm1, int& sector1, int& side1,
                                    int& arm2, int& sector2, int& side2)
{
  if (phiarm1 != -1)
    {
      arm1 = phiarm1;
      sector1 = phisector1;
      side1 = phiside1;

      arm2 = phiarm2;
      sector2 = phisector2;
      side2 = phiside2;

      return true;

    }
  return false;
}
//________________________________
bool  EMCalRichDecode::GetRawBitSingleElectron(int& arm, int& sector, int& side)
{
  if (earm != -1)
    {
      arm = earm;
      sector = esector;
      side = eside;

      return true;
    }
  return false;
}
//________________________________
bool  EMCalRichDecode::GetRawBitChargedHadron(int& arm, int& sector, int& side)
{
  if (arm2x2 != -1)
    {
      arm = arm2x2;
      sector =  sector2x2;
      side = side2x2;

      return true;
    }
  return false;
}
//________________________________
int  EMCalRichDecode::GetPacket(int arm)
{
  if (arm == 1)
    {
      return 14201; //__ east arm
    }
  if (arm == 0)
    {
      return 14200; //__ west arm
    }
  return -1;
}

//________________________________
void  EMCalRichDecode::GetBitPosition(int arm, int sector, int sm, int& bitEMC, int& bitRICH)
{
  int* SMBitPos;

  if (arm == 1 && (sector == 0 || sector == 1))
    {
      SMBitPos = SMBitPosPBGL;
    }
  else
    {
      SMBitPos = SMBitPosPBSC;
    }

  bitEMC = SMBitPos[sm];
  bitRICH = SMBitPosRICH[sm];
}

//________________________________
void  EMCalRichDecode::GetRocWord(int arm, int sector, int trgType, int sm, int& roc, int& word)
{
  //--------------------------------
  //  determine the side of the SM
  //
  int sideEMC, sideRICH;

  if (arm == 1)   //__ PBSC
    {
      if ((sm / 3) % 2 == 0)
        {
          sideEMC = 0;  //__ south
        }
      else
        {
          sideEMC = 1;  //__ north
        }
    }
  else
    {
      if ((sm / 3) % 2 == 0)
        {
          sideEMC = 1;  //__ north
        }
      else
        {
          sideEMC = 0;  //__ south
        }
    }

  if (arm == 1 && (sector == 0 || sector == 1))   //__PBGL
    {

      if ((sm / 4) % 2 == 0)
        {
          sideEMC = 0;  //__ south
        }
      else
        {
          sideEMC = 1;  //__ south
        }
    }

  if (arm == 1)   //__ RICH
    {
      if ((sm / 4) % 2 == 0)
        {
          sideRICH = 0;  //__ south
        }
      else
        {
          sideRICH = 1;  //__ north
        }
    }
  else
    {
      if ((sm / 4) % 2 == 0)
        {
          sideRICH = 1;  //__ north
        }
      else
        {
          sideRICH = 0;  //__ south
        }
    }

  if (trgType == 0)   //__ 4x4A
    {
      roc = word4x4ACoord[sector][sideEMC][0];
      word = word4x4ACoord[sector][sideEMC][1];
    }

  else if (trgType == 1)   //__ 4x4B
    {
      roc = word4x4BCoord[sector][sideEMC][0];
      word = word4x4BCoord[sector][sideEMC][1];
    }

  else if (trgType == 2)   //__ 4x4C
    {
      roc = word4x4CCoord[sector][sideEMC][0];
      word = word4x4CCoord[sector][sideEMC][1];
    }

  else if (trgType == 3)   //__ 2x2
    {
      roc = word2x2Coord[sector][sideEMC][0];
      word = word2x2Coord[sector][sideEMC][1];
    }

  else if (trgType == 4)   //__ 4x5 RICH
    {
      roc = word4x5CoordRICH[sector][sideRICH][0];
      word = word4x5CoordRICH[sector][sideRICH][1];
    }
}
//________________________________
void  EMCalRichDecode::GetRocWordBitPacket(int arm, int sector, int trgType, int sm, int& roc, int& word, int& packet, int& bitEMC, int& bitRICH)
{
  GetBitPosition(arm, sector, sm, bitEMC, bitRICH);
  packet = GetPacket(arm);
  GetRocWord(arm, sector, trgType, sm, roc, word);
}




int EMCalRichDecode::Decode(PHCompositeNode *topNode, int simulationFlag)
{
  Reset();
  if (!simulationFlag) // from PRDF to DST
    {
      Event* event = findNode::getClass<Event>(topNode, "PRDF");
      if (!event)
        {
          cout << "ErtDecode  PRDF node is empty!" << endl;
          return 1;
        }


      // Get the relevant packets from the Event object and transfer the
      // data to the subsystem-specific table.
      Packet* p;
      static const int id_base = 14200;
      static const int max_packets = 2;
      int id;
      for (int k = 0; k < max_packets; k++)
        {
          id = id_base + k;
          if ( (p = event->getPacket(id)) != 0)
            {
              for (int iroc = 0; iroc < 20; iroc++)
                {
                  for (int iword = 0; iword < 6; iword++)
                    {
                      long tmp_word = p->iValue(iword, iroc);
                      SetPacketData(id, iroc, iword, tmp_word);
                    }
                }
            }
          delete p;   // event->getPacket creates a packet object on the
          // heap, so delete it here!
        }
    }
  else   // from PISA to DST
    {

      dErtFemDataWrapper* dErtFemData = findNode::getClass<dErtFemDataWrapper>(topNode, "dErtFemData");

      if (!dErtFemData)
        {
          cout << " EMCalRichDecode: could not find table dErtFemData" << endl;
        }

      int id, iroc, iword;
      long tmp_word;
      for (unsigned int i = 0; i < dErtFemData->RowCount(); i++)
        {
          if (dErtFemData->get_crate(i) == 0)
            {
              id = 14200;
            }
          else
            {
              id = 14201;
            }

          iroc = dErtFemData->get_Roc(i);
          iword = dErtFemData->get_word(i);
          tmp_word = dErtFemData->get_Value(i);

          SetPacketData(id, iroc, iword, tmp_word);
        }
    }

  Calculate();

  return 0;

}


void EMCalRichDecode::DstStore(PHCompositeNode * root)
{
  ErtOut *ertout = findNode::getClass<ErtOut>(root, "ErtOut");
  if (ertout)
    {
      ertout->Clear();
      int arm, sector, sm;

      for (arm = 0; arm < 2; arm++)
        {
          for (sector = 0; sector < 4; sector++)
            {
              for (sm = 0; sm < 32; sm++)
                {
                  if (GetBit4x4ASMEMC(arm, sector, sm))
                    {
                      ertout->set_ERTbit(0, arm, sector, sm);
                    }
                  if (GetBit4x4BSMEMC(arm, sector, sm))
                    {
                      ertout->set_ERTbit(1, arm, sector, sm);
                    }
                  if (GetBit4x4CSMEMC(arm, sector, sm))
                    {
                      ertout->set_ERTbit(2, arm, sector, sm);
                    }
                  if (GetBit2x2SMEMC(arm, sector, sm))
                    {
                      ertout->set_ERTbit(3, arm, sector, sm);
                    }
                  if (GetBit4x5SMRICH(arm, sector, sm))
                    {
                      ertout->set_ERTbit(4, arm, sector, sm);
                    }
                }
            }
        }
    }
}

//_______________________________________
int EMCalRichDecode::GetRichBitPos(int arm, int sect, int side, int sm)
{
  int *richsmcoord;
  if (arm == 1)
    {
      richsmcoord = side ? SMCoordModeBRICH : SMCoordModeARICH;
    }
  else
    {
      richsmcoord = side ? SMCoordModeARICH : SMCoordModeBRICH;
    }

  for (int bit = 0; bit < 16; bit++)
    {
      if (sm == richsmcoord[bit])
        {
          return bit;
        }
    }
  cout << "This is not good, could not find bit " << sm << endl;
  exit(1);
}
