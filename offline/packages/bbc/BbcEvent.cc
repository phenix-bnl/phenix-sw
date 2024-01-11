#include <BbcEvent.hh>

#include <dBbcRawWrapper.h>
#include <dBbcDCMWrapper.h>
#include <dBbcOutWrapper.h>

#include <BbcRaw.h>
#include <BbcOut.h>

#include <getClass.h>
#include <Event.h>
#include <PHTable.hh>
#include <recoConsts.h>

#include <TRandom.h>

#include <gsl/gsl_const.h>

#include <cmath>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// light velocity [cm/ns]
static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;

BbcEvent::BbcEvent(void)
{
  //set default values
  setEventNumber(0);
  for (int i = 0;i < BBC_N_PMT;i++)
    {
      setAdc(0, i);
      setTdc0(0, i);
      setTdc1(0, i);
    }
}
///
void BbcEvent::Clear()
{
  EventNumber = 0;
  ///reset BBC raw data
  for ( int iloop = 0; iloop < BBC_N_PMT; iloop++ )
    {
      Adc[iloop] = 0;
      Tdc0[iloop] = 0;
      Tdc1[iloop] = 0;
      iHit[iloop] = 0;
      armHitPmt[iloop] = 0;
      TrueAdc[iloop] = 0;
      Charge[iloop] = 0;
      HitTime0[iloop] = 0;
      HitTime1[iloop] = 0;
    }
  ///Reset BBC hits information
  for ( int iloop = 0; iloop < 2; iloop++ )
    {
      nHitPmt[iloop] = 0;
      ChargeSum[iloop] = 0;
      ArmHitTime[iloop] = 0;
      ArmHitTimeError[iloop] = 0;
    }
  ///Reset end product to prepare next event
  ZVertex = 0.0;
  ZVertexError = 0.0;
  TimeZero = 0.0;
  TimeZeroError = 0.0;
}
///
PHBoolean
BbcEvent::setRawData(PHCompositeNode* topNode)
{
  // Skip getting raw if going from PISA to DST
  // BbcRaw is already filled by BbcSimReco
  recoConsts* rc = recoConsts::instance();
  int simflag = 0;
  if ( rc->FlagExist("SIMULATIONFLAG") )
    {
      simflag = rc->get_IntFlag("SIMULATIONFLAG");
    }

  if ( simflag == 2 )
    {
      return 0;
    }
  
  Event* event = findNode::getClass<Event>(topNode,"PRDF");
  if (!event)
    {
      cout << PHWHERE << "BbcGetDCM-E5  PRDF node is empty!" << endl;
      return 1;
    }


  dBbcRawWrapper* BbcRawWrapper = findNode::getClass<dBbcRawWrapper>(topNode,"dBbcRaw");
  DBBCRAW_ST* bbcrawwrap = NULL;

  bbcrawwrap = BbcRawWrapper->TableData();

  BbcRaw* bbcraw = findNode::getClass<BbcRaw>(topNode,"BbcRaw");

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.

  // From Run5 two DCM packets are read
  static const int single_reado = 1001; // 1001 for single readout up to Run4
  static const int double_north = 1002; // 1002 for double readout North
  static const int double_south = 1003; // 1003 for double readout South

  Packet *p =0;
  Packet *pn=0;
  Packet *ps=0;

  p  = event->getPacket(single_reado);
  pn = event->getPacket(double_north);
  ps = event->getPacket(double_south);

  int iret=-1;

  // Single readout
  if ((p)!=0) {

    if (bbcrawwrap) {
      DBBCRAW_ST * p1;
      for (int iraw=0; iraw<BBC_N_PMT; iraw++) {
	int fem_board_id = p->iValue((int)(iraw/8), "BOARD");
	int fem_chan_id = iraw % 8 + 1;
	int pmt_number  = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	p1 = bbcrawwrap + pmt_number;
	p1->Pmt  = pmt_number;
	p1->Adc  = p->iValue(iraw);
	p1->Tdc0 = p->iValue(iraw, "T1");
	p1->Tdc1 = p->iValue(iraw, "T2");
      }
      BbcRawWrapper->SetRowCount(BBC_N_PMT);
    }

    if (bbcraw) {
      short int Adc,Tdc0,Tdc1,Pmt;
      for (int iraw=0; iraw<BBC_N_PMT; iraw++ ) {
	int fem_board_id = p->iValue((int)(iraw/8),"BOARD");
	int fem_chan_id  = iraw % 8 + 1;
	Pmt = calib.getPmtNumber( fem_board_id, fem_chan_id )-1;
	Adc = p->iValue(iraw);
	Tdc0= p->iValue(iraw,"T1");
	Tdc1= p->iValue(iraw,"T2");
	bbcraw->AddBbcRawHit(Pmt, Adc,Tdc0,Tdc1,Pmt);
      }
      bbcraw->set_npmt(BBC_N_PMT);
    }

    iret = 0;

  // splitting readout
  } else if ((pn)!=0 && (ps)!=0) {

    if (bbcrawwrap) {
      DBBCRAW_ST * p1;
      for (int iraw=0; iraw<BBC_N_PMT; iraw++) {
        if (iraw<BBC_N_PMT/2) {
	  // North
	  int fem_board_id = pn->iValue((int)(iraw/8), "BOARD");
	  int fem_chan_id  = iraw % 8 + 1;
	  int pmt_number  = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	  p1 = bbcrawwrap + pmt_number;
	  p1->Pmt  = pmt_number;
	  p1->Adc  = pn->iValue(iraw);
	  p1->Tdc0 = pn->iValue(iraw, "T1");
	  p1->Tdc1 = pn->iValue(iraw, "T2");
	} else {
	  // South
	  int indx = iraw - (int)(BBC_N_PMT/2); // Only 64 channels from head have data
	  int fem_board_id = ps->iValue((int)(indx/8), "BOARD");
	  int fem_chan_id  = indx % 8 + 1;
	  int pmt_number   = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	  p1 = bbcrawwrap + pmt_number;
	  p1->Pmt  = pmt_number;
	  p1->Adc  = ps->iValue(indx);
	  p1->Tdc0 = ps->iValue(indx, "T1");
	  p1->Tdc1 = ps->iValue(indx, "T2");
	}
      }
      BbcRawWrapper->SetRowCount(BBC_N_PMT);
    }

    if (bbcraw) {
      short int Adc,Tdc0,Tdc1,Pmt;
      for (int iraw=0; iraw<BBC_N_PMT; iraw++) {
	if (iraw<BBC_N_PMT/2) {
	  // North
	  int fem_board_id = pn->iValue((int)(iraw/8),"BOARD");
	  int fem_chan_id  = iraw % 8 + 1;  
	  Pmt = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	  Adc = pn->iValue(iraw);
	  Tdc0= pn->iValue(iraw,"T1");
	  Tdc1= pn->iValue(iraw,"T2");
	}
        else {
	  // South
	  int indx = iraw - (int)(BBC_N_PMT/2);
	  int fem_board_id = ps->iValue((int)(indx/8),"BOARD");
	  int fem_chan_id  = iraw % 8 + 1;  
	  Pmt = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	  Adc = ps->iValue(indx);
	  Tdc0= ps->iValue(indx,"T1");
	  Tdc1= ps->iValue(indx,"T2");
	}
        bbcraw->AddBbcRawHit(Pmt, Adc,Tdc0,Tdc1,Pmt);
      }
      bbcraw->set_npmt(BBC_N_PMT);
    }

    iret = 0;

  } else {

    if (EventNumber > 0) cout << "BbcEvent> No BBC packets" << endl;

  }

  delete p;
  delete pn;
  delete ps;

  return iret;

}

PHBoolean
BbcEvent::setRawData(Event *event)
{
  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.

  // From Run5 two DCM packets are read
  static const int single_reado = 1001; // 1001 for single readout up to Run4
  static const int double_north = 1002; // 1002 for double readout North
  static const int double_south = 1003; // 1003 for double readout South

  Packet *p =0;
  Packet *pn=0;
  Packet *ps=0;

  p  = event->getPacket(single_reado);
  pn = event->getPacket(double_north);
  ps = event->getPacket(double_south);

  int iret=-1;

  // single readout
  if ((p )!=0) {

    for (int iraw=0; iraw<BBC_N_PMT; iraw++) {
      int fem_board_id = p->iValue((int)(iraw/8), "BOARD");
      int fem_chan_id  = iraw % 8 + 1;
      int pmt_number   = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
      Adc[pmt_number]  = p->iValue(iraw);
      Tdc0[pmt_number] = p->iValue(iraw, "T1");
      Tdc1[pmt_number] = p->iValue(iraw, "T2");
    }

    iret = 0;

  // splitting readout
  } else if ((pn)!=0 && (ps)!=0) {

    for (int iraw=0; iraw<BBC_N_PMT; iraw++) {
      if (iraw < BBC_N_PMT/2) {
	// North
	int fem_board_id = pn->iValue((int)(iraw/8), "BOARD");
	int fem_chan_id  = iraw % 8 + 1;
	int pmt_number   = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	Adc[pmt_number]  = pn->iValue(iraw);
	Tdc0[pmt_number] = pn->iValue(iraw, "T1");
	Tdc1[pmt_number] = pn->iValue(iraw, "T2");
      } else {
	// South
	int indx = iraw - (int)(BBC_N_PMT/2); // Only 64 channels from head have data
	int fem_board_id = ps->iValue((int)(indx/8), "BOARD");
	int fem_chan_id  = indx % 8 + 1;
	int pmt_number   = calib.getPmtNumber( fem_board_id, fem_chan_id ) - 1;
	Adc[pmt_number]  = ps->iValue(indx);
	Tdc0[pmt_number] = ps->iValue(indx, "T1");
	Tdc1[pmt_number] = ps->iValue(indx, "T2");
      }
    }

    iret = 0;

  } else {

    if (EventNumber > 0) cout << "BbcEvent> No BBC packets" << endl;

  }

  delete p ;
  delete pn;
  delete ps;

  return iret;

}

PHBoolean
BbcEvent::DcmToRaw(PHCompositeNode *root)
{

  PHNodeIterator i(root), *j;
  TableNode_t *d;
  PHCompositeNode *bbcNode, *dcmNode, *dstNode, *outNode;

  bbcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "BBC"));
  if (!bbcNode)
    {
      bbcNode = new PHCompositeNode("BBC");
      root->addNode(bbcNode);
    }

  dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
  if (!dcmNode)
    {
      dcmNode = new PHCompositeNode("DCM");
      root->addNode(dcmNode);
    }

  dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      dstNode = new PHCompositeNode("DST");
      root->addNode(dstNode);
    }

  dBbcDCMWrapper* BbcDCMWrapper;

  outNode = dcmNode;
  j = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode", "dBbcDCM"));
  if (!d)
    {
      cout << "ERROR:  'in' parameter dBbcDCM not found" << endl;
      BbcDCMWrapper = new dBbcDCMWrapper("dBbcDCM", 1);
      if (!BbcDCMWrapper)
        {
          return 1;
        }
      d = new TableNode_t(BbcDCMWrapper, "dBbcDCM");
      outNode->addNode(d);
    }
  else
    {
      BbcDCMWrapper = static_cast<dBbcDCMWrapper*>(d->getData());
      if (!BbcDCMWrapper)
        {
          return 1;
        }
    }
  delete j;

  dBbcRawWrapper* BbcRawWrapper = findNode::getClass<dBbcRawWrapper>(root,"dBbcRaw");
  DBBCRAW_ST* bbcrawwrap = BbcRawWrapper->TableData();

  DBBCDCM_ST* bbcdcm = BbcDCMWrapper->TableData();

  BbcRaw* bbcraw = findNode::getClass<BbcRaw>(root,"BbcRaw");

  unsigned long DCMwcnt;
  unsigned long DCMword;

  short fem_board_id = -1;
  short fem_chan_id;
  short remainder1;
  short remainder2;
  short pmt_number;

  for (DCMwcnt = 1;DCMwcnt < bbcdcm->nWord;DCMwcnt++)
    {
      DCMword = bbcdcm->DCM[DCMwcnt];
      if ((DCMword&0xf0000) == 0x00000)
        {
          remainder1 = (DCMwcnt - 6) % 25;

          if (remainder1 == 0)
            {
              fem_board_id = bbcdcm->DCM[DCMwcnt];
            }
          else
            {
              fem_chan_id = (remainder1 - 1) / 3 + 1;
              pmt_number = calib.getPmtNumber(fem_board_id, fem_chan_id) - 1;
              remainder2 = (remainder1 % 3);
              if (remainder2 == 1)
                {
                  if (pmt_number < BBC_N_PMT)
                    {
                      Adc[pmt_number] = bbcdcm->DCM[DCMwcnt];
                    }
                }
              else if (remainder2 == 2)
                {
                  if (pmt_number < BBC_N_PMT)
                    {
                      Tdc0[pmt_number] = bbcdcm->DCM[DCMwcnt];
                    }
                }
              else if (remainder2 == 0)
                {
                  if (pmt_number < BBC_N_PMT)
                    {
                      Tdc1[pmt_number] = bbcdcm->DCM[DCMwcnt];
                    }
                }
            }
        }
    }

  DBBCRAW_ST *p1;
  for ( int ipmt = 0;ipmt < BBC_N_PMT;ipmt++ )
    {
      p1 = bbcrawwrap + ipmt;
      p1->Pmt = ipmt;
      p1->Adc = Adc[ipmt];
      p1->Tdc0 = Tdc0[ipmt];
      p1->Tdc1 = Tdc1[ipmt];
    }

  BbcRawWrapper->SetRowCount(BBC_N_PMT);

  if (bbcraw)
    {
      for ( int ipmt=0;ipmt<BBC_N_PMT;ipmt++ ) 
	{
	  bbcraw->AddBbcRawHit(ipmt, Adc[ipmt],Tdc0[ipmt],Tdc1[ipmt], ipmt);
	}
      bbcraw->set_npmt(BBC_N_PMT);
    }

  return 0;
}

PHBoolean
BbcEvent::DstStore(PHCompositeNode *root)
{

  //
  // Generate BBC output table.
  //
  dBbcOutWrapper* BbcOutWrapper = findNode::getClass<dBbcOutWrapper>(root,"dBbcOut");
  DBBCOUT_ST* bbcoutwrap = BbcOutWrapper->TableData();

  BbcOut* bbcout = findNode::getClass<BbcOut>(root,"BbcOut");

  // filling the end products into the 'dBbcOut' table if it exists
  if (bbcoutwrap)
    {
      bbcoutwrap->VertexPoint = ZVertex;
      bbcoutwrap->dVertexPoint = ZVertexError;
      bbcoutwrap->TimeZero = TimeZero;
      bbcoutwrap->dTimeZero = TimeZeroError;
      bbcoutwrap->NhitPmtNorth = nHitPmt[1];
      bbcoutwrap->NhitPmtSouth = nHitPmt[0];
      bbcoutwrap->ChargeSumNorth = ChargeSum[1];
      bbcoutwrap->ChargeSumSouth = ChargeSum[0];
      for ( int i = 0; i < BBC_N_PMT; i++ )
	{
	  bbcoutwrap->Adc[i] = Adc[i];
	  bbcoutwrap->Tdc[i] = Tdc0[i];
	}

      BbcOutWrapper->SetRowCount(1);
    }

  if (bbcout)
    {
      bbcout->set_TimeVertex(TimeZero,TimeZeroError,ZVertex,ZVertexError);
      for (short itmp=0;itmp<2;itmp++)
	{ 
	  bbcout->AddBbcNS((short) nHitPmt[itmp],ChargeSum[itmp],ArmHitTime[itmp],itmp);
	}
    }

  return True;
}
///
int BbcEvent::calculate()
{
  for (int i = 0;i < BBC_N_PMT;i++)
    calcPmtProduct(i);
  // suppose to Z vertex=0 to calculate the transit time
  ZVertex = 0.0;
  int flag = -1;
  for (int i = 0; i < 1; i++)
    {
      calcArmProduct(Bbc::South);
      calcArmProduct(Bbc::North);
      flag = calcEndProduct();
    }

  return (flag);
}

int
BbcEvent::printPmtChTime(void) const
  {
    for (int iPmt = 0; iPmt < 128; iPmt++)
      {
        cout << iPmt << "  " << Charge[iPmt]
        << "  " << HitTime0[iPmt] << "  " << HitTime1[iPmt] << endl;
      }

    return 0;
  }

int
BbcEvent::printAdcTdc(void) const
  {
    for (int iPmt = 0; iPmt < 128; iPmt++)
      {
        cout << iPmt << "  " << Adc[iPmt]
        << "  " << Tdc0[iPmt]	<< "  " << Tdc1[iPmt]	<< endl;
      }

    return 0;
  }

int BbcEvent::calcPmtProduct(int PmtIndx)
{

  TrueAdc[PmtIndx] = calib.getAdc(PmtIndx, Adc[PmtIndx]);
  Charge[PmtIndx] = calib.getCharge(PmtIndx, Adc[PmtIndx]);
  HitTime0[PmtIndx] = calib.getHitTime0(PmtIndx, Tdc0[PmtIndx], Adc[PmtIndx]);
  HitTime1[PmtIndx] = calib.getHitTime1(PmtIndx, Tdc1[PmtIndx], Adc[PmtIndx]);

  if ( HitTime0[PmtIndx] > -998.0 )
    {  // if TDC value is not underflow and overflow
      iHit[PmtIndx] = true;
    }
  else
    {
      iHit[PmtIndx] = false;
    }

  return (iHit[PmtIndx]);
}
/*==================================================================*/
///
int 
BbcEvent::calcEndProduct(void)
{
  const float L = 288.7;        /* distance between BBC north and south [cm]*/
  const float TTT = L / C;        /* total transit time [ns] */

  // The selection criteria of return_flag
  //--------------------------------------------------------------------------
  // hit_flag  south_flag north_flag | return_flag |  zvertex value
  //--------------------------------------------------------------------------
  //    0           0          0     |      0      | go to next calculation
  //    0           0          1     |      1      |  -99901
  //    0           0          2     |      2      |  -99902
  //    0           1          0     |      3      |  -99903
  //    0           1          1     |      4      |  -99904
  //    0           1          2     |      5      |  -99905
  //    0           2          0     |      6      |  -99906
  //    0           2          1     |      7      |  -99907
  //    0           2          2     |      8      |  -99908
  //--------------------------------------------------------------------------
  //    1           0         -1     |      9      |  -99909
  //    1           1         -1     |     10      |  -99910
  //    1           2         -1     |     11      |  -99911
  //--------------------------------------------------------------------------
  //    2          -1          0     |     12      |  -99912
  //    2          -1          1     |     13      |  -99913
  //    2          -1          2     |     14      |  -99914
  //--------------------------------------------------------------------------
  //   -1          -1         -1     |     15      |  -99915(both arms has no hit)
  //--------------------------------------------------------------------------

  int flag = calcFlag();  // get flag.
  float TimeSouth, TimeNorth;

  switch (flag)
    {
    case 0:
      TimeSouth = 
	calib.getArmHitTime(Bbc::South, ArmHitTime[Bbc::South]);
      TimeNorth = 
	calib.getArmHitTime(Bbc::North, ArmHitTime[Bbc::North]);

      ZVertex = (TimeSouth - TimeNorth) * C / 2.0;
      TimeZero = calib.getCorrectedTzero(TimeSouth, TimeNorth) - TTT / 2.0;

      // chp: the following code for the error determination is not 
      // implemented - it gives allways zero 
      ZVertexError =
        sqrt(ArmHitTimeError[Bbc::North] * ArmHitTimeError[Bbc::North]
             + ArmHitTimeError[Bbc::South] * ArmHitTimeError[Bbc::South]) / 2.0 * C;
      TimeZeroError =
        sqrt(ArmHitTimeError[Bbc::North] * ArmHitTimeError[Bbc::North]
             + ArmHitTimeError[Bbc::South] * ArmHitTimeError[Bbc::South]) / 2.0;
      break;

    case 1:
      ZVertex = -99901.0;
      TimeZero = -99901.0;
      ZVertexError = -99901.0;
      TimeZeroError = -99901.0;
      break;
    case 2:
      ZVertex = -99902.0;
      TimeZero = -99902.0;
      ZVertexError = -99902.0;
      TimeZeroError = -99902.0;
      break;
    case 3:
      ZVertex = -99903.0;
      TimeZero = -99903.0;
      ZVertexError = -99903.0;
      TimeZeroError = -99903.0;
      break;
    case 4:
      ZVertex = -99904.0;
      TimeZero = -99904.0;
      ZVertexError = -99904.0;
      TimeZeroError = -99904.0;
      break;
    case 5:
      ZVertex = -99905.0;
      TimeZero = -99905.0;
      ZVertexError = -99905.0;
      TimeZeroError = -99905.0;
      break;
    case 6:
      ZVertex = -99906.0;
      TimeZero = -99906.0;
      ZVertexError = -99906.0;
      TimeZeroError = -99906.0;
      break;
    case 7:
      ZVertex = -99907.0;
      TimeZero = -99907.0;
      ZVertexError = -99907.0;
      TimeZeroError = -99907.0;
      break;
    case 8:
      ZVertex = -99908.0;
      TimeZero = -99908.0;
      ZVertexError = -99908.0;
      TimeZeroError = -99908.0;
      break;
    case 9:
      ZVertex = -99909.0;
      TimeZero = -99909.0;
      ZVertexError = -99909.0;
      TimeZeroError = -99909.0;
      break;
    case 10:
      ZVertex = -99910.0;
      TimeZero = -99910.0;
      ZVertexError = -99910.0;
      TimeZeroError = -99910.0;
      break;
    case 11:
      ZVertex = -99911.0;
      TimeZero = -99911.0;
      ZVertexError = -99911.0;
      TimeZeroError = -99911.0;
      break;
    case 12:
      ZVertex = -99912.0;
      TimeZero = -99912.0;
      ZVertexError = -99912.0;
      TimeZeroError = -99912.0;
      break;
    case 13:
      ZVertex = -99913.0;
      TimeZero = -99913.0;
      ZVertexError = -99913.0;
      TimeZeroError = -99913.0;
      break;
    case 14:
      ZVertex = -99914.0;
      TimeZero = -99914.0;
      ZVertexError = -99914.0;
      TimeZeroError = -99914.0;
      break;
    case 15:
      ZVertex = -99915.0;
      TimeZero = -99915.0;
      ZVertexError = -99915.0;
      TimeZeroError = -99915.0;
      break;
    }  // end of switch
  return (flag);
}

int 
BbcEvent::calcArmProduct(Bbc::ArmType arm)
{
  int PmtOffset;
  if (arm == Bbc::South)
    {
      PmtOffset = 0;
    }
  else if (arm == Bbc::North)
    {
      PmtOffset = 64;
    }
  else
    {
      cerr << "BbcEvent: Error invalid argument" << endl;
      exit(5);
    }
  // first step : finding simple average
  ChargeSum[arm] = 0.0;
  int nHit = 0;
  float SumTime = 0.0;
  float HitTime[BBC_N_PMT];
  for (int i = PmtOffset;i < 64 + PmtOffset;i++)
    {
      if ( iHit[i] )
        {
          nHit++;
          HitTime[i] = HitTime0[i] - TimeLagOfTransitTime(i, ZVertex);
          SumTime += HitTime[i];
        }
    }
  nHitPmt[arm] = nHit;
  float AvgHitTime = SumTime / nHit;

  //  histogram : h0[nbin]
  //  range: mean +- 20 ns
  const float BinWidth = 0.20;   //  [ns]
  const int nbin = 80;
  float xlow = int(AvgHitTime - BinWidth * nbin / 2.0);  // round up
  float xup = xlow + BinWidth * nbin;

  // peak finding
  int h0[nbin];
  for (int i = 0;i < nbin;i++)
    h0[i] = 0;
  for (int i = PmtOffset;i < 64 + PmtOffset;i++)
    {
      if ( iHit[i] )
        {
          if (HitTime[i] > xlow && HitTime[i] < xup)
            {
              int bin = int((HitTime[i] - xlow) / BinWidth);
              if (bin < 0 || bin >= nbin)
                {
                  cerr << "Out of range !! " << bin << endl;
		  exit(1);
                }
              h0[bin]++;
            }
        }
    }
  int peak = 0;
  float MaxContents = 0;

  for (int i = 0;i < nbin;i++)
    {
      if ( h0[i] > MaxContents)
        {
          peak = i;
          MaxContents = h0[i];
        }
    }
  float TimeOfPeak = (peak + 0.5) * BinWidth + xlow;
  /*
  TH1F h0("h0", "Hit time distribution", nbin, xlow, xup);
  for(int i=PmtOffset;i<64+PmtOffset;i++){
    if( iHit[i] )  h0.Fill(HitTime[i]);
}
  float TimeOfPeak = (h0.GetMaximumBin()+0.5)*BinWidth+xlow;
  */
  float MinRange = TimeOfPeak - 3.0;
  float MaxRange = TimeOfPeak + 3.0;
  int NValidHits = 0;

  SumTime = 0;
  for (int i = PmtOffset;i < 64 + PmtOffset;i++)
    {
      if (iHit[i] && HitTime[i] >= MinRange && HitTime[i] < MaxRange )
        {
          armHitPmt[i] = 1;
          NValidHits++;
          SumTime += HitTime[i];
          ChargeSum[arm] += Charge[i];
        }
    }

  if (NValidHits == 0)
    {
      ArmHitTimeError[arm] = 0.0;
      ArmHitTime[arm] = -9999.0;
    }
  else
    {
      ArmHitTime[arm] = SumTime / NValidHits;
      ArmHitTimeError[arm] = 0.0;
    }
  nHitPmt[arm] = NValidHits;
  return (0);
}
/*==================================================================*/
///
float BbcEvent::TimeLagOfTransitTime(int PmtIndx, float Z)const
  {
    // square of distance each PMT from the beam axis [cm*cm]
    static const float R2[32] =
      {
        183.552402, 159.355605, 195.629199, 147.235605,
        114.973203, 183.514004, 127.054795, 86.726797,
        62.530000, 187.534805, 123.010000, 74.616401,
        207.691602, 135.101201, 78.641997, 163.328398,
        98.803594, 207.691602, 135.101201, 78.641997,
        187.534805, 123.010000, 74.616401, 183.514004,
        127.054795, 86.726797, 62.530000, 195.629199,
        147.235605, 114.973203, 183.552402, 159.355605
      };
    if (PmtIndx < 0 || PmtIndx >= BBC_N_PMT)
      {
        cerr << "BbcEvent: Error invalid argument " << PmtIndx << endl;
        exit(5);
      }
    // distance of each arm from nominal vertex position [cm]
    static const float L = 144.350;
    // distance of each from actual vertex position [cm]
    float Distance = (PmtIndx < 64) ? L + Z : L - Z;
    // This calculation has enough of resolution although it is subtraction
    // between two similar values.
    float TransitTime = (sqrt(Distance * Distance + R2[PmtIndx % 32]) - Distance) / C;

    return TransitTime;
  }
/*==================================================================*/
///
void 
BbcEvent::setCalibDataAll( )
{
  calib.restore( );
}

void 
BbcEvent::setCalibDataAll( const BbcTime_t& time)
{
  calib.restore( time );
}

void 
BbcEvent::setCalibDataAll( BbcCalib *ExistCalib )
{
  calib = *ExistCalib;
}

void 
BbcEvent::setFakePedestal()
{
  TRandom fpede(EventNumber);
  for ( int ipmt = 0; ipmt < BBC_N_PMT; ipmt++ )
    {
      Adc[ipmt] =  (int)fpede.Gaus(100, 3);
      Tdc0[ipmt] = 4095;
      Tdc1[ipmt] = 4095;
    }
}

void BbcEvent::setFakeEvent()
{
  // square of distance each PMT from the beam axis [cm*cm]
  static const float R2[32] =
    {
      183.552402, 159.355605, 195.629199, 147.235605,
      114.973203, 183.514004, 127.054795, 86.726797,
      62.530000, 187.534805, 123.010000, 74.616401,
      207.691602, 135.101201, 78.641997, 163.328398,
      98.803594, 207.691602, 135.101201, 78.641997,
      187.534805, 123.010000, 74.616401, 183.514004,
      127.054795, 86.726797, 62.530000, 195.629199,
      147.235605, 114.973203, 183.552402, 159.355605
    };

  // distance of each arm from nominal vertex position [cm]
  static const float L = 144.350;

  TRandom fpede(EventNumber);
  float Z = fpede.Gaus(0, 30);

  for ( int ipmt = 0; ipmt < BBC_N_PMT; ipmt++ )
    {
      // distance of each from actual vertex position [cm]
      float Distance = (ipmt < 64) ? L + Z : L - Z;

      float TmpAdc = fabs(fpede.Gaus(250, 250.0));

      float TransitTime = ( sqrt(Distance * Distance + R2[ipmt % 32]) ) / C;

      float timing = fpede.Gaus( TransitTime + 20.0 / sqrt(TmpAdc), 0.05 );
      float hittime = timing / 0.007;

      Adc[ipmt] =  (int)TmpAdc + 890;
      Tdc0[ipmt] = (int)hittime;
      Tdc1[ipmt] = (int)hittime;
    }
}

int BbcEvent::calcFlag()
{

  int hit_flag = -1;   // 0 : south hit and north hit,
  // 1 : no north hit and south hit,
  // 2 : no south hit and north hit,

  int south_flag = -1; // 0: valid range,    2.0 < ArmHitTime[South] < 20.0
  // 1: invalid range,        ArmHitTime[South] < 2.0
  // 2: invalid range, 20.0 < ArmHitTime[South]

  int north_flag = -1; // 0: valid range,    2.0 < ArmHitTime[South] < 20.0
  // 1: invalid range,        ArmHitTime[South] < 2.0
  // 2: invalid range, 20.0 < ArmHitTime[South]

  if ( nHitPmt[Bbc::South] != 0 && nHitPmt[Bbc::North] != 0 )
    {
      hit_flag = 0;

      // South
      if ( ArmHitTime[Bbc::South] > 2.0 && ArmHitTime[Bbc::South] < 20.0)
        {
          // OK
          south_flag = 0;
        }
      else if ( ArmHitTime[Bbc::South] <= 2.0)
        {
          // under flow
          south_flag = 1;
        }
      else if ( ArmHitTime[Bbc::South] >= 20.0)
        {
          // over flow
          south_flag = 2;
        }
      // North
      if ( ArmHitTime[Bbc::North] > 2.0 && ArmHitTime[Bbc::North] < 20.0 )
        {
          // OK
          north_flag = 0;
        }
      else if ( ArmHitTime[Bbc::North] <= 2.0)
        {
          // under flow
          north_flag = 1;
        }
      else if ( ArmHitTime[Bbc::North] >= 20.0)
        {
          // over flow
          north_flag = 2;
        }
    }
  else if ( nHitPmt[Bbc::South] != 0 && nHitPmt[Bbc::North] == 0)
    {
      hit_flag = 1;
      // South
      if ( ArmHitTime[Bbc::South] > 2.0 && ArmHitTime[Bbc::South] < 20.0)
        {
          // OK
          south_flag = 0;
        }
      else if ( ArmHitTime[Bbc::South] <= 2.0)
        {
          // under flow
          south_flag = 1;
        }
      else if ( ArmHitTime[Bbc::South] >= 20.0)
        {
          // over flow
          south_flag = 2;
        }
    }
  else if ( nHitPmt[Bbc::South] == 0 && nHitPmt[Bbc::North] != 0)
    {
      hit_flag = 2;
      // North
      if ( ArmHitTime[Bbc::North] > 2.0 && ArmHitTime[Bbc::North] < 20.0 )
        {
          // OK
          north_flag = 0;
        }
      else if ( ArmHitTime[Bbc::North] <= 2.0)
        {
          // under flow
          north_flag = 1;
        }
      else if ( ArmHitTime[Bbc::North] >= 20.0)
        {
          // over flow
          north_flag = 2;
        }
    }

  // The selection criteria of return_flag
  //--------------------------------------------------------------------------
  // hit_flag  south_flag north_flag | return_flag |  zvertex value
  //--------------------------------------------------------------------------
  //    0           0          0     |      0      | go to next calculation
  //    0           0          1     |      1      |  -99901
  //    0           0          2     |      2      |  -99902
  //    0           1          0     |      3      |  -99903
  //    0           1          1     |      4      |  -99904
  //    0           1          2     |      5      |  -99905
  //    0           2          0     |      6      |  -99906
  //    0           2          1     |      7      |  -99907
  //    0           2          2     |      8      |  -99908
  //--------------------------------------------------------------------------
  //    1           0         -1     |      9      |  -99909
  //    1           1         -1     |     10      |  -99910
  //    1           2         -1     |     11      |  -99911
  //--------------------------------------------------------------------------
  //    2          -1          0     |     12      |  -99912
  //    2          -1          1     |     13      |  -99913
  //    2          -1          2     |     14      |  -99914
  //--------------------------------------------------------------------------
  //   -1          -1         -1     |     15      |  -99915(each arm has no hit)
  //--------------------------------------------------------------------------

  int return_flag = -1;

  switch (hit_flag)
    {
    case 0:  //both arms have hit
      switch (south_flag)
        {
        case 0:
          switch (north_flag)
            {
            case 0:
              return_flag = 0;
              break;
            case 1:
              return_flag = 1;
              break;
            case 2:
              return_flag = 2;
              break;
            } // north_flag  switch

          break;
        case 1:
          switch (north_flag)
            {
            case 0:
              return_flag = 3;
              break;
            case 1:
              return_flag = 4;
              break;
            case 2:
              return_flag = 5;
              break;
            } // north_flag  switch

          break;
        case 2:
          switch (north_flag)
            {
            case 0:
              return_flag = 6;
              break;
            case 1:
              return_flag = 7;
              break;
            case 2:
              return_flag = 8;
              break;
            } // north_flag  switch
          break;
        } // south_flag  switch

      break;
    case 1:
      switch (south_flag)
        {
        case 0:
          return_flag = 9;
          break;
        case 1:
          return_flag = 10;
          break;
        case 2:
          return_flag = 11;
          break;
        }
      // south_flag

      break;
    case 2:
      switch (north_flag)
        {
        case 0:
          return_flag = 12;
          break;
        case 1:
          return_flag = 13;
          break;
        case 2:
          return_flag = 14;
          break;
        }
      // north_flag
      break;

    default:  // flag==-1
      return_flag = 15;
      break;
    } // flag switch

  return return_flag;
}
