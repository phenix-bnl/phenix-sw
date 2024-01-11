//INCLUDECHECKER: Removed this line: #include "SpinEventConstants.h"
//INCLUDECHECKER: Removed this line: #include "SpinDataEventOut.h"
#include "SpinDataEventOutv2.h"
#include "SpinEvent.h"
#include "phool.h"
//INCLUDECHECKER: Removed this line: #include "PHDataNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

#include "Event.h"

#include <iostream>

ClassImp(SpinEvent)

using namespace std;

typedef PHIODataNode<SpinDataEventOut> SpinDataEventOutNode_t;
typedef PHDataNode<Event>              EventNode_t;
//________________________________________________________________________________


SpinEvent::SpinEvent()
{
  pSpinDataEventOut = NULL;

  ShiftCrossing  = shiftCrossing;  // this should be replaced by calibrated value
}
//________________________________________________________________________________


int SpinEvent::event(PHCompositeNode *topNode) {

  PHNodeIterator iter(topNode);
  Event *evt = NULL;
  
  EventNode_t* EventNode = dynamic_cast<EventNode_t*> (iter.findFirst("PHDataNode","PRDF"));

  if (!EventNode) {
    cout << PHWHERE << "[ERROR] PRDF Node missing in SpinEvent" << endl;
    return 0;
  }
  
  evt = EventNode->getData();         // Get event pointer from EventNode
  if (!evt) {
    cout << PHWHERE << "[ERROR] NULL Event Pointer in SpinEvent" << endl;
    return 0;
  }


  PHTypedNodeIterator<SpinDataEventOut> pSpinDataEventOut_iter(topNode);
  SpinDataEventOutNode_t *SpinDataEventOutNode = pSpinDataEventOut_iter.find("SpinDataEventOut");
  if (SpinDataEventOutNode) {
    pSpinDataEventOut = SpinDataEventOutNode->getData();
    if (!pSpinDataEventOut) {
      cout << PHWHERE << " Unable to get SpinDataEventOut, is Node missing?" << endl;
      return 0;
    }
  } else {
    cout << PHWHERE << " SpinDataEventOut Node is missing" << endl;
    return 0;
  }
    
  setRawData(evt);

  return 1;
} 
//________________________________________________________________________________


PHBoolean SpinEvent::setRawData (Event *event) {

  if ( !pSpinDataEventOut ) {
    pSpinDataEventOut = new SpinDataEventOutv2();
  }

  pSpinDataEventOut -> SetEventSequence( event -> getEvtSequence() );

  Packet* pGL1     = NULL;  // Gloval Level 1
  Packet* pGL1P[2];
          pGL1P[0] = NULL;  // GL1P scaler first board
          pGL1P[1] = NULL;  // GL1P scaler second board
  Packet* pGL1PSum = NULL;  // GL1P scaler sum on DCM


  pGL1     = event -> getPacket(GL1_packet_number);     // Packet numbers are
  pGL1P[0] = event -> getPacket(GL1P1_packet_number);   // obtained from 
  pGL1P[1] = event -> getPacket(GL1P2_packet_number);   // SpinEventConstants.h
  pGL1PSum = event -> getPacket(GL1PSum_packet_number); //

  if ( pGL1 == 0 )
    {
      return False;
    }
  else
    {
      setGL1PacketData(pGL1);
      delete pGL1;
    }

  for ( int iboard = 0 ; iboard < nGL1PBoard ; iboard++)
    {
      if ( pGL1P[iboard] != 0 )
	{
	  setGL1PPacketData(pGL1P[iboard], iboard);
	  delete pGL1P[iboard];
	}
    }

  if ( pGL1PSum != 0 )
    {
      setGL1PSumPacketData(pGL1PSum);
      delete pGL1PSum;
    }

  
  return True;

}
//________________________________________________________________________________

void SpinEvent::setGL1PacketData(Packet* pGL1) {

  int GL1_Crossing_ID = (int)( pGL1 -> iValue(0,"CROSSCTR") );
  pSpinDataEventOut -> SetGL1CrossingID(GL1_Crossing_ID);

  int GL1_Crossing_ID_corrected = (GL1_Crossing_ID+ ShiftCrossing ) % nCrossing;
  pSpinDataEventOut -> SetSpinGL1CrossingID(GL1_Crossing_ID_corrected);

  unsigned int GL1_gdisablebits = (unsigned int)( pGL1 -> iValue(0,"GDISABLE") );

  unsigned int is_trigger_Blue        = GL1_gdisablebits & gdisablebitsBlue;
  unsigned int is_trigger_Yellow      = GL1_gdisablebits & gdisablebitsYellow;

  unsigned int is_trigger_BlueUp      = is_trigger_Blue   == gdisablebitsBlueUp;
  unsigned int is_trigger_BlueDown    = is_trigger_Blue   == gdisablebitsBlueDown;
  unsigned int is_trigger_BlueUnpol   = is_trigger_Blue   == gdisablebitsBlueUnpol;

  unsigned int is_trigger_YellowUp    = is_trigger_Yellow == gdisablebitsYellowUp;
  unsigned int is_trigger_YellowDown  = is_trigger_Yellow == gdisablebitsYellowDown;
  unsigned int is_trigger_YellowUnpol = is_trigger_Yellow == gdisablebitsYellowUnpol;

  short upordown_Blue=-99;
  if (is_trigger_BlueUp     ) { upordown_Blue = 1; }
  if (is_trigger_BlueDown   ) { upordown_Blue =-1; }
  if (is_trigger_BlueUnpol  ) { upordown_Blue = 0; }
 
  short upordown_Yellow=-99;
  if (is_trigger_YellowUp   ) { upordown_Yellow = 1; }
  if (is_trigger_YellowDown ) { upordown_Yellow =-1; }
  if (is_trigger_YellowUnpol) { upordown_Yellow = 0; }

  pSpinDataEventOut -> SetSpinDirectionBlueFromV124(upordown_Blue);
  pSpinDataEventOut -> SetSpinDirectionYellowFromV124(upordown_Yellow);

  pSpinDataEventOut -> SetSpinDirectionBlueFromCDEVFillPattern(-99);   // Should be changed to be read from CDEV database
  pSpinDataEventOut -> SetSpinDirectionYellowFromCDEVFillPattern(-99); //

}
//________________________________________________________________________________

void SpinEvent::setGL1PPacketData(Packet* pGL1P, int iboard) {

  pSpinDataEventOut -> SetGL1PEventNumber(iboard, pGL1P -> iValue(0,"EVNUMBER") );

  int GL1P_Crossing_ID = (int)(pGL1P -> iValue(0,"CLOCK"));
  pSpinDataEventOut -> SetGL1PCrossingID(iboard, GL1P_Crossing_ID);

  for (int j=0 ; j<nGL1PScaler ; j++){
    int scaler_count = pGL1P -> iValue(j,"SCALER");
    pSpinDataEventOut -> SetGL1PScalerCount(iboard, j, scaler_count);
  }

  int GL1P_Crossing_ID_corrected = ( GL1P_Crossing_ID + ShiftCrossing ) % nCrossing;
  pSpinDataEventOut -> SetSpinGL1PCrossingID(iboard, GL1P_Crossing_ID_corrected);

}
//________________________________________________________________________________

void SpinEvent::setGL1PSumPacketData(Packet* pGL1PSum) {

  pSpinDataEventOut -> SetGL1PSumEventNumber( pGL1PSum -> iValue(0,"EVTNR") );

  int GL1PSum_Crossing_ID = (int)( pGL1PSum -> iValue(0,"BEAMCROSSID") );
  pSpinDataEventOut -> SetGL1PSumCrossingID(GL1PSum_Crossing_ID);

  for (int iboard=0 ; iboard<nGL1PBoard ; iboard++){
    for (int iscaler=0 ; iscaler<nGL1PScaler ; iscaler++){
      long gl1psumscalercount = (long)( pGL1PSum -> iValue(iboard*nGL1PScaler + iscaler) );
      pSpinDataEventOut -> SetGL1PSumScalerCount(iboard, iscaler, gl1psumscalercount);
    }
  }

  pSpinDataEventOut -> SetGL1PSumCrossingCount( pGL1PSum->iValue(0,"CROSSCTR") );


  int GL1PSum_Crossing_ID_corrected = ( GL1PSum_Crossing_ID + ShiftCrossing ) % nCrossing;
  pSpinDataEventOut -> SetSpinGL1PSumCrossingID(GL1PSum_Crossing_ID_corrected);

}
//________________________________________________________________________________

SpinDataEventOut* SpinEvent::getSpinDataEventOut(){
  return pSpinDataEventOut;
}
//________________________________________________________________________________

int SpinEvent::print() {
  return 1;
}
//________________________________________________________________________________
