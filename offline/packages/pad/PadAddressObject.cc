//--------------------------------------------------------------- 
//                                                                
// Created by David Silvermyr
//                                                                
// Description: Implementation of PAD Address Class  
//                                                                
//----------------------------------------------------------------

#include "PadAddressObject.hh" 

using namespace std;

// Constructor
PadAddressObject::PadAddressObject()
{
  current_det=-1;
  current_arm=-1;
  current_side=-1;
  current_sector=-1;
  current_packetid=-1;
  current_grow=-1;
  current_gcol=-1;
  current_tgl=-1;
  current_channel=-1;
  current_hole=-1;
  current_channelid=-1;
  current_padz=-1;
  current_padx=-1;
  current_wiresect=-1;
  current_cellsect=-999; // -1 is an acceptable value, so avoid that
 
  iFlag=-1; // don't have any data yet
}
//**********************************************************************
// Destructor
PadAddressObject::~PadAddressObject() { }

//**********************************************************************
// member functions
//**********************************************************************
// Set current indices
PHBoolean PadAddressObject::setSoft (int idet, int iarm, int iside, int isector, int ipadz, int ipadx){
  //----------------------------------------
  // Set Software Indices
  //----------------------------------------
  current_det=idet;
  current_arm=iarm;
  current_side=iside;
  current_sector=isector;
  current_padz=ipadz;
  current_padx=ipadx;

  iFlag=0; // now we have set something and we are free to calc. something else..
  
  // and set the other software indices..
  current_cellsect=getCellsect(iside,ipadz);
  current_wiresect=getWiresect(idet,iarm,iside,ipadz,ipadx);
  
  //----------------------------------------
  // and Calculate Hardware Indices
  //----------------------------------------
  PHBoolean ok = FromSoftToHard();  
  if(!ok) return False;
  return True;
}
//**********************************************************************
// Set current indices
PHBoolean PadAddressObject::setHard(int ipacketid, int ichannelid){
  //----------------------------------------
  // Set Hardware Indices
  //----------------------------------------
  current_packetid=ipacketid;
  current_channelid=ichannelid;

  iFlag=0; // now we have set something and we are free to calc. something else..

  // and set the other hardware indices
  current_grow=getGrow(ichannelid);
  current_gcol=getGcol(ichannelid);
  current_tgl=getTgl(ichannelid);
  current_channel=getChannel(ichannelid);
  current_hole=getHole(current_tgl, current_channel);

  //----------------------------------------
  // and Calculate Software Indices
  //----------------------------------------
  PHBoolean ok = FromHardToSoft();
  if(!ok) return False;
  return True;
}
//**********************************************************************
PHBoolean PadAddressObject::FromSoftToHard() {
  //---------------------------------------------------
  // How to get from Software indices to Hardware ones
  //---------------------------------------------------
  int idet    = current_det;  
  int iarm    = current_arm;  
  int iside   = current_side;  
  int isector = current_sector;  
  int ipadz   = current_padz;  
  int ipadx   = current_padx;  

  int ipacketid=getPacketid(idet, iarm, iside, isector, ipadx);
  int ichannelid=getChannelid(idet, ipadz, ipadx);

  // Set Hardware Indices
  //----------------------------------------
  current_packetid=ipacketid;
  current_channelid=ichannelid;
  
  // and set the other hardware indices
  current_grow=getGrow(ichannelid);
  current_gcol=getGcol(ichannelid);
  current_tgl=getTgl(ichannelid);
  current_channel=getChannel(ichannelid);
  current_hole=getHole(current_tgl, current_channel);

  return True;
}
//**********************************************************************
PHBoolean PadAddressObject::FromHardToSoft() {
  //---------------------------------------------------
  // How to get from Hardware indices to Software ones
  //---------------------------------------------------
  int ipacketid   = current_packetid;
  int ichannelid = current_channelid;

  int idet=getDet(ipacketid);
  int iarm=getArm(ipacketid);
  int isector=getSector(ipacketid);
  int iside=getSide(ipacketid);
  int ipadz=getPadz(ichannelid);
  int ipadx=getPadx(ipacketid, ichannelid);
  
  current_det=idet;
  current_arm=iarm;
  current_sector=isector;
  current_side=iside;
  current_padz=ipadz;
  current_padx=ipadx;
  
  // and set the other software indices..
  current_cellsect=getCellsect(iside,ipadz);
  current_wiresect=getWiresect(idet,iarm,iside,ipadz,ipadx);
  
  return True;
}
//**********************************************************************
//******** Here starts the simple conversion methods ***************
//*************** Hard to Soft**************************************
//**********************************************************************
int PadAddressObject::getDet(int ipacketid)
{
  // Gets Software DET/Chamber from Hardware indices.
  int tmpdet=-1;
  if ((ipacketid>4000) && (ipacketid<4097)) 
    tmpdet=(ipacketid-4001)/32; // 0 for PC1, 1 for PC2, 2 for PC3
  else {
    cerr << "PadAddressObject ERROR getDet: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpdet;
} 
/* end getDet() */
//**********************************************************************
int PadAddressObject::getArm(int ipacketid)
{
  // Gets Software ARM from Hardware indices.
  int tmparm=-1;
  if ((ipacketid>4000) && (ipacketid<4097)) { 
    tmparm=((ipacketid-4001)%32)/16; // 0 for West and 1 for East
    tmparm=1-tmparm; // new convention: 0 for East and 1 for West
  }
  else {
    cerr << "PadAddressObject ERROR getArm: argument(s) out of bounds \n";
    return -1;
  }  
  return tmparm;
} 
/* end getArm() */
//**********************************************************************
int PadAddressObject::getSide(int ipacketid)
{
  // Gets Software SIDE from Hardware indices.
  int tmpside=-1;
  if ((ipacketid>4000) && (ipacketid<4097)) 
    tmpside=((ipacketid-4001)%16)/8; // 0 for South and 1 for North
  else {
    cerr << "PadAddressObject ERROR getSide: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpside;
} 
/* end getSide() */
//**********************************************************************
int PadAddressObject::getSector(int ipacketid)
{
  // Gets Software SECTOR from Hardware indices.
  int tmpsect=-1;
  if ((ipacketid>4000) && (ipacketid<4097)) {
    int tmpdet=(ipacketid-4001)/32; 
    if (tmpdet==0) tmpsect=(ipacketid-4001)%8; // 0 to 7 for PC1
    else tmpsect=((ipacketid-4001)%8)/2; // 0 to 3 for PC2/3
  }
  else {
    cerr << "PadAddressObject ERROR getSector: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpsect;
} 
/* end getSector() */
//**********************************************************************
int PadAddressObject::getPadz(int ichannelid)
{
  // Gets Software PADZ from Hardware indices.
  int tmppadz=-1;
  if ((ichannelid>-1) && (ichannelid<2160)) 
    tmppadz=ichannelid/20;    

  return tmppadz;
} 
/* end getPadz() */
//**********************************************************************
int PadAddressObject::getPadx(int ipacketid, int ichannelid)
{
  // Gets Software PADX from Hardware indices.
  int tmppadx=-1;
  if ( (ipacketid>4000) && (ipacketid<4097) &&
       (ichannelid>-1) && (ichannelid<2160) ) {
    tmppadx=ichannelid%20;    
    // for PC2/3 we might need to add 20 to the padx value, depending on which
    // det, arm,subsector and side we are in.. This is the readable version.
    // It's possible to write this routine much shorter..
    int tmpdet=(ipacketid-4001)/32;
    int tmparm=((ipacketid-4001)%32)/16;
    tmparm=1-tmparm; // 0 = East and 1 = West
    int tmpside=((ipacketid-4001)%16)/8;
    int tmpmodsubsect=(ipacketid-4001)%2;
    int offset=0;
    if ( (tmpdet==1) || (tmpdet==2) ) { // PC2 or PC3
      if ( ((tmparm==tmpside) && (tmpmodsubsect!=0)) ||
	   ((tmparm!=tmpside) && (tmpmodsubsect==0)) ) offset=1;
    }
    if ( (tmpdet==2) && (tmparm==1) ) { // PC3 West has MB facing IP
      offset=1-offset; // should be the other way around..
    }
    tmppadx+=offset*20;
  }
  else {
    cerr << "PadAddressObject ERROR getPadx: argument(s) out of bounds \n";
    return -1;
  }  

  return tmppadx;
} 
/* end getPadx() */
//**********************************************************************
//*************** Soft to Soft***************
//**********************************************************************
int PadAddressObject::getCellsect(int iside, int ipadz)
{
  const int cells_along_wire=106;
  
  // Gets Software CELLSECT from Software indices.
  int tmpcellsect=-999; // -1 is an acceptable return value from this routine..

  if ( (iside>-1) && (iside<2) && (ipadz>-1) && (ipadz<108) ) {
    tmpcellsect=ipadz-1; // -1 to 106   
    if (iside==0) tmpcellsect=(cells_along_wire-1)-tmpcellsect; // South side
    else tmpcellsect+=cells_along_wire; // North side
  }
  else {
    cerr << "PadAddressObject ERROR getCellsect: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpcellsect;
} 
/* end getCellsect() */
//**********************************************************************
int PadAddressObject::getWiresect(int idet, int iarm, int iside, int ipadz, int ipadx)
{
  const int wire_adjust[3] = {0,-1,1};
  const int cells_across_wire[3] = {58,116,116}; 
  
  // Gets Software WIRESECT from Software indices.
  int tmpwiresect=-1; 
  
  if ( (idet>-1) && (idet<3) && (iside>-1) && (iside<2) && 
       (iarm>-1) && (iarm<2) && (ipadz>-1) && (ipadz<108) && (ipadx>-1) ) {
    if ( ((idet==0) && (ipadx<20)) || ((idet>0) && (ipadx<40)) ) {
      int tempz=ipadz-1;
      tmpwiresect=3*(ipadx%20)+wire_adjust[(tempz+3)%3] + (ipadx/20)*58;
      if (iside==0) 
	tmpwiresect=(cells_across_wire[idet]-1)-tmpwiresect; // South side
      if ( (idet==2) && (iarm==1) ) 
	{ // PC3 West has MB facing IP
	  tmpwiresect=(cells_across_wire[idet]-1)-tmpwiresect; 
	}
    }
    else {
      cerr << "PadAddressObject ERROR getWiresect: argument(s) out of bounds \n";
      return -1;
    }  
  }
  else {
    cerr << "PadAddressObject ERROR getWiresect: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpwiresect;
} 
/* end getWiresect() */
//**********************************************************************
int PadAddressObject::getPadzFromCellsect(int iside, int icellsect)
{
  const int cells_along_wire=106;
  
  // Gets Software Padz from Software indices.
  int tmppadz=-999; // -1 is an acceptable return value from this routine..

  if ( (iside>-1) && (iside<2) && (icellsect>-2) && (icellsect<107) ) {
    tmppadz=icellsect;
    if (iside==0) tmppadz=(cells_along_wire-1)-tmppadz; // South side
    else tmppadz-=cells_along_wire; // North side
    tmppadz=tmppadz+1; // 0 to 107   
  }
  else {
    cerr << "PadAddressObject ERROR getPadzFromCellsect: argument(s) out of bounds \n";
    return -1;
  }  
  return tmppadz;
} 
/* end getPadzFromCellsect() */
//**********************************************************************
int PadAddressObject::getPadxFromCellWiresect(int idet, int iarm, int iside, int icellsect, int iwiresect)
{
  const int wire_adjust[3] = {0,-1,1};
  const int cells_across_wire[3] = {58,116,116}; 
  
  // Gets Software PADX from Software indices.
  int tmppadx=-1; 

  if ( (idet>-1) && (idet<3) && (iside>-1) && (iside<2) && (iarm>-1) 
       && (iarm<2) && (icellsect>-2) && (icellsect<107) && (iwiresect>-2) ) {
    if ( ((idet==0) && (iwiresect<=58)) || ((idet>0) && (iwiresect<=116)) ) {
      int tempz=getPadzFromCellsect(iside,icellsect)-1;
      if ( ((idet==1) || (idet==2)) && (iwiresect>=58) ) {
	iwiresect=iwiresect%58;
      }
      // flip wire direction
      if  (iside==0) 
	iwiresect=(cells_across_wire[idet]-1)-iwiresect; // South side
      if ( (idet == 2) && (iarm == 1) ) 
	iwiresect=(cells_across_wire[idet]-1)-iwiresect; // PC3 W MB facing IP

      tmppadx=(iwiresect-wire_adjust[(tempz+3)%3])/3;
    }
    else {
      cerr << "PadAddressObject ERROR getPadxFromCellWiresect: argument(s) out of bounds \n";
      return -1;
    }  
  }
  else {
    cerr << "PadAddressObject ERROR getPadxFromCellWiresect: argument(s) out of bounds \n";
    return -1;
  }  
  return tmppadx;
} 
/* end getPadxFromCellWiresect() */
//**********************************************************************
//*************** Soft to Hard ***************
//**********************************************************************
int PadAddressObject::getPacketid(int idet, int iarm, int iside, int isector, int ipadx)
{
  // Gets Hardware PACKETID from Software indices.
  int tmppacketid=-1; 
  
  if ( (idet>-1) && (idet<3) && (iarm>-1) && (iarm<2) && (iside>-1) && (iside<2) 
       && (isector>-1) && (isector<8) && (ipadx>-1) && (ipadx<40) ) {
    // + detector specific constraints..
    if ( ((idet==0) && (ipadx<20)) || ((idet>0) && (isector<4)) ) {
      
      if (idet==0) tmppacketid = 4001 + (1-iarm)*16 + iside*8 + isector; // PC1
      else tmppacketid = 4001 + idet*32 + (1-iarm)*16 + iside*8 + isector*2 + ipadx/20; // PC2,3
    }
    else {
      cerr << "PadAddressObject ERROR getPacketid: argument(s) out of bounds \n";
      return -1;
    }  
  }
  else {
    cerr << "PadAddressObject ERROR getPacketid: argument(s) out of bounds \n";
    return -1;
  }  
  return tmppacketid;
} 
/* end getChannelid() */
//**********************************************************************
int PadAddressObject::getChannelid(int idet, int ipadz, int ipadx)
{
  // Gets Hardware CHANNELID from Software indices.
  int tmpchannelid=-1; 
  
  if ( (idet>-1) && (idet<3) && (ipadz>-1) && (ipadz<108) && (ipadx>-1) ) {
    // + detector specific constraints..
    if ( ((idet==0) && (ipadx<20)) || ((idet>0) && (ipadx<40)) ) {
      tmpchannelid=ipadz*20+(ipadx%20);
    }
    else {
      cerr << "PadAddressObject ERROR getChannelid: argument(s) out of bounds \n";
      return -1;
    }  
  }
  else {
    cerr << "PadAddressObject ERROR getChannelid: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpchannelid;
} 
/* end getChannelid() */
//**********************************************************************
//*************** Hard to Hard ***************
//**********************************************************************
int PadAddressObject::getGrow(int ichannelid)
{
  // Gets Hardware GROW from Software indices.
  int tmpgrow=-1; 
  
  if ( (ichannelid>-1) && (ichannelid<2160) ) 
    tmpgrow=(ichannelid%20)/4;
  else {
    cerr << "PadAddressObject ERROR getGrow: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpgrow;
} 
/* end getGrow() */
//**********************************************************************
int PadAddressObject::getGcol(int ichannelid)
{
  // Gets Hardware GCOL from Software indices.
  int tmpgcol=-1; 
  
  if ( (ichannelid>-1) && (ichannelid<2160) ) 
    tmpgcol=ichannelid/240;
  else {
    cerr << "PadAddressObject ERROR getGcol: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpgcol;
} 
/* end getGcol() */
//**********************************************************************
int PadAddressObject::getTgl(int ichannelid)
{
  // Gets Hardware TGL from Software indices.
  int tmptgl=-1; 
  
  if ( (ichannelid>-1) && (ichannelid<2160) ) 
    tmptgl=(ichannelid%240)/80 + 1; // 1 to 3
  else {
    cerr << "PadAddressObject ERROR getTgl: argument(s) out of bounds \n";
    return -1;
  }  
  return tmptgl;
} 
/* end getTgl() */
//**********************************************************************
int PadAddressObject::getChannel(int ichannelid)
{
  // Gets Hardware CHANNEL from Software indices.
  int tmpchannel=-1; 
  
  if ( (ichannelid>-1) && (ichannelid<2160) ) 
    tmpchannel=(((ichannelid%240)/20)%4)*4 + ichannelid%4 + 1; // 1 to 16
  else {
    cerr << "PadAddressObject ERROR getChannel: argument(s) out of bounds \n";
    return -1;
  }  
  return tmpchannel;
} 
/* end getChannel() */
//**********************************************************************
int PadAddressObject::getHole(int itgl, int ichannel)
{
  const int plated_through_hole[16] = { 37, 25, 13, 1, 38, 26, 14, 2,
					39, 27, 15, 3, 40, 28, 16, 4 };
  // Gets Hardware HOLE from Software indices.
  int tmphole=-1; 
  
  if ( (ichannel>0) && (ichannel<17) ) {
    tmphole=plated_through_hole[ichannel-1];
    
    if (itgl == 2)
	tmphole = tmphole + 4;
    else if (itgl == 3) {
      if ((ichannel < 5) || (ichannel > 12)) tmphole = tmphole + 8;
      else if ((ichannel < 9) && (ichannel > 4)) tmphole = tmphole + 9;
      else if ((ichannel < 13) && (ichannel > 8)) tmphole = tmphole + 7;
    }
  }
  else {
    cerr << "PadAddressObject ERROR getHole: argument(s) out of bounds \n";
    return -1;
  }  

  return tmphole;
} 
/* end getHole() */
//**********************************************************************
void PadAddressObject::print()
{
  // Print the parameter information
  if(iFlag!=0) {
    cerr << "PadAddressObject ERROR print: PAO not initialized." << endl;
  }
  cout << "DET= " << current_det << endl; 
  cout << "ARM= " << current_arm << endl; 
  cout << "SIDE= " << current_side << endl; 
  cout << "SECTOR= " << current_sector << endl; 
  cout << "PACKETID= " << current_packetid << endl; 
  cout << "GROUP ROW= " << current_grow << endl; 
  cout << "GROUP COLUMN= " << current_gcol << endl;
  cout << "TGL= " << current_tgl << endl;
  cout << "CHANNEL= " << current_channel << endl;
  cout << "HOLE= " << current_hole << endl;
  cout << "CHANNELID= " << current_channelid << endl;
  cout << "PADZ= " << current_padz << endl;
  cout << "PADX= " << current_padx << endl;
  cout << "CELLSECT= " << current_cellsect << endl; 
  cout << "WIRESECT= " << current_wiresect  << endl;

} 
/* end print() */
