#include <packet_muid_ll1.h>
#include <string.h>

Packet_muid_ll1::Packet_muid_ll1(PACKET_ptr data) : Packet_w4 (data)
{
  muidll1 = NULL;
}

int *Packet_muid_ll1::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_muid_ll1( temp
			      ,(int *)  findPacketDataStart(packet) 
			      ,dlength
			      ,MAX_OUTLENGTH, &olength);

  if (status || olength<=0 ) return NULL;
 
  p = new int[olength];
  k = p;
  for (i =0; i<olength; i++) *k++ = temp[i];
  *nwout = olength;
  return p;
}

int Packet_muid_ll1::iValue(const int ich, const char *what){
  
  if(!muidll1) {
    int status = demangle();
    muidll1->DecodeStatus = status;
  }
  if(!muidll1){
    std::cout << "Failed to fill data structure" << std::endl;
    return -1;
  }

  if(strcmp(what,"HEADER")==0) {
    
    return muidll1->header;
    
  } else if(strcmp(what,"ROADS")==0) {
    
    if(ich<0 || ich>4) return -1;
    else return muidll1->Road[ich];

  } else if(strcmp(what,"SROADS")==0) {
    
    if(ich<0 || ich>4) return -1;
    else return muidll1->ShallowRoad[ich];
    
  } else if(strcmp(what,"PARTSUM")==0) {
    
    if(ich<0 || ich>5) return -1;
    else return muidll1->PartialSum[ich];
    
  } else if(strcmp(what,"DATAERR")==0) {

    if(ich<0 || ich>4) return -1;    
    return muidll1->DataErr[ich+1];
    
  } else if(strcmp(what,"SYNCERR")==0) {

    if(ich<0 || ich>4) return -1;        
    return muidll1->SyncErr[ich+1];
    
  } else if(strcmp(what,"STRIP")==0) {
    
    return muidll1->Strip;
    
  } else if(strcmp(what,"ORIENTSTRIP")==0) {
    
    return muidll1->OrientStrip;
    
  } else if(strcmp(what,"LUT2")==0) {
    
    return muidll1->Lut2;
    
  } else if(strcmp(what,"TOTALSUM")==0) {
    
    return muidll1->Sum;
    
  } else if(strcmp(what,"MODEBITS")==0) {
    
    return muidll1->ModeBits;
    
  } else if(strcmp(what,"CHIPTYPE")==0) {
    if(ich<0 || ich>5) return -1;
    
    return muidll1->ChipType[ich];

  } else if(strcmp(what,"ALGCHIP")==0) {
    if(ich<0 || ich>5) return -1;
    
    return muidll1->AlgChip[ich];
    
  } else if(strcmp(what,"CHIPVER")==0) {
    if(ich<0 || ich>5) return -1;
    
    return muidll1->ChipVer[ich];
    
  } else if(strcmp(what,"MUIDVERT")==0) {
    if(ich<0 || ich>5) return -1;
    
    return muidll1->MUIDvert[ich];
    
  } else if(strcmp(what,"SCALER")==0) {
    if(ich<0 || ich>5) return -1;        
    return muidll1->Scaler[ich];
    
  } else if(strcmp(what,"STRIPLUT")==0) {
    
    return muidll1->StripLut;
    
  } else if(strcmp(what,"ALIGN")==0) {
    
    return muidll1->MuonAlign;

  } else if(strcmp(what,"SHALLOWSUM")==0) {
    
    return muidll1->SSum;

  } else if(strcmp(what,"DLTUBE")==0) {
   
    if(ich<0 || ich>5) return -1;
    int bitvector = 0; 
    //cout << endl; 
    for(int i=0; i<13; i++){
      //cout << muidll1->diagLT[ich][i] << endl;
      if(muidll1->diagLT[ich][i]>0) bitvector |= (0x1<<i);
    }
    //cout << endl;
    return bitvector;
    
  } else if(strcmp(what,"DCODESTAT")==0) {
    
    return muidll1->DecodeStatus;

  }

  //  std::cout <<"iValue(int ich, char* what): Unrecognized value "<<what<<std::endl;
  return 0;

}

int Packet_muid_ll1::iValue(const int ich, const  int what){
  
  if(!muidll1) {
    int status = demangle();
    muidll1->DecodeStatus = status; 
  }
  if(!muidll1){
    std::cout<<"Failed to fill data structure"<<std::endl;
    return -1;
  }
  switch (what) {
    
  case HEADER:
    
    { return muidll1->header; }
    
  case ROADS:
    
    { if(ich<0 || ich> 4) return -1;
      else return muidll1->Road[ich]; }
    
  case PARTSUM:
    
    { if(ich<0 || ich>5) return -1;
      else return muidll1->PartialSum[ich]; }
    
  case DATAERR:
    { 
      if(ich<0 || ich>4) return -1;    
      return muidll1->DataErr[ich+1];
    }
    
  case SYNCERR:
    {
      if(ich<0 || ich>4) return -1;        
      return muidll1->SyncErr[ich+1];
    }
    
  case STRIP:
    
    { return muidll1->Strip; }
    
  case ORIENTSTRIP:
    
    { return muidll1->OrientStrip; }
    
  case LUT2:
    
    { return muidll1->Lut2; }
    
  case TOTALSUM:
    
    { return muidll1->Sum; }
    
  case MODEBITS:
    
    { return muidll1->ModeBits; }

  case CHIPTYPE:
    
    { if(ich<0 || ich>5) return -1;
      else return muidll1->ChipType[ich]; }
    
  case ALGCHIP:
    
    { if(ich<0 || ich>5) return -1;
      else return muidll1->AlgChip[ich]; }
    
  case CHIPVER:
    
    { if(ich<0 || ich>5) return -1;
      else return muidll1->ChipVer[ich]; }

  case MUIDVERT:
    
    { if(ich<0 || ich>5) return -1;
      else return muidll1->MUIDvert[ich]; }
    
  case SCALER:
    {
      if(ich<0 || ich>4) return -1;        
      return muidll1->Scaler[ich];
    }
  case STRIPLUT:
    
    { return muidll1->StripLut; }
    
  case ALIGN:
    
    { return muidll1->MuonAlign; }
     
  case DCODESTAT:
    
    { return muidll1->DecodeStatus; }
       
  }

  return -1;

}

int Packet_muid_ll1::fillIntArray (int destination[],    // the data go here 
			       const int length,      // space we have in destination
			       int * nw,              // words actually used
			       const char * what) // type of data (see above)
{
  if(!muidll1) {
    int status = demangle();
    muidll1->DecodeStatus = status; 
  }
  if(!muidll1){
    std::cout<<"Failed to fill data structure"<<std::endl;
    return -1;
  }
  *nw=sizeof(muidll1);
  if(*nw>length*4) *nw=length*4;
  memcpy((void *)destination, (const void *)muidll1,*nw);
  *nw/=4;
  return 0;
}

void Packet_muid_ll1::dump ( OSTREAM &os){

  int m;
  this->identify(os); 
  if(!muidll1) {
    int status = demangle();
    muidll1->DecodeStatus = status;
  }
  if(!muidll1){
    os<<"Failed to fill muidll1. Exit"<<std::endl;
    return;
  }
  int i;
  unsigned int oldFlags;
  oldFlags=os.flags();
  char oldFill=os.fill('0');
  for(m=0;m<54;m++) os<<"=";
  os<<std::hex<<std::endl;
  os<<"MUID LL1 data packet:"<< std::endl;
  os<<"Header:"<<muidll1->header<<std::endl;
  os<<"Packet Decode Status = " << muidll1->DecodeStatus << std::endl << std::endl; 
  os<<"Chip Type:    "<<std::dec;
  for(i=0;i<6;i++)
    os<<" "<<muidll1->ChipType[i];
  os<<std::endl;

  os<<"Chip #        "<<std::dec;
  for(i=0;i<6;i++)
    os<<" "<<muidll1->AlgChip[i];
  os<<std::endl;

  os<<"Is Vertical?: "<<std::dec;
  for(i=0;i<6;i++)
    os<<" "<<muidll1->MUIDvert[i];
  os<<std::endl;

  os<<"Chip Version: "<<std::dec;
  for(i=0;i<6;i++)
    os <<" "<<muidll1->ChipVer[i];
  os <<std::endl;

  os<<"Chip scaler:  "<<std::dec;
  for(i=0;i<6;i++)
    os<<" "<<muidll1->Scaler[i];
  os<<std::endl;
  
  os<< std::endl <<"DEEP Roads: "
    <<"157 - 128 "<<"127 - 096 "<<"095 - 064 "<<"063 - 032 "<<"031 - 000"<<std::endl
    <<"             "<<SETW(8)<<std::hex<<muidll1->Road[4]<<"  "<<SETW(8)<<muidll1->Road[3]
    <<"  "<<SETW(8)<<muidll1->Road[2]<<"  "<<SETW(8)<<muidll1->Road[1]
    <<"  "<<SETW(8)<<muidll1->Road[0]<<std::dec
    <<std::endl << std::endl;
  //os.fill(oldFill);
  os<<"DEEP Partial sums:"<<std::endl;
  for(i=0;i<6;i++){
    if(muidll1->ChipType[i]==1)continue;
    os<<"Chip "<<muidll1->AlgChip[i]<<"="<<SETW(2)<<muidll1->PartialSum[i]<<std::endl;
  }
  os<<"DEEP Total Sum: "<<muidll1->Sum<<std::endl<<std::endl;
  
  if(muidll1->ChipVer[5]>=2){
    oldFill=os.fill('0');
    os<< std::endl <<"SHALLOW Roads: "
      <<"157 - 128 "<<"127 - 096 "<<"095 - 064 "<<"063 - 032 "<<"031 - 000"<<std::endl
      <<"                "<<SETW(8)<<std::hex<<muidll1->ShallowRoad[4]<<"  "<<SETW(8)<<muidll1->ShallowRoad[3]
      <<"  "<<SETW(8)<<muidll1->ShallowRoad[2]<<"  "<<SETW(8)<<muidll1->ShallowRoad[1]
      <<"  "<<SETW(8)<<muidll1->ShallowRoad[0]<<std::dec
      <<std::endl << std::endl;
    //os.fill(oldFill);
    os<<"SHALLOW Partial sums:"<<std::endl;
    for(i=0;i<6;i++){
      if(muidll1->ChipType[i]==1)continue;
      os<<"Chip "<<muidll1->AlgChip[i]<<"="<<SETW(2)<<muidll1->ShallowPartialSum[i]<<std::endl;
    }
    os<<"SHALLOW Total Sum: "<<muidll1->SSum<<std::endl<< std::endl;
  }

  char err;
  int c, fb;
  os.fill(' ');
  os<<"Fiber #    :";
  for(c=0;c<20;c++)
    os<<" "<<SETW(2)<<std::dec<<c;
  os<<std::endl;
    
  for(c=0;c<6;c++){
    if(muidll1->ChipType[c]==1)continue;
    if(muidll1->AlgChip[c]<1 || muidll1->AlgChip[c]>5)continue;
    os<<"SYNC Err["<<muidll1->AlgChip[c]<<"]:";
    for(fb=0;fb<20;fb++){
      if(!fiberAssign[muidll1->AlgChip[c]][fb])err='-';
      else {
	err=((muidll1->SyncErr[c]&(0x1<<fb))!=0)?'1':'0';
     }
      os<<"  "<<err;
    }
    os<<std::endl;
  }
  os << std::endl;
  for(c=0;c<6;c++){
    if(muidll1->ChipType[c]==1)continue;
    if(muidll1->AlgChip[c]<1 || muidll1->AlgChip[c]>5)continue;
    os<<"DATA Err["<<muidll1->AlgChip[c]<<"]:";
    for(fb=0;fb<20;fb++){
      if(!fiberAssign[muidll1->AlgChip[c]][fb])err='-';
      else {
	err=((muidll1->DataErr[c]&(0x1<<fb))!=0)?'1':'0';
      }
      os<<"  "<<err;
    }
    os<<std::endl;
  }

  os << std::endl;
  os.fill(oldFill);
  //os<<"Offboard Strip: 0x"<< std::hex << muidll1->Strip<<std::dec<<std::endl;
  //if(muidll1->MUIDvert[0])os<<"Vertical Strip: 0x";
  //else os<<"Horizontal Strip: 0x";
  //os<<std::hex<<muidll1->OrientStrip<<std::dec<<std::endl;
  os<<"Lut2: 0x"<<std::hex<<muidll1->Lut2<<std::dec<<std::endl;
  os<<"Mode Bits: 0x"<< std::hex <<muidll1->ModeBits<< std::dec << std::endl;
  //os<<"Strip Lut: "<<muidll1->StripLut<<std::endl;
  os<<"Muon Alignment : "<<muidll1->MuonAlign<<std::endl;

  if(muidll1->ChipVer[5]==2){

    os<< std::endl<< "Cosmic Trigger Sums:" << std::endl;
    os<< "Upper East: " << muidll1->UE_sum[0]<<"("<<muidll1->UE_sum[1]<<")"<<std::endl;
    os<< "Upper West: " << muidll1->UW_sum[0]<<"("<<muidll1->UW_sum[1]<<")"<<std::endl;
    os<< "Lower East: " << muidll1->LE_sum[0]<<"("<<muidll1->LE_sum[1]<<")"<<std::endl;
    os<< "Lower West: " << muidll1->LW_sum[0]<<"("<<muidll1->LW_sum[1]<<")"<<std::endl;

    if(muidll1->MUIDvert[5]==0) {
      os<<std::endl<<"Cosmic Bits Out : 0x" <<std::hex<< muidll1->cosmic_out<<std::dec << std::endl;
      if(muidll1->cosmic_out&0x1) os<<" LOWER EAST quadrant fired. " << std::endl;
      if(muidll1->cosmic_out&0x2) os<<" LOWER WEST quadrant fired. " << std::endl;
      if(muidll1->cosmic_out&0x4) os<<" UPPER EAST quadrant fired. " << std::endl;
      if(muidll1->cosmic_out&0x8) os<<" UPPER WEST quadrant fired. " << std::endl;
    }
  }

  if(muidll1->ChipVer[5]==3){

    os << std::endl; 
    if(muidll1->CtlSqCsmcSel==1)
      os << "Control Chip SQUARE HOLE Sums Selected" << std::endl; 
    else
      os << "Control Chip COSMIC Sums Selected" << std::endl; 

    if(muidll1->AlgSqCsmcSel[0]==1)
      os << "Alg. Chip 2 SQUARE HOLE Sums Selected" << std::endl; 
    else
      os << "Alg. Chip 2 COSMIC Sums Selected" << std::endl; 

    if(muidll1->AlgSqCsmcSel[1]==1)
      os << "Alg. Chip 4 SQUARE HOLE Sums Selected" << std::endl; 
    else
      os << "Alg. Chip 4 COSMIC Sums Selected" << std::endl; 

    if(muidll1->CtlSqCsmcSel==0){

      if(muidll1->MUIDvert[5]==0) {
        os<< std::endl<< "Cosmic Trigger Sums:" << std::endl;
        os<< "Upper East: " << muidll1->UE_sum[0];
        if(muidll1->AlgSqCsmcSel[1]==0) os <<"("<<muidll1->UE_sum[1]<<")";
        os <<std::endl;
        os << "Upper West: " << muidll1->UW_sum[0];
        if(muidll1->AlgSqCsmcSel[1]==0) os<<"("<<muidll1->UW_sum[1]<<")";
        os <<std::endl;
        os<< "Lower East: " << muidll1->LE_sum[0];
        if(muidll1->AlgSqCsmcSel[0]==0) os <<"("<<muidll1->LE_sum[1]<<")";
        os <<std::endl;
        os<< "Lower West: " << muidll1->LW_sum[0];
        if(muidll1->AlgSqCsmcSel[0]==0) os <<"("<<muidll1->LW_sum[1]<<")";
        os <<std::endl;
      }
      else{
        os<< std::endl<< "Cosmic Trigger Sums:" << std::endl;
        os<< "Upper East: " << muidll1->UE_sum[0];
        if(muidll1->AlgSqCsmcSel[0]==0) os <<"("<<muidll1->UE_sum[1]<<", 2-bit partial)";
        os <<std::endl;
        os << "Upper West: " << muidll1->UW_sum[0];
        if(muidll1->AlgSqCsmcSel[1]==0) os<<"("<<muidll1->UW_sum[1]<<", 2-bit partial)";
        os <<std::endl;
        os<< "Lower East: " << muidll1->LE_sum[0];
        if(muidll1->AlgSqCsmcSel[0]==0) os <<"("<<muidll1->LE_sum[1]<<", 2-bit partial)";
        os <<std::endl;
        os<< "Lower West: " << muidll1->LW_sum[0];
        if(muidll1->AlgSqCsmcSel[1]==0) os <<"("<<muidll1->LW_sum[1]<<", 2-bit partial)";
        os <<std::endl;
      }

    }
    else{
      if(muidll1->MUIDvert[5]==0) {
        os<< std::endl<< "CTL Chip Square Hole Trigger Sums:" << std::endl;
        os<< "Square Hole Deep: " << muidll1->SqHole[0] <<std::endl;
        os<< "Square Hole Shallow: " << muidll1->SqHoleShal[0] <<std::endl;
      }
      else{
        os<< std::endl<< "CTL Chip Square Hole Trigger Sums:" << std::endl;
        os<< "Square Hole Deep: " << muidll1->SqHole[0] << "(" 
	  << muidll1->SqHole[1] << "+" 
	  << muidll1->SqHole[2] << "+" 
	  << muidll1->SqHole[3] << ")" 
	  <<std::endl;
        os<< "Square Hole Shallow: " << muidll1->SqHoleShal[0] << "("
	  << muidll1->SqHole[1] << "+" 
	  << muidll1->SqHole[2] << "+" 
	  << muidll1->SqHole[3] << ")" 
	  <<std::endl;
      }
    }
    
    if(muidll1->MUIDvert[5]==0) {
      os<<std::endl<<"Cosmic Bits Out : 0x" <<std::hex<< muidll1->cosmic_out<<std::dec << std::endl;
      if(muidll1->cosmic_out&0x1) os<<" LOWER EAST quadrant fired. " << std::endl;
      if(muidll1->cosmic_out&0x2) os<<" LOWER WEST quadrant fired. " << std::endl;
      if(muidll1->cosmic_out&0x4) os<<" UPPER EAST quadrant fired. " << std::endl;
      if(muidll1->cosmic_out&0x8) os<<" UPPER WEST quadrant fired. " << std::endl;
    }

  }

  os<<std::endl;

  // Finally, a generic HEX dump:
  
  int j,l,dlength;
  dlength = getDataLength();
  j = 0;
  int* k=( int *) findPacketDataStart(packet);
  if ( k ==0 ) return;
  os.fill('0');
  while (1)
    {
      os << std::endl << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<4;l++)
	{
	  os << std::hex << SETW(8) << k[j++] << " ";
	  if (j>=dlength) break;
	}
      if (j>=dlength) break;
    }	
  os << std::endl;
  for(m=0;m<54;m++) os<<"=";
  os << std::endl;
  os.fill(oldFill);
}
