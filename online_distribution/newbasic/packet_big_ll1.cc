#include <packet_big_ll1.h>
#include <string.h>

Packet_big_ll1::Packet_big_ll1(PACKET_ptr data) : Packet_w4 (data)
{
  muidll1_nv = NULL;
  muidll1_nh = NULL;
  muidll1_sv = NULL;
  muidll1_sh = NULL;
  zdclvl1 = NULL; 
}

Packet_big_ll1::~Packet_big_ll1()
{
  if(muidll1_nv) delete muidll1_nv;
  if(muidll1_sv) delete muidll1_sv;
  if(muidll1_nh) delete muidll1_nh;
  if(muidll1_sh) delete muidll1_sh;
  if(zdclvl1) delete zdclvl1;
}

int *Packet_big_ll1::decode (int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_big_ll1( temp
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

int Packet_big_ll1::iValue(const int ich, const char *what){
  
  if(!zdclvl1) {
    fillDataStructures(std::cout);
    if(!zdclvl1){
      std::cout << "Failed to fill data structures." << std::endl;
      return -1;
    }
  }

  if ( strcmp(what,"ZDCVTX")==0)
    {
      return (int) zdclvl1->Vertex;
    }
  else if ( strcmp(what,"ZDCAOK")==0)
    {
      return zdclvl1->VTXA_OK;
    }
  else if ( strcmp(what,"ZDCBOK")==0)
    {
      return zdclvl1->VTXB_OK;
    }
  else if ( strcmp(what,"ZDC_SOUTH_TDC_OK")==0)
    {
      return zdclvl1->SouthTDC_OK;
    }
  else if ( strcmp(what,"ZDC_NORTH_TDC_OK")==0)
    {
      return zdclvl1->NorthTDC_OK;
    }    
  else if ( strcmp(what,"ZDC_NORTH_TDC_OK")==0)
    {
      return zdclvl1->NorthTDC_OK;
    }    
  else if(strcmp(what,"ZDC_DCODESTAT")==0) 
    {
      return zdclvl1->DecodeStatus;
    }  
  else if(strcmp(what,"ROADS")==0) 
    { 
      if(ich<0 || ich>19) return -1;
      if(ich>=0 && ich<=4 && muidll1_sh) return muidll1_sh->Road[ich];
      else if(ich>=5 && ich<=9 && muidll1_sv) return muidll1_sv->Road[ich-5];
      else if(ich>=10 && ich<=14 && muidll1_nh) return muidll1_nh->Road[ich-10]; 
      else if(ich>=15 && ich<=19 && muidll1_nv) return muidll1_nv->Road[ich-15]; 
      else
	return -1; 
     } 
  else if(strcmp(what,"SROADS")==0) 
    {
      if(ich<0 || ich>19) return -1;
      if(ich>=0 && ich<=4 && muidll1_sh) return muidll1_sh->ShallowRoad[ich];
      else if(ich>=5 && ich<=9 && muidll1_sv) return muidll1_sv->ShallowRoad[ich-5];
      else if(ich>=10 && ich<=14 && muidll1_nh) return muidll1_nh->ShallowRoad[ich-10]; 
      else if(ich>=15 && ich<=19 && muidll1_nv) return muidll1_nv->ShallowRoad[ich-15]; 
      else
	return -1; 
    } 
  else if(strcmp(what,"PARTSUM")==0) 
    {
      if(ich<0 || ich>23) return -1;
      if(ich>=0 && ich<=5 && muidll1_sh) return muidll1_sh->PartialSum[ich];
      else if(ich>=6 && ich<=11 && muidll1_sv) return muidll1_sv->PartialSum[ich-6];
      else if(ich>=12 && ich<=17 && muidll1_nh) return muidll1_nh->PartialSum[ich-12];  
      else if(ich>=18 && ich<=23 && muidll1_nv) return muidll1_nv->PartialSum[ich-18];
      else
	return -1;
    } 
  else if(strcmp(what,"DATAERR")==0) 
    {
      if(ich<0 || ich>19) return -1;
      if(ich>=0 && ich<=4 && muidll1_sh) return muidll1_sh->DataErr[ich+1];
      else if(ich>=5 && ich<=9 && muidll1_sv) return muidll1_sv->DataErr[ich-5+1];
      else if(ich>=10 && ich<=14 && muidll1_nh) return muidll1_nh->DataErr[ich-10+1]; 
      else if(ich>=15 && ich<=19 && muidll1_nv) return muidll1_nv->DataErr[ich-15+1]; 
      else 
	return -1;
    } 
  else if(strcmp(what,"SYNCERR")==0) 
    {
      if(ich<0 || ich>19) return -1;
      if(ich>=0 && ich<=4 && muidll1_sh) return muidll1_sh->SyncErr[ich+1]; 
      else if(ich>=5 && ich<=9 && muidll1_sv) return muidll1_sv->SyncErr[ich-5+1]; 
      else if(ich>=10 && ich<=14 && (muidll1_nh)) return muidll1_nh->SyncErr[ich-10+1];  
      else if(ich>=15 && ich<=19 && muidll1_nv) return muidll1_nv->SyncErr[ich-15+1];
      else
	return -1;
    } 
  else if(strcmp(what,"STRIP")==0) 
    {    
      if(ich<0 || ich>3) return -1;
      if(ich==0 && muidll1_sh) return muidll1_sh->Strip;
      else if(ich==1 && muidll1_sv) return muidll1_sv->Strip;
      else if(ich==2 && muidll1_nh) return muidll1_nh->Strip;
      else if(ich==3 && muidll1_nv) return muidll1_nv->Strip;
      else
	return -1;
    } 
  else if(strcmp(what,"ORIENTSTRIP")==0) 
    {
      if(ich<0 || ich>3) return -1;
      if(ich==0 && muidll1_sh) return muidll1_sh->OrientStrip;
      else if(ich==1 && muidll1_sv) return muidll1_sv->OrientStrip;
      else if(ich==2 && muidll1_nh) return muidll1_nh->OrientStrip;
      else if(ich==3 && muidll1_nv) return muidll1_nv->OrientStrip;
      else
	return -1;
    } 
  else if(strcmp(what,"LUT2")==0) 
    {
      if(ich<0 || ich>3) return -1;
      if(ich==0 && muidll1_sh) return muidll1_sh->Lut2;
      else if(ich==1 && muidll1_sv) return muidll1_sv->Lut2;
      else if(ich==2 && muidll1_nh) return muidll1_nh->Lut2;
      else if(ich==3 && muidll1_nv) return muidll1_nv->Lut2;
      else
	return -1; 
    } 
  else if(strcmp(what,"TOTALSUM")==0) 
    {
      if(ich<0 || ich>3) return -1;
      if(ich==0 && muidll1_sh) return muidll1_sh->Sum;
      else if(ich==1 && muidll1_sv) return muidll1_sv->Sum;
      else if(ich==2 && muidll1_nh) return muidll1_nh->Sum;
      else if(ich==3 && muidll1_nv) return muidll1_nv->Sum;
      else 
	return -1; 
    } 
  else if(strcmp(what,"CHIPTYPE")==0) 
    {
      if(ich<0 || ich>23) return -1;
      if(ich>=0 && ich<=5 && muidll1_sh) return muidll1_sh->ChipType[ich]; 
      else if(ich>=5 && ich<=11 && muidll1_sv) return muidll1_sv->ChipType[ich-6];
      else if(ich>=12 && ich<=17 && muidll1_nh) return muidll1_nh->ChipType[ich-12]; 
      else if(ich>=18 && ich<=23 && muidll1_nv) return muidll1_nv->ChipType[ich-18];
      else
	return -1;
    } 
  else if(strcmp(what,"ALGCHIP")==0) 
    {
      if(ich<0 || ich>23) return -1;
      if(ich>=0 && ich<=5 && muidll1_sh) return muidll1_sh->AlgChip[ich];
      else if(ich>=6 && ich<=11 && muidll1_sv) return muidll1_sv->AlgChip[ich-6]; 
      else if(ich>=12 && ich<=17 && muidll1_nh) return muidll1_nh->AlgChip[ich-12];  
      else if(ich>=18 && ich<=23 && muidll1_nv) return muidll1_nv->AlgChip[ich-18];  
      else
	return -1; 
    } 
  else if(strcmp(what,"CHIPVER")==0) 
    {
      if(ich<0 || ich>23) return -1;
      if(ich>=0 && ich<=5 && muidll1_sh) return muidll1_sh->ChipVer[ich];
      else if(ich>=6 && ich<=11 && muidll1_sv) return muidll1_sv->ChipVer[ich-6];
      else if(ich>=12 && ich<=17 && muidll1_nh) return muidll1_nh->ChipVer[ich-12]; 
      else if(ich>=18 && ich<=23 && muidll1_nv) return muidll1_nv->ChipVer[ich-18];
      else
	return -1; 
    } 
  else if(strcmp(what,"MUIDVERT")==0) 
    {
      if(ich<0 || ich>23) return -1;
      if(ich>=0 && ich<=5 && muidll1_sh) return muidll1_sh->MUIDvert[ich];
      else if(ich>=6 && ich<=11 && muidll1_sv) return muidll1_sv->MUIDvert[ich-6]; 
      else if(ich>=12 && ich<=17 && muidll1_nh) return muidll1_nh->MUIDvert[ich-12];  
      else if(ich>=18 && ich<=23 && muidll1_nv) return muidll1_nv->MUIDvert[ich-18];
      else
	return -1; 
    } 
  else if(strcmp(what,"SCALER")==0) 
    {
      if(ich<0 || ich>23) return -1;
      if(ich>=0 && ich<=5 && muidll1_sh) return muidll1_sh->Scaler[ich];
      else if(ich>=6 && ich<=11 && muidll1_sv) return muidll1_sv->Scaler[ich-6];
      else if(ich>=12 && ich<=17 && muidll1_nh) return muidll1_nh->Scaler[ich-12]; 
      else if(ich>=18 && ich<=23 && muidll1_nv) return muidll1_nv->Scaler[ich-18]; 
      else
	return -1; 
    } 
  else if(strcmp(what,"STRIPLUT")==0) 
    {
      if(ich<0 || ich>3) return -1;
      if(ich==0 && muidll1_sh) return muidll1_sh->StripLut; 
      else if(ich==1 && muidll1_sv) return muidll1_sv->StripLut; 
      else if(ich==2 && muidll1_nh) return muidll1_nh->StripLut;
      else if(ich==3 && muidll1_nv) return muidll1_nv->StripLut;
      else
	return -1; 
    } 
  else if(strcmp(what,"SHALLOWSUM")==0) 
    {
      if(ich<0 || ich>3) return -1;
      if(ich==0 && muidll1_sh) return muidll1_sh->SSum; 
      else if(ich==1 && muidll1_sv) return muidll1_sv->SSum; 
      else if(ich==2 && muidll1_nh) return muidll1_nh->SSum; 
      else if(ich==3 && muidll1_nv) return muidll1_nv->SSum;
      else 
	return -1; 
    } 
  else if(strcmp(what,"DLTUBE")==0) 
    {
      if(ich<0 || ich>19) return -1;
      int bitvector = 0; 
      for(int i=0; i<13; i++)
	{
	  if(ich>=0 && ich<=4)
	    {
	      if(muidll1_sh) 
		{
		  if(muidll1_sh->diagLT[ich][i]>0) bitvector |= (0x1<<i);
		}
	    }
	  if(ich>=5 && ich<=9)
	    {
	      if(muidll1_sv) 
		{
		  if(muidll1_sv->diagLT[ich-5][i]>0) bitvector |= (0x1<<i);
		}
	    }
	  if(ich>=10 && ich<=14)
	    {
	      if(muidll1_nh) 
		{
		  if(muidll1_nh->diagLT[ich-10][i]>0) bitvector |= (0x1<<i);
		}
	    }
	  if(ich>=15 && ich<=19)
	    {
	      if(muidll1_nv) 
		{
		  if(muidll1_nv->diagLT[ich-15][i]>0) bitvector |= (0x1<<i);
		}
	    }
	}
      return bitvector; 
    } 
  else if(strcmp(what,"MUID_DCODESTAT")==0) 
    {
      if(ich<0 || ich>3) return -1;
      if((ich==0) && (muidll1_sh)) return muidll1_sh->DecodeStatus; 
      else if((ich==1) && (muidll1_sv)) return muidll1_sv->DecodeStatus;
      else if((ich==2) && (muidll1_nh)) return muidll1_nh->DecodeStatus; 
      else if((ich==3) && (muidll1_nv)) return muidll1_nv->DecodeStatus;
      else
        return -1; 
    }

  std::cout <<"iValue(int ich, char* what): Unrecognized value "<< what << std::endl;

  return -1;

}

void Packet_big_ll1::dump ( OSTREAM &os){

  unsigned int *k;
  char oldFill;

  this->identify(os);
  int dlength = getDataLength();
  k= (unsigned int *) findPacketDataStart(packet);
  if (k == 0) return;

  // ZDC LL1 decoding:

  if(!zdclvl1) {
    fillDataStructures(os);
    if(!zdclvl1){
      os << "Failed to fill data structures" << std::endl;
      return;
    }
  }

  oldFill=os.fill('0');
  int m;
  for(m=0;m<54;m++) os<<"_";
  os <<std::endl;
	
  os << "BIG LL1 data packet (ZDC LL1, MuID LL1 North and South): ";
  os << std::endl;
  os<<"Number of words(32) is "<<dlength<< "." <<std::endl;
  if(dlength==0) return;
  os<<"DCM Check Word 1 = 0x"<< std::hex <<SETW(8)<< k[dlength-2] <<std::endl;  
  os<<"DCM Check Word 2 = 0x"<< std::hex <<SETW(8)<< k[dlength-1] <<std::endl;
  os<< std::endl;
  if(dlength<4) {
    os<<" Data length less than four words (ZDCLL1), bailing out!" <<std::endl;  
    return; 
  }

  os<<"ZDC LL1 Accepted Event Data:" <<std::endl;
  for(m=0;m<54;m++) os<<"_";
  os <<std::endl<<std::endl;

  os << "Header      = 0x" << std::hex << SETW(4) << zdclvl1->header << std::endl;
  os << "CTL Chip ESN= 0x" << std::hex << SETW(1) << zdclvl1->scaler[0] << std::endl;
  os << "ALG Chip ESN= 0x" << std::hex << SETW(1) << zdclvl1->scaler[1] << std::endl << std::endl;

  os << "CTL Chip Accept Counter= 0x" << std::dec << zdclvl1->accept << std::endl << std::endl;
  
  os << "Control Chip Mode Bits:" << std::endl;
  os << "MODE 1      = 0x" << std::hex << SETW(1) << zdclvl1->mode1 << std::endl;
  os << "MODE 5      = 0x" << std::hex << SETW(1) << zdclvl1->mode5 << std::endl;
  os << "MODE 6      = 0x" << std::hex << SETW(1) << zdclvl1->mode6 << std::endl;
  os << "MODE 7      = 0x" << std::hex << SETW(1) << zdclvl1->mode7 << std::endl;
  os << "MODE 8(ENBL)= 0x" << std::hex << SETW(1) << zdclvl1->mode8 << std::endl << std::endl;

  os << "ZDCA  = 0x" << std::hex << SETW(1) << zdclvl1->ZDCA << std::endl;
  os << "ZDCB  = 0x" << std::hex << SETW(1) << zdclvl1->ZDCB << std::endl;
  os << std::endl;
  
  os << "ZDC Alg. Ver= 0x" << std::hex << SETW(1) << zdclvl1->AlgVer << std::endl;
  os << "ZDC Vertex  = "<< std::dec << (int) zdclvl1->Vertex << std::endl;
  os << "ZDC South TDC OK = " << SETW(1) << zdclvl1->SouthTDC_OK << std::endl; 
  os << "ZDC North TDC OK = " << SETW(1) << zdclvl1->NorthTDC_OK << std::endl; 
  os << "ZDCA Vtx.OK = 0x" << std::hex << SETW(1) <<  zdclvl1->VTXA_OK << std::endl;
  os << "ZDCB Vtx.OK = 0x" << std::hex << SETW(1) <<  zdclvl1->VTXB_OK << std::endl;
  os << std::endl;

  // MuID Data (if it exists)

  if(muidll1_sh) printOutMuid(muidll1_sh,os);
  if(muidll1_sv) printOutMuid(muidll1_sv,os);
  if(muidll1_nh) printOutMuid(muidll1_nh,os);
  if(muidll1_nv) printOutMuid(muidll1_nv,os);

  // Finally, a generic HEX dump:
  
  for(m=0;m<54;m++) os<<"_";
  os <<std::endl;

  os <<"Generic Hex Dump:" <<std::endl;

  int j,l;
  j = 0;
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

  for(m=0;m<54;m++) os<<"_";
  os <<std::endl;
  os.fill(oldFill);

}

int Packet_big_ll1::zdclvl1_demangle(unsigned int *buf, int ShortWord,OSTREAM &os){

  if(zdclvl1) delete zdclvl1;
  zdclvl1 = new ZDC_BIGLL1_BOARD;
  if(!zdclvl1){
    COUT<<"can't allocate memory for ZDC_BIGLL1_BOARD structure "<<std::endl;
    return -1;
  }
  memset(zdclvl1,0,sizeof(ZDC_BIGLL1_BOARD));

  if (buf == 0) return -1;

  int base = ShortWord/2;
  int shift = (int) 16* (ShortWord%2);

  // unpack the data into a ZDC structure:

  // header: 
  zdclvl1->header = (buf[base]>>(shift))&0xFFFF;

  ShortWord++; 
  base = ShortWord/2;
  shift = (int) 16* (ShortWord%2);

  zdclvl1->accept = (buf[base]>>(shift+8));
  zdclvl1->mode1 = (buf[base]>>(shift))&0x1; 
  zdclvl1->mode5 = (buf[base]>>(shift+2))&0x1; 
  zdclvl1->mode6 = (buf[base]>>(shift+3))&0x1; 
  zdclvl1->mode7 = (buf[base]>>(shift+4))&0x1; 
  zdclvl1->mode8 = (buf[base]>>(shift+5))&0x1; 
 
  ShortWord++; 
  base = ShortWord/2;
  shift = (int) 16* (ShortWord%2);

  zdclvl1->scaler[0] = (buf[base]>>(shift+14));
  zdclvl1->ZDCA = (buf[base]>>(shift+8));
  zdclvl1->ZDCA = (buf[base]>>(shift+9));

  ShortWord+=2; 
  base = ShortWord/2;
  shift = (int) 16* (ShortWord%2);

  zdclvl1->Vertex=(buf[base]>>(shift+8))&0xFF;

  ShortWord++; 
  base = ShortWord/2;
  shift = (int) 16* (ShortWord%2);

  zdclvl1->scaler[1] = (buf[base]>>(shift+10));
  zdclvl1->AlgVer = (buf[base]>>(shift+6));

  zdclvl1->SouthTDC_OK = (buf[base]>>(shift))&0x1;
  zdclvl1->NorthTDC_OK = (buf[base]>>(shift+1))&0x1;
  zdclvl1->VTXA_OK = (buf[base]>>(shift+4))&0x1;
  zdclvl1->VTXB_OK = (buf[base]>>(shift+5))&0x1;

  return 0; 

}

int Packet_big_ll1::horiz_muidlvl1_demangle(MUID_BIGLL1_BOARD *&muidll1, unsigned int *buf, int shortWord,OSTREAM &os){
  int i,j;
  if(muidll1) delete muidll1;
  muidll1 = new MUID_BIGLL1_BOARD;
  if(!muidll1){
    COUT<<"can't allocate memory for MUID_BIGLL1_BOARD structure "<<std::endl;
    return -1;
  }
  memset(muidll1,0,sizeof(MUID_BIGLL1_BOARD));
  int fiberMap[6][20]={
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},//no chip
    {1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0},//chip 1
    {1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0},//chip 2
    {0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1},//chip 3
    {0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1},//chip 4
    {0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1} //chip 5
  };

  for(i=0;i<6;i++)
    for(j=0;j<20;j++)
      fiberAssignHoriz[i][j]=fiberMap[i][j];

  for(i=0;i<5;i++){
    muidll1->Road[i] = 0;
    muidll1->ShallowRoad[i] = 0;
  }

  if (buf == 0) return -1;

  //  we must be very careful in what follows - we need to 
  //  be totally flexible in how we handle variations in the 
  //  output format that can occur with different versions of the 
  //  FPGA code.

  int base = shortWord/2;
  int shift = (int) 16* (shortWord%2);

  // unpack the data into a MUID structure:

  // header: 
  muidll1->header = (buf[base]>>(shift))&0xFFFF;
  shortWord++;

  // new header word:
  for(i=0;i<6;i++){

    base = shortWord/2;
    shift = (int) 16* (shortWord%2);

    // Chip Version:
    muidll1->ChipVer[i] = (buf[base]>>(shift))&0xFF;
    // Algorithm Chip:
    muidll1->AlgChip[i] = (buf[base]>>(10+shift))&0x7;
   // MUID Vertical Orientation?
    muidll1->MUIDvert[i] = (buf[base]>>(13+shift))&0x1;
    // Chip Type:
    muidll1->ChipType[i] = (buf[base]>>(14+shift))&0x3; 

    if(muidll1->ChipType[i]==1){

      if((muidll1->AlgChip[i])==1){
	
	switch(muidll1->ChipVer[i]){

	case 1:
       
        //Control chip
	  
	// set base and shift to get the data we want from the 
        // words in order

	// Strip bits
	base = (shortWord+1)/2;
	shift = (int)16*((shortWord+1)%2);
        muidll1->OrientStrip = (buf[base]>>(8+shift))&0xFF;
        muidll1->Strip = (buf[base]>>(8+shift))&0xFF;

	// LUT2 output
	base = (shortWord+2)/2;
	shift = 16*((shortWord+2)%2);
        muidll1->Lut2 = (buf[base]>>(shift))&0xFFFF;
	  
	base = (shortWord+3)/2;
	shift = 16*((shortWord+3)%2);
        // Mode Bits:
        muidll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        // MUON Aligned
        muidll1->MuonAlign = (buf[base]>>(13+shift))&0x1;
        // Strip Lut:
        muidll1->StripLut = (buf[base]>>(15+shift))&0x1;

	base = (shortWord+4)/2;
	shift = 16*((shortWord+4)%2);
    
        muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;  
        muidll1->Sum = (buf[base]>>(shift))&0xFF;

	// no partial sum for control chip
	muidll1->PartialSum[i] = 0;

	// skip based on data size
        shortWord += 5;

	break;

	case 2:

	// LUT2 output
	base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
	muidll1->UE_sum[0] = (buf[base]>>(shift + 11))&0x7;
	muidll1->UW_sum[0] = (buf[base]>>(shift + 8))&0x7;
	muidll1->SSum = (buf[base]>>(shift))&0xFF;

	base = (shortWord+2)/2;
	shift = 16*((shortWord+2)%2);
        muidll1->Lut2 = (buf[base]>>(shift + 6))&0x3F;
	muidll1->LE_sum[0] = (buf[base]>>(shift + 3))&0x7;
	muidll1->LW_sum[0] = (buf[base]>>(shift))&0x7;
	  
	base = (shortWord+3)/2;
	shift = 16*((shortWord+3)%2);
        // Mode Bits:
        muidll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        // MUON Aligned
        muidll1->MuonAlign = (buf[base]>>(13+shift))&0x1;
        // Strip Lut:
        muidll1->StripLut = (buf[base]>>(15+shift))&0x1;

	base = (shortWord+4)/2;
	shift = 16*((shortWord+4)%2);
        muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;  
        muidll1->Sum = (buf[base]>>(shift))&0xFF;
	muidll1->cosmic_out =  (buf[base]>>(shift+9))&0xF;
	
	// skip based on data size
        shortWord += 5;

	break;

	case 3:
	  
	base = (shortWord+3)/2;
	shift = 16*((shortWord+3)%2);
        // Mode Bits:
        muidll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        // MUON Aligned
        muidll1->MuonAlign = (buf[base]>>(13+shift))&0x1;
        // Strip Lut:
        muidll1->StripLut = (buf[base]>>(15+shift))&0x1;

	// Cosmic Select bit
	muidll1->CtlSqCsmcSel = (buf[base]>>(12+shift))&0x1;

	if(muidll1->CtlSqCsmcSel==0){
	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->UE_sum[0] = (buf[base]>>(shift + 11))&0x7;
	  muidll1->UW_sum[0] = (buf[base]>>(shift + 8))&0x7;
	  muidll1->SSum = (buf[base]>>(shift))&0xFF;

	  base = (shortWord+2)/2;
	  shift = 16*((shortWord+2)%2);
          muidll1->Lut2 = (buf[base]>>(shift + 6))&0x3F;
	  muidll1->LE_sum[0] = (buf[base]>>(shift + 3))&0x7;
	  muidll1->LW_sum[0] = (buf[base]>>(shift))&0x7;
	}
	else{
	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->SqHole[0] = (buf[base]>>(shift + 8))&0x3F;
	  muidll1->SSum = (buf[base]>>(shift))&0xFF;

	  base = (shortWord+2)/2;
	  shift = 16*((shortWord+2)%2);
	  muidll1->SqHoleShal[0] = (buf[base]>>(shift))&0x3F;
          muidll1->Lut2 = (buf[base]>>(shift + 6))&0x3F;
	}

	base = (shortWord+4)/2;
	shift = 16*((shortWord+4)%2);
        muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;  
        muidll1->Sum = (buf[base]>>(shift))&0xFF;
	muidll1->cosmic_out =  (buf[base]>>(shift+9))&0xF;
	
	// skip based on data size
        shortWord += 5;

	break; 

	default:
	os << "packet_muid_hor_ll1: unknown control chip version" << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1; 
	break;
  
	}
    
      }
      else{
        os << "packet_muid_hor_ll1: unknown algorithm id type for control chip data" 
		  << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1;
      }

    }
    else if(muidll1->ChipType[i]==0) {

      // Algorithm chip types

      // Check valid version numbers

      if( (muidll1->ChipVer[i]>3) || (muidll1->ChipVer[i]<0) ){
        os << "packet_muid_hor_ll1:unknown algorithm version for chip = "
		  << muidll1->AlgChip[i] << " = " << muidll1->ChipVer[i] << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1; 
      }

      // Decode common elements first

      if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

        // Partial Sum
        base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
        muidll1->PartialSum[i] = (buf[base]>>shift)&0x1F;

	// Fiber Flags
	base = (shortWord+2)/2;
	shift = 16*((shortWord+2)%2);
	unsigned int CheckWord = (buf[base]>>shift)&0xFFFF;

	int bitnum = 0;
	for(j=0; j<20; j++){ 
	  if(fiberMap[muidll1->AlgChip[i]][j]==1){
      	    if(CheckWord&(0x1<<bitnum)) muidll1->SyncErr[i] |= (0x1<<j);
	    if(CheckWord&(0x1<<(bitnum+1))) muidll1->DataErr[i] |= (0x1<<j);
	    bitnum+=2;
	    if(bitnum>15){
	      bitnum = 0; 
	      base = (shortWord+3)/2;
	      shift = 16*((shortWord+3)%2);
	      CheckWord = (buf[base]>>shift)&0xFFFF;	
	    }
	  }
	}

      }
      if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)){

        // Partial Sum
        base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
        muidll1->ShallowPartialSum[i] = (buf[base]>>(shift+8))&0x1F;

      }

      switch (muidll1->AlgChip[i]) {

      case 1:

	if( (muidll1->ChipVer[i]<=2) && (muidll1->ChipVer[i]>0) ){
		
	  // Road Words

	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[0] |= (((buf[base]>>shift))&0xFFFF);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[0] |= (((buf[base]>>shift)<<16)&0x3FF0000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if(muidll1->ChipVer[i]==2){
	  
	  // Shallow Roads

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[0] |= (((buf[base]>>shift))&0xFFFF);
	  
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[0] |= (((buf[base]>>shift)<<16)&0x3FF0000);

	  // diagnostic tubes

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[0][0] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
      	  muidll1->diagLT[0][1] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
      	  muidll1->diagLT[0][2] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[0][3] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[0][4] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[0][5] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[0][6] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
      	  
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[0][7] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[0][8] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[0][9] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[0][10] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[0][11] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[0][12] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	}

	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if(muidll1->ChipVer[i]==2) 
	  shortWord += 8;
	else {
	  os << "packet_muid_hor_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;
	  
      case 2:
	
	if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

	  // Road words

	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[0] |= (((buf[base]>>shift)<<26)&0xFC000000);
	  muidll1->Road[1] |= (((buf[base]>>(6+shift)))&0x3FF);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[1] |= (((buf[base]>>shift)<<10)&0xFC00);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)){

	  // Shallow Roads

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[0] |= (((buf[base]>>shift)<<26)&0xFC000000);
	  muidll1->ShallowRoad[1] |= (((buf[base]>>(6+shift)))&0x3FF);
	  
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[1] |= (((buf[base]>>shift)<<10)&0xFC00);

	  if(muidll1->ChipVer[i]==2){  

	    // Cosmic sums 
	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->LE_sum[1] = (buf[base]>>(shift+12))&0x7;
	    muidll1->LW_sum[1] = (buf[base]>>(shift+8))&0x7;

	  }
	  else if(muidll1->ChipVer[i]==3){

	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->AlgSqCsmcSel[0] = (buf[base]>>(shift+7))&0x1;

	    if(muidll1->AlgSqCsmcSel[0]==0) {	    
	      // Cosmic sums 
	      muidll1->LE_sum[1] = (buf[base]>>(shift+11))&0x7;
	      muidll1->LW_sum[1] = (buf[base]>>(shift+8))&0x7;
	    }
	    else{
	      // Square hole sums
	      muidll1->SqHole[1] = (buf[base]>>(shift+12))&0xF;
	      muidll1->SqHoleShal[1] = (buf[base]>>(shift+8))&0xF;
	    }
	   
	  }

	  // diagnostic tubes

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[1][0] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
      	  muidll1->diagLT[1][1] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
      	  muidll1->diagLT[1][2] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[1][3] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[1][4] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[1][5] = (((buf[base]>>shift)&0x0100)!=0) ? 1:0;
	  muidll1->diagLT[1][6] = (((buf[base]>>shift)&0x0200)!=0) ? 1:0;
	  muidll1->diagLT[1][7] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[1][8] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[1][9] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[1][10] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[1][11] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[1][12] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	}

	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if((muidll1->ChipVer[i]>=2)||(muidll1->ChipVer[i]>=2))
	  shortWord += 8;
	else {
	  os << "packet_muid_hor_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;

      case 3:

	if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

	  // Road words
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[1] |= (((buf[base]>>shift)<<16)&0xFFFF0000);

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[2] |= (((buf[base]>>shift))&0x3F);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)){

	  // Shallow Roads
 
	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[1] |= (((buf[base]>>shift)<<16)&0xFFFF0000);

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[2] |= (((buf[base]>>shift))&0x3F);

	  if(muidll1->ChipVer[i]==3){
	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->SqHoleShal[2] = (buf[base]>>(shift+8)&0xF);
	    muidll1->SqHole[2] = (buf[base]>>(shift+8)&0xF);
	  }

	  // diagnostic tubes

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[2][0] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
      	  muidll1->diagLT[2][1] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
      	  muidll1->diagLT[2][2] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[2][3] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[2][4] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[2][5] = (((buf[base]>>shift)&0x0100)!=0) ? 1:0;
	  muidll1->diagLT[2][6] = (((buf[base]>>shift)&0x0200)!=0) ? 1:0;
	  muidll1->diagLT[2][7] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[2][8] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[2][9] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[2][10] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[2][11] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[2][12] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	}

	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)) 
	  shortWord += 8;
	else {
	  os << "packet_muid_hor_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}
	
	break;

      case 4:
	
	if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

	  // Road words
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[2] |= (((buf[base]>>shift)<<6)&0x3FFFC0);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[2] |= (((buf[base]>>shift)<<22)&0xFC00000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if( (muidll1->ChipVer[i]==2) || (muidll1->ChipVer[i]==3) ){

	  // Shallow Roads
 
	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[2] |= (((buf[base]>>shift)<<6)&0x3FFFC0);
	  
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[2] |= (((buf[base]>>shift)<<22)&0xFC00000);

	  if(muidll1->ChipVer[i]==2){  

	    // Cosmic sums 
	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->UE_sum[1] = (buf[base]>>(shift+12))&0x7;
	    muidll1->UW_sum[1] = (buf[base]>>(shift+8))&0x7;

	  }
	  else if(muidll1->ChipVer[i]==3){

	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->AlgSqCsmcSel[1] = (buf[base]>>(shift+7))&0x1;

	    if(muidll1->AlgSqCsmcSel[1]==0) {	    
	      // Cosmic sums 
	      muidll1->UE_sum[1] = (buf[base]>>(shift+11))&0x7;
	      muidll1->UW_sum[1] = (buf[base]>>(shift+8))&0x7;
	    }
	    else{
	      // Square hole sums
	      muidll1->SqHole[3] = (buf[base]>>(shift+12))&0xF;
	      muidll1->SqHoleShal[3] = (buf[base]>>(shift+8))&0xF;
	    }
	   
	  }

	  // diagnostic tubes

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[3][0] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
      	  muidll1->diagLT[3][1] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
      	  muidll1->diagLT[3][2] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[3][3] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[3][4] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[3][5] = (((buf[base]>>shift)&0x0100)!=0) ? 1:0;
	  muidll1->diagLT[3][6] = (((buf[base]>>shift)&0x0200)!=0) ? 1:0;
	  muidll1->diagLT[3][7] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[3][8] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[3][9] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[3][10] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[3][11] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[3][12] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	}

	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3))
	  shortWord += 8;
	else{
	  os << "packet_muid_hor_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}
	  
	break;

      case 5:
	
	if( (muidll1->ChipVer[i]<=2) && (muidll1->ChipVer[i]>0) ){

	  // Road words
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[2] |= (((buf[base]>>shift)<<28)&0xF0000000);
	  muidll1->Road[3] |= (((buf[base]>>(shift+4)))&0xFFF);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[3] |= (((buf[base]>>shift)<<12)&0x7FF000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if(muidll1->ChipVer[i]==2){

	  // Road words
 
	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[2] |= (((buf[base]>>shift)<<28)&0xF0000000);
	  muidll1->ShallowRoad[3] |= (((buf[base]>>(shift+4)))&0xFFF);
	  
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[3] |= (((buf[base]>>shift)<<12)&0x7FF000);

	  // diagnostic tubes

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[4][0] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
      	  muidll1->diagLT[4][1] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
      	  muidll1->diagLT[4][2] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[4][3] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[4][4] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[4][5] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[4][6] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
      	  
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[4][7] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[4][8] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[4][9] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[4][10] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[4][11] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[4][12] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	}

	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if(muidll1->ChipVer[i]==2) 
	  shortWord += 8;
	else {
	  os << "packet_muid_hor_ll1: unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;

      default:
        os << "packet_muid_hor_ll1: unknown algorithm chip type! "
		  << muidll1->ChipType[i] << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1; 
      }// end switch
    }
    else{
      os << "packet_muid_hor_ll1: unknown chip type! "<< muidll1->ChipType[i] << std::endl;
      os << "unable to continue decoding packet" << std::endl;
      return -1; 
    }

  }// end for(i=0;i<6;i++)

  return 0;

}


int Packet_big_ll1::vert_muidlvl1_demangle(MUID_BIGLL1_BOARD *&muidll1, unsigned int *buf, int shortWord, OSTREAM &os){
  int i,j;
  if(muidll1) delete muidll1;
  muidll1 = new MUID_BIGLL1_BOARD;
  if(!muidll1){
    COUT<<"can't allocate memory for MUID_BIGLL1_BOARD structure "<<std::endl;
    return -1;
  }
  memset(muidll1,0,sizeof(MUID_BIGLL1_BOARD));
  int fiberMap[6][20]={
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},//no chip
    {1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0},//chip 1
    {1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0},//chip 2
    {0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1},//chip 3
    {0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1},//chip 4
    {0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1} //chip 5
  };

  for(i=0;i<6;i++)
    for(j=0;j<20;j++)
      fiberAssignVert[i][j]=fiberMap[i][j];

  for(i=0;i<5;i++){
    muidll1->Road[i] = 0;
    muidll1->ShallowRoad[i] = 0;
  }

  if (buf == 0) return -1;

  //  we must be very careful in what follows - we need to 
  //  be totally flexible in how we handle variations in the 
  //  output format that can occur with different versions of the 
  //  FPGA code.

  int base = shortWord/2;
  int shift = 16*((shortWord+2)%2);

  // unpack the data into a MUID structure:

  // header: 
  muidll1->header = (buf[base]>>(shift))&0xFFFF;
  shortWord++;

  // new header word:
  for(i=0;i<6;i++){

    base = shortWord/2;
    shift = 16*((shortWord+2)%2);

    // Chip Version:
    muidll1->ChipVer[i] = (buf[base]>>(shift))&0xFF;
    // Algorithm Chip:
    muidll1->AlgChip[i] = (buf[base]>>(10+shift))&0x7;
   // MUID Vertical Orientation?
    muidll1->MUIDvert[i] = (buf[base]>>(13+shift))&0x1;
    // Chip Type:
    muidll1->ChipType[i] = (buf[base]>>(14+shift))&0x3; 

    if(muidll1->ChipType[i]==1){

      if((muidll1->AlgChip[i])==1){
	
	switch(muidll1->ChipVer[i]){

	case 1:
       
        //Control chip
	  
	// set base and shift to get the data we want from the 
        // words in order

	// Strip bits
	base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
        muidll1->OrientStrip = (buf[base]>>(8+shift))&0xFF;
        muidll1->Strip = (buf[base]>>(8+shift))&0xFF;

	// LUT2 output
	base = (shortWord+2)/2;
	shift = 16*((shortWord+2)%2);
        muidll1->Lut2 = (buf[base]>>(shift))&0xFFFF;
	  
	base = (shortWord+3)/2;
	shift = 16*((shortWord+3)%2);
        // Mode Bits:
        muidll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        // MUON Aligned
        muidll1->MuonAlign = (buf[base]>>(13+shift))&0x1;
        // Strip Lut:
        muidll1->StripLut = (buf[base]>>(15+shift))&0x1;

	base = (shortWord+4)/2;
	shift = 16*((shortWord+4)%2);

        muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;  
        muidll1->Sum = (buf[base]>>(shift))&0xFF;

	// no partial sum for control chip
	muidll1->PartialSum[i] = 0; 	

 	// skip based on data size
        shortWord += 5;

	break;

	case 2:

	// LUT2 output
	base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
	muidll1->LW_sum[0] = (buf[base]>>(shift + 11))&0x7;
	muidll1->UW_sum[0] = (buf[base]>>(shift + 8))&0x7;
	muidll1->SSum = (buf[base]>>(shift))&0xFF;

	base = (shortWord+2)/2;
	shift = 16*((shortWord+2)%2);
        muidll1->Lut2 = (buf[base]>>(shift + 6))&0x3F;
	muidll1->LE_sum[0] = (buf[base]>>(shift + 3))&0x7;
	muidll1->UE_sum[0] = (buf[base]>>(shift))&0x7;
	  
	base = (shortWord+3)/2;
	shift = 16*((shortWord+3)%2);
        // Mode Bits:
        muidll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        // MUON Aligned
        muidll1->MuonAlign = (buf[base]>>(13+shift))&0x1;
        // Strip Lut:
        muidll1->StripLut = (buf[base]>>(15+shift))&0x1;

	base = (shortWord+4)/2;
	shift = 16*((shortWord+4)%2);
        muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;  
        muidll1->Sum = (buf[base]>>(shift))&0xFF;
	muidll1->cosmic_out = 0x0; // no cosmic for vert. chip
	
 	// skip based on data size
        shortWord += 5;

	break; 
	
	case 3:
	  
	base = (shortWord+3)/2;
	shift = 16*((shortWord+3)%2);
        // Mode Bits:
        muidll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        // MUON Aligned
        muidll1->MuonAlign = (buf[base]>>(13+shift))&0x1;
        // Strip Lut:
        muidll1->StripLut = (buf[base]>>(15+shift))&0x1;

	// Cosmic Select bit
	muidll1->CtlSqCsmcSel = (buf[base]>>(12+shift))&0x1;

	if(muidll1->CtlSqCsmcSel==0){
	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->LW_sum[0] = (buf[base]>>(shift + 11))&0x7;
	  muidll1->UW_sum[0] = (buf[base]>>(shift + 8))&0x7;
	  muidll1->SSum = (buf[base]>>(shift))&0xFF;

	  base = (shortWord+2)/2;
	  shift = 16*((shortWord+2)%2);
          muidll1->Lut2 = (buf[base]>>(shift + 6))&0x3F;
	  muidll1->LE_sum[0] = (buf[base]>>(shift + 3))&0x7;
	  muidll1->UE_sum[0] = (buf[base]>>(shift))&0x7;
	}
	else{
	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->SqHole[0] = (buf[base]>>(shift + 8))&0x3F;
	  muidll1->SSum = (buf[base]>>(shift))&0xFF;

	  base = (shortWord+2)/2;
	  shift = 16*((shortWord+2)%2);
	  muidll1->SqHoleShal[0] = (buf[base]>>(shift))&0x3F;
          muidll1->Lut2 = (buf[base]>>(shift + 6))&0x3F;
	}

	base = (shortWord+4)/2;
	shift = 16*((shortWord+4)%2);
        muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;  
        muidll1->Sum = (buf[base]>>(shift))&0xFF;
	
	// skip based on data size
        shortWord += 5;

	break; 

	default:
	os << "packet_muid_ver_ll1: unknown control chip version" << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1; 
	break;

	}
    
      }
      else{
        os << "packet_muid_ver_ll1: unknown algorithm id type for control chip data" 
		  << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1;
      }
      
    }
    else if(muidll1->ChipType[i]==0) {

      // Check valid version numbers

      if( (muidll1->ChipVer[i]>3) || (muidll1->ChipVer[i]<0) ){
        os << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		  << muidll1->AlgChip[i] << " = " << muidll1->ChipVer[i] << std::endl;
	os << "unable to continue decoding packet" << std::endl;
	return -1; 
      }

      // Algorithm chip
      // Decode common elements first

      if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

        // Partial Sum
        base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
        muidll1->PartialSum[i] = (buf[base]>>shift)&0x3F;

	// Fiber Flags
	base = (shortWord+2)/2;
	shift = 16*((shortWord+2)%2);
	unsigned int CheckWord = (buf[base]>>shift)&0xFFFF;

	int bitnum = 0;
	for(j=0; j<20; j++){ 
	  if(fiberMap[muidll1->AlgChip[i]][j]==1){
      	    if(CheckWord&(0x1<<bitnum)) muidll1->SyncErr[i] |= (0x1<<j);
	    if(CheckWord&(0x1<<(bitnum+1))) muidll1->DataErr[i] |= (0x1<<j);
	    bitnum+=2;
	    if(bitnum>15){
	      bitnum = 0; 
	      base = (shortWord+3)/2;
	      shift = 16*((shortWord+3)%2);
	      CheckWord = (buf[base]>>shift)&0xFFFF;	
	    }
	  }
	}

      }
      if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)){

        // Partial Sum
        base = (shortWord+1)/2;
	shift = 16*((shortWord+1)%2);
        muidll1->ShallowPartialSum[i] = (buf[base]>>(shift+8))&0x1F;

      }

      switch (muidll1->AlgChip[i]) {

      case 1:

	if( (muidll1->ChipVer[i]<=2) && (muidll1->ChipVer[i]>0) ){
	
	  // Road words
	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->Road[0] |= ((buf[base]>>(12+shift))&0xF);
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[0] |= (((buf[base]>>shift)<<4)&0xFFFF0);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[0] |= (((buf[base]>>shift)<<20)&0xFFF00000);
	  muidll1->Road[1] |= ((buf[base]>>(shift+12))&0x3);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if(muidll1->ChipVer[i]==2){

	  // Shallow Roads

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[0] |= (((buf[base]>>shift)<<2)&0x3FFFC);
 
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[0] |= (((buf[base]>>shift)<<18)&0xFFFC0000);
	  muidll1->ShallowRoad[1] |= ((buf[base]>>(shift+14))&0x3);
	  
	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->ShallowRoad[0] |= (((buf[base]>>(shift+1)))&3);

	  // diagnostic tubes

	  base = (shortWord+2)/2;
	  shift = 16*((shortWord+2)%2);
	  muidll1->diagLT[0][0] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[0][1] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[0][2] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[0][3] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[0][4] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[0][5] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[0][6] = (((buf[base]>>shift)&0x0008)!=0) ? 1:0;
	  muidll1->diagLT[0][7] = (((buf[base]>>shift)&0x0010)!=0) ? 1:0;
	  muidll1->diagLT[0][8] = (((buf[base]>>shift)&0x0020)!=0) ? 1:0;
	  muidll1->diagLT[0][9] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[0][10] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[0][11] = (((buf[base]>>shift)&0x0100)!=0) ? 1:0;
	  muidll1->diagLT[0][12] = (((buf[base]>>shift)&0x0200)!=0) ? 1:0;

	}

        // The data word skip is determined by the chip type.
	// The default for an unrecognized type is to assume a skip for the 
	// most recent chip type.
	
	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if(muidll1->ChipVer[i]==2)
	  shortWord += 8;
	else{ 
	  os << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;
	  
      case 2:
	
	if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

	  // Road words
	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->Road[1] |= (((buf[base]>>(10+shift))&0x3C));
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[1] |= (((buf[base]>>shift)<<6)&0x3FFFC0);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[1] |= (((buf[base]>>shift)<<22)&0xFFC00000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}

	if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)) {

	  // Shallow Roads

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[1] |= ((((buf[base]>>shift)<<2)&0x3FFFC));
 
	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[1] |= ((((buf[base]>>shift)<<18)&0xFFFC0000));

	  if(muidll1->ChipVer[i]==2){  

	    // Cosmic sums 
	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->LE_sum[1] = (buf[base]>>(shift+8))&0x7;
	    muidll1->UE_sum[1] = (buf[base]>>(shift+4))&0x7;

	  }
	  else if(muidll1->ChipVer[i]==3){

	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->AlgSqCsmcSel[0] = (buf[base]>>(shift+4))&0x1;

	    if(muidll1->AlgSqCsmcSel[0]==0) {	    
	      // Cosmic sums 
	      muidll1->LE_sum[1] = (buf[base]>>(shift+9))&0x3;
	      muidll1->UE_sum[1] = (buf[base]>>(shift+5))&0x3;
	    }
	    else{
	      // Square hole sums
	      muidll1->SqHole[1] = (buf[base]>>(shift+9))&0x3;
	      muidll1->SqHoleShal[1] = (buf[base]>>(shift+5))&0x3;
	    }

	  }  

	  // diagnostic tubes

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[1][0] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[1][1] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[1][2] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[1][3] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[1][4] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[1][5] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[1][6] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[1][7] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->diagLT[1][8] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[1][9] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[1][10] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[1][11] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;
	  muidll1->diagLT[1][12] = 0;
 
	}

        // The data word skip is determined by the chip type.
	// The default for an unrecognized type is to assume a skip for the 
	// most recent chip type.
	
	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)) 
	  shortWord += 8;
	else {
	  os << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;

      case 3:

	if( (muidll1->ChipVer[i]<=2) && (muidll1->ChipVer[i]>0) ){

	  // Road words
	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->Road[2] |= ((buf[base]>>(12+shift))&0xF);
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[2] |= (((buf[base]>>shift)<<4)&0xFFFF0);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[2] |= (((buf[base]>>shift)<<20)&0x3FF00000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;

	}
	if(muidll1->ChipVer[i]==2){

	  // Shallow Roads

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[2] |= ((buf[base]>>(shift))&0xFFFF);

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[2] |= (((buf[base]>>(shift))<<16)&0x3FFF0000);

	  // diagnostic tubes

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[2][0] = (((buf[base]>>shift)&0x0100)!=0) ? 1:0;
	  muidll1->diagLT[2][1] = (((buf[base]>>shift)&0x0200)!=0) ? 1:0;
	  muidll1->diagLT[2][2] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[2][3] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[2][4] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[2][5] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[2][6] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[2][7] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[2][8] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[2][9] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->diagLT[2][10] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[2][11] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[2][12] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;

	}

        // The data word skip is determined by the chip type.
	// The default for an unrecognized type is to assume a skip for the 
	// most recent chip type.
	
	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if(muidll1->ChipVer[i]==2) 
	  shortWord += 8;
	else{
	  os << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}
	
	break;

      case 4:
	
	if( (muidll1->ChipVer[i]<=3) && (muidll1->ChipVer[i]>0) ){

	  // Road words
	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->Road[2] |= (((buf[base]>>(12+shift))<<30)&0xC0000000);
	  muidll1->Road[3] |= (((buf[base]>>(14+shift))&0x3));
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[3] |= (((buf[base]>>shift)<<2)&0x3FFFC);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[3] |= (((buf[base]>>shift)<<18)&0xFFC0000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)){

	  // Shallow Roads

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[2] |= (((buf[base]>>(shift))<<30)&0xC0000000);
	  muidll1->ShallowRoad[3] |= (((buf[base]>>(shift+2)))&0x3FFF);

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[3] |= (((buf[base]>>(shift))<<14)&0xFFFC000);

	  if(muidll1->ChipVer[i]==2){  

	    // Cosmic sums 
	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->UW_sum[1] = (buf[base]>>(shift+4))&0x7;
	    muidll1->LW_sum[1] = (buf[base]>>(shift+8))&0x7;

	  }
	  else if(muidll1->ChipVer[i]==3){

	    base = (shortWord+3)/2;
	    shift = 16*((shortWord+3)%2);
	    muidll1->AlgSqCsmcSel[1] = (buf[base]>>(shift+4))&0x1;

	    if(muidll1->AlgSqCsmcSel[1]==0) {	    
	      // Cosmic sums 
	      muidll1->LW_sum[1] = (buf[base]>>(shift+9))&0x3;
	      muidll1->UW_sum[1] = (buf[base]>>(shift+5))&0x3;
	    }
	    else{
	      // Square hole sums
	      muidll1->SqHole[3] = (buf[base]>>(shift+9))&0x3;
	      muidll1->SqHoleShal[3] = (buf[base]>>(shift+5))&0x3;
	    }
	   
	  }

	  // diagnostic tubes

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[3][0] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[3][1] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;

	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->diagLT[3][2] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[3][3] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[3][4] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[3][5] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->diagLT[3][6] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[3][7] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+1)/2;
	  shift = 16*((shortWord+1)%2);
	  muidll1->diagLT[3][8] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[3][9] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[3][10] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[3][11] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;
	  muidll1->diagLT[3][12] = 0;

	}

        // The data word skip is determined by the chip type.
	// The default for an unrecognized type is to assume a skip for the 
	// most recent chip type.
	
	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if((muidll1->ChipVer[i]==2)||(muidll1->ChipVer[i]==3)) 
	  shortWord += 8;
	else{
	  os << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}
	
	break;

      case 5:
	
	if( (muidll1->ChipVer[i]<=2) && (muidll1->ChipVer[i]>0) ){

	  // Road words
	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->Road[3] |= (((buf[base]>>(12+shift))<<28)&0xF0000000);
 
	  base = (shortWord+4)/2;
	  shift = 16*((shortWord+4)%2);
	  muidll1->Road[4] |= (((buf[base]>>shift))&0xFFFF);
	  
	  base = (shortWord+5)/2;
	  shift = 16*((shortWord+5)%2);
	  muidll1->Road[4] |= (((buf[base]>>shift)<<16)&0x3FFF0000);
	  
          muidll1->Scaler[i] = (buf[base]>>(14+shift))&0x3;
	  
	}
	if(muidll1->ChipVer[i]==2){

	  // Shallow Roads

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->ShallowRoad[3] |= (((buf[base]>>(shift+1))<<28)&0x30000000);

	  base = (shortWord+6)/2;
	  shift = 16*((shortWord+6)%2);
	  muidll1->ShallowRoad[3] |= (((buf[base]>>(shift))<<30)&0xC0000000);
	  muidll1->ShallowRoad[4] |= (((buf[base]>>(shift+2)))&0x3FFF);

	  base = (shortWord+7)/2;
	  shift = 16*((shortWord+7)%2);
	  muidll1->ShallowRoad[4] |= (((buf[base]>>(shift))<<14)&0x3FFFC000);

	  // diagnostic tubes

	  base = (shortWord+2)/2;
	  shift = 16*((shortWord+2)%2);
	  muidll1->diagLT[4][0] = (((buf[base]>>shift)&0x0400)!=0) ? 1:0;
	  muidll1->diagLT[4][1] = (((buf[base]>>shift)&0x0800)!=0) ? 1:0;
	  muidll1->diagLT[4][2] = (((buf[base]>>shift)&0x1000)!=0) ? 1:0;
	  muidll1->diagLT[4][3] = (((buf[base]>>shift)&0x2000)!=0) ? 1:0;
	  muidll1->diagLT[4][4] = (((buf[base]>>shift)&0x4000)!=0) ? 1:0;
	  muidll1->diagLT[4][5] = (((buf[base]>>shift)&0x8000)!=0) ? 1:0;

	  base = (shortWord+3)/2;
	  shift = 16*((shortWord+3)%2);
	  muidll1->diagLT[4][6] = (((buf[base]>>shift)&0x0008)!=0) ? 1:0;
	  muidll1->diagLT[4][7] = (((buf[base]>>shift)&0x0010)!=0) ? 1:0;
	  muidll1->diagLT[4][8] = (((buf[base]>>shift)&0x0020)!=0) ? 1:0;
	  muidll1->diagLT[4][9] = (((buf[base]>>shift)&0x0040)!=0) ? 1:0;
	  muidll1->diagLT[4][10] = (((buf[base]>>shift)&0x0080)!=0) ? 1:0;
	  muidll1->diagLT[4][11] = (((buf[base]>>shift)&0x0100)!=0) ? 1:0;
	  muidll1->diagLT[4][12] = (((buf[base]>>shift)&0x0200)!=0) ? 1:0;

	}

        // The data word skip is determined by the chip type.
	// The default for an unrecognized type is to assume a skip for the 
	// most recent chip type.
	
	if(muidll1->ChipVer[i]==1) 
	  shortWord += 6;
	else if(muidll1->ChipVer[i]==2) 
	  shortWord += 8;
	else{
	  os << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  os << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;

      default:
	break;

      }// end switch
    }
    else{
      os << "packet_muid_ver_ll1: unknown chip type! "<< muidll1->ChipType[i] << std::endl;
      os << "unable to continue decoding packet" << std::endl;
      return -1; 
    }

  }// end for(i=0;i<6;i++)

  return 0;

}


void Packet_big_ll1::printOutMuid(MUID_BIGLL1_BOARD *muidll1, OSTREAM &os)
{
  int m,i;
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
      if(muidll1->MUIDvert[c]){
        if(!fiberAssignVert[muidll1->AlgChip[c]][fb])err='-';
        else {
	  err=((muidll1->SyncErr[c]&(0x1<<fb))!=0)?'1':'0';
	}
        os<<"  "<<err;
      }
      else{
        if(!fiberAssignHoriz[muidll1->AlgChip[c]][fb])err='-';
        else {
	  err=((muidll1->SyncErr[c]&(0x1<<fb))!=0)?'1':'0';
	}
        os<<"  "<<err;
      }
    }
    os<<std::endl;
  }
  os << std::endl;
  for(c=0;c<6;c++){
    if(muidll1->ChipType[c]==1)continue;
    if(muidll1->AlgChip[c]<1 || muidll1->AlgChip[c]>5)continue;
    os<<"DATA Err["<<muidll1->AlgChip[c]<<"]:";
    for(fb=0;fb<20;fb++){
      if(muidll1->MUIDvert[c]){
        if(!fiberAssignVert[muidll1->AlgChip[c]][fb])err='-';
        else {
	  err=((muidll1->DataErr[c]&(0x1<<fb))!=0)?'1':'0';
        }
        os<<"  "<<err;
      }
      else{
        if(!fiberAssignHoriz[muidll1->AlgChip[c]][fb])err='-';
        else {
	  err=((muidll1->DataErr[c]&(0x1<<fb))!=0)?'1':'0';
        }
        os<<"  "<<err;
      }
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
  
  return; 

}


void Packet_big_ll1::fillDataStructures ( OSTREAM &os){


  unsigned int *k;
  int dlength = getDataLength();
  k= (unsigned int *) findPacketDataStart(packet);
  if (k == 0) return;

  // We need to set the start of the data using the 0x1bxx marker for the ZDCLVL1 data.
  // This is either in the lower or upper word, or we bail out.
  // The ZDCLVL1 should always be the first packet in the readout.

  int ShortWord = 0; 
  if((k[0]&0x1b00)==0x1b00) 
    ShortWord = 0; 
  else if((k[0]&0x1b000000)==0x1b000000)
    ShortWord = 1; 
  else{
    os << "Failed to find 0x1b marker in first 32-bit word - bailing out!" << std::endl;
    return;
  }

  // ZDC LL1 decoding:

  if(!zdclvl1) {
    int status = zdclvl1_demangle(k,ShortWord,os);
    zdclvl1->DecodeStatus = status;
    ShortWord+=8; 
  }
  if(!zdclvl1){
    os << "Failed to fill zdclvl1 data structure" << std::endl;
    return;
  }

  // Check for the presence of MuID LL1 words

  if((dlength==4)||(dlength<27)) return; // Only ZDCLL1 in the packet?

  bool BailOut = false; 

  while(((2*(dlength-2)-ShortWord)>0) && !BailOut){

    int base = ShortWord/2;
    int shift = (int) 16* (ShortWord%2);

    unsigned short next_header = ((k[base]>>(shift))&0xFFFF);
    
    // Check to make sure that all the headers have the same Level-1 SN
    // If we've got an extra word from a previous event then we will have 
    // a copy of the next event's header at the end of this event.

    if( (zdclvl1->header&0xFF)!=(next_header&0xFF) ){
      //os<<" Header SN does not match ZDC, header = 0x"<< std::hex << next_header << ", bailing out!" <<std::endl;  
      return;
      break;
    }
  
    switch(next_header&0xFF00){

    case(0xca00):
      {
        int status = horiz_muidlvl1_demangle(muidll1_nh,k,ShortWord,os);
        if(muidll1_nh){ 
	  muidll1_nh->DecodeStatus = status;
          ShortWord+=46;
	}
	else
	  BailOut = true; 
        break;
      }
    case(0xda00):
      {
        int status = vert_muidlvl1_demangle(muidll1_nv,k,ShortWord,os);
        if(muidll1_nv){
	  muidll1_nv->DecodeStatus = status;
          ShortWord+=46;
	}
	else
	  BailOut = true; 
        break;
      }
    case(0xaa00):
      {
        int status = horiz_muidlvl1_demangle(muidll1_sh,k,ShortWord,os);
        if(muidll1_sh){
	  muidll1_sh->DecodeStatus = status;
          ShortWord+=46;
	}
	else
	  BailOut = true; 
        break;
      }
    case(0xba00):
      {
        int status = vert_muidlvl1_demangle(muidll1_sv,k,ShortWord,os);
        if(muidll1_sv){
	  muidll1_sv->DecodeStatus = status;
          ShortWord+=46;
        }
	else
	  BailOut = true; 
        break;
      }
    default:
      os<<" Unrecognized LL1 data packet header = 0x"<< std::hex << next_header << ", bailing out!" <<std::endl;  
      return;
      break;
    
    }

  }

  return; 

}
