#include <packet_ert_ll1.h>
#include <string.h>

Packet_ert_ll1::Packet_ert_ll1(PACKET_ptr data) : Packet_w4 (data)
{
  ertll1 = NULL;
}

int *Packet_ert_ll1::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_ert_ll1( temp
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

int Packet_ert_ll1::iValue(const int ich, const char *what){
  
  if(!ertll1) demangle();
  if(!ertll1){
    std::cout << "Failed to fill data structure" << std::endl;
    return -1;
  }

  if(strcmp(what,"HEADER")==0) {
    
    return ertll1->header;
        
  } else if(strcmp(what,"FIBERERR")==0) {
    
    return ertll1->FiberErr;

  } else if(strcmp(what,"PRELUT2")==0) {
    
    return ertll1->PreLut2;
        
  } else if(strcmp(what,"MODEBITS")==0) {
    
    return ertll1->ModeBits;
        
  } else if(strcmp(what,"CHIPVER")==0) {
    
    return ertll1->ChipVer;
        
  } else if(strcmp(what,"SCALER")==0) {

    if(ich<0 || ich>1) return -1;        
    return ertll1->Scaler[ich];
        
  } else if(strcmp(what,"4x4A")==0) {
    
    return ertll1->FourByFourA;

  } else if(strcmp(what,"4x4B")==0) {
    
    return ertll1->FourByFourB;

  } else if(strcmp(what,"4x4C")==0) {
    
    return ertll1->FourByFourC;

  } else if(strcmp(what,"2x2")==0) {
    
    return ertll1->TwoByTwo;

  } else if(strcmp(what,"ELECTRON")==0) {
    
    return ertll1->electron;

  } else if(strcmp(what,"E_ALGVER")==0) {
    
    return ertll1->electron_alg_ver;

  } else if(strcmp(what,"TWOELECTRON")==0) {
    
    return ertll1->two_electron;

  } else if(strcmp(what,"ACCEPT")==0) {
    
    return ertll1->AcceptCtr;

  } else if(strcmp(what,"RXNP_N")==0) {
    
    return ertll1->RxNP_N;

  } else if(strcmp(what,"RXNP_S")==0) {
    
    return ertll1->RxNP_S;

  }

  //  std::cout <<"iValue(int ich, char* what): Unrecognized value "<<what<<std::endl;
  return 0;

}

int Packet_ert_ll1::iValue(const int ich, const  int what){
  
  if(!ertll1) demangle();
  if(!ertll1){
    std::cout<<"Failed to fill data structure"<<std::endl;
    return -1;
  }
  switch (what) {
    
  case HEADER:
    
    { return ertll1->header; }
        
  case FIBERERR:
    {     
      return ertll1->FiberErr;
    }
    
  case PRELUT2:
    
    { return ertll1->PreLut2; }

  case MODEBITS:
    
    { return ertll1->ModeBits; }
    
  case CHIPVER:
    
    { return ertll1->ChipVer; }
    
  case SCALER:
    {
      if(ich<0 || ich>1) return -1;        
      return ertll1->Scaler[ich];
    }
  case FOURBYFOURA:
    
    { return ertll1->FourByFourA; }
    
  case FOURBYFOURB:
    
    { return ertll1->FourByFourB; }

  case FOURBYFOURC:
    
    { return ertll1->FourByFourC; }
    
  case TWOBYTWO:
    
    { return ertll1->TwoByTwo; }
    
  case ELECTRON:
    
    { return ertll1->electron; }

  case E_ALGVER:
    
    { return ertll1->electron_alg_ver; }

  case TWOELECTRON:
    
    { return ertll1->two_electron; }

  case ACCEPT:
    
    { return ertll1->AcceptCtr; }

  case RXNP_N:
    
    { return ertll1->RxNP_N; }

  case RXNP_S:
    
    { return ertll1->RxNP_S; }

  }

  return -1;

}

int Packet_ert_ll1::fillIntArray (int destination[],    // the data go here 
			          const int length,     // space we have in destination
			          int * nw,             // words actually used
			          const char * what)    // type of data (see above)
{
  if(!ertll1) demangle();
  if(!ertll1){
    std::cout<<"Failed to fill data structure"<<std::endl;
    return -1;
  }
  *nw=sizeof(ertll1);
  if(*nw>length*4) *nw=length*4;
  memcpy((void *)destination, (const void *)ertll1,*nw);
  *nw/=4;
  return 0;
}

void Packet_ert_ll1::dump ( OSTREAM &os){

  int m;
  this->identify(os); 
  if(!ertll1) demangle();
  if(!ertll1){
    os<<"Failed to fill ertll1. Exit"<<std::endl;
    return;
  }
  int i;
  unsigned int oldFlags;
  oldFlags=os.flags();
  char oldFill=os.fill('0');
  for(m=0;m<54;m++) os<<"=";
  os<<std::hex<<std::endl;
  os<<"ERT LL1 data packet:"<< std::endl;
  os<<"Header:"<<ertll1->header<<std::endl;

  os<<"Chip Version: "<<std::dec;
  os <<" "<<ertll1->ChipVer;
  os <<std::endl;

  os<<"Chip scaler:  "<<std::dec;
  for(i=0;i<2;i++)
    os<<" "<<ertll1->Scaler[i];
  os<<std::endl;
  
  os<<"Mode Bits: 0x"<< std::hex <<ertll1->ModeBits<< std::dec << std::endl;
  os<<"Accept Counter: 0x"<< std::hex <<ertll1->AcceptCtr<< std::dec << std::endl;
  os<<"Electron Algorithm ID: 0x"<<std::hex<<ertll1->electron_alg_ver << std::dec << std::endl; 
  os<<std::endl;

  char err = '-';
  int c, fb;
  os.fill(' ');
  os<<"Fiber #    :";
  for(c=0;c<20;c++)
    os<<" "<<SETW(2)<<std::dec<<c;
  os<<std::endl;
    
  os<<"SYNC Err   :";
    
  for(fb=0;fb<20;fb++){

    if((fb>3)&&(fb<16)) err='-';
    else {
      if(fb==0) err=((ertll1->FiberErr&0x1)!=0)? '1':'0';
      if(fb==1) err=((ertll1->FiberErr&0x4)!=0)? '1':'0';
      if(fb==2) err=((ertll1->FiberErr&0x10)!=0)? '1':'0';
      if(fb==3) err=((ertll1->FiberErr&0x40)!=0)? '1':'0';
      if(fb==16) err=((ertll1->FiberErr&0x100)!=0)? '1':'0';
      if(fb==17) err=((ertll1->FiberErr&0x400)!=0)? '1':'0';
      if(fb==18) err=((ertll1->FiberErr&0x1000)!=0)? '1':'0';
      if(fb==19) err=((ertll1->FiberErr&0x4000)!=0)? '1':'0';
    }
    os<<"  "<<err;
  }
  os<<std::endl;

  os<<"DATA Err   :";
    
  for(fb=0;fb<20;fb++){

    if((fb>3)&&(fb<16)) err='-';
    else {
      if(fb==0) err=((ertll1->FiberErr&0x2)!=0)?'1':'0';
      if(fb==1) err=((ertll1->FiberErr&0x8)!=0)?'1':'0';
      if(fb==2) err=((ertll1->FiberErr&0x20)!=0)?'1':'0';
      if(fb==3) err=((ertll1->FiberErr&0x80)!=0)?'1':'0';
      if(fb==16) err=((ertll1->FiberErr&0x200)!=0)?'1':'0';
      if(fb==17) err=((ertll1->FiberErr&0x800)!=0)?'1':'0';
      if(fb==18) err=((ertll1->FiberErr&0x2000)!=0)?'1':'0';
      if(fb==19) err=((ertll1->FiberErr&0x8000)!=0)?'1':'0';
    }
    os<<"  "<<err;
  }
  os<<std::endl;

  os << std::endl;
  os.fill(oldFill);

  os<<"PreLut2: 0x"<<std::hex<<ertll1->PreLut2<<std::dec<<std::endl;
  os<<"4x4a: " << ertll1->FourByFourA << std::endl;
  os<<"4x4b: " << ertll1->FourByFourB << std::endl;
  os<<"4x4c: " << ertll1->FourByFourC << std::endl;
  os<<"2x2: " << ertll1->TwoByTwo << std::endl;
  os<<"electron: " << ertll1->electron << std::endl;
  os<<"two electron (RxNP): " << ertll1->two_electron << std::endl;
  os<<"RxNP_N: " << ertll1->RxNP_N << std::endl;
  os<<"RxNP_S: " << ertll1->RxNP_S << std::endl;

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

// The main data decoding routine

void Packet_ert_ll1::demangle(){
 
  if(ertll1) delete ertll1;
  ertll1 = new ERT_LL1_BOARD;
  if(!ertll1){
    COUT<<"can't allocate memory for ERT_LL1_BOARD structure "<<std::endl;
    return;
  }
  memset(ertll1,0,sizeof(ERT_LL1_BOARD));

  unsigned int* buf = (unsigned int *) findPacketDataStart(packet);
  if (buf == 0) return;
  // unpack the data into an ERT structure:

  // header: 
  ertll1->header = buf[0]&0xFFFF;

  //  we must be very careful in what follows - we need to 
  //  be totally flexible in how we handle variations in the 
  //  output format that can occur with different versions of the 
  //  FPGA code.

  // "pointer" to which 16-bit word we are starting with
  int shortWord = 1;

  // ERT CONTROL CHIP 

  // Chip Version:
  int base = shortWord/2;
  int shift = (int) 16* (shortWord%2);
  ertll1->ChipVer = (buf[base]>>(shift))&0xFF;
	  
  // Mode Bits:
  base = (shortWord+3)/2;
  shift = 16*((shortWord+3)%2);
  ertll1->ModeBits = (buf[base]>>(shift))&0x1FF;
        
  // Sync Counter
  base = (shortWord+4)/2;
  shift = 16*((shortWord+4)%2);  
  ertll1->Scaler[0] = (buf[base]>>(14+shift))&0x3;  

  // PreLut2
  base = (shortWord+2)/2;
  shift = 16*((shortWord+2)%2);  
  ertll1->PreLut2 = (buf[base]>>(6+shift))&0xFF;  

  // Offset the data pointer 
  shortWord += 5; 

  // ERT ALGORITHM CHIP

  // Algorithm Bits:
  base = (shortWord)/2;
  shift = 16*((shortWord)%2);
  ertll1->FourByFourA = ((buf[base]>>(shift))&0x1)>0 ? 1:0;
  ertll1->FourByFourB = ((buf[base]>>(shift))&0x2)>0 ? 1:0;
  ertll1->FourByFourC = ((buf[base]>>(shift))&0x4)>0 ? 1:0;
  ertll1->TwoByTwo = ((buf[base]>>(shift))&0x10)>0 ? 1:0;
  ertll1->electron = ((buf[base]>>(shift))&0x20)>0 ? 1:0;
  ertll1->two_electron = ((buf[base]>>(shift))&0x8)>0 ? 1:0;
  ertll1->RxNP_S =  ((buf[base]>>(shift))&0x80)>0 ? 1:0;
  ertll1->RxNP_N =  ((buf[base]>>(shift))&0x40)>0 ? 1:0;

  // Accept Counter
  base = (shortWord+1)/2;
  shift = 16*((shortWord+1)%2);
  ertll1->AcceptCtr = (buf[base]>>(shift))&0xFFFF;

  base = (shortWord+2)/2;
  shift = 16*((shortWord+2)%2);
  ertll1->AcceptCtr |= ((buf[base]>>(shift))&0xFFFF)<<16;

  // Sync Counter
  base = (shortWord+5)/2;
  shift = 16*((shortWord+5)%2);  
  ertll1->Scaler[1] = (buf[base]>>(14+shift))&0x3;

  // Fiber Error Bits
  base = (shortWord+3)/2;
  shift = 16*((shortWord+3)%2);  
  ertll1->FiberErr = (buf[base]>>(shift))&0xFFFF;

  // Electron Algorithm ID
  base = (shortWord+4)/2;
  shift = 16*((shortWord+4)%2);
  ertll1->electron_alg_ver = (buf[base]>>(shift))&0xFFFF;

  return;

}
