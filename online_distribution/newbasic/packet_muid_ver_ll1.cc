#include <packet_muid_ver_ll1.h>
#include <string.h>
#include <math.h>

int Packet_muid_ver_ll1::demangle(){
  int i,j;
  if(muidll1) delete muidll1;
  muidll1 = new MUID_LL1_BOARD;
  if(!muidll1){
    COUT<<"can't allocate memory for MUID_LL1_BOARD structure "<<std::endl;
    return -1;
  }
  memset(muidll1,0,sizeof(MUID_LL1_BOARD));
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
      fiberAssign[i][j]=fiberMap[i][j];

  for(i=0;i<5;i++){
    muidll1->Road[i] = 0;
    muidll1->ShallowRoad[i] = 0;
  }

  unsigned int* buf = (unsigned int *) findPacketDataStart(packet);
  if (buf == 0) return -1;
  // unpack the data into a MUID structure:
  // header: 
  muidll1->header = buf[0]&0xFFFF;

  //  we must be very careful in what follows - we need to 
  //  be totally flexible in how we handle variations in the 
  //  output format that can occur with different versions of the 
  //  FPGA code.

  // "pointer" to which 16-bit word we are starting with
  int shortWord = 1;

  // new header word:
  for(i=0;i<6;i++){

    int base = shortWord/2;
    int shift = 16*((shortWord+2)%2);

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
	std::cout << "packet_muid_ver_ll1: unknown control chip version" << std::endl;
	std::cout << "unable to continue decoding packet" << std::endl;
	return -1; 
	break;

	}
    
      }
      else{
        std::cout << "packet_muid_ver_ll1: unknown algorithm id type for control chip data" 
		  << std::endl;
	std::cout << "unable to continue decoding packet" << std::endl;
	return -1;
      }
      
    }
    else if(muidll1->ChipType[i]==0) {

      // Check valid version numbers

      if( (muidll1->ChipVer[i]>3) || (muidll1->ChipVer[i]<0) ){
        std::cout << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		  << muidll1->AlgChip[i] << " = " << muidll1->ChipVer[i] << std::endl;
	std::cout << "unable to continue decoding packet" << std::endl;
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
	  std::cout << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  std::cout << "unable to continue decoding packet" << std::endl;
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
	  std::cout << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  std::cout << "unable to continue decoding packet" << std::endl;
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
	  std::cout << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  std::cout << "unable to continue decoding packet" << std::endl;
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
	  std::cout << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  std::cout << "unable to continue decoding packet" << std::endl;
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
	  std::cout << "packet_muid_ver_ll1:unknown algorithm version for chip = "
		    << muidll1->AlgChip[i] << std::endl;
	  std::cout << "unable to continue decoding packet" << std::endl;
	  return -1; 
	}

	break;

      default:
	break;

      }// end switch
    }
    else{
      std::cout << "packet_muid_ver_ll1: unknown chip type! "<< muidll1->ChipType[i] << std::endl;
      std::cout << "unable to continue decoding packet" << std::endl;
      return -1; 
    }

  }// end for(i=0;i<6;i++)

  return 0;

}
