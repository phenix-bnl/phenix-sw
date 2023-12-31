#include <oncsSub_idrcpetdata.h>
#include <cstring>

oncsSub_idrcpetdata::oncsSub_idrcpetdata(subevtdata_ptr data)
  :oncsSubevent_w4 (data)
{
  samples = 0;
  d_time = 0;
  d_timelength = 0;
  lval = 0;
}
  
oncsSub_idrcpetdata::~oncsSub_idrcpetdata()
{
  if ( d_timelength )
    {
      delete [] d_time;
      d_time = 0;
    }
  if ( lval)
    {
      delete [] lval;
      lval = 0;
    }
}


int *oncsSub_idrcpetdata::decode ( int *nwout)
{
  int *p;

  unsigned long long timebits, coarse_timestamp, fine_timestamp;
  unsigned int word1, word2;
  unsigned int clockID;


  int i;
  unsigned int *SubeventData = (unsigned int *) &SubeventHdr->data;
  //  SubeventData++;
  int dlength = getLength()-4 - getPadding() -1;
  samples = dlength /2;

  p = new int [samples];  //block id

  decoded_data2 = new int [samples]; //  crystal id
  data2_length = samples;

  decoded_data3 = new int [samples];  // event type
  data3_length = samples;

  lval  = new long long [samples];  // raw samples

  d_time = new double [samples];     // time
  d_timelength = samples;

  int k = 0;

  for ( i = 0; i < samples; i++) 
    {

      word2 = (unsigned int) SubeventData[2*i];
      fine_timestamp = (word2 >> 2) & 0x3f;

      if (fine_timestamp <= 32)
	{

	  word1 = (unsigned int) SubeventData[2*i+1];
	  clockID = (word1 >> 30) & 0x3;

	  p[k] = (word1 >> 26) & 0xf;  //block id
	  if (  p[k] ==3)  p[k]=2;
	  else if (  p[k] ==2)  p[k]=3;

	  decoded_data2[k]  = (word1 >> 21) & 0x1f;  // crystal id

	  timebits = (word1 >> 18) & 0x7;
	  coarse_timestamp = timebits << 36; //fill bits 38,37,36
	  timebits = (word1 & 0xfffc) >> 2;
	  coarse_timestamp += timebits << 22; //fill bits 35 - 22
	  timebits = (word2 >> 18);
	  coarse_timestamp += timebits << 8; //fill bits 21 - 8
	  coarse_timestamp += (word2 >> 8) & 0xff; //fill bits 7 - 0
	  d_time[k] = (coarse_timestamp - clockID)*18 - (fine_timestamp);

	  lval[i] = word2;
	  lval[i] <<=32;
	  lval[i] |= word1 ;

	  k++;
	}

      //      std::cout << std::hex << lval[i] << std::dec<< std::endl;
    }
  samples = k;
  *nwout = samples;
  return p;
}

//int oncsSub_idrcpetdata::iValue(const int ch)
  //{

//  if ( decoded_data1 == 0 ) decoded_data1 = decode(&data1_length);

//  if ( ch < 0 || ch >31 ) return 0;

//  return decoded_data1[ch];

//}

int oncsSub_idrcpetdata::iValue(const int ich,const char *what)
{

  if ( decoded_data1 == 0 ) decoded_data1 = decode(&data1_length);

  if ( strcmp(what,"SAMPLES") == 0 )
  {
    return samples;
  }

  if ( strcmp(what,"BLOCKID") == 0 )
  {
    if ( ich < 0 || ich >= samples) return 0;
    return decoded_data1[ich];
  }

  if ( strcmp(what,"CRYSTALID") == 0 )
  {
    if ( ich < 0 || ich >= samples) return 0;
    return decoded_data2[ich];
  }

  return 0;

}

double oncsSub_idrcpetdata::dValue(const int ich)
{

  if ( decoded_data1 == 0 ) decoded_data1 = decode(&data1_length);

  if ( ich < 0 || ich >= samples) return 0;
    return d_time[ich];
}

long long  oncsSub_idrcpetdata::lValue(const int ich)
{

  if ( decoded_data1 == 0 ) decoded_data1 = decode(&data1_length);

  if ( ich < 0 || ich >= samples) return 0;
    return lval[ich];
}


void  oncsSub_idrcpetdata::dump ( OSTREAM& os )
{
  int i;
  //  int *SubeventData = &SubeventHdr->data;
  int is =  iValue(0,"SAMPLES");

  //  os << "Samples: " << iValue(0,"SAMPLES") << std::endl;

  os << " sample  B id    C id   time   ( " << is << " Samples )" << std::endl;
  
  for ( i = 0; i < is ; i++)
    {
      int prec = os.precision();

      os << std::setw(5) << i << "  |  "; 
 
      os << std::setw(4) << iValue(i,"BLOCKID") << "  ";
      os << std::setw(4) << iValue(i,"CRYSTALID") << "  ";
      //      os << std::setw(6) << std::hex << "0x" << iValue(i,"EVENTTYPE") 
      //	 << std::dec<< "    ";
      os << std::setw(24) << std::setprecision(24) << dValue(i)
	 << std::setprecision(prec) ;
	
      os << std::endl;
    }
  os << std::endl;
}

