#ifndef __MUTOO_FEM_H__
#define __MUTOO_FEM_H__

//! widely used front-end electronic enumerations
namespace MUTOO_FEM {
  
  /*! Mutr PacketID offset */  
  const int PACKET_ID_BASE = 11001; 
  
  /*! Mutr Packet size (PHDWORD) */  
  const int PACKET_SIZE_MAX = 2000; 
  
  /*! Maximum number of packets*/
  const int NPACKET_MAX = 362; 
  
  /*! Last Mutr PacketID in South Arm */
  const int SOUTH_PACKET_MAX = 11168; 
  
  /*! Last Mutr PacketID in South Arm */
  const int NORTH_PACKET_MAX = 11362; 
  
  /*! Number of ADC samples per cathode strip */
  const int NSAMPLES = 4;    
  
  /*! Number of user words from FEM */
  const int NUWORDS = 8;     
  
  /*! Number of header words in DCM */
  const int NHWORDS = 5;    
  
  /*! Number of trailer words in DCM */
  const int NTWORDS = 10;    
  
  /*! Number data words in cathode FEM */
  const int NCATHWORDS = 512;   
  
  /*! Number of channels in a Cath. cable */
  const int NUMCABLE = 16;      
  
  /*! Number of channels in a Cath. FEM */
  const int CATH_FEM_SIZE = 128; 
  
  /*! Number data words in anode FEM */
  const int NANWORDS = 120;     
  
  /*! Number of bits holding latch info/word*/
  const int NANBITS = 16;       
  
  /*! Number of latches in an anode FEM */
  const int ANFEMSIZE = 1920;   
  
  /*! Minimum value of ADC */
  const int ADC_MIN = 0;
  
  /*! RMS error assigned to saturated ADCs */
  const int SATURATED_ADC_ERROR = 100;
  
  /*! RMS error assigned to scratched cathodes */
  const int ATTEN_ADC_ERROR = 200;
  
  /*! RMS error assigned to dead channels */
  const int DEAD_CHANNEL_ERROR = 300;
  
  // Bits for packing detector information into Module Address:
  //
  const int ARMSHIFT = 12;
  const int ARMSTART = 0x1000;
  const int ARMMASK = 0x1000;
  const int STSHIFT = 10;
  const int STSTART = 0x400;
  const int STMASK = 0xc00;
  const int OCSHIFT = 7;
  const int OCSTART = 0x80;
  const int OCMASK = 0x380;
  const int FEMADDSHIFT = 2;
  const int FEMADDSTART = 0x4;
  const int FEMADDMASK = 0x7c;
  
  //! anode DetID
  const int ANDETID = 0xa00;
  
  //! cathode DetID
  const int CATHDETID = 0xb00;
  
  //! FPGA version until run4 (included)
  const int FPGAVERSIONRUN4 = 0x80000;
  
  //! FPGA version fom run5 (included)
  const int FPGAVERSIONRUN5 = 0x80140;
  
  //! FPGA version fom run7 (included)
  const int FPGAVERSIONRUN7 = 0x80140;
  
}

#endif
