#ifndef _RAWDATACHECKDEFS_H_
#define _RAWDATACHECKDEFS_H_

// definitions for the raw data check values stored in the event header - never ever change the existing values
// we can add up to 255 different values, that should be enough even for the worst subsystem

namespace RawChk
{
  enum {BADLEN = 1, 
        DcmCheckSum = 2, 
        FEMParity = 3, 
        DcmFEMParity = 4, 
        FEMClock = 5, 
        FEMEvent = 6, 
        GL1Clock = 7, 
        SUBSYSCHK = 8, 
        LDTBTIMEOUT = 9, 
        NOACTIVERCC = 10, 
        RCCCLOCK = 11, 
        BADCELLID = 12, 
        MIXEDCELLID = 14, 
        SPIROEvent = 15,
        SPIROClock03 = 16,
        SPIROClock47 = 17,
        INFO = 0xFF};
};

#endif

 
