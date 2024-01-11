#ifndef __FVTXOO_FEM_H__
#define __FVTXOO_FEM_H__

//! widely used front-end electronic enumerations
namespace FVTXOO_FEM
{

  // Update to new FVTX packet map, Jin Huang <jhuang@bnl.gov>
  const int SOUTH_PACKET_ID_BASE = 25001;
  const int NORTH_PACKET_ID_BASE = 25101;

  /*! Fvtx PacketID offset */
  const int PACKET_ID_BASE = SOUTH_PACKET_ID_BASE;

  // max packet size = 512 (size per FEMID) x 4 (FEMIDs per packet)
  const int PACKET_SIZE_MAX = 512 * 4 + 100;

  /*! Maximum number of packets*/
  const int NPACKET_MAX_PER_CAGE = 12;
  const int NPACKET_MAX_PER_ARM = NPACKET_MAX_PER_CAGE * 2;
  const int NPACKET_MAX = NPACKET_MAX_PER_ARM * 2;

  /*! Last Fvtx PacketID in South Arm */
  //  const int SOUTH_PACKET_MAX = 24024;
  const int SOUTH_PACKET_MAX = 25024;

  /*! Last Fvtx PacketID in South Arm */
//  const int NORTH_PACKET_MAX = 24048;
  const int NORTH_PACKET_MAX = 25124;

  // Simple way to get PACKET_ID using index packet_i = 0, 1 ... NPACKET_MAX -1
  class GET_FVTX_PACKET_ID
  {
  public:
    GET_FVTX_PACKET_ID(const int packet_i)
    {
      fPACKET_ID = 0;
      if (packet_i < 0 || packet_i >= NPACKET_MAX)
        fPACKET_ID = 0;
      else if (packet_i < NPACKET_MAX_PER_ARM)
        fPACKET_ID = SOUTH_PACKET_ID_BASE + packet_i;
      else
        fPACKET_ID = NORTH_PACKET_ID_BASE + packet_i - NPACKET_MAX_PER_ARM;
    }
    operator int()
    {
      return fPACKET_ID;
    }
    operator unsigned int()
    {
      return fPACKET_ID;
    }

    virtual
    ~GET_FVTX_PACKET_ID()
    {
    }

    int fPACKET_ID;

  };

  /*! Number of ADC samples per strip */
  const int NSAMPLES = 8;

  /*! Number of user words from FEM */
  const int NUWORDS = 0;

  /*! Number of header words in DCM */
  const int NHWORDS = 5;

  /*! Number of trailer words in DCM */
  const int NTWORDS = 2;

  /*! Number data words in FEM */
  const int NWEDGEWORDS = 512;

  /*! Number of ROC per cage */
  const int NUMROC_PER_CAGE = 6;

  /*! Number of column per ROC */
  const int NUMCOL_PER_ROC = 4;

  /*! Number of channels in a FEM card */
  const int NUMCABLE = 4;

  /*! Number of chips in a FEM channel */
  const int CHIP_FEM_SIZE = 52;

  /*! Number of channels in a chip */
  const int CHANNEL_CHIP_SIZE = 128;

  class LINEAR_CHAN_MAP
  {

    LINEAR_CHAN_MAP(){};
    ~ LINEAR_CHAN_MAP(){};
  public:

    static int  GET_CHAN(
        const int packet,
        const int fem_id,
        const int chip,
        const int channel
        )
    {
      int packet_idx = -1;
      if (packet >= SOUTH_PACKET_ID_BASE
          && packet < SOUTH_PACKET_ID_BASE + NPACKET_MAX_PER_ARM)
        {
          packet_idx = packet - SOUTH_PACKET_ID_BASE;
        }
      else if (packet >= NORTH_PACKET_ID_BASE
          && packet < NORTH_PACKET_ID_BASE + NPACKET_MAX_PER_ARM)
        {
          packet_idx = packet - NORTH_PACKET_ID_BASE + NPACKET_MAX_PER_ARM;
        }
      else
        return -1;

      if (fem_id<0 || fem_id >= NUMCABLE)
        return -2;
      if (chip<=0 || chip > CHIP_FEM_SIZE)
        return -3;
      if (channel<0 || channel >= CHANNEL_CHIP_SIZE)
        return -4;

      return ((packet_idx * NUMCABLE + fem_id) * CHIP_FEM_SIZE + (chip-1))
          * CHANNEL_CHIP_SIZE + channel;
    }

    static const int MAX_CHAN = NPACKET_MAX * NUMCABLE * CHIP_FEM_SIZE
        * CHANNEL_CHIP_SIZE;

  };

  /*! Number of bits holding latch info/word*/
  //const int NANBITS = 16;       
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
  const int ARMSHIFT = 15;
  const int ARMSTART = 0x8000;
  const int ARMMASK = 0x8000;
  const int CAGESHIFT = 14;
  const int CAGESTART = 0x4000;
  const int CAGEMASK = 0x4000;
  const int STSHIFT = 12;
  const int STSTART = 0x1000;
  const int STMASK = 0x3000;
  const int WEDGESHIFT = 7;
  const int WEDGESTART = 0x80;
  const int WEDGEMASK = 0xc80;
  const int FEMADDSHIFT = 2;
  const int FEMADDSTART = 0x4;
  const int FEMADDMASK = 0x7c;

  //! FVTX DetID
  const int FVTXDETID = 0x000a;

  //! FPGA version until run12 (included)
  const int FPGAVERSIONRUN12 = 0x40000;

}

#endif
