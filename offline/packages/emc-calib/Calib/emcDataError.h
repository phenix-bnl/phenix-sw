#ifndef __EMCDATAERROR_H__
#define __EMCDATAERROR_H__

/** About data readout errors.
    
Except at the very last stage of calibration,
data are never reset to zero, they are only flagged. 
Thus even if counter is knowingly dead - its energy and time are not 
necessarily zeros. 
Checks should be made both for the dead mask and for the error code 
(with 0x2000 mask).

@ingroup calibration
*/

  class emcDataError
  {

  public:

    /// Everything is fine.
    static int OK(void)
    {
      return 0;
    }

    /// high gain pre-sample value is OUT-OF-RANGE
    static int HG_PRE_OUT(void)
    {
      return 0x4;
    }

    /// high gain post-sample value is OUT-OF-RANGE
    static int HG_POST_OUT(void)
    {
      return 0x8;
    }

    /// low gain pre-sample value is OUT-OF-RANGE
    static int LG_PRE_OUT(void)
    {
      return 0x40;
    }

    /// low gain post-sample value is OUT-OF-RANGE
    static int LG_POST_OUT(void)
    {
      return 0x80;
    }

    /// TAC value is OUT-OF-RANGE
    static int TAC_OUT(void)
    {
      return 0x400;
    }

    /// Channel has been disabled
    static int CHANNEL_DISABLED(void)
    {
      return 0x2000;
    }

    /// Minimum high gain (either pre or post) allowed
    static int HG_MIN(void)
    {
      return 1024;
    }

    /// Minimum low gain (either pre or post) allowed
    static int LG_MIN(void)
    {
      return 0;
    }

    /// Maximum high gain (either pre or post) allowed
    static int HG_MAX(void)
    {
      return 4095;
    }

    /// Maximum low gain (either pre or post) allowed
    static int LG_MAX(void)
    {
      return 4095;
    }

    /// Minimum tac-sample value allowed
    static int TAC_MIN(void)
    {
      return 50;
    }

    /// Maximum tac-sample value allowed
    static int TAC_MAX(void)
    {
      return 4000;
    }

  };

#endif
