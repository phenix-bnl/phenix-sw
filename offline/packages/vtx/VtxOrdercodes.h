#ifndef __VtxOrder_h__
#define __VtxOrder_h__

// $Id: VtxOrdercodes.h,v 1.5 2011/08/11 13:48:40 akimoto Exp $

//! VTX namespace (to avoid collision with the rest of phenix code
namespace VTX
{
  
  /*! 
  enumeration used to decide order (i.e. precedence) of the available vertex estimates
  in VtxOut object.
  1 is reserved for use of a forced vertex
  negative numbers -> vertex is ignored in vertex selection
  */
  
  enum Order
  {
    
    //! Forced
    FORCEDORDER = 1,
    
    //! SVX_PRECISE: calculated with reconstructed tracks 
    SVX_PRECISEORDER = 2,

    //! SVX : calculated with hit clusters
    SVXORDER = 3,
    
    //! BBC
    BBCORDER = 4,
      
    //! ZDC
    ZDCORDER = 5,
    
    //! PAD
    PADORDER = 6,
    
    //! NTC
    NTCORDER = 7,
  
    //! NTCP (same order since ntc and ntcp were never together)
    NTCPORDER = 7,
  
    //! simulated vertex
    /*! it is used by VtxSimReco to set the vertex retrieved from the PISA node */
    SIMORDER = 10,
    
    //! MVD
    MVDORDER = -8,
    
    //! Muon 
    MUONORDER = -9,
    
    //! DEFAULT
    DEFAULTORDER = 100,
  };
  
};

#endif
