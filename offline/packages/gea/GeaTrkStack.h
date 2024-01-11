#ifndef __GEATRKSTACK_H__
#define __GEATRKSTACK_H__

long GeaTrkStack(const Int_t& mcTrack, Int_t& idPart, Int_t& idParent,
		 Float_t& pTot, Float_t& rVertex, Float_t& zVertex,
                 Float_t& pTheta, Float_t& pPhi, Int_t& nFile, 
                 Int_t& itParent, Int_t& itOrigin, Int_t& idOrigin);

#endif /*__GEATRKSTACK_H__*/
