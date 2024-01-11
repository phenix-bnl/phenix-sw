#ifndef __DCGLTRACK_H__
#define __DCGLTRACK_H__


typedef struct {
   short id;
   short arm;
   short dctracksid;
   short tectrackid;
   short pc1clusid;
   short pc2clusid;
   short pc3clusid;
   short tofrecid;
   short emcclusid;
   short richringid;
   float quality;
   short trackModel;
} DCGLTRACK_ST;
#endif /*__DCGLTRACK_H__*/
