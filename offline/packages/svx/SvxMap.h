#ifndef __SVXMAP_H__
#define __SVXMAP_H__

class PHTimeStamp;

class SvxMap {
  public:
    SvxMap(){};
    virtual ~SvxMap(){};

  public:

    bool commitPixelPacketMap(const char* description, PHTimeStamp tStart, PHTimeStamp tStop, int size, int* array);
    bool fetchPixelPacketMap(PHTimeStamp tStart, int *size, int** array);

  protected:
    bool commitDBIntArray(const char *tablename, const char *description, 
                          PHTimeStamp tStart, PHTimeStamp tStop, int size, int* array);
    bool fetchDBIntArray(const char *tablename, PHTimeStamp tStart, int *size, int** array);
};

#endif

