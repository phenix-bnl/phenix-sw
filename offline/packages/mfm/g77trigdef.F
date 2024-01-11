      real function sind(x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      sind = sin(x*RADEG)
      return
      end

      real function cosd(x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      cosd = cos(x*RADEG)
      return
      end

      real function tand(x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      tand = tan(x*RADEG)
      return
      end

      real function asind(x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      asind = DEGRAD*asin(x)
      return
      end

      real function acosd(x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      acosd = DEGRAD*acos(x)
      return
      end

      real function atand(x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      atand = DEGRAD*atan(x)
      return
      end

      real function atan2d(y,x)
      real DEGRAD
      parameter (DEGRAD = 57.295779513)
      real RADEG
      parameter (RADEG = 1.745329252E-02)
      atan2d = DEGRAD*atan2(y,x)
      return
      end
