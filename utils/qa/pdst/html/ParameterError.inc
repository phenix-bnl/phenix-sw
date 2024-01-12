<?php
class ParameterError
{
  var $Parameter = 0.0;
  var $ParError = 0.0;
  var $name = " ";
}

class ParameterErrorBank
{
  var $nparameters = 0;
  function ParameterErrorBank($str)
    {
      $str = substr($str,86);
      $pattern = substr($str,0,4);
      $istr = explode($pattern,$str, 40);
      $this->nparameters = 0 ;
      for($i=0; $i<count($istr); $i++)
	{
	  $tmpname = quoted_printable_decode(substr($istr[$i+1],11,29));
	  if (strlen($tmpname)==0) break;
	  $par[$i] = new ParameterError;
	  $this->par[$i]->Parameter = $this->bin2float(substr($istr[$i+1],3,4));
	  $this->par[$i]->ParError = $this->bin2float(substr($istr[$i+1],7,4));
	  $this->par[$i]->name = htmlentities($tmpname);
	  $this->nparameters ++ ;
	}
    }

   function bin2float($data)
   {
     $iret = 0.0;
     $sign = (@ord($data[0]) & 0x80)>>7;
     $exponent = ((@ord($data[0]) & 0x7F)<<1) | ((@ord($data[1]) & 0x80)>>7);
     $exponent -= 127;
     $mantissa = (@ord($data[1]) & 0x7F)<<16 | @ord($data[2])<<8 | @ord($data[3]);
     if ($mantissa!=0)
       {
	 $fraction = 1.0;
	 for($j=0; $j<23; ++$j)
	   {
	     $bitfrac = ($mantissa >> (22-$j)) & 0x00000001;
	     $fraction += floatval($bitfrac)/pow(2,$j+1);
	   }
	 $iret = pow(-1,$sign)*pow(2, $exponent)*$fraction;
       }
     return $iret;
   }
}
?>
