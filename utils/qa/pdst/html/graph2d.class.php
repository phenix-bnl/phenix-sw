<?php
/*******************************************************************************************
 *           Author    : Yuriy Rusinov                                                     *
 *           Contact   : rusinov@quasar.ipa.nw.ru                                          *
 *           Copyright : free for non-commercial use.                                      *
 *******************************************************************************************/

    class LabelFormat
    {
        var $width;
        var $precision;
        var $specific;
        var $vertical;
        var $fontsize;
        var $fun;

        function LabelFormat ($width=0, $prec=1, $spec="f", $fontsize=3, $vert=0, $function="")
        {
            $this->fun=$function;
            $this->width=$width;
            $this->precision=$prec;
            $this->specific=$spec;
            $this->fontsize=$fontsize;
            $this->vertical=$vert;
        }

        function Out ($value)
        {
//            $this->fun=$fun;
            $format_string="%";
            if ($this->width)
                $format_string.=$this->width;
            if ($this->precision)
                $format_string.=".".$this->precision;
            $format_string.=$this->specific;
            if (!empty($this->fun))
            {
                $out=call_user_func ($this->fun, $value);
                $wstring=sprintf($format_string, $out);
            }
            else
                $wstring=sprintf($format_string, $value);
            return $wstring;
        }
    }

    class graph2d
    {
//
//      Image type: jpeg, png, wbmp
//
        var $image_type;
//
//      Image sizes
//
        var $dimx;
        var $dimy;
//
//      x, y ranges, scales, formats
//
        var $xmin;
        var $xmax;

        var $ymin;
        var $ymax;

        var $xstep;
        var $ystep;

        var $xskip;
        var $yskip;

        var $xformat;
        var $yformat;
//
//      background and foreground colors and trasparent;
//
        var $b_red;
        var $b_green;
        var $b_blue;
        var $b_alpha;
        var $a_red;
        var $a_green;
        var $a_blue;
        var $a_alpha;
//
//      coordinate grid
//
        var $is_grid;
        var $grid_red;
        var $grid_green;
        var $grid_blue;
//        var $grid_style;
//
//      left, top, right, bottom shifts
//
        var $top;
        var $left;
        var $right;
        var $bottom;
//
//      Titles
//
        var $xlegend;
        var $ylegend;
        var $title;
//
//      Image resource
//
        var $image;

        function graph2d ($image_type, $width, $height, $xleft=0, $ytop=0, $xright=0, $ybottom=0, $br=0xFF, $bg=0xFF, $bb=0xFF, $b_alpha=0, $ar=0, $ag=0, $ab=0, $a_alpha=0)
        {
            $this->image_type=$image_type;
            $this->dimx=(int)$width;
            $this->dimy=(int)$height;
            
            $this->b_red=(int)$br;
            $this->b_green=(int)$bg;
            $this->b_blue=(int)$bb;

            $this->a_red=(int)$ar;
            $this->a_green=(int)$ag;
            $this->a_blue=(int)$ab;

            $this->a_alpha=(int)$a_alpha;
            $this->b_alpha=(int)$b_alpha;

            $this->left=(int)$xleft;
            $this->top=(int)$ytop;
            $this->right=(int)$xright;
            $this->bottom=(int)$ybottom;

            $this->xskip=1;
            $this->yskip=1;

            $this->xformat=new LabelFormat(1,0,"d",6,0,"");
            $this->yformat=new LabelFormat(1,2,"f",6,0,"");

            $this->xlegend="";
            $this->ylegend="";
            $this->title="";

            $this->is_grid=FALSE;
//            $this->grid_style=array();

            $imtype="Content-type: image/".$this->image_type;
            header ($imtype);
            $this->image=@imagecreate ($this->dimx, $this->dimy);// or die "Cannot initialize new GD Image stream";
        }

        function SetAxisScales ($xmin, $xmax, $xstep, $xskip, $ymin, $ymax, $ystep, $yskip)
        {
            $this->xmin=(double)$xmin;
            $this->xmax=(double)$xmax;
            $this->xstep=(double)$xstep;
            $this->xskip=(int)$xskip;

            $this->ymin=(double)$ymin;
            $this->ymax=(double)$ymax;
            $this->ystep=(double)$ystep;
            $this->yskip=(int)$yskip;
        }

        function TranslateEntity($str)
            {
                 $trans = array("&lt;" => "<", "&gt;" => ">");
                 return strtr($str, $trans);
           }

        function SetXLegend ($xleg) { $this->xlegend=$this->TranslateEntity($xleg); }

        function SetYLegend ($yleg) { $this->ylegend=$this->TranslateEntity($yleg); }

        function SetTitle ($title) { $this->title=$title; }

        function SetLineThickness ($thikness)
        {
             if ( is_resource ($this->image) && is_int ($thikness))
                 imagesetthickness ($this->image, $thikness);
        }

        function SetGrid($grid, $red, $green, $blue)
        {
            $this->is_grid=$grid;
            $this->grid_red=$red;
            $this->grid_green=$green;
            $this->grid_blue=$blue;
//            $this->grid_style=$style;
        }

        function GraphCoord ()
        {
            if (function_exists("imagecolorallocatealpha"))
            {
                $background=imagecolorallocatealpha ($this->image, $this->b_red, $this->b_green, $this->b_blue, $this->b_alpha);
                $foreground=imagecolorallocatealpha ($this->image, $this->a_red, $this->a_green, $this->a_blue, $this->a_alpha);
            }
            else
            {
                $background=imagecolorallocate ($this->image, $this->b_red, $this->b_green, $this->b_blue);
                if ($this->b_alpha)
                    imagecolortransparent($this->image, $background);
                $foreground=imagecolorallocate ($this->image, $this->a_red, $this->a_green, $this->a_blue);
            }

            $X=$this->dimx-($this->left+$this->right);
            $Y=$this->dimy-($this->top+$this->bottom);

            $scaleX=($this->xmax-$this->xmin)/$X;
            $scaleY=($this->ymax-$this->ymin)/$Y;

            $gcolor=imagecolorallocate ($this->image, $this->grid_red, $this->grid_green, $this->grid_blue);
            if ($this->is_grid)
                $coord=imagerectangle($this->image, $this->left, $this->top, $this->dimx-$this->right, $this->dimy-$this->bottom, $gcolor);
            else
                $coord=imagerectangle($this->image, $this->left, $this->top, $this->dimx-$this->right, $this->dimy-$this->bottom, $foreground);
            if (!empty($this->title))
                imagestring ($this->image, 10, $this->left+$X/2-strlen($this->title)*imagefontwidth(10)/2, $this->top/2-imagefontheight(10)/2, $this->title, $foreground);
            $numx=($this->xmax-$this->xmin)/$this->xstep+1;
            $numy=($this->ymax-$this->ymin)/$this->ystep+1;
            for ($i=0; $i<$numx; $i++)
            {
                $x=($i*$X)/($numx-1)+$this->left;
                if ($i>0 && $i<$numx-1)
                    if ($this->is_grid)
                        imageline ($this->image, $x, $Y+$this->top, $x, $this->top, $gcolor);
                    else
                        imageline ($this->image, $x, $Y+$this->top-5, $x, $Y+$this->top+5, $foreground);
                if ($i%$this->xskip==0)
                {
									$tmpstr = $this->xmin+($i*$X)*$scaleX/($numx-1);
									if ($tmpstr>9999999)  // if is unix time
										$tmpstr = time2run($this->xmin+($i*$X)*$scaleX/($numx-1)); // translate time to run number. Cesar
                   $xstr=$this->xformat->Out($tmpstr); 
                    if ($this->xformat->vertical)
                        imagestringup ($this->image, $this->xformat->fontsize, $x-imagefontwidth($this->xformat->fontsize), $Y+$this->top+strlen($xstr)*imagefontheight($this->xformat->fontsize)/2+5, $xstr, $foreground);
                    else
                        imagestring ($this->image, $this->xformat->fontsize, $x-strlen($xstr)*imagefontwidth($this->xformat->fontsize)/2, $Y+$this->top+imagefontheight($this->xformat->fontsize)/2, $xstr, $foreground);
                }
            }
            if (!empty($this->xlegend))
                imagestring ($this->image, 8, $this->left+$X/2, $this->top+$Y+3*imagefontheight(8)/2, $this->xlegend, $foreground);
            for ($i=0; $i<$numy; $i++)
            {
                $y=($i*$Y)/($numy-1);
                if ($i>0 && $i<$numy-1)
                    if ($this->is_grid)
                        imageline ($this->image, $this->left-5, $y+$this->top, $this->left+$X, $y+$this->top, $gcolor);
                    else
                        imageline ($this->image, $this->left-5, $y+$this->top, $this->left+5, $y+$this->top, $foreground);
                if ($i%$this->yskip==0)
                {
                    $ystr=$this->yformat->Out($this->ymin+($i*$Y)*$scaleY/($numy-1));
                    if ($this->yformat->vertical)
                        imagestringup ($this->image, $this->yformat->fontsize, $this->left-imagefontheight($this->yformat->fontsize)-5, $Y+$this->top+$this->bottom-$y-3*strlen($ystr)*imagefontwidth($this->yformat->fontsize)/2, $ystr, $foreground);
                    else
                        imagestring ($this->image, $this->yformat->fontsize, $this->left-strlen($ystr)*imagefontwidth($this->yformat->fontsize)-5, $Y+$this->top-$y-imagefontheight($this->yformat->fontsize)/2, $ystr, $foreground);
                }
            }
            if (!empty($this->ylegend))
                imagestringup ($this->image, 8, $this->left/2-3*imagefontheight(8)/2, $this->top+$Y/2+strlen($this->ylegend)*imagefontwidth(8)/2, $this->ylegend, $foreground);
        }

        function Draw ($xarray, $yarray, $yerror, $r=0, $g=0, $b=0)
        {
            if (!is_array($xarray) || !is_array($yarray) || !is_array($yerror)) return;//die "Invalid arguments<br>\n";

            if (function_exists("imagecolorallocatealpha"))
            {
                $background=imagecolorallocatealpha ($this->image, $this->b_red, $this->b_green, $this->b_blue, $this->b_alpha);
                $foreground=imagecolorallocatealpha ($this->image, $this->a_red, $this->a_green, $this->a_blue, $this->a_alpha);
            }
            else
            {
                $background=imagecolorallocate ($this->image, $this->b_red, $this->b_green, $this->b_blue);
                if ($this->b_alpha)
                    imagecolortransparent($this->image, $background);
                $foreground=imagecolorallocate ($this->image, $this->a_red, $this->a_green, $this->a_blue);
            }
            $graphcolor=imagecolorallocate ($this->image, $r, $g, $b);

            $X=$this->dimx-($this->left+$this->right);
            $Y=$this->dimy-($this->top+$this->bottom);

            $scaleX=($this->xmax-$this->xmin)/$X;
            $scaleY=($this->ymax-$this->ymin)/$Y;

            $numx=($this->xmax-$this->xmin)/$this->xstep+1;
            $numy=($this->ymax-$this->ymin)/$this->ystep+1;

            $nx=count($xarray);
            $ny=count($yarray);
            $n=min($nx, $ny);
            $Xerr = $X/200;

            for ($i=0; $i<$n; $i++)
               {
                     if (($yarray[$i]+$yerror[$i])<$this->ymin || ($yarray[$i]-$yerror[$i])>$this->ymax) continue; 
                     if ($yerror[$i]>0) $error = $yerror[$i];
                     else $error = ($this->ymax-$this->ymin)/160;
                     imageline($this->image, $this->left+($xarray[$i]-$this->xmin)/$scaleX, $this->top+($this->ymax-$yarray[$i]+$error)/$scaleY, $this->left+($xarray[$i]-$this->xmin)/$scaleX, $this->top+($this->ymax-$yarray[$i]-$error)/$scaleY, $graphcolor);
                       imageline($this->image, $this->left+($xarray[$i]-$this->xmin)/$scaleX-$Xerr, $this->top+($this->ymax-$yarray[$i])/$scaleY, $this->left+($xarray[$i]-$this->xmin)/$scaleX+$Xerr, $this->top+($this->ymax-$yarray[$i])/$scaleY, $graphcolor);
            }
        }

        function ImageOut ($filename)
        {
            if (strcasecmp ($this->image_type, "jpeg")==0)
                imagejpeg ($this->image, $filename, 100);
            elseif (strcasecmp ($this->image_type, "png")==0)
                imagepng ($this->image, $filename, 100);
            elseif (strcasecmp ($this->image_type, "gif")==0)
                imagegif ($this->image, $filename, 100);
            elseif (strcasecmp ($this->image_type, "wbmp")==0)
                imagewbmp ($this->image, $filename, 100);
        }

        function Close ()
        {
            imagedestroy ($this->image);
        }
    }
?>