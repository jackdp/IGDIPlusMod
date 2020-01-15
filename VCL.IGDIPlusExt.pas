{******************************************************************************

              Copyright (C) 2008-2015 by Boian Mitov
              mitov@mitov.com
              www.mitov.com
              www.igdiplus.org
              www.openwire.org

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

******************************************************************************}
unit VCL.IGDIPlusExt;

{$IFDEF FPC}{$mode Delphi}{$ENDIF}

interface

uses
  IGDIPlus, {$IFDEF DCC}VCL.Graphics;{$ELSE} Graphics;{$ENDIF}

type
  TIGPBitmapHelper = class helper for TIGPBitmap
  public
    constructor Create( ABitmap : TBitmap ); overload;
    constructor Create( AIcon : TIcon ); overload;

  end;
//---------------------------------------------------------------------------
  TIGPGraphicsHelper = class helper for TIGPGraphics
  public
    constructor Create( canvas : TCanvas ); overload;

  public
    class function FromCanvas( canvas : TCanvas ) : TIGPGraphics; overload;

  end;
//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------
constructor TIGPBitmapHelper.Create( ABitmap : TBitmap );
begin
  CreateHBitmap( ABitmap.Handle, ABitmap.Palette );
end;
//---------------------------------------------------------------------------
constructor TIGPBitmapHelper.Create( AIcon : TIcon );
begin
  CreateHICON( AIcon.Handle );
end;
//---------------------------------------------------------------------------
class function TIGPGraphicsHelper.FromCanvas( canvas : TCanvas ) : TIGPGraphics;
begin
  Result := TIGPGraphics.Create(canvas);
end;
//---------------------------------------------------------------------------
constructor TIGPGraphicsHelper.Create( canvas : TCanvas );
begin
  Create( canvas.Handle );
end;
//---------------------------------------------------------------------------
end.
