{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazIGDIPlus;

interface

uses
  IGDIPlus, IGDIPlusHelpers, VCL.IGDIPlusExt, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazIGDIPlus', @Register);
end.
