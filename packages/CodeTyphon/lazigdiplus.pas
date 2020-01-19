{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazIGDIPlus;

{$warn 5023 off : no warning about unused units}
interface

uses
  IGDIPlus, IGDIPlusHelpers, VCL.IGDIPlusExt, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazIGDIPlus', @Register);
end.
