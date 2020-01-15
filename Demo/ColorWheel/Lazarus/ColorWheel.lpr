program ColorWheel;

{$MODE Delphi}

{$IFDEF DCC}
{$IF CompilerVersion >= 21.0}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

{$SetPEFlags 1} // IMAGE_FILE_RELOCS_STRIPPED
{$SetPEFlags $20} // IMAGE_FILE_LARGE_ADDRESS_AWARE
{$ENDIF}

uses
  {$IFDEF DCC}Vcl.Forms,{$ELSE}Forms,{$ENDIF}
  {$IFDEF FPC}Interfaces,{$ENDIF}
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  {$IFDEF DCC}Application.MainFormOnTaskbar := True;{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
