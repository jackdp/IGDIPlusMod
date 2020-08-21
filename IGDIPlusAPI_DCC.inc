{$IFNDEF FPC}
  {$IF CompilerVersion < 22}
    {$DEFINE DCC}
  {$IFEND}
{$ENDIF}

{$IFDEF DCC}


  {$IF CompilerVersion <= 20}
    {$DEFINE DELPHI2009_OR_BELOW}
  {$IFEND}

  {$IF CompilerVersion >= 20}
    {$DEFINE DELPHI2009_OR_ABOVE}
    {$DEFINE DELPHI_UNICODE}
  {$IFEND}


  {$IF CompilerVersion <= 21}
    {$DEFINE DELPHI2010_OR_BELOW}
  {$IFEND}

  {$IF CompilerVersion >= 21}
    {$DEFINE DELPHI2010_OR_ABOVE}
  {$IFEND}


  {$IF CompilerVersion >= 22}
    {$DEFINE DELPHIXE_OR_UP}
    {$DEFINE HAS_FORMATSETTINGS}
  {$IFEND}

  {$IF CompilerVersion >= 23}
    {$DEFINE DELPHIXE2_OR_ABOVE}
    {$DEFINE HAS_SYSTEM_UITYPES}
  {$IFEND}

  {$IF CompilerVersion >= 24}
    {$DEFINE DELPHI_XE3_OR_ABOVE}
  {$IFEND}

  {$IF CompilerVersion >= 25}
    {$DEFINE DELPHI_XE4_OR_ABOVE}
  {$IFEND}



{$ENDIF}


