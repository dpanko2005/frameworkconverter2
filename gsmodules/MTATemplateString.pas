{ ------------------------------------------------------------------- }
{ Unit:    MTATemplateString.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit that holds a template string with tokens that are  }
{ replaced at runtime with valid values and saved as the Converter  }
{ metadata control file (.mta)
{ ------------------------------------------------------------------- }

unit MTATemplateString;

interface

const mtaTemplateStr : string =
'## NAME: ModelRunScenarioID: STRING' + sLineBreak +
'## DSCR: Used by the user to identify the run with a user friendly long name' + sLineBreak +
'$$ModelRunScenarioID$$     ' + sLineBreak +
'                               ' + sLineBreak +
'## NAME: SWMMNodeID: STRING   ' + sLineBreak +
'## DSCR: This is the identifier of the SWMM node for which the timeseries will be retrieved ' + sLineBreak +
'$$SWMMNodeID$$       ' + sLineBreak +
'                                 '+ sLineBreak +
'## NAME: SWMMFilePath: STRING    ' + sLineBreak +
'## DSCR: Path to swmm output file from which timeseries will be extracted  ' + sLineBreak +
'$$SWMMOutputFilePath$$           ' + sLineBreak +
'                                   ' + sLineBreak +
'## NAME: scratchFilePath : STRING     ' + sLineBreak +
'## DSCR: Scratch File Path    ' + sLineBreak +
'$$scratchFilePath$$           ' + sLineBreak +
'                            ' + sLineBreak +
'## NAME: FlowConv: FLOAT      ' + sLineBreak +
'## DSCR: Flow Unit Conversion Factor to potentially convert SWMM Flow Time Series to other units (e.g. metric, user-define, etc)    ' + sLineBreak +
'## DFLT: 1.0 - [no conversion]   ' + sLineBreak +
'## OTHR: 2.2 - [user defined]  ' + sLineBreak +
'$$FlowConv$$     ' + sLineBreak +
'                         '+ sLineBreak +
'## NAME: NumPolls  ' + sLineBreak +
'## DSCR: Total number of Pollutants in SWMM Model    ' + sLineBreak +
'$$NumPolls$$          ' + sLineBreak +
'                             ' + sLineBreak +
'## NAME: FrameworkPollutants ' + sLineBreak +
'## DSCR: SWMM Pollutant Matching - use na if pollutant not available in SWMM model / Concentration Unit Conversion Factor to mg/L     ' + sLineBreak +
'## FRMT: [''Framework Pollutant: STRING'' = ''SWMM Pollutant : STRING'' / ''Concentration conversion factor : FLOAT'']    ' + sLineBreak +
'$$FWPollutants$$          ' + sLineBreak +
'                  ' + sLineBreak +
'################## BEGIN OPTIONAL PARAMETERS ##########################################################################      ' + sLineBreak +
'                                                                                                                  ' + sLineBreak +
'## Scratch File Format Code: INTEGER                                                                               ' + sLineBreak +
'## DSCR: This will allow different file formats (e.g. XML, JSON) to be supported without the need to change this file    ' + sLineBreak +
'## DFLT: 0 - [Year, Month, Day, Hour, Flow, Load1 … Loadn]        ' + sLineBreak +
'## OTHR: 1 - [XML file - not yet supported]            ' + sLineBreak +
'## OTHR: 2 - [JSON file - not yet supported]      ' + sLineBreak +
'0                                           ' + sLineBreak +
'                                        ' + sLineBreak +
'## NAME: SeriesType: INTEGER           ' + sLineBreak +
'## DSCR: Type of time series for node   ' + sLineBreak +
'## DFLT: 0 - [total influent flows and loads only]    ' + sLineBreak +
'## OTHR: 1 - [total influent volumes and loads only - potential future versions may allow other time series types]   ' + sLineBreak +
'## OTHR: 2 - [other - potential future versions may allow other time series types]   ' + sLineBreak +
'0                                    ' + sLineBreak +
'                                    ' + sLineBreak +
'## NAME: GetAllNodes: Boolean        ' + sLineBreak +
'## DSCR: Option to either process all time series for all nodes or read specified node only      ' + sLineBreak +
'## DFLT: FALSE - [Process requested node ID only]      ' + sLineBreak +
'## OTHR: TRUE -  [Process all nodes]         ' + sLineBreak +
'FALSE                                       ' + sLineBreak +
'                                        ' + sLineBreak +
'## NAME: SilentMode: INTEGER      ' + sLineBreak +
'## DSCR: Used to allow program to display / ignore error messages or not, which is useful for debugging and troubleshooting  ' + sLineBreak +
'## DFLT: 0 - [no error messages]      ' + sLineBreak +
'## OTHR: 1 - [verbose mode - display error messages]    ' + sLineBreak +
'0                                  ' + sLineBreak +
'                             ' + sLineBreak +
'## NAME: StartDateTime : STRING       ' + sLineBreak +
'## DSCR: Option to allow extraction of timeseries starting from a specified period only   ' + sLineBreak +
'## DFLT: n/a - leave blank to extract entire timeseries                        ' + sLineBreak +
'## OTHR: 1990 10 01 00.00 - [Specify Year: 4 digit INTEGER, Month: 2 digit INTEGER, Day: 2 digit INTEGER, Hour: FLOAT]     ' + sLineBreak +
'0                          ' + sLineBreak +
'                           ' + sLineBreak +
'## NAME: EndDateTime : STRING   ' + sLineBreak +
'## DSCR: Option to allow extraction of timeseries ending at a specified period only    ' + sLineBreak +
'## DFLT: n/a - leave blank to extract entire timeseries   ' + sLineBreak +
'## OTHR: 1996 10 01 00.00 - [Specify Year: 4 digit INTEGER, Month: 2 digit INTEGER, Day: 2 digit INTEGER, Hour(00.00 to 23.99): FLOAT]  ';

implementation

end.
