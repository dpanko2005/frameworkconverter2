{ ------------------------------------------------------------------- }
{ Unit:    WriteMTA.pas                                               }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit for writting the Converter                       }
{ metadata control file (.mta)                                        }
{ ------------------------------------------------------------------- }

unit WriteMTA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO,
  MTATemplateString;

///	<summary>
///	  Function to write SWMM 5 converter control file to (*.mta) to disc using
///	  the contents of an array of structured records (TMTARecords).Uses a string
///  template stored in unit MTATemplateString.pas and searches and replaces the
///  token placeholders in the template with actual values
///	</summary>
///	<param name="mtaFilePath">
///	  path to the location where .mta will be writte
///	</param>
///	<param name="MTADataArr">
///	  array of MTA records to write to disc
///	</param>
///	<returns>
///	  True if operation was successful
///	</returns>
function Write(mtaFilePath: string; MTADataArr: TArray<TMTARecord>): Boolean;

implementation

function Write(mtaFilePath: string; MTADataArr: TArray<TMTARecord>): Boolean;
var
  FileContentsList: TStringList;
  TplContentsList: TStringList;
  i: integer;
  modelRunScenarioID: string;
  SWMMNodeID: string;
  swmmFilePath: string;
  scratchFilePath: string;
  flowConvFactor: double;
  numPolls: integer;
  tplTokens: TArray<string>;
  tplTokenVals: TArray<string>;
  FWPollutantsStr: string;
  tempStr: string;
begin
  Assert(Assigned(MTADataArr));

  FileContentsList := TStringList.Create;
  TplContentsList := TStringList.Create;
  FWPollutantsStr := '';

  //list of tokens found in template string in unit MTATemplateStrings.pas which
  // will be searched for and replaced with real values
  //create array of token placeholders
  tplTokens := TArray<string>.Create('$$ModelRunScenarioID$$', '$$SWMMNodeID$$',
    '$$SWMMOutputFilePath$$', '$$scratchFilePath$$', '$$FlowConv$$',
    '$$NumPolls$$', '$$FWPollutants$$');

  modelRunScenarioID := MTADataArr[0].modelRunScenarioID;
  //if no description is provided use the default description below
  if modelRunScenarioID = '' then
    modelRunScenarioID := 'No description provided';

  SWMMNodeID := MTADataArr[0].tsNodeName;
  swmmFilePath := MTADataArr[0].swmmFilePath;
  scratchFilePath := MTADataArr[0].scratchFilePath;
  flowConvFactor := MTADataArr[0].convFactor;
  numPolls := 0;

  //starts at 1 to exclude flow which is first item in MTADataArr
  for i := 1 to High(MTADataArr) do
  begin
  //if the data structure for the current constituent is populated then process it
    if (MTADataArr[i].constituentFWName <> '') then
    begin
      FWPollutantsStr := FWPollutantsStr + Format('''%s = %s / %f''',
        [MTADataArr[i].constituentFWName, MTADataArr[i].constituentSWMMName,
        MTADataArr[i].convFactor]) + sLineBreak;
        inc(numPolls);
    end;
  end;

  //creates and array of token values as companion array for array of token
  // placeholders above (tplTokens)
  tplTokenVals := TArray<string>.Create(modelRunScenarioID, SWMMNodeID,
    swmmFilePath, scratchFilePath, FloatToStr(flowConvFactor),
    IntToStr(numPolls), FWPollutantsStr);
  try

    tempStr := MTATemplateString.mtaTemplateStr;
    //loop through and replace each token placeholder with its corresponding token value
    // in the string template
    for i := 0 to High(tplTokens) do
    begin
      tempStr := StringReplace(tempStr, tplTokens[i], tplTokenVals[i],
        [rfReplaceAll, rfIgnoreCase]);
    end;
    TplContentsList.Add(tempStr);
    //save the .mta
    TplContentsList.SaveToFile(mtaFilePath);
  finally
    FileContentsList.Free;
    TplContentsList.Free;
  end;
  result := true;
end;

end.
