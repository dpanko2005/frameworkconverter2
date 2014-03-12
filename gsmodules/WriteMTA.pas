{ ------------------------------------------------------------------- }
{ Unit:    ReadMTA.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit for writting the Converter  }
{ metadata control file (.mta)
{ ------------------------------------------------------------------- }

unit WriteMTA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO,
  MTATemplateString;

function Write(mtaFilePath: string; MTADataArr: TArray<TMTARecord>): Boolean;

implementation

function Write(mtaFilePath: string; MTADataArr: TArray<TMTARecord>): Boolean;
var
  FileContentsList: TStringList;
  TplContentsList: TStringList;
  intTokenLoc: integer;
  i: integer;
  // j: integer;
  modelRunScenarioID: string;
  SWMMNodeID: string;
  swmmFilePath: string;
  scratchFilePath: string;
  flowConvFactor: double;
  numPolls: integer;
  tplTokens: TArray<string>;
  tplTokenVals: TArray<string>;
  FWPollutantsStr: string;
  tplFilePath: string;
  tempStr: string;
begin
  Assert(Assigned(MTADataArr));

  FileContentsList := TStringList.Create;
  TplContentsList := TStringList.Create;
  FWPollutantsStr := '';
  tplTokens := TArray<string>.Create('$$ModelRunScenarioID$$', '$$SWMMNodeID$$',
    '$$SWMMOutputFilePath$$', '$$scratchFilePath$$', '$$FlowConv$$',
    '$$NumPolls$$', '$$FWPollutants$$');

  modelRunScenarioID := MTADataArr[0].modelRunScenarioID;
  if modelRunScenarioID = '' then
    modelRunScenarioID := 'No description provided';

  SWMMNodeID := MTADataArr[0].tsNodeName;
  swmmFilePath := MTADataArr[0].swmmFilePath;
  scratchFilePath := MTADataArr[0].scratchFilePath;
  flowConvFactor := MTADataArr[0].convFactor;
  numPolls := 0;

  for i := 1 to High(MTADataArr) do // exclude flow which is first item in MTADataArr
  begin
    if (MTADataArr[i].constituentFWName <> '') then
    begin
      FWPollutantsStr := FWPollutantsStr + Format('''%s = %s / %f''',
        [MTADataArr[i].constituentFWName, MTADataArr[i].constituentSWMMName,
        MTADataArr[i].convFactor]) + sLineBreak;
        inc(numPolls);
    end;
  end;

  tplTokenVals := TArray<string>.Create(modelRunScenarioID, SWMMNodeID,
    swmmFilePath, scratchFilePath, FloatToStr(flowConvFactor),
    IntToStr(numPolls), FWPollutantsStr);
  // tplFilePath :=
  // 'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\mtaTemplate.txt';
  try
    { First check if the file exists. }
    // if (FileExists(tplFilePath)) then
    // begin
    // replace tokens in template with values
    //TplContentsList.LoadFromFile(tplFilePath);
    tempStr := MTATemplateString.mtaTemplateStr;
    for i := 0 to High(tplTokens) do
    begin
      tempStr := StringReplace(tempStr, tplTokens[i], tplTokenVals[i],
        [rfReplaceAll, rfIgnoreCase]);
      // intTokenLoc := TplContentsList.IndexOf(tplTokens[i]);
      // if (intTokenLoc > 0) then
      // TplContentsList[intTokenLoc] := tplTokenVals[i];
    end;
    TplContentsList.Add(tempStr);
    TplContentsList.SaveToFile(mtaFilePath);
    // end;
  finally
    FileContentsList.Free;
    TplContentsList.Free;
  end;
  result := true;
end;

end.
