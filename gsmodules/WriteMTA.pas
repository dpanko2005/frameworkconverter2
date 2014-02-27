unit WriteMTA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO;

function Write(mtaFilePath: string; MTADataArr: TArray<TMTARecord>): Boolean;
procedure LoadResourceFile(aFile: string; ms: TMemoryStream);

implementation

function Write(mtaFilePath: string; MTADataArr: TArray<TMTARecord>): Boolean;
var
  FileContentsList: TStringList;
  TplContentsList: TStringList;
  //TempTokens: TStringList;
  //TempStrList: TStringList;
  //lineNumber: integer;
  intTokenLoc: integer;
  //strLine: string;
  //strToken: string;
  i: integer;
  //j: integer;
  modelRunScenarioID: string;
  SWMMNodeID: string;
  swmmFilePath: string;
  scratchFilePath: string;
  flowConvFactor: double;
  //pollConvFactor: double;
  numPolls: integer;
  //swmmConstituentName: string;
  //fwConstituentName: string;
  //tmpStream: TResourceStream;
  //ms: TMemoryStream;
  tplTokens: TArray<string>;
  tplTokenVals: TArray<string>;
  FWPollutantsStr: string;
  tplFilePath: string;
begin
  Assert(Assigned(MTADataArr));

  FileContentsList := TStringList.Create;
  TplContentsList := TStringList.Create;
  //TempStrList := TStringList.Create;
  FWPollutantsStr := '';
  tplTokens := TArray<string>.Create('$$ModelRunScenarioID$$', '$$SWMMNodeID$$',
    '$$SWMMOutputFilePath$$', '$$scratchFilePath$$', '$$FlowConv$$',
    '$$NumPolls$$', '$$FWPollutants$$');

  modelRunScenarioID := MTADataArr[0].modelRunScenarioID;
  SWMMNodeID := MTADataArr[0].tsNodeName;
  swmmFilePath := MTADataArr[0].swmmFilePath;
  scratchFilePath := MTADataArr[0].scratchFilePath;
  flowConvFactor := MTADataArr[0].convFactor;
  numPolls := High(MTADataArr);

  for i := 1 to numPolls do    //exclude flow which is first item in MTADataArr
  begin
    FWPollutantsStr := FWPollutantsStr + Format('''%s = %s / %f''',
      [MTADataArr[i].constituentFWName, MTADataArr[i].constituentSWMMName,
      MTADataArr[i].convFactor]) + sLineBreak;
  end;

  tplTokenVals := TArray<string>.Create(modelRunScenarioID, SWMMNodeID,
    swmmFilePath, scratchFilePath, FloatToStr(flowConvFactor),
    IntToStr(numPolls), FWPollutantsStr);
  tplFilePath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\mtaTemplate.txt';
  try
    { First check if the file exists. }
    if (FileExists(tplFilePath)) then
    begin
      // replace tokens in template with values
      TplContentsList.LoadFromFile(tplFilePath);
      for i := 0 to High(tplTokens) do
      begin
        intTokenLoc := TplContentsList.IndexOf(tplTokens[i]);
        if (intTokenLoc > 0) then
          TplContentsList[intTokenLoc] := tplTokenVals[i];
      end;
      TplContentsList.SaveToFile(mtaFilePath);
    end;
  finally
    FileContentsList.Free;
    TplContentsList.Free;
  end;
  result := true;
end;

procedure LoadResourceFile(aFile: string; ms: TMemoryStream);
var
  HResInfo: HRSRC;
  HGlobal: THandle;
  Buffer, GoodType: pchar;
  //i: integer;
  Ext: string;
begin
  Ext := uppercase(extractfileext(aFile));
  Ext := copy(Ext, 2, length(Ext));
  if Ext = 'HTM' then
    Ext := 'HTML';
  GoodType := pchar(Ext);
  aFile := changefileext(aFile, '');
  HResInfo := FindResource(HInstance, pchar(aFile), GoodType);
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal = 0 then
    raise EResNotFound.Create('Can''t load resource: ' + aFile);
  Buffer := LockResource(HGlobal);
  ms.clear;
  ms.WriteBuffer(Buffer[0], SizeOfResource(HInstance, HResInfo));
  ms.Seek(0, 0);
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
end;

end.
