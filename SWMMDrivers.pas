unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Defaults, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Grids,
  Vcl.ExtCtrls, UserInputConfirmationDlg, OperationStatusDlgFrm, SWMMIO,
  SWMMInput, SWMMOutput, ReadMTA, WriteMTA, ComCtrls, GIFImg;

const
  constituentNames: array [0 .. 7] of string = ('FLOW', 'TSS', 'TP', 'DP',
    'DZn', 'TZN', 'DCU', 'TCU');
  Errs: array [0 .. 1] of string =
    ('An unknown error occured when reading the SWMM file',
    'An unknown error occured when saving the new SWMM file');

type
  TForm1 = class(TForm)
    OpenTextFileDialog1: TOpenTextFileDialog;
    SaveTextFileDialog1: TSaveTextFileDialog;
    btnSelectSWMMFile: TButton;
    txtSwmmFilePath: TLabel;
    cbxSwmmNode: TComboBox;
    sgdUserInputGrid: TStringGrid;
    cbxFlow: TComboBox;
    cbxDCu: TComboBox;
    cbxTZn: TComboBox;
    cbxDZn: TComboBox;
    cbxDP: TComboBox;
    cbxTP: TComboBox;
    cbxTSS: TComboBox;
    Label2: TLabel;
    btnCancel: TButton;
    btnHelp: TButton;
    btnNext: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    cbxTCu: TComboBox;
    RadioGroup1: TRadioGroup;
    txtScenarioDescr: TEdit;
    Label1: TLabel;
    Label9: TLabel;
    lblOperatingMode: TLabel;
    procedure btnSelectSWMMFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

    /// <summary>
    /// Displays a dialog to the user for confirmation of user input
    /// parameters and then processes the user import or export request when
    /// the confirmation dialog is dismissed
    /// </summary>
    /// <param name="Sender">
    /// Parent form - currently not used
    /// </param>
    procedure btnNextClick(Sender: TObject);
    procedure ProgressCallback(InProgressOverall: TProgressBar);
    // procedure executeImportExport(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    workingDirPath: string;
    { Private declarations }

  var
    swmmFilePath: string;

  public
    { Public declarations }
    function readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
      var Conv: TArray<TMTARecord>; Sender: TObject): TArray<TMTARecord>;

    procedure AssignMTARecFields(SWMMNodeName: string; var Conv: TMTARecord;
      constituentName: string; convFactor: Double; tsType: string;
      ScenarioDescription: string; swmmFilePath: string;
      fwScratchFilePath: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.ProgressCallback(InProgressOverall: TProgressBar);
var
  Index: Integer;
  kIndex: Integer;
begin
  MessageDlg('Press OK to start a long task...', mtInformation, [mbOK], 0);
  // 1000 steps
  InProgressOverall.Max := 1000;

  for Index := 1 to InProgressOverall.Max do
  begin
    InProgressOverall.Position := Index;
    // force application to process messages
    Application.ProcessMessages;
  end; // for Index := 1 to InProgressOverall.Max do begin
  MessageDlg('Task completed!', mtInformation, [mbOK], 0);
end;

procedure TForm1.btnNextClick(Sender: TObject);
var
  ConvertedFWTSArr: TArray<TMTARecord>;
  constituentCbxs: TArray<TComboBox>;
  tempStr: string;
  frameworkTSFileInPath, frameworkTSFileOutPath: string;
  j: Integer;
  tempModalResult: Integer;
  numSelectedConstituents: Integer;
  newSWMMInputFilePath: string;
  selectedNodeName: string;
  swmmNodeResults: TStringList;
  scenarioDescr: string;
  mtaFilePath: string;
begin

  // Launch project form and force user to provide the project information
  SWMMUserInputVerificationFrm := TUserInputVerificationFrm.Create(Application);
  // TODO delete - hardcoded filepaths
  frameworkTSFileInPath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemo.sct';
  frameworkTSFileOutPath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemoOut.sct';
  mtaFilePath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemo.mta';

  SetLength(ConvertedFWTSArr, High(constituentNames));
  scenarioDescr := txtScenarioDescr.Text;
  if (operatingMode = opModes[0]) then // importing from swmm Binary
    // do nothing swmmFilePath set globally by swmm file selection button,
  else // exporting to swmm input file
  begin
    newSWMMInputFilePath := workingDirPath + '\' +
      ChangeFileExt(ExtractFileName(Self.txtSwmmFilePath.Caption), '') +
      FormatDateTime('yyyymmddhhnnss', Now) + '.inp';
    swmmFilePath := newSWMMInputFilePath;
  end;

  with SWMMUserInputVerificationFrm do
  begin
    constituentCbxs := TArray<TComboBox>.Create(cbxFlow, cbxTSS, cbxTP, cbxDP,
      cbxDZn, cbxTZn, cbxDCu, cbxTCu);

    try
      begin // populate dialog fields
        txtSwmmFilePath.Caption := Self.txtSwmmFilePath.Caption;

        if Self.cbxSwmmNode.ItemIndex <> -1 then
          txtSWMMNodeID.Caption := cbxSwmmNode.Items[cbxSwmmNode.ItemIndex];
        selectedNodeName := txtSWMMNodeID.Caption;

        // column headers
        StringGrid1.Cells[0, 0] := '* Framework Constituent';
        StringGrid1.Cells[1, 0] := 'SWMM Equivalent';
        StringGrid1.Cells[2, 0] := 'Unit Conversion Factor';

        // Fill in confirmation string grid with user inputs
        for j := Low(constituentNames) to High(constituentNames) do
        begin
          StringGrid1.Cells[0, j + 1] := constituentNames[j];
          StringGrid1.Cells[1, j + 1] := 'Not Selected';
          // set all cells to not select and overwrite selected ones later
          StringGrid1.Cells[2, j + 1] := Self.sgdUserInputGrid.Cells[2, j + 1];
        end;

        // Flow is a special case constituent which is always selected
        StringGrid1.Cells[1, 1] := 'FLOW'; // flow is always selected
        AssignMTARecFields(selectedNodeName, ConvertedFWTSArr[0], 'FLOW',
          StrToFloat(Self.sgdUserInputGrid.Cells[2, 1]), 'FLOW', scenarioDescr,
          swmmFilePath, frameworkTSFileInPath);

        // save conversion factors and selected constituents into record array for use later
        numSelectedConstituents := 1;
        for j := Low(constituentNames) + 1 to High(constituentNames) do
        begin
          if constituentCbxs[j].ItemIndex <> -1 then
          begin
            StringGrid1.Cells[1, j + 1] := constituentCbxs[j].Items
              [constituentCbxs[j].ItemIndex];
            AssignMTARecFields(selectedNodeName, ConvertedFWTSArr[j],
              constituentNames[j], StrToFloat(Self.sgdUserInputGrid.Cells[2, 1]
              ), 'CONCEN', scenarioDescr, swmmFilePath, frameworkTSFileInPath);
            inc(numSelectedConstituents);
          end;
        end;
      end;
      lblNumberOfConstituents.Caption := 'Constituents (' +
        IntToStr(numSelectedConstituents) + ' of 8 selected)';

      tempModalResult := SWMMUserInputVerificationFrm.ShowModal;
      if (tempModalResult = mrOk) then
      begin
        imgBusy.Visible := true;
        (imgBusy.Picture.Graphic as TGIFImage).Animate := True;
        tempModalResult := SWMMUserInputVerificationFrm.ShowModal;
        // ShowProgress(ProgressCallback);
        // show form again and make progressbar visible
        if (operatingMode = opModes[0]) then // importing from swmm Binary
        begin
          SWMMOutput.importFromSWMMToFW(swmmFilePath, frameworkTSFileOutPath,
            selectedNodeName, ConvertedFWTSArr);
        end
        else // exporting to swmm input file
        begin
          ConvertedFWTSArr := readInFrameworkTSFile(frameworkTSFileInPath,
            StringGrid1, ConvertedFWTSArr, Sender);
          SWMMInput.finalizeExport(ConvertedFWTSArr, workingDirPath, Sender);
          // write exported TS to the confirmation dialog
          for j := Low(ConvertedFWTSArr) to High(ConvertedFWTSArr) do
          begin
            if (ConvertedFWTSArr[j].convertedTSFilePath <> '') then
              tempStr := tempStr + #13#10 + ConvertedFWTSArr[j]
                .convertedTSFilePath;
          end;
          // populate operation status confirmation dialog
          OperationStatusDlg.lblSWMMFilePath.Caption := newSWMMInputFilePath;
          OperationStatusDlg.lblTSFilesCreated.Caption := tempStr;
          OperationStatusDlg.ShowModal;

          // write TS and Inflow blocks to swmm input file and save as new file
          SWMMInput.updateSWMMInputFile(ConvertedFWTSArr, workingDirPath,
            Self.txtSwmmFilePath.Caption);
        end;
        // save user settings to MTA file to allow subsequent commandline executions without user intervention
        WriteMTA.Write(mtaFilePath + '2', ConvertedFWTSArr);
      end;

      { tempModalResult := SWMMUserInputVerificationFrm.ShowModal;
        if (tempModalResult = mrOk) then
        begin
        ConvertedFWTSArr := readInFrameworkTSFile(frameworkTSFileInPath,
        StringGrid1, ConvertedFWTSArr, Sender);
        SWMMInput.finalizeExport(ConvertedFWTSArr, workingDirPath, Sender);

        // write exported TS to the confirmation dialog
        for j := Low(ConvertedFWTSArr) to High(ConvertedFWTSArr) do
        begin
        if (ConvertedFWTSArr[j].convertedTSFilePath <> '') then
        tempStr := tempStr + #13#10 + ConvertedFWTSArr[j]
        .convertedTSFilePath;
        end;
        // populate operation status confirmation dialog
        OperationStatusDlg.lblSWMMFilePath.Caption := newSWMMInputFilePath;
        OperationStatusDlg.lblTSFilesCreated.Caption := tempStr;
        OperationStatusDlg.ShowModal;

        if (operatingMode = opModes[0]) then // importing from swmm Binary
        begin
        SWMMOutput.importFromSWMMToFW(swmmFilePath, frameworkTSFileOutPath,
        selectedNodeName, ConvertedFWTSArr);
        end
        else // exporting to swmm input file
        begin
        // write TS to swmm input file and save as new file
        SWMMInput.updateSWMMInputFile(ConvertedFWTSArr, workingDirPath,
        Self.txtSwmmFilePath.Caption);
        end;
        // save user settings to MTA file to allow subsequent commandline executions without user intervention
        WriteMTA.Write(mtaFilePath + '2', ConvertedFWTSArr);
        end; }
    finally
      // ConvertedFWTSArr.Free;
    end;
  end;
  Self.Close;
end;

{ procedure TForm1.executeImportExport(ConvertedFWTSArr: TArray<TMTARecord>; TASender:TObject);
  begin
  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then // importing from swmm Binary
  begin
  SWMMOutput.importFromSWMMToFW(swmmFilePath, frameworkTSFileOutPath,
  selectedNodeName, ConvertedFWTSArr);
  end
  else // exporting to swmm input file
  begin
  ConvertedFWTSArr := readInFrameworkTSFile(frameworkTSFileInPath,
  StringGrid1, ConvertedFWTSArr, Sender);
  SWMMInput.finalizeExport(ConvertedFWTSArr, workingDirPath, Sender);
  // write exported TS to the confirmation dialog
  for j := Low(ConvertedFWTSArr) to High(ConvertedFWTSArr) do
  begin
  if (ConvertedFWTSArr[j].convertedTSFilePath <> '') then
  tempStr := tempStr + #13#10 + ConvertedFWTSArr[j].convertedTSFilePath;
  end;
  // populate operation status confirmation dialog
  OperationStatusDlg.lblSWMMFilePath.Caption := newSWMMInputFilePath;
  OperationStatusDlg.lblTSFilesCreated.Caption := tempStr;
  OperationStatusDlg.ShowModal;

  // write TS and Inflow blocks to swmm input file and save as new file
  SWMMInput.updateSWMMInputFile(ConvertedFWTSArr, workingDirPath,
  Self.txtSwmmFilePath.Caption);
  end;
  // save user settings to MTA file to allow subsequent commandline executions without user intervention
  WriteMTA.Write(mtaFilePath + '2', ConvertedFWTSArr);
  end; }

procedure TForm1.AssignMTARecFields(SWMMNodeName: string; var Conv: TMTARecord;
  constituentName: string; convFactor: Double; tsType: string;
  ScenarioDescription: string; swmmFilePath: string; fwScratchFilePath: string);
begin
  Conv.tsUnitsFactor := 1.0;
  Conv.constituentSWMMName := constituentName;
  Conv.constituentFWName := constituentName + 'f';
  Conv.convFactor := convFactor;
  Conv.tsNodeName := SWMMNodeName;
  Conv.tsType := tsType;
  Conv.tsName := constituentName;
  Conv.modelRunScenarioID := ScenarioDescription;
  Conv.swmmFilePath := swmmFilePath;
  Conv.scratchFilePath := fwScratchFilePath;
end;

function TForm1.readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
  var Conv: TArray<TMTARecord>; Sender: TObject): TArray<TMTARecord>;
var
  FileContentsList: TStringList;
  strLine: string;
  lineNumber: Integer;
  tempStrList: TStrings;
  tempDateTimeStr: string;
  tempValueStr: string;
  i: Integer;
  j: Integer;
begin
  FileContentsList := TStringList.Create;
  tempStrList := TStringList.Create;
  try
    for i := Low(Conv) to High(Conv) do
    begin
      Conv[i].convertedTS := TStringList.Create;
    end;

    FileContentsList.LoadFromFile(filePath);
    lineNumber := 0;
    while lineNumber < FileContentsList.Count - 1 do
    begin
      strLine := FileContentsList[lineNumber];
      // ignore comment lines
      if (Pos('#', strLine) < 1) and (Length(strLine) > 1) then
      begin
        tempStrList.Clear();
        ExtractStrings([','], [], PChar(strLine), tempStrList);
        tempDateTimeStr := tempStrList[0] + '/' + tempStrList[1] + '/' +
          tempStrList[2] + ' ' + tempStrList[3];
        for i := Low(Conv) to High(Conv) do
        begin
          j := i + 4;
          if (j < tempStrList.Count - 1) then
          begin
            tempValueStr := tempStrList[j];
            Conv[i].convertedTS.Add(tempDateTimeStr + '	' + tempValueStr);
          end;
        end;
      end;
      inc(lineNumber);
    end;
  finally
    FileContentsList.Free;
    tempStrList.Free;
  end;
  result := Conv;
end;

procedure TForm1.btnSelectSWMMFileClick(Sender: TObject);
var
  TempListArr: TArray<TStringList>;
  intTokenLoc: Integer;
  swmmIDsListArr: TArray<TStringList>;
begin
  intTokenLoc := 0;
  try
    if OpenTextFileDialog1.Execute then
      { First check if the file exists. }
      if FileExists(OpenTextFileDialog1.FileName) then
      begin
        // save the directory so can write TS to same directory later
        workingDirPath := ExtractFileDir(OpenTextFileDialog1.FileName);
        swmmFilePath := OpenTextFileDialog1.FileName;
        txtSwmmFilePath.Caption := swmmFilePath;

        if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
        // importing from swmm binary file
        begin
          TempListArr := SWMMIO.getSWMMNodeIDsFromBinary(swmmFilePath);
          if (TempListArr[0].Count < 1) then
            ErrsList.Add(Errs[0]); // Unable to read nodes in SWMM input file
          if (TempListArr[1].Count < 1) then
            ErrsList.Add(Errs[0]);
          // Unable to read pollutant names in SWMM input file

          NodeNameList := TempListArr[0];
          PollList := TempListArr[1];
        end
        else // we are exporting to swmm so using SWMM input file
        begin

          // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
          swmmIDsListArr := SWMMIO.getSWMMNodeIDsFromTxtInput(swmmFilePath);
          SWMMIO.TSList := swmmIDsListArr[2];
          SWMMIO.InflowsList := swmmIDsListArr[3];
          SWMMIO.NodeNameList := swmmIDsListArr[0];
          SWMMIO.PollList := swmmIDsListArr[1];
        end
      end
      else
      begin
        { Otherwise, raise an exception. }
        raise Exception.Create('File does not exist.');
        Exit
      end;
    cbxSwmmNode.Items := SWMMIO.NodeNameList;
    cbxFlow.Items := SWMMIO.PollList;
    cbxDCu.Items := SWMMIO.PollList;
    cbxTCu.Items := SWMMIO.PollList;
    cbxTZn.Items := SWMMIO.PollList;
    cbxDZn.Items := SWMMIO.PollList;
    cbxDP.Items := SWMMIO.PollList;
    cbxTP.Items := SWMMIO.PollList;
    cbxTSS.Items := SWMMIO.PollList;
  finally
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ErrsList := TStringList.Create();
  Form1.color := clwhite;

  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
  // SWMM_TO_FW importing from swmm binary file
  begin
    lblOperatingMode.Caption := 'Exporting from SWMM to WERF Framework';
    btnSelectSWMMFile.Caption := 'Select SWMM Results Output File';
    OpenTextFileDialog1.Filter := 'SWMM Results (*.out)|*.OUT';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
  end
  else // SWMM_FROM_FW we are exporting to swmm so using SWMM input file
  begin
    lblOperatingMode.Caption := 'Importing from SWMM to WERF Framework';
    btnSelectSWMMFile.Caption := 'Select SWMM Input File';
    OpenTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
  end;

  // column headers
  sgdUserInputGrid.Cells[0, 0] := '* Framework Constituent';
  sgdUserInputGrid.Cells[1, 0] := 'SWMM Equivalent';
  sgdUserInputGrid.Cells[2, 0] := 'Unit Conversion Factor';

  // default constituents
  sgdUserInputGrid.Cells[0, 1] := 'FLOW ';
  sgdUserInputGrid.Cells[0, 2] := 'TSS';
  sgdUserInputGrid.Cells[0, 3] := 'TP';
  sgdUserInputGrid.Cells[0, 4] := 'DP ';
  sgdUserInputGrid.Cells[0, 5] := 'DZn';
  sgdUserInputGrid.Cells[0, 6] := 'TZN';
  sgdUserInputGrid.Cells[0, 7] := 'DCU';
  sgdUserInputGrid.Cells[0, 8] := 'TCU';

  // default selected constituents - Flow is allways selected
  sgdUserInputGrid.Cells[1, 1] := 'FLOW ';

  // default unit conversion factors
  sgdUserInputGrid.Cells[2, 1] := '1.00';
  sgdUserInputGrid.Cells[2, 2] := '1.00';
  sgdUserInputGrid.Cells[2, 3] := '1.00';
  sgdUserInputGrid.Cells[2, 4] := '1.00 ';
  sgdUserInputGrid.Cells[2, 5] := '1.00';
  sgdUserInputGrid.Cells[2, 6] := '1.00';
  sgdUserInputGrid.Cells[2, 7] := '1.00';
  sgdUserInputGrid.Cells[2, 8] := '1.00';

  { // mode of operation radio buttons
    RadioGroup1.Items.AddObject('Import from SWMM', TObject(0));
    RadioGroup1.Items.AddObject('Export to SWMM', TObject(1));
    RadioGroup1.ItemIndex := 0; }
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
  Index: Integer;
  val: Integer;
begin
  index := RadioGroup1.ItemIndex;
  Assert(index >= 0);
  // Sanity check
  val := Integer(RadioGroup1.Items.Objects[index]);
  if (val = 0) then
  begin
    btnSelectSWMMFile.Caption := 'Select SWMM Output File';
  end;
  if (val = 1) then
  begin
    btnSelectSWMMFile.Caption := 'Select SWMM Input File';
  end;
  operatingMode := opModes[val];
end;

end.
