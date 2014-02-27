unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Defaults, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Grids,
  Vcl.ExtCtrls, UserInputConfirmationDlg, OperationStatusDlgFrm, SWMMIO,
  SWMMInput, SWMMOutput, ReadMTA, WriteMTA;

const
  constituentNames: array [0 .. 7] of string = ('FLOW', 'TSS', 'TP', 'DP',
    'DZn', 'TZN', 'DCU', 'TCU');
  Errs: array [0 .. 1] of string =
    ('An unknown error occured when reading the SWMM file',
    'An unknown error occured when saving the new SWMM file');
  opModes: array [0 .. 1] of string = ('import', 'export');

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
    Image1: TImage;
    cbxTCu: TComboBox;
    RadioGroup1: TRadioGroup;

    procedure FormCreate(Sender: TObject);
    procedure btnSelectSWMMFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    workingDirPath: string;
    { Private declarations }

  var
    TSList: TStringList; // stores existing swmm TS names in swmm inputfile
    InflowsList: TStringList;
    ErrsList: TStringList;
    operatingMode: string;
    swmmFilePath: string;
    NodeNameList: TStringList;
    PollList: TStringList;
    // stores existing swmm Inflow entry names in swmm inputfile

  public
    { Public declarations }
    function readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
      var Conv: TArray<TMTARecord>; Sender: TObject): TArray<TMTARecord>;

    procedure AssignConstituentInputsFromGrid(SWMMNodeName: string;
      var Conv: TMTARecord; constituentName: string; convFactor: Double;
      tsType: string; Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.btnNextClick(Sender: TObject);
var
  ConvertedFWTSArr: TArray<TMTARecord>;
  // i: TUserInputVerificationFrm;
  constituentCbxs: TArray<TComboBox>;
  // FileContentsList: TStringList;
  tempStr: string;
  frameworkTSFilePath, frameworkTSFileOutPath: string;
  // constinuentName: string;
  j: integer;
  tempModalResult: integer;
  numSelectedConstituents: integer;
  newSWMMInputFilePath: string;
  selectedNodeName: string;
  swmmNodeResults: TStringList;
begin

  // Launch project form and force user to provide the project information
  SWMMUserInputVerificationFrm := TUserInputVerificationFrm.Create(Application);
  frameworkTSFilePath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemo.sct';
  frameworkTSFileOutPath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemoOut.sct';
  SetLength(ConvertedFWTSArr, High(constituentNames));
  // swmmNodeResults := TStringList.Create();

  with SWMMUserInputVerificationFrm do
  begin
    // if Length(constituentCbxs) = 0 then
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
        AssignConstituentInputsFromGrid(selectedNodeName, ConvertedFWTSArr[0],
          'FLOW', StrToFloat(Self.sgdUserInputGrid.Cells[2, 1]),
          'FLOW', Sender);

        // save conversion factors and selected constituents into record array for use later
        // Framework Pollutants
        numSelectedConstituents := 1;
        for j := Low(constituentNames) + 1 to High(constituentNames) do
        begin
          if constituentCbxs[j].ItemIndex <> -1 then
          begin
            StringGrid1.Cells[1, j + 1] := constituentCbxs[j].Items
              [constituentCbxs[j].ItemIndex];
            AssignConstituentInputsFromGrid(txtSWMMNodeID.Caption,
              ConvertedFWTSArr[j], constituentNames[j],
              StrToFloat(Self.sgdUserInputGrid.Cells[2, 1]), 'CONCEN', Sender);
            inc(numSelectedConstituents);
          end;
        end;
      end;
      lblNumberOfConstituents.Caption := 'Constituents (' +
        IntToStr(numSelectedConstituents) + ' of 8 selected)';
      tempModalResult := SWMMUserInputVerificationFrm.ShowModal;
      if (tempModalResult = mrOk) then
      begin
        ConvertedFWTSArr := readInFrameworkTSFile(frameworkTSFilePath,
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
        newSWMMInputFilePath := workingDirPath + '\' +
          ChangeFileExt(ExtractFileName(Self.txtSwmmFilePath.Caption), '') +
          FormatDateTime('yyyymmddhhnnss', Now) + '.inp';
        OperationStatusDlg.lblSWMMFilePath.Caption := newSWMMInputFilePath;
        OperationStatusDlg.lblTSFilesCreated.Caption := tempStr;
        OperationStatusDlg.ShowModal;

        if (operatingMode = opModes[0]) then // importing from swmm Binary
        begin
          swmmNodeResults := SWMMOutput.importFromSWMMToFW(swmmFilePath,
            frameworkTSFileOutPath, selectedNodeName, PollList,
            ConvertedFWTSArr);
        end
        else // exporting to swmm input file
        begin
          // write TS to swmm input file and save as new file
          SWMMInput.updateSWMMInputFile(ConvertedFWTSArr, TSList,
            workingDirPath, Self.txtSwmmFilePath.Caption, Sender);
          Self.Close;
        end;
      end;
    finally
      // ConvertedFWTSArr.Free;
    end;
  end;
end;

procedure TForm1.AssignConstituentInputsFromGrid(SWMMNodeName: string;
  var Conv: TMTARecord; constituentName: string; convFactor: Double;
  tsType: string; Sender: TObject);
begin
  Conv.tsNodeName := SWMMNodeName;
  Conv.tsType := tsType;
  Conv.tsName := 'FrameworkTS' + constituentName;
  Conv.tsUnitsFactor := 1.0;
  Conv.constituentSWMMName := constituentName;
  Conv.convFactor := convFactor;
end;

function TForm1.readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
  var Conv: TArray<TMTARecord>; Sender: TObject): TArray<TMTARecord>;
var
  FileContentsList: TStringList;
  // OutList: TStringList;
  // PollList: TStringList;
  // SwmmTokens: TStringList;
  strLine: string;
  // intTokenLoc: integer;
  // startDate: string;
  // endDate: string;
  lineNumber: integer;
  tempStrList: TStrings;
  tempDateTimeStr: string;
  tempValueStr: string;
  i: integer;
  j: integer;
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
  FileContentsList: TStringList;
  TempListArr: TArray<TStringList>;
  SwmmTokens: TStringList;
  lineNumber: integer;
  intTokenLoc: integer;
  // strLine: string;
  // strToken: string;
  strNodeName: string;
  // tempInt: integer;
  // i: integer;
  mtaFilePath: string; // TODO Delete
  mtaData: TArray<TMTARecord>;
  swmmIDsListArr: TArray<TStringList>;
begin
  FileContentsList := TStringList.Create;
  NodeNameList := TStringList.Create;
  PollList := TStringList.Create;
  TSList := TStringList.Create;
  InflowsList := TStringList.Create;
  intTokenLoc := 0;

  // TODO delete - testing read mta
  mtaFilePath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemo.mta';
  mtaData := ReadMTA.Read(mtaFilePath);
  WriteMTA.Write(mtaFilePath + '2', mtaData);
  try
    { Execute an open file dialog. }
    // openFileFlag := OpenTextFileDialog1.Execute;
    if OpenTextFileDialog1.Execute then
      { First check if the file exists. }
      if FileExists(OpenTextFileDialog1.FileName) then
      begin
        // save the directory so can write TS to same directory later
        workingDirPath := ExtractFileDir(OpenTextFileDialog1.FileName);
        swmmFilePath := OpenTextFileDialog1.FileName;
        txtSwmmFilePath.Caption := swmmFilePath;

        if (operatingMode = opModes[0]) then // importing from swmm binary file
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
          { If it exists, load the data into the stringlist. }
          FileContentsList.LoadFromFile(swmmFilePath);
          // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
          swmmIDsListArr := SWMMIO.getSWMMNodeIDsFromTxtInput(swmmFilePath);
          TSList := swmmIDsListArr[2];
          InflowsList := swmmIDsListArr[3];
          NodeNameList := swmmIDsListArr[0];
          PollList := swmmIDsListArr[1];
        end
      end
      else
      begin
        { Otherwise, raise an exception. }
        raise Exception.Create('File does not exist.');
        Exit
      end;
    cbxSwmmNode.Items := NodeNameList;
    cbxFlow.Items := PollList;
    cbxDCu.Items := PollList;
    cbxTCu.Items := PollList;
    cbxTZn.Items := PollList;
    cbxDZn.Items := PollList;
    cbxDP.Items := PollList;
    cbxTP.Items := PollList;
    cbxTSS.Items := PollList;
  finally
    FileContentsList.Free;
    // NodeNameList.Free;
    // PollList.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenTextFileDialog1.Filter := 'Text files (*.txt)|*.TXT|Any file (*.*)|*.*';
  SaveTextFileDialog1.Filter := 'Text files (*.txt)|*.TXT|Any file (*.*)|*.*';

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ErrsList := TStringList.Create();
  Form1.color := clwhite;
  // customize stringGrid
  // column headers
  sgdUserInputGrid.Cells[0, 0] := '* Framework Constituent';
  sgdUserInputGrid.Cells[1, 0] := 'SWMM Equivalent';
  sgdUserInputGrid.Cells[2, 0] := 'Unit Conversion Factor';

  // polutants
  sgdUserInputGrid.Cells[0, 1] := 'FLOW ';
  sgdUserInputGrid.Cells[0, 2] := 'TSS';
  sgdUserInputGrid.Cells[0, 3] := 'TP';
  sgdUserInputGrid.Cells[0, 4] := 'DP ';
  sgdUserInputGrid.Cells[0, 5] := 'DZn';
  sgdUserInputGrid.Cells[0, 6] := 'TZN';
  sgdUserInputGrid.Cells[0, 7] := 'DCU';
  sgdUserInputGrid.Cells[0, 8] := 'TCU';

  // unit conversion factors
  sgdUserInputGrid.Cells[2, 1] := '1.00';
  sgdUserInputGrid.Cells[2, 2] := '1.00';
  sgdUserInputGrid.Cells[2, 3] := '1.00';
  sgdUserInputGrid.Cells[2, 4] := '1.00 ';
  sgdUserInputGrid.Cells[2, 5] := '1.00';
  sgdUserInputGrid.Cells[2, 6] := '1.00';
  sgdUserInputGrid.Cells[2, 7] := '1.00';
  sgdUserInputGrid.Cells[2, 8] := '1.00';

  // mode of operation radio buttons
  RadioGroup1.Items.AddObject('Import from SWMM', TObject(0));
  RadioGroup1.Items.AddObject('Export to SWMM', TObject(1));
  RadioGroup1.ItemIndex := 0;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
  index: integer;
  val: integer;
begin
  index := RadioGroup1.ItemIndex;
  Assert(index >= 0);
  // Sanity check
  val := integer(RadioGroup1.Items.Objects[index]);
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
