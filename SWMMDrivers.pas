unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Defaults, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Grids,
  Vcl.ExtCtrls, UserInputConfirmationDlg, OperationStatusDlgFrm, SWMMIO;

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
    procedure saveTextFileToDisc(FileContentsList: TStringList;
      filePath: string; Sender: TObject);

    procedure finalizeExport(var Conv: array of TConvertedFWTS;
      filePathDir: string; Sender: TObject);
    function updateSWMMInputFile(var Conv: array of TConvertedFWTS;
      SWMMTSList: TStringList; filePathDir: string; swmmInputFilePath: string;
      Sender: TObject): string;

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
      var Conv: TArray<TConvertedFWTS>; Sender: TObject)
      : TArray<TConvertedFWTS>;

    procedure AssignConstituentInputsFromGrid(SWMMNodeName: string;
      var Conv: TConvertedFWTS; constituentName: string; convFactor: Double;
      tsType: string; Sender: TObject);

    function checkForDuplicateTS(tsBlockInsertPosition: integer;
      NewFileContentsList: TStringList; tsName: string): integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.checkForDuplicateTS(tsBlockInsertPosition: integer;
  NewFileContentsList: TStringList; tsName: string): integer;
var
  i: integer;
begin
  // check our cached list of TS names for a hit
  if (TSList.IndexOf(tsName) > 0) then
  begin
    // duplicate TS exists in the swmmfile so return its line number so can overwrite
    while ((Pos(';;', NewFileContentsList[tsBlockInsertPosition]) > 0) and
      (tsBlockInsertPosition < NewFileContentsList.Count)) do
    begin
      inc(tsBlockInsertPosition);
    end;

    while ((Pos(tsName, NewFileContentsList[tsBlockInsertPosition]) = 0) and
      (tsBlockInsertPosition < NewFileContentsList.Count)) do
    begin
      inc(tsBlockInsertPosition);
    end;

    if (Pos(tsName, NewFileContentsList[tsBlockInsertPosition]) > 0) then
    begin
      result := tsBlockInsertPosition;
      Exit;
    end
    else
      result := 0;
  end;
  result := 0;
end;

function TForm1.updateSWMMInputFile(var Conv: array of TConvertedFWTS;
  SWMMTSList: TStringList; filePathDir: string; swmmInputFilePath: string;
  Sender: TObject): string;
var
  NewFileContentsList: TStringList;
  // TSList: TStringList;
  lineNumber: integer;
  intTokenLoc: integer;
  // strLine: string;
  // strToken: string;
  // strNodeName: string;
  tempInt: integer;
  // i: integer;
  tsBlockInsertPosition: integer;
  tempRec: TConvertedFWTS;
  pathSuffix: string;
  newSWMMInputFilePath: string;
  duplicateLineNumber: integer;
begin
  // FileContentsList := TStringList.Create;
  NewFileContentsList := TStringList.Create;
  // TSList := TStringList.Create;
  // InflowsList := TStringList.Create;

  pathSuffix := FormatDateTime('yyyymmddhhnnss', Now) + '.inp';
  newSWMMInputFilePath := filePathDir + '\' +
    ChangeFileExt(ExtractFileName(swmmInputFilePath), '') + pathSuffix;
  try

    { First check if the file exists. }
    if FileExists(swmmInputFilePath) then
    begin
      { If it exists, load the data into the stringlist. }
      NewFileContentsList.LoadFromFile(OpenTextFileDialog1.FileName);

      tsBlockInsertPosition := NewFileContentsList.IndexOf('[REPORTS]');
      if (tsBlockInsertPosition < 0) then
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[CURVES]');
      if (tsBlockInsertPosition < 0) then
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[TAGS]');
      if (tsBlockInsertPosition < 0) then
      begin
        raise Exception.Create
          ('Check SWMM input file format. Unable to write timeseries to SWMM input file');
        Exit;
      end;

      // 1. Write TimeSeries Block
      // check TS list that was passed in to see if input file already contains TS
      if (SWMMTSList.Count > 0) then
      begin
        // timeseries section already exists in swmm input file so simply add to it - check for duplicate names
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[TIMESERIES]');
        while (Pos(';;', NewFileContentsList[tsBlockInsertPosition + 1]) < 1) do
        begin
          inc(tsBlockInsertPosition);
        end;
        // see check for duplicate TS names in swmm file on near line 196 below;
      end
      else
      begin
        // timeseries section does not already exist in swmm input file so write times series block and add to TS to it
        NewFileContentsList.Insert(tsBlockInsertPosition, '[TIMESERIES]');
        NewFileContentsList.Insert(tsBlockInsertPosition + 2,
          ';;Name          	Type      	Path');
        NewFileContentsList.Insert(tsBlockInsertPosition + 3,
          ';;-------------- ---------- ---------- ----------');
      end;

      tempInt := 3;
      for tempRec in Conv do
      begin
        if (tempRec.convertedTSFilePath <> '') then
        begin
          duplicateLineNumber := 0;
          duplicateLineNumber := checkForDuplicateTS(tsBlockInsertPosition,
            NewFileContentsList, tempRec.tsName);
          if (duplicateLineNumber <> 0) then
            NewFileContentsList.Delete(duplicateLineNumber);

          NewFileContentsList.Insert(tsBlockInsertPosition + tempInt,
            tempRec.tsName + '      FILE      "' +
            tempRec.convertedTSFilePath + '"');
        end;
        inc(tempInt);
      end;
      tsBlockInsertPosition := tsBlockInsertPosition + tempInt - 2;

      // 2. Write Inflow Block
      // check TS list that was passed in to see if input file already contains TS
      if ((SWMMTSList.Count > 0) and (NewFileContentsList.IndexOf('[INFLOWS]')
        > -1)) then
      begin
        // Inflow section already exists in swmm input file so simply add to it - check for duplicate names
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[INFLOWS]') + 1;
        while (Pos(';;', NewFileContentsList[tsBlockInsertPosition]) > 0) do
        begin
          inc(tsBlockInsertPosition);
        end;
        // TODO check for duplicates names in inflow block
      end
      else
      begin
        // Inflow section does not already exist in swmm input file so write times series block and add to TS to it
        NewFileContentsList.Insert(tsBlockInsertPosition, '');
        NewFileContentsList.Insert(tsBlockInsertPosition + 1, '[INFLOWS]');
        NewFileContentsList.Insert(tsBlockInsertPosition + 2,
          ';;                                                 Param    Units    Scale    Baseline Baseline');
        NewFileContentsList.Insert(tsBlockInsertPosition + 3,
          ';;Node           Parameter        Time Series      Type     Factor   Factor   Value    Pattern');
        NewFileContentsList.Insert(tsBlockInsertPosition + 4,
          ';;-------------- ---------------- ---------------- -------- -------- -------- -------- --------');
        tsBlockInsertPosition := tsBlockInsertPosition + 5;
      end;

      tempInt := 0;
      for tempRec in Conv do
      begin
        if (tempRec.convertedTSFilePath <> '') then
        begin
          NewFileContentsList.Insert(tsBlockInsertPosition + tempInt,
            tempRec.tsNodeName + '        ' + tempRec.constituentName +
            '        ' + tempRec.tsName + '        ' + tempRec.tsType +
            '        ' + tempRec.tsUnitsFactor + '        ' +
            FloatToStr(tempRec.convFactor));
        end;
        inc(tempInt);
      end;
      saveTextFileToDisc(NewFileContentsList, newSWMMInputFilePath, Sender);
    end
    else
      { Otherwise, raise an exception. }
      raise Exception.Create('File does not exist.');
  finally
    result := '';
    NewFileContentsList.Free;
  end;
  result := newSWMMInputFilePath;
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.btnNextClick(Sender: TObject);
var
  ConvertedFWTSArr: TArray<TConvertedFWTS>;
  i: TUserInputVerificationFrm;
  constituentCbxs: TArray<TComboBox>;
  FileContentsList: TStringList;
  tempStr: string;
  frameworkTSFilePath: string;
  constinuentName: string;
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
  SetLength(ConvertedFWTSArr, High(constituentNames));
  swmmNodeResults := TStringList.Create();

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
        finalizeExport(ConvertedFWTSArr, workingDirPath, Sender);

        // write exported TS to the confirmation dialog
        j := 0;
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
        tempModalResult := OperationStatusDlg.ShowModal;

        if (operatingMode = opModes[0]) then
        begin
          swmmNodeResults := getSWMMNodeResults(swmmFilePath, selectedNodeName,
            ConvertedFWTSArr, NodeNameList,PollList);
        end
        else
        begin
          // write TS to swmm input file and save as new file
          updateSWMMInputFile(ConvertedFWTSArr, TSList, workingDirPath,
            Self.txtSwmmFilePath.Caption, Sender);
          Self.Close;
        end;
      end;
    finally
      // ConvertedFWTSArr.Free;
    end;
  end;
end;

procedure TForm1.finalizeExport(var Conv: array of TConvertedFWTS;
  filePathDir: string; Sender: TObject);
var
  filePath: string;
  pathPrefix: string;
  pathSuffix: string;
  i: integer;
begin
  pathPrefix := filePathDir + '\TS\FrameworkTS_';
  pathSuffix := FormatDateTime('yyyymmddhhnnss', Now) + '.dat';

  for i := Low(Conv) to High(Conv) do
  begin
    if ((Conv[i].constituentName <> '') and (Conv[i].convFactor <> 0)) then
    begin
      filePath := pathPrefix + Conv[i].constituentName + pathSuffix;
      Conv[i].convertedTSFilePath := filePath;
      saveTextFileToDisc(Conv[i].convertedTS, filePath, Sender);
    end;
  end;
end;

procedure TForm1.AssignConstituentInputsFromGrid(SWMMNodeName: string;
  var Conv: TConvertedFWTS; constituentName: string; convFactor: Double;
  tsType: string; Sender: TObject);
begin
  Conv.tsNodeName := SWMMNodeName;
  Conv.tsType := tsType;
  Conv.tsName := 'FrameworkTS' + constituentName;
  Conv.tsUnitsFactor := '1.0';
  Conv.constituentName := constituentName;
  Conv.convFactor := convFactor;
end;

function TForm1.readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
  var Conv: TArray<TConvertedFWTS>; Sender: TObject): TArray<TConvertedFWTS>;
var
  FileContentsList: TStringList;
  OutList: TStringList;
  PollList: TStringList;
  // SwmmTokens: TStringList;
  strLine: string;
  // intTokenLoc: integer;
  startDate: string;
  endDate: string;
  lineNumber: integer;
  tempStrList: TStrings;
  tempDateTimeStr: string;
  tempValueStr: string;
  i: integer;
  j: integer;
begin

  // 1. array of conversion factors
  // 2. save outputs to same location as swmmm input file
  // 3. no array of timeseries names + descriptions + filepaths

  try
    FileContentsList := TStringList.Create;
    OutList := TStringList.Create;
    tempStrList := TStringList.Create;
    i := 0;
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
        { tempStrList.Text := StringReplace(strLine, ',', #13#10,
          [rfReplaceAll]); }

        tempDateTimeStr := tempStrList[0] + '/' + tempStrList[1] + '/' +
          tempStrList[2] + ' ' + tempStrList[3];
        i := 0;
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

procedure TForm1.saveTextFileToDisc(FileContentsList: TStringList;
  filePath: string; Sender: TObject);
var
  savedfilePath: string;
  dirName: string;
begin
  // Save a new swmm file back to disc
  if (FileContentsList <> nil) then
  begin
    // check if directory exists
    dirName := ExtractFilePath(filePath);
    if (DirectoryExists(dirName) = false) then
    begin
      if CreateDir(dirName) then
        // do nothing ShowMessage('New directory added OK')
      else
      begin
        raise Exception.Create
          ('Unable to create directory for saving timeseries - error : ' +
          IntToStr(GetLastError));
        Exit;
      end;
    end;

    { First check if the file exists. }
    if FileExists(filePath) then
      { If it exists, raise an exception. }
      raise Exception.Create('File already exists. Cannot overwrite.')
    else
      FileContentsList.SaveToFile(filePath);
  end;
end;

procedure TForm1.btnSelectSWMMFileClick(Sender: TObject);
var
  FileContentsList: TStringList;
  TempListArr: TArray<TStringList>;
  //NodeList: TStringList;
  //PollList: TStringList;
  SwmmTokens: TStringList;
  lineNumber: integer;
  intTokenLoc: integer;
  strLine: string;
  strToken: string;
  strNodeName: string;
  tempInt: integer;
  i: integer;
  // mode: string;
  // openFileFlag: Boolean;
  // dict : TDictionary<integer, TStringList> ;
begin
  FileContentsList := TStringList.Create;
  NodeNameList := TStringList.Create;
  PollList := TStringList.Create;
  TSList := TStringList.Create;
  InflowsList := TStringList.Create;
  intTokenLoc := 0;
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

        if (operatingMode = opModes[0]) then
        begin
          TempListArr := SWMMIO.getSWMMNodeNames(swmmFilePath);
          if (TempListArr[0].Count < 1) then
            ErrsList.Add(Errs[0]); // Unable to read nodes in SWMM input file
          if (TempListArr[1].Count < 1) then
            ErrsList.Add(Errs[0]);
          // Unable to read pollutant names in SWMM input file

          NodeNameList := TempListArr[0];
          PollList := TempListArr[1];
        end
        else
        begin
          { If it exists, load the data into the stringlist. }
          FileContentsList.LoadFromFile(OpenTextFileDialog1.FileName);
          txtSwmmFilePath.Caption := OpenTextFileDialog1.FileName;

          // Define a string list object, and point our variable at it
          SwmmTokens := TStringList.Create;

          // Add supported SWMM Node types to the list of tokens to parse
          SwmmTokens.Delimiter := ' '; // Each list item will be blank separated
          SwmmTokens.QuoteChar := '|'; // And each item will be quoted with |'s
          SwmmTokens.DelimitedText :=
            '|[DIVIDERS]| |[JUNCTIONS]| |[OUTFALLS]| |[STORAGE]| |[POLLUTANTS]| |[TIMESERIES]| |[INFLOWS]|';

          lineNumber := 0;
          // strToken := '[JUNCTIONS]';
          // strToken := LowerCase('[JUNCTIONS]');
          while lineNumber < FileContentsList.Count - 1 do
          begin
            strLine := LowerCase(FileContentsList[lineNumber]);
            for i := 0 to SwmmTokens.Count - 1 do
            begin
              strToken := LowerCase(SwmmTokens[i]);
              intTokenLoc := Pos(strToken, strLine);

              // check inputfile line to see if token present
              if intTokenLoc > 0 then
                break;
            end;

            // if token found read in node names
            if intTokenLoc > 0 then
            begin
              Repeat
                inc(lineNumber);
                strLine := FileContentsList[lineNumber];
                intTokenLoc := Pos('[', strLine);
                if intTokenLoc > 0 then
                begin
                  dec(lineNumber);
                  break;
                end;
                // ignore comment lines
                if (Pos(';;', strLine) < 1) and (Length(strLine) > 1) then
                begin
                  // extract node name
                  tempInt := Pos(' ', strLine);
                  if tempInt > 0 then
                  begin
                    strNodeName := Copy(strLine, 1, tempInt - 1);
                    if i = 4 then
                    // if we are in the [POLLUTANTS] block save names to pollutants list
                    begin
                      PollList.Add(strNodeName);
                    end
                    else if i = 5 then
                    // if we are in the [TIMESERIES] block save names to TIMESERIES list
                    begin
                      TSList.Add(strNodeName);
                    end
                    else if i = 6 then
                    // if we are in the [INFLOWS] block save names to INFLOWS list
                    begin
                      tempInt := Pos(' ', strLine, tempInt + 1); //
                      strNodeName := Copy(strLine, 1, tempInt - 1);
                      InflowsList.Add(strNodeName);
                    end
                    else
                      // if we are not in the [POLLUTANTS] block save names to nodes list
                      NodeNameList.Add(strNodeName);
                  end;
                end;
              until intTokenLoc > 0;
            end;
            inc(lineNumber);

          end;
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
    //NodeNameList.Free;
    //PollList.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {
    Initialize the dialog filters to open/save *.txt files
    and also files with arbitrary extensions.
  }
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
