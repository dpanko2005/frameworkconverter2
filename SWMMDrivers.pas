unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Defaults, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Grids,
  Vcl.ExtCtrls, UserInputConfirmationDlg;

type
  TConvertedFWTS = record // converted framework timeseries data structure
    constituentName: string;
    convFactor: Double;
    convertedTS: TStringList;
  end;

const
  constituentNames: array [0 .. 7] of string = ('FLOW', 'TSS', 'TP', 'DP',
    'DZn', 'TZN', 'DCU', 'TCU');

type
  TForm1 = class(TForm)
    OpenTextFileDialog1: TOpenTextFileDialog;
    SaveTextFileDialog1: TSaveTextFileDialog;
    btnSelectSWMMFile: TButton;
    txtSwmmFilePath: TLabel;
    cbxSwmmNode: TComboBox;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
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

  private
    { Private declarations }
  public
    { Public declarations }
    procedure readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
      var Conv: array of TConvertedFWTS; Sender: TObject);

    procedure AssignConstituentInputsFromGrid(convGrid: TStringGrid;
      var Conv: TConvertedFWTS; constituentName: string; rowNumber: integer;
      Sender: TObject);

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnNextClick(Sender: TObject);

var
  frameworkTSFilePath: string;
  ConvertedFWTSArr: array of TConvertedFWTS;
  i: TUserInputVerificationFrm;
var
  tempStr: string;
  constinuentName: string;
  j: integer;
  constituentCbxs: TArray<TComboBox>;
  { const
    constituentCbxs: array [0 .. 7] of TComboBox = (cbxFlow, cbxTSS, cbxTP, cbxDP,
    cbxDZn, cbxTZn, cbxDCu, cbxTCu); }

begin
  // Launch project form and force user to provide the project information
  SWMMUserInputVerificationFrm := TUserInputVerificationFrm.Create(Application);
  frameworkTSFilePath :=
    'C:\Users\dpankani\Documents\RAD Studio\Projects\SWMMDrivers\testfiles\RockCreekDemo.sct';
  SetLength(ConvertedFWTSArr, 6);

  with SWMMUserInputVerificationFrm do
  begin
    // if Length(constituentCbxs) = 0 then
    constituentCbxs := TArray<TComboBox>.Create(cbxFlow, cbxTSS, cbxTP, cbxDP,
      cbxDZn, cbxTZn, cbxDCu, cbxTCu);

    try
      begin // populate dialog fields
        txtSwmmFilePath.Caption := Self.txtSwmmFilePath.Caption;
        if cbxSwmmNode.ItemIndex <> -1 then
          txtSWMMNodeID.Caption := cbxSwmmNode.Items[cbxSwmmNode.ItemIndex];

        { txtTSStartDate.Caption := Self.txtTSStartDate.Caption;
          txtTSEndDate.Caption := Self.txtTSEndDate.Caption;
          txtTSFilePath.Caption := Self.txtTSFilePath.Caption;
          txtErrors.Caption := Self.txtErrors.Caption; }

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
          StringGrid1.Cells[2, j + 1] := Self.StringGrid1.Cells[2, j + 1];
          // copy unit conversion factors from this grid to other grid on user input verification form
        end;

        // Flow is a special case constituent which is always selected
        StringGrid1.Cells[1, 1] := 'FLOW'; // flow is always selected
        AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[0],
          'Flow', 1, Sender);

        // save conversion factors and selected constituents into record array for use later
        // Framework Pollutants
        for j := Low(constituentNames) + 1 to High(constituentNames) do
        begin
          if constituentCbxs[j].ItemIndex <> -1 then
          begin
            StringGrid1.Cells[1, j + 1] := constituentCbxs[j].Items
              [constituentCbxs[j].ItemIndex];
            AssignConstituentInputsFromGrid(Self.StringGrid1,
              ConvertedFWTSArr[j], constituentNames[j], j + 1, Sender);
          end;
        end;

        { if cbxTSS.ItemIndex <> -1 then
          begin
          StringGrid1.Cells[1, 2] := cbxTSS.Items[cbxTSS.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[1],
          'TSS', 2, Sender);
          end;

          if cbxTP.ItemIndex <> -1 then
          begin
          StringGrid1.Cells[1, 3] := cbxTP.Items[cbxTP.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[2],
          'TP', 3, Sender);
          end;

          if cbxDP.ItemIndex <> -1 then
          begin
          StringGrid1.Cells[1, 4] := cbxDP.Items[cbxDP.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[3],
          'DP', 4, Sender);
          end;

          if cbxDZn.ItemIndex <> -1 then
          StringGrid1.Cells[1, 5] := cbxDZn.Items[cbxDZn.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[4],
          'DZn', 5, Sender);

          if cbxTZn.ItemIndex <> -1 then
          begin
          StringGrid1.Cells[1, 6] := cbxTZn.Items[cbxTZn.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[5],
          'TZN', 6, Sender);
          end;

          if cbxDCu.ItemIndex <> -1 then
          begin
          StringGrid1.Cells[1, 7] := cbxDCu.Items[cbxDCu.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[6],
          'DCU', 7, Sender);
          end;

          if cbxTCu.ItemIndex <> -1 then
          begin
          StringGrid1.Cells[1, 8] := cbxTCu.Items[cbxTCu.ItemIndex];
          AssignConstituentInputsFromGrid(Self.StringGrid1, ConvertedFWTSArr[7],
          'TCU', 8, Sender);
          end; }

        // copy unit conversion factors from this grid to other grid on user input verification form
        { StringGrid1.Cells[2, 1] := Self.StringGrid1.Cells[2, 1];
          StringGrid1.Cells[2, 2] := Self.StringGrid1.Cells[2, 2];
          StringGrid1.Cells[2, 3] := Self.StringGrid1.Cells[2, 3];
          StringGrid1.Cells[2, 4] := Self.StringGrid1.Cells[2, 4];
          StringGrid1.Cells[2, 5] := Self.StringGrid1.Cells[2, 5];
          StringGrid1.Cells[2, 6] := Self.StringGrid1.Cells[2, 6];
          StringGrid1.Cells[2, 7] := Self.StringGrid1.Cells[2, 7];
          StringGrid1.Cells[2, 8] := Self.StringGrid1.Cells[2, 8]; }
      end;
      readInFrameworkTSFile(frameworkTSFilePath, StringGrid1,
        ConvertedFWTSArr, Sender);
      SWMMUserInputVerificationFrm.ShowModal;
    finally
      // ConvertedFWTSArr.Free;
    end;
  end;
end;

procedure TForm1.AssignConstituentInputsFromGrid(convGrid: TStringGrid;
  var Conv: TConvertedFWTS; constituentName: string; rowNumber: integer;
  Sender: TObject);
var
  tempStr: string;
begin
  Conv.constituentName := constituentName;
  tempStr := convGrid.Cells[2, rowNumber];
  Conv.convFactor := StrToFloat(tempStr);
end;

procedure TForm1.readInFrameworkTSFile(filePath: string; convGrid: TStringGrid;
  var Conv: array of TConvertedFWTS; Sender: TObject);
var
  FileContentsList: TStringList;
  OutList: TStringList;
  PollList: TStringList;
  SwmmTokens: TStringList;
  strLine: string;
  // intTokenLoc: integer;
  startDate: string;
  endDate: string;
  lineNumber: integer;
  tempStrList: TStrings;
begin

  // 1. array of conversion factors
  // 2. save outputs to same location as swmmm input file
  // 3. no array of timeseries names + descriptions + filepaths

  try
    FileContentsList := TStringList.Create;
    OutList := TStringList.Create;
    tempStrList := TStringList.Create;

    FileContentsList.LoadFromFile(filePath);
    lineNumber := 0;
    while lineNumber < FileContentsList.Count - 1 do
    begin
      strLine := FileContentsList[lineNumber];
      // ignore comment lines
      if (Pos('#', strLine) < 1) and (Length(strLine) > 1) then
      begin
        ExtractStrings([','], [], PChar(strLine), tempStrList);
        // Writeln(strLine);
        OutList.Add('test');
      end;
      inc(lineNumber);
    end;
    FileContentsList.Free;
    OutList.Free;
    tempStrList.Free;
  finally

  end;
end;

procedure TForm1.btnSelectSWMMFileClick(Sender: TObject);
var
  FileContentsList: TStringList;
  NodeList: TStringList;
  PollList: TStringList;
  SwmmTokens: TStringList;
  lineNumber: integer;
  intTokenLoc: integer;
  strLine: string;
  strToken: string;
  strNodeName: string;
  tempInt: integer;
  i: integer;
  // openFileFlag: Boolean;
  // dict : TDictionary<integer, TStringList> ;
begin
  try
    FileContentsList := TStringList.Create;
    NodeList := TStringList.Create;
    PollList := TStringList.Create;
    { Execute an open file dialog. }
    // openFileFlag := OpenTextFileDialog1.Execute;
    if OpenTextFileDialog1.Execute then
      { First check if the file exists. }
      if FileExists(OpenTextFileDialog1.FileName) then
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
          '|[DIVIDERS]| |[JUNCTIONS]| |[OUTFALLS]| |[STORAGE]| |[POLLUTANTS]|';

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
                  else // if we are not in the [POLLUTANTS] block save names to nodes list
                    NodeList.Add(strNodeName);
                end;
              end;
            until intTokenLoc > 0;
          end;
          inc(lineNumber);

          // Save a new swmm file back to disc
          { Execute a save file dialog. }
          // if SaveTextFileDialog1.Execute then
          { First check if the file exists. }
          // if FileExists(SaveTextFileDialog1.FileName) then
          { If it exists, raise an exception. }
          // raise Exception.Create('File already exists. Cannot overwrite.')
          // else
          // FileContentsList.SaveToFile(SaveTextFileDialog1.FileName);
        end;
        Memo1.Lines.AddStrings(PollList);
        Memo1.Lines.AddStrings(NodeList);
        cbxSwmmNode.Items := NodeList;
        cbxFlow.Items := PollList;
        cbxDCu.Items := PollList;
        cbxTCu.Items := PollList;
        cbxTZn.Items := PollList;
        cbxDZn.Items := PollList;
        cbxDP.Items := PollList;
        cbxTP.Items := PollList;
        cbxTSS.Items := PollList;
      end
      else
        { Otherwise, raise an exception. }
        raise Exception.Create('File does not exist.');
  finally
    FileContentsList.Free;
    NodeList.Free;
    PollList.Free;
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

  Form1.color := clwhite;
  // customize stringGrid
  // column headers
  StringGrid1.Cells[0, 0] := '* Framework Constituent';
  StringGrid1.Cells[1, 0] := 'SWMM Equivalent';
  StringGrid1.Cells[2, 0] := 'Unit Conversion Factor';

  // polutants
  StringGrid1.Cells[0, 1] := 'FLOW ';
  StringGrid1.Cells[0, 2] := 'TSS';
  StringGrid1.Cells[0, 3] := 'TP';
  StringGrid1.Cells[0, 4] := 'DP ';
  StringGrid1.Cells[0, 5] := 'DZn';
  StringGrid1.Cells[0, 6] := 'TZN';
  StringGrid1.Cells[0, 7] := 'DCU';
  StringGrid1.Cells[0, 8] := 'TCU';

  // unit conversion factors
  StringGrid1.Cells[2, 1] := '1.00';
  StringGrid1.Cells[2, 2] := '1.00';
  StringGrid1.Cells[2, 3] := '1.00';
  StringGrid1.Cells[2, 4] := '1.00 ';
  StringGrid1.Cells[2, 5] := '1.00';
  StringGrid1.Cells[2, 6] := '1.00';
  StringGrid1.Cells[2, 7] := '1.00';
  StringGrid1.Cells[2, 8] := '1.00';

  // mode of operation radio buttons
  RadioGroup1.Items.AddObject('Import from SWMM', TObject(0));
  RadioGroup1.Items.AddObject('Export to SWMM', TObject(1));
  RadioGroup1.ItemIndex := 0;

end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
  index: integer;
  value: integer;
begin
  index := RadioGroup1.ItemIndex;
  Assert(index >= 0); // Sanity check
  value := integer(RadioGroup1.Items.Objects[index]);
  if (value = 0) then
    btnSelectSWMMFile.Caption := 'Select SWMM Output File';
  if (value = 1) then
    btnSelectSWMMFile.Caption := 'Select SWMM Input File';
end;

{ procedure TForm1.WriteControlFile(Sender: TObject);
  begin

  AssignFile(CtrlFile, " controlFile.txt ");
  Reset(CtrlFile);
  if SaveDialog1.Execute then
  begin
  AssignFile(F2, SaveDialog1.FileName);
  Rewrite(F2);
  while not Eof(F1) do
  begin
  Read(F1, Ch);
  Write(F2, Ch);
  end;
  CloseFile(F2);
  end;
  CloseFile(F1);
  end;

  // Save a TStringGrid to a file
  procedure SaveStringGrid(StringGrid: TStringGrid; const FileName: TFileName);
  var
  CtrlFile: TextFile;
  i, k: integer;
  strLine: string;
  begin
  AssignFile(CtrlFile, FileName);
  Rewrite(CtrlFile);
  with StringGrid do
  begin
  // loop through cells
  for i := 0 to RowCount - 1 do
  begin
  strLine := '';
  for k := 0 to ColCount - 1 do
  strLine = strLine + Cells[i, k];

  Writeln(CtrlFile, strLine);
  end;
  end;
  CloseFile(CtrlFile);
  end;
}
end.
