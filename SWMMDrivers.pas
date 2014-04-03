{ ------------------------------------------------------------------- }
{ Unit:    SWMMDrivers.pas                                            }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit that for the main interface GUI that             }
{ ------------------------------------------------------------------- }
unit SWMMDrivers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Generics.Defaults, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Grids,
  Vcl.ExtCtrls, UserInputConfirmationDlg, OperationStatusDlgFrm,
  ImportHelpDialogFrm, ExportHelpDlgFrm, SWMMIO,
  SWMMInput, SWMMOutput, ReadMTA, WriteMTA, ComCtrls, BusyDialogFrm, GIFImg,
  FWControlScratchFile, StrUtils;

const

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
    lblHelp: TLabel;

    ///	<summary>
    ///	  Handler for button used to browse to SWMM file
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner of the button (this form)
    ///	</param>
    procedure btnSelectSWMMFileClick(Sender: TObject);

    ///	<summary>
    ///	  Handler for form show event. Most of the setup of the form occurs in
    ///	  this method
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner (this form)
    ///	</param>
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
    //procedure ProgressCallback(InProgressOverall: TProgressBar);
    //procedure RadioGroup1Click(Sender: TObject);

    ///	<summary>
    ///	  Handler for cancel button
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner of the button (this form)
    ///	</param>
    procedure btnCancelClick(Sender: TObject);

    ///	<summary>
    ///	  Change handler for SWMM node selection combo box
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner of the combo box (this form)
    ///	</param>
    procedure cbxSwmmNodeChange(Sender: TObject);

    ///	<summary>
    ///	  Handler for help button. Displays a dialog with help on how to use
    ///	  this tool
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner of the button (this form)
    ///	</param>
    procedure btnHelpClick(Sender: TObject);

    ///	<summary>
    ///	  Handler for help link. Displays a dialog with help on how to use this
    ///	  tool
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner of the button (this form)
    ///	</param>
    procedure lblHelpClick(Sender: TObject);

  private
    workingDirPath: string;

    ///	<summary>
    ///	  method for populating a TMTARecFields data structure which holds user input
    ///	  collected on this form
    ///	</summary>
    ///	<param name="SWMMNodeName">
    ///	  Name of SWMM node being acted on
    ///	</param>
    /// <param name="Conv">
    ///	  Name of SWMM node being acted on
    ///	</param>
    /// <param name="constituentName">
    ///	  Name of constituent
    ///	</param>
    /// <param name="convFactor">
    ///	  Conversion factor
    ///	</param>
    /// <param name="tsType">
    ///	  Type of constituent (either CONCEN or FLOW)
    ///	</param>
    /// <param name="ScenarioDescription">
    ///	  Description of scenario
    ///	</param>
    /// <param name="swmmFilePath">
    ///	  SWMM input or output filepath
    ///	</param>
    /// <param name="fwScratchFilePath">
    ///	  Framework control scratch file path
    ///	</param>
    /// <param name="mtaFilePath">
    ///	  SWMM control file path
    ///	</param>
    procedure AssignMTARecFields(SWMMNodeName: string; var Conv: TMTARecord;
      constituentName: string; convFactor: Double; tsType: string;
      ScenarioDescription: string; swmmFilePath: string;
      fwScratchFilePath: string; mtaFilePath: string);
    { Private declarations }

  var
    swmmFilePath: string;

  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.btnHelpClick(Sender: TObject);
begin
  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then // importing from SWMM
    ImportHelpDialog.ShowModal()
  else
    ExportHelpDialogFrm.ShowModal();
end;

procedure TForm1.lblHelpClick(Sender: TObject);
begin
  btnHelpClick(Sender);
end;

{procedure TForm1.ProgressCallback(InProgressOverall: TProgressBar);
var
  Index: Integer;
  // kIndex: Integer;
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
end;}

procedure TForm1.btnNextClick(Sender: TObject);
var
  ConvertedFWTSArr: TArray<TMTARecord>;
  constituentCbxs: TArray<TComboBox>;
  tempStr: string;
  j: Integer;
  tempModalResult: Integer;
  numSelectedConstituents: Integer;
  newSWMMInputFilePath: string;
  selectedNodeName: string;
  scenarioDescr: string;
  FWCtrlRecord: FWCtrlScratchRecord;
  fwTSFileToCreatePath: string;
  tempStrList: TStringList;
begin

  // Launch project form and force user to provide the project information
  SWMMUserInputVerificationFrm := TUserInputVerificationFrm.Create(Application);
  // TODO delete - hardcoded filepaths

  SetLength(ConvertedFWTSArr, High(constituentNames) + 1);
  scenarioDescr := txtScenarioDescr.Text;
  if (operatingMode = opModes[0]) then // importing from swmm Binary
    newSWMMInputFilePath := Self.txtSwmmFilePath.Caption
  else // exporting to swmm input file
  begin
    newSWMMInputFilePath := workingDirPath + '\' +
      AnsiLeftStr(ChangeFileExt(ExtractFileName(Self.txtSwmmFilePath.Caption),
      ''), 10) + '_' + FormatDateTime('yyyymmddhhnnss', Now) + '.inp';
    // swmmFilePath := newSWMMInputFilePath;
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

        // make up file path for the Framework scratch timeseries data file
        fwTSFileToCreatePath := ExtractFilePath(swmmFilePath) + selectedNodeName
          + 'FWTS.sct';
        // make up file path for the Converter Control file
        SWMMIO.mtaFilePath := ExtractFilePath(swmmFilePath) + selectedNodeName +
          SWMMIO.operatingMode + '.mta';

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
          newSWMMInputFilePath, fwTSFileToCreatePath, mtaFilePath);

        // save conversion factors and selected constituents into record array for use later
        numSelectedConstituents := 1;
        for j := Low(constituentNames) + 1 to High(constituentNames) do
        begin
          if constituentCbxs[j].ItemIndex > 0 then
          begin
            StringGrid1.Cells[1, j + 1] := constituentCbxs[j].Items
              [constituentCbxs[j].ItemIndex];
            AssignMTARecFields(selectedNodeName, ConvertedFWTSArr[j],
              constituentNames[j], StrToFloat(Self.sgdUserInputGrid.Cells[2,
              j + 1]), 'CONCEN', scenarioDescr, newSWMMInputFilePath,
              fwTSFileToCreatePath, mtaFilePath);
            inc(numSelectedConstituents);
          end;
        end;
      end;
      lblNumberOfConstituents.Caption := 'Constituents (' +
        IntToStr(numSelectedConstituents) + ' of 8 selected)';

      tempModalResult := SWMMUserInputVerificationFrm.ShowModal;
      if (tempModalResult = mrOk) then
      begin
        Application.CreateForm(TBusyFrm, BusyFrm);
        Self.Hide;

        Screen.FocusedForm := BusyFrm;
        SendMessage(BusyFrm.Handle, CM_ACTIVATE, 0, 0);
        (BusyFrm.Image1.Picture.Graphic as TGIFImage).AnimateLoop := glEnabled;
        (BusyFrm.Image1.Picture.Graphic as TGIFImage).Animate := true;
        BusyFrm.Show;
        Application.ProcessMessages;

        if (operatingMode = opModes[0]) then // importing from swmm Binary
        begin
          BusyFrm.Caption := 'Importing from SWMM';
          FWCtrlRecord := SWMMOutput.importFromSWMMToFW(swmmFilePath,
            fwTSFileToCreatePath, selectedNodeName, ConvertedFWTSArr);
          // save user settings to MTA file to allow subsequent commandline executions without user intervention
          WriteMTA.Write(mtaFilePath, ConvertedFWTSArr);
          BusyFrm.Close;
          // populate operation status confirmation dialog
          OperationStatusDlg.Caption := 'Import Operation Status';
          OperationStatusDlg.lblHeader.Caption :=
            'Import Operation was successful!';
          OperationStatusDlg.lblSWMMFilePath.Visible := false;
          OperationStatusDlg.lblSWMMFileLabel.Visible := false;
          OperationStatusDlg.lblTSFilesCreated.Caption :=
            FWCtrlRecord.scratchFilePath;
          OperationStatusDlg.ShowModal;
        end
        else // exporting to swmm input file
        begin
          BusyFrm.Caption := 'Exporting to SWMM';
          // open control file and read in path to scratchfile
          tempStrList := TStringList.Create();
          tempStrList.LoadFromFile(SWMMIO.frameCtrlFilePath);
          // first line in the file should be the path to the timeseries input file
          ConvertedFWTSArr := SWMMIO.readInFrameworkTSFile(tempStrList[0],
            ConvertedFWTSArr);
          SWMMInput.finalizeExport(ConvertedFWTSArr, workingDirPath);
          // write exported TS to the confirmation dialog
          for j := Low(ConvertedFWTSArr) to High(ConvertedFWTSArr) do
          begin
            if (ConvertedFWTSArr[j].convertedTSFilePath <> '') then
              tempStr := tempStr + #13#10 + ConvertedFWTSArr[j]
                .convertedTSFilePath;
          end;
          // write TS and Inflow blocks to swmm input file and save as new file
          SWMMInput.updateSWMMInputFile(ConvertedFWTSArr, swmmFilePath,
            newSWMMInputFilePath);
          // save user settings to MTA file to allow subsequent commandline executions without user intervention
          WriteMTA.Write(mtaFilePath, ConvertedFWTSArr);
          BusyFrm.Close;
          // populate operation status confirmation dialog
          OperationStatusDlg.Caption := 'Export Operation Status';
          OperationStatusDlg.lblHeader.Caption :=
            'Export Operation was successful!';
          OperationStatusDlg.lblSWMMFilePath.Caption := newSWMMInputFilePath;
          OperationStatusDlg.lblTSFilesCreated.Caption := tempStr;
          OperationStatusDlg.ShowModal;
          Self.Close;
        end;
        Self.Close;
      end;
    finally
      BusyFrm.Close;
      // ConvertedFWTSArr.Free;
    end;
  end;
end;

procedure TForm1.AssignMTARecFields(SWMMNodeName: string; var Conv: TMTARecord;
  constituentName: string; convFactor: Double; tsType: string;
  ScenarioDescription: string; swmmFilePath: string; fwScratchFilePath: string;
  mtaFilePath: string);
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
  Conv.mtaFilePath := mtaFilePath;
end;

procedure TForm1.btnSelectSWMMFileClick(Sender: TObject);
var
  TempListArr: TArray<TStringList>;
  // intTokenLoc: Integer;
  swmmIDsListArr: TArray<TStringList>;
begin
  // intTokenLoc := 0;
  try
    if OpenTextFileDialog1.Execute then
    begin
      { First check if the file exists. }
      if FileExists(OpenTextFileDialog1.FileName) then
      begin
        Height := 234;
        // save the directory so can write TS to same directory later
        workingDirPath := ExtractFileDir(OpenTextFileDialog1.FileName);
        swmmFilePath := OpenTextFileDialog1.FileName;
        txtSwmmFilePath.Caption := swmmFilePath;

        if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
        // importing from swmm binary file
        begin
          TempListArr := SWMMIO.getSWMMNodeIDsFromBinary(swmmFilePath);
          if (TempListArr[0].Count < 1) then
            SWMMIO.errorsList.Add(Errs[0]);
          // Unable to read nodes in SWMM input file
          if (TempListArr[1].Count < 1) then
            SWMMIO.errorsList.Add(Errs[0]);
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

      SWMMIO.PollList.Insert(0,'Exclude');
      cbxFlow.Items := SWMMIO.PollList;
      cbxDCu.Items := SWMMIO.PollList;
      cbxTCu.Items := SWMMIO.PollList;
      cbxTZn.Items := SWMMIO.PollList;
      cbxDZn.Items := SWMMIO.PollList;
      cbxDP.Items := SWMMIO.PollList;
      cbxTP.Items := SWMMIO.PollList;
      cbxTSS.Items := SWMMIO.PollList;
    end;
  finally
  end;
end;

procedure TForm1.cbxSwmmNodeChange(Sender: TObject);
begin
  Height := 620;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Height := 130;
  SWMMIO.errorsList := TStringList.Create();
  Form1.color := clwhite;

  // need framwork metadata scratchfile file for export
  // control file tells us what the name of the framework node is that we will be exporting from
  // so if framwork metadata scratchfile was not passed in via commandline so ask for it
  if (SWMMIO.frameCtrlFilePath = '') and
    (SWMMIO.operatingMode = SWMMIO.opModes[1]) then
  begin
    SWMMIO.frameCtrlFilePath := SWMMIO.workingDir + 'swmmconvertstring.txt';
    if (Not(FileExists(SWMMIO.frameCtrlFilePath))) then
    begin
      MessageDlg
        ('A Valid Framework Control File was not located. Press okay to browse and select one',
        mtInformation, [mbOK], 0);
      OpenTextFileDialog1.Filter := 'Framework Control File (*.txt)|*.TXT';

      if OpenTextFileDialog1.Execute then
      begin
        { First check if the file exists. }
        if FileExists(OpenTextFileDialog1.FileName) then
        begin
          SWMMIO.frameCtrlFilePath := OpenTextFileDialog1.FileName;
        end
        else // we are exporting to swmm so using SWMM input file
        begin
          MessageDlg('A Valid Framework Control File was not Found',
            mtInformation, [mbOK], 0);
        end;
      end;
    end;
  end;

  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
  // SWMM_TO_FW importing from swmm binary file
  begin
    lblOperatingMode.Caption := 'Importing from SWMM to WERF Framework';
    btnSelectSWMMFile.Caption := 'Select SWMM Results Output File';
    OpenTextFileDialog1.Filter := 'SWMM Results (*.out)|*.OUT';
    SaveTextFileDialog1.Filter := 'SWMM Input (*.inp)|*.INP';
  end
  else // SWMM_FROM_FW we are exporting to swmm so using SWMM input file
  begin
    lblOperatingMode.Caption := 'Exporting from WERF Framework to SWMM';
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
end;

{procedure TForm1.RadioGroup1Click(Sender: TObject);
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
end;}

end.
